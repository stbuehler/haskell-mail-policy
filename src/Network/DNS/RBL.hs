
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.DNS.RBL
-- Copyright   :  (c) 2014 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
--
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- DNS real-time blackhole functions (<http://en.wikipedia.org/wiki/DNSBL>)
--
-----------------------------------------------------------------------------

module Network.DNS.RBL
	( rblLookup
	, rblLookupHits
	, rblLookupFirst
	) where

import Control.Concurrent.Async
import qualified Data.ByteString as B
import Data.IP
import Data.List
import Data.Maybe (catMaybes)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T
import Network.DNS
import Numeric
import Text.Read (readMaybe)

{-|
Waits for the async jobs in order, returning the first 'Just' value it finds,
cancelling the remaining jobs.
-}
waitMaybeFirstCancel :: [Async (Maybe a)] -> IO (Maybe a)
waitMaybeFirstCancel [] = return Nothing
waitMaybeFirstCancel (x:xs) = waitCatch x >>= \c -> case c of
	Right r@(Just _) -> mapM_ cancel xs >> return r
	_ -> waitMaybeFirstCancel xs

{-|
Runs the tasks in parallel, waiting for the first job returning a 'Just' value
-}
mapMaybeFirst :: [IO (Maybe a)] -> IO (Maybe a)
mapMaybeFirst tasks = do
	jobs <- mapM async tasks
	waitMaybeFirstCancel jobs

{-|
Produces reverse hostname suitable for PTR lookup - or RBL.
-}
ipv4ReverseName :: IPv4 -> T.Text
ipv4ReverseName ip = let dot = T.pack "." in T.intercalate dot $ reverse $ T.splitOn dot $ T.pack $ show ip

{-|
Produces reverse hostname suitable for PTR lookup - or RBL.
-}
ipv6ReverseName :: IPv6 -> T.Text
ipv6ReverseName ip = T.pack $ intersperse '.' $ concatMap w $ reverse $ fromIPv6 ip
	where
	w i = take 4 $ reverse $ "000" ++ showHex i ""

{-|
Produces hostname suitable for RBL lookup (needs to reverse IP addresses)
-}
domainToRBLName :: T.Text -> T.Text
domainToRBLName domain = case readMaybe (T.unpack domain) :: Maybe IPv4 of
	Just ip -> ipv4ReverseName ip
	Nothing -> case readMaybe (T.unpack domain) :: Maybe IPv6 of
		Just ip -> ipv6ReverseName ip
		Nothing -> domain

{-|
Run RBL lookup (with some lookup* function, like 'lookupA' or 'lookupTXT'),
returning 'Nothing' on error or empty results, and 'Just' a non empty result
list otherwise.
-}
runLookup :: (Resolver -> B.ByteString -> IO (Either DNSError [a])) -> ResolvSeed -> T.Text -> T.Text -> IO (Maybe [a])
runLookup with rs entry list = do
	let hostname = encodeUtf8 $ T.concat [entry, T.pack ".", list]
	result <- withResolver rs $ \resolver -> with resolver hostname
	case result of
		Left _ -> return Nothing
		Right [] -> return Nothing
		Right r -> return $ Just r

{-|
Create a list of IO tasks querying an RBL entry (only 'A' records) on
different lists.
-}
buildRblSimpleLookups :: [T.Text] -> ResolvSeed -> T.Text -> [IO (Maybe (T.Text, [IPv4]))]
buildRblSimpleLookups lists rs domain = entry `seq` map go lists
	where
	entry = domainToRBLName domain
	go list = do
		aResult <- runLookup lookupA rs entry list
		return $ aResult >>= \r' -> return (list, r')

{-|
Create a list of IO tasks querying an RBL entry on different lists. If at
least one A record is found in a list it also tries to lookup the first TXT
record.
-}
buildRblLookups :: [T.Text] -> ResolvSeed -> T.Text -> [IO (Maybe (T.Text, [IPv4], Maybe T.Text))]
buildRblLookups lists rs domain = entry `seq` map go lists
	where
	entry = domainToRBLName domain
	go list = do
		aResult <- runLookup lookupA rs entry list
		case aResult of
			Nothing -> return Nothing
			Just l -> do
				txtResult <- runLookup lookupTXT rs entry list
				let reason = maybe Nothing (Just . decodeUtf8 . head) txtResult
				return $ Just (list, l, reason)

{-|
Lookup an address or hostname in multiple RBL lists (in parallel) and return
all hits, including the reason (from the first TXT record) if possible.
-}
rblLookup :: [T.Text] -> ResolvSeed -> T.Text -> IO [(T.Text, [IPv4], Maybe T.Text)]
rblLookup lists rs domain = do
	results <- mapConcurrently id $ buildRblLookups lists rs domain
	return $ catMaybes results

{-|
Lookup an address or hostname in multiple RBL lists (in parallel) and return
(in list order) the first hit, including the reason (from the first TXT
record) if possible.
Also cancel requests in the list after the first hit.
-}
rblLookupFirst :: [T.Text] -> ResolvSeed -> T.Text -> IO (Maybe (T.Text, [IPv4], Maybe T.Text))
rblLookupFirst lists rs domain = mapMaybeFirst $ buildRblLookups lists rs domain

{-|
Lookup an address or hostname in multiple RBL lists (in parallel) and return
all list names that returned a hit.
-}
rblLookupHits :: [T.Text] -> ResolvSeed -> T.Text -> IO [T.Text]
rblLookupHits lists rs domain = do
	results <- mapConcurrently id $ buildRblSimpleLookups lists rs domain
	return $ map fst $ catMaybes results
