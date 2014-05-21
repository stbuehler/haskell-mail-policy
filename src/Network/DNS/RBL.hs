
module Network.DNS.RBL
	( rblLookup
	, rblLookupSimple
	, rblLookupReason
	) where

import Control.Concurrent.Async
import Data.IP
import Data.List
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T
import Network.DNS
import Numeric
import Text.Read (readMaybe)


ipv4ReverseName :: IPv4 -> T.Text
ipv4ReverseName ip = let dot = T.pack "." in T.intercalate dot $ reverse $ T.splitOn dot $ T.pack $ show ip

ipv6ReverseName :: IPv6 -> T.Text
ipv6ReverseName ip = T.pack $ intersperse '.' $ concatMap w $ reverse $ fromIPv6 ip
	where
	w i = take 4 $ reverse $ "000" ++ showHex i ""

domainToRBLName :: T.Text -> T.Text
domainToRBLName domain = case readMaybe (T.unpack domain) :: Maybe IPv4 of
	Just ip -> ipv4ReverseName ip
	Nothing -> case readMaybe (T.unpack domain) :: Maybe IPv6 of
		Just ip -> ipv6ReverseName ip
		Nothing -> domain

rblLookup :: [T.Text] -> ResolvSeed -> T.Text -> IO ([Either DNSError [IPv4]])
rblLookup lists rs domain = dom `seq` mapConcurrently go lists
	where
	dom = domainToRBLName domain
	go :: T.Text -> IO (Either DNSError [IPv4])
	go list = do
		let hostname = encodeUtf8 $ T.concat [dom, T.pack ".", list]
		withResolver rs $ \resolver -> lookupA resolver hostname

-- returns lists which returned a hit
rblLookupSimple :: [T.Text] -> ResolvSeed -> T.Text -> IO [T.Text]
rblLookupSimple lists rs domain = do
	results <- rblLookup lists rs domain
	return $ map fst $ filter (\(_, r) -> either (const False) (not . null) r) $ zip lists results

rblLookupReason :: T.Text -> ResolvSeed -> T.Text -> IO (Maybe T.Text)
rblLookupReason list rs domain = do
	let dom = domainToRBLName domain
	let hostname = encodeUtf8 $ T.concat [dom, T.pack ".", list]
	res <- withResolver rs $ \resolver -> lookupTXT resolver hostname
	case res of
		Right (x:_) -> return $ Just $ decodeUtf8 x
		_ -> return Nothing
