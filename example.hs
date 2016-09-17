{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RankNTypes #-}

import Control.Exception
import Data.String
import Data.Text as T
import Network.Policy
import Network.Policy.Utils
import System.IO
import System.Log.Logger
import Network.DNS
import Network.DNS.RBL
import Network.Socket
import qualified Data.Map.Strict as M
import Control.Monad.Trans (MonadIO(..))

-- if client is listed on any of those, reject them right away
rblRejectLists :: [Text]
rblRejectLists =
	[ "ix.dnsbl.manitu.net"
	, "cbl.abuseat.org"
	]

-- message to reject with after a hit on rblRejectLists (similar to postfix)
blockMessage :: Monad m => Text -> Maybe Text -> PolicyHandlerT m Text
blockMessage list reason = formatMessage
	[ "Service unavailable; Client host ["
	, Format_Parameter "client_address"
	, "] blocked using "
	, Format_String list
	, Format_String $ maybe "" (\reason' -> T.append "; " reason') reason
	]

-- building the result for hit on rblRejectLists
rblRejectRespond :: Text -> Maybe Text -> PolicyHandler
rblRejectRespond list reason = return . Policy_Reject =<< blockMessage list reason

-- if client is listed on any of those forward them to postgrey
rblGreyLists :: [Text]
rblGreyLists =
	[ "dnsbl.sorbs.net"
	, "bl.spamcop.net"
	, "pbl.spamhaus.org"
	, "zen.spamhaus.org"
	]

-- tell postfix to query postgrey using a "smtpd restriction class" (that
-- needs to be defined in the postfix config)
rblGreyRespond :: Text -> Maybe Text -> PolicyHandler
rblGreyRespond _ _ = return $ Policy_RAW "check_policy_postgrey"


-- build a text message based on request data
data Format
	= Format_String Text -- simple string
	| Format_Parameter Text -- lookup parameter in data
	| Format_Conditional Text [Format] [Format] -- if parameter exists and is non empty print first otherwise second
	deriving (Eq)

-- create Format_String from string literals "..." with OverloadedStrings
instance IsString Format where
	fromString = Format_String . fromString

formatMessage' :: PolicyParameters -> [Format] -> Text
formatMessage' params = T.concat . Prelude.map fmt
	where
	fmt :: Format -> Text
	fmt (Format_String t) = t
	fmt (Format_Parameter p) = case M.lookup p params of
		Just t -> t
		Nothing -> ""
	fmt (Format_Conditional p y n) = case M.lookup p params of
		Just t -> formatMessage' params $ if T.null t then n else y
		Nothing -> formatMessage' params n

formatMessage :: Monad m => [Format] -> PolicyHandlerT m Text
formatMessage fmt = do
	params <- getParameters
	return $ formatMessage' params fmt


-- search for first hit and reason in `lists`, print "WARNING" on hits with details
-- on hit call handler `h` with list name and reason to determine result,
-- otherwise return Policy_Pass
handleRBL :: [Text] -> Logger -> ResolvSeed -> (Text -> Maybe Text -> PolicyHandler) -> PolicyHandler
handleRBL lists l rs h = do
	withParameter' "client_address" $ \addr -> do
		rbl <- liftIO $ rblLookupFirst lists rs addr
		case rbl of
			Just (list, _, reason) -> do
				liftIO . logL l WARNING . T.unpack =<< formatMessage
					[ "client ["
					, Format_Parameter "client_address"
					, "] listed on "
					, Format_String list
					, Format_String $ case reason of Nothing -> ""; Just reason' -> T.append "; " reason'
					]
				h list reason
			Nothing -> return Policy_Pass -- no hit


main :: IO ()
main = do
		hSetBuffering stderr LineBuffering
		updateGlobalLogger rootLoggerName (setLevel NOTICE)
		l <- getRootLogger

		rs <- makeResolvSeed $ defaultResolvConf { resolvTimeout = 5 * 1000 * 1000 }

		s <- catch (do
				s' <- makeListeningSocket 0
				logL l NOTICE "Listening on socket from fd 0 (stdin)"
				return s'
			) $ \(_ :: IOException) -> do
				s' <- listenLocalhostIPv4 10022
				getSocketName s' >>= \n -> logL l NOTICE ("Listening on " ++ show n)
				return s'

		go rs l s
	where
	go rs l s = acceptLoopFork s $ \c _ -> handleServerConnection l handleRequest c
		where
		handleRequest :: PolicyHandler
		handleRequest = do
			r <- handleWithFirstNonPass [handleRblReject, handleRblGreylist]
			log_prefix <- formatMessage [ "client [", Format_Parameter "client_address", "]: "]
			liftIO $ logL l NOTICE $ T.unpack log_prefix ++ show r
			return r
		handleRblReject :: PolicyHandler
		handleRblReject = handleRBL rblRejectLists l rs rblRejectRespond
		handleRblGreylist :: PolicyHandler
		handleRblGreylist = handleRBL rblGreyLists l rs rblGreyRespond
