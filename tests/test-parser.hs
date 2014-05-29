{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import qualified Data.Text as T
import Network.Policy
import Network.Policy.Serialize
import qualified Data.Attoparsec as A
import Data.Text.Encoding (encodeUtf8)

test_response_parser :: T.Text -> PolicyAction -> IO ()
test_response_parser s r = do
	case A.parseOnly parsePolicyAction $ encodeUtf8 s of
		Left errmsg -> fail $ "parsing " ++ show s ++ ": " ++ errmsg
		Right r' -> when (r /= r') $ fail $ "parsing " ++ show s ++ " => '" ++ show r' ++ "', expected '" ++ show r ++ "'"
	msg <- formatPolicyAction r
	case A.parseOnly parsePolicyAction msg of
		Left errmsg -> fail $ "parsing " ++ show msg ++ ": " ++ errmsg
		Right r' -> when (r /= r') $ fail $ "serialized '" ++ show r ++ "' => " ++ show msg ++ " and parsed again => '" ++ show r' ++ "'"


test_responses :: [(T.Text, PolicyAction)]
test_responses =
	[ ("action=OK\n\n", Policy_Accept)
	, ("action=DUNNO\n\n", Policy_Pass)
	, ("action=215\n\n", Policy_Accept_Num 215)
	, ("action=DEFER\n\n", Policy_Defer "")
	, ("action=DEFER \n\n", Policy_Defer "")
	, ("action=DEFER come back later\n\n", Policy_Defer "come back later")
	, ("action=450 come back later\n\n", Policy_Defer_Num 450 "come back later")
	, ("action=DEFER_IF_REJECT\n\n", Policy_Defer_If_Reject "")
	, ("action=DEFER_IF_REJECT \n\n", Policy_Defer_If_Reject "")
	, ("action=DEFER_IF_REJECT come back later\n\n", Policy_Defer_If_Reject "come back later")
	, ("action=DEFER_IF_PERMIT\n\n", Policy_Defer_If_Permit "")
	, ("action=DEFER_IF_PERMIT \n\n", Policy_Defer_If_Permit "")
	, ("action=DEFER_IF_PERMIT go away!\n\n", Policy_Defer_If_Permit "go away!")
	, ("action=REJECT\n\n", Policy_Reject "")
	, ("action=REJECT \n\n", Policy_Reject "")
	, ("action=REJECT go away!\n\n", Policy_Reject "go away!")
	, ("action=554 go away!\n\n", Policy_Reject_Num 554 "go away!")
	, ("action=some_other_actions\n\n", Policy_RAW "some_other_actions")
	]

-- import Network.Socket
-- import Control.Concurrent (forkIO)
--
-- main :: IO ()
-- main = do
-- 		(s1, s2) <- socketPair AF_UNIX Stream defaultProtocol
-- 		l <- getRootLogger
-- 		_ <- forkIO $ handleServerConnection l handle s1
-- 		r <- makeRequest' s2 $ M.fromList [("test", "abc")]
-- 		print r
-- 	where
-- 	handle :: PolicyParameters -> IO PolicyAction
-- 	handle _ = return $ Policy_RAW "550 test"

main = do
	mapM_ (uncurry test_response_parser) test_responses
