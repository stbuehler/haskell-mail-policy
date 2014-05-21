{-# LANGUAGE OverloadedStrings #-}

module Network.Policy.Result
	( PolicyResult(..)
	, policyResultLine
	, policyResultMessage
	) where

import Control.Monad (when)
import qualified Data.Text as T

data PolicyResult
	= Policy_Accept -- "OK"
	| Policy_Accept_Num Int -- [200..299]
	| Policy_Defer T.Text
	| Policy_Defer_Num Int T.Text -- [400..499], string not empty
	| Policy_Reject T.Text
	| Policy_Reject_Num Int T.Text -- [500..599], string not empty
	| Policy_Defer_If_Reject T.Text
	| Policy_Defer_If_Permit T.Text
	| Policy_Pass -- "DUNNO"
	| Policy_RAW T.Text
	deriving (Eq)

instance Show PolicyResult where
	show (Policy_Accept             ) = "OK"
	show (Policy_Accept_Num code    ) = "(ok) " ++ show code
	show (Policy_Defer           msg) = "DEFER " ++ T.unpack msg
	show (Policy_Defer_Num  code msg) = "(defer) " ++ show code ++ " " ++ T.unpack msg
	show (Policy_Reject          msg) = "REJECT " ++ T.unpack msg
	show (Policy_Reject_Num code msg) = "(reject) " ++ show code ++ " " ++ T.unpack msg
	show (Policy_Defer_If_Reject msg) = "DEFER_IF_REJECT " ++ T.unpack msg
	show (Policy_Defer_If_Permit msg) = "DEFER_IF_PERMIT " ++ T.unpack msg
	show (Policy_Pass               ) = "DUNNO"
	show (Policy_RAW             raw) = "(raw) " ++ T.unpack raw


policyResultLine :: Monad m => PolicyResult -> m T.Text
policyResultLine (Policy_Accept             ) = return "OK"
policyResultLine (Policy_Accept_Num code    ) = if code >= 200 && code <= 299 then return $ T.pack $ show code else fail $ "Invalid code for OK: " ++ show code
policyResultLine (Policy_Defer           msg) = return $ T.concat $ "DEFER" : if T.null msg then [] else [" ", msg]
policyResultLine (Policy_Defer_Num  code msg) =
	if code >= 400 && code <= 499
		then if not (T.null msg)
			then return $ T.concat $ [T.pack (show code), " ", msg]
			else fail $ "Empty message for defer code"
		else fail $ "Invalid code for DEFER: " ++ show code
policyResultLine (Policy_Reject          msg) = return $ T.concat $ "REJECT" : if T.null msg then [] else [" ", msg]
policyResultLine (Policy_Reject_Num code msg) =
	if code >= 500 && code <= 599
		then if not (T.null msg)
			then return $ T.concat $ [T.pack (show code), " ", msg]
			else fail $ "Empty message for reject code"
		else fail $ "Invalid code for REJECT: " ++ show code
policyResultLine (Policy_Defer_If_Reject msg) = return $ T.concat $ "DEFER_IF_REJECT" : if T.null msg then [] else [" ", msg]
policyResultLine (Policy_Defer_If_Permit msg) = return $ T.concat $ "DEFER_IF_PERMIT" : if T.null msg then [] else [" ", msg]
policyResultLine (Policy_Pass               ) = return $ "DUNNO"
policyResultLine (Policy_RAW             raw) = return $ raw

policyResultMessage :: Monad m => PolicyResult -> m T.Text
policyResultMessage r = do
	line <- policyResultLine r
	when (T.any (\c -> '\n' == c || '\0' == c) line) $ error $ "Invalid result line: " ++ show line
	return $ T.concat ["action=", line, "\n\n"]
