{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Policy.Action
-- Copyright   :  (c) 2014 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
--
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- Type for the policy "action=..." response data.
--
-----------------------------------------------------------------------------

module Network.Policy.Action
	( PolicyAction(..)
	, policyActionText
	) where

import qualified Data.Text as T

{-|
Action to return as response to a policy request; for basic actions see
<http://www.postfix.org/access.5.html>. To execute complex actions in postfix
use "smtpd_restriction_classes" and return the name of your class.
-}
data PolicyAction
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

-- "debug" show strings, hinting which constructor was used
instance Show PolicyAction where
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

{-|
Build action string from 'PolicyAction' (the value to return in the "action"
response parameter).
-}
policyActionText :: Monad m => PolicyAction -> m T.Text
policyActionText (Policy_Accept             ) = return "OK"
policyActionText (Policy_Accept_Num code    ) = if code >= 200 && code <= 299 then return $ T.pack $ show code else fail $ "Invalid code for OK: " ++ show code
policyActionText (Policy_Defer           msg) = return $ T.concat $ "DEFER" : if T.null msg then [] else [" ", msg]
policyActionText (Policy_Defer_Num  code msg) =
	if code >= 400 && code <= 499
		then if not (T.null msg)
			then return $ T.concat $ [T.pack (show code), " ", msg]
			else fail $ "Empty message for defer code"
		else fail $ "Invalid code for DEFER: " ++ show code
policyActionText (Policy_Reject          msg) = return $ T.concat $ "REJECT" : if T.null msg then [] else [" ", msg]
policyActionText (Policy_Reject_Num code msg) =
	if code >= 500 && code <= 599
		then if not (T.null msg)
			then return $ T.concat $ [T.pack (show code), " ", msg]
			else fail $ "Empty message for reject code"
		else fail $ "Invalid code for REJECT: " ++ show code
policyActionText (Policy_Defer_If_Reject msg) = return $ T.concat $ "DEFER_IF_REJECT" : if T.null msg then [] else [" ", msg]
policyActionText (Policy_Defer_If_Permit msg) = return $ T.concat $ "DEFER_IF_PERMIT" : if T.null msg then [] else [" ", msg]
policyActionText (Policy_Pass               ) = return $ "DUNNO"
policyActionText (Policy_RAW             raw) = return $ raw
