{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Policy.Serialize
-- Copyright   :  (c) 2014 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
--
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- Parsing and formatting request parameters and response actions.
--
-----------------------------------------------------------------------------

module Network.Policy.Serialize
	( parsePolicyParameters
	, formatPolicyParameters
	, parsePolicyAction
	, formatPolicyAction
	) where

import Network.Policy.Handler
import Control.Applicative
import Control.Monad (when)
import Data.Attoparsec.ByteString.Char8 (stringCI, space, decimal)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word8)
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T

{-|
Parses a policy request (see <http://www.postfix.org/SMTPD_POLICY_README.html>).
Each parameter is terminated by a newline, key and value are separated by the
first '=' in a parameter, the request is terminated by another new lines, and
the parameters must not contain any NUL bytes, and neither key nor value can
contain a newline.
-}
parsePolicyParameters :: A.Parser PolicyParameters
parsePolicyParameters = do
		reqLines <- A.manyTill parseLine emptyLine
		return $ M.fromList reqLines
	where
	nul :: Word8
	nul = 0
	newline :: Word8
	newline = 10 -- fromIntegral $ fromEnum '\n'
	equals :: Word8
	equals = 61 -- fromIntegral $ fromEnum '='

	emptyLine :: A.Parser ()
	emptyLine = A.try $ A.word8 newline >> return ()

	parseLine :: A.Parser (T.Text, T.Text)
	parseLine = do
		name <- A.takeWhile1 (\b -> nul /= b && newline /= b && equals /= b)
		_ <- A.word8 equals A.<?> "require '=' after variable name"
		value <- A.takeWhile (\b -> nul /= b && newline /= b)
		_ <- A.word8 newline A.<?> "require end of line"
		return (decodeUtf8 name, decodeUtf8 value)

{-|
Build message body from parameters as "key=value" pairs, ended by newlines and
terminated by an extra newline.
-}
formatPolicyParameters :: Monad m => PolicyParameters -> m B.ByteString
formatPolicyParameters params = return $ B.concat $ concat (map (\(k, v) -> [encodeUtf8 k, B.pack [61], encodeUtf8 v, B.pack[10] ]) $ M.toList params) ++ [B.pack [10]]

{-|
Parses a policy response; the basic format is the same as for
'parsePolicyParameters', but it expects exactly one parameter with the key
"action".

It then tries to parse the value of the 'action' parameter; if it can't parse
it as some known action it will return it as 'Policy_RAW' instead.
-}
parsePolicyAction :: A.Parser PolicyAction
parsePolicyAction = do
		_ <- A.string "action=" A.<?> "expected 'action='"
		value <- A.takeWhile (\b -> nul /= b && newline /= b)
		_ <- A.word8 newline A.<?> "require end of line"
		_ <- A.word8 newline A.<?> "only one response line allowed"
		case A.parseOnly parseAction value of
			Left err -> fail err
			Right act -> return act
	where
	nul :: Word8
	nul = 0
	newline :: Word8
	newline = 10 -- fromIntegral $ fromEnum '\n'

	first :: [A.Parser x] -> A.Parser x
	first [] = fail "no patterns to match"
	first [x] = A.try x
	first (x:xs) = A.try x <|> first xs

	parseAction :: A.Parser PolicyAction
	parseAction = first $ map (\p -> p >>= \r -> A.endOfInput >> return r)
		[ stringCI "OK" >> return Policy_Accept
		, stringCI "DUNNO" >> return Policy_Pass
		, stringCI "DEFER" >> parseOptionalMessage >>= return . Policy_Defer
		, stringCI "DEFER_IF_REJECT" >> parseOptionalMessage >>= return . Policy_Defer_If_Reject
		, stringCI "DEFER_IF_PERMIT" >> parseOptionalMessage >>= return . Policy_Defer_If_Permit
		, stringCI "REJECT" >> parseOptionalMessage >>= return . Policy_Reject
		, (decimal :: A.Parser Int) >>= \code -> do
			if code >= 200 && code <= 299
				then return $ Policy_Accept_Num code
				else space >> if code >= 400 && code <= 499
					then parseMessage >>= return . Policy_Defer_Num code
					else if code >= 500 && code <= 599
						then parseMessage >>= return . Policy_Reject_Num code
						else fail "unknown code range"
		, parseMessage >>= return . Policy_RAW
		]

	parseOptionalMessage :: A.Parser T.Text
	parseOptionalMessage = A.try (space >> parseMessage) <|> return ""

	parseMessage :: A.Parser T.Text
	parseMessage = do
		A.takeByteString >>= return . decodeUtf8

{-|
Build message body from action response.
-}
formatPolicyAction :: Monad m => PolicyAction -> m B.ByteString
formatPolicyAction action = do
	line <- policyActionText action
	when (T.any (\c -> '\n' == c || '\0' == c) line) $ error $ "Invalid result line: " ++ show line
	return $ encodeUtf8 $ T.concat ["action=", line, "\n\n"]
