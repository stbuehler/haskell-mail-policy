{-# LANGUAGE OverloadedStrings #-}

module Network.Policy.Parser
	( parseRequest
	, parseResponse
	) where

import Network.Policy.Types
import Control.Applicative
import qualified Data.Attoparsec as A
import Data.Attoparsec.ByteString.Char8 (stringCI, space, decimal)
import Data.Word (Word8)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

parseRequest :: A.Parser PolicyRequestData
parseRequest = do
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

parseResponse :: A.Parser PolicyResult
parseResponse = do
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

	parseAction :: A.Parser PolicyResult
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
