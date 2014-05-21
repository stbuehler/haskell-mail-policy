{-# LANGUAGE OverloadedStrings #-}

module Network.Policy.Client
	( makeRequest
	, makeRequest'
	) where

import Network.Policy.Parser
import Network.Policy.Types
import Network.Policy.Utils

import Control.Exception
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Data.Text.Encoding (encodeUtf8)


formatData :: PolicyRequestData -> IO (B.ByteString)
formatData d = return $ B.concat $ concat (map (\(k, v) -> [encodeUtf8 k, B.pack [61], encodeUtf8 v, B.pack[10] ]) $ M.toList d) ++ [B.pack [10]]

makeRequest :: SockAddr -> PolicyRequestData -> IO (PolicyResult)
makeRequest addr d = do
	s <- socket (sockAddrFamily addr) Stream defaultProtocol
	finally (makeRequest' s d) (close s)

makeRequest' :: Socket -> PolicyRequestData -> IO (PolicyResult)
makeRequest' sock d = do
	raw <- formatData d
	sendAll sock raw
	result <- (recv sock 4096) >>= A.parseWith (recv sock 4096) parseResponse
	case result of
		A.Done remainder res -> do
			if not (B.null remainder)
				then error "Received too much data"
				else return res
		A.Fail _ _ msg -> error $ "Couldn't parse response: " ++ show msg
		A.Partial _ -> error "Unexpected connection close"
