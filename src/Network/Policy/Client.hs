{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Policy.Client
-- Copyright   :  (c) 2014 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
--
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- Client implementation for policy requests.
--
-----------------------------------------------------------------------------

module Network.Policy.Client
	( makeRequest
	, makeRequest'
	) where

import Network.Policy.Serialize
import Network.Policy.Types
import Network.Policy.Utils

import Control.Exception
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B

{-|
Make a request to a given 'SockAddr' (creating a TCP/Unix connection to it).
-}
makeRequest :: SockAddr -> PolicyParameters -> IO PolicyAction
makeRequest addr params = do
	s <- socket (sockAddrFamily addr) Stream defaultProtocol
	finally (makeRequest' s params) (close s)

{-|
Sends a request on an already established socket and returns the response;
doesn't close the socket. Don't pipeline requests, the parser fails if it
receives data for more than one result (and breaks the second response too by
throwing away the data).
-}
makeRequest' :: Socket -> PolicyParameters -> IO PolicyAction
makeRequest' sock params = do
	raw <- formatPolicyParameters params
	sendAll sock raw
	result <- (recv sock 4096) >>= A.parseWith (recv sock 4096) parsePolicyAction
	case result of
		A.Done remainder res -> do
			if not (B.null remainder)
				then error "Received too much data"
				else return res
		A.Fail _ _ msg -> error $ "Couldn't parse response: " ++ show msg
		A.Partial _ -> error "Unexpected connection close"
