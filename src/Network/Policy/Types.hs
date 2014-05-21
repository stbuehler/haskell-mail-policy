
module Network.Policy.Types
	( module Network.Policy.Result
	, PolicyRequestData
	, PolicyRequest(..)
	, PolicyHandler
	) where

import Network.Policy.Result

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B

type PolicyRequestData = M.Map T.Text T.Text
data PolicyRequest = PolicyRequest { policyRequestRaw :: B.ByteString, policyRequestData :: PolicyRequestData }

type PolicyHandler = PolicyRequest -> IO PolicyResult
