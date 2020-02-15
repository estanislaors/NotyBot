module NotyTypes where
import qualified Data.Text as T
import Network.Linklater.Types
import Control.Concurrent
import Data.Time

type Result = T.Text
type Reason = T.Text
type Cmd    = String

-- Request to launch a reminder
data NotyReq req = NReq Cmd User Channel (Maybe req)

-- Processed Request to launch a reminder
data NotyEntry op arg = NEntry {
  cmd::Cmd,
  operator::op,
  user::User,
  channel::Channel,
  argument::(Maybe arg),
  next::(Maybe UTCTime),
  step::(Maybe NominalDiffTime),
  times::(Maybe Int),
  counter::Int,
  thread::(Maybe ThreadId)
} deriving (Show)

-- Response with info about reminder op result
data NotyResp op = NOk Cmd op User Channel Result | NFail Cmd op User Channel Reason deriving (Show)