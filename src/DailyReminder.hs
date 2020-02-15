module DailyReminder where
import qualified Data.Text as T
import Network.Linklater.Types
import Text.Printf
import Parsing
import Data.Time
import NotyTypes
import ReminderTypes
import ReminderDatetime
import ReminderUtils


parseDaily :: T.Text ->  User -> Channel -> IO (Either NotyRespRem NotyReqRem)
parseDaily arg user channel = 
  case parse daily $ T.unpack arg of
    [(req,y)] -> return $ Right $ NReq "daily" user channel $ Just req
    _ -> return $ Left $ NFail "daily" RemEmpty user channel $ texrror user "bad op"

verifyDaily :: (Either NotyRespRem NotyReqRem) -> IO (Either NotyRespRem NotyEntryRem)
verifyDaily (Right (NReq cmd usr ch (Just (RemReq RemAdd (Just (Rmd id ldt (Just it))))))) =
  let step       = freq it
      times      = occurrences it
      stepInSecs = stepToSecs step
  in case ldt of
      Nothing  -> do  now <- getCurrentTime
                      let nextUtc = addUTCTime stepInSecs now
                      return $ Right $ NEntry cmd RemAdd usr ch (Just (Rmd id ldt (Just it))) (Just nextUtc) (Just stepInSecs) (Just times) 0 Nothing
      (Just x) -> do  now <- getCurrentLocalTime
                      nowUtc <- getCurrentTime
                      tz  <- getCurrentTimeZone
                      let time = mytime x
                      let h = hour time
                      let m = minute time
                      let timeOfDay = (TimeOfDay h m 0)
                      let nextLocal = LocalTime (localDay now) timeOfDay
                      let nextUtc   = localTimeToUTC tz nextLocal
                      let secsTo    = diffUTCTime nextUtc nowUtc 
                      let nextUtc'  = if secsTo < 0 then (addUTCTime nominalDay nextUtc) else nextUtc
                      if checkHour(h) && checkMinute(m) 
                      then return $ Right $ NEntry cmd RemAdd usr ch (Just (Rmd id ldt (Just it))) (Just nextUtc') (Just stepInSecs) (Just times) 0 Nothing
                      else return $ Left $ NFail cmd RemAdd usr ch $ texrror usr "invalid time"
verifyDaily (Right (NReq cmd usr ch (Just (RemReq RemDel (Just (Rmd id ldt it)))))) = return (Right (NEntry cmd RemDel usr ch (Just (Rmd id ldt it)) Nothing Nothing Nothing 0 Nothing))
verifyDaily (Right (NReq cmd usr ch (Just (RemReq RemList Nothing)))) = return (Right (NEntry cmd RemList usr ch Nothing Nothing Nothing Nothing 0 Nothing))
verifyDaily (Left resp) = return (Left resp)