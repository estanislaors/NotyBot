module EventualReminder where
import qualified Data.Text as T
import Network.Linklater.Types
import Text.Printf
import Parsing
import Data.Time
import NotyTypes
import ReminderTypes
import ReminderDatetime
import ReminderUtils

parseEventual :: T.Text ->  User -> Channel -> IO (Either NotyRespRem NotyReqRem)
parseEventual arg user channel =
  case parse eventual (T.unpack arg) of
    [(req,y)] -> return $ Right $ NReq "eventual" user channel (Just req)
    _         -> return $ Left $ NFail "eventual" RemEmpty user channel $ texrror user "bad op"


verifyEventual :: (Either NotyRespRem NotyReqRem) -> IO (Either NotyRespRem NotyEntryRem)
verifyEventual (Right (NReq cmd usr ch (Just (RemReq RemAdd (Just (Rmd id (Just dt) (Just it))))))) =
  let date        = mydate dt
      y           = year date
      mon         = month date
      d           = day date
      time        = mytime dt
      h           = hour time
      min         = minute time
      datetimeStr = (show y) ++ "-" ++ (show mon) ++ "-" ++ (show d) ++ "T" ++ (show h) ++ ":" ++ (show min)
      mDatetime   = parseTimeM True defaultTimeLocale "%Y-%-m-%-dT%-H:%-M" datetimeStr :: Maybe LocalTime
  in case mDatetime of
      Nothing    -> return $ Left $ NFail cmd RemAdd usr ch $ texrror usr "invalid date"
      (Just ldt) -> do tz <- getCurrentTimeZone
                       nowUtc <- getCurrentTime
                       let nextUtc    = localTimeToUTC tz ldt
                       let step       = freq it
                       let times      = occurrences it
                       let stepInSecs = stepToSecs(step)
                       let secsTo     = diffUTCTime nextUtc nowUtc
                       if (secsTo < 0) && (times == 0)
                       then return $ Left $ NFail cmd RemAdd usr ch $ texrror usr "past date"
                       else return $ Right $ NEntry cmd RemAdd usr ch (Just (Rmd id (Just dt) (Just it))) (Just nextUtc) (Just stepInSecs) (Just times) 0 Nothing
verifyEventual (Right (NReq cmd usr ch (Just (RemReq RemDel rem@(Just (Rmd id ldt it)))))) = return (Right (NEntry cmd RemDel usr ch rem Nothing Nothing Nothing 0 Nothing))
verifyEventual (Right (NReq cmd usr ch (Just (RemReq RemList Nothing)))) = return (Right (NEntry cmd RemList usr ch Nothing Nothing Nothing Nothing 0 Nothing))
verifyEventual (Right (NReq cmd usr ch (Just (RemReq op _)))) = return $ Left $ NFail cmd op usr ch $ texrror usr "invalid format"
verifyEventual (Left resp) = return (Left resp)