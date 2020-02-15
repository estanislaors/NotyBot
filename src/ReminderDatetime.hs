module ReminderDatetime where
import Data.Time
import Data.Time.LocalTime
import ReminderTypes

--Change for addGregorian4MonthsClip, if add a month to day 31?

stepToSecs :: Freq -> NominalDiffTime
stepToSecs (Fmin m)   = realToFrac (60 * m)
stepToSecs (Fhour h)  = realToFrac (3600 * h)
stepToSecs (Fday d)   = realToFrac (86400 * d)
stepToSecs (Fmon m)   = realToFrac (2592000 * m)
stepToSecs (Fyear y)  = realToFrac (31104000 * y)

getCurrentLocalTime :: IO (LocalTime)
getCurrentLocalTime =
  do tz <- getCurrentTimeZone
     utc <- getCurrentTime
     return (utcToLocalTime tz utc)

currentDay :: IO(Day) -- :: (year,month,day)
currentDay = getCurrentTime >>= return . utctDay

checkMinute :: Int -> Bool
checkMinute m = (0 <= m) && (m < 60)

checkHour :: Int -> Bool
checkHour h = (0 <= h) && (h < 24)

nominalDay :: NominalDiffTime
nominalDay = stepToSecs (Fday 1)

{- CHECK MONTH AND YEAR CALCULATION

addGregorianMonthsClip moves a date a number of months forward or backward,
 adjusting it when necessary to stay within short months
 
-}