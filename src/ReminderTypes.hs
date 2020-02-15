module ReminderTypes where
import NotyTypes

type Id = String
data Freq = Fmin Int | Fhour Int | Fday Int | Fmon Int | Fyear Int deriving (Show)
data MyIter = Iter {freq::Freq, occurrences::Int} deriving (Show)
data MyDate = Date {year::Int, month::Int, day::Int} deriving (Eq, Ord, Show)
data MyTime = Time {hour::Int, minute::Int} deriving (Eq, Ord, Show)
data MyDateTime = DateTime {mydate::MyDate,mytime::MyTime} deriving (Eq, Ord, Show)
data Reminder = Rmd Id (Maybe MyDateTime) (Maybe MyIter) deriving (Show)
data ReminderOp = RemAdd | RemDel | RemList | RemEmpty deriving (Show) 
data ReminderReq = RemReq ReminderOp (Maybe Reminder) deriving (Show)

-- NotyTypes for reminders
type NotyReqRem = NotyReq ReminderReq
type NotyEntryRem = NotyEntry ReminderOp Reminder
type NotyRespRem = NotyResp ReminderOp