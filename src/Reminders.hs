module Reminders
    ( manageCmd,
      parseCmd,
      verifyCmd,
      resolveReq,
      responseReq,
      initSavedReminders
    ) where

import Network.Linklater
import Network.Linklater.Types
import Data.Aeson (encode)
import qualified Data.Text as T
import Control.Monad.Except
import Control.Exception.Base
import Control.Concurrent
import Data.Text.Read
import Text.Printf
import System.IO.Unsafe
import Data.Time
import Data.Time.LocalTime
import Data.Bson
---
import SlackConfig
import Parsing
import ReminderDB as DB
import ReminderTypes
import DailyReminder
import EventualReminder
import ReminderUtils
import NotyTypes

--- ASYNC ---
data Async a = Async ThreadId (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  m <- newEmptyMVar
  t <- forkFinally action (putMVar m)
  return (Async t m)
---

parseCmd :: Command -> IO (Either NotyRespRem NotyReqRem)
parseCmd (Command cmd user channel (Just arg)) =
  case T.unpack cmd of
    "daily" -> parseDaily arg user channel
    "eventual" -> parseEventual arg user channel


verifyCmd :: (Either NotyRespRem NotyReqRem) -> IO (Either NotyRespRem NotyEntryRem)
verifyCmd req@(Right (NReq cmd usr ch (Just (RemReq op rmd)))) =
 case cmd of
   "daily"   -> verifyDaily req
   "eventual" -> verifyEventual req
verifyCmd (Left resp) = return (Left resp)


resolveReq :: (Either NotyRespRem NotyEntryRem) -> IO NotyRespRem
resolveReq (Right entry) = case operator entry of
  RemAdd -> do exists <- existsReminder entry
               if exists
               then return $ NFail (cmd entry) RemAdd (user entry) (channel entry) $ texrror (user entry) "reminder name already exists"
               else do started <- launchReminder entry
                       result  <- saveReminder started
                       return result
  RemDel -> do result  <- deleteReminder entry
               return result
  RemList -> do result <- getUsrActiveReminders entry
                return result
resolveReq (Left resp) = return resp


responseReq :: NotyRespRem -> IO ()
responseReq (NOk cmd op (User usr) ch result) = do
  config <- webHook
  let reply = (SimpleMessage (EmojiIcon T.empty) usr ch result)
  printf "> Outgoing message: %s\n" $ show (encode reply)
  eResult <- runExceptT $ say reply config
  case eResult of
    (Left e) -> do {printf "!! Exeption: %s\n" $ showRequestError e; return ()}
    (Right _) -> return ()
responseReq (NFail cmd op (User usr) ch reason) = do
  config <- webHook
  let reply = (SimpleMessage (EmojiIcon T.empty) usr ch reason)
  printf "> Outgoing message: %s\n" $ show (encode reply)
  eResult <- runExceptT $ say reply config
  case eResult of
    (Left e) -> do {printf "!! Exeption: %s\n" $ showRequestError e; return ()}
    (Right _) -> return ()

relaunchReminder :: NotyEntryRem -> IO ()
relaunchReminder entry = case operator entry of
  RemAdd -> do started <- launchReminder entry
               saveReminder started
               return ()


launchReminder :: NotyEntryRem -> IO (Either NotyRespRem NotyEntryRem)
launchReminder entry@(NEntry cmd op usr ch arg (Just nextUTC) step (Just repeat) count Nothing) = do
  (Async t m) <- async (startReminder entry)
  return $ Right $ NEntry cmd op usr ch arg (Just nextUTC) step (Just repeat) count (Just t)
launchReminder (NEntry cmd op usr ch arg Nothing _ _ _ _) = return $ Left $ NFail cmd op usr ch $ texrror usr "empty datetime"
launchReminder (NEntry cmd op usr ch arg _ Nothing _ _ _) = return $ Left $ NFail cmd op usr ch $ texrror usr "empty frequency"
launchReminder (NEntry cmd op usr ch arg _ _ Nothing _ _) = return $ Left $ NFail cmd op usr ch $ texrror usr "empty repeat"


existsReminder :: NotyEntryRem -> IO Bool
existsReminder entry = do 
  list <- searchReminder entry
  case list of
    []  -> return False
    _   -> return True


searchReminder :: NotyEntryRem -> IO [Document]
searchReminder entry = do
  let (Channel chId chName)  = channel entry
  let (User usrName)         = user entry
  let (Just (Rmd rmdId _ _)) = argument entry
  let channelDoc             = [T.pack "id" =: chId
                               ,T.pack "name" =: chName]
  let userDoc                = [T.pack "name" =: usrName]
  let selectDoc = [T.pack "type" =: cmd entry
                  ,T.pack "user" =: userDoc
                  ,T.pack "channel" =: channelDoc
                  ,T.pack "name" =: rmdId
                  ,T.pack "active" =: True] :: Document
  list <- DB.getReminders selectDoc
  return list


saveReminder :: (Either NotyRespRem NotyEntryRem) -> IO NotyRespRem
saveReminder (Right entry) = do
  let (User usrName)         = user entry
  let (Channel chId chName)  = channel entry
  let (Just (Rmd rmdId _ _)) = argument entry
  let (Just thId)            = thread entry
  let nextUTC                = next entry
  let channelDoc             = [T.pack "id" =: chId
                               ,T.pack "name" =: chName]
  let userDoc                = [T.pack "name" =: usrName]
  let reminderDoc            = [T.pack "type" =: cmd entry
                                ,T.pack "user" =: userDoc
                                ,T.pack "channel" =: channelDoc
                                ,T.pack "name" =: rmdId
                                ,T.pack "next" =: nextUTC
                                ,T.pack "step" =: step entry
                                ,T.pack "repeat" =: times entry
                                ,T.pack "counter" =: counter entry
                                ,T.pack "active" =: True
                                ,T.pack "threadId" =: show thId] :: Document
  list <- searchReminder entry
  case list of
    []  -> do DB.saveReminderDoc reminderDoc
              return $ NOk (cmd entry) (operator entry) (user entry) (channel entry) $ texresp (user entry) "Done!"
    [x] -> do let reminderUpdated = merge reminderDoc x
              DB.saveReminderDoc reminderUpdated
              return $ NOk (cmd entry) (operator entry) (user entry) (channel entry) $ texresp (user entry) "Done!" 
    _   -> return $ NFail (cmd entry) (operator entry) (user entry) (channel entry) $ texrror (user entry) "Failed save, contact to admin"
saveReminder (Left resp) = return resp


deleteReminder :: NotyEntryRem -> IO NotyRespRem
deleteReminder entry = do 
  list <- searchReminder entry
  case list of
    [x] -> do DB.deleteReminders x
              return $ NOk (cmd entry) (operator entry) (user entry) (channel entry) $ texresp (user entry) "Done!"
    []  -> return $ NFail (cmd entry) (operator entry) (user entry) (channel entry) $ texrror (user entry) "Nothing to delete"
    _   -> return $ NFail (cmd entry) (operator entry) (user entry) (channel entry) $ texrror (user entry) "Failed delete, contact to admin"


startReminder :: NotyEntryRem -> IO ()
startReminder entry@(NEntry cmd op (User usr) ch (Just (Rmd name dt iter)) (Just nextUTC) (Just step) (Just repeat) count _) = do
  nowUTC   <- getCurrentTime
  let secsToRem = round $ diffUTCTime nextUTC nowUTC
  let nextUTC'   = addUTCTime step nextUTC
  threadDelay (10^6 * secsToRem)
  thId <- myThreadId
  exists <- existsReminder entry
  if exists == False
  then return ()
  else do    
    config        <- webHook
    let reply     = texresp (User usr) ("Reminder for " ++ name)
    eResult       <- runExceptT $ say (SimpleMessage (EmojiIcon T.empty) usr ch reply) config
    let userDoc   = [T.pack "name" =: usr]
    let selectDoc = [T.pack "type" =: cmd, T.pack "name" =: name, T.pack "user" =: userDoc, T.pack "active" =: True]
    let count'    = count + 1
    let times     = repeat + 1
    case eResult of
      (Left e) -> do printf "ERROR: %s\n" $ showRequestError e
                     return ()
      _        -> case (count' < times) of
                    False -> do DB.setReminderField selectDoc (T.pack "counter") (val count')
                                DB.setReminderField selectDoc (T.pack "active") (val False)
                                return ()
                    True  -> do async $ relaunchReminder $ NEntry cmd op (User usr) ch (Just (Rmd name dt iter)) (Just nextUTC') (Just step) (Just repeat) count' Nothing
                                return ()


getReminderInfo :: Document -> String
getReminderInfo doc =
    let name    = at (T.pack "name") doc :: String
        next    = at (T.pack "next") doc :: UTCTime
        step    = at (T.pack "step") doc :: NominalDiffTime
        repeat  = at (T.pack "repeat") doc :: Int
        count   = at (T.pack "counter") doc :: Int
        times   = repeat + 1
    in "> " ++ name ++ " ( " ++ show count ++ " / " ++ show times ++ " )" 


getChannel :: Document -> Channel
getChannel doc = let
  id   = at (T.pack "id") doc
  name = at (T.pack "name") doc
  in (Channel id name)


getUser :: Document -> User
getUser doc = let
  name = at (T.pack "name") doc
  in (User name)


getReminderEntry :: Document -> IO NotyEntryRem
getReminderEntry doc = do
  let channelDoc = at (T.pack "channel") doc
  let usrDoc     = at (T.pack "user") doc
  let cmd        = at (T.pack "type") doc
  let name       = at (T.pack "name") doc
  let nextUTC    = at (T.pack "next") doc
  let step       = at (T.pack "step") doc
  let repeat      = at (T.pack "repeat") doc
  let count      = at (T.pack "counter") doc
  let usr        = getUser usrDoc
  let ch         = getChannel channelDoc
  return (NEntry cmd RemAdd usr ch (Just (Rmd name Nothing Nothing)) (Just nextUTC) (Just step) (Just repeat) count Nothing)


getRemindersLine :: [Document] -> String
getRemindersLine [] = ""
getRemindersLine (x:xs) = (getReminderInfo x) ++ "\n" ++ (getRemindersLine xs)


getUsrActiveReminders ::  NotyEntryRem -> IO NotyRespRem
getUsrActiveReminders entry = do
    let (Channel chId chName)  = channel entry
    let (User usrName)         = user entry
    let channelDoc             = [T.pack "id" =: chId
                                 ,T.pack "name" =: chName]
    let userDoc                = [T.pack "name" =: usrName]
    let selectDocs             = [T.pack "type" =: cmd entry
                                 ,T.pack "user" =: userDoc
                                 ,T.pack "channel" =: channelDoc
                                 ,T.pack "active" =: True]
    list <- DB.getReminders selectDocs
    return $ NOk (cmd entry) (operator entry) (user entry) (channel entry) $ texresp (user entry) $ makeResponseList (cmd entry) $ getRemindersLine list


getAllActiveReminders :: IO [Document]
getAllActiveReminders = do
  list <- DB.getReminders [T.pack "active" =: True]
  return list


makeResponseList :: Cmd -> String -> String
makeResponseList cmd [] = "You don't have active reminders"
makeResponseList cmd xs = "Your active " ++ cmd ++ " reminders:\n" ++ xs

manageCmd :: Command -> IO ()
manageCmd cmd = do
  result0 <- parseCmd cmd
  result1 <- verifyCmd result0
  result2 <- resolveReq result1
  responseReq result2


initSavedReminders :: IO ()
initSavedReminders = do
  docList <- getAllActiveReminders
  entryList <- mapM getReminderEntry docList
  mapM relaunchReminder entryList
  return ()