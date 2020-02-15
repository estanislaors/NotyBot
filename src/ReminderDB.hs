module ReminderDB where
import qualified Database.MongoDB as M
import qualified Data.Text as T
import qualified Data.Bson as B
import qualified BasicDB as D
import Text.Printf

--- Defines ---

reminders :: IO T.Text
reminders = return $ T.pack "reminders"

--- Reminder API ---

deleteReminders :: M.Selector -> IO ()
deleteReminders fields = do
    pipe <- D.open
    coll <- reminders
    D.delete pipe coll fields
    return ()

getReminders :: M.Selector -> IO [B.Document]
getReminders fields = do
    pipe <- D.open
    coll <- reminders
    list <- D.findAll pipe coll fields
    D.close pipe
    return list

saveReminderDoc :: B.Document -> IO ()
saveReminderDoc doc = do 
    pipe  <- D.open
    coll  <- reminders
    D.save pipe coll doc
    D.close pipe
    return  ()

setReminderField :: M.Selector -> T.Text -> B.Value-> IO Bool
setReminderField document field value = do
    pipe <- D.open
    coll <- reminders
    mDoc <- D.findOne pipe coll document
    case mDoc of
      (Just doc) -> do let doc' = B.merge [field B.:= value] doc
                       saveReminderDoc doc'
                       return True
      _          -> return False
