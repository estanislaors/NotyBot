module Main where
import Control.Exception.Base
import Control.Monad.Except
import Data.Aeson (encode)
import qualified Data.Text as T
import Network.Linklater
import Network.Wai.Handler.Warp (run)
import Text.Printf
import Reminders
import SlackConfig

attendCmd:: Command -> IO T.Text
attendCmd command = do
  printf "< Incoming command: %s\n" $ show command
  fstRep <- fastReply command
  config <- webHook
  printf "> Outgoing message: %s\n" $ show (encode fstRep)
  eitherFstRep <- runExceptT $ say fstRep config
  case eitherFstRep of
    (Left e) -> do printf "!! Exception: %s\n" $ showRequestError e
                   return T.empty
    (Right _) -> do {manageCmd command; return T.empty}

main :: IO ()
main = do
    printf "+ Initializing saved active reminders..\n"
    initSavedReminders
    printf "+ Listening on port %s\n" $ show port
    run port (slashSimple attendCmd)
      where
        port = 3000
