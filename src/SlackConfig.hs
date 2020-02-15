module SlackConfig
    ( webHook,
    showRequestError,
    fastReply
    ) where

{-
Brief: Encapsulate slack app config for communication
NOTE: save incoming web hook URL into "hook" file at repo's root path
-}

import qualified Data.Text as T
import Network.Linklater
import Network.Linklater.Types

firstResp :: String -> T.Text
firstResp user = T.pack $  "Ok " ++ user ++ ", go that.."

fastReply :: Command -> IO Message
fastReply (Command cmd (User user) channel arg) = return (SimpleMessage (EmojiIcon T.empty) user channel (firstResp $ T.unpack user))


showRequestError :: RequestError -> String
showRequestError e = let e1 = _requestErrorException e
                         e2 = _requestErrorResponse e
                         e3 = _requestErrorDecoding e
                    in  case e1 of
                        (Just e') -> show e'
                        _ -> case e2 of
                                   (Just e') -> show e'
                                   _ -> case e3 of
                                        (Just e') -> show e'
                                        _ -> show "Nothing"
                        

readSlackFile :: FilePath -> IO T.Text
readSlackFile filename = T.filter (/= '\n') . T.pack <$> Prelude.readFile filename

-- get slack webhook URL
webHook :: IO Network.Linklater.Config
webHook = Config <$> (readSlackFile "hook")