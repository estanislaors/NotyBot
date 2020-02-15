module ReminderUtils where
import qualified Data.Text as T
import Network.Linklater.Types

texrror :: User -> String -> T.Text
texrror (User name) detail = T.append name $ T.pack (", error: " ++ detail)

texresp :: User -> String -> T.Text
texresp (User name) detail = T.append name $ T.pack (": " ++ detail)