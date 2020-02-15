module BasicDB where
import qualified Database.MongoDB as M
import qualified Data.Text as T
import qualified Data.Bson as B

--- Defines ---

serverAddr :: String
serverAddr = "127.0.0.1"

dbName :: T.Text
dbName = T.pack "notybot"

--- Database API ---

open :: IO M.Pipe
open = do 
    pipe <- M.connect $ M.host serverAddr
    return $ pipe

close :: M.Pipe -> IO ()
close pipe = M.close pipe

run :: M.Pipe -> M.Action IO a -> IO a
run pipe act = do
    exec <- M.access pipe M.master dbName act
    return exec

insert :: M.Pipe -> M.Collection -> B.Document -> IO B.Value
insert pipe collection document = do
    exec <- run pipe $ M.insert collection document
    return $ exec

insertMany :: M.Pipe -> M.Collection -> [B.Document] -> IO [B.Value]
insertMany pipe collection documents = do 
    exec <- run pipe $ M.insertMany collection documents
    return $ exec

count :: M.Pipe -> M.Selector -> M.Collection -> IO Int
count pipe fields collection = do 
    exec <- run pipe $ M.count $ M.select fields collection
    return $ exec

save :: M.Pipe -> M.Collection -> M.Selector -> IO ()
save pipe collection fields = do
    exec <- run pipe $ M.save collection fields
    return $ exec
    
delete :: M.Pipe -> M.Collection -> M.Selector -> IO ()
delete pipe collection fields  = do 
    exec <- run pipe $ M.delete $ M.select fields collection
    return $ exec

findOne :: M.Pipe -> M.Collection -> M.Selector -> IO (Maybe B.Document)
findOne pipe collection fields = do 
    exec <- run pipe $ M.findOne $ M.select fields collection
    return $ exec

findAll :: M.Pipe -> M.Collection -> M.Selector -> IO [B.Document]
findAll pipe collection fields = do 
    exec <- run pipe $ M.find (M.select fields collection) >>= M.rest
    return $ exec
