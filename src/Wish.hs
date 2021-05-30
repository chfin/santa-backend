{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Wish
  ( startApp
  , app
  , generateJS
  , writeJS
  )
where

import           Data.Aeson
import           Network.Wai.Handler.Warp       ( run )
import           Servant
import           GHC.Generics                   ( Generic )
import           Servant.JS                     ( jsForAPI
                                                , vanillaJSWith
                                                , defCommonGeneratorOptions
                                                , CommonGeneratorOptions
                                                  ( urlPrefix
                                                  )
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
                                                ( writeFile )
import           Database.SQLite.Simple
import           Data.Maybe                     ( listToMaybe )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )

-- config
dbfile :: String
dbfile = "santa.db"

-- data types
data Wish = Wish
  { wishId :: Int
  , wishContent :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Wish

instance FromRow Wish where
  fromRow = Wish <$> field <*> field

-- API
type GetWishes = "wishes" :> Get '[JSON] [Wish]
type GetWish = "wish" :> Capture "wishid" Int :> Get '[JSON] (Maybe Wish)
type DeleteWish = "wish" :> Capture "wishid" Int :> DeleteNoContent
type AddWish = "addWish" :> QueryParam "content" String :> PostNoContent
type WishesAPI = GetWishes :<|> GetWish :<|> DeleteWish :<|> AddWish
type WishesGroupAPI = Capture "group" String :> WishesAPI

type Static = "static" :> Raw
type API = WishesGroupAPI :<|> Static

-- data
initDB :: IO ()
initDB = withConnection dbfile $ \conn -> execute_
  conn
  "CREATE TABLE IF NOT EXISTS wishes (id INTEGER PRIMARY KEY, group text not null, content text not null);"

-- API implementation
getWishes :: String -> IO [Wish]
getWishes group = withConnection dbfile $ \conn ->
  query conn "SELECT * FROM wishes WHERE group IS (?);" (Only group)

getWish :: Int -> IO (Maybe Wish)
getWish id = withConnection dbfile $ \conn ->
  listToMaybe <$> query conn "SELECT * FROM wishes WHERE ID IS (?);" (Only id)

deleteWish :: Int -> IO NoContent
deleteWish id = do
  withConnection dbfile
    $ \conn -> execute conn "DELETE FROM wishes WHERE ID IS (?)" (Only id)
  pure NoContent

addWish :: String -> Maybe String -> IO NoContent
addWish group (Just content) = do
  withConnection dbfile $ \conn -> executeNamed
    conn
    "INSERT INTO wishes (group,content) VALUES (:group,:content);"
    [":group" := group, ":content" := content]
  pure NoContent
addWish _ Nothing = pure NoContent

-- server
wishesServer :: Server WishesGroupAPI
wishesServer group =
  liftIO (getWishes group)
    :<|> liftIO
    .    getWish
    :<|> liftIO
    .    deleteWish
    :<|> liftIO
    .    addWish group

server :: Server API
server = wishesServer :<|> serveDirectoryWebApp "static"

-- running stuff
wishesApi :: Proxy WishesGroupAPI
wishesApi = Proxy

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

startApp :: IO ()
startApp = do
  initDB
  run 2412 app

-- generate JS
generateJS :: T.Text -> T.Text
generateJS pfx = jsForAPI wishesApi $ vanillaJSWith $ defCommonGeneratorOptions
  { urlPrefix = pfx
  }

writeJS :: FilePath -> T.Text -> IO ()
writeJS fn pfx = T.writeFile fn $ generateJS pfx
