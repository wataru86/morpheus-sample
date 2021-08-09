{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Monad
import Data.Morpheus
import Data.Morpheus.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import GHC.Generics
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

data Query m = Query
  { user :: UserArgs -> m User
  } deriving (Generic, GQLType)

data User = User
  { name :: Text
  , email :: Text
  } deriving (Generic, GQLType)

data UserArgs = UserArgs
  { name :: Text
  } deriving (Generic, GQLType)

userDB :: [User]
userDB = 
  [ User "Thoru" "thoru@maidragon.com"
  , User "Kanna" "kanna@maidragon.com"
  , User "Fafnir" "fafnir@maidragon.com"
  , User "Lucoa" "lucoa@maidragon.com"
  , User "Elma" "elma@maidragon.com"
  , User "Ilulu" "ilulu@maidragon.com"
  ]

resolveUser :: UserArgs -> ResolverQ e IO User
resolveUser (UserArgs name) = maybe (fail "User not found.") return $ findUserByName name

findUserByName :: Text -> Maybe User
findUserByName name = find (\(User name' _) -> name == name') userDB

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {user = resolveUser}
    , mutationResolver = Undefined
    , subscriptionResolver = Undefined
    }

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter rootResolver

app :: Application
app request respond = do
  body <- getRequestBodyChunk request
  respond . responseLBS status200 [("Content-Type", "text/plain")] <=< gqlApi $ B.fromStrict body

main :: IO ()
main = do
  putStrLn $ "http://localhost:3000/"
  run 3000 app
