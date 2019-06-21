{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main, Widget, SomeAssignmentId, resourcesSimple) where

import Control.Lens (preview, (^..))
import Control.Monad.Logger (MonadLogger, NoLoggingT, runNoLoggingT)
import Control.Monad.Reader (ReaderT, liftIO, replicateM_, runReaderT)
import Data.Aeson (ToJSON, Value, defaultOptions, genericToJSON, toJSON)
import Data.Aeson.Lens (key, _Array, _Number, _String)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text, unpack)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
  ( Filter
  , SelectOpt(Asc, LimitTo)
  , deleteWhere
  , insert
  , keyValueEntityToJSON
  , persistIdField
  , selectList
  , (==.)
  , (>.)
  )
import Database.Persist.Sql (SqlBackend, runMigration)
import Database.Persist.Sqlite (withSqliteConn)
import Database.Persist.TH
  (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Network.Wai.Test (simpleBody)
import Test.Hspec (hspec, shouldBe)
import Yesod
  ( MonadUnliftIO
  , Yesod
  , YesodPersist
  , YesodPersistBackend
  , mkYesod
  , parseRoutes
  , renderRoute
  , returnJson
  , runDB
  )
import Yesod.Page
import qualified Yesod.Page.QueryParam as Param
import Yesod.Test

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SomeAssignment
  teacherId Int
  courseId Int
  createdAt UTCTime
  deriving (Generic)
|]

instance ToJSON SomeAssignment where
  toJSON = genericToJSON defaultOptions

data Simple = Simple

instance Yesod Simple

runDB' :: (MonadUnliftIO m, MonadLogger m) => ReaderT SqlBackend m a -> m a
runDB' f = withSqliteConn ":test:" $ runReaderT f

instance YesodPersist Simple where
  type YesodPersistBackend Simple = SqlBackend
  runDB = runDB'

mkYesod "Simple" [parseRoutes|
/some-route SomeR GET
|]

getSomeR :: Handler Value
getSomeR = do
  let
    parseParams =
      (,) <$> Param.required "teacherId" <*> Param.optional "courseId"
  page <- withPage entityPage parseParams $ \Cursor {..} -> do
    let (teacherId, mCourseId) = cursorParams
    runDB $ selectList
      (catMaybes
        [ Just $ SomeAssignmentTeacherId ==. teacherId
        , (SomeAssignmentCourseId ==.) <$> mCourseId
        , whereClause cursorPosition
        ]
      )
    [LimitTo $ fromMaybe 100 cursorLimit, Asc persistIdField]
  returnJson $ keyValueEntityToJSON <$> page
 where
  whereClause = \case
    First -> Nothing
    Next p -> Just $ persistIdField >. p

main :: IO ()
main = do
  runNoLoggingT . runDB' $ runMigration migrateAll
  hspec . yesodSpec Simple $ ydescribe "Cursor" $ do
    yit "returns no cursor when there are no items" $ do
      runNoLoggingT . runDB' $ deleteAssignments
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
      mNext <- mayLink "next"
      liftIO $ mNext `shouldBe` Nothing

    yit "traverses a list with a next Cursor" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 12 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "4"
      assertKeys [1, 2, 3, 4]
      get =<< getLink "next"
      assertKeys [5, 6, 7, 8]
      get =<< getLink "next"
      assertKeys [9, 10, 11, 12]

    yit "finds a null next when no items are left" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 2 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "3"
      assertKeys [1, 2]
      mNext <- mayLink "next"
      liftIO $ mNext `shouldBe` Nothing

    yit "returns the same response for the same cursor" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 5 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "2"
      assertKeys [1, 2]
      next <- getLink "next"
      let
        go = do
          get next
          assertKeys [3, 4]
          getBody
      response1 <- go
      response2 <- go
      liftIO $ response1 `shouldBe` response2

    yit "limits are optional" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 5 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
      _next <- getLink "next"
      assertKeys [1, 2, 3, 4, 5]

    yit "parses optional params" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        _ <- insert $ SomeAssignment 1 3 now
        replicateM_ 5 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "courseId" "3"
      _next <- getLink "next"
      assertKeys [1]

    yit "can link to first" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 6 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "2"
      assertKeys [1, 2]
      get =<< getLink "next"
      assertKeys [3, 4]
      get =<< getLink "next"
      assertKeys [5, 6]
      get =<< getLink "first"
      assertKeys [1, 2]

deleteAssignments
  :: ReaderT SqlBackend (NoLoggingT (SIO (YesodExampleData Simple))) ()
deleteAssignments = deleteWhere ([] :: [Filter SomeAssignment])

assertKeys :: HasCallStack => [Scientific] -> SIO (YesodExampleData site) ()
assertKeys expectedKeys = do
  statusIs 200
  keys <- getDataKeys
  liftIO $ keys `shouldBe` expectedKeys

getLink :: Text -> SIO (YesodExampleData site) Text
getLink rel = fromMaybe (error $ "no " <> unpack rel) <$> mayLink rel

mayLink :: Text -> YesodExample site (Maybe Text)
mayLink rel = withResponse $ pure . preview (key rel . _String) . simpleBody

getBody :: YesodExample site ByteString
getBody = withResponse $ pure . simpleBody

getDataKeys :: YesodExample site [Scientific]
getDataKeys =
  withResponse
    $ pure
    . (^.. (key "data" . _Array . traverse . key "key" . _Number))
    . simpleBody
