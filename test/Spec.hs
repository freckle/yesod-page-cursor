{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main, Widget, SomeAssignmentId, resourcesSimple) where

import Control.Lens (preview, (^..))
import Control.Monad.Logger (MonadLogger, NoLoggingT, runNoLoggingT)
import Control.Monad.Reader (ReaderT, liftIO, replicateM_, runReaderT)
import Data.Aeson (ToJSON, Value, defaultOptions, genericToJSON, toJSON)
import Data.Aeson.Lens (key, _Array, _Number, _String)
import Data.ByteString.Lazy (ByteString)
import Data.List (find, sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
  ( Entity(entityKey)
  , Filter
  , Key
  , SelectOpt(..)
  , deleteWhere
  , insert
  , keyValueEntityToJSON
  , persistIdField
  , selectList
  , (==.)
  , (>.)
  , (<.)
  )
import Database.Persist.Sql (SqlBackend, runMigration)
import Database.Persist.Sqlite (withSqliteConn)
import Database.Persist.TH
  (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Network.HTTP.Link
import Network.HTTP.Types.Status (status400)
import Network.Wai.Test (simpleBody, simpleHeaders)
import Test.Hspec (hspec, shouldBe)
import Yesod
  ( MonadHandler
  , MonadUnliftIO
  , Yesod
  , YesodPersist
  , YesodPersistBackend
  , lookupGetParam
  , mkYesod
  , parseRoutes
  , renderRoute
  , returnJson
  , runDB
  , sendResponseStatus
  )
import Yesod.Page
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
/some-route-link SomeLinkR GET
|]

optionalParam :: Read a => MonadHandler m => Text -> m (Maybe a)
optionalParam name = fmap (read . unpack) <$> lookupGetParam name

requireParam :: (MonadHandler m, Read a) => Text -> m a
requireParam name = maybe badRequest pure =<< optionalParam name
 where
  badRequest =
    sendResponseStatus status400 $ "A " <> name <> " parameter is required."

getSomeR :: Handler Value
getSomeR = makePaginationRoute withPage

getSomeLinkR :: Handler Value
getSomeLinkR = makePaginationRoute withPageLink

type Pagination m f a
    = (Entity a -> Key a) -> (Cursor (Key a) -> m [Entity a]) -> m (f (Entity a))

makePaginationRoute
  :: (Functor f, ToJSON (f Value))
  => Pagination Handler f SomeAssignment
  -> Handler Value
makePaginationRoute withPage' = do
  teacherId <- requireParam "teacherId"
  mCourseId <- optionalParam "courseId"

  items <- withPage' entityKey $ \Cursor {..} ->
    runDB $ sort cursorPosition <$> selectList
      (catMaybes
        [ Just $ SomeAssignmentTeacherId ==. teacherId
        , (SomeAssignmentCourseId ==.) <$> mCourseId
        , whereClause cursorPosition
        ]
      )
      [LimitTo $ unLimit cursorLimit, orderBy cursorPosition]
  returnJson $ keyValueEntityToJSON <$> items
 where
  whereClause = \case
    First -> Nothing
    Next p -> Just $ persistIdField >. p
    Previous p -> Just $ persistIdField <. p
    Last -> Nothing

  orderBy = \case
    First -> Asc persistIdField
    Next{} -> Asc persistIdField
    Previous{} -> Desc persistIdField
    Last -> Desc persistIdField

  sort = \case
    First -> id
    Next{} -> id
    Previous{} -> sortOn entityKey
    Last -> sortOn entityKey

main :: IO ()
main = do
  runNoLoggingT . runDB' $ runMigration migrateAll
  hspec . yesodSpec Simple $ ydescribe "Cursor" $ do
    yit "responds with a useful message on invalid limit" $ do
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "-1"

      statusIs 400
      bodyContains "must be positive and non-zero"

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
      assertDataKeys [1, 2, 3, 4]
      get =<< getLink "next"
      assertDataKeys [5, 6, 7, 8]
      get =<< getLink "next"
      assertDataKeys [9, 10, 11, 12]

    yit "finds a null next when no items are left" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 2 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "3"
      assertDataKeys [1, 2]
      mNext <- mayLink "next"
      liftIO $ mNext `shouldBe` Nothing

    yit "finds a null next even with limit defaulted" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 2 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
      mNext <- mayLink "next"
      liftIO $ mNext `shouldBe` Nothing

    yit "finds a null next even with page-aligned data" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 2 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "2"
      mNext <- mayLink "next"
      liftIO $ mNext `shouldBe` Nothing

    yit "finds a null next on the last page" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 2 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "2"
      get =<< getLink "last"
      mNext <- mayLink "next"
      liftIO $ mNext `shouldBe` Nothing

    yit "finds a null previous on the first page" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 2 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "2"
      mPrevious <- mayLink "previous"
      liftIO $ mPrevious `shouldBe` Nothing

    yit "returns the same response for the same cursor" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 5 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "2"
      assertDataKeys [1, 2]
      next <- getLink "next"
      let
        go = do
          get next
          assertDataKeys [3, 4]
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
      assertDataKeys [1, 2, 3, 4, 5]

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
      assertDataKeys [1]

    yit "can link to first" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 6 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "2"
      assertDataKeys [1, 2]
      get =<< getLink "next"
      assertDataKeys [3, 4]
      get =<< getLink "next"
      assertDataKeys [5, 6]
      get =<< getLink "first"
      assertDataKeys [1, 2]

    yit "can link to last" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 6 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "2"
      assertDataKeys [1, 2]
      get =<< getLink "last"
      assertDataKeys [5, 6]
      get =<< getLink "previous"
      assertDataKeys [3, 4]
      get =<< getLink "previous"
      assertDataKeys [1, 2]

    yit "can traverse via Link" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        deleteAssignments
        replicateM_ 6 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeLinkR
        addGetParam "teacherId" "1"
        addGetParam "limit" "2"
      assertKeys [1, 2]
      get =<< getLinkViaHeader "next"
      assertKeys [3, 4]
      get =<< getLinkViaHeader "next"
      assertKeys [5, 6]
      get =<< getLinkViaHeader "first"
      assertKeys [1, 2]
      get =<< getLinkViaHeader "last"
      assertKeys [5, 6]
      get =<< getLinkViaHeader "previous"
      assertKeys [3, 4]
      get =<< getLinkViaHeader "previous"
      assertKeys [1, 2]

deleteAssignments
  :: ReaderT SqlBackend (NoLoggingT (SIO (YesodExampleData Simple))) ()
deleteAssignments = deleteWhere ([] :: [Filter SomeAssignment])

assertDataKeys :: HasCallStack => [Scientific] -> SIO (YesodExampleData site) ()
assertDataKeys expectedKeys = do
  statusIs 200
  keys <- getDataKeys
  liftIO $ keys `shouldBe` expectedKeys

assertKeys :: HasCallStack => [Scientific] -> SIO (YesodExampleData site) ()
assertKeys expectedKeys = do
  statusIs 200
  keys <- getKeys
  liftIO $ keys `shouldBe` expectedKeys

getLink :: HasCallStack => Text -> SIO (YesodExampleData site) Text
getLink rel =
  fromMaybe (error $ "no " <> unpack rel <> " in JSON response") <$> mayLink rel

mayLink :: Text -> YesodExample site (Maybe Text)
mayLink rel = withResponse $ pure . preview (key rel . _String) . simpleBody

getLinkViaHeader :: HasCallStack => Text -> SIO (YesodExampleData site) Text
getLinkViaHeader rel =
  fromMaybe (error $ "no " <> unpack rel <> " in Link header") <$> mayLinkViaHeader rel

mayLinkViaHeader :: Text -> SIO (YesodExampleData site) (Maybe Text)
mayLinkViaHeader rel = withResponse $ \resp -> pure $ do
  header <- lookup "Link" $ simpleHeaders resp
  parsed <- either (const Nothing) Just $ parseLinkHeader' $ decodeUtf8 header
  link <- find (((Rel, rel) `elem`) . linkParams) parsed
  pure $ pack $ show $ href link

getBody :: YesodExample site ByteString
getBody = withResponse $ pure . simpleBody

getKeys :: YesodExample site [Scientific]
getKeys =
  withResponse
    $ pure
    . (^.. (_Array . traverse . key "key" . _Number))
    . simpleBody

getDataKeys :: YesodExample site [Scientific]
getDataKeys =
  withResponse
    $ pure
    . (^.. (key "data" . _Array . traverse . key "key" . _Number))
    . simpleBody
