{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Control.Lens (preview, (^..))
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson.Lens (key, _Array, _Number, _String)
import Data.ByteString.Lazy (ByteString)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (getCurrentTime)
import Database.Persist (Filter, deleteWhere, insert)
import Database.Persist.Sql (SqlPersistT, runMigration)
import GHC.Stack (HasCallStack)
import Network.HTTP.Link
import Network.Wai.Test (simpleBody, simpleHeaders)
import Test.Hspec
  (Spec, SpecWith, before, beforeAll_, describe, hspec, it, shouldBe)
import TestApp
import Yesod.Test

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
  describe "Cursor" $ do
    it "responds with a useful message on invalid limit" $ do
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "-1"

      statusIs 400
      bodyContains "must be positive and non-zero"

    it "returns no cursor when there are no items" $ do
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
      mNext <- mayLink "next"
      liftIO $ mNext `shouldBe` Nothing

    it "traverses a list with a next Cursor" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
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

    it "finds a null next when no items are left" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        replicateM_ 2 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "3"
      assertDataKeys [1, 2]
      mNext <- mayLink "next"
      liftIO $ mNext `shouldBe` Nothing

    it "finds a null next even with limit defaulted" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        replicateM_ 2 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
      mNext <- mayLink "next"
      liftIO $ mNext `shouldBe` Nothing

    it "finds a null next even with page-aligned data" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        replicateM_ 2 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "2"
      mNext <- mayLink "next"
      liftIO $ mNext `shouldBe` Nothing

    it "finds a null next on the last page" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        replicateM_ 2 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "2"
      get =<< getLink "last"
      mNext <- mayLink "next"
      liftIO $ mNext `shouldBe` Nothing

    it "finds a null previous on the first page" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        replicateM_ 2 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "limit" "2"
      mPrevious <- mayLink "previous"
      liftIO $ mPrevious `shouldBe` Nothing

    it "returns the same response for the same cursor" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
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

    it "limits are optional" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        replicateM_ 5 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
      _next <- getLink "next"
      assertDataKeys [1, 2, 3, 4, 5]

    it "parses optional params" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
        _ <- insert $ SomeAssignment 1 3 now
        replicateM_ 5 . insert $ SomeAssignment 1 2 now
      request $ do
        setUrl SomeR
        addGetParam "teacherId" "1"
        addGetParam "courseId" "3"
      _next <- getLink "next"
      assertDataKeys [1]

    it "can link to first" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
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

    it "can link to last" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
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

    it "can traverse via Link" $ do
      now <- liftIO getCurrentTime
      runNoLoggingT . runDB' $ do
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

withApp :: SpecWith (TestApp Simple) -> Spec
withApp = before (testApp Simple id <$ wipeDB) . beforeAll_ setupDB

setupDB :: IO ()
setupDB = liftIO $ runNoLoggingT . runDB' $ runMigration migrateAll

wipeDB :: IO ()
wipeDB = liftIO $ runNoLoggingT . runDB' $ deleteAssignments

deleteAssignments :: MonadIO m => SqlPersistT m ()
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
  fromMaybe (error $ "no " <> unpack rel <> " in Link header")
    <$> mayLinkViaHeader rel

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
