{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yesod.Page
  ( withPage
  , Page(..)
  , Cursor(..)
  , Position(..)
  -- * Configuration
  , PageConfig(..)
  , entityPage
  )
where

import Control.Monad (guard)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid (getLast, getSum)
import qualified Data.Monoid as Monoid
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.Persist
import Yesod.Core
  (HandlerSite, MonadHandler, RenderRoute, invalidArgs, lookupGetParam)
import Yesod.Page.RenderedRoute

-- | Configuration for an unsorted persistent Entity page
entityPage :: PageConfig (Entity a) (Key a)
entityPage = PageConfig Nothing entityKey

withPage
  :: ( MonadHandler m
     , ToJSON position
     , FromJSON position
     , RenderRoute (HandlerSite m)
     )
  => PageConfig a position
  -> (Cursor position -> m [a]) -- ^ Handler
  -> m (Page a)
withPage pageConfig go = do
  (cursor, with) <- getPaginated pageConfig
  with <$> go cursor

data PageConfig a position = PageConfig
  { baseDomain :: Maybe Text
  , makePosition :: a -> position
  }

data Page a = Page
  { pageData :: [a]
  , pageFirst :: RenderedRoute
  , pageNext :: Maybe RenderedRoute
  }
  deriving (Functor)

instance ToJSON a => ToJSON (Page a) where
  toJSON p = object
    [ "data" .= pageData p
    , "first" .= pageFirst p
    , "next" .= pageNext p
    ]

-- | An encoding of the position in a page
--
-- A Cursor encodes all necessary information to determine the position in a
-- specific page.
--
data Cursor position = Cursor
  { cursorRoute :: RenderedRoute -- ^ The route of the parsed request
  , cursorPosition :: Position position -- ^ The last position seen by the endpoint consumer
  , cursorLimit :: Maybe Int -- ^ The page size requested by the endpoint consumer
  }

data Position position = First | Next position

instance FromJSON p => FromJSON (Position p) where
  parseJSON = withObject "Position" $ \o -> do
    position <- o .: "position"
    case position :: Text of
      "first" -> pure First
      "next" -> Next <$> o .: "keySet"
      unexpected -> fail $ show unexpected

instance ToJSON p => ToJSON (Position p) where
  toJSON = \case
    First -> object ["position" .= ("first" :: Text)]
    Next p -> object ["position" .= ("next" :: Text), "keySet" .= p]

getPaginated
  :: ( MonadHandler m
     , ToJSON position
     , FromJSON position
     , RenderRoute (HandlerSite m)
     )
  => PageConfig a position
  -> m (Cursor position, [a] -> Page a)
getPaginated pageConfig = do
  cursor <- runParseParams pageConfig
  pure (cursor, withCursor pageConfig cursor)

withCursor
  :: (ToJSON position)
  => PageConfig a position
  -> Cursor position
  -> [a]
  -> Page a
withCursor pageConfig cursor items = Page
  { pageData = items
  , pageFirst = makeRoute $ nextParameter First
  , pageNext = do
    guard . not $ null items || maybe False (len <) (cursorLimit cursor)
    makeRoute . nextParameter . Next <$> mLastId
  }
 where
  (len, mLastId) = unwrap $ foldMap wrap items
  wrap x = (1, Monoid.Last . Just $ makePosition pageConfig x)
  unwrap (s, l) = (getSum s, getLast l)
  makeRoute mNext = updateQueryParameter "next" mNext $ cursorRoute cursor
  nextParameter = \case
    First -> Nothing
    Next p -> Just $ encodeText p

runParseParams
  :: (MonadHandler m, FromJSON position, RenderRoute (HandlerSite m))
  => PageConfig a position
  -> m (Cursor position)
runParseParams pageConfig = do
  meNext <- fmap eitherDecodeText <$> lookupGetParam "next"
  position <- case meNext of
    Nothing -> pure First
    Just (Left err) -> invalidArgs [pack err]
    Just (Right p) -> pure $ Next p

  -- TODO: limit is a simple number always; do we need FromJSON?
  mLimit <- (decodeText =<<) <$> lookupGetParam "limit"
  renderedRoute <- getRenderedRoute $ baseDomain pageConfig

  pure $ Cursor renderedRoute position mLimit

eitherDecodeText :: FromJSON a => Text -> Either String a
eitherDecodeText = eitherDecode . BSL.fromStrict . encodeUtf8

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . BSL.fromStrict . encodeUtf8

encodeText :: ToJSON a => a -> Text
encodeText = decodeUtf8 . BSL.toStrict . encode
