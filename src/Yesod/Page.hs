{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Yesod.Page
  ( withPage
  , Page(..)
  , Cursor(..)
  -- * Configuration
  , PageConfig(..)
  , entityPage
  )
where

import Control.Monad (guard)
import Data.Aeson
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(Last, getLast), getSum)
import Data.Text (Text, intercalate, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.Persist
import UnliftIO (throwString)
import Yesod.Core
  ( HandlerSite
  , MonadHandler
  , RenderRoute
  , Route
  , getCurrentRoute
  , invalidArgs
  , lookupGetParam
  , renderRoute
  )
import Yesod.Page.QueryParam.Internal

-- | Configuration for an unsorted persistent Entity page
entityPage :: PageConfig (Entity a) (Key a)
entityPage = PageConfig Nothing entityKey

-- | Page parsing and encoding
--
-- `withPage` wraps a parser and handlers query param parsing and encoding of
-- paginated requests/responses.
--
withPage
  :: ( MonadHandler m
     , ToJSON position
     , FromJSON position
     , ToJSON params
     , FromJSON params
     , RenderRoute (HandlerSite m)
     )
  => PageConfig a position
  -> ParseParamM params -- ^ Query param parser
  -> (Cursor params position -> m [a]) -- ^ Handler
  -> m (Page a)
withPage pageConfig parse go = do
  (cursor, with) <- getPaginated pageConfig parse
  with <$> go cursor

data PageConfig a position = PageConfig
  { baseDomain :: Maybe Text
  , makePosition :: a -> position
  }

data Page a = Page
  { pageData :: [a]
  , pageNext :: Maybe (Cursor Value Value)
  }
  deriving (Functor)

instance ToJSON a => ToJSON (Page a) where
  toJSON p = object
    [ "data" .= pageData p
    , "next" .= pageNext p
    ]

-- | An encoding of the position in a page
--
-- A Cursor encodes all necessary information to determine the position in a
-- specific page.
--
data Cursor params position = Cursor
  { cursorPath :: Text -- ^ The path of the parsed request
  , cursorParams :: params -- ^ Query parameters passed from the request
  , cursorLastPosition :: Maybe position -- ^ The last position seen by the endpoint consumer
  , cursorLimit :: Maybe Int -- ^ The page size requested by the endpoint consumer
  }

instance ToJSON (Cursor Value Value) where
  toJSON c = do
    let next = decodeUtf8 . Base64.encode . BSL.toStrict . encode $ object
          [ "path" .= cursorPath c
          , "params" .= cursorParams c
          , "lastPosition" .= cursorLastPosition c
          , "limit" .= cursorLimit c
          ]
    toJSON $ cursorPath c <> "?next=" <> next

instance (FromJSON a, FromJSON b) => FromJSON (Cursor a b) where
  parseJSON = withText "Cursor" $ \t ->
    case Base64.decode $ encodeUtf8 t of
      Left err -> fail err
      Right rawJson -> case eitherDecode $ BSL.fromStrict rawJson of
        Left err -> fail err
        Right value -> withObject "Cursor" parseCursor value
   where
    parseCursor o = Cursor
      <$> o .: "path"
      <*> o .: "params"
      <*> (Just <$> o .: "lastPosition")
      <*> (o .:? "limit")

getPaginated
  :: ( MonadHandler m
     , ToJSON position
     , FromJSON position
     , ToJSON params
     , FromJSON params
     , RenderRoute (HandlerSite m)
     )
  => PageConfig a position
  -> ParseParamM params
  -> m (Cursor params position, [a] -> Page a)
getPaginated pageConfig parser = do
  route <- maybe (throwString "no route") pure =<< getCurrentRoute
  cursor <- runParseParams pageConfig route parser
  pure (cursor, withCursor pageConfig cursor)

withCursor
  :: (ToJSON params, ToJSON position)
  => PageConfig a position
  -> Cursor params position
  -> [a]
  -> Page a
withCursor pageConfig cursor items = Page
  { pageData = items
  , pageNext = do
    guard . not $ null items || maybe False (len <) (cursorLimit cursor)
    Just $ Cursor
      { cursorPath = cursorPath cursor
      , cursorParams = toJSON $ cursorParams cursor
      , cursorLastPosition = Just $ toJSON mLastId
      , cursorLimit = cursorLimit cursor
      }
  }
 where
  (mLastId, len) = unwrap $ foldMap wrap items
  wrap = (, 1) . Last . Just . makePosition pageConfig
  unwrap = bimap getLast getSum

runParseParams
  :: (MonadHandler m, FromJSON params, FromJSON position, RenderRoute r)
  => PageConfig a position
  -> Route r
  -> ParseParamM params
  -> m (Cursor params position)
runParseParams pageConfig route f = lookupGetParam "next" >>= \case
  Nothing -> do
    params <- parseParams f
    limit <- (decodeText =<<) <$> lookupGetParam "limit"
    pure $ Cursor path params Nothing limit
  Just next -> case eitherDecodeText $ "\"" <> next <> "\"" of
    Left err -> invalidArgs [pack err]
    Right cursor -> pure cursor
 where
  path =
    fromMaybe "" (baseDomain pageConfig)
      <> "/"
      <> (intercalate "/" . fst $ renderRoute route)

eitherDecodeText :: FromJSON a => Text -> Either String a
eitherDecodeText = eitherDecode . BSL.fromStrict . encodeUtf8

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . BSL.fromStrict . encodeUtf8
