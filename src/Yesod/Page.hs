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
  )
where

import Control.Monad (guard)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid (getLast, getSum)
import qualified Data.Monoid as Monoid
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Yesod.Core
  (HandlerSite, MonadHandler, RenderRoute, invalidArgs, lookupGetParam)
import Yesod.Page.RenderedRoute

withPage
  :: ( MonadHandler m
     , ToJSON position
     , FromJSON position
     , RenderRoute (HandlerSite m)
     )
  => (a -> position)
  -- ^ How to get an item's position
  --
  -- For example, this would be @'entityKey'@ for paginated @'Entity'@ values.
  --
  -> (Cursor position -> m [a])
  -- ^ How to fetch one page of data at the given @'Cursor'@
  -> m (Page a)
withPage makePosition fetchItems = do
  cursor <- parseCursorParams
  items <- fetchItems cursor

  let (len, mLast) = getLengthAndLast items

  pure Page
    { pageData = items
    , pageFirst = cursorRouteAtPosition cursor First
    , pageNext = do
      guard . not $ null items || maybe False (len <) (cursorLimit cursor)
      cursorRouteAtPosition cursor . Next . makePosition <$> mLast
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

cursorRouteAtPosition
  :: ToJSON position => Cursor position -> Position position -> RenderedRoute
cursorRouteAtPosition cursor = \case
  First -> withPosition Nothing
  Next p -> withPosition $ Just $ encodeText p
 where
  withPosition mPosition =
    updateQueryParameter "position" mPosition $ cursorRoute cursor

parseCursorParams
  :: (MonadHandler m, FromJSON position, RenderRoute (HandlerSite m))
  => m (Cursor position)
parseCursorParams = do
  mePosition <- fmap eitherDecodeText <$> lookupGetParam "position"
  position <- case mePosition of
    Nothing -> pure First
    Just (Left err) -> invalidArgs [pack err]
    Just (Right p) -> pure $ Next p

  -- TODO: limit is a simple number always; do we need FromJSON?
  mLimit <- (decodeText =<<) <$> lookupGetParam "limit"
  renderedRoute <- getRenderedRoute

  pure $ Cursor renderedRoute position mLimit

eitherDecodeText :: FromJSON a => Text -> Either String a
eitherDecodeText = eitherDecode . BSL.fromStrict . encodeUtf8

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . BSL.fromStrict . encodeUtf8

encodeText :: ToJSON a => a -> Text
encodeText = decodeUtf8 . BSL.toStrict . encode

getLengthAndLast :: [a] -> (Int, Maybe a)
getLengthAndLast xs = unwrap $ foldMap wrap xs
 where
  wrap x = (1, Monoid.Last $ Just x)
  unwrap (s, l) = (getSum s, getLast l)
