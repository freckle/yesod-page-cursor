{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yesod.Page
  ( withPageLink
  , withPage
  , Page(..)
  , Cursor(..)
  , Position(..)
  , Limit
  , unLimit
  )
where

import Control.Monad (guard)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Link (writeLinkHeader)
import Yesod.Core
  ( HandlerSite
  , MonadHandler
  , RenderRoute
  , addHeader
  , invalidArgs
  , lookupGetParam
  )
import Yesod.Page.RenderedRoute

-- | @'withPage'@ and adding pagination data to a @Link@ response header
withPageLink
  :: ( MonadHandler m
     , ToJSON position
     , FromJSON position
     , RenderRoute (HandlerSite m)
     )
  => (a -> position)
  -> (Cursor position -> m [a])
  -> m [a]
withPageLink makePosition fetchItems = do
  page <- withPage makePosition fetchItems

  let
    link = writeLinkHeader $ catMaybes
      [ Just $ renderedRouteLink "first" $ pageFirst page
      , renderedRouteLink "next" <$> pageNext page
      ]

  pageData page <$ addHeader "Link" link

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

  -- We have to fetch page-size+1 items to know if there is a next page or not
  let (Limit realLimit) = cursorLimit cursor
  items <- fetchItems cursor { cursorLimit = Limit $ realLimit + 1 }

  pure Page
    { pageData = take realLimit items
    , pageFirst = cursorRouteAtPosition cursor First
    , pageNext = do
      guard $ length items > realLimit
      pure
        $ cursorRouteAtPosition cursor
        $ Next
        $ makePosition
        $ items
        !! (realLimit - 1)
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
  , cursorLimit :: Limit -- ^ The page size requested by the endpoint consumer
  }

data Position position = First | Next position

newtype Limit = Limit { unLimit :: Int }

readLimit :: Text -> Either String Limit
readLimit t = case readMaybe @Int $ unpack t of
    Nothing -> limitMustBe "an integer"
    Just limit | limit <= 0 -> limitMustBe "positive and non-zero"
    Just limit -> Right $ Limit limit
  where
    limitMustBe msg = Left $ "Limit must be " <> msg <> ": " <> show t

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

  limit <-
    either (\e -> invalidArgs [pack e]) pure
        . readLimit
        . fromMaybe "100"
        =<< lookupGetParam "limit"

  renderedRoute <- getRenderedRoute
  pure $ Cursor renderedRoute position limit

eitherDecodeText :: FromJSON a => Text -> Either String a
eitherDecodeText = eitherDecode . BSL.fromStrict . encodeUtf8

encodeText :: ToJSON a => a -> Text
encodeText = decodeUtf8 . BSL.toStrict . encode
