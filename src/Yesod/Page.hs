{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.Foldable (asum)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Link (writeLinkHeader)
import Text.Read (readMaybe)
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
  => Limit
  -- ^ Default limit if not specified in the @'Cursor'@
  -> (a -> position)
  -- ^ How to get an item's position
  --
  -- For example, this would be @'entityKey'@ for paginated @'Entity'@ values.
  --
  -> (Cursor position -> m [a])
  -- ^ How to fetch one page of data at the given @'Cursor'@
  -> m [a]
withPageLink defaultLimit makePosition fetchItems = do
  page <- withPage defaultLimit makePosition fetchItems

  let
    link = writeLinkHeader $ catMaybes
      [ Just $ renderedRouteLink "first" $ pageFirst page
      , renderedRouteLink "next" <$> pageNext page
      , renderedRouteLink "previous" <$> pagePrevious page
      , Just $ renderedRouteLink "last" $ pageLast page
      ]

  pageData page <$ addHeader "Link" link

withPage
  :: ( MonadHandler m
     , ToJSON position
     , FromJSON position
     , RenderRoute (HandlerSite m)
     )
  => Limit
  -- ^ Default limit if not specified in the @'Cursor'@
  -> (a -> position)
  -- ^ How to get an item's position
  --
  -- For example, this would be @'entityKey'@ for paginated @'Entity'@ values.
  --
  -> (Cursor position -> m [a])
  -- ^ How to fetch one page of data at the given @'Cursor'@
  -> m (Page a)
withPage defaultLimit makePosition fetchItems = do
  cursor <- parseCursorParams defaultLimit

  -- We have to fetch page-size+1 items to know if there is a next page or not
  let (Limit realLimit) = cursorLimit cursor
  items <- fetchItems cursor { cursorLimit = Limit $ realLimit + 1 }

  let
    page = case cursorPosition cursor of
      First -> take realLimit items
      Next{} -> take realLimit items
      Previous{} -> takeEnd realLimit items
      Last -> takeEnd realLimit items

    hasExtraItem = length items > realLimit

    hasNextLink = case cursorPosition cursor of
      First -> hasExtraItem
      Next{} -> hasExtraItem
      Previous{} -> True
      Last -> False

    hasPreviousLink = case cursorPosition cursor of
      First -> False
      Next{} -> True
      Previous{} -> hasExtraItem
      Last -> hasExtraItem

  pure Page
    { pageData = page
    , pageFirst = cursorRouteAtPosition cursor First
    , pageNext = do
      guard hasNextLink
      item <- lastMay page
      pure $ cursorRouteAtPosition cursor $ Next $ makePosition item
    , pagePrevious = do
      guard hasPreviousLink
      item <- headMay page
      pure $ cursorRouteAtPosition cursor $ Previous $ makePosition item
    , pageLast = cursorRouteAtPosition cursor Last
    }

data Page a = Page
  { pageData :: [a]
  , pageFirst :: RenderedRoute
  , pageNext :: Maybe RenderedRoute
  , pagePrevious :: Maybe RenderedRoute
  , pageLast :: RenderedRoute
  }
  deriving (Functor)

instance ToJSON a => ToJSON (Page a) where
  toJSON p = object
    [ "data" .= pageData p
    , "first" .= pageFirst p
    , "next" .= pageNext p
    , "previous" .= pagePrevious p
    , "last" .= pageLast p
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

data Position position
    = First
    | Next position
    | Previous position
    | Last

instance ToJSON position => ToJSON (Position position) where
  toJSON = \case
    First -> String "first"
    Next p -> object ["next" .= p]
    Previous p -> object ["previous" .= p]
    Last -> String "last"

instance FromJSON position => FromJSON (Position position) where
  parseJSON = \case
    Null -> pure First
    String t -> case t of
      "first" -> pure First
      "last" -> pure Last
      _ -> invalidPosition
    Object o -> do
      mNext <- o .:? "next"
      mPrevious <- o .:? "previous"
      maybe invalidPosition pure $ asum [Next <$> mNext, Previous <$> mPrevious]

    _ -> invalidPosition
   where
    invalidPosition =
      fail
        $ "Position must be the String \"first\" or \"last\","
        <> " or an Object with a \"next\" or \"previous\" key"

newtype Limit = Limit { unLimit :: Int }
  deriving newtype (Eq, Ord, Show, Num)

readLimit :: Text -> Either String Limit
readLimit t = case readMaybe @Int $ unpack t of
  Nothing -> limitMustBe "an integer"
  Just limit | limit <= 0 -> limitMustBe "positive and non-zero"
  Just limit -> Right $ Limit limit
  where limitMustBe msg = Left $ "Limit must be " <> msg <> ": " <> show t

cursorRouteAtPosition
  :: ToJSON position => Cursor position -> Position position -> RenderedRoute
cursorRouteAtPosition cursor position =
  updateQueryParameter "position" (Just $ encodeText position)
    $ cursorRoute cursor

parseCursorParams
  :: (MonadHandler m, FromJSON position, RenderRoute (HandlerSite m))
  => Limit
  -> m (Cursor position)
parseCursorParams defaultLimit = do
  mePosition <- fmap eitherDecodeText <$> lookupGetParam "position"
  position <- case mePosition of
    Nothing -> pure First
    Just (Left err) -> invalidArgs [pack err]
    Just (Right p) -> pure p

  limit <-
    either (invalidArgs . pure . pack) pure
    . maybe (Right defaultLimit) readLimit
    =<< lookupGetParam "limit"

  renderedRoute <- getRenderedRoute
  pure $ Cursor renderedRoute position limit

eitherDecodeText :: FromJSON a => Text -> Either String a
eitherDecodeText = eitherDecode . BSL.fromStrict . encodeUtf8

encodeText :: ToJSON a => a -> Text
encodeText = decodeUtf8 . BSL.toStrict . encode

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay [x] = Just x
lastMay (_ : xs) = lastMay xs

takeEnd :: Int -> [a] -> [a]
takeEnd i xs = f xs (drop i xs)
 where
  f (_ : xs') (_ : ys) = f xs' ys
  f xs' _ = xs'
