{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yesod.Page.RenderedRoute
  ( RenderedRoute
  , getRenderedRoute
  , updateQueryParameter
  )
where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, intercalate)
import UnliftIO (throwString)
import Yesod.Core
  ( HandlerSite
  , MonadHandler
  , RenderRoute
  , getCurrentRoute
  , getRequest
  , renderRoute
  , reqGetParams
  )

-- | Information about the initial route
data RenderedRoute = RenderedRoute
  { renderedRoutePath :: [Text]
  , renderedRouteQuery :: [(Text, Text)]
  }

instance ToJSON RenderedRoute where
  -- | Renders as a @'Text'@ URL
  --
  -- FIXME: This is wildly naive and we should move to a proper library to
  -- ensure escaping is handled, but it's meant only to prove the approach and
  -- pass the current test suite.
  --
  toJSON RenderedRoute {..} = String
    $ intercalate "/" renderedRoutePath
    <> renderQuery renderedRouteQuery
   where
    renderQuery [] = ""
    renderQuery qs = "?" <> intercalate "&" (map (\(k, v) -> k <> "=" <> v) qs)

-- | Get the current route as a @'RenderedRoute'@
getRenderedRoute
  :: (MonadHandler m, RenderRoute (HandlerSite m)) => m RenderedRoute
getRenderedRoute = do
  route <- maybe (throwString "no route") pure =<< getCurrentRoute

  -- When I just use _query, it's always empty. Why would renderRoute return
  -- this tuple if the Route value (apparently) never has the query information?
  let (path, _query) = renderRoute route
  query <- reqGetParams <$> getRequest

  pure $ RenderedRoute {renderedRoutePath = path, renderedRouteQuery = query}

-- | Update a single query parameter and preserve the rest
--
-- If given @'Nothing'@, the parameter is removed.
--
updateQueryParameter :: Text -> Maybe Text -> RenderedRoute -> RenderedRoute
updateQueryParameter name = overQuery . asMap . updateKey name

-- Lens? meh
overQuery
  :: ([(Text, Text)] -> [(Text, Text)]) -> RenderedRoute -> RenderedRoute
overQuery f renderedRoute =
  renderedRoute { renderedRouteQuery = f $ renderedRouteQuery renderedRoute }

asMap :: Ord k => (Map k v -> Map k v) -> [(k, v)] -> [(k, v)]
asMap f = Map.toList . f . Map.fromList

updateKey :: Ord k => k -> Maybe v -> Map k v -> Map k v
updateKey k = maybe (Map.delete k) $ Map.insert k
