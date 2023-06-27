{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Page.Persist
  ( selectPageLinkAbsolute
  ) where

import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Data.Aeson
import Database.Persist
import Yesod.Core (HandlerSite, RenderRoute, Yesod, MonadHandler)
import Yesod.Page

selectPageLinkAbsolute
  :: ( MonadHandler m
     , Yesod (HandlerSite m)
     , RenderRoute (HandlerSite m)
     , PersistQueryRead backend
     , PersistRecordBackend record backend
     , PersistEntity record
     , PersistField position
     , FromJSON position
     , ToJSON position
     )
  => EntityField record position
  -> Int
  -> [Filter record]
  -> [SelectOpt record]
  -> ReaderT backend m (Page (Entity record))
selectPageLinkAbsolute = withPageToSelect withPageLinkAbsolute

withPageToSelect
  :: ( EntityField record position
     -> Int
     -> [Filter record]
     -> [SelectOpt record]
     -> (Cursor position -> m [a]) -> m [a]
     )
  -> EntityField record position
  -> Int
  -> [Filter record]
  -> [SelectOpt record]
  -> ReaderT backend m (Page (Entity record))
withPageToSelect f entityField limit filters opts =
  f limit pagePosition $ \cursor@Cursor {..} ->
    pageSort cursorPosition <$> selectList
      (filters <> pageFilters cursorPosition)
      (opts <> pageOpts cursor)
 where
  pagePosition = view $ fieldLens entityField

  pageSort = \case
    First -> id
    Previous _ -> reverse
    Next _ -> id
    Last -> reverse

  pageFilters = \case
    First -> []
    Previous p -> [entityField <. p]
    Next p -> [entityField >. p]
    Last -> []

  pageOpts Cursor {..} =
      [ LimitTo $ unLimit cursorLimit
      , case cursorPosition of
          First -> Asc persistIdField
          Previous _ -> Desc persistIdField
          Next _ -> Asc persistIdField
          Last -> Desc persistIdField
      ]
