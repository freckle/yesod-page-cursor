{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Page.QueryParam.Internal
  ( optional
  , required
  , ParseParamM
  , ParseParam(LookupGetParam, ParseParamError)
  , parseParams
  )
where

import Control.Monad.Free (Free(Free, Pure), liftF)
import Data.Text (Text)
import Yesod.Core

data ParseParam next
  = LookupGetParam Text (Maybe Text -> next)
  | ParseParamError Text next
  deriving (Functor)

type ParseParamM = Free ParseParam

-- | Parse an option a query param
optional :: PathPiece a => Text -> Free ParseParam (Maybe a)
optional param = liftF $ LookupGetParam param (fromPathPiece =<<)

-- | Parse a query param and fails when it is not found
required :: PathPiece a => Text -> Free ParseParam a
required param = do
  mTxt <- optional param
  maybe (failedParse $ "Could not parse query param " <> param) pure mTxt

failedParse :: Text -> Free ParseParam a
failedParse txt = liftF $ ParseParamError txt (error "impossible")

parseParams :: MonadHandler m => ParseParamM a -> m a
parseParams = \case
  (Free (LookupGetParam param next)) ->
    parseParams . next =<< lookupGetParam param
  (Free (ParseParamError err _)) -> invalidArgs [err]
  (Pure x) -> pure x
