{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yesod.Page.QueryParam.Internal
  ( optional
  , required
  , ParseParamM
  , ParseParam(LookupGetParam, ParseParamError)
  , eitherDecodeText
  , decodeText
  )
where

import Control.Monad.Free (Free, liftF)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Yesod.Core

data ParseParam next
  = LookupGetParam Text (Maybe Text -> next)
  | ParseParamError Text next
  deriving (Functor)

type ParseParamM = Free ParseParam

eitherDecodeText :: FromJSON a => Text -> Either String a
eitherDecodeText = eitherDecode . BSL.fromStrict . encodeUtf8

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . BSL.fromStrict . encodeUtf8

optional :: PathPiece a => Text -> Free ParseParam (Maybe a)
optional param = liftF $ LookupGetParam param (fromPathPiece =<<)

required :: PathPiece a => Text -> Free ParseParam a
required param = do
  mTxt <- optional param
  maybe (failedParse $ "Could not parse query param " <> param) pure mTxt

failedParse :: Text -> Free ParseParam a
failedParse txt = liftF $ ParseParamError txt (error "impossible")
