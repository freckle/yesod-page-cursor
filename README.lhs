# yesod-page-cursor

[![Hackage](https://img.shields.io/hackage/v/yesod-page-cursor.svg?style=flat)](https://hackage.haskell.org/package/yesod-page-cursor)
[![Stackage Nightly](http://stackage.org/package/yesod-page-cursor/badge/nightly)](http://stackage.org/nightly/package/yesod-page-cursor)
[![Stackage LTS](http://stackage.org/package/yesod-page-cursor/badge/lts)](http://stackage.org/lts/package/yesod-page-cursor)
[![CI](https://github.com/freckle/yesod-page-cursor/actions/workflows/ci.yml/badge.svg)](https://github.com/freckle/yesod-page-cursor/actions/workflows/ci.yml)

Cursor based pagination for `yesod` using index friendly keyset cursors.

Primer: [No Offset](https://use-the-index-luke.com/no-offset)

<!--
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (module Main) where

import Prelude

import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Yesod.Core
import Yesod.Page

data App = App

mkYesodData "App" [parseRoutes|
/ SomeR GET
|]

instance Yesod App

requiredParam :: String -> Handler a
requiredParam = undefined

optionalParam :: String -> Handler (Maybe a)
optionalParam = undefined

runDB :: SqlPersistT m a -> m a
runDB = undefined

```
-->

```haskell
mkPersist sqlSettings [persistLowerCase|
SomeAssignment json
  createdAt     UTCTime
  teacherId     Int
  courseId      Int
|]

getSomeR :: Handler Value
getSomeR = do
  teacherId <- requiredParam "teacherId"
  mCourseId <- optionalParam "courseId"
  page <- withPageAbsolute 100 entityKey $ \Cursor {..} -> do
    fmap (sort cursorPosition) . runDB $ selectList
      (catMaybes
        [ Just $ SomeAssignmentTeacherId ==. teacherId
        , (SomeAssignmentCourseId ==.) <$> mCourseId
        , whereClause cursorPosition
        ]
      )
      [LimitTo $ unLimit cursorLimit, orderBy cursorPosition]
  returnJson $ keyValueEntityToJSON <$> page
 where
  whereClause = \case
    First -> Nothing
    Previous p -> Just $ persistIdField <. p
    Next p -> Just $ persistIdField >. p
    Last -> Nothing
  orderBy = \case
    First -> Asc persistIdField
    Previous _ -> Desc persistIdField
    Next _ -> Asc persistIdField
    Last -> Desc persistIdField
  sort = \case
    First -> id
    Previous _ -> reverse
    Next _ -> id
    Last -> reverse
```

`cursorLastPosition` is configurable. A page sorted by `created_at` may look
like:

```haskell
getSortedSomeR :: Handler Value
getSortedSomeR = do
  page <- withPageAbsolute 100 (someAssignmentCreatedAt . entityVal)
    $ \Cursor {..} -> do
      fmap (sort cursorPosition) . runDB $ selectList
        (whereClause cursorPosition)
        [ LimitTo $ unLimit cursorLimit
        , orderBy cursorPosition
        ]
  returnJson $ keyValueEntityToJSON <$> page
 where
  whereClause = \case
    First -> []
    Previous createdAt ->
      [ SomeAssignmentCreatedAt <=. createdAt
      ]
    Next createdAt ->
      [ SomeAssignmentCreatedAt >=. createdAt
      ]
    Last -> []
  orderBy = \case
    First -> Asc SomeAssignmentCreatedAt
    Previous _ -> Desc SomeAssignmentCreatedAt
    Next _ -> Asc SomeAssignmentCreatedAt
    Last -> Desc SomeAssignmentCreatedAt
  sort = \case
    First -> id
    Previous _ -> reverse
    Next _ -> id
    Last -> reverse
```

## Usage

Paginated requests return a single page and a link with a cursor token to
retrieve the next page.

```console
% curl 'some-rest.com/endpoint?limit=3'
{
  "first": : "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "previous": null,
  "next": "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "data": [...]
}
```

The link can be used to retrieve the next page.

```console
% curl 'some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ=='
{
  "first": : "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "previous": "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "next": "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "data": [...]
}
```

If no pages remain then no link is returned

```console
% curl 'some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ=='
{
  "first": : "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "previous": "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "next": null,
  "data": [...]
}
```

<!--
```haskell
main :: IO ()
main = pure ()
```
-->
