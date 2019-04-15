# yesod-page-cursor

Cursor based pagination for `yesod`.

```hs
getSomeR :: Handler Value
getSomeR = do
  let
    parseParams =
      (,) <$> Param.required "teacherId" <*> Param.optional "courseId"
  page <- withPage entityPage parseParams $ \Cursor {..} -> do
    let (teacherId, mCourseId) = cursorParams
    runDB $ selectList
      (catMaybes
        [ Just $ SomeAssignmentTeacherId ==. teacherId
        , (SomeAssignmentCourseId ==.) <$> mCourseId
        , case cursorPosition of
          First -> Nothing
          Previous p -> Just $ persistIdField <. p
          Next p -> Just $ persistIdField >. p
        ]
      )
      [LimitTo $ fromMaybe 100 cursorLimit]
  returnJson $ keyValueEntityToJSON <$> page
```

`cursorLastPosition` is configurable. A page sorted by `created_at` may look like:

```hs
createdAtPage = PageConfig
  { makePosition = \x ->
      (entityKey x, someAsssignmentCreatedAt $ entityVal x)
  , baseDomain = Nothing
  }

getSortedSomeR :: Handler Value
getSortedSomeR = do
  let parseParams = pure ()
  page <- withPage createdAtPage parseParams $ \Cursor {..} -> do
    runDB $ selectList
      (case cursorPosition of
        First -> []
        Previous (pId, createdAt) ->
          [ SomeAssingmentCreatedAt <=. createdAt
          , persistIdField <. pId
          ]
        Next (pId, createdAt) ->
          [ SomeAssingmentCreatedAt >=. createdAt
          , persistIdField >. pId
          ]
      )
      [ LimitTo $ fromMaybe 100 cursorLimit
      , Asc SomeAssignmentCreatedAt
      ]
  returnJson $ keyValueEntityToJSON <$> page
```

## Usage

Paginated requests return a single page and a link with a cursor token to retrieve the next page.

```sh
$ curl 'some-rest.com/endpoint?limit=3'
{
  "first": : "some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "previous": null,
  "next": "some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "data": [...]
}
```

The link can be used to retrieve the next page.

```sh
$ curl 'some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ=='
{
  "first": : "some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "previous": "some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "next": "some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "data": [...]
}
```

If no pages remain then no link is returned

```sh
$ curl 'some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ=='
{
  "first": : "some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "previous": "some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "next": null,
  "data": [...]
}
```
