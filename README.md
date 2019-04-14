# yesod-page-cursor

Cursor based pagination for `yesod`.

## Usage

Paginated requests return a single page and a link with a cursor token to retrieve the next page.

```sh
$ curl 'some-rest.com/endpoint?limit=3'
{
  "next": "/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "data": [...]
}
```

The token can then be passed as a query param to retrieve the next page.

```sh
$ curl 'some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ=='
{
  "next": "/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "data": [...]
}
```

If no pages remain then no token is returned

```sh
$ curl 'some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ=='
{
  "next": null,
  "data": [...]
}
```

## Example

```sh
$ curl 'localhost:3000/?teacherId=1&limit=3'
{
  "next": "/?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "data": [
    {
      "value": {
        "someAssignmentCourseId": 2,
        "someAssignmentTeacherId": 1,
        "someAssignmentCreatedAt": "2019-04-14T18:02:09.100432095Z"
      },
      "key": 1
    },
    {
      "value": {
        "someAssignmentCourseId": 2,
        "someAssignmentTeacherId": 1,
        "someAssignmentCreatedAt": "2019-04-14T18:02:09.100432095Z"
      },
      "key": 2
    },
    {
      "value": {
        "someAssignmentCourseId": 2,
        "someAssignmentTeacherId": 1,
        "someAssignmentCreatedAt": "2019-04-14T18:02:09.100432095Z"
      },
      "key": 3
    }
  ]
}

$ curl 'localhost:3000/?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ=='
{
  "next": "/?next=eyJsYXN0UG9zaXRpb24iOjYsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "data": [
    {
      "value": {
        "someAssignmentCourseId": 2,
        "someAssignmentTeacherId": 1,
        "someAssignmentCreatedAt": "2019-04-14T18:02:09.100432095Z"
      },
      "key": 4
    },
    {
      "value": {
        "someAssignmentCourseId": 2,
        "someAssignmentTeacherId": 1,
        "someAssignmentCreatedAt": "2019-04-14T18:02:09.100432095Z"
      },
      "key": 5
    },
    {
      "value": {
        "someAssignmentCourseId": 2,
        "someAssignmentTeacherId": 1,
        "someAssignmentCreatedAt": "2019-04-14T18:02:09.100432095Z"
      },
      "key": 6
    }
  ]
}

$ curl 'localhost:3000/?next=eyJsYXN0UG9zaXRpb24iOjYsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ=='
{
  "next": "/?next=eyJsYXN0UG9zaXRpb24iOjksInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "data": [
    {
      "value": {
        "someAssignmentCourseId": 2,
        "someAssignmentTeacherId": 1,
        "someAssignmentCreatedAt": "2019-04-14T18:02:09.100432095Z"
      },
      "key": 7
    },
    {
      "value": {
        "someAssignmentCourseId": 2,
        "someAssignmentTeacherId": 1,
        "someAssignmentCreatedAt": "2019-04-14T18:02:09.100432095Z"
      },
      "key": 8
    },
    {
      "value": {
        "someAssignmentCourseId": 2,
        "someAssignmentTeacherId": 1,
        "someAssignmentCreatedAt": "2019-04-14T18:02:09.100432095Z"
      },
      "key": 9
    }
  ]
}
```
