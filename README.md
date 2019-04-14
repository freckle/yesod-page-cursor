# yesod-page-cursor

Cursor based pagination for `yesod`

```sh
$ curl 'localhost:3000?teacherId=1&limit=3'
{
  "next": "eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
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

$ curl 'localhost:3000?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ=='
{
  "next": "eyJsYXN0UG9zaXRpb24iOjYsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
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

$ curl 'localhost:3000?next=eyJsYXN0UG9zaXRpb24iOjYsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ=='
{
  "next": "eyJsYXN0UG9zaXRpb24iOjksInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
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
