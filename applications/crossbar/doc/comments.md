

Allow you to add comments to "any" documents in Kazoo.

#### Comments

* GET - Gets the current comment(s).
* PUT - Add a comment.
* POST - Update a comment.
* DELETE - Remove a comment(s).

##### Fetch Comments:

###### Request

- Verb: `GET`
- Url: `/v2/accounts/{{ACCOUNT_ID}/{{DOC_TYPE}}/{{ID}}/comments`
- Payload: None

###### Response

```json
{
  "data": {
    "comments": [{{COMMENT_1}}, {{COMMENT_2}}]
  },
  "status": "success"
}
```

##### Fetch a Comment:

###### Request

- Verb: `GET`
- Url: `/v2/accounts/{{ACCOUNT_ID}/{{DOC_TYPE}}/{{ID}}/comments/{{COMMENT_NUMBER}}`
- Payload: None

###### Response

```json
{
  "data": {{COMMENT}},
  "status": "success"
}
```

##### Add a Comment:

###### Request

- Verb: `PUT`
- Url: `/v2/accounts/{{ACCOUNT_ID}/{{DOC_TYPE}}/{{ID}}/comments`
- Payload:

```json
{
  "data": {
    "comments": [{{COMMENT_3}}]
  }
}
```

###### Response

```json
{
  "data": {
    "comments": [{{COMMENT_1}}, {{COMMENT_2}}, {{COMMENT_3}}]
  },
  "status": "success"
}
```

##### Update a Comment:

###### Request

- Verb: `POST`
- Url: `/v2/accounts/{{ACCOUNT_ID}/{{DOC_TYPE}}/{{ID}}/comments/{{COMMENT_NUMBER}}`
- Payload:

```json
{
  "data": {{COMMENT}}
}
```

###### Response

```json
{
  "data": {{COMMENT}},
  "status": "success"
}
```

##### Delete all Comments:

###### Request

- Verb: `DELETE`
- Url: `/v2/accounts/{{ACCOUNT_ID}/{{DOC_TYPE}}/{{ID}}/comments`
- Payload: None

###### Response


```json
{
  "data": [],
  "status": "success"
}
```

##### Delete a Comment:

###### Request

- Verb: `DELETE`
- Url: `/v2/accounts/{{ACCOUNT_ID}/{{DOC_TYPE}}/{{ID}}/comments/{{COMMENT_NUMBER}}`
- Payload: None

###### Response

```json
{
  "data": {
    "comments": [{{COMMENT_1}}, {{COMMENT_2}}]
  },
  "status": "success"
}
```
