### Lists

#### About Lists

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`description` | A friendly list description | `string` |   | `false`
`name` | A friendly match list name | `string` |   | `true`
`org` | Full legal name of the organization | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/lists

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/lists

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/lists/{LISTID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists/{LISTID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/lists/{LISTID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists/{LISTID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/lists/{LISTID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists/{LISTID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/lists/{LISTID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists/{LISTID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/lists/{LISTID}/entries

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists/{LISTID}/entries
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/lists/{LISTID}/entries

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists/{LISTID}/entries
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/lists/{LISTID}/entries

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists/{LISTID}/entries
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/lists/{LIST}/entries/{ENTRYID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists/{LIST}/entries/{ENTRYID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/lists/{LIST}/entries/{ENTRYID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists/{LIST}/entries/{ENTRYID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/lists/{LIST}/entries/{ENTRYID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists/{LIST}/entries/{ENTRYID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/lists/{LIST}/entries/{ENTRYID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists/{LIST}/entries/{ENTRYID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/lists/{LIST}/entries/{ENTRYID}/photo

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists/{LIST}/entries/{ENTRYID}/photo
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/lists/{LIST}/entries/{ENTRYID}/vcard

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/lists/{LIST}/entries/{ENTRYID}/vcard
```

