### Storage

#### About Storage

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`attachments` |   | [#/definitions/storage.attachments](#storageattachments) |   | `false`
`connections` |   | [#/definitions/storage.connections](#storageconnections) |   | `false`
`plan` |   | [#/definitions/storage.plan](#storageplan) |   | `false`


##### storage.attachment.aws

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`handler` |   | `string('s3')` |   | `true`
`name` |   | `string` |   | `false`
`settings` |   | `object` |   | `true`
`settings.bucket` |   | `string` |   | `true`
`settings.host` |   | `string` |   | `false`
`settings.key` |   | `string` |   | `true`
`settings.path` |   | `string` |   | `false`
`settings.secret` |   | `string` |   | `true`

##### storage.attachment.google_drive

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`handler` |   | `string('google_drive')` |   | `true`
`name` |   | `string` |   | `false`
`settings` |   | `object` |   | `true`
`settings.oauth_doc_id` |   | `string` |   | `true`
`settings.path` |   | `string` |   | `false`

##### storage.attachments

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------

##### storage.connection.couchdb

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`driver` |   | `string('kazoo_couch')` |   | `true`
`name` |   | `string` |   | `false`
`settings` |   | `object` |   | `true`
`settings.connect_options` |   | `object` |   | `false`
`settings.connect_options.keepalive` |   | `boolean` |   | `false`
`settings.connect_timeout` |   | `integer` |   | `false`
`settings.credentials` |   | [#/definitions/#/definitions/credentials](##/definitions/credentials) |   | `false`
`settings.ip` |   | `string` |   | `true`
`settings.max_pipeline_size` |   | `integer` |   | `false`
`settings.max_sessions` |   | `integer` |   | `false`
`settings.pool` |   | [#/definitions/#/definitions/pool](##/definitions/pool) |   | `false`
`settings.port` |   | `integer` |   | `true`

##### storage.connections

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------

##### storage.plan

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`account` |   | [#/definitions/storage.plan.database](#storageplan.database) |   | `false`
`modb` |   | [#/definitions/storage.plan.database](#storageplan.database) |   | `false`
`system` |   | [#/definitions/storage.plan.database](#storageplan.database) |   | `false`

##### storage.plan.database

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`attachments` |   | [#/definitions/storage.plan.database.attachment](#storageplan.database.attachment) |   | `false`
`connection` |   | `string` |   | `false`
`database` |   | [#/definitions/#/definitions/database](##/definitions/database) |   | `false`
`types` |   | `object` |   | `false`
`types.call_recording` |   | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false`
`types.fax` |   | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false`
`types.mailbox_message` |   | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false`
`types.media` |   | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false`

##### storage.plan.database.attachment

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`handler` |   | `string` |   | `false`
`params` |   | `object` |   | `false`
`stub` |   | `boolean` |   | `false`

##### storage.plan.database.document

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`attachments` |   | [#/definitions/storage.plan.database.attachment](#storageplan.database.attachment) |   | `false`
`connection` |   | `string` |   | `false`

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/storage/plans

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage/plans
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/storage/plans

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage/plans
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
```

