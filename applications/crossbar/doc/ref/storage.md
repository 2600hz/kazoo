### Storage

#### About Storage

#### Schema



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^_` | Ignores CouchDB fields prefixed by underscores | `string() | integer() | boolean() | object()` |   | `false` |  
`^pvt_` | Ignores Kazoo private fields prefixed by pvt_ | `string() | integer() | boolean()` |   | `false` |  
`attachments` | Defines where and how to store attachments | [#/definitions/storage.attachments](#storageattachments) |   | `false` |  
`connections` | Describes alternative connections to use (such as alternative CouchDB instances | [#/definitions/storage.connections](#storageconnections) |   | `false` |  
`id` | ID of the storage document | `string()` |   | `false` |  
`plan` | Describes how to store documents depending on the database or document type | [#/definitions/storage.plan](#storageplan) |   | `false` |  
`ui_metadata` |   | `object()` |   | `false` |  

##### storage.attachment.aws

schema for AWS attachment entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` | What AWS service to use | `string('s3')` |   | `true` |  
`settings.bucket` | Bucket name to store data to | `string(6..63)` |   | `true` |  
`settings.bucket_access_method` | how to access the host. | `string('auto' | 'vhost' | 'path')` |   | `false` |  
`settings.bucket_after_host` | use bucket after host as part of url | `boolean()` |   | `false` |  
`settings.host` | the s3 host, leave empty for default | `string(1..)` |   | `false` |  
`settings.key` | AWS Key to use | `string(1..128)` |   | `true` |  
`settings.port` | port to use | `integer()` |   | `false` |  
`settings.region` | the region where the bucket is located | `string(1..)` |   | `false` |  
`settings.scheme` | scheme to use to access host | `string('http' | 'https')` |   | `false` |  
`settings.secret` | AWS Secret to use | `string(1..128)` |   | `true` |  
`settings` | AWS API settings | `object()` |   | `true` |  

##### storage.attachment.azure

schema for azure attachment entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` | What handler module to use | `string('azure')` |   | `true` |  
`settings.account` | the azure account name | `string()` |   | `true` |  
`settings.container` | the azure container where the files should be saved | `string()` |   | `true` |  
`settings.key` | the azure api key | `string()` |   | `true` |  
`settings` | Settings for the Azure account | `object()` |   | `true` |  

##### storage.attachment.dropbox

schema for dropbox attachment entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` | What handler module to use | `string('dropbox')` |   | `true` |  
`settings.oauth_doc_id` | Doc ID in the system 'auth' database | `string()` |   | `true` |  
`settings` | Settings for the DropBox account | `object()` |   | `true` |  

##### storage.attachment.google_drive

schema for google drive attachment entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` | What handler module to use | `string('google_drive')` |   | `true` |  
`settings.folder_id` | Folder ID in which to store the file, if any | `string()` |   | `false` |  
`settings.oauth_doc_id` | Doc ID in the system 'auth' database | `string()` |   | `true` |  
`settings` | Settings for the Google Drive account | `object()` |   | `true` |  

##### storage.attachment.google_storage

schema for google storage attachment entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` | What handler module to use | `string('google_storage')` |   | `true` |  
`settings` | Settings for the Google Storage account | `object()` |   | `true` |  

##### storage.attachment.onedrive

schema for OneDrive attachment entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` | What handler module to use | `string('onedrive')` |   | `true` |  
`settings.oauth_doc_id` | Doc ID in the system 'auth' database | `string()` |   | `true` |  
`settings` | Settings for the OneDrive account | `object()` |   | `true` |  

##### storage.attachments

Keys are 32-character identifiers to be used in storage plans


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^[a-z0-9]{32}$.field_list` | list of field to compose destination url | `array()` |   | `false` |  
`^[a-z0-9]{32}$.field_separator` | toplevel, field separator to compose destination url | `string()` |   | `false` |  
`^[a-z0-9]{32}$.folder_base_path` | base folder path | `string()` |   | `false` |  
`^[a-z0-9]{32}$.name` | Friendly name for this configuration | `string()` |   | `false` |  
`^[a-z0-9]{32}$` | Configuration for the supported storage backends | `object()` |   | `false` |  

##### storage.connection.couchdb

schema for couchdb connection entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`driver` |   | `string('kazoo_couch')` |   | `true` |  
`name` |   | `string()` |   | `false` |  
`settings.connect_options.keepalive` |   | `boolean()` |   | `false` |  
`settings.connect_options` |   | `object()` |   | `false` |  
`settings.connect_timeout` |   | `integer()` |   | `false` |  
`settings.credentials.password` |   | `integer()` |   | `true` |  
`settings.credentials.username` |   | `string()` |   | `true` |  
`settings.credentials` |   | `object()` |   | `false` |  
`settings.ip` |   | `string()` |   | `true` |  
`settings.max_pipeline_size` |   | `integer()` |   | `false` |  
`settings.max_sessions` |   | `integer()` |   | `false` |  
`settings.pool.name` |   | `string()` |   | `true` |  
`settings.pool.size` |   | `integer()` |   | `true` |  
`settings.pool` |   | `object()` |   | `false` |  
`settings.port` |   | `integer()` |   | `true` |  
`settings` |   | `object()` |   | `true` |  

##### storage.connections


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^([a-z,0-9]){32}$` |   | [#/definitions/storage.connection.couchdb](#storageconnection.couchdb) |   | `false` |  
`local` |   | `object()` |   | `false` |  

##### storage.plan

schema for storage plan


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`account` |   | [#/definitions/storage.plan.database](#storageplan.database) |   | `false` |  
`modb` |   | [#/definitions/storage.plan.database](#storageplan.database) |   | `false` |  
`system` |   | [#/definitions/storage.plan.database](#storageplan.database) |   | `false` |  

##### storage.plan.database

schema for database storage plan


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`attachments` |   | [#/definitions/storage.plan.database.attachment](#storageplan.database.attachment) |   | `false` |  
`connection` |   | `string()` |   | `false` |  
`database.create_options` |   | `object()` |   | `false` |  
`database` |   | `object()` |   | `false` |  
`types.call_recording` |   | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false` |  
`types.fax` |   | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false` |  
`types.mailbox_message` |   | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false` |  
`types.media` |   | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false` |  
`types` |   | `object()` |   | `false` |  

##### storage.plan.database.attachment

schema for attachment ref type storage plan


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` |   | `string()` |   | `false` |  
`params.folder_path` | folder path | `string()` |   | `false` |  
`params` |   | `object()` |   | `false` |  
`stub` |   | `boolean()` |   | `false` |  

##### storage.plan.database.document

schema for document type storage plan


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`attachments` |   | [#/definitions/storage.plan.database.attachment](#storageplan.database.attachment) |   | `false` |  
`connection` |   | `string()` |   | `false` |  



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X GET \
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

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X DELETE \
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

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
```

