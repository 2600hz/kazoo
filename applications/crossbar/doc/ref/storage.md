# Storage

## About Storage

#### Schema



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^_` | Ignores CouchDB fields prefixed by underscores | `boolean() | integer() | object() | string()` |   | `false` |  
`^pvt_` | Ignores Kazoo private fields prefixed by pvt_ | `boolean() | integer() | string()` |   | `false` |  
`attachments` | Defines where and how to store attachments. Keys are 32-character identifiers to be used in storage plans | [#/definitions/storage.attachments](#storageattachments) |   | `false` |  
`connections` | Describes alternative connections to use (such as alternative CouchDB instances | [#/definitions/storage.connections](#storageconnections) |   | `false` |  
`id` | ID of the storage document | `string()` |   | `false` |  
`plan` | Describes how to store documents depending on the database or document type | [#/definitions/storage.plan](#storageplan) |   | `false` |  

### storage.attachment.aws

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

### storage.attachment.azure

schema for azure attachment entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` | What handler module to use | `string('azure')` |   | `true` |  
`settings.account` | the azure account name | `string()` |   | `true` |  
`settings.container` | the azure container where the files should be saved | `string()` |   | `true` |  
`settings.key` | the azure api key | `string()` |   | `true` |  
`settings` | Settings for the Azure account | `object()` |   | `true` |  

### storage.attachment.dropbox

schema for dropbox attachment entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` | What handler module to use | `string('dropbox')` |   | `true` |  
`settings.oauth_doc_id` | Doc ID in the system 'auth' database | `string(1..)` |   | `true` |  
`settings` | Settings for the Dropbox account | `object()` |   | `true` |  

### storage.attachment.google_drive

schema for google drive attachment entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` | What handler module to use | `string('google_drive')` |   | `true` |  
`settings.folder_id` | Folder ID in which to store the file, if any | `string()` |   | `false` |  
`settings.oauth_doc_id` | Doc ID in the system 'auth' database | `string(1..)` |   | `true` |  
`settings` | Settings for the Google Drive account | `object()` |   | `true` |  

### storage.attachment.google_storage

schema for google storage attachment entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` | What handler module to use | `string('google_storage')` |   | `true` |  
`settings` | Settings for the Google Storage account | `object()` |   | `true` |  

### storage.attachment.http

schema for HTTP(s) attachment entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` | The handler interface to use | `string('http')` |   | `true` |  
`settings.base64_encode_data` | Toggles whether to base64-encode the attachment data | `boolean()` | `false` | `false` |  
`settings.send_multipart` | Toggle whether to send multipart payload when storing attachment - will include metadata JSON if true | `boolean()` |   | `false` |  
`settings.url` | The base HTTP(s) URL to use when creating the request | `string()` |   | `true` |  
`settings.verb` | The HTTP verb to use when sending the data | `string('post' | 'put')` | `put` | `false` |  
`settings` | HTTP server settings | `object()` |   | `true` |  

### storage.attachment.onedrive

schema for OneDrive attachment entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` | What handler module to use | `string('onedrive')` |   | `true` |  
`settings.oauth_doc_id` | Doc ID in the system 'auth' database | `string(1..)` |   | `true` |  
`settings` | Settings for the OneDrive account | `object()` |   | `true` |  

### storage.attachments

Defines where and how to store attachments. Keys are 32-character identifiers to be used in storage plans


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^[a-z0-9]{32}$.name` | Friendly name for this configuration | `string()` |   | `false` |  
`^[a-z0-9]{32}$.settings.field_list` | list of fields to compose destination url | `["array(", "[#/definitions/storage.attachments.field](#storageattachments.field)", ")"]` |   | `false` |  
`^[a-z0-9]{32}$.settings.field_separator` | toplevel, field separator to compose destination url | `string()` |   | `false` |  
`^[a-z0-9]{32}$.settings.folder_base_path` | base folder path | `string()` |   | `false` |  
`^[a-z0-9]{32}$.settings` | Settings all handlers implement | `object()` |   | `false` |  
`^[a-z0-9]{32}$` | Configuration for the supported storage backends | `object()` |   | `false` |  

### storage.attachments.field

field used when composing destination url


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------

### storage.connection.couchdb

schema for CouchDB connection entry


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

### storage.connections

Describes alternative connections to use (such as alternative CouchDB instances


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^([a-z,0-9]){32}$` |   | [#/definitions/storage.connection.couchdb](#storageconnection.couchdb) |   | `false` |  
`local` |   | `object()` |   | `false` |  

### storage.plan

Describes how to store documents depending on the database or document type


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`account` | schema for database storage plan | [#/definitions/storage.plan.database](#storageplan.database) |   | `false` |  
`aggregate` | schema for database storage plan | [#/definitions/storage.plan.database](#storageplan.database) |   | `false` |  
`modb` | schema for database storage plan | [#/definitions/storage.plan.database](#storageplan.database) |   | `false` |  
`system` | schema for database storage plan | [#/definitions/storage.plan.database](#storageplan.database) |   | `false` |  

### storage.plan.database

schema for database storage plan


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`attachments` | Describes what attachment types to store using this plan | [#/definitions/storage.plan.database.attachment](#storageplan.database.attachment) |   | `false` |  
`connection` | Which connection UUID to use when storing to this database type | `string()` |   | `false` |  
`database.create_options` |   | `object()` |   | `false` |  
`database.names.[]` |   | `string()` |   | `false` |  
`database.names` | List of database names to match (non-matching names won't use this plan) | `array(string())` |   | `false` |  
`database` |   | `object()` |   | `false` |  
`types.call_recording` | schema for document type storage plan | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false` |  
`types.fax` | schema for document type storage plan | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false` |  
`types.function` | schema for document type storage plan | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false` |  
`types.mailbox_message` | schema for document type storage plan | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false` |  
`types.media` | schema for document type storage plan | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false` |  
`types` | The document types to store with this plan | `object()` |   | `false` |  

### storage.plan.database.attachment

schema for attachment ref type storage plan


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` |   | `string()` |   | `false` |  
`params.folder_path` | folder path | `string()` |   | `false` |  
`params` |   | `object()` |   | `false` |  
`stub` |   | `boolean()` |   | `false` |  

### storage.plan.database.document

schema for document type storage plan


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`attachments` | schema for attachment ref type storage plan | [#/definitions/storage.plan.database.attachment](#storageplan.database.attachment) |   | `false` |  
`connection` |   | `string()` |   | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/storage/plans

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage/plans
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/storage/plans

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage/plans
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
```

