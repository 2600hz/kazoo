# Storage

## About Storage

Storage plans allow an account to control where data related to their account is stored. This can be critical when compliance with regulations is required.

A storage plan has three main components:

1. `attachments`: configuration for where to store attachments (binary data typically, like voicemails or faxes)
2. `connections`: connection information to various third-party storage sites
3. `plan`: a description of the storage plan(s) for the account.

### Attachments

Rather than storing binary data like voicemails, received faxes, and call recordings, in the Kazoo databases, it can be convenient to store them in third-party storage services like [Amazon S3](https://aws.amazon.com/s3), [Google Drive](https://www.google.com/drive/), etc. This keeps the binary data in a place that an account or user maintains control over. Kazoo keeps a pointer to the location when it needs to fetch the binary data (such as when you call into your voicemail box).

The `attachments` object configures storage back-ends - for instance, if you want to store to S3, you'll add your AWS secret and key and your S3 bucket information here.

### Connections

Connections can be used to point to an alternative CouchDB instance for storing the JSON documents or attachments (for instance: putting CDRs in their own cluster). These can be specified in the storage plans.

### Plans

Plans determine what to do with certain classes of databases:

1. `account`: where to store account data
2. `modb`: where to store temporal data, like CDRs or voicemails
3. `system`: where to store system data, like prompts

Within the `database` classification, you can define things like the connection to use when reading/writing, what types of documents should be stored/retrieved, etc.

### Enabling the storage endpoint

Crossbar must have the storage endpoint enabled first:

```
sup crossbar_maintenance start_module cb_storage
```

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
`modb` | schema for database storage plan | [#/definitions/storage.plan.database](#storageplan.database) |   | `false` |  
`system` | schema for database storage plan | [#/definitions/storage.plan.database](#storageplan.database) |   | `false` |  

### storage.plan.database

schema for database storage plan


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`attachments` | schema for attachment ref type storage plan | [#/definitions/storage.plan.database.attachment](#storageplan.database.attachment) |   | `false` |  
`connection` |   | `string()` |   | `false` |  
`database.create_options` |   | `object()` |   | `false` |  
`database` |   | `object()` |   | `false` |  
`types.call_recording` | schema for document type storage plan | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false` |  
`types.fax` | schema for document type storage plan | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false` |  
`types.mailbox_message` | schema for document type storage plan | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false` |  
`types.media` | schema for document type storage plan | [#/definitions/storage.plan.database.document](#storageplan.database.document) |   | `false` |  
`types` |   | `object()` |   | `false` |  

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

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "attachments": {
            "{UUID}": {
                "handler": "s3",
                "name": "S3 Storage",
                "settings": {
                    "bucket": "{S3_BUCKET}",
                    "key": "{AWS_ACCESS_KEY}",
                    "secret": "{AWS_SECRET_KEY}"
                }
            }
        },
        "id": "{ACCOUNT_ID}",
        "plan": {
            "modb": {
                "types": {
                    "mailbox_message": {
                        "attachments": {
                            "handler": "{UUID}"
                        }
                    }
                }
            }
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

For instance, setting up your HTTP server to receive new voicemails for the account:

```json
{
  "data": {
    "attachments": {
      "{UUID}": {
        "handler": "http",
        "name": "My HTTP server",
        "settings": {
          "url": "http://my.http.server:37635/some_prefix",
          "verb": "post"
        }
      }
    },
    "plan": {
      "modb": {
        "types": {
          "mailbox_message": {
            "attachments": {
              "handler": "{UUID}"
            }
          }
        }
      }
    }
  }
}
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

## Skipping attachment settings validation

When a storage plan is PUT/POSTed to Crossbar, KAZOO will attempt to use the attachments' settings and store a small text file to verify that files can be stored remotely. KAZOO will then issue a GET request to read the file back to test retrieval.

For "dumb" storage backends this is typically a non-issue as storing/retrieving files is what the backend does!

For "smart" backends, where a custom handler (like an HTTP web app) is accepting the files, adding code to handle this test file's storage/retrieval could place an unnecessary burden on the backend or be redundant after the first test if using the same destination for all accounts. As such, a request parameter can be included to skip this portion of the validation:

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{...}}' \
  http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage?validate_settings=false
```

!!! danger
If the storage backend is unable to process the storage request, you could lose the data attempting to be stored.

### Enabling This Feature

By default, Kazoo will not allow clients to skip settings validation. Clients that include the `validate_settings` request parameter on these systems will receive a 400 validation error indicating attachment storage settings must be tested.

Sysadmins can allow clients by setting a `system_config` flag: `sup kzs_plan allow_validation_overrides`

Disabling it later is similar: `sup kzs_plan disallow_validation_overrides`


## URL formatting

It is possible to craft the URLs used by the handler based on the JSON document and attachment being saved by specifying a `field_list` array of objects that will help KAZOO map values to the generated URL:

```json
{UUID}:{
    "handler":"{HANDLER}",
    "settings":{
        "field_list":[
            {"arg":"account_id"}
            ,{"arg":"id"}
            ,{"arg":"attachment"}
        ],
        "url":"http://base.your.domain/"
    }
}
```

In this case (the default for the HTTP handler) the URL provided in the handler's settings will be appended with `/{ACCOUNT_ID}/{DOC_ID}/{ATTACHMENT_NAME}`.

#### Field Options

list of fields to compose destination url



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`arg` | a argument passed to the handler | `string('account_id' | 'db' | 'id' | 'attachment')` |   | `false` |
`const` | a constant value added to the string | `string()` |   | `false` |
`field` | a field from the metadata document | `string()` |   | `false` |
`group` | group the inner fields definitions with an empty separator | `array()` |   | `false` |



#### Examples

Given a base URL of `http://my_server.com/storage`, an attachment `call.mp3` being stored in the `account000` account on the `abc123` doc defined below:

```json
{"_id":"abc123"
 ,"foo":"bar"
 ,"bing":"bang"
}
```

We can create the following generated URLs:

| URL                                                           | `field_list`                                                                     |   |
| http://my_server.com/storage/account000/abc123/call.mp3       | `[{"arg":"account_id"}, {"arg":"id"}, {"arg":"attachment"}]`                     |   |
| http://my_server.com/storage?path=/account000/abc123/call.mp3 | `[{"const":"?path="}, {"arg":"account_id"}, {"arg":"id"}, {"arg":"attachment"}]` |   |
| http://my_server.com/storage/bar/call.mp3 | `[{"field":"foo"}, {"arg":"attachment"}]` |   |
| http://my_server.com/storage/account001_call.mp3 | `[{"group":[{"arg":"account_id"}, {"const":"_"}, {"arg":"attachment"}]}]` |   |
