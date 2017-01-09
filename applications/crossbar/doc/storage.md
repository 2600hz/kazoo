### Storage

#### About Storage

Storage plans allow an account to control where data related to their account is stored. This can be critical when compliance with regulations is required.

A storage plan has three main components:

1. `attachments`: configuration for where to store attachments (binary data typically, like voicemails or faxes)
2. `connections`: connection information to various third-party storage sites
3. `plan`: a description of the storage plan(s) for the account.

#### Attachments

Rather than storing binary data like voicemails, received faxes, and call recordings, in the Kazoo databases, it can be convenient to store them in third-party storage services like [Amazon S3](https://aws.amazon.com/s3), [Google Drive](https://www.google.com/drive/), etc. This keeps the binary data in a place that an account or user maintains control over. Kazoo keeps a pointer to the location when it needs to fetch the binary data (such as when you call into your voicemail box).

The `attachments` object configures storage backends - for instance, if you want to store to S3, you'll add your AWS secret and key and your S3 bucket information here.

#### Connections

Connections can be used to point to an alternative CouchDB instance for storing the JSON documents or attachments (for instance: putting CDRs in their own cluster). These can be specified in the storage plans.

#### Plans

Plans determine what to do with certain classes of databases:

1. `account`: where to store account data
2. `modb`: where to store temporal data, like CDRs or voicemails
3. `system`: where to store system data, like prompts

Within the `database` classification, you can define things like the connection to use when reading/writing, what types of documents should be stored/retrieved, etc.

#### Enabling the storage endpoint

Crossbar must have the storage endpoint enabled first:

```
sup crossbar_maintenance start_module cb_storage
```

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`attachments` | Defines where and how to store attachments | [#/definitions/storage.attachments](#storageattachments) |   | `false`
`connections` | Describes alternative connections to use (such as alternative CouchDB instances | [#/definitions/storage.connections](#storageconnections) |   | `false`
`id` | ID of the storage document | `string` |   | `false`
`plan` | Describes how to store documents depending on the database or document type | [#/definitions/storage.plan](#storageplan) |   | `false`


##### storage.attachment.aws

schema for AWS attachment entry

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`handler` | What AWS service to use | `string('s3')` |   | `true`
`name` | Friendly name for this configuration | `string` |   | `false`
`settings` | AWS API settings | `object` |   | `true`
`settings.bucket` | Bucket name to store data to | `string` |   | `true`
`settings.host` | Region-specific hostname to use, if applicable | `string` |   | `false`
`settings.key` | AWS Key to use | `string` |   | `true`
`settings.path` | Custom path to use as a prefix when saving files | `string` |   | `false`
`settings.secret` | AWS Secret to use | `string` |   | `true`

##### storage.attachment.google_drive

schema for google drive attachment entry

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`handler` | What handler module to use | `string('google_drive')` |   | `true`
`name` | Friendly name for this configuration | `string` |   | `false`
`settings` | Settings for the Google Drive account | `object` |   | `true`
`settings.folder_id` | Folder ID in which to store the file, if any | `string` |   | `false`
`settings.oauth_doc_id` | Doc ID in the system 'oauth' database | `string` |   | `true`

##### storage.attachments

Keys are 32-character identifiers to be used in storage plans

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------

##### storage.connection.couchdb

schema for couchdb connection entry

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

schema for storage plan

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`account` |   | [#/definitions/storage.plan.database](#storageplan.database) |   | `false`
`modb` |   | [#/definitions/storage.plan.database](#storageplan.database) |   | `false`
`system` |   | [#/definitions/storage.plan.database](#storageplan.database) |   | `false`

##### storage.plan.database

schema for database storage plan

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

schema for attachment ref type storage plan

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`handler` |   | `string` |   | `false`
`params` |   | `object` |   | `false`
`stub` |   | `boolean` |   | `false`

##### storage.plan.database.document

schema for document type storage plan

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`attachments` |   | [#/definitions/storage.plan.database.attachment](#storageplan.database.attachment) |   | `false`
`connection` |   | `string` |   | `false`



#### List storage settings

> GET /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

```json
{
    "auth_token": "{AUTH_TOKEN}"
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
