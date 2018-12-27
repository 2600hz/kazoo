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

## Schema



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^_` | Ignores CouchDB fields prefixed by underscores | `string() | integer() | boolean() | object()` |   | `false` |
`^pvt_` | Ignores Kazoo private fields prefixed by pvt_ | `string() | integer() | boolean()` |   | `false` |
`attachments` | Defines where and how to store attachments | [#/definitions/storage.attachments](#storageattachments) |   | `false` |
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
`settings.key` | the azure API key | `string()` |   | `true` |
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
`name` | Friendly name for this attachment handler | `string()` |   | `false` |
`settings.url` | The base HTTP(s) URL to use when creating the request | `string()` |   | `true` |
`settings.verb` | The HTTP verb to use when sending the data | `string('POST' | 'PUT')` | `POST` | `false` |
`settings` | HTTP server settings | `object()` |   | `true` |

### storage.attachment.onedrive

schema for OneDrive attachment entry


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`handler` | What handler module to use | `string('onedrive')` |   | `true` |
`settings.oauth_doc_id` | Doc ID in the system 'auth' database | `string(1..)` |   | `true` |
`settings` | Settings for the OneDrive account | `object()` |   | `true` |

### storage.attachments

Keys are 32-character identifiers to be used in storage plans


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^[a-z0-9]{32}$.field_list` | list of field to compose destination url | `array()` |   | `false` |
`^[a-z0-9]{32}$.field_separator` | top-level, field separator to compose destination url | `string()` |   | `false` |
`^[a-z0-9]{32}$.folder_base_path` | base folder path | `string()` |   | `false` |
`^[a-z0-9]{32}$.name` | Friendly name for this configuration | `string()` |   | `false` |
`^[a-z0-9]{32}$` | Configuration for the supported storage backends | `object()` |   | `false` |

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


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^([a-z,0-9]){32}$` |   | [#/definitions/storage.connection.couchdb](#storageconnection.couchdb) |   | `false` |
`local` |   | `object()` |   | `false` |

### storage.plan

schema for storage plan


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`account` |   | [#/definitions/storage.plan.database](#storageplan.database) |   | `false` |
`modb` |   | [#/definitions/storage.plan.database](#storageplan.database) |   | `false` |
`system` |   | [#/definitions/storage.plan.database](#storageplan.database) |   | `false` |

### storage.plan.database

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
`attachments` |   | [#/definitions/storage.plan.database.attachment](#storageplan.database.attachment) |   | `false` |
`connection` |   | `string()` |   | `false` |

#### Create

First, you should create a 32-character UUID to identify the S3 configuration:

```shell
echo $(tr -dc a-f0-9 < /dev/urandom | dd bs=32 count=1 2> /dev/null)
403f90f67d1b71341f2ea6426eed3d90
```

Using your `{UUID}`, create the storage document using the `{UUID}` as the reference to the S3 configuration parameters.

> PUT /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
 -d '{"data":{
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
        }}' \
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
          "verb": "POST"
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
