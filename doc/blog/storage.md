- [Alternative Storage Options for Kazoo](#org8916b87)
  - [Scenarios](#orgdbd1b36)
  - [Introducing Storage Plans](#orga265582)
  - [Getting started](#org6770240)
    - [Create the initial storage](#org8386c70)
    - [Add S3 configuration](#org889959c)
  - [Creating Storage Plans](#orge72995a)
    - [MODB plan](#org5099381)
    - [Mailbox message attachments](#org782a1d9)
    - [Adding the plan](#org150e76d)
  - [Wrapping Up](#orga6745b5)



<a id="org8916b87"></a>

# Alternative Storage Options for Kazoo

Kazoo has long been opinionated about where it stores its data - [CouchDB](https://en.wikipedia.org/wiki/CouchDB). Historically, the project relied on [BigCouch](https://en.wikipedia.org/wiki/BigCouch) but with the release of CouchDB 2.0 and Kazoo 4.0, modern installations of Kazoo can leverage the new codebase.

However, while CouchDB serves the majority of Kazoo's needs well, there are scenarios where CouchDB isn't the best option. We'll try to cover some of those scenarios as well as provide step-by-step instructions for how to take advantage of alternative storage options.


<a id="orgdbd1b36"></a>

## Scenarios

The most frequent request is to put call recordings (and voicemail) into a company's existing storage system, be it [Google Drive](https://en.wikipedia.org/wiki/Google_Drive), [Amazon S3](https://en.wikipedia.org/wiki/Amazon_S3), or what have you. This makes a lot of sense for companies with compliance requirements, service providers that want to use the media (perhaps run voicemail through an [ASR engine](https://en.wikipedia.org/wiki/Speech_recognition) and provide a transcript), or folks that just want to save a sweet voicemail from their loved one somewhere they can enjoy it at their leisure.

Other requests that have come up have been having the ability to save [CDRs](https://en.wikipedia.org/wiki/Call_detail_record) directly to a provider's database or spreadsheet. This eases the provider's ability to bill, analyze usage patterns, etc.

Other possibilities include:

-   Sending and receiving faxes (add a PDF to your Google Drive, have it faxed automatically!)
-   Store recorded calls for compliance, training, or other purposes.
-   Serve custom music-on-hold or other recordings for your customers, allowing you to manage them through your storage provider's interface.
-   Delete voicemail message from your mailbox when you delete the file from storage.
-   And many more!


<a id="orga265582"></a>

## Introducing Storage Plans

With Kazoo 4.0, the 'storage plan' concept is introduced to allow system administrators, resellers, and account holders, to configure alternative storage options for their needs. Storage is configured in two phases:

1.  Configure 'storage', what storage engines are available (such as adding AWS credentials for S3).
2.  Configure 'plans', to determine what types of data will use which configured 'storage' option.


<a id="org6770240"></a>

## Getting started

System administrators will need to enable the 'storage' endpoint in Crossbar:

```sh
sup crossbar_maintenance start_module cb_storage
```

Once started, query the base storage endpoint:

```sh
curl -v -H "X-Auth-Token: $AUTH_TOKEN" \ "http://localhost:8000/v2/accounts/$ACCOUNT_ID/storage"
```

```json
{
  "data": {
    "cause": "{ACCOUNT_ID}",
    "message": "bad identifier"
  },
  "error": "404",
  "message": "bad_identifier",
  "status": "error",
  "request_id": "{REQUEST_ID}",
  "auth_token": "{AUTH_TOKEN}"
}
```

What happened?

Well, the storage hasn't been configured yet, so of course it wasn't found.


<a id="org8386c70"></a>

### Create the initial storage

Let's create the initial storage configuration document:

```sh
curl -v -X PUT \
 -H "X-Auth-Token: $AUTH_TOKEN" \
 -d '{"data":{}}'
 "http://localhost:8000/v2/accounts/$ACCOUNT_ID/storage"
```

```json
{
  "data": {
    "id": "{ACCOUNT_ID}"
  },
  "revision": "1-f0b539057db194c04f76cd1e0a2144cd",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}
}
```


<a id="org889959c"></a>

### Add S3 configuration

First, we need a 32-character UUID which will be the ID of the S3 configuration that we'll use later in the storage plan setup. Creating a UUID is easy to do with the following cURL command:

```sh
curl https://www.uuidgenerator.net/api/version4/1 | sed 's/-//g'
e3e26e06a4b1465599e6dc9e1516fd8b
```

We'll use the UUID to PATCH the 'storage' document with our 'attachments' object. First, the 'storage' JSON to PATCH:

```json
{
    "attachments": {
        "{UUID}":{
            "handler":"s3",
            "name":"Kazoo S3 storage config",
            "settings":{
                "bucket":"my_kazoo_bucket_name",
                "key":"{AWS_ACCESS_KEY}",
                "secret":"{AWS_SECRET_KEY}"
            }
        }
    }
}
```

Let's now add that to the JSON envelope and PATCH it in:

```sh
curl -v -X PATCH \
 -H "X-Auth-Token: $AUTH_TOKEN" \
 -d '{"data":{
    "attachments": {
        "{UUID}":{
            "handler":"s3",
            "name":"Kazoo S3",
            "settings":{
                "bucket":"{S3_BUCKET}",
                "key":"{AWS_ACCESS_KEY}",
                "secret":"{AWS_SECRET_KEY}"
            }
        }
    }
}}' \
 "http://localhost:8000/v2/accounts/$ACCOUNT_ID/storage"
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "attachments": {
            "{UUID}": {
                "handler": "s3",
                "name": "Kazoo S3",
                "settings": {
                    "bucket": "{S3_BUCKET}",
                    "key": "{AWS_ACCESS_KEY}",
                    "secret": "{AWS_SECRET_KEY}"
                }
            }
        },
        "id": "{ACCOUNT_ID}"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "1-f0b539057db194c04f76cd1e0a2144cd",
    "status": "success"
}
```

Success! We now have an attachment configuration at '{UUID}' to reference elsewhere.


<a id="orge72995a"></a>

## Creating Storage Plans

Now that we've created a storage configuration for S3 we can create a storage plan for our account. This will allow us to specify what types of data we want to use our S3 backend.

There are three database categories available for configuration:

1.  Account - documents that would be stored in the account's database, like configuration data for devices, users, etc.
2.  Month-only (modb) - temporal data related to an account (CDRs, recordings, etc)
3.  System - for administrators, store system data elsewhere.

For our purposes, we'll focus on the modb plan to store our voicemails.

Our base "plan" will look like this:

```json
{
    "plan":{
        "modb":{
        }
    }
}
```


<a id="org5099381"></a>

### MODB plan

For the modb plan, we want to only store certain types of attachments (voicemails) so we define a 'types' object that will configure Kazoo to store voicemails to our S3. In Kazoo, voicemails are known as 'mailbox messages' since we plan on supporting video voicemail and other things in the future.

Augmenting our plan to add the 'types' restriction:

```json
{
    "plan":{
        "modb":{
            "types":{
                "mailbox_message":{
                }
            }
        }
    }
}
```


<a id="org782a1d9"></a>

### Mailbox message attachments

We just want to store the binary data (and not the metadata) in S3, so we'll define an 'attachments' object to point to S3. This is done in the 'handler' property and will use the UUID we defined above:

```json
{
    "plan":{
        "modb":{
            "types":{
                "mailbox_message":{
                    "attachments":{
                        "handler":"{UUID}"
                    }
                }
            }
        }
    }
}
```


<a id="org150e76d"></a>

### Adding the plan

Just as we did with the S3 configs, we'll PATCH our storage document to add our plan:

```sh
curl -v -X PATCH -H "content-type: application/json" -H "X-Auth-Token: $AUTH_TOKEN" "http://localhost:8000/v2/accounts/$ACCOUNT_ID/storage" -d '{"data":{"plan":{
        "modb":{
            "types":{
                "mailbox_message":{
                    "attachments":{
                        "handler":"e3e26e06a4b1465599e6dc9e1516fd8b"
                    }
                }
            }
        }
    }}}'
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "attachments": {
            "{UUID}": {
                "handler": "s3",
                "name": "Kazoo S3",
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
    "revision": "7-ec3ee8ff8893e59ad8064bcbe684bde5",
    "status": "success"
}
```


<a id="orga6745b5"></a>

## Wrapping Up

Now that you've created the storage plan for the account, try placing a call to leave a voicemail. Once left, you should see a file appear in your S3 bucket within a minute or two!

There's a lot more to storage than just pushing voicemails to S3. We'll cover those in future articles!