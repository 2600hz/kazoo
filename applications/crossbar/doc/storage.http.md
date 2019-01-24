# HTTP Storage Backend

## Using the HTTP storage backend

When you maintain your own web server, you can opt to store attachments like voicemail messages on your system (letting you provide additional services to your customers).

### Create the storage backend

First, create a UUID:

```shell
echo $(tr -dc a-f0-9 < /dev/urandom | dd bs=32 count=1 2> /dev/null)
403f90f67d1b71341f2ea6426eed3d90
```

This UUID will be your reference to your HTTP server in the storage plan.

There are two main pieces of the storage plan to configure now: the `attachments` where you'll define the HTTP server information, and the `plan` where you'll configure KAZOO to use your HTTP server.

For instance, you can store new voicemails with a storage config like the following:
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

PUT-ing this to `/v2/accounts/{ACCOUNT_ID}/storage` will result in your web server receiving a PUT/POST to `/some_prefix/{ACCOUNT_ID}/{TEST_ID}/{RANDOM}_test_credentials_file.txt`. The text file will contain something like `some random content: {RANDOM}`. Respond with a 201 to let KAZOO know the reception occurred.

Next, KAZOO will attempt to GET that attachment back. Your web server will see a request for `/some_prefix/{ACCOUNT_ID}/{TEST_ID}/{RANDOM}_test_credentials_file.txt` and expects to see a 200 OK and the contents.

If both the PUT/POST and the GET are successful, the API request to create the storage config will return a 201. You can now safely delete `{RANDOM}_test_credentials_file.txt` from your web server.

### On save

Now, when a voicemail (for instance) is saved to the account, your web server will receive a PUT/POST request to `PUT req /some_prefix/{ACCOUNT_ID}/{MESSAGE_ID}/uploaded_file_{TIMESTAMP}.mp3` with the binary data as the body. Your web server will then need to respond with a 201 to let KAZOO know storing the data was successful.

!!! note
Save processing of the file for a later process; return the 201 to KAZOO as soon as your server confirms storing the file was successful locally.

### Multipart requests

If you want to receive both the binary data and the JSON metadata, you can add `"send_multipart":true` to the settings of the handler:

```json
"{UUID}": {
    "handler": "http",
    "name": "My HTTP server",
    "settings": {
        "url": "http://my.http.server:37635/some_prefix",
        "verb": "post",
        "send_multipart":true
    }
}
```

On save, KAZOO will send a `multipart/mixed` request that will something like:

```
{BOUNDARY}
content-type: application/json

{"name":"mailbox 1010 message MM-DD-YYYY HH:MM:SS","description":"voicemail message with media","source_type":"voicemail","source_id":"{SOURCE_ID}","media_source":"recording","streamable":true,"utc_seconds":{TIMESTAMP},"metadata":{"timestamp":{TIMESTAMP},"from":"{SIP_FROM}","to":"{SIP_TO}","caller_id_number":"{CID_NUMBER}","caller_id_name":"{CID_NAME}","call_id":"{CALL_ID}","folder":"new","length":1,"media_id":"{MEDIA_ID}"},"id":"{MEDIA_ID}"}
{BOUNDARY}
content-type: audio/mp3

{BINARY_DATA}
{BOUNDARY}
```

When the attachment is fetched (say via the `/vmboxes` API endpoint), the HTTP server will receive a GET and will need to return the binary data (not the multipart if using for the storage portion).

### Base64 encoding

If the backend needs to receive the attachment binary as a base64-encoded value, the storage settings can include `base64_encode_data`:

```json
        "settings": {
          "url": "http://my.http.server:37635/some_prefix",
          "verb": "post",
          "base64_encode_data":true
        }
```

When fetching the attachment back from your server, you can return either the raw binary or the base64-encoded version. KAZOO will decode it if necessary.
