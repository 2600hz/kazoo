# Recording

Recording the caller (or caller and callee in a bridged-call scenario) is straightforward to start. What's trickier is how to store the recording.

## Recording Caller/Callee situations

### Starting recording

```json
{
    "module": "record_call",
    "data": {
        "action": "start",
        "time_limit": 1200,
        "format": "mp3",
        "url": "http://your.server.com/recordings"
    }
}
```

This will start the call recording, limiting it to 1200 seconds, and will encode the audio into an MP3 file (alternatively, you can use "wav"). The `url` is where the resulting file will be sent via an HTTP PUT request. It is then up to the receiving server to properly handle the request and store the file for later use.

Note: `time_limit` is constrained by the `system_config/media` doc's `max_recording_time_limit` entry (default is 10800 seconds). If your recordings are not long enough, that is the setting that needs increasing.

Note: `url` will be used as the base URL for the resulting PUT. The final URL will be `URL/call_recording_CALL_ID.EXT` where `URL` is the supplied URL, `CALL_ID` is the call ID of the A-leg being recorded, and `EXT` is the `format` parameter.

Note: If `url` is not provided, Kazoo will check the `system_config/media` doc for the `store_recordings` flag. If "false", the recording will not be stored (kinda pointless). If "true", Kazoo will store the recording into the account's database.

### Stop recording

If you need to programmatically stop the current recording (vs implicitly when the call ends):

```json
{
    "module": "record_call",
    "data": {
        "action": "stop"
    }
}
```

### Sample Recording Per User

```json
{
    "module": "record_call",
    "data": {
        "action": "start",
        "format": "mp3",
        "url": "http://my.recording.server/{ACCOUNT_ID}/{USER_ID}",
        "time_limit":360
    },
    "children": {
        "_": {
            "module": "user",
            "data": {
                "id": "{USER_ID}"
            },
            "children": {
                "_": {
                    "module": "record_call",
                    "data": {
                        "action": "stop"
                    },
                    "children": {
                        "module": "voicemail",
                        "data": {
                            "id": "{VMBOX_ID}"
                        },
                        "children": {}
                    }
                }
            }
        }
    }
}
```

Note: Call recording and Voicemail do not play well together. You will need to stop the recording before voicemail to avoid conflict.

## Recording just the caller

This action is more appropriate for recording just the caller (think voicemail or recording menu prompts).

```json
{
    "module": "say",
    "data": {
        "text": "Please leave your message after the beep"
    },
    "children": {
        "_": {
            "module": "record_caller",
            "data": {
                "format": "mp3",
                "url": "http://my.recording.server/voicemail/{ACCOUNT_ID}/{BOX_ID}",
                "time_limit":360
            }
        }
    }
}
```

## Receiving a recording

Here is a simple PHP/`.htaccess` combo for receiving a recording.

1. Assume `url` in our `data` object is "http://your.server.com/kzr"
2. Create a `.htaccess` file in the DocumentRoot. This will direct the request to `/kzr/index.php` with the `recording` query string parameter set to `CALL_ID.EXT`.

```
        <IfModule mod_rewrite.c>
            RewriteEngine On
            RewriteBase /
            RewriteRule ^kzr/call_recording_(.+)\.(.+)$  kzr/index.php?recording=$1.$2 [QSA,L]
        </IfModule>
```

3. Create `/kzr/index.php` to receive and store the recording.

```php
<?php
/* PUT data comes in on the stdin stream */
$putdata = fopen("php://input", "r");

$r = $_REQUEST["recording"];
/* Open a file for writing */
$fp = fopen("/tmp/$r", "w");
/* Read the data 1 KB at a time and write to the file */
while ($data = fread($putdata, 1024))
    fwrite($fp, $data);

/* Close the streams */
fclose($fp);
fclose($putdata);
?>
```

A file should be created in `/tmp/` named `CALL_ID.EXT`. You could, of course, store this in MySQL, Postgres, S3, feed it to a transcription service, etc.
