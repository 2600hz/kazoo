# Media

## About Media

Uploading media for custom music on hold, IVR prompts, or TTS (if a proper TTS engine is enabled).


Kazoo provides some default system media files for common things like voicemail prompts. These are accessible via the media Crossbar endpoint as well, if your user has super duper admin privileges. To manipulate those resources, simply omit the `/accounts/{ACCOUNT_ID}` from the URI.

For example, to get a listing of all system media files:

```shell
curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media
```

You can then get the `id` of the media file and manipulate it in a similar fashion as regular account media (including TTS if you have a TTS engine like iSpeech configured).

### Media Languages

Part of the schema of media files is a language attribute. It defaults to a `system_config/media` value for the `default_language` key (and is `"en-us"` by default). Properly defined media files can be searched for based on language using the basic filters provided by Crossbar:

```shell
curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media?filter_language=en
curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media?filter_language=en-US
curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media?filter_language=fr-FR
```

The comparison is case-insensitive, but `en` and `en-US` are treated separately. If a media metadata object is missing a language attribute (on an older installation when system media was imported with no language field, say), use `key_missing=language` in the request.


Once you've assigned languages, you can use the [`language` callflow action](../../callflow/doc/language.md) to set the language for that call.

### Normalize Media Files

Kazoo can be configured to normalize uploaded media files. This can fix things like:

* Normalizing volume
* Fix clipping
* Standardize formats

By default, if enabled, normalization will convert all media to MP3 (retaining the original upload as well) using the [*sox* utility](http://sox.sourceforge.net/) to accomplish the conversion.

#### Enable Normalization Via SUP

* Enable normalization for this particular server: `sup kapps_config set crossbar.media normalize_media true`
* Enable normalization for all servers: `sup kapps_config set_default crossbar.media normalize_media true`

#### Enable Normalization Via DB

1. Open `system_config/crossbar.media` document, create or update the key `normalize_media` to `true`.
2. Flush the `kapps_config` cache, `sup kapps_config flush crossbar.media`, on all servers running Crossbar.

#### Set Target Format Via SUP

* For the server: `sup kapps_config set crossbar.media normalization_format ogg`
* For all servers: `sup kapps_config set_default crossbar.media normalization_format ogg`

#### Set Target Format Via DB

In the `system_config/crossbar.media` document, create or update the key `normalization_format` to your desired format (`mp3`, `wav`, etc). Flush the kapps_config cache on all servers running Crossbar. All new uploads will be normalized (if possible) to the new format.


### Normalization parameters

The default *sox* command is `sox -t <input_format> - -r 8000 -t <output_format> -` but this is configurable via the `system_config/media` document (or similar SUP command).

You can fine-tune the source and destination arguments using the `normalize_source_args` and `normalize_destination_args` keys respectively. By default, the source args are "" and the destination args are "-r 8000" (as can be seen from the default sox command above.

The normalizer code uses stdin to send the binary data to sox and reads from stdout to get the normalized binary data back (the " - " (there are 2) in command above).

You can also set the specific path for `sox` in the `normalize_executable` key, in case you've installed it to a non-standard path.

Be sure to install sox with mp3 support! Conversion will not happen (assuming you're targeting mp3) if sox can't write the mp3. You can check the media meta document for the key `normalization_error` if sox failed for some reason.


#### Schema

Schema for media



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`content_length` | Length, in bytes, of the file | `integer()` |   | `false` | `supported`
`content_type` | Used to override the automatic upload type | `string('audio/mp3' | 'audio/mpeg' | 'audio/mpeg3' | 'audio/x-wav' | 'audio/wav' | 'audio/ogg' | 'video/x-flv' | 'video/h264' | 'video/mpeg' | 'video/quicktime' | 'video/mp4' | 'video/webm')` |   | `false` | `supported`
`description` | A brief description of the media update, usually the original file name | `string(1..128)` |   | `false` | `supported`
`language` | The language of the media file or text | `string()` | `en-us` | `false` | `supported`
`media_source` | Defines the source of the media | `string('recording' | 'upload' | 'tts')` | `upload` | `true` | `supported`
`name` | A friendly name for the media | `string(1..128)` |   | `true` | `supported`
`prompt_id` | The prompt this media file represents | `string()` |   | `false` |  
`source_id` | If the media was generated from a callflow module, this is ID of the properties | `string(32)` |   | `false` | `beta`
`source_type` | If the media was generated from a callflow module, this is the module name | `string()` |   | `false` | `beta`
`streamable` | Determines if the media can be streamed | `boolean()` | `true` | `false` | `supported`
`tts.text` | The text to be converted into audio | `string(1..)` |   | `false` | `supported`
`tts.voice` | The voice to be used during the conversion | `string('female/en-US' | 'male/en-US' | 'female/en-CA' | 'female/en-AU' | 'female/en-GB' | 'male/en-GB' | 'female/es-US' | 'male/es-US' | 'female/us-US' | 'female/zh-CN' | 'male/zh-CN' | 'female/zh-HK' | 'female/zh-TW' | 'female/ja-JP' | 'male/ja-JP' | 'female/ko-KR' | 'male/ko-KR' | 'female/da-DK' | 'female/de-DE' | 'male/de-DE' | 'female/ca-ES' | 'female/es-ES' | 'male/es-ES' | 'female/fi-FI' | 'female/fr-CA' | 'male/fr-CA' | 'female/fr-FR' | 'male/fr-FR' | 'female/it-IT' | 'male/it-IT' | 'female/nb-NO' | 'female/nl-NL' | 'female/pl-PL' | 'female/pt-BR' | 'female/pt-PT' | 'male/pt-PT' | 'female/ru-RU' | 'male/ru-RU' | 'female/sv-SE' | 'female/hu-HU' | 'female/cs-CZ' | 'female/tr-TR' | 'male/tr-TR' | 'male/ru-RU/Vladimir' | 'female/ru-RU/Julia' | 'female/ru-RU/Anna' | 'female/ru-RU/Viktoria' | 'male/ru-RU/Alexander' | 'female/ru-RU/Maria' | 'female/ru-RU/Lidia' | 'es-ES-Standard-A' | 'it-IT-Standard-A' | 'ja-JP-Standard-A' | 'ko-KR-Standard-A' | 'pt-BR-Standard-A' | 'tr-TR-Standard-A' | 'sv-SE-Standard-A' | 'nl-NL-Standard-A' | 'en-US-Wavenet-D' | 'de-DE-Wavenet-A' | 'de-DE-Wavenet-B' | 'de-DE-Wavenet-C' | 'de-DE-Wavenet-D' | 'en-AU-Wavenet-A' | 'en-AU-Wavenet-B' | 'en-AU-Wavenet-C' | 'en-AU-Wavenet-D' | 'en-GB-Wavenet-A' | 'en-GB-Wavenet-B' | 'en-GB-Wavenet-C' | 'en-GB-Wavenet-D' | 'en-US-Wavenet-A' | 'en-US-Wavenet-B' | 'en-US-Wavenet-C' | 'en-US-Wavenet-E' | 'en-US-Wavenet-F' | 'fr-FR-Wavenet-A' | 'fr-FR-Wavenet-B' | 'fr-FR-Wavenet-C' | 'fr-FR-Wavenet-D' | 'it-IT-Wavenet-A' | 'ja-JP-Wavenet-A' | 'nl-NL-Wavenet-A' | 'en-GB-Standard-A' | 'en-GB-Standard-B' | 'en-GB-Standard-C' | 'en-GB-Standard-D' | 'en-US-Standard-B' | 'en-US-Standard-C' | 'en-US-Standard-D' | 'en-US-Standard-E' | 'de-DE-Standard-A' | 'de-DE-Standard-B' | 'en-AU-Standard-A' | 'en-AU-Standard-B' | 'en-AU-Standard-C' | 'en-AU-Standard-D' | 'fr-CA-Standard-A' | 'fr-CA-Standard-B' | 'fr-CA-Standard-C' | 'fr-CA-Standard-D' | 'fr-FR-Standard-A' | 'fr-FR-Standard-B' | 'fr-FR-Standard-C' | 'fr-FR-Standard-D')` | `female/en-US` | `false` | `supported`
`tts` | Text-to-speech options used to create audio files from text | `object()` | `{}` | `false` | `supported`



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/media

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{MEDIA_ID}",
            "is_prompt": false,
            "language": "en-us",
            "media_source": "tts",
            "name": "Main AA BG"
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


## Create a new media object (required before uploading the actual media data)

> PUT /v2/accounts/{ACCOUNT_ID}/media

* For a file:

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{
        "streamable":true,
        "name": "File",
        "description": "My Test Media File",
        }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media
```

* For a prompt:

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{
        "streamable": true,
        "name": "FR-vm-enter_pass",
        "description": "FR - Enter Password prompt",
        "prompt_id": "vm-enter_pass",
        "language":"fr"
        }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media
```

* For a TTS document: (requires iSpeech to be enabled)

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{
        "name": "TestTTS",
        "media_source": "tts",
        "tts": {"text": "Testing TTS", "voice": "female/en-US"}
        }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media
```

A response:

```json
{
    "data":
    {
        "streamable": true,
        "name": "vm-enter_pass",
        "description": "FR - Enter Password prompt",
        "prompt_id": "vm-enter_pass",
        "language": "fr-fr",
        "tts": {
            "voice": "female/en-US"
        },
        "media_source": "upload",
        "id": "fr-fr%2Fvm-enter_pass"
    },
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

## Remove metadata

Optional Parameter: "hard_delete": true - will perform a hard delete of the document (default is soft delete)

> DELETE /v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}
```

## Get metadata about a media file

> GET /v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "description": "tts file",
        "id": "{MEDIA_ID}",
        "language": "en-us",
        "media_source": "tts",
        "name": "Main AA BG",
        "streamable": true,
        "tts": {
            "text": "Thank you for calling My Amazing Company where we do amazing things. You may dial any extension at any time. To schedule an appointment, press 1. For billing questions about your account, press 2. For all other inquiries, press 0.  To hear this menu again, please stay on the line.",
            "voice": "female/en-US"
        },
        "ui_metadata": {
            "origin": "callflows",
            "ui": "monster-ui",
            "version": "4.0-7"
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


## Update metadata

> POST /v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}
```

## List all prompts and the number of translations existing

> GET /v2/accounts/{ACCOUNT_ID}/media/prompts

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/prompts
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "agent-already_logged_in": 1,
            "agent-enter_pin": 1,
            "agent-invalid_choice": 1,
            "agent-logged_in": 1,
            "agent-logged_out": 1,
            "agent-not_call_center_agent": 1,
            "agent-pause": 1,
            "agent-resume": 1,
            "agent_enter_pin": 1,
            "agent_logged_already_in": 1,
            "agent_logged_in": 1,
            "agent_logged_out": 1,
            "cf-disabled": 1,
            "cf-disabled_menu": 1,
            "cf-enabled_menu": 1,
            "cf-enter_number": 1,
            "cf-move-no_channel": 1,
            "cf-move-no_owner": 1,
            "cf-move-too_many_channels": 1,
            "cf-not_available": 1,
            "cf-now_forwarded_to": 1,
            "cf-unauthorized_call": 1,
            "conf-alone": 1,
            "conf-bad_conf": 1,
            "conf-bad_pin": 1
        }
    ],
    "next_start_key": "conf-deaf",
    "page_size": 25,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## List languages available

> GET /v2/accounts/{ACCOUNT_ID}/media/languages

This request will return a list of languages found, as well as the counts of how many media files have that language defined:

Note, the "missing" key indicates how many media files have no associated language.


```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/languages
```

```json
{
    "data": [{ "en": 3
               ,"missing": 1
             }
            ],
}
```

## Get the raw media file

Streams back an the uploaded media.

> GET /v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Accept: audio/mp3' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/
```

!!! note
    There is a deprecated but maintained URL, `GET /v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/raw`, as well.

## Add the media binary file to the media meta data

> POST /v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: audio/mp3' \
    --data-binary @/path/to/file.mp3 \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/
```
```json
{
  "auth_token": "{AUTH_TOKEN}",
  "data": {
    "id": "{MEDIA_ID}",
    "language": "{LANG}",
    "media_source": "upload",
    "name": "{FRIENDLY_NAME}",
    "streamable": true,
    "tts": {
      "voice": "female/en-US"
    }
  },
  "node": "{NODENAME}",
  "request_id": "{REQUEST_ID}",
  "revision": "{REVISION}",
  "status": "success",
  "timestamp": "{TIMESTAMP}"
}
```

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: audio/x-wav' \
    --data-binary @/path/to/file.wav \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/raw
```

Only one of the above; any subsequent POSTs will overwrite the existing binary data.

!!! note
    There is a deprecated but maintained URL, `GET /v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/raw`, as well.

## List all translations of a given prompt

> GET /v2/accounts/{ACCOUNT_ID}/media/prompts/{PROMPT_ID}

You can use that list to fetch the specific media files associated with that prompt.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/prompts/{PROMPT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        "fr-fr%2Fvm-enter_pass",
        "en-us%2Fvm-enter_pass"
    ],
    "page_size": 2,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "start_key": "vm-enter_pass",
    "status": "success"
}
```

## List media files with specific language

> GET /v2/accounts/{ACCOUNT_ID}/media/languages/{LANGUAGE}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/languages/{LANGUAGE}
```

```json
{
    "data":["media_id_1", "media_id_2",...]
}
```




## To get the IDs of the media docs missing a language:

```shell
curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media/languages/missing
...
"data":["media_id_1", "media_id_2",...]
...
```
