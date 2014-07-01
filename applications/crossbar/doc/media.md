/*
Section: Crossbar
Title: Media
Language: en-US
*/

Uploading media for custom music on hold, IVR prompts, or TTS (if a proper TTS engine is enabled).

## Sample cURL Requests for account media

### Get a listing of media files

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media

### Create a new media meta object (required before uploading the actual media data)

    curl -v -X PUT -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media -d '{"data":{"streamable":true,"name":"File","description":"My Test Media File"}}'

### Create a new prompt meta object (required before uploading the actual prompt data)

    curl -v -X PUT -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media -d '{"data":{"streamable":true,"name":"FR-vm-enter_pass","description":"FR - Enter Password prompt","prompt_id":"vm-enter_pass", "language":"fr"}}'

### Add the media binary file to the media meta data

    curl -v -X POST -H "Content-Type: audio/mp3" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/raw --data-binary @/path/to/file.mp3

### Create a new TTS media document (requires iSpeech to be enabled)

    curl -v -X PUT -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media -d {"data":{"name":"TestTTS","media_source":"tts","tts":{"text":"Testing TTS","voice":"female/en-US"}}

### Get a metadata about media file

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}

### Get the raw media file

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Accept: audio/mp3" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/raw -o {MEDIA_ID}.mp3

## System Media

Kazoo provides some default system media files for common things like voicemail prompts. These are accessible via the media Crossbar endpoint as well, if your user has superduper admin privileges. To manipulate those resources, simply omit the `/accounts/{ACCOUNT_ID}` from the URI.

For example, to get a listing of all system media files:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media

You can then get the "id" of the media file and manipulate it in a similar fashion as regular account media (including TTS if you have a TTS engine like iSpeech configured).

### Create a new prompt meta object (required before uploading the actual prompt data)

    curl -v -X PUT -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media -d '{"data":{"streamable":true,"name":"File","description":"Enter Pin prompt","prompt_id":"vm-enter_pass", "language":"fr-fr"}}'
    {"data":{"streamable":true,"name":"vm-enter_pass","description":"FR - Enter Pin prompt","prompt_id":"vm-enter_pass","language":"fr-fr","tts":{"voice":"female/en-US"},"media_source":"upload","id":"fr-fr%2Fvm-enter_pass"},"revision":"{REVISION}","request_id":"{REQUEST_ID}","status":"success","auth_token":"{AUTH_TOKEN}"}

### Add the media binary file to the media meta data

    curl -v -X POST -H "Content-Type: audio/mp3" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media/{MEDIA_ID}/raw --data-binary @/path/to/file.mp3
    curl -v -X POST -H "Content-Type: audio/x-wav" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media/{MEDIA_ID}/raw --data-binary @/path/to/file.wav

Only one of the above; any subsequent POSTs will overwrite the existing binary data.

### List all prompts and the number of translations existing

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" 'http://server.com:8000/v2/media/prompts'
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

### List all translations of a given prompt

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" 'http://server.com:8000/v2/media/prompts/vm-enter_pass'
    {
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        "fr-fr%2Fvm-enter_pass",
        "en-us%2Fvm-enter_pass"
    ],
    "page_size": 2,
    "request_id": "{REQUEST_ID}",
    "revision": "undefined",
    "start_key": "vm-enter_pass",
    "status": "success"
    }

You can use that list to fetch the specific media files associated with that prompt.

## Languages

Part of the schema of media files is a language attribute. It defaults to a `system_config/media` value for the `default_language` key (and is "en-us" by default). Properly defined media files can be searched for based on language using the basic filters provided by Crossbar:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media?filter_language=en
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media?filter_language=en-US
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media?filter_language=fr-FR

The comparison is case-insensitive, but `en` and `en-US` are treated separately. If a media metadata object is missing a language attribute (on an older installation when system media was imported with no language field, say), use `key_missing=language` in the request.

### List languages available

This request will return a list of languages found, as well as the counts of how many media files have that language defined:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media/languages
    ...
    "data": [{ "en": 3
               ,"missing": 1
             }
            ],
    ...

Here, the "missing" key indicates how many media files have no associated language

### List media files with specific language

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media/languages/en
    ...
    "data":["media_id_1", "media_id_2",...]
    ...

To get the IDs of the media docs missing a language:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media/languages/missing
    ...
    "data":["media_id_1", "media_id_2",...]
    ...

## Callflows

Once you've assigned languages, you can use the [`language` callflow action](../../callflow/doc/language.md) to set the language for that call.
