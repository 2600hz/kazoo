/*
Section: APIs
Title: Media
Language: en-US
*/

Uploading media for custom music on hold, IVR prompts, or TTS (if a proper TTS engine is enabled).

## Sample cURL Requests for account media

### Get a listing of media files

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/media

### Create a new media meta object (required before uploading the actual media data)

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/media -d '{"data":{"streamable":true,"name":"File","description":"My Test Media File"}}'

### Add the media binary file to the media meta data

    curl -v -X POST -H "Context-Type: audio/mp3" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/raw --data-binary=@/path/to/file.mp3

### Create a new TTS media document (requires iSpeech to be enabled)

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/media -d {"data":{"name":"TestTTS","media_source":"tts","tts":{"text":"Testing TTS","voice":"female/en-US"}}

### Get a metadata about media file

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}

### Get the raw media file

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Accept: audio/mp3" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/raw -o {MEDIA_ID}.mp3

## System Media

Kazoo provides some default system media files for common things like voicemail prompts. These are accessible via the media Crossbar endpoint as well, if your user has superduper admin privileges. To manipulate those resources, simply omit the `/accounts/{ACCOUNT_ID}` from the URI.

For example, to get a listing of all system media files:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/media

You can then get the "id" of the media file and manipulate it in a similar fashion as regular account media (including TTS if you have a TTS engine like iSpeech configured).

## Languages

Part of the schema of media files is a language attribute. It defaults to a `system_config/media` value for the `default_language` key (and is "en-us" by default). Properly defined media files can be searched for based on language using the basic filters provided by Crossbar:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/media?filter_language=en
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/media?filter_language=en-US
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/media?filter_language=fr-FR

The comparison is case-insensitive, but `en` and `en-US` are treated separately. If a media metadata object is missing a language attribute (on an older installation when system media was imported with no language field, say), use `key_missing=language` in the request.

### List languages available

This request will return a list of languages found, as well as the counts of how many media files have that language defined:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/media/languages
    ...
    "data": [{ "en": 3
               ,"missing": 1
             }
            ],
    ...

Here, the "missing" key indicates how many media files have no associated language

### List media files with specific language

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/media/languages/en
    ...
    "data":["media_id_1", "media_id_2",...]
    ...

To get the IDs of the media docs missing a language:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/media/languages/missing
    ...
    "data":["media_id_1", "media_id_2",...]
    ...
