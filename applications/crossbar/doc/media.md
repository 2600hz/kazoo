/*
Section: APIs
Title: Media
Language: en-US
*/

Uploading media for custom music on hold, IVR prompts, or TTS (if a proper TTS engine is enabled).

## Sample cURL Requests

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
