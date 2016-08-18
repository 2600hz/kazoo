

Uploading media for custom music on hold, IVR prompts, or TTS (if a proper TTS engine is enabled).

#### Sample cURL Requests for account media

##### Get a listing of media files

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media

##### Create a new media meta object (required before uploading the actual media data)

    curl -v -X PUT -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media -d '{"data":{"streamable":true,"name":"File","description":"My Test Media File"}}'

##### Create a new prompt meta object (required before uploading the actual prompt data)

    curl -v -X PUT -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media -d '{"data":{"streamable":true,"name":"FR-vm-enter_pass","description":"FR - Enter Password prompt","prompt_id":"vm-enter_pass", "language":"fr"}}'

##### Add the media binary file to the media meta data

    curl -v -X POST -H "Content-Type: audio/mp3" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/raw --data-binary @/path/to/file.mp3

##### Create a new TTS media document (requires iSpeech to be enabled)

    curl -v -X PUT -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media -d '{"data":{"name":"TestTTS","media_source":"tts","tts":{"text":"Testing TTS","voice":"female/en-US"}}'

##### Get a metadata about media file

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}

##### Get the raw media file

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Accept: audio/mp3" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/raw -o {MEDIA_ID}.mp3

#### System Media

Kazoo provides some default system media files for common things like voicemail prompts. These are accessible via the media Crossbar endpoint as well, if your user has superduper admin privileges. To manipulate those resources, simply omit the `/accounts/{ACCOUNT_ID}` from the URI.

For example, to get a listing of all system media files:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media

You can then get the "id" of the media file and manipulate it in a similar fashion as regular account media (including TTS if you have a TTS engine like iSpeech configured).

##### Create a new prompt meta object (required before uploading the actual prompt data)

    curl -v -X PUT -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media -d '{"data":{"streamable":true,"name":"File","description":"Enter Pin prompt","prompt_id":"vm-enter_pass", "language":"fr-fr"}}'
    {"data":{"streamable":true,"name":"vm-enter_pass","description":"FR - Enter Pin prompt","prompt_id":"vm-enter_pass","language":"fr-fr","tts":{"voice":"female/en-US"},"media_source":"upload","id":"fr-fr%2Fvm-enter_pass"},"revision":"{REVISION}","request_id":"{REQUEST_ID}","status":"success","auth_token":"{AUTH_TOKEN}"}

##### Add the media binary file to the media meta data

    curl -v -X POST -H "Content-Type: audio/mp3" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media/{MEDIA_ID}/raw --data-binary @/path/to/file.mp3
    curl -v -X POST -H "Content-Type: audio/x-wav" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media/{MEDIA_ID}/raw --data-binary @/path/to/file.wav

Only one of the above; any subsequent POSTs will overwrite the existing binary data.

##### List all prompts and the number of translations existing

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

##### List all translations of a given prompt

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

#### Languages

Part of the schema of media files is a language attribute. It defaults to a `system_config/media` value for the `default_language` key (and is "en-us" by default). Properly defined media files can be searched for based on language using the basic filters provided by Crossbar:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media?filter_language=en
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media?filter_language=en-US
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/media?filter_language=fr-FR

The comparison is case-insensitive, but `en` and `en-US` are treated separately. If a media metadata object is missing a language attribute (on an older installation when system media was imported with no language field, say), use `key_missing=language` in the request.

##### List languages available

This request will return a list of languages found, as well as the counts of how many media files have that language defined:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media/languages
    ...
    "data": [{ "en": 3
               ,"missing": 1
             }
            ],
    ...

Here, the "missing" key indicates how many media files have no associated language

##### List media files with specific language

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media/languages/en
    ...
    "data":["media_id_1", "media_id_2",...]
    ...

To get the IDs of the media docs missing a language:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media/languages/missing
    ...
    "data":["media_id_1", "media_id_2",...]
    ...

#### Callflows

Once you've assigned languages, you can use the [`language` callflow action](../../callflow/doc/language.md) to set the language for that call.

#### Normalization

Kazoo can be configured to normalize uploaded media files. This can fix things like:

* Normalizing volume
* Fix clipping
* Standardize formats

By default, if enabled, normalization will convert all media to MP3 (retaining the original upload as well) using the [*sox* utility](http://sox.sourceforge.net/) to accomplish the conversion.

##### Enable Normalization

###### Via SUP

Enable normalization for this particular server: `sup kapps_config set crossbar.media normalize_media true`

Enable normalization for all servers: `sup kapps_config set_default crossbar.media normalize_media true`

###### Via DB

1. Open `system_config/crossbar.media` document, create or update the key `normalize_media` to `true`.
2. Flush the kapps_config cache, `sup kapps_config flush crossbar.media`, on all servers running Crossbar.

##### Set Target Format

###### Via SUP

For the server: `sup kapps_config set crossbar.media normalization_format ogg`

For all servers: `sup kapps_config set_default crossbar.media normalization_format ogg`

###### Via DB

In the `system_config/crossbar.media` document, create or update the key `normalization_format` to your desired format (`mp3`, `wav`, etc). Flush the kapps_config cache on all servers running Crossbar. All new uploads will be normalized (if possible) to the new format.

##### Normalization parameters

The default *sox* command is `sox -t <input_format> - -r 8000 -t <output_format> -` but this is configurable via the `system_config/media` document (or similar SUP command).

You can fine-tune the source and destination arguments using the `normalize_source_args` and `normalize_destination_args` keys respectively. By default, the source args are "" and the destination args are "-r 8000" (as can be seen from the default sox command above.

The normalizer code uses stdin to send the binary data to sox and reads from stdout to get the normalized binary data back (the " - " (there are 2) in command above).

You can also set the specific path for `sox` in the `normalize_executable` key, in case you've installed it to a non-standard path.

Be sure to install sox with mp3 support! Conversion will not happen (assuming you're targeting mp3) if sox can't write the mp3. You can check the media meta document for the key `normalization_error` if sox failed for some reason.
