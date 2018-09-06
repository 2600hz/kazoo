# Media

## About Media

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

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/media

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/media/prompts

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/prompts
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/media/languages

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/languages
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/raw

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/raw
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/raw

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/{MEDIA_ID}/raw
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/media/prompts/{PROMPT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/prompts/{PROMPT_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/media/languages/{LANGUAGE}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/media/languages/{LANGUAGE}
```

