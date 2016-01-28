### Media

#### About Media

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`content_length` | Length, in bytes, of the file | `integer` |   | `false`
`content_type` | Used to override the automatic upload type | `string` |   | `false`
`description` | A breif description of the media update, usally the original file name | `string` |   | `false`
`language` | The language of the media file or text | `string` | `en-us` | `false`
`media_source` | Defines the source of the media | `string` | `upload` | `false`
`name` | A friendly name for the media | `string` |   | `true`
`prompt_id` | The prompt this media file represents | `string` |   | `false`
`source_id` | If the media was generated from a callflow module, this is ID of the properties | `string` |   | `false`
`source_type` | If the media was generated from a callflow module, this is the module name | `string` |   | `false`
`streamable` | Determines if the media can be streamed | `boolean` | `true` | `false`
`tts` | Text-to-speech options used to create audio files from text | `object` | `{}` | `false`
`tts.text` | The text to be converted into audio | `string` |   | `false`
`tts.voice` | The voice to be used during the conversion | `string` | `female/en-US` | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/media

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/media
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/media

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/media
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/media/{MEDIAID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/media/{MEDIAID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/media/{MEDIAID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/media/{MEDIAID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/media/{MEDIAID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/media/{MEDIAID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/media/prompts

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/media/prompts
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/media/languages

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/media/languages
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/media/{MEDIAID}/raw

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/media/{MEDIAID}/raw
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/media/{MEDIAID}/raw

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/media/{MEDIAID}/raw
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/media/prompts/{PROMPTID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/media/prompts/{PROMPTID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/media/languages/{LANGUAGE}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/media/languages/{LANGUAGE}
```

