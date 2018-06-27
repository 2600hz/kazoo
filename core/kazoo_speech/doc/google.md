# Google Cloud Speech API

## Configuration

Set the ASR provider setting to Google:

```shell
sup kapps_config set_default speech asr_provider google
```
Then configure the Google-specific ASR settings:

```shell
sup kapps_config set_default speech.google {KEY} {VALUE}
```

| Key                                   | Value                                             |
| ---                                   | -----                                             |
| `asr_api_key`                         | "Your-API-Key"                                    |
| `asr_url`                             | https://speech.googleapis.com/v1/speech:recognize |
| `asr_enable_word_time_offsets` | `false`                                           |
| `asr_profanity_filter`         | `false`                                           |
