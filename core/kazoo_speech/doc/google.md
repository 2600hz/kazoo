# Google Cloud Speech API

## Configuration
Requred edit `system_config.speech` doc

```
       "asr_provider": "google"
```

Then edit `system_config.speech.google` doc. Mandatory is 'asr_api_key'

```
       "asr_api_key": "your_key_goes_here"
       "asr_enable_word_time_offsets_google": false
       "asr_profanity_filter_google": false
       "asr_url": "https://speech.googleapis.com/v1/speech:recognize"
```

Or execute

```
sup kapps_config set_default speech asr_provider google
sup kapps_config set_default speech.google asr_api_key your_key_goes_here
```
