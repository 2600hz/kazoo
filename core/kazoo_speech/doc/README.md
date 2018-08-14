# kazoo Speech

This core library governs integrations with [Text-to-Speech (TTS) providers](https://en.wikipedia.org/wiki/Speech_synthesis) and with (Automatic Speech Recognition (ASR) engines)[https://en.wikipedia.org/wiki/Speech_recognition] to allow Kazoo callflows to take advantage of the advanced functionality these provide.

## TTS

Kazoo, by default, will use the internal [mod_flite](https://freeswitch.org/confluence/display/FREESWITCH/mod_flite) to play the text from a [TTS callflow action](../../../applications/callflow/doc/tts.md).

There are also modules for [iSpeech](http://www.ispeech.org/api/#text-to-speech) and [VoiceFabric](https://voicefabric.ru/user_documentation).

## ASR

Kazoo currently supports as an ASR engine:

1. [iSpeech](http://www.ispeech.org/api/#automated-speech-recognition)
2. [Google Cloud Speech API](https://cloud.google.com/speech/)
