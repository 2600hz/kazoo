### Call Recording

#### Disambiguate callflows

The callflow modules are hard to differentiate at first. Hopefully this helps.

`cf_record_caller` uses the [`record`](https://wiki.freeswitch.org/wiki/Misc._Dialplan_Tools_record) command, intended for recording messages (such as voicemails).

`cf_record_call` starts a `kz_media_recording` process (by default) which uses the `record_call` command which utilizes [`uuid_record`](https://wiki.freeswitch.org/wiki/Mod_commands#uuid_record) to record the call leg's audio stream.
