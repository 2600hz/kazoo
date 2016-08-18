/*
Section: Callflow
Title: Fax Detect
Language: en-US
*/

# Fax Detect

Fax Detect allows the detection of FAX/VOICE in a call.

```
"data": {
    "duration": DURATION
}
```

The `DURATION` is the number of seconds the detection lasts, default is 3.
if the detection fails, it defaults to `voice`

## Connectors
   Fax detect action has 2 connectors for next actions.
* ON_VOICE
* ON_FAX
