# Callflow PBX Functionality

The callflow application handles the processing of [callflow JSON](../../crossbar/doc/callflows/) flows.

## Structure of a callflow JSON flow

A flow consists of an `action` module, a `data` object for that action, and optionally a `children` object to branch the flow to the next action.

```json
"flow":{
  "module":"some_action"
  ,"data":{"date":"for", "the":"action"}
  ,"children":{
    "branch":{flow}
  }
}
```

Each action has a JSON schema that defines what fields are available in the `data` object.

### Children

The default child key, `"_"` is used when no other matching child keys are found. For instance, in a `user`->`voicemail` callflow:

```json
{"module":"user"
 ,"data":{"id":"{USER_ID}"}
 ,"children":{
   "_":{
     "module":"voicemail"
     ,"data":{"id":"{VM_BOX}"}
   }
 }
}
```

When the `children` key is omitted or has no child branches, the call will generally be terminated.

#### Bridging Failures

When bridging the incoming leg, you can add child branches based on the SIP response code or reason to have custom handling of various failure scenarios.

For instance, you can handle `404` user not found errors differently than `486` user busy errors (and still maintain the default behaviour with `"_"` if a different error occurred:

```json
{"module":"user"
 ,"data":{"id":"{USER_ID}"}
 ,"children":{
   "sip:404":{
     "module":"tts"
     ,"data":{"text":"The user could not be found"}
   }
   ,"sip:486":{
     "module":"tts"
     ,"data":{"text":"The user could not come to the phone right now"}
   }
   ,"_":{
     "module":"tts"
     ,"data":{"text":"The user could not be reached at this time"}
   }
 }
```
