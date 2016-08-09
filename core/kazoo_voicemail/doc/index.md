### Voicemail Messages

This modules tries to make a common abstraction over where voicemail message is stored.

Starting with Kazoo 4.0 all new voicemail messages will be stored in the account MODbs alongside their message media recording. All previous voicemail message will be moved to MODbs when updating their properties(changing their folder).

Additionally, the v2 vmboxes API will no longer return the messages array and the manipulation the messages array on the v1 vmboxes API is strongly discuraged. For more information on vmboxes API see crossbar voicemail documentation.

#### New message

Callflow voicemail module specifically uses `kz_vm_message:new_message/5` to record and store the new voicemail message. This function expects props from callflow as follow:

```erlang
[{<<"Attachment-Name">>, AttachmentName}
,{<<"Box-Id">>, BoxId}
,{<<"OwnerId">>, OwnerId}
,{<<"Length">>, Length}
,{<<"Transcribe-Voicemail">>, MaybeTranscribe}
,{<<"After-Notify-Action">>, Action}
,{<<"Attachment-Name">>, AttachmentName}
,{<<"Box-Num">>, BoxNum}
,{<<"Timezone">>, Timezone}
]
```

#### Messages in MODb

All new messages will be stored in MODBs and old messages wil be moved to MODbs when its user made a change to message(changing message folder or deleting it). For most operations, this should be seamless from the end users.

For all of the operations(counting messages in vmbox, set folders on message, getting the message, etc...) this modules tries to make an abstraction layer so the other modules doesn't need to worry about where and how to find the voicemail message.

All messages stored in MODbs have their document id in the format of `YYYYMM-{32_random_character}`. This is used for identifying which message is in MODbs and which one is still in vmbox messages array.

For operations on whole messages in a voicemail box, vmbox messages array will be fetched first and then its message array will be merged with result of account MODbs messages.

Operations on a specific message will be performed based on it's `id` format.

With Ids in old non-MODb format, media recording document with that `Id` will be fetched from `accountdb` and the message metadata will be fetched from vmbox array and they will be merged into the new message document format for accessing to them.

For Ids with the newer MODbs fomrat, the whole new message document(which has the metadata and media together on the same document) will be fetched from account MODb.

#### Retention duration

There is a new configuration parameter `message_retention_duration` under `system_config/callflow/voicemail` for how many days voicemail messages should be considered when accessing a voicemail box. This duration also applies to deleting heard and or old messages as well.

#### Migrate old messages to MODbs

If you want to manually move all messages from accountdb to account MODb you can use `kz_vm_maintenance:migrate/0`, `kz_vm_maintenance:migrate/1` and `kz_vm_maintenance:migrate/2`.

#### Clean old and heard messages

For deleting old messages and heard messages you can use `kz_vm_maintenance:cleanup_heard_voicemail/1`.
