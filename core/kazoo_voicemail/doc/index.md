# Kazoo Voicemail

This library is for accessing and managing all voicemail messages in the system.

Starting with Kazoo 4.0 all new voicemail messages goes into modb. All Kazoo Administrators need to migrate their voicemail messages from Kazoo version 3.22 to MODB. The `kvm_migrate` module is written to do this transition. For migrating messages in specific account or mailbox use helper functions from `kvm_maintenance`.

**Note:** Kazoo 4.0 assumes you migrated all voicemail messages to MODB, and it's only using MODBs for accessing and managing messages!

Additionally, the vmboxes API endpoint will no longer return the messages array when fetching voicemail box settings.

## Messages in MODb

Voicemails used to be stored in the same location as configuration settings. Accounts older than 12 months would often get too big,
thus this became a design issue. Voicemails have now been redesigned so that the messages are stored in a monthly database that can be purged later.

All messages store in MODbs with their document id in the `YYYYMM-{32_random_character}` format.

## Migrate messages to MODB process

Simply moving all messages alongside their media recording to MODBs have huge impact on the system, especially if for bigger systems with thousands of messages in each account.

For making this transition easy and without any side effect, Kazoo voicemail is just making a new document with each message's metadata and store them in the __current__ MODB of each account.
All media recordings (private_media) remain in account db to be moved to message documents in MODBs at later time. Each moved message documents contain necessary info for how to access their attachemnts in the account db(soft attachment).

Kazoo datamanager handle of fetching the actual media attachment from account db anytime MODB documents attachemnt tries to be reached. This process is seamless from users and are handled entirely by
Kazoo data layer.

These are configurations (`system_config/callflow/voicemail`) for controlling migration process:

Key | Description | Type | Default
--- | ----------- | ---- | ------- | --------
`migrate_max_mailbox_process` | How many mailbox must be process in iterations | `integer` | 10
`migrate_interbox_delay_ms` | Control wait time between processing each mailbox batch iterations | `integer` | 1000
`migrate_interaccount_delay_ms` | Control wait time between processing each account | `integer` | 10000


## New message

Callflow voicemail module specifically uses `kvm_message:new_message/5` to record and store the new voicemail message. This function expects props from callflow as follow:

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

## Retention duration

There is a new configuration parameter `message_retention_duration` under `system_config/callflow/voicemail` for how many days voicemail messages should be considered when accessing a voicemail box. This duration also applies to deleting heard and or old messages as well.

## Clean old and heard messages

For deleting old messages and heard messages you can use `kvm_maintenance:cleanup_heard_voicemail/1`.
