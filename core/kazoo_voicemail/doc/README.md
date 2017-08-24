# Kazoo Voicemail

This library is for accessing and managing all voicemail messages in the system.

Starting with Kazoo 4.0 all new voicemail messages goes into modb. All Kazoo Administrators need to migrate their voicemail messages from Kazoo version 3.22 to MODB. The `kvm_migrate` module is written to do this transition. For migrating messages in specific account or mailbox use helper functions from `kazoo_voicemail_maintenance`.

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

## System Config settings

These are configurations (`system_config/callflow`) for controlling migration process:

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`voicemail.delete_after_notify` | callflow delete after notify | `boolean()` | `false` | `false`
`voicemail.extension` | Default media file format for Callflow modules | `string()` | `mp3` | `false`
`voicemail.force_require_pin` | If true, ignore the setting on the vmbox and require all users to enter a pin | `boolean()` | `false` | `false`
`voicemail.max_box_number_length` | callflow maximum box number length | `integer()` | `15` | `false`
`voicemail.max_login_attempts` | callflow max login attempts | `integer()` | `3` | `false`
`voicemail.max_message_count` | callflow maximum message count | `integer()` | `100` | `false`
`voicemail.max_message_length` | callflow maximum message length | `integer()` | `500` | `false`
`voicemail.max_pin_length` | callflow maximum pin length | `integer()` | `6` | `false`
`voicemail.message_retention_duration` | callflow message retention duration | `integer()` | `93` | `false`
`voicemail.migrate_interaccount_delay_ms` | callflow migrate interaccount delay in milliseconds | `integer()` | `2000` | `false`
`voicemail.migrate_max_bulk_insert` | callflow migrate maximum bulk insert | `integer()` | `2000` | `false`
`voicemail.migrate_max_worker` | callflow migrate max worker | `integer()` | `10` | `false`
`voicemail.min_message_size` | callflow minimum message size | `integer()` | `500` | `false`
`voicemail.save_after_notify` | callflow save after notify | `boolean()` | `false` | `false`
`voicemail.vm_message_foraward_type` | Enable or disable the ability to prepend a message when forwarding a voicemail message | `string()` | `only_forward` | `false`

## New message

Callflow voicemail module uses `kvm_message:new_message/2` to record and store the new voicemail message. This function expects these props:

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

There is a new configuration parameter, `message_retention_duration`, that specifies how many days back are searched when listing voicemails.
