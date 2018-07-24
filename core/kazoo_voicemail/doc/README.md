# Kazoo Voicemail

This library is for accessing and managing all voicemail messages in the system.

Starting with Kazoo 4.0 all new voicemail messages goes into MODB. All Kazoo Administrators need to migrate voicemail messages from Kazoo version 3.22 to MODB to be able to have access to voicemail messages prior to update. The SUP command `kazoo_voicemail_maintenance` exists to do this transition.

> **Caution:** Kazoo 4.0 assumes you migrated all voicemail messages to MODB, and it's only using MODBs for accessing and managing messages. If you did not do the migration users can not access to their messages.

Additionally, the voicemail Crossbar API will no longer returns the `messages` array when fetching voicemail box settings.

## Messages in MODB

Voicemails used to be stored in the same location as configuration settings. Accounts older than 12 months would often get too big,
thus this became a design issue. Voicemails have now been redesigned so that the messages are stored in a monthly database that can be purged later.

All messages store in MODBs with their document id in the `YYYYMM-{32_random_character}` format.

## Migrate Messages to MODB Process

Simply moving all messages alongside their media recording to MODBs have huge impact on the system, especially if for bigger systems with thousands of messages in each account or mailbox.

For making this transition easy and without any side effect, Kazoo voicemail is just making a new document with each message's metadata and store them in the MODB of each account. The MODB is determined base on time stamp (when the message left) of the message.

All media recording attachments (documents with type `private_media`) remain in account DB, and can be moved to message their counterpart documents in MODBs at later time. Each moved message documents contain necessary information for how to access their attachments from the account DB(soft attachment) if their attachment is not migrated.

Kazoo Data Manager would fetch the actual media attachment from account DB if the message attachment is a soft attachment. This process is seamless from users and are handled entirely by Kazoo data layer.

## System Config Settings

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`voicemail.delete_after_notify` | Move the voicemail to delete folder after the notification has been sent | `boolean()` | `false` | `false`
`voicemail.extension` | Default media file format for voicemail audio file | `string()` | `mp3` | `false`
`voicemail.force_require_pin` | If true, ignore the setting on the voicemail box and require all users to enter a pin | `boolean()` | `false` | `false`
`voicemail.max_box_number_length` | Maximum length of voicemail box number | `integer()` | `15` | `false`
`voicemail.max_login_attempts` | Maximum login attempts to voicemail box | `integer()` | `3` | `false`
`voicemail.max_message_count` | Maximum number of voicemail messages a mailbox can hold | `integer()` | `100` | `false`
`voicemail.max_message_length` | Maximum message length in seconds | `integer()` | `500` | `false`
`voicemail.max_pin_length` | Maximum pin number length for mailbox | `integer()` | `6` | `false`
`voicemail.message_retention_duration` | How many days back are searched when listing voicemails | `integer()` | `93` | `false`
`voicemail.migrate_interaccount_delay_ms` | Control wait time between processing each account in each iteration | `integer()` | `2000` | `false`
`voicemail.migrate_max_bulk_insert` | Number of messages should each migration worker reads and write in each cycle | `integer()` | `2000` | `false`
`voicemail.migrate_max_worker` | Maximum number of workers when migrating voicemail messages to MODB | `integer()` | `10` | `false`
`voicemail.min_message_size` | Minimum message length in milliseconds a voicemail message must be to consider it to save in database | `integer()` | `500` | `false`
`voicemail.save_after_notify` | Move the voicemail to save folder after the notification has been sent (This setting will override delete_after_notify) | `boolean()` | `false` | `false`
`voicemail.vm_message_forward_type` | Enable or disable the ability to prepend a message when forwarding a voicemail message | `string()` | `only_forward` | `false`

## Retention Duration

There is a new configuration parameter, `message_retention_duration`, that specifies how many days back are searched when listing voicemails, default value is `93` days (3 months). Messages older than this value will not accessible through API and mail box, but they are still exists in monthly databases and can be purged later by system administrator.
