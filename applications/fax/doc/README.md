
# Fax

Faxing is still a critical business requirement for many industries. The faxes app provides a set of faxing related features to kazoo accounts and supports sending and receving faxes directly from email or the API. The fax entities can be tied to a callflow to terminate incoming faxes without having to use a fax machine.

### Receive Fax Callflow Element

The original fax application feature added the ability to [create a call flow element](https://github.com/2600hz/kazoo/blob/master/applications/callflow/doc/receive_fax.md) that can be attached to a phone number and used to terminate faxes, generating an email to the user with the received faxed document attached. This feature has been replaced by the `faxbox` feature set, but the `receive fax` call flow element is still supported currently as a legacy feature.

### Faxbox

The faxbox acts as a virtual fax machine. The faxbox is used for both receiving and sending faxes. The faxbox is tied into the API and Email Notification systems and will send receipts or error notifications when a fax is sent or fails to send, send notifications when a fax is received and history and content of sent and received faxes can be retrieved via the faxes API. Faxboxes can be [added to kazoo call flows](https://github.com/2600hz/kazoo/blob/master/applications/callflow/doc/faxbox.md) just like receive fax, but unlike receive fax, multiple email addresses can be added to the notification list. Unlike receive fax, faxboxes can be used to send faxes, which can either be uploaded via the faxes API directly, or submitted by sending an email to the faxbox `smtp_email_address` or `custom_smtp_email_address` or the account `realm`. The faxbox supports multiple file formats for sending faxes, most commonly, `pdf`and `tiff`, but openxml and other OpenOffice compatible documents can be configured as well.

The faxes API also provide an email inbox/sent items type interface for accessing sent and received faxes and debugging failed fax transmission attempts. Checkout the [faxbox documentation](faxbox.md) for more details on how the faxbox works.

### Configuration Parameters

`system_config.fax` global settings

Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`allow_all_addresses_when_empty` | fax allow all addresses when empty | `boolean()` | `false` | `false` |
`allowed_content_types.[]` |   | `string()` |   | `false` |
`allowed_content_types` | fax allowed content types | `array(string())` | `["application/pdf", "image/tiff", "{"prefix":"image"}", "{"prefix":"application/vnd.openxmlformats-officedocument."}", "{"prefix":"application/vnd.oasis.opendocument."}", "application/msword", "application/vnd.ms-excel", "application/vnd.ms-powerpoint"]` | `false` |
`cloud_registration_pool_interval` | fax cloud registration pool interval | `integer()` | `5000` | `false` |
`default_compare_field` | fax default compare field | `string()` | `result_cause` | `false` |
`default_fax_extension` | fax default fax extension | `string()` | `.tiff` | `false` |
`default_retry_count` | fax default retry count | `integer()` | `3` | `false` |
`default_retry_period` | fax default retry period | `integer()` | `300` | `false` |
`default_smtp_domain` | fax defaultsmtp domain | `string()` | `fax.kazoo.io` | `false` |
`delete_empty_faxes` | fax delete empty faxes | `boolean()` | `false` | `false` |
`denied_content_types` | fax denied content types | `array(object())` | `["{"prefix":"image/"}"]` | `false` |
`endpoint_timeout` | fax endpoint timeout | `integer()` | `40` | `false` |
`ensure_valid_caller_id` | fax ensure valid caller id | `boolean()` | `true` | `false` |
`fax_file_path` | fax fax file path | `string()` | `/tmp/` | `false` |
`fax_settings` | fax fax settings | `object()` | `{"override_fax_identity":true,"override_callee_number":false}` | `false` |
`file_cache_path` | fax file cache path | `string()` | `/tmp/` | `false` |
`ignore_early_media` | fax ignore early media | `boolean()` | `false` | `false` |
`image_min_size` | fax image minimum size | `string()` | `700x10` | `false` |
`image_size_cmd_format` | fax image size cmd format | `string()` | `echo -n ```identify -format "%[fx:w]x%[fx:h]" ~s`` | `false` |
`inbound_t38_default` | fax inbound t38 default | `string()` | `true` | `false` |
`log_faxbox_errors` | fax log faxbox errors | `boolean()` | `true` | `false` |
`max_outbound` | fax max outbound | `integer()` | `10` | `false` |
`max_storage_retry` | fax maximum storage retry | `integer()` | `5` | `false` |
`port` | fax port | `integer()` | `30950` | `false` |
`report_anonymous_system_errors` | fax report anonymous system errors | `boolean()` | `false` | `false` |
`report_faxbox_system_errors` | fax report faxbox system errors | `boolean()` | `true` | `false` |
`report_smtp_errors` | Report SMTP-related errors via notifications | `boolean()` | `true` | `false` |
`reschedule` | fax reschedule | `object()` | `{}` | `false` |
`serialize_outbound_numbers` | Serialize fax transmissions by outbound number globally | `boolean()` | `true` | `false` |
`smtp_max_msg_size` | fax smtp maximum msg size | `integer()` | `10485670` | `false` |
`smtp_port` | fax smtp port | `integer()` | `19025` | `false` |
`smtp_sessions` | fax smtp sessions | `integer()` | `50` | `false` |
`store_fax_pdf` | store the post processed fax document | `boolean()` | `true` | `false` |
`store_fax_tiff` | store a pdf copy of the post processed fax document | `boolean()` | `true` | `false` |
`store_url_document` | store the document url result in the database | `boolean()` | `true` | `false` |
`wait_for_fax_timeout_ms` | fax wait for fax timeout in milliseconds | `integer()` | `3600000` | `false` |
`workers` | fax workers | `integer()` | `50` | `false` |
`xmpp_interval` | fax xmpp interval | `integer()` | `600000` | `false` |

### Useful Sup Commands

To see the pending faxes currently in queue:

```
sup fax_maintenance pending_jobs
```

To see the active faxes:

```
sup fax_maintenance active_jobs
```

To see the jobs currently pending in an account:

```
sup fax_maintenance account_jobs {ACCOUNT_ID}
```


