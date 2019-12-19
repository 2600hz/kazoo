# Fax

Faxing is still a critical business requirement for many industries. The faxes app provides a set of faxing related features to kazoo accounts and supports sending and receiving faxes directly from email or the API. The fax entities can be tied to a callflow to terminate incoming faxes without having to use a fax machine.

### Receive Fax Callflow Element

The original fax application feature added the ability to [create a call flow element](https://github.com/2600hz/kazoo/blob/master/applications/callflow/doc/receive_fax.md) that can be attached to a phone number and used to terminate faxes, generating an email to the user with the received faxed document attached. This feature has been replaced by the `faxbox` feature set, but the `receive fax` call flow element is still supported currently as a legacy feature.

### Faxbox

The faxbox acts as a virtual fax machine. The faxbox is used for both receiving and sending faxes. The faxbox is tied into the API and Email Notification systems and will send receipts or error notifications when a fax is sent or fails to send, send notifications when a fax is received and history and content of sent and received faxes can be retrieved via the faxes API. Faxboxes can be [added to kazoo call flows](https://github.com/2600hz/kazoo/blob/master/applications/callflow/doc/faxbox.md) just like receive fax, but unlike receive fax, multiple email addresses can be added to the notification list. Unlike receive fax, faxboxes can be used to send faxes, which can either be uploaded via the faxes API directly, or submitted by sending an email to the faxbox `smtp_email_address` or `custom_smtp_email_address` or the account `realm`. The faxbox supports multiple file formats for sending faxes, most commonly, `pdf`and `tiff`, but OpenXML and other OpenOffice compatible documents can be configured as well.

The faxes API also provide an email inbox/sent items type interface for accessing sent and received faxes and debugging failed fax transmission attempts. Checkout the [faxbox documentation](faxbox.md) for more details on how the faxbox works.

## Api Fax

In addition to sending faxes via email to fax, the `crossbar` application provides [an API that can be used to deliver faxes using two methods](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/doc/faxes.md), `url` and `multipart`.

### Url

If the `url` method is used, a url to access the document must be provided in the request body using a url that is reachable by kazoo.

### Multipart

If `multipart` method is used, the fax document is attached as part of a multipart body, which also contains a JSON object which provides the from and to numbers for the fax.

## Sending Faxes

Two methods are provided for sending faxes, `api fax` and `faxbox`. Faxbox can use either google cloud printer, or the `fax_smtp` server to handle requests to send faxes. Api fax uses the crossbar API to provide the fax content. When the request is received via any method, the requested fax document is processed, a document is created in the `faxes` database, and the document is set to a `pending` status putting it into queue per account by `fax_monitor`, by spawning an instance of `fax_jobs` using the unique name `fax_outbound_{account_id}`.

The fax jobs module provides a queuing and rate limiting mechanism per account, defaulting to `max_outbound` at 10 active jobs per account. For the permitted number of jobs in queue, `fax_jobs` spawns an instance of `fax_worker` under the global name `fax_outbound_{destination phone number}` or if `serialize_outbound_number` is disabled `fax_outbound_{random_uuid}`. By default only one fax job per outbound number can be executed system wide, if `serialize_outbound_number` is `false`, only the per-account limitation is used to serialize jobs and multiple outbound faxes to the same number can be initiated.

Fax worker fetches a fax compatible file from the queued fax job, the file is then written to the configured `file_cache_path`, this file is served via the `fax_file_proxy` module which provides an HTTP file server accessible from the FreeSWITCH servers in the cluster, and transmitted using a FreeSWITCH originate to the destination phone number. Upon receiving the originate request, the FreeSWITCH fetches the fax document from the `fax_file_proxy` and attempts to transmit it using the T.30 or T.38 protocol based on configuration and negotiation with the far end.

If fax transmission fails, and the `system_config.fax` document has reschedule rules configured, these are applied, based on the type of failure. For example, a reschedule rule can be added for user-busy condition that will result in a re-transmission after a configured `retry-after` period. If no reschedule rules are applied the transmission will be re-attempted until the configured number of `retries` on the faxbox is exhausted.

When all `retries` are exhausted, the fax will be put into state `failed` and an email notification will be sent indicating the cause of the error. If the fax transmission is successful, a receipt will be sent to the user and/or any email addresses specified in the faxbox outbound notification list.

## Receiving Faxes

For receiving faxes, either the `receive_fax` or `faxbox` callflow elements must be attached to a callflow. When an incoming call is placed to the callflow's phone number, FreeSWITCH receives the call, passes this up to the call control layer via ecallmgr. The callflow is matched via the callflow app the fax reception is then handled via the `fax_request` module. The `t38` configuration and the filename is provided to FreeSWITCH to use to cache the file and then the fax handshake and transmission is handled via the FreeSWITCH.

When the transmission succeeds, the file is put in the directory defined in the `fax_file_path` in `system_config.ecallmgr`. A document is created in the db then `fax_request` instructs the FreeSWITCH to upload the document to the database, using the naming scheme `received_fax-<fax-id>.tiff`. Once the fax content is uploaded to the db, a notification is generated with the message attached in the format defined in `fax.attachment_format` defined in `system_config.kazoo_convert` (tiff or pdf).

When the transmission fails, an error notification is generated indicating the cause of the failure.

## Kazoo Fax Converter And Attachment Storage Handling And Configuration

If a document is submitted for an outbound fax request in a format unsuitable for faxing (word or OpenXML document, pdf or tiff in a non-faxable format), the attachment will be converted to a fax compliant tiff. The conversions by default will occur whenever a fax document is received via the API or fax_smtp server.

The parameters `store_fax_tiff`, `store_fax_pdf` and `store_fax_url` are used to dictate if the results of the conversions should be stored in the database, or if conversions should take place on the fly when a file in one of these formats are required. By default kazoo will store the original document, a faxable tiff version of the document, and a pdf generated from the faxable version of the document.

If these settings are NOT enabled, kazoo will fetch the original document either via the original file or if a url document is specified, fetching the document from the url using HTTP. Kazoo will then execute any conversions required for the type of file requested (faxable tiff or pdf) and do those conversions each time the fax is requested. This is much slower, especially for the API, but does reduce the size of the database documents since only the original document or document `url` is stored in the database. If `url` fax documents are provided and `store_fax_url` is set to `false`, every time the file is requested, it will be downloaded from the `url`, converted to the format required and that document will be returned.

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
`default_smtp_domain` | fax default smtp domain | `string()` | `fax.kazoo.io` | `false` |
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
