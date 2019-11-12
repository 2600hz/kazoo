
### Crossbar Configuration

#### System Configs

The following table outlines the configs that can be found in the `system_config` database, `crossbar` document:

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `api_auth_tokens` | `integer()` | 35 | Default token cost of creating an auth token via API key |
| `autoload_modules` | `list(string())` | See [here](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/crossbar.hrl#L65-L119) | The list of Crossbar modules initially started |
| `cache_ttl` | `integer()` | 300 | Cache TTL, in seconds |
| `cleanup_timer` | `integer()` | 86400 | Time, in seconds, to run the cleanup routines |
| `compress_response_body` | `boolean()` | `true` | Whether to compress the response body before sending |
| `default_allow_anonymous_quickcalls` | `boolean()` | `true` | Whether to allow unauthenticated quickcall API requests |
| `default_language` | `string()` | `en-US` | The default language, if none are defined on the account |
| `magic_path_patterns` | `list(string())` | See [here](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/crossbar_default_handler.erl#L21-L24) | Magic path templates |
| `max_upload_size` | `integer()` | 8000000 bytes (8Mb) | Max upload size for request bodies |
| `maximum_range` | `integer()` | 2682000 | Maximum range, in seconds, for time-based view queries |
| `pagination_page_size` | `integer()` | 50 | Default page size when paginating |
| `port` | `integer()` | 8000 | Port to listen for unencrypted traffic |
| `pretty_metrics` | `boolean()` | `true` | Pretty-print metrics in logs |
| `request_timeout_ms` | `integer()` | 10000 | Time, in milliseconds, for requests to timeout
| `reset_id_size` | `integer()` | 250 | Password-reset ID length |
| `schema_strict_validation` | `boolean()` | `false` | Toggles whether to perform type conversions on client data when validating |
| `soft_delete_pause_ms` | `integer()` | 10000 | Time, in milliseconds, to pause between deletions |
| `ssl_ca_cert` | `string()` | `undefined` | Path to CA cert file |
| `ssl_cert` | `string()` | `/path/to/crossbar/priv/ssl/crossbar.crt` | Path to cert file |
| `ssl_key` | `string()` | `/path/to/crossbar/priv/ssl/crossbar.key` | Path to key file |
| `ssl_password` | `string()` | `""` | Cert password |
| `ssl_port` | `integer()` | 8443 | Port to listen for SSL traffic |
| `ssl_workers` | `integer()` | 100 | Number of SSL listeners to start |
| `token_costs` | `integer()` | 1 | Default token cost of an API request |
| `trace_path` | `string()` | `/tmp` | Path to put trace files when profiling API requests |
| `use_plaintext` | `boolean()` | `true` | Whether to start unencrypted listener (port 8000 traffic, typically) |
| `use_ssl` | `boolean()` | `false` | Whether to start an SSL listener |
| `user_auth_tokens` | `integer()` | 35 | Default token cost of creating an auth token via username |
| `workers` | `integer()` | 100 | Number of TCP listeners to start |


#### Additional Configs

Some modules use the `crossbar` namespace to create a specific `system_config` document for settings as well.

##### `crossbar.accounts`
##### `crossbar.auth`
##### `crossbar.braintree`
##### `crossbar.callflows`
##### `crossbar.cdrs`
##### `crossbar.devices`
##### `crossbar.fax`
##### `crossbar.media`
##### `crossbar.notifications`
##### `crossbar.port_requests`
##### `crossbar.presence`
##### `crossbar.queues`
##### `crossbar.resource_selectors`
##### `crossbar.resource_templates`
##### `crossbar.resources`
##### `crossbar.services`
##### `crossbar.sms`
##### `crossbar.token_restrictions`
