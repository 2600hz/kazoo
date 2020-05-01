## SUP-able functions

| Function | Arguments | Description |
| -------- | --------- | ----------- |
| `check_pending_sms_for_delivery/1` | `(AccountId)` | |
| `check_pending_sms_for_outbound_delivery/1` | `(AccountId)` | |
| `check_sms_by_device_id/2` | `(AccountId,DeviceId) | (_AccountId,undefined)` | |
| `check_sms_by_owner_id/2` | `(AccountId,OwnerId) | (_AccountId,undefined)` | |
| `flush/0` |  | |
| `send_outbound_sms/2` | `(To,Msg)` | |
| `send_outbound_sms/3` | `(To,Msg,Times)` | |
| `send_outbound_sms/4` | `(To,From,RouteId,Msg)` | |
| `send_outbound_sms/5` | `(To,From,RouteId,Msg,Times)` | |
| `start_check_sms_by_account/2` | `(AccountId,JObj)` | |
| `start_check_sms_by_device_id/2` | `(AccountId,DeviceId)` | |
| `start_check_sms_by_owner_id/2` | `(AccountId,OwnerId)` | |
