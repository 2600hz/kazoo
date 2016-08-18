### Schemas

#### About Schemas

Kazoo uses [JSON Schemas](http://json-schema.org/) to validate incoming data from clients.

Any fields that aren't defined in the JSON schema will be stored, unmodified, along side the validated fields (assuming all is well). This excludes Kazoo-managed private fields (top-level keys prefixed with "_" and "pvt_").

#### Schema



#### Fetch a listing of available schemas

> GET /v2/schemas

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/schemas
{
    "auth_token": "",
    "data": [
        "access_lists",
        "account_rate_limits",
        "accounts",
        "acls",
        "allotments",
        "api_auth",
        "app",
        "audit_logs",
        "blacklists",
        "bookkeepers",
        "call_waiting",
        "caller_id",
        "callflows",
        "callflows.collect_dtmf",
        "callflows.conference",
        "callflows.language",
        "callflows.lookupcidname",
        "callflows.manual_presence",
        "callflows.nomorobo",
        "callflows.pivot",
        "callflows.record_call",
        "callflows.response",
        "callflows.ring_group",
        "callflows.send_dtmf",
        "callflows.tts",
        "callflows.voicemail",
        "cccps",
        "cdr",
        "clicktocall",
        "conferences",
        "connectivity",
        "device_rate_limits",
        "devices",
        "dialplans",
        "directories",
        "domain_hosts",
        "domains",
        "faxbox",
        "faxes",
        "ledgers",
        "limits",
        "list_entries",
        "lists",
        "media",
        "menus",
        "metaflows",
        "notifications",
        "notify.callback",
        "phone_numbers",
        "port_requests",
        "profile",
        "provisioner_v5",
        "queue_update",
        "queues",
        "rates",
        "resource_jobs",
        "resources",
        "service_plans",
        "shared_auth",
        "sms",
        "temporal_rules",
        "temporal_rules_sets",
        "token_restrictions",
        "trunkstore",
        "ubiquiti_auth",
        "user_auth",
        "user_auth_recovery",
        "users",
        "vmboxes",
        "webhook_attempts",
        "webhooks",
        "whitelabels"
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "undefined",
    "status": "success"
}
```

#### Fetch the schema definitions

> GET /v2/schemas/{SCHEMANAME}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/schemas/acls
{
    "auth_token": "",
    "data": {
        "$schema": "http://json-schema.org/draft-03/schema#",
        "additionalProperties": false,
        "description": "Access Control List entries",
        "id": "acls",
        "name": "ACL Entry",
        "properties": {
            "cidr": {
                "description": "Classless Inter-Domain Routing IP notation for use on the ACL",
                "name": "CIDR",
                "required": true,
                "type": "string"
            },
            "description": {
                "description": "Will be added as a comment for quick identification later",
                "maxLen": 30,
                "name": "Name",
                "required": false,
                "type": "string"
            },
            "network-list-name": {
                "description": "The trusted list should represent anything that can issue calls without authorization.  The authoritative list should indicate inter-network routing equipment (SBC, etc).",
                "enum": [
                    "authoritative",
                    "trusted"
                ],
                "name": "ACL List",
                "required": true,
                "type": "string"
            },
            "type": {
                "default": "allow",
                "description": "Allow or deny this CIDR",
                "enum": [
                    "allow",
                    "deny"
                ],
                "name": "Type",
                "required": true,
                "type": "string"
            }
        },
        "required": true,
        "type": "object"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Perform a validation

Test your request data against the validation schema (without performing a database operation).

> PUT /v2/schemas/{SCHEMA_NAME}/validation

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{...}}'
    http://{SERVER}:8000/v2/schemas/{ID}/validation
{
    "auth_token":"",
    "data":{...},
    "request_id":"{REQUEST_ID}",
    "revision":"undefined",
    "status":"success"
}
```
