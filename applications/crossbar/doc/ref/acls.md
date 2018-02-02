# Acls

## About Acls

#### Schema

Access Control List entries



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`cidr` | Classless Inter-Domain Routing IP notation for use on the ACL | `string()` |   | `true` |  
`description` | Will be added as a comment for quick identification later | `string(0..30)` |   | `false` |  
`network-list-name` | The trusted list should represent anything that can issue calls without authorization.  The authoritative list should indicate inter-network routing equipment (SBC, etc). | `string('authoritative' | 'trusted')` |   | `true` |  
`type` | Allow or deny this CIDR | `string('allow' | 'deny')` | `allow` | `true` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/acls

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/acls
```

