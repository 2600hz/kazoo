### Dialplans

#### About Dialplans

#### Schema

Permit local dialing by converting the dialed number to a routable form



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`system.[]` |   | `string()` |   | `false` |  
`system` | List of system dial plans | `array(string())` |   | `false` |  



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/dialplans

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/dialplans
```

Also make sure the dialplans API endpoint is started: `sup crossbar_maintenance start_module cb_dialplans`

