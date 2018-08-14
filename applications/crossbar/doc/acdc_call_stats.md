### Acdc Call Stats

#### About Acdc Call Stats

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/acdc_call_stats

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/acdc_call_stats
```

Get a time range of ACDC Call Stats (using Gregorian seconds for timestamps):

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/acdc_call_stats?created_from={FROM_TIMESTAMP}&created_to={TO_TIMESTAMP}
```

Get ACDC Call Stats as CSV:

```shell
curl -v -X GET \
    -H "Accept: text/csv" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/acdc_call_stats
```
