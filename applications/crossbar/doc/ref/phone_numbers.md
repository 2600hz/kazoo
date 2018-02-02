# Phone Numbers

## About Phone Numbers

#### Schema

Schema for a number



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`carrier_name` |   | `string(1..30)` |   | `false` |  
`cnam.display_name` |   | `string(1..15)` |   | `false` |  
`cnam.inbound_lookup` |   | `boolean()` |   | `false` |  
`cnam` |   | `object()` |   | `false` |  
`create_with_state` | The state to create numbers in | `string('aging' | 'available' | 'deleted' | 'discovery' | 'in_service' | 'port_in' | 'port_out' | 'released' | 'reserved')` |   | `false` |  
`e911.activated_time` | The time stamp e911 was provisioned | `string()` |   | `false` |  
`e911.caller_name` | The name that will show to emergency services | `string(3..)` |   | `false` |  
`e911.extended_address` | The suit/floor/apt. address where the number is in service | `string()` |   | `false` |  
`e911.latitude` | The e911 provisioning system calculated service address latitude | `string()` |   | `false` |  
`e911.legacy_data.house_number` | The name that will show to emergency services | `string()` |   | `false` |  
`e911.legacy_data.predirectional` | The name that will show to emergency services | `string()` |   | `false` |  
`e911.legacy_data.streetname` | The name that will show to emergency services | `string()` |   | `false` |  
`e911.legacy_data.suite` | The name that will show to emergency services | `string()` |   | `false` |  
`e911.legacy_data` | Legacy E911 information | `object()` |   | `false` |  
`e911.locality` | The locality (city) where the number is in service | `string()` |   | `true` |  
`e911.location_id` | The e911 provisioning system internal id for this service address | `string()` |   | `false` |  
`e911.longitude` | The e911 provisioning system calculated service address longitude | `string()` |   | `false` |  
`e911.plus_four` | The extended zip/postal code where the number is in service | `string()` |   | `false` |  
`e911.postal_code` | The zip/postal code where the number is in service | `string()` |   | `true` |  
`e911.region` | The region (state) where the number is in service | `string(2)` |   | `true` |  
`e911.status` | The e911 provisioning system status for this service address | `string('INVALID' | 'GEOCODED' | 'PROVISIONED' | 'REMOVED' | 'ERROR')` |   | `false` |  
`e911.street_address` | The street address where the number is in service | `string()` |   | `true` |  
`e911` |   | `object()` |   | `false` |  
`porting.billing_account_id` | The account id the losing carrier has on file | `string()` |   | `false` |  
`porting.billing_extended_address` | The suit/floor/apt. address the losing carrier has on file | `string()` |   | `false` |  
`porting.billing_locality` | The locality (city) the losing carrier has on file | `string()` |   | `false` |  
`porting.billing_name` | The name or company name the losing carrier has on file | `string()` |   | `false` |  
`porting.billing_postal_code` | The zip/postal code the losing carrier has on file | `string()` |   | `false` |  
`porting.billing_region` | The region (state) the losing carrier has on file | `string()` |   | `false` |  
`porting.billing_street_address` | The street address the losing carrier has on file | `string()` |   | `false` |  
`porting.billing_telephone_number` | The BTN of the account the number belongs to | `string()` |   | `false` |  
`porting.comments.[]` |   | `string()` |   | `false` |  
`porting.comments` | An array of comments | `array(string())` |   | `false` |  
`porting.customer_contact` | The phone number that can be used to contact the owner of the number | `string()` |   | `false` |  
`porting.port_id` | The id of the port request | `string()` |   | `false` |  
`porting.requested_port_date` | The requested port date | `string()` |   | `false` |  
`porting.service_provider` | The name of the losing carrier | `string()` |   | `false` |  
`porting` | Porting (in) information for the phone number | `object()` |   | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/check

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/check
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/locality

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/locality
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/prefix

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/prefix
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/fix

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/fix
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/carriers_info

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/carriers_info
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}/identify

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}/identify
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}/port

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}/port
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}/reserve

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}/reserve
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}/activate

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONE_NUMBER}/activate
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers/{PHONE_NUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers/{PHONE_NUMBER}
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection/activate

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection/activate
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/fix/{PHONE_NUMBER}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/fix/{PHONE_NUMBER}
```

