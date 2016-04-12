### Phone Numbers

The 2600hz mobile API set: activate and manage numbers.


#### Search for numbers

Looks for numbers using the carrier module set up for your account.

- `PREFIX`: a 3-digit number prefix such as an area code (e.g. `415`)
- `QUANTITY`: maximum amount of numbers to be returned (e.g. `2`)
- `OFFSET`: page number (e.g. `0`)

> GET /v2/phone_numbers?prefix={PREFIX}&quantity={QUANTITY}&offset={OFFSET}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/phone_numbers?prefix={PREFIX}&quantity={QUANTITY}&offset={OFFSET}
```

```json
{
    "auth_token": "",
    "data": [
        {
            "e164": "+14152338397",
            "formatted_number": "1-415-233-8397",
            "npa_nxx": "415233",
            "number": "+14152338397",
            "number_id": "4AA418FB-3409-4340-8210-E7EAFE2AB118",
            "rate_center": {
                "lata": "722",
                "name": "SAN RAFAEL",
                "state": "CA"
            },
            "status": "Available",
            "ten_digit": "4152338397"
        },
        {
            "e164": "+14152338421",
            "formatted_number": "1-415-233-8421",
            "npa_nxx": "415233",
            "number": "+14152338421",
            "number_id": "0CD68E85-F149-477F-9C13-1E720ACCC3EE",
            "rate_center": {
                "lata": "722",
                "name": "SAN RAFAEL",
                "state": "CA"
            },
            "status": "Available",
            "ten_digit": "4152338421"
        }
    ],
    "request_id": "1c9a13a0f729c2d6d35a9c4515e5da03",
    "revision": "undefined",
    "status": "success"
}
```


#### List an account's phone numbers

This lists the numbers an account owns, along with their properties.

Note: one can apply filters such as `?filter_state=in_service` or `?created_from=63627345744`

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers
```

```json
{
    "auth_token": "1931484e3fba5777588176584828e7be",
    "data": {
        "casquade_quantity": 0,
        "numbers": {
            "+14155555555": {
                "assigned_to": "4b8c6fec4b2597882c0390202d195419",
                "created": 63602230185,
                "features": {},
                "state": "in_service",
                "updated": 63602230212,
                "used_by": "callflow"
            },
            "+14158865100": {
                "assigned_to": "4b8c6fec4b2597882c0390202d195419",
                "created": 63624719324,
                "features": {},
                "state": "in_service",
                "updated": 63624719325,
                "used_by": ""
            }
        }
    },
    "request_id": "923fce7eec4d13d5e7df09ca6fbbcadd",
    "revision": "19-d21bca301d4721b03f368b73de35f813",
    "status": "success"
}
```


### Per-number CRUD operations

- Note: `PHONENUMBER` has to be URL-encoded
    * e.g. turn `+14155555555` into `%2B14155555555`
    * Note `4123456789` is turned into `+14123456789`
    * Note however, `41234567` is turned into `+41234567`, so be careful!
- Note: to add/modify numbers, either:
    * Account document must be showing `pvt_wnm_allow_additions` as `true`
    * Or auth must be done via master account.

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}
```

#### List an account's specific phone number

Show the number's properties along with user-defined properties.

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}
```

##### Response

###### Success

```json
{
    "auth_token": "1a82795e8759f62d77f6552de80ce32e",
    "data": {
        "id": "+14155555555",
        "my_own_field": {},
        "assigned_to": "4b8c6fec4b2597882c0390202d195419",
        "created": 63602230185,
        "features": {},
        "state": "in_service",
        "updated": 63602230212,
        "used_by": "callflow"
    },
    "request_id": "e268c22be3076a12d8581ebab0d89cc8",
    "revision": "undefined",
    "status": "success"
}
```

###### Failure

Possible reasons for failure:

* Account does not have enough privileges to read number
* Number does not exist

```json
{
    "auth_token": "1a82795e8759f62d77f6552de80ce32e",
    "data": {
        "message": "bad identifier",
        "not_found": "The number could not be found"
    },
    "error": "404",
    "message": "bad_identifier",
    "request_id": "94cf464971722272dd0b04cbc491303f",
    "status": "error"
}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}
```

#### Add a number to the database

Adds a number to the database, returning its properties.

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"my_field": "my value"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}
```

##### Response

###### Success

```json
{
    "auth_token": "9376332cc1c4ef5fc31371bfaa92ff0a",
    "data": {
        "assigned_to": "009afc511c97b2ae693c6cc4920988e8",
        "created": 63627604811,
        "my_field": "my value",
        "id": "+14242424248",
        "state": "reserved",
        "updated": 63627604811
    },
    "request_id": "c62107997ee2b31c978077953fafaebd",
    "revision": "undefined",
    "status": "success"
}
```

###### Failure

####### Number already exists

```json
{
    "auth_token": "9376332cc1c4ef5fc31371bfaa92ff0a",
    "data": {
        "cause": "+14242424246",
        "code": 409,
        "error": "number_exists",
        "message": "number +14242424246 already exists"
    },
    "error": "409",
    "message": "number_exists",
    "request_id": "230c62bcb9ed7f97538e22d72f2c724f",
    "status": "error"
}
```

####### Account unauthorized

```json
{
    "auth_token": "b913eb25a2bc681671414a9a8ca8a0e9",
    "data": {
        "message": "unknown failure",
        "unauthorized": "Not authorized to perform requested number operation"
    },
    "error": "500",
    "message": "unauthorized",
    "request_id": "e74ecf82e529dd2fa2a1dc57af3783d9",
    "status": "error"
}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/check

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/check
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/locality

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/locality
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/prefix

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/prefix
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/fix

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/fix
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/identify

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/identify
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/reserve

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/reserve
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/activate

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/activate
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers/{PHONENUMBER}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers/{PHONENUMBER}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection/activate

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection/activate
```


#### Check Phone Numbers availability

This API check if the numbers are still available for purchase.

- IN <- List of numbers
- OUT -> JSON; Key = Number, Value = status

##### Request

- Verb: `POST`
- Url: `/accounts/{{ACCOUNT_ID}}/phone_numbers/check`
- Payload:

```json
{
    "data": {
        "numbers": [
            "+14159383408",
            "+14156715576"
        ]
    }
}
```

##### Response

```json
{
    "data": {
        "+14159383408": "success"
        "+14156715576": "error"
    }
    "revision": "undefined"
    "request_id": "3934255dbf74ac0ff38443450ce8753d"
    "status": "success"
    "auth_token": "7797206dda2166f139b18eee58e64c79"
}
```

#### Check classifier for a number

##### Request

```shell
curl -X GET -H 'Content-Type: application/json' http://{{SERVER}}:8000/v2/phone_numbers/classifiers/4158867900
```

##### Response

```json
{
    "auth_token": "{{AUTH_TOKEN}}"
     ,"data": {
         "e164": "+14158867900"
         ,"friendly_name": "US DID"
         ,"name": "did_us"
         ,"number": "4158867900"
         ,"pretty_print": "SS(###) ##### - ####"
         ,"regex": "^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$"
     }
     ,"request_id": {{REQUEST_ID}}
     ,"revision": "undefined"
     ,"status": "success"
}
```


#### Fix Phone Numbers

##### Request

- Verb: `POST`
- Url: `/v2/accounts/{{ACCOUNT_ID}}/phone_numbers/fix`
- Payload: none

##### Response

```json
{
    "data": {}
    "status": "success"
}
```

#### Activate a new phone number

```shell
curl -X PUT -H "Content-Type: application/json" -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/phone_numbers/{{NUMBER}}/activate -d '{}'
```

#### E911

##### Request

- Verb: `POST`
- Url: `/v2/accounts/{{ACCOUNT_ID}}/phone_numbers/{{NUMBER}}`
- Payload:

```json
{
    "data": {
        "used_by": "callflow",
        "id": "{{NUMBER}}",
        "dash_e911": {
            "postal_code": "{{ZIP_CODE}}",
            "street_address": "{{ADDRESS}}",
            "extended_address": "{{EXTENDED}}",
            "locality": "{{CITY}}",
            "region": "{{STATE}}"
        }
    }
}
```

##### Response

###### Invalid address

```json
{
    "data": {
        "address": {
            "invalid": {
                "cause": {
                    "postal_code": "{{ZIP_CODE}}",
                    "street_address": "{{ADDRESS}}",
                    "extended_address": "{{EXTENDED}}",
                    "locality": "{{CITY}}",
                    "region": "{{STATE}}"
                },
                "message": "Location is not geocoded",
                "provider": "dash_e911"
            }
        }
    },
    "error": "400",
    "message": "invalid data",
    "status": "error"
}
```
###### Multiple choice

```json
{
    "data": {
        "multiple_choice": {
            "dash_e911": {
                "cause": {
                    "postal_code": "{{ZIP_CODE}}",
                    "street_address": "{{ADDRESS}}",
                    "extended_address": "{{EXTENDED}}",
                    "locality": "{{CITY}}",
                    "region": "{{STATE}}"
                },
                "details": [{
                    "postal_code": "{{ZIP_CODE}}",
                    "street_address": "{{ADDRESS}}",
                    "extended_address": "{{EXTENDED}}",
                    "locality": "{{CITY}}",
                    "region": "{{STATE}}"
                }, {
                    "postal_code": "{{ZIP_CODE}}",
                    "street_address": "{{ADDRESS}}",
                    "extended_address": "{{EXTENDED}}",
                    "locality": "{{CITY}}",
                    "region": "{{STATE}}"
                }],
                "message": "more than one address found"
            }
        }
    },
    "error": "400",
    "message": "multiple_choice",
    "status": "error"
}
```

###### Success

```json
{
    "data": {
        "used_by": "callflow",
        "id": "{{NUMBER}}",
        "dash_e911": {
            "street_address": "116 NATOMA ST",
            "extended_address": "APT 116",
            "caller_name": "Valued Customer",
            "locality": "SAN FRANCISCO",
            "latitude": "37.786861",
            "longitude": "-122.399484",
            "location_id": "27578725",
            "plus_four": "3745",
            "postal_code": "94105",
            "region": "CA",
            "status": "PROVISIONED",
            "legacy_data": {
                "house_number": "116",
                "streetname": "NATOMA ST",
                "suite": "APT 116"
            }
        }
    },
    "status": "success"
}
```


#### Remove a number from its account

Delete a number without returning its properties.

##### Request

- Verb: `DELETE`
- Url: `/v2/accounts/{{ACCOUNT_ID}}/phone_numbers/{{PHONENUMBER}}`
- Payload: none

##### Response

###### Success

```json
{
    "auth_token": "1a82795e8759f62d77f6552de80ce32e",
    "data": {},
    "request_id": "7fc1a404904d140b55246d76e9575580",
    "revision": "undefined",
    "status": "success"
}
```



### Load classifiers

#### Request

`GET` `http://{{SERVER}}/v2/accounts/{{account_id}}/phone_numbers/classifiers`

#### Response

```json
{
    "data": {
        "tollfree_us": {
            "regex": "^\\\\+1((?:800|888|877|866|855)\\\\d{7})$",
            "friendly_name": "US TollFree",
            "pretty_print": "SS(###) ### - ####"
        },
        "toll_us": {
            "regex": "^\\\\+1(900\\\\d{7})$",
            "friendly_name": "US Toll",
            "pretty_print": "SS(###) ### - ####"
        },
        "emergency": {
            "regex": "^(911)$",
            "friendly_name": "Emergency Dispatcher"
        },
        "caribbean": {
            "regex": "^\\\\+?1((?:684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340)\\\\d{7})$",
            "friendly_name": "Caribbean",
            "pretty_print": "SS(###) ### - ####"
        },
        "did_us": {
            "regex": "^\\\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$",
            "friendly_name": "US DID",
            "pretty_print": "SS(###) ### - ####"
        },
        "international": {
            "regex": "^(011\\\\d*)$|^(00\\\\d*)$",
            "friendly_name": "International"
        },
        "unknown": {
            "regex": "^(.*)$",
            "friendly_name": "Unknown"
        }
    },
    "status": "success"
}
```

### Classify a number

#### Request

`GET` `http://{{SERVER}}/v2/accounts/{{account_id}}/phone_numbers/classifiers/{{NUMBER}}`

#### Response

```json
{
    "data": {
        "regex": "^\\\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$",
        "friendly_name": "US DID",
        "pretty_print": "SS(###) ### - ####",
        "e164": "+14158551292",
        "number": "4158551292",
        "name": "did_us"
    },
    "status": "success"
}
```

### Locality

#### Request

`POST` `http://{{SERVER}}/v2/accounts/{{account_id}}/phone_numbers/locality`

```json
{
    "data": {
        "numbers": ["{{NUMBER1}}", "{{NUMBER2}}"]
    }
}
```

#### Response

```json
{
    "data": {
        "{{NUMBER1}}": {
            "country": "US",
            "number": "{{NUMBER1}}",
            "e164_number": "{{NUMBER1}}",
            "geocode": {
                "locality": "California"
            },
            "locality": {
                "locality": "San Francisco",
                "province": "CA",
                "postal_code": "94108",
                "extended_postal_code": null,
                "latitude": "37.789973706997",
                "longitude": "-122.406446128903",
                "type": "WIRELESS",
                "alt_postal_codes": [
                    "94104",
                    "94103"
                ],
                "switch": "098IGUH"
            },
            "carrier": {
                "id": "6529",
                "type": "WIRELESS",
                "company": "T-Mobile USA Inc",
                "dba": "T-Mobile USA Inc"
            },
            "status": "success"
        },
        "{{NUMBER2}}": {
            "country": "US",
            "number": "{{NUMBER2}}",
            "e164_number": "{{NUMBER2}}",
            "geocode": {
                "locality": "San Francisco, CA"
            },
            "locality": {
                "locality": "San Francisco",
                "province": "CA",
                "postal_code": "94108",
                "extended_postal_code": null,
                "latitude": "37.789973706997",
                "longitude": "-122.406446128903",
                "type": "WIRELESS",
                "alt_postal_codes": [
                    "94104",
                    "94103"
                ],
                "switch": "098IGUH"
            },
            "carrier": {
                "id": "6529",
                "type": "WIRELESS",
                "company": "T-Mobile USA Inc",
                "dba": "T-Mobile USA Inc"
            },
            "status": "success"
        }
    },
    "status": "success"
}
```

### Fix

#### Request

`GET` `http://{{SERVER}}/v2/accounts/{{account_id}}/phone_numbers/fix`

#### Response

[SEE](#user-content-list-accounts-phone-numbers)
