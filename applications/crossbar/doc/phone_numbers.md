
### Phone Numbers
The 2600hz mobile API set: activate and manage numbers.


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


#### List an account's phone numbers

This lists the numbers an account owns, along with their properties.

##### Request

- Verb: `GET`
- Url: `/v2/accounts/{{ACCOUNT_ID}}/phone_numbers`
- Payload: none
- Note: one can apply filters such as `?filter_state=in_service` or `?created_from=63627345744`

##### Response

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


#### Search for numbers

Looks for numbers using the carrier module set up for your account.

##### Request

- Verb: `GET`
- Url: `/v2/phone_numbers?prefix={PREFIX}&quantity={QUANTITY}&offset={OFFSET}`
- Payload: none
- `PREFIX`: a 3-digit number prefix such as an area code (e.g. `415`)
- `QUANTITY`: maximum amount of numbers to be returned (e.g. `2`)
- `OFFSET`: page number (e.g. `0`)

##### Response

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
