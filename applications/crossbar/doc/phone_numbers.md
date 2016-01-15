/*
Section: Crossbar
Title: Phone Numbers
Language: en-US
Version: 3.18
*/

### Phone Numbers
Learn how to use the 2600hz mobile API set to activate and manage numbers.


#### Check Phone Numbers availability

This API check if the numbers are still available for purchase.

- IN <- List of numbers
- OUT -> JSON; Key = Number, Value = status

##### Request

- Verb: `POST`
- Url: `/accounts/ACCOUNT_ID/phone_numbers/check`
- Payload:

        {"data": {
             "numbers": [
                 "+14159383408",
                 "+14156715576"
             ]
         }
        }

##### Response

```
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

##### _GET_ Classifier for a number

    curl -X GET -H "Content-Type: application/json" http://crossbar:8000/v2/phone_numbers/classifiers/4158867900
    {"auth_token": "{AUTH_TOKEN}"
     ,"data": {
         "e164": "+14158867900"
         ,"friendly_name": "US DID"
         ,"name": "did_us"
         ,"number": "4158867900"
         ,"pretty_print": "SS(###) ##### - ####"
         ,"regex": "^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$"
     }
     ,"request_id": {REQUEST_ID}
     ,"revision": "undefined"
     ,"status": "success"
    }


#### Fix Phone Numbers

##### Request

- Verb: `POST`
- Url: `v2/accounts/ACCOUNT_ID/phone_numbers/fix`
- Payload: None

##### Response

```
{
    "data": {}
    "status": "success"
}
```

#### Activate a new phone number

    curl -X PUT -H "Content-Type: application/json" -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    http://server:8000/v2/accounts/{{ACCOUNT_ID}}/phone_numbers/{{NUMBER}}/activate -d '{}'


#### E911

##### Request

- Verb: `POST`
- Url: `v2/accounts/{{ACCOUNT_ID}}/phone_numbers/{{NUMBER}}`
- Payload:

```
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

```
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

```
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

```
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
