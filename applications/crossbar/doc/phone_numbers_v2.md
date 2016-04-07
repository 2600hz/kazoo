/*
Section: Crossbar
Title: Phone Numbers
Language: en-US
Version: 3.18
*/

# Phone Numbers
Learn how to use the 2600hz mobile API set to activate and manage numbers.

## APIs

* [Load classifiers](#user-content-load-classifiers)
* [Classify a number](#user-content-classify-a-number)
* [Get a number](#user-content-get-a-number)
* [Locality](#user-content-locality)
* [Fix](#user-content-fix)


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

### Get a number

#### Request

`GET` `http://{{SERVER}}/v2/accounts/{{account_id}}/phone_numbers/{{NUMBER}}`

#### Response

```json
{
    "data": {
        "id": "+14158551292"
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