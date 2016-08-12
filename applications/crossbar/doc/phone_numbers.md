### Phone Numbers

The 2600hz mobile API set: manage numbers.


#### Search for numbers

Looks for numbers using the carrier module set up for your account.

- `PREFIX`: a 3-digit number prefix such as an area code (e.g. `415`)
- `QUANTITY`: maximum amount of numbers to be returned (e.g. `2`)
- `OFFSET`: page number (e.g. `0`)

> GET /v2/phone_numbers?prefix={PREFIX}&quantity={QUANTITY}&offset={OFFSET}

```shell
curl -v -X GET \
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


#### Search for available numbers you own

Set `number_manager.search_prefers_available_numbers` to `true` to only look for
available numbers you already bought & imported.

- `PREFIX`: a 3-digit number prefix or an URL-encoded e164 prefix (e.g. `499` or `%2B1499`)
- `QUANTITY`: maximum amount of numbers to be returned (e.g. `2`)
- `OFFSET`: page number (e.g. `0`)

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers?prefix={PREFIX}&quantity={QUANTITY}&offset={OFFSET}

```shell
curl -v -X GET \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers?prefix={PREFIX}&quantity={QUANTITY}&offset={OFFSET}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "number": "+14990000027",
            "state": "reserved"
        },
        {
            "number": "+14990000028",
            "state": "available"
        }
    ],
    "request_id": "d4261f0c19bba7aa19e53dd6a51efa22",
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
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "casquade_quantity": 0,
        "numbers": {
            "+14155555555": {
                "assigned_to": "{ACCOUNT_ID}",
                "created": 63602230185,
                "features": [
                    "local"
                ],
                "state": "in_service",
                "updated": 63602230212,
                "used_by": "callflow"
            },
            "+14158865100": {
                "assigned_to": "{ACCOUNT_ID}",
                "created": 63624719324,
                "state": "in_service",
                "updated": 63624719325
            }
        }
    },
    "page_size": 2,
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


#### Remove a number from the account owning it

> DELETE /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}
```

##### Response

###### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "_read_only": {
            "created": 63627848588,
            "modified": 63627848588,
            "state": "available"
        },
        "id": "{PHONENUMBER}",
        "state": "available"
    },
    "request_id": "df394a4f3175bfe9367919482dc7f217",
    "revision": "undefined",
    "status": "success"
}
```

###### Number not in account

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "message": "bad identifier",
        "not_found": "The number could not be found"
    },
    "error": "404",
    "message": "bad_identifier",
    "request_id": "326492fe35f072b8f65e985e92af32ef",
    "status": "error"
}
```


#### Remove a number from account (admin only)

> DELETE /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}?hard=true
```

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "_read_only": {
            "created": 63627848588,
            "modified": 63627848588,
            "state": "deleted"
        },
        "id": "{PHONENUMBER}",
        "state": "deleted"
    },
    "request_id": "712f7cd2849197639efb5713f4a493fd",
    "revision": "undefined",
    "status": "success"
}
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
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "_read_only": {
            "created": 63627848989,
            "features": [
                "local"
            ],
            "modified": 63627848989,
            "state": "reserved"
        },
        "features": [
            "local"
        ],
        "id": "{PHONENUMBER}",
        "state": "reserved",
        "my_own_field": {}
    },
    "request_id": "609d2ddbc57fbbce22b42be229b67840",
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
    "auth_token": "{AUTH_TOKEN}",
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


#### Update public fields of a number

Note: some public fields are used to configure number features.

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"my_own_field":"some other value", "cnam":{"display_name":"My caller ID", "inbound_lookup":true}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "_read_only": {
            "created": 63635220353,
            "features": [
                "outbound_cnam",
                "inbound_cnam"
            ],
            "modified": 63635220353,
            "state": "in_service",
            "used_by": "callflow"
        },
        "cnam": {
            "display_name": "My caller ID",
            "inbound_lookup": true
        },
        "features": [
            "outbound_cnam",
            "inbound_cnam"
        ],
        "id": "{PHONENUMBER}",
        "state": "in_service",
        "used_by": "callflow"
    },
    "request_id": "7530a90fe4061b095a92eabd7ab872e7",
    "revision": "undefined",
    "status": "success"
}
```


#### Add a number to the database

Adds a number to the database, returning its properties.

Note: payload is facultative.

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"my_own_field": {}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}
```

##### Response

###### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "_read_only": {
            "created": 63627848989,
            "modified": 63627848989,
            "state": "reserved"
        },
        "id": "{PHONENUMBER}",
        "state": "reserved",
        "my_own_field": {}
    },
    "request_id": "609d2ddbc57fbbce22b42be229b67840",
    "revision": "undefined",
    "status": "success"
}
```

###### Failure

####### Number already exists

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "cause": "{PHONENUMBER}",
        "code": 409,
        "error": "number_exists",
        "message": "number {PHONENUMBER} already exists"
    },
    "error": "409",
    "message": "number_exists",
    "request_id": "230c62bcb9ed7f97538e22d72f2c724f",
    "status": "error"
}
```

####### Number does not conform to E.164 format

A non-conforming `{PHONENUMBER}`: `"+141510010+15"`.

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "cause": "{PHONENUMBER}",
        "code": 404,
        "error": "not_reconcilable",
        "message": "number {PHONENUMBER} is not reconcilable"
    },
    "error": "404",
    "message": "not_reconcilable",
    "request_id": "fe5547b999afe27a85a92d97f786ffa4",
    "status": "error"
}
```

####### Account unauthorized

```json
{
    "auth_token": "{AUTH_TOKEN}",
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


#### Check availability of phone numbers

This API check if the numbers are still available for purchase.

- IN <- List of numbers
- OUT -> JSON; Key = Number, Value = status

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/check

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"numbers": [{PHONENUMBER1}, {PHONENUMBER2}]}}'
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/check
```

##### Response

###### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "{PHONENUMBER1}": "success",
        "{PHONENUMBER2}": "error"
    }
    "request_id": "3934255dbf74ac0ff38443450ce8753d",
    "revision": "undefined",
    "status": "success"
}
```

###### Failure

When server encounters an error `"data": {}` is returned.

It may be due to:

* Number not being handled by carrier `other`
* `phonebook` being unresponsive

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {},
    "request_id": "feaf1bc6fb1c4e3531840d188ea67344",
    "revision": "undefined",
    "status": "success"
}
```


#### Get locality information for a collection of numbers

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/locality

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"numbers": ["{PHONENUMBER1}", "{PHONENUMBER2}"]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/locality
```

##### Responses

###### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "{PHONENUMBER1}": {
            "carrier": {
                "company": "T-Mobile USA Inc",
                "dba": "T-Mobile USA Inc",
                "id": "6529",
                "type": "WIRELESS"
            },
            "country": "US",
            "e164_number": "{PHONENUMBER1}",
            "geocode": {
                "locality": "California"
            },
            "locality": {
                "alt_postal_codes": [
                    "94965",
                    "94941"
                ],
                "extended_postal_code": null,
                "latitude": "37.8725359094361",
                "locality": "Belvedere",
                "longitude": "-122.465900466078",
                "postal_code": "94920",
                "province": "CA",
                "switch": "OKLECAZVGT0",
                "type": "WIRELESS"
            },
            "number": "{PHONENUMBER1}",
            "status": "success"
        },
        "{PHONENUMBER2}": {
            "carrier": {
                "company": "Bandwidth.com CLEC LLC - CA",
                "dba": "Bandwidth.com CLEC LLC",
                "id": "981E",
                "type": "CLEC"
            },
            "country": "US",
            "e164_number": "{PHONENUMBER2}",
            "geocode": {
                "locality": "California"
            },
            "locality": {
                "alt_postal_codes": [
                    "94939",
                    "94976"
                ],
                "extended_postal_code": null,
                "latitude": "37.9267845442655",
                "locality": "Corte Madera",
                "longitude": "-122.527924297914",
                "postal_code": "94904",
                "province": "CA",
                "switch": "SNFCCA21XUY",
                "type": "LANDLINE"
            },
            "number": "{PHONENUMBER2}",
            "status": "success"
        }
    },
    "request_id": "22001e34f692908cf0fed818060db6ba",
    "revision": "undefined",
    "status": "success"
}
```

###### Backend to PhoneBook not set up

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": "Unable to acquire numbers missing carrier url",
    "error": "500",
    "message": "init failed",
    "request_id": "58d565bb44e9da3c5500255940d88e1d",
    "status": "error"
}
```


#### List available numbers of a given US city

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/prefix

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/prefix?city={CITY}
```

##### Responses

###### Success

```json

```

###### Country or city not found

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "data": {},
        "error": 404,
        "message": "Not Found",
        "status": "error"
    },
    "error": "500",
    "message": "init failed",
    "request_id": "8346e500b7e1e71423a922f1348f351e",
    "status": "error"
}
```

###### Backend to PhoneBook not set up

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": "Unable to acquire numbers missing carrier url",
    "error": "500",
    "message": "init failed",
    "request_id": "d2beaea7fe3d6b8e195ff9567516c351",
    "status": "error"
}
```


#### Remove a list of numbers from the database

> DELETE /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"numbers": ["{PHONENUMBER1}", "{PHONENUMBER2}", "{PHONENUMBER3}"]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "success": {
            "{PHONENUMBER1": {
                "_read_only": {
                    "created": 63628473168,
                    "modified": 63628473168,
                    "state": "available"
                },
                "id": "{PHONENUMBER1}",
                "state": "available"
            },
            "{PHONENUMBER2}": {
                "_read_only": {
                    "created": 63628473168,
                    "modified": 63628473168,
                    "state": "available"
                },
                "id": "{PHONENUMBER2}",
                "state": "available"
            },
            "{PHONENUMBER3}": {
                "_read_only": {
                    "created": 63628473168,
                    "modified": 63628473168,
                    "state": "available"
                },
                "id": "{PHONENUMBER3}",
                "state": "available"
            }
        }
    },
    "request_id": "144493e2280213db59b81f7377fbff48",
    "revision": "undefined",
    "status": "success"
}
```


#### Remove a list of numbers from account (admin only)

> DELETE /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"numbers": ["{PHONENUMBER1}", "{PHONENUMBER2}", "{PHONENUMBER3}"]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection?hard=true
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "success": {
            "{PHONENUMBER1": {
                "_read_only": {
                    "created": 63628473168,
                    "modified": 63628473168,
                    "state": "deleted"
                },
                "id": "{PHONENUMBER1}",
                "state": "deleted"
            },
            "{PHONENUMBER2}": {
                "_read_only": {
                    "created": 63628473168,
                    "modified": 63628473168,
                    "state": "deleted"
                },
                "id": "{PHONENUMBER2}",
                "state": "deleted"
            },
            "{PHONENUMBER3}": {
                "_read_only": {
                    "created": 63628473168,
                    "modified": 63628473168,
                    "state": "deleted"
                },
                "id": "{PHONENUMBER3}",
                "state": "deleted"
            }
        }
    },
    "request_id": "34cfbf7737f18b95bd3120822e3947b1",
    "revision": "undefined",
    "status": "success"
}
```


#### Update public fields of a list of numbers

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"numbers": ["{PHONENUMBER1}", "{PHONENUMBER2}"], "myfield": 1337}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "success": {
            "{PHONENUMBER1}": {
                "_read_only": {
                    "created": 63628454912,
                    "modified": 63628454912,
                    "state": "reserved"
                },
                "id": "{PHONENUMBER1}",
                "myfield": 1337,
                "state": "reserved"
            },
            "{PHONENUMBER2}": {
                "_read_only": {
                    "created": 63628454912,
                    "modified": 63628454912,
                    "state": "reserved"
                },
                "id": "{PHONENUMBER2}",
                "myfield": 1337,
                "state": "reserved"
            }
        }
    },
    "request_id": "58191a05bcbbf528009fc5f4f0fe7ce5",
    "revision": "undefined",
    "status": "success"
}
```


#### Add a list of numbers to the database

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"numbers": ["{PHONENUMBER1}", "{PHONENUMBER2}", "{PHONENUMBER3}"]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection
```

##### Responses

###### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "success": {
            "{PHONENUMBER1}": {
                "_read_only": {
                    "created": 63628454912,
                    "modified": 63628454912,
                    "state": "reserved"
                },
                "id": "{PHONENUMBER1}",
                "state": "reserved"
            },
            "{PHONENUMBER2}": {
                "_read_only": {
                    "created": 63628454912,
                    "modified": 63628454912,
                    "state": "reserved"
                },
                "id": "{PHONENUMBER2}",
                "state": "reserved"
            },
            "{PHONENUMBER3}": {
                "_read_only": {
                    "created": 63628454912,
                    "modified": 63628454912,
                    "state": "reserved"
                },
                "id": "{PHONENUMBER3}",
                "state": "reserved"
            }
        }
    },
    "request_id": "f0e0f0abdabaa2f0bb39d83ad3e3f3c6",
    "revision": "undefined",
    "status": "success"
}
```

###### Failure

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "{PHONENUMBER2}": {
            "cause": "{PHONENUMBER2}",
            "code": 409,
            "error": "number_exists",
            "message": "number {PHONENUMBER2} already exists"
        },
        "{PHONENUMBER3}": {
            "cause": "{PHONENUMBER3}",
            "code": 409,
            "error": "number_exists",
            "message": "number {PHONENUMBER3} already exists"
        }
    },
    "error": "400",
    "message": "client error",
    "request_id": "34fded1acd0c8a440515fc6d42e409b6",
    "status": "error"
}
```


#### List classifiers

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "caribbean": {
            "friendly_name": "Caribbean",
            "pretty_print": "SS(###) ### - ####",
            "regex": "^\\+?1((?:684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340)\\d{7})$"
        },
        "did_us": {
            "friendly_name": "US DID",
            "pretty_print": "SS(###) ### - ####",
            "regex": "^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$"
        },
        "emergency": {
            "friendly_name": "Emergency Dispatcher",
            "regex": "^(911)$"
        },
        "international": {
            "friendly_name": "International",
            "regex": "^(011\\d*)$|^(00\\d*)$"
        },
        "toll_us": {
            "friendly_name": "US Toll",
            "pretty_print": "SS(###) ### - ####",
            "regex": "^\\+1(900\\d{7})$"
        },
        "tollfree_us": {
            "friendly_name": "US TollFree",
            "pretty_print": "SS(###) ### - ####",
            "regex": "^\\+1((?:800|888|877|866|855)\\d{7})$"
        },
        "unknown": {
            "friendly_name": "Unknown",
            "regex": "^(.*)$"
        }
    },
    "request_id": "3a7cf0257c33fa959263dec20b184fe5",
    "revision": "undefined",
    "status": "success"
}
```


#### Fix issues

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/fix

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/fix
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "casquade_quantity": 0,
        "numbers": {
            "+14152338421": {
                "assigned_to": "{ACCOUNT_ID}",
                "created": 63627334163,
                "state": "in_service",
                "updated": 63627447350
            },
            "+14155555555": {
                "assigned_to": "{ACCOUNT_ID}",
                "created": 63602230185,
                "state": "in_service",
                "updated": 63602230212,
                "used_by": "callflow"
            },
            "+14158865100": {
                "assigned_to": "{ACCOUNT_ID}",
                "created": 63624719324,
                "state": "in_service",
                "updated": 63624719325
            }
        }
    },
    "page_size": 2,
    "request_id": "923fce7eec4d13d5e7df09ca6fbbcadd",
    "revision": "19-d21bca301d4721b03f368b73de35f813",
    "status": "success"
}
```


#### Return which account a number belongs to

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/identify

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/identify
```

##### Responses

###### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "account_id": "009afc511c97b2ae693c6cc4920988e8",
        "number": "{PHONENUMBER}"
    },
    "request_id": "f8cee053b9992435924eaa1554d7555d",
    "revision": "undefined",
    "status": "success"
}
```

###### Number not found or not enough privileges

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "message": "bad identifier",
        "not_found": "The number could not be found"
    },
    "error": "404",
    "message": "bad_identifier",
    "request_id": "8c3fa68c1ebf758ca4e1628e2048fb16",
    "status": "error"
}
```


#### Move a number to the reserved state

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/reserve

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/reserve
```

##### Responses

###### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "_read_only": {
            "created": 63628556896,
            "modified": 63628556896,
            "state": "reserved"
        },
        "id": "{PHONENUMBER}",
        "state": "reserved"
    },
    "request_id": "86d675b8d76e35f69328922d5607f200",
    "revision": "undefined",
    "status": "success"
}
```

###### Number already in reserved state

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "code": 400,
        "error": "no_change_required",
        "message": "no change required"
    },
    "error": "400",
    "message": "no_change_required",
    "request_id": "e26c4a9d57890f73a37762990b469592",
    "status": "error"
}
```

###### Number does not exist

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "message": "bad identifier",
        "not_found": "The number could not be found"
    },
    "error": "404",
    "message": "bad_identifier",
    "request_id": "b36a44a38086ec1b630790e5645b8045",
    "status": "error"
}
```


#### Buy a number once searched for

Note: one is not charged if number is already in service.

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/activate

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/activate
```

##### Responses

###### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "_read_only": {
            "created": 63628027112,
            "modified": 63628027112,
            "state": "in_service"
        },
        "id": "{PHONENUMBER}",
        "state": "in_service"
    },
    "request_id": "4a1a73bfa12d11ac63c74d377cd961f6",
    "revision": "undefined",
    "status": "success"
}
```

###### Number was not returned in previous search results or other error

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "code": 500,
        "error": "unspecified_fault",
        "message": "missing_provider_url"
    },
    "error": "500",
    "message": "unspecified_fault",
    "request_id": "9a1deab4464a3dbd1d5b5b9d93d8d790",
    "status": "error"
}
```

###### Carrier fault

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "cause": "{PHONENUMBER}",
        "code": 500,
        "error": "unspecified_fault",
        "message": "fault by carrier"
    },
    "error": "500",
    "message": "unspecified_fault",
    "request_id": "0800ba14b0dfce2417185eb15c3a8f96",
    "status": "error"
}
```

#### Classify a number

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers/{PHONENUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers/{PHONENUMBER}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "e164": "+1{PHONENUMBER}",
        "friendly_name": "US DID",
        "name": "did_us",
        "number": "{PHONENUMBER}",
        "pretty_print": "SS(###) ### - ####",
        "regex": "^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$"
    },
    "request_id": "89bd26ad10ea97ad789fbde584be9568",
    "revision": "undefined",
    "status": "success"
}
```


#### Buy a list of numbers

Note: numbers must have appeared as part of the results of a numbers search.

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection/activate

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"numbers": ["{PHONENUMBER1}", "{PHONENUMBER2}"]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/phone_numbers/collection/activate
```

##### Responses

###### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "success": {
            "{PHONENUMBER1}": {
                "_read_only": {
                    "created": 63628542222,
                    "modified": 63628542222,
                    "state": "in_service"
                },
                "id": "{PHONENUMBER1}",
                "state": "in_service"
            },
            "{PHONENUMBER2}": {
                "_read_only": {
                    "created": 63628542222,
                    "modified": 63628542222,
                    "state": "in_service"
                },
                "id": "{PHONENUMBER2}",
                "state": "in_service"
            }
        }
    },
    "request_id": "1fd30d420f3c4f601e05f8f9eb041e46",
    "revision": "undefined",
    "status": "success"
}
```

###### Number not found or other error

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "{PHONENUMBER2}": {
            "code": 500,
            "error": "unspecified_fault",
            "message": "missing_provider_url"
        }
    },
    "error": "400",
    "message": "client error",
    "request_id": "892b3cc884bd913cd3912c5b4c757421",
    "status": "error"
}
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
