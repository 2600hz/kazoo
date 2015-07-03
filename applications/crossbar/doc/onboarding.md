# Onboarding

Used to create new accounts without being signed in.

## An example request
Taken from https://2600hz.atlassian.net/browse/KAZOO-63

PUT to `/v1/onboard`
```
{
    "data": {
        "account": {
            "available_apps": [
                "voip", 
                "cluster", 
                "userportal", 
                "accounts", 
                "developer", 
                "numbers", 
                "pbxs"
            ], 
            "caller_id": {
                "default": {
                    "number": "+14152753640"
                }, 
                "emergency": {
                    "number": "+14152753640"
                }
            }, 
            "default_api_url": "http://192.168.1.61:8000/v1", 
            "name": "testcompany", 
            "role": "reseller"
        }, 
        "braintree": {
            "company": "testcompany", 
            "credit_card": {
                "billing_address": {
                    "country": "US", 
                    "extended_address": "2", 
                    "first_name": "Testname", 
                    "last_name": "", 
                    "locality": "San Francisco", 
                    "postal_code": "94105", 
                    "region": "California", 
                    "street_address": "116 Natoma st."
                }, 
                "cardholder_name": "Testname", 
                "cvv": "411", 
                "expiration_date": "01/2014", 
                "make_default": true, 
                "number": "4111111111111111"
            }, 
            "email": "pmccoy@2600hz.com", 
            "first_name": "Testname", 
            "last_name": ""
        }, 
        "extensions": [
            {
                "callflow": {
                    "numbers": [
                        "+14152753640", 
                        "1"
                    ]
                }, 
                "user": {
                    "apps": {
                        "accounts": {
                            "api_url": "http://192.168.1.61:8000/v1", 
                            "icon": "account", 
                            "label": "Accounts"
                        }, 
                        "numbers": {
                            "api_url": "http://192.168.1.61:8000/v1", 
                            "icon": "menu1", 
                            "label": "Number Manager"
                        }, 
                        "voip": {
                            "api_url": "http://192.168.1.61:8000/v1", 
                            "icon": "phone", 
                            "label": "Hosted PBX"
                        }
                    }, 
                    "credentials": "a9e4685a973f0ed2844ee9f36e211736", 
                    "email": "pmccoy@2600hz.com", 
                    "first_name": "test", 
                    "last_name": "name", 
                    "priv_level": "admin"
                }
            }, 
            {
                "callflow": {
                    "numbers": [
                        "2"
                    ]
                }, 
                "user": {
                    "first_name": "test2", 
                    "last_name": "name", 
                    "priv_level": "user"
                }
            }, 
            {
                "callflow": {
                    "numbers": [
                        "3"
                    ]
                }, 
                "user": {
                    "first_name": "test3", 
                    "last_name": "name", 
                    "priv_level": "user"
                }
            }, 
            {
                "callflow": {
                    "numbers": [
                        "4"
                    ]
                }, 
                "user": {
                    "first_name": "test4", 
                    "last_name": "name", 
                    "priv_level": "user"
                }
            }, 
            {
                "callflow": {
                    "numbers": [
                        "5"
                    ]
                }, 
                "user": {
                    "first_name": "test5", 
                    "last_name": "name", 
                    "priv_level": "user"
                }
            }
        ], 
        "invite_code": "9351b14aa94b9d580dea57b8deefff0c", 
        "phone_numbers": {
            "+14152753640": {
                "dash_e911": {
                    "extended_address": "2", 
                    "locality": "San Francisco", 
                    "postal_code": "94105", 
                    "region": "California", 
                    "street_address": "116 Natoma st."
                }
            }
        }
    }, 
    "verb": "PUT"
}
```
