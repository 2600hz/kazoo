### Onboarding

Used to create new accounts without being signed in.

#### NOTICE

This API has been deprecated and is no longer maintained by the 2600Hz core team.  The preferred method would be to code your own middleware to create accounts using an appropriate parent account API key or auth-token.  The PHP SDK is an excellent starting point or you can roll your own!

#### An example request

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
                    "number": "{COMPANY_PRIMARY_CALLERID}"
                },
                "emergency": {
                    "number": "{COMPANY_EMERGENCY_CALLERID}"
                }
            },
            "default_api_url": "http://{SERVER}/v1",
            "name": "{COMPANY_NAME}",
            "role": "{ACCOUNT_TYPE}"
        },
        "braintree": {
            "company": "{COMPANY_NAME}",
            "credit_card": {
                "billing_address": {
                    "country": "{COMPANY_COUNTRY}",
                    "extended_address": "{COMPANY_EXTENDED_ADDRESS}",
                    "first_name": "{BILLING_CONTACT_FIRST_NAME}",
                    "last_name": "{BILLING_CONTACT_LAST_NAME}",
                    "locality": "{COMPANY_CITY}",
                    "postal_code": "{COMPANY_ZIP_CODE}",
                    "region": "{COMPANY_STATE}",
                    "street_address": "{COMPANY_ADDRESS}"
                },
                "cardholder_name": "{CREDIT_CARD_HOLDER_NAME}",
                "cvv": "{CREDIT_CARD_SECURITY_CODE}",
                "expiration_date": "{CREDIT_CARD_EXPIRATION}",
                "make_default": true,
                "number": "{CREDIT_CARD_NUMBER}"
            },
            "email": "{ADMIN_EMAIL}",
            "first_name": "{ADMIN_FIRST_NAME}",
            "last_name": "{ADMIN_LAST_NAME}"
        },
        "extensions": [
            {
                "callflow": {
                    "numbers": [
                        "{ADMIN_DID}",
                        "{ADMIN_EXTENSION_NUMBER}"
                    ]
                },
                "user": {
                    "apps": {
                        "accounts": {
                            "api_url": "http://{SERVER}:8000/v1",
                            "icon": "account",
                            "label": "Accounts"
                        },
                        "numbers": {
                            "api_url": "http://{SERVER}:8000/v1",
                            "icon": "menu",
                            "label": "Number Manager"
                        },
                        "voip": {
                            "api_url": "http://{SERVER}:8000/v1",
                            "icon": "phone",
                            "label": "Hosted PBX"
                        }
                    },
                    "credentials": "a9e4685a973f0ed2844ee9f36e211736",
                    "email": "{ADMIN_EMAIL}",
                    "first_name": "{ADMIN_FIRST_NAME}",
                    "last_name": "{ADMIN_LAST_NAME}",
                    "priv_level": "admin"
                }
            },
            {
                "callflow": {
                    "numbers": [
                        "{USER1_EXTENSION_NUMBER}"
                    ]
                },
                "user": {
                    "first_name": "{USER1_FIRST_NAME}",
                    "last_name": "{USER1_LAST_NAME}",
                    "priv_level": "user"
                }
            },
            {
                "callflow": {
                    "numbers": [
                        "{USER2_EXTENSION_NUMBER}"
                    ]
                },
                "user": {
                    "first_name": "{USER2_FIRST_NAME}",
                    "last_name": "{USER2_LAST_NAME}",
                    "priv_level": "user"
                }
            },
            {
                "callflow": {
                    "numbers": [
                        "{USER3_EXTENSION_NUMBER}"
                    ]
                },
                "user": {
                    "first_name": "{USER3_FIRST_NAME}",
                    "last_name": "{USER4_LAST_NAME}",
                    "priv_level": "user"
                }
            },
            {
                "callflow": {
                    "numbers": [
                        "{USER4_EXTENSION_NUMBER}"
                    ]
                },
                "user": {
                    "first_name": "{USER4_FIRST_NAME}",
                    "last_name": "{USER4_LAST_NAME}",
                    "priv_level": "user"
                }
            }
        ],
        "invite_code": "9351b14aa94b9d580dea57b8deefff0c",
        "phone_numbers": {
            "{ADMIN_EXTENSION_NUMBER}": {
                "e911": {
                    "extended_address": "{ADMIN_EXTENDED_ADDRESS}",
                    "locality": "{ADMIN_CITY}",
                    "postal_code": "{ADMIN_ZIP_CODE}",
                    "region": "{ADMIN_STATE}",
                    "street_address": "{ADMIN_ADDRESS}"
                }
            }
        }
    },
    "verb": "PUT"
}
```
