
### Ubiquiti Single Sign On

#### Crossbar Requests

##### PUT

    curl -v -X PUT -H "Content-Type: application/json" http://crossbar:8000/v2/ubiquiti_auth -d '{"data":{"username":"{USERNAME}", "password":"{PASSWORD}"}}'
    {
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "sso": {
            "accounts": {
                "{UBIQUITI_ACCOUNT_NAME}": "{UBIQUITI_ACCOUNT_ID}",
                "{UBIQUITI_ACCOUNT_NAME}": "{UBIQUITI_ACCOUNT_ID}"
            },
            "auth_token": "{UBIQUITI_AUTH_TOKEN}",
            "curr_privacy_rev": "REV2013-01-18",
            "curr_terms_rev": "REV2013-01-18",
            "email": "{EMAIL_ADDRESS}",
            "fields_missing": [
                "security_question",
                "security_answer"
            ],
            "first_name": "{FIRST_NAME}",
            "is_verified": true,
            "last_name": "{LAST_NAME}",
            "provider": "ubiquiti",
            "time_created": "2014-02-01T22:16:54Z",
            "username": "{USER_NAME}",
            "uuid": "{UBIQUITI_UUID}"
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
    }
