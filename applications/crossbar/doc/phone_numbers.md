/*
Section: APIs
Title: Phone Numbers
Weight: 90
*/

# Phone Numbers
Learn how to use the 2600hz mobile API set to activate and manage numbers.


## Check Phone Numbers availability

This API check if the numbers are still available for purchase.

- IN <- List of numbers
- OUT -> JSON; Key = Number, Value = status

### Request

- Verb: `POST`
- Url: `/accounts/ACCOUNT_ID/phone_numbers/check`
- Payload:
```
{
    "data": {
        "numbers": [
            "+14159383408",
            "+14156715576"
        ]
    }
}
```

### Response
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