/*
Section: Crossbar
Title: Phone Numbers
Language: en-US
Version: 3.18
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

        {"data": {
             "numbers": [
                 "+14159383408",
                 "+14156715576"
             ]
         }
        }

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

### _GET_ Classifier for a number

    curl -X GET -H "Content-Type: application/json" http://crossbar:8000/v2/phone_numbers/classifiers/4158867900
    {"auth_token": "{AUTH_TOKEN}"
     ,"data": {
         "e164": "+14158867900"
         ,"friendly_name": "US DID"
         ,"name": "did_us"
         ,"number": "4158867900"
         ,"pretty_print": "SS(###) ### - ####"
         ,"regex": "^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$"
     }
     ,"request_id": {REQUEST_ID}
     ,"revision": "undefined"
     ,"status": "success"
    }


## Fix Phone Numbers


### Request

- Verb: `POST`
- Url: `v2/accounts/ACCOUNT_ID/phone_numbers/fix`
- Payload: None

### Response

```
{
    "data": {}
    "status": "success"
}
```