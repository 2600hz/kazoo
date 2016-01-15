
### Generating an auth token from credentials

Using your username and password, along with an account identifier, will instruct Crossbar to create an authentication token to be used on subsequent requests requiring authentication.

#### The Authentication Process

Easy as 1, 2, 3:

1. First, select the hashing method and create your credentials hash:
    * MD5: `echo -n "{USERNAME}:{PASSWORD}" | md5sum`
    * SHA1: `echo -n "{USERNAME}:{PASSWORD}" | sha1sum`
2. Select an account identifier (any one of the three will suffice):
    * Account Name ("account_name")
    * SIP Realm ("realm")
    * A Phone Number assigned to the account ("phone_number")
3. Send an HTTP PUT:

        curl -v -X PUT -H "content-type:application/json" http://{SERVER}:8000/v2/user_auth -d '{"data":{"credentials":"{CREDENTIALS_HASH}", "account_name":"{ACCOUNT_NAME"}, "method":{MD5_OR_SHA1}}'
        {"auth_token": "{AUTH_TOKEN}"
         ,"data": {
           "account_id": "{ACCOUNT_ID}"
           ,"apps": []
           ,"is_reseller": true
           ,"language": "en-US"
           ,"owner_id": "{OWNER_ID}"
           ,"reseller_id": "{RESELLER_ID}"
         }
         ,"request_id": "{REQUEST_ID}
         ,"revision": "automatic"
         ,"status": "success"
        }

##### The Response

* {AUTH_TOKEN}: this is your authentication token to include in future requests
* {ACCOUNT_ID}: your account's ID, useful for constructing URIs
* {OWNER_ID}: The user's ID of the owner of the credentials used to generate this token
* {RESELLER_ID}: The account's reseller account ID, if any
* {REQUEST_ID}: Useful for debugging requests on your installation

### Password Recovery

Sometimes it is necessary to recover a password.

    curl -v -X PUT -H "content-type: application/json" 'http://{SERVER}:8000/v2/user_auth' -d '{"data":{"username":"API_USERNAME", "account_realm":"ACCOUNT_REALM"}}'

Similar to user authentication, you can supply the account realm, the account name, or a phone number associated with the account to send a password reset to the user's email.

### Getting token auth info

#### Request

- Verb: `GET`
- Url: `accounts/{ACCOUNT_ID}/user_auth/{AUTH_TOKEN}`
- Payload: None

#### Response

```
{
    "data": {
        "account_id": "{ACCOUNT_ID}",
        "owner_id": "{USER_ID}",
        "method": "cb_user_auth",
        "id": "{AUTH_TOKEN}",
        "reseller_id": "{RESELLER_ID}",
        "is_reseller": false,
        "account_name": "{ACCOUNT_NAME}",
        "language": "en-us",
        "apps": [{
            "id": "8bda62bf7ccf8f8acc219d5d2c515376",
            "name": "accounts",
            "api_url": "http://192.168.0.2:8000/v2/",
            "label": "Accounts Manager"
        }, {
            "id": "99d5f033f0a4176640f9bf1c4e81abed",
            "name": "numbers",
            "api_url": "http://192.168.0.2:8000/v2/",
            "label": "Number Manager"
        }, {
            "id": "0306d5162bad2c7a951b6842483f73cd",
            "name": "voip",
            "api_url": "http://192.168.0.2:8000/v2/",
            "label": "Smart PBX"
        }]
    },
    "auth_token": "{AUTH_TOKEN}",
    "status": "success"
}
```
