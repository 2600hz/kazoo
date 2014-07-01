/*
Section: Crossbar
Title: User Authentication
Language: en-US
*/

# Generating an auth token from credentials

Using your username and password, along with an account identifier, will instruct Crossbar to create an authentication token to be used on subsequent requests requiring authentication.

## The Authentication Process

Easy as 1, 2, 3:

1. First, select the hashing method and create your credentials hash:
    * MD5: `echo -n "{USERNAME}:{PASSWORD}" | md5sum`
    * SHA1: `echo -n "{USERNAME}:{PASSWORD}" | sha1sum`
2. Select an account identifier (any one of the three will suffice):
    * Account Name ("account_name")
    * SIP Realm ("realm")
    * A Phone Number assigned to the account ("phone_number")
3. Send an HTTP PUT:

        curl -v -X PUT -H "content-type:application/json" http://{SERVER}:8000/v1/user_auth -d '{"data":{"credentials":"{CREDENTIALS_HASH}", "account_name":"{ACCOUNT_NAME"}, "method":{MD5_OR_SHA1}}'
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

### The Response

* {AUTH_TOKEN}: this is your authentication token to include in future requests
* {ACCOUNT_ID}: your account's ID, useful for constructing URIs
* {OWNER_ID}: The user's ID of the owner of the credentials used to generate this token
* {RESELLER_ID}: The account's reseller account ID, if any
* {REQUEST_ID}: Useful for debugging requests on your installation
