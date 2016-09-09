
### Generating an auth token from your API token

Use your account's API token to instruct Crossbar to create an authentication token to be used on subsequent requests requiring authentication.

#### The Authentication Process

1. Get your API key for your account:
    * This value can be obtained by users on an account via the accounts api endpoint `api_key`.
    * This value can also be accessed by system administrators directly from the database by using curl to request the account doc from Couch:

            curl -v http://localhost:15984/accounts/{ACCOUNT_ID}
            {...
            "pvt_api_key": "dfdb4869092fcaa210077109e42bdbac255dda8b9fe6eeb962b880bea7f9f372",
            ...
            }


2. Send an HTTP PUT:

        curl -v -X PUT -H "content-type:application/json" http://{SERVER}:8000/v1/api_auth -d '{"data":{"api_key":"{API_KEY}"}'
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
         ,"revision": "{REVISION}"
         ,"status": "success"
        }

##### The Response

* {AUTH_TOKEN}: this is your authentication token to include in future requests
* {ACCOUNT_ID}: your account's ID, useful for constructing URIs
* {OWNER_ID}: The user's ID of the owner of the credentials used to generate this token
* {RESELLER_ID}: The account's reseller account ID, if any
* {REQUEST_ID}: Useful for debugging requests on your installation
