

The Channels API allows queries to find active channels for an account, a user, or a device. Given a call-id for a channel, a limited set of commands are allowed to be executed against that channel (such as hangup, transfer, or play media).

#### Crossbar

##### _GET_ - Fetch account channels

    curl -v -X GET -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" 'http://localhost:8000/v2/accounts/{ACCOUNT_ID}/channels'
    {
        "auth_token": "b72c847cbb652606e1d68ed399aff89e",
        "data": [
            {
                "answered": true,
                "authorizing_id": "63fbb9ac78e11f3ccb387928a423798a",
                "authorizing_type": "device",
                "destination": "user_zu0bf7",
                "direction": "outbound",
                "other_leg": "d220c187-e18edc42-bab2459d@10.26.0.91",
                "owner_id": "72855158432d790dfb22d03ff64c033e",
                "presence_id": "user_zu0bf7@account.realm.com",
                "timestamp": 63573977746,
                "username": "user_zu0bf7",
                "uuid": "dab25c76-7479-4ed2-ba92-6b725d68e351"
            }
        ],
        "request_id": "b44ca302b68fe946af314b038f94d1ad",
        "revision": "10-7f2bd6cdb5a776fc60b585ffa22e5950",
        "status": "success"
    }

##### _GET_ - Fetch User/Device channels

* User

        curl -v -X GET -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" 'http://localhost:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/channels'

* Device

        curl -v -X GET -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" 'http://localhost:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/channels'


##### _GET_ - Channel Details

        curl -v -X GET -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" 'http://localhost:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}'

##### _POST_ - Execute an application against a Channel

     curl -v -X POST -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" 'http://localhost:8000/v2/accounts/{ACCOUNT_ID}/channels/{CALL_ID} -d '{"data": {"action": "transfer", "target": "2600", "takeback_dtmf": "*1", "moh": "media_id" }}'

* `action`: What to do to the channel
  * `transfer`: Transfers the `{CALL_ID}` leg to the `target` extension/DID and places the other leg on hold
  * `hangup`: Hangup the `{CALL_ID}` leg
* All other arguments are determined by the `action` value
