

The Registrations API provides an easy way to see and manage current registrations.

#### Crossbar Requests

##### _GET_ - Fetch account registrations

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" 'http://server:8000/v2/accounts/{ACCOUNT_ID}/registrations'
    {"auth_token": "{AUTH_TOKEN}",
     "data": [
         {"account_name": "{ACCOUNT_NAME}",
          "account_realm": "{ACCOUNT_REALM}",
          "authorizing_id": "{DEVICE_ID}",
          "authorizing_type": "device",
          "call_id": "792957271@10.26.0.158",
          "contact": "sip:{SIP_USERNAME}@{IP.AD.DR.ESS}:{PORT}...",
          "contact_ip": "{IP.AD.DR.ESS}",
          "contact_port": "{PORT}",
          "event_timestamp": 63581321366,
          "expires": 257,
          "from_host": "{ACCOUNT_REALM}",
          "from_user": "{SIP_USERNAME}",
          "network_ip": "undefined",
          "network_port": "undefined",
          "original_contact": "sip:{SIP_USERNAME}@{IP.AD.DR.ESS}:{PORT}...",
          "owner_id": "{USER_ID}",
          "proxy_ip": "{KAMAILIO_IP}",
          "proxy_port": "{KAMAILIO_PORT}",
          "realm": "{ACCOUNT_REALM}",
          "suppress_unregister_notify": true,
          "to_host": "{ACCOUNT_REALM}",
          "to_user": "{SIP_USERNAME}",
          "user_agent": "Yealink SIP-T38G 38.0.0.115",
          "username": "{SIP_USERNAME}"
          }
          ,...
      ],
      "request_id": "{REQUEST_ID}",
      "revision": "undefined",
      "status": "success"
     }

##### _GET_ - Fetch account registration count

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" 'http://server:8000/v2/accounts/{ACCOUNT_ID}/registrations/count'
    {"auth_token": "{AUTH_TOKEN}",
     "data": {
         "count": 4
     },
     "request_id": "{REQUEST_ID}",
     "revision": "undefined",
     "status": "success"
    }

##### _DELETE_ - Flush all registrations

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" 'http://server:8000/v2/accounts/{ACCOUNT_ID}/registrations/'
    {"auth_token": "{AUTH_TOKEN}",
     "data": "ok",
     "request_id": "{REQUEST_ID}",
     "revision": "undefined",
     "status": "success"
    }

##### _DELETE_ - Flush a specific device's registration

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" 'http://server:8000/v2/accounts/{ACCOUNT_ID}/registrations/{SIP_USERNAME}'
    {"auth_token": "{AUTH_TOKEN}",
     "data": "ok",
     "request_id": "{REQUEST_ID}",
     "revision": "undefined",
     "status": "success"
    }
