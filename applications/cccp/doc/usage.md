####CouchDB system_config doc example:

````
{
   "_id": "cccp",
   "_rev": "22-e4e3d5976b62807677ea5e2692431f60",
   "default": {
       "cccp_cb_number": "78127481093",
       "cccp_cc_number": "78127481000",
       "last_number_redial_code": "*0",
       "ensure_valid_caller_id": true,
       "default_caller_id_number": "00000000000",
       "allowed_callee_regex": "^\\+?\\d{11,}$",
       "callback_delay": 3
   },
   "pvt_account_id": "system_config",
   "pvt_account_db": "system_config",
   "pvt_created": 63582529472,
   "pvt_modified": 63617518172,
   "pvt_type": "config",
   "pvt_node": "kazoo_apps@kz.domain.tld"
}

````

####Add PIN

      curl -X PUT -H X-Auth-Token:{AUTH_TOKEN} https://{SERVER}:8443/v1/accounts/{ACCOUNT_ID}/cccps -d '{"data":{"pin":"150674729083", "outbound_cid":"+78122404700", "user_id":"e6da57c768533ebf0d349845394ccf26", "active":true}}'

####Add CID

      curl -X PUT -H X-Auth-Token:{AUTH_TOKEN} https://{SERVER}:8443/v1/accounts/{ACCOUNT_ID}/cccps -d '{"data":{"cid":"78121234567", "outbound_cid":"+78122404700", "user_id":"e6da57c768533ebf0d349845394ccf26", "active":true}}'

####Delete CID/PIN

      curl -X DELETE -H X-Auth-Token:{AUTH_TOKEN} https://{SERVER}:8443/v1/accounts/{ACCOUNT_ID}/cccps/{DOC_ID}

####Call initiation over API:

      curl -X PUT -H X-Auth-Token:{AUTH_TOKEN} https://{SERVER}:8443/v1/accounts/{ACCOUNT_ID}/cccps/autodial -d '{"data": { "a_leg_number": "1234567", "outbound_cid": "78123634500", "b_leg_number": "5579", "callback_delay": 10}}'
