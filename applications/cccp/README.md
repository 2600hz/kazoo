cccp
====
Calling Card Callback Platform

Could be good for road warriors:

- give a call to your customer as if you are calling from your office, i.e. hide your private phone number behind your companie's CID;

- relay long distance calls expences to your company instead of your own using callback;



HOW-TO

1. cd /opt/kazoo/applications/
2. git clone https://github.com/onnet/cccp.git
3. cd cccp
4. sh priv/copy_to_outside.sh
5. cd /opt/kazoo
6. make
7. sup whapps_maintenance migrate

8. reboot or try to mess with this:
    - sup whistle_maintenance hotload cb_cccps;
    - sup crossbar_maintenance start_module cb_cccps;
    - /opt/kazoo/scripts/conn-to-apps.sh
      - whistle_apps@host 1> {ok, Path} = file:get_cwd().
      - whistle_apps@host 2> code:add_patha(filename:join([Path, "../applications/cccp/ebin"])).
      - whistle_apps@host 3> whapps_controller:start_app(cccp).

9. Edit system_config db's cccp doc:
    - "cccp_cb_number": "7123456789" - callback number
    - "cccp_cc_number": "7098765432" - calling card dial-in number

12. Add PIN for pin auth:
    - curl -v -X PUT -H "X-Auth-Token: f8a68a3asdf3b6d9cd33se35ac634aae" https://your_kazoo_url:8443/v1/accounts/33caq229e4d85ew3423eb39e4ffe1452/cccps -d '{"data":{"pin": "0192837465", "outbound_cid": "+0987654321", "active": true}}'

13. Add CID for cid auth:
    - curl -v -X PUT -H "X-Auth-Token: f8a68a3asdf3b6d9cd33se35ac634aae" https://your_kazoo_url:8443/v1/accounts/33caq229e4d85ew3423eb39e4ffe1452/cccps -d '{"data":{"cid": "1234567890", "outbound_cid": "+0987654321", "active": true}}'


An example of CID/PIN administration:

![Alt text](https://github.com/onnet/onnet/blob/84fe1d1941ff5d00b823800ae35c5b46f1c3bbe3/lib/images/callbackadmin.png "Telephony call recordings")
