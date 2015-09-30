/*
Section: Crossbar
Title: Devices
Language: en-US
Version: 3.20
*/

# Kazoo Devices
Learn how to use the 2600hz mobile API set to activate and manage phones.


## Static Presence ID

You can set a presence ID at the device level. By adding: `presence_id` field on your device document. This will fix any presence issue regarding inbound/outbound call on a cellphone device.

## QuickCall

Additional info for [QuickCall is on the wiki.](https://2600hz.atlassian.net/wiki/display/APIs/QuickCall+API)

```
http://{your kazoo domain}:8000/v1/accounts/{account_id}/devices/{device_id}/quickcall/{number_to_call}?auth_token={API key or Auth Token}&cid-number={caller ID number}&cid-name={caller ID name}
```

If you are using HTTPs don't forget to update the port as well!
```
https://{your kazoo domain}:8443/v1/accounts/{account_id}/devices/{device_id}/quickcall/{number_to_call}?auth_token={API key or Auth Token}&cid-number={caller ID number}&cid-name={caller ID name}
```

_Both cid-number and cid-name are optional_

Above are all the options of QuickCall (As Far as I can tell) They should be added to the examples.

Also under Resource Parameters add "cid-name" info to the "cid-number" info

esoare

## Sync

Some devices support receiving SIP NOTIFY packets with `event` = `check-sync`. This is typically used to reboot the phone if the configuration has changed. Kazoo will generate the NOTIFY packet if the device is registered.

    curl -v -X POST -H "X-Auth-Token:{AUTH_TOKEN}" http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync


One can also attempt `check-sync` while updating a device (in the same request) setting `"sync": true`:

    curl -v -X POST -H "X-Auth-Token:{AUTH_TOKEN}" http://{SERVER}:8000/v{API version}/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID} -d '
    {
      "data": {
          "sync": true,
          …
      }
    }'

If `"sync"` is set to `false` or is not defined, no `check-sync` will be attempted.

## Presence / BLF

Occassionally a phone's BLF might be out of sync with the true state of the phone. You can explicitly ask Kazoo to reset the presence status via API to resolve this.

    curl -v -X POST -H "X-Auth-Token:{AUTH_TOKEN}" http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/presence

No data payload at this time is required. This will send the appropriate internal command to reset the presence of the device (following `presence_id` if required.

## Load a user's devices

Often you'll want to see what devices belong to a user, or devices that a user has hot-desked into.

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/devices
    {"auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "device_type": "sip_device",
            "enabled": true,
            "hotdesked": false,
            "id": "{DEVICE_ID_1}",
            "mac_address": "",
            "name": "USER_ID_DEVICE",
            "owner_id": "{USER_ID}"
        },
        {
            "device_type": "sip_device",
            "enabled": true,
            "hotdesked": true,
            "id": "{DEVICE_ID_2}",
            "mac_address": "",
            "name": "OWNER_ID_DEVICE",
            "owner_id": "{OWNER_ID}"
        }
      ],
     "request_id": "{REQUEST_ID}",
     "revision": "{REVISION}",
     "status": "success"
    }

Notice that the first device, `{DEVICE_ID_1}` is owned by `{USER_ID}` but the second device, `{DEVICE_ID_2}`, is owned by `{OWNER_ID}` **and** is currently hotdesked to `{USER_ID}` (see the `"hotdesked":true` attribute).
