/*
Section: Crossbar
Title: White-Labeling
Language: en-US
Version: 3.21
*/

## Domains

When you white label Kazoo's services, DNS settings are needed to make sure your hostname maps appropriate for the various DNS entries (CNAME, A, NAPTR, etc). If the system admin has configured their settings on the backend, you can query Crossbar to show you what your settings should map to.

    curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    'http://{CROSSBAR_URL}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/domains'

Assuming your whitelabel domain is "mydomain.com" you should receive a payload similar to:

    {"auth_token": "{AUTH_TOKEN}",
     "data": {
         "A": {
             "us-central.mydomain.com": {
                 "mapping": [
                     "166.78.105.67"
                 ],
                 "name": "Secondary Proxy",
                 "zone": "us-central"
             },
             "us-east.mydomain.com": {
                 "mapping": [
                     "8.36.70.3"
                 ],
                 "name": "Primary Proxy",
                 "zone": "us-east"
             },
             "us-west.mydomain.com": {
                 "mapping": [
                     "8.30.173.3"
                 ],
                 "name": "Tertiary Proxy",
                 "zone": "us-west"
             }
         },
         "CNAM": {
             "api.mydomain.com": {
                 "mapping": [
                     "api.zswitch.net"
                 ],
                 "name": "API"
             },
             "portal.mydomain.com": {
                 "mapping": [
                     "ui.zswitch.net"
                 ],
                 "name": "Web GUI"
             }
         },
         "MX": {},
         "NAPTR": {
             "proxy-central.mydomain.com": {
                 "mapping": [
                     "10 100 \"S\" \"SIP+D2U\" \"\" _sip._udp.proxy-central.mydomain.com."
                 ],
                 "name": "Central NAPTR"
             },
             "proxy-east.mydomain.com": {
                 "mapping": [
                     "10 100 \"S\" \"SIP+D2U\" \"\" _sip._udp.proxy-east.mydomain.com."
                 ],
                 "name": "East NAPTR"
             },
             "proxy-west.mydomain.com": {
                 "mapping": [
                     "10 100 \"S\" \"SIP+D2U\" \"\" _sip._udp.proxy-west.mydomain.com."
                 ],
                 "name": "West NAPTR"
             }
         },
         "SRV": {
             "_sip._udp.proxy-east.mydomain.com": {
                 "mapping": [
                     "10 10 7000 us-east.mydomain.com.",
                     "15 15 7000 us-central.mydomain.com.",
                     "20 20 7000 us-west.mydomain.com."
                 ],
                 "name": "East SRV"
             }
         },
         "TXT": {}
     },
     "request_id": "{REQUEST_ID}",
     "revision": "{REVISION}",
     "status": "success"
    }

Here you can see which DNS records are supported and where they should point to access the Kazoo cluster.
