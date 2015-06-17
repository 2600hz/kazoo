/*
Section: Crossbar
Title: White-Labeling
Language: en-US
Version: 3.21
*/

## Domains

When you white label Kazoo's services, DNS settings are needed to make sure your hostname maps appropriate for the various DNS entries (CNAM, A, NAPTR, etc). If the system admin has configured their settings on the backend, you can query Crossbar to show you what your settings should map to.

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

### Configuring the Domains

System administrators can set/update the domains object that is used when resellers whitelabel the service. The generic format of the JSON object is:

    {"{DNS_RECORD_TYPE}":{
        "{WHITELABEL_ABLE_DOMAIN}":{
            "mapping":["{IP_ADDRESS}", "{SRV_RECORD}", "{NAPTR_RECORD}"],
            "name":"Friendly name",
            "zone":"{KAZOO_ZONE}"
        }
     }
    }

* `{DNS_RECORD_TYPE}`: In all uppercase, the DNS record type. "CNAM", "A", "SRV", "MX", etc, that you have defined.
* `{WHITELABEL_ABLE_DOMAIN}`: The template for what the hostname will look like when whitelabeled. The only template parameter is `{{domain}}`, which will be replaced by the whitelabel domain of the reseller.
* `mapping`: This is a list of records the reseller should use when configuring their DNS entries for this DNS record type. It could be a list of IP addresses for CNAM or A, or listings of NAPTR/SRV records. Again, the mappings can use the `{{domain}}` placeholder for the whitelabeled domain.
* `{KAZOO_ZONE}`: what zone this host is located in. If using dedicated IPs for the reseller, this will help when building the IP addresses usable by the reseller. Currently, however, this is purely informational.

To set the system domains object, the API is:

    curl -v -X POST \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    'http://{CROSSBAR_URL}:8000/v2/whitelabel/domains' \
    -d '{"data":{DOMAINS_OBJECT}}'

Where `{DOMAINS_OBJECT}` is the JSON. If you look at the [default domains fixture](https://github.com/2600hz/kazoo/branch/master/core/kazoo_documents/priv/fixtures/domains.json) for a good base JSON object to modify to your needs.

If you receive a 400 when POSTing with a response like:

    {"auth_token": "{AUTH_TOKEN}",
     "data": {
         "domains": {
             "required": {
                 "message": "The domains schema is missing, unable to validate request"
             }
         }
     },
     "error": "400",
     "message": "invalid data",
     "request_id": "{REQUEST_ID}",
     "status": "error"
    }

You will need to run `sup whapps_maintenance refresh system_schemas` to ensure the `domains` schema is available.
