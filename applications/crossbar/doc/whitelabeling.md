# Whitelabeling

Whitelabling is one of the core functionality of the Kazoo which allows to make your own brand.

#### Schema

Whitelabel settings



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`company_name` | The company name to display to users | `string()` |   | `false` | `supported`
`domain` | This is the whitelabeled domain that users will be entering to reach the UI | `string()` |   | `false` | `supported`
`fake_api_url` | This is a whitelabeled API URL, primarily used by the developer application | `string()` |   | `false` | `beta`
`hide_credits` | When checked this hides the credits | `boolean()` | `false` | `false` | `beta`
`hide_powered` | When checked this hides the powered by 2600Hz on the bottom right | `boolean()` | `false` | `false` | `supported`
`hide_registration` | When checked this hides the ability to register for a new account | `boolean()` | `false` | `false` | `beta`
`inbound_trunks_price` | The price to show for inbound trunks, this is currently only for display purposes | `string()` |   | `false` | `beta`
`nav.help` | The URL to use when the help link is clicked | `string()` |   | `false` | `supported`
`nav.learn_more` | The URL to use when the 'Learn More!' link is clicked | `string()` |   | `false` | `supported`
`nav` | Properties related to navigation in the UI | `object()` |   | `false` |  
`outbound_trunks_price` | The price to show for outbound trunks, this is currently only for display purposes | `string()` |   | `false` | `beta`
`port.authority` | The email(s) to be used for admin port requests | `string() | array(string())` |   | `false` | `supported`
`port.features` | The URL to use when the features link is clicked | `string()` |   | `false` | `supported`
`port.loa` | The URL to use when the LOA link is clicked | `string()` |   | `false` | `supported`
`port.resporg` | The URL to use when the resporg link is clicked | `string()` |   | `false` | `supported`
`port.support_email` | The support email address to display to the user | `string()` |   | `false` | `supported`
`port.terms` | The URL to use when the terms and conditions link is clicked | `string()` |   | `false` | `supported`
`port` | Parameters related to white-labeling port requests | `object()` |   | `false` |  
`twoway_trunks_price` | The price to show for twoway trunks, this is currently only for display purposes | `string()` |   | `false` | `beta`



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/whitelabel/domains

When you white label Kazoo's services, DNS settings are needed to make sure your hostname maps appropriate for the various DNS entries (CNAM, A, NAPTR, etc). If the system admin has configured their settings on the backend, you can query Crossbar to show you what your settings should map to.

You have two options on the request for what domain to use:

1. If you've already configured your whitelabel domain for the account, the API will use that value.
2. If you specify `domain=some.realm.com` on the request, `some.realm.com` will be used instead.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/domains?domain=some.realm.com
```

OR

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"domain": "some.realm.com"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/domains
```

Assuming your whitelabel domain is "mydomain.com" you should receive a payload similar to:

```json
{
    "auth_token": "{AUTH_TOKEN}",
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
```

Here you can see which DNS records are supported and where they should point to access the Kazoo cluster.

## Testing your domains

> POST /v2/accounts/{ACCOUNT_ID}/whitelabel/domains

Kazoo will attempt to validate your whitelabel settings if you send it a POST to do so:

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/domains
```

Similar to the GET, you can include a `domain=` parameter in the request to test your domains before you create the whitelabel document. A sample response is below:

```json
{
    "auth_token": "{AUTH_TOKEN}",
     "data": {
         "A": {
             "us-central.r1.244.com": {
                 "actual": [
                     "{IP_ADDRESS}"
                 ],
                 "expected": [
                     "166.78.105.67"
                 ]
             },
             "us-east.r1.244.com": {
                 "actual": [
                     "{IP_ADDRESS}"
                 ],
                 "expected": [
                     "8.36.70.3"
                 ]
             },
             "us-west.r1.244.com": {
                 "actual": [
                     "{IP_ADDRESS}"
                 ],
                 "expected": [
                     "8.30.173.3"
                 ]
             }
         },
         "CNAM": {
             "api.r1.244.com": {
                 "actual": [],
                 "expected": [
                     "api.zswitch.net"
                 ]
             },
             "portal.r1.244.com": {
                 "actual": [],
                 "expected": [
                     "ui.zswitch.net"
                 ]
             }
         },
         "MX": {},
         "NAPTR": {
             "proxy-central.r1.244.com": {
                 "actual": [],
                 "expected": [
                     "10 100 \"S\" \"SIP+D2U\" \"\" _sip._udp.proxy-central.r1.244.com."
                 ]
             },
             "proxy-east.r1.244.com": {
                 "actual": [],
                 "expected": [
                     "10 100 \"S\" \"SIP+D2U\" \"\" _sip._udp.proxy-east.r1.244.com."
                 ]
             },
             "proxy-west.r1.244.com": {
                 "actual": [],
                 "expected": [
                     "10 100 \"S\" \"SIP+D2U\" \"\" _sip._udp.proxy-west.r1.244.com."
                 ]
             }
         },
         "SRV": {
             "_sip._udp.proxy-east.r1.244.com": {
                 "actual": [],
                 "expected": [
                     "10 10 7000 us-east.r1.244.com.",
                     "15 15 7000 us-central.r1.244.com.",
                     "20 20 7000 us-west.r1.244.com."
                 ]
             }
         },
         "TXT": {}
     },
     "request_id": "{REQUEST_ID}",
     "revision": "{REVISION}",
     "status": "success"
}
```

You should be able to compare your hosts in each DNS type against the expected values configured by the system admin and adjust your DNS settings as appropriate.

## Configuring the Domains (System Administrators only)

System administrators can set/update the domains object that is used when resellers whitelabel the service. The generic format of the JSON object is:

```json
{
    "{DNS_RECORD_TYPE}":{
        "{WHITELABEL_ABLE_DOMAIN}":{
            "mapping":["{IP_ADDRESS}", "{SRV_RECORD}", "{NAPTR_RECORD}"],
            "name":"Friendly name",
            "zone":"{KAZOO_ZONE}"
        }
     }
    }
```

* `{DNS_RECORD_TYPE}`: In all uppercase, the DNS record type. "CNAM", "A", "SRV", "MX", etc, that you have defined.
* `{WHITELABEL_ABLE_DOMAIN}`: The template for what the hostname will look like when whitelabeled. The only template parameter is `{{domain}}`, which will be replaced by the whitelabel domain of the reseller.
* `mapping`: This is a list of records the reseller should use when configuring their DNS entries for this DNS record type. It could be a list of IP addresses for CNAM or A, or listings of NAPTR/SRV records. Again, the mappings can use the `{{domain}}` placeholder for the whitelabeled domain.
* `{KAZOO_ZONE}`: what zone this host is located in. If using dedicated IPs for the reseller, this will help when building the IP addresses usable by the reseller. Currently, however, this is purely informational.

To set the system domains object, the API is:

> POST /v2/whitelabel/domains

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {DOMAINS_OBJECT}}' \
    http://{SERVER}:8000/v2/whitelabel/domains
```

Where `{DOMAINS_OBJECT}` is the JSON. If you look at the [default domains fixture](https://github.com/2600hz/kazoo/branch/master/core/kazoo_documents/priv/fixtures/domains.json) for a good base JSON object to modify to your needs.

If you receive a 400 when POSTing with a response like:

```json
{
    "auth_token": "{AUTH_TOKEN}",
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
```

You will need to run `sup kapps_maintenance refresh system_schemas` to ensure the `domains` schema is available.
