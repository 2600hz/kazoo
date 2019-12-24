# Pivot

## About Pivot

Execute an HTTP request to a web server about the call, expecting more callflow instructions in the response.

#### Schema

Validator for the Pivot callflow element



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`cdr_url` | Optional URL to send the CDR to at the end of the call | `string()` |   | `false` |  
`debug` | Store debug logs related to processing this Pivot call | `boolean()` | `false` | `false` |  
`method` | What HTTP verb to send the request(s) with | `string('get' | 'post' | 'GET' | 'POST')` | `get` | `false` |  
`req_body_format` | What format should the request body have when using POST | `string('form' | 'json')` | `form` | `false` |  
`req_format` | What format of Pivot will the your server respond with | `string('kazoo' | 'twiml')` | `kazoo` | `false` |  
`req_timeout_ms` | How long, in milliseconds, to wait for a Pivot response from the HTTP server | `integer()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`voice_url` | What URL to request the initial Pivot callflow | `string()` |   | `true` |  






##### TwiML

TwiML support is limited at the moment; KAZOO JSON is highly encouraged.

!!! note
    `cdr_url` is only applicable when using the XML (TwiML) format. When using the kazoo format, control is handed off to the Callflows app, with the Pivot process ending (and nothing waiting for the CDR). Instead, please use [webhooks](./webhooks.md) (specifically the CHANNEL_DESTROY event) to receive CDRs.

### Allowed Hosts

It is advisable to restrict the hostname in the `voice_url` by setting up some blacklists. These will be consulted when validating the URL provided by the client.

By default RFC1918 IPs and `localhost` blocked.

#### Blacklist CIDR

Blacklist network ranges using CIDR notation:

    sup kazoo_web_maintenance blacklist_client_ip aaa.bbb.ccc.ddd/32

The default list is `127.0.0.1/32` and `0.0.0.0/32`. Consider adding RFC1918 addresses, `10.0.0.0/8`, `172.16.0.0/12`, and `192.168.0.0/16`, as well as your cluster's subnets.

#### Blacklist Hostname

Blacklist a hostname:

    sup kazoo_web_maintenance blacklist_client_host {HOSTNAME}

`{HOSTNAME}` will just match the `HOST` portion of a URL. In the following examples, the `HOST` portion will be `client.host`:

    http://client.host
    https://client.host:9876
    http://user@pass:client.host
    https://user@pass:client.host:4356

Currently, there is no attempt to resolve the hostname to an IP address for CIDR checking.
