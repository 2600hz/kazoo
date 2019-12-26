# Pivot

## Overview

The Pivot gives developers greater control over callflows than what comes natively in Kazoo. Pivot attempts to corral the salient data and send it, via HTTP to the developer's web server. Pivot expects a response with appropriate XML or JSON, and will execute the callflow returned on behalf of the developer.

### Example Callflow

The most basic callflow for Pivot:

```json
{
 "flow":{
     "module":"pivot"
     ,"data":{
         "voice_url":"http://your.pivot.server/path/to/script.php"
         ,"req_format":"kazoo"
         ,"method":"get"
         ,"debug":false
     }
 }
}
```

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

## Response Formats

* [Kazoo JSON](./kazoo/README.md)
* [TwiML](./twiml/README.md)

## Debugging

You can set the `debug` flag to `true` to log the requests and responses Pivot receives from your Pivot Callflows. Those logs are then available via the [Pivot API](../../crossbar/doc/pivot/#debugging-pivot-attempts)
