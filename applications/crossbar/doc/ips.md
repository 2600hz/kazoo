/*
Section: Crossbar
Title: IPs
Language: en-US
*/

The IPs API allows users to list the address assigned to their accounts.  In the future they will be able to fully manage dedicated IPs.

## Schema

* `ip`: The IP address
* `zone`: The zone the IP currently routes to, this will be an arbitrary value

## Sample curl Requests

### List Assigned IPs

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/ips/assigned
