

As an alternative for generated Tokens.

#### Username

Should be set to `account_id` of the authorizing account.

#### Password

Should be set to md5sum of username:password

```shell
PASSWORD=`echo -n username:password | md5sum | awk '{print $1}'`
```

#### Sample cURL Requests

    curl -v -basic -user {AUTH_ACCOUNT_ID}:$PASSWORD http://server.com:8000/v2/accounts/{ACCOUNT_ID}/devices

This will still allow a reseller to execute requests against a sub-account.
