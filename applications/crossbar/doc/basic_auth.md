# Basic Auth

As an alternative for generated Tokens, you can make request with HTTP Basic Auth using your account ID and user name and password.

**Basic Auth Username**

Should be set to `account_id` of the authorizing account.

**Basic Password Password**

Should be set to MD5 hash of your `username:password`

```shell
PASSWORD=`echo -n username:password | md5sum | awk '{print $1}'`
```

## Sample cURL Requests

```shell
curl -v \
    --basic --user {AUTH_ACCOUNT_ID}:$PASSWORD \
    http://server.com:8000/v2/accounts/{ACCOUNT_ID}/devices
```

This is useful for reseller to execute requests against a their sub-account quickly.
