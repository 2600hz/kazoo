/*
Section: Crossbar
Title: Basic Authentication
Language: en-US
*/

As an alternative for generated Tokens.

## Username
Should be set to `account_id`

## Password
Should be set to md5sum of username:password
 ```
PASSWORD=`echo -n username:password | md5sum | awk '{print $1}'`
```

## Sample cURL Requests

    curl -v -basic -user {ACCOUNT_ID}:$PASSWORD http://server.com:8000/v2/accounts/{ACCOUNT_ID}/devices
