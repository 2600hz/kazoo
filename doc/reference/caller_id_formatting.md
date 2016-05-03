/*
Section: Kazoo
Title: Caller ID Formatting
*/

# Caller ID Formatting

Formatting the Caller ID value that is sent for calls can be done in two ways, either via the account / device configuration documents, or on a trunkstore server.  For either of  these methods the configuration and behavior of the formatting is the same.

An example configuration:
```
"format": {
    "did_us": {
        "regex": ".*97(.*)",
        "prefix": "111"
    }
}
```
All configuration is done based on classifiers, here we are defining the formatting that is to be applied to the 'did_us' class of numbers.

'regex' specifies a regex that is run against the Caller ID number, this is the first step of the formatting.  If the regex matches it will take the first group returned (in the regex above that will strip off everything from the front of the Caller ID up to and including the digits '97'.

More information on the exact regex syntax can be found at: http://www.erlang.org/doc/man/re.html#regexp_syntax

Once the regex has been matched and returns a portion (or possibly the entire number) 'prefix' will be appended, and optionally 'suffix' will be prepended.

## Account / Device configuration

The account and device level configuration is done on the account and device document at the root level.  The key should be called 'caller_id_options'.

Example (device document):
```
  "caller_id_options": {
       "format": {
           "did_us": {
               "regex": ".*97(.*)",
               "prefix": "111"
           }
       }
   }
```

## Trunkstore configuration

The trunkstore formatting is configured on the trunkstore document under servers -> options with the key also called 'caller_id_options'.

Example (trunkstore document):
```
   "servers": [
       {
           "DIDs": {
               "+11112223333": {
               }
           },
           "options": {
               "enabled": true,
               "inbound_format": "e164",
               "international": false,
               "caller_id": {
               },
               "caller_id_options": {
                   "format": {
                       "did_us": {
                           "regex": ".*97(.*)",
                           "prefix": "111"
                       }
                   }
               },
               "e911_info": {
               },
               "failover": {
               },
               "media_handling": "bypass"
           }
    }
```
