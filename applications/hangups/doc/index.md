/*
Section: Hangups
Title: Hangups
Language: en-US
*/

# Hangups
Abnormal hangup cause logging


## Two ways of logging hangups

Since KAZOO-3235, one can receive alerts via:

* Email
* JSON sent as POST to a URL
* Or both

Mind that if POSTing JSON fails for whatever reason, an email will be sent instead.

### Configuration

`hangups` configuration is filed under `system_config`.

Receiving email can be turned on/off by setting `enable_email_alerts` to `true`/`false` (respectively).

The URL through which to receive JSON is ruled by the `subscriber_url` field.
It takes a string representing a valid URL pointing to a valid server.
In case this string is causing issues, `hangups` will log the complaint and the method
picked up will be *email*.

The JSON sent has the following fields:
* "details"
* "hangup_cause"
* "source"
* "destination"
* "direction"
* "realm"
* "account_id"

Example:
```erlang
{ok, _NewConf} = whapps_config:set(<<"hangups">>, <<"enable_email_alerts">>, false).
URL = <<"http://yourserver.com/your/api/receiving/hangups/data">>.
{ok, _} = whapps_config:set(<<"hangups">>, <<"subscriber_url">>, URL).
```

Note that URL needs to be a binary, that is, not a list.
