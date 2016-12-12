
# Notify

Two ways of logging hangups. One can receive alerts via:

* Email
* JSON sent as POST to a URL
* Or both

Mind that if POSTing JSON fails for whatever reason, an email will be sent instead.

## *Email Notifications* Configuration
You must update the smtp_client document in system_config in order to receive emails from Notify. There are a couple ways to do this.

#### Sup Commands
Configure the smtp_client via the command line using sup.
```
sup notify_maintenance configure_smtp_relay my.relay.com
sup notify_maintenance configure_smtp_username username
sup notify_maintenance configure_smtp_password password
sup notify_maintenance configure_smtp_auth always
sup notify_maintenance configure_smtp_port 123
```

### Couch document
Update your the smtp_client document via couch manually. Should look like this.
Example:
```
{
   "_id": "smtp_client",
   "default": {
       "relay": "smtp.sendgrid.net",
       "username": "username",
       "password": "password",
       "auth": "always",
       "port": "587",
   },
   "pvt_account_id": "system_config",
   "pvt_account_db": "system_config",
   "pvt_created": 63581123482,
   "pvt_modified": 63581123482,
   "pvt_type": "config",
   "pvt_node": "kazoo_apps@fqdn.com"
}

```


After you modify this document to ensure kazoo has the latest config in the cache.
```
sup notify_maintenance reload_smtp_configs
```

### Update templates
```
sup notify_maintenance refresh_template
```


## *JSON POSTing* Configuration

Receiving email can be turned on/off by setting `enable_email_alerts` to `true`/`false` (respectively).

The URL through which to receive JSON is ruled by the `subscriber_url` field.
It takes a string representing a valid URL pointing to a valid server.
In case this string is causing issues, `notify` will log the complaint and the method
picked up will be *email*.

The JSON sent has the following fixed fields:

* `Message`: the error message emitted
* `Format`: the format string of Message. You can match against this field.
* `Subject`: subject field if it were an email
* `Details`: error-specific information

### Setting the system up

#### Via Erlang
```erlang
Config = <<"notify.system_alert">>.
{ok, _NewConf} = kapps_config:set_default(Config, <<"enable_email_alerts">>, false).
URL = <<"http://my.alerts.platform.com/hangups.php">>.
{ok, _} = kapps_config:set_default(Config, <<"subscriber_url">>, URL).
```

Note that URL needs to be a binary, that is, not a list.

#### Via SUP

    sup kapps_config set_default notify.system_alert enable_email_alerts false
    sup kapps_config set_default notify.system_alert subscriber_url 'http://my.alerts.platform.com/hangups.php'
