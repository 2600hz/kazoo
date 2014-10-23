/*
Section: Notify
Title: Notify
Language: en-US
*/

# Notify *Emails notifications*

## Configuration
You must update the smtp_client document in system_config in order to receive emails from Notify. There are a couple ways to do this.

### Sup Commands
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
   "pvt_node": "whistle_apps@fqdn.com"
}

```


After you modify this document to ensure kazoo has the latest config in the cache.
```
sup notify_maintenance reload_smtp_configs
```

## Update templates
```
sup notify_maintenance refresh_template
```
