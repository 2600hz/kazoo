/*
Section: Notify
Title: Notify
Language: en-US
*/

# Notify *Emails notifications*

## System_Config
Update smtp_client document

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
### Update templates

    sup notify_maintenance refresh_template
