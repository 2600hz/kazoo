### Kazoo OAuth Integration

#### Overview

Kazoo can be configured to use third-party resources such as authentication, storage, etc.

#### Kazoo as a OAuth Client

1. Create the oauth consumer in the third-party system (google, Microsoft, Salesforce, ...).
2. Register the consumer in kazoo.
3. Use `kz_auth_client` to obtain a `token` to use the third-party resources

#### Using Google Drive for voicemails

1. Create a new Project in https://console.developers.google.com
   1.1 After you are logged in go to https://console.developers.google.com/
   1.2 Look for `Google Drive API` in the search field and open it
   1.3 Click *Enable* button (in case you don't have it enabled yet)
   1.4 Do the same for `Google+ API` (search for it and enable it)
   1.5 On the left hand sidebar, click *Credentials*
   1.6 Click *OAuth consent screen* tab and fill the form with the corresponding information
   1.7 Click *Credentials* tab
   1.8 In the *Create credentials* drop-down menu select *OAuth client ID*
   1.9 Application type: *Web application*
   1.10 Name it and then click on *create* button
   1.11 Take note of the *client ID* and *client secret* values (we will need those later)

2. Register the auth app:
   2.1 example: *sup kazoo_auth_maintenance register_auth_app {kazoo_master_account_id} {gdrive_client_ID} {gdrive_client_secret} google*

3. Edit index.html in the example dir
   3.1 change YOUR_CLIENT_ID and WEB_URL
   3.2 navigate to WEB_URL
   3.3 click `authorize`
   3.4 gather `code : xxxx`
   3.5 call kazoo API /v2/auth/callback (PUT)
   ```json
   {
     "data": {
       "client_id" : {CLIENT_ID},
       "redirect_uri" : {WEB_URL},
       "code" : {CODE},
       "provider" : "google"
     }
   }
   ```

   3.6 grab the `token` returned and use the page to decode it
   3.7 grab `auth_id`
   3.8 use the `auth_id`to configure your data_plan
   ```
   $ cat data_plan.json
   {"data":
     {
       "attachments": {
           "5fa1f134417927ae200fb12879acec71": {
               "handler": "google_drive",
               "name": "mydrive",
               "settings": {
                   "oauth_doc_id": {AUTH_ID}
               }
           }
       },
       "id": "{KAZOO_MASTER_ACCOUNT_ID}",
       "plan": {
           "modb": {
               "types": {
                   "mailbox_message": {
                       "attachments": {
                           "handler": "5fa1f134417927ae200fb12879acec71"
                           "params": {
                               "folder_name": "kazoo"
                           },
                           "stub": true
                       }
                   }
               }
           }
       }
     }
   }

   $ curl -X PUT Kazoo API /v2/accounts/{account_id}/storage \
       -H "Content-Type: application/json"
       -H "X-Auth-Token: {YOUR_KAZOO_TOKEN}"
       -d @data_plan.json
   ```

   3.9 leave a voicemail
