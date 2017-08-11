### Kazoo OAuth Integration

#### Overview

Kazoo can be configured to use third-party resources such as authentication, storage, etc.

#### Kazoo as a OAuth Client

1. Create the oauth consumer in the third-party system (google, microsoft, salesforce, ...).
2. Register the consumer in kazoo.
3. Use `kz_auth_client` to obtain a `token` to use the third-party resources

#### Using Google Drive for voicemails

1. Create a new Project in https://console.developers.google.com
2. Create Credentials in `API Manager`
    2.1 OAuth client id of type `web server`
    2.2 Add the MONSTER-UI url (protocol+domain) to allowed origin and callback
    2.3 Add the `Google Drive API` to the project
    2.4 Gather `client id`, `client secret`
3. Register the client_id in kazoo
    3.1 `sup kazoo_auth_maintenance register_auth_app {MASTER_ACCOUNT} {CLIENT_ID}  {CLIENT_SECRET} google`
4. Edit index.html in the example dir
    4.1 change YOUR_CLIENT_ID and WEB_URL
    4.2 navigate to WEB_URL
    4.3 click `authorize`
    4.4 gather `code : xxxx`
    4.5 call kazoo api /v2/auth/callback
{
  "data": {
    "client_id" : {CLIENT_ID},
    "redirect_uri" : {WEB_URL},
    "code" : {CODE},
    "provider" : "google"
  }
}

    4.6 grab the `token` returned and use the page to decode it
    4.7 grab `auth_id`
    4.8 use the `auth_id`to configure your data_plan

attachments
"5fa1f134417927ae200fb12879acec71": {
       "handler": "google_drive",
       "name": "mydrive",
       "settings": {
           "oauth_doc_id": {AUTH_ID}
       }
   }

   plan
 "modb": {
       "types": {
           "mailbox_message": {
               "attachments": {
                   "handler": "5fa1f134417927ae200fb12879acec71",
                   "params": {
                       "folder_name": "kazoo"
                   },
                   "stub": true
               }
           }

    4.9 leave a voicemail

