/*
Section: GCP
Title: Google Cloud Printer
Language: en-US
*/

# Google Cloud Printer
## Features
* No problem rendering. Google will render the document and deliver it as pdf to kazoo
* Printer can be shared among people with google account without being tied to kazoo
* Can be used by any device with a browser connected to the internet
* Native Drivers for Windows, OSX, Linux

## Setup
you need a Google developer account.
go to google developer console and create a new project or use an existing one.
Go to Credentials and create a new Web Application Client ID or use an existing one.
note down the following

* CLIENT ID (OAuthId)
* EMAIL ADDRESS (EMail)
* CLIENT SECRET (Secret)

You will need a reseller account or your master account id.

You can retrieve your master account with `sup whapps_util get_master_account_id`

issue the following sup commands replacing with proper values, do not include the []

* `sup kazoo_oauth_maintenance register_common_providers`
* `sup kazoo_oauth_maintenance register_oauth_app [AccountId] [OAuthId] [EMail] [Secret] google`
* `sup whapps_account_config set [accounId] fax cloud_oauth_app [OAuthId]`
* `sup whapps_account_config set [accountId] fax enable_cloud_connector true`

every new faxbox will register a new google cloud printer waiting to be claimed.
the url for claiming the printer is in the document and is also returned on a faxbox creation.
after claimed (a user with a google account goes into the url and claims it) the printer will be put online.
the google user can share the printer with whom he wishes and all the jobs will be assigned to the accountid.
