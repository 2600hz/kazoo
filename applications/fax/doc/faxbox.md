## Faxbox

The faxbox provides all the functionality of a virtual fax machine. A `faxbox` is a configurable entity in kazoo which can be used to send and receive faxes. For more information about how to configure a faxbox see the [crossbar faxbox documentation](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/doc/faxboxes.md).

## Receiving Faxes

The `faxbox`, like `receive_fax` can be [attached to a call flow](https://github.com/2600hz/kazoo/blob/master/applications/callflow/doc/faxbox.md) and used to receive faxes, though unlike `receive_fax` which requires a one to one relationship between the `receive_fax` call flow element and an email address, faxbox permits configuration of inbound recipient lists which will all receive a copy of the faxed document.

## Sending Faxes

Two primary methods are provided by the faxbox feature for sending faxes, `api fax` and `email to fax`. In either case, A fax compatible version of the document is generated from the document sent as part of the request. When the request is received, the requested fax document is processed, a document is created in the `faxes` database, and the document is set to a `pending` status putting it into queue per account by `fax_monitor`, by spawning an instance of `fax_jobs` using the unique name `fax_outbound_{account_id}`.

The fax jobs module provides a queuing and rate limiting mechanism per account, defaulting to `max_outbound` at 10 active jobs per account. For the permitted number of jobs in queue, `fax_jobs` spawns an instance of `fax_worker` under the global name `fax_outbound_{destination phone number}` or if `serialize_outbound_number` is disabled `fax_outbound_{random_uuid}`. By default only one fax job per outbound number can be executed system wide, if `serialize_outbound_number` is `false`, only the per-account limitation is used to serialize jobs and multiple outbound faxes to the same number can be initiated.

Fax worker fetches a fax compatible file from the queued fax job, the file is then written to the configured `file_cache_path`, this file is served via the `fax_file_proxy` module which provides an HTTP file server accessible from the FreeSWITCH servers in the cluster, and transmitted using a FreeSWITCH originate to the destination phone number. Upon receiving the originate request, the FreeSWITCH fetches the fax document from the `fax_file_proxy` and attempts to transmit it using the T.30 or T.38 protocol.

If fax transmission fails, the transmission will be re-attempted until the configured number of `retries` on the faxbox is exhausted. If all `retries` are exhausted, the fax will be put into state `failed` and an email notification will be sent indicating the cause of the error. If the fax transmission is successful, a receipt will be sent to the user and/or any email addresses specified in the faxbox outbound notification list.

## Kazoo Fax Converter And Attachment Storage Configuration

If a document is sent to a faxbox in a format unsuitable for faxing (word or OpenXML document, pdf or tiff in a non-faxable format), the attachment will be converted to a fax compliant tiff. The conversions by default will occur whenever a fax document is received via the API or fax_smtp server. The parameters `store_fax_tiff`, `store_fax_pdf` and `store_fax_url` are used to dictate if the results of the conversions should be stored in the database, or if conversions should take place on the fly when a file in one of these formats are required. By default kazoo will store the original document, a faxable tiff version of the document, and a pdf generated from the faxable version of the document.

If these settings are NOT enabled, kazoo will fetch the original document either via the original file or if a url document is specified, fetching the document from the url using HTTP. Kazoo will then execute any conversions required for the type of file requested (faxable tiff or pdf) and do those conversions each time the fax is requested. This is much slower, especially for the API, but does reduce the size of the database documents since only the original document or document `url` is stored in the database. If `url` fax documents are provided and `store_fax_url` is set to `false`, every time the file is requested, it will be downloaded from the `url`, converted to the format required and that document will be returned.

## Email to Fax

Kazoo fax application uses an SMTP server, `fax_smtp` to receive the outbound fax requests and initiate a fax transmission using the content attached to the email.

To initiate an email to fax, an email is sent by the user to the faxbox domain using the format: `{destination_phone_number}@faxbox_domain`.

Kazoo will determine which faxbox should be used for transmission based on the sender email address and destination domain. If the sender is authorized to send a fax via the faxbox, a fax will be transmitted to the `destination_phone_number` provided in the user-part of the `to` email address using the `faxbox` domain name specified in the domain-part of the `to` email address.

### DNS and Email

The Kazoo fax SMTP server is found by email servers by looking up the MX record of either the realm assigned to the account, a custom smtp address, or the generated faxbox domain name.

#### Custom SMTP Address

The `custom_smtp_email_address` is a configurable parameter on the faxbox. This is used when the owner of the faxbox wants to register a unique domain name for the smtp server MX record instead of generating an address from defaults.

#### Realm

For simplicity, the accounts realm can also be used for white labeling the fax domain. This can be accomplished by registering a wildcard MX record for `*.yourdomain.com`. When the realm is used, the account is identified.

#### Faxbox SMTP email address

When a faxbox is created, a random faxbox `smtp_email_address` is generated using the `default_smtp_domain` defined in either the global `system_config/fax` document or at the account or parent accounts level, account config `fax` document. This address can be used directly if a wildcard MX record is registered for `*.your_domain.com`. Since the `smtp_email_address` is unique system wide for the faxbox, the faxbox is identified from the `to` header in the email and access to it validated by the `from` address, checking if it is associated with a user on the account's email address or the faxbox's `smtp_permissions_list`. The smtp domain address will be in the form XXXX.[default_smtp_domain].

The `default_smtp_domain` can be changed via sup using the following commands:

Customize the global default_smtp_domain:
* `sup kapps_config set fax default_smtp_domain your.domain.tld`

Customize an accounts default_smtp_domain:
* `sup kapps_account_config [{ACCOUNT_ID}] set fax default_smtp_domain your.domain.tld`

#### How Faxboxes Are Found For Email To Faxbox

To determine the faxbox to use to send the fax, the `fax_smtp` server first looks at the domain part of the `to` email address to see if it is a faxbox `smtp_email_address` or `custom_smtp_email_address`. If a match is found, the `from` email address is validated against the associated the user of the `smtp_permissions_list`.

If no matches are found for the faxbox the domain part is checked against the account `realm`.

If no matches are found for the `realm`, `smtp_email_address` and `custom_smtp_email_address`, the email is rejected with a `554 not found` SMTP error.

If the `realm` matches the domain part, the the `from` header in the email message is checked. If this is associated with a user on the account which is associated with a faxbox, the users fax box is selected. If no users are found associated with the faxbox, the senders email address is checked against the addresses configured in the faxboxes `smtp_permissions_list` and this faxbox is selected.

If no matches are found for the `from` email address in either user or `smtp_permissions_list`, the email is rejected with a `554 not allowed` SMTP error.

## Notes about DNS configuration for use in email to fax

### Generated `smtp_email_address` Addresses

This is an example of a simple bind configuration for the faxboxes generated `smtp_email_address` addresses.

Note: this example assumes that `fax.kazoo.io` is the `default_smtp_domain` configured in `v2/system_config/fax`.

```
$ORIGIN kazoo.io.
	MX	0 mail-01.kazoo.io.
        MX  0 mail-01.kazoo.io.
        TXT "v=spf1 mx ptr -all"
mail-01     A   x.x.x.x
mail-02     A   x.x.x.x
$ORIGIN fax.kazoo.io.
*			MX	5 mail-01.kazoo.io.
*           MX  5 mail-02.kazoo.io.
```

## Custom SMTP Addresses

When a user customizes the SMTP address of the faxbox, the user must
also add a DNS MX record to the domain. For example, if the user
configured the SMTP address to be `myfax.mydomain.tld`, then the user
would have to add the following DNS entry:

```
$ORIGIN mydomain.tld.
myfax       CNAME   kazoo.io.
```

If user has many faxboxes they can also add wildcards for the fax domain.

```
$ORIGIN fax.mydomain.tld.
*           CNAME   kazoo.io.
```

* Important : add SPF policy to DNS to prevent email address spoofing

## Api Fax

I addition to sending faxes via email to fax, the `crossbar` application provides [an API that can be used to deliver faxes using two methods](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/doc/faxes.md), `url` and `multipart`.

### Url

If the `url` method is used, a url to access the document must be provided in the request body using a url that is reachable by kazoo.

### Multipart

If `multipart` method is used, the fax document is attached as part of a multipart body, which also contains a JSON object which provides the from and to numbers for the fax.

## Google Cloud Printer

Also faxes can be transmitted by linking your google developer account to the faxbox using a google cloud printer.

### Features

* Google will render the document and deliver it as pdf to kazoo
* The printer can be shared amongst users with google accounts without them being tied to kazoo
* Can be used by any device with a browser connected to the internet
* Native Drivers for Windows, OSX, Linux

### Setup

To setup the google fax printer feature, you need a Google developer account. Go to the `Google Developer console` and create a new project or use an existing one.
Go to Credentials and create a new Web Application Client ID or use an existing one. After creating the client id, note the following

* CLIENT ID (OAuthId)
* EMAIL ADDRESS (Email)
* CLIENT SECRET (Secret)

You will need a reseller account or your master account id to configure this via kazoo.

Note: you can retrieve your master account with `sup kapps_util get_master_account_id`

Issue the following sup commands replacing with proper values:

* `sup kazoo_oauth_maintenance register_common_providers`
* `sup kazoo_oauth_maintenance register_oauth_app {ACCOUNT_ID} {OAUTH_ID} {EMAIL} {SECRET} google`
* `sup kapps_account_config set {ACCOUNT_ID} fax cloud_oauth_app {OAUTH_ID}`
* `sup kapps_account_config set {ACCOUNT_ID} fax enable_cloud_connector true`

Every new faxbox will register a new google cloud printer waiting to be claimed.
The url for claiming the printer is in the document and is also returned on a faxbox creation.
After it is claimed (a user with a google account goes into the url and claims it) the printer will be put online.
the google user can share the printer with whomever they wish and all the jobs will be assigned to the `account_id`.
