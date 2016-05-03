/*
Section: SMTP
Title: SMTP Integration
Language: en-US
*/

# SMTP Integration
kazoo will generate an smtp domain address for each faxbox on its creation.
the smtp domain address will be in the form XXXX.[default_smtp_domain]
[default_smtp_domain] is taken from fax document in system_config or reseller accountdb. to change it issue

* `sup kapps_config set fax default_smtp_domain your.domain.tld`
* `sup kapps_account_config [AccountId] set fax default_smtp_domain your.domain.tld`

# how it works
incoming emails in the form 15557770001@xxxx.fax.kazoo.io will use the attachment (pdf / tiff)
and the properties from the faxbox to send the fax to 15557770001.
the process will notify the sender as long with the configured address in the notifications object in faxbox doc.
