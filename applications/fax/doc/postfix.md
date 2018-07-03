
# Postfix role in smtp-to-fax
Although you can expose kazoo fax on port 25 or use haproxy to relay incoming email messages directly to the fax_smtp server, we recommend to use postfix to filter email spam before delivering to haproxy/kazoo

# Simple Postfix setup
## inistall postfix, python & curl
* `yum -y install curl postfix`
* `yum -y install python python-dns python-pydns`
* `yum -y install python-pyspf pypolicyd-spf postgrey`

## edit etc/sysconfig/postgrey with
`OPTIONS="--unix=/var/spool/postfix/postgrey/socket --delay=60"`

## start services
* `service postgrey start`
* `service postfix reload`
* `chkconfig --levels 345 postgrey on`

## edit /etc/postfix/main.cf and add the following lines at the end

```
relay_domains = hash:/etc/postfix/kz_smtp_domains
# relayhost should be the IP:PORT of haproxy-smtp-listener or kazoo fax whapp
relayhost = 127.0.0.1:2525

policy-spf_time_limit = 3600s

smtpd_delay_reject = yes
smtpd_helo_required = yes
smtpd_helo_restrictions =
    permit_mynetworks,
    reject_non_fqdn_helo_hostname,
    reject_invalid_helo_hostname,
    permit

smtpd_sender_restrictions =
    permit_mynetworks,
    reject_non_fqdn_sender,
    reject_unknown_sender_domain,
     check_sender_access regexp:/etc/postfix/kz_allowed_senders,
    reject

smtpd_recipient_restrictions =
   reject_unauth_pipelining,
   reject_non_fqdn_recipient,
   reject_unknown_recipient_domain,
   permit_mynetworks,
   reject_unauth_destination,
   check_policy_service unix:private/policyd-spf,
   check_sender_access regexp:/etc/postfix/kz_allowed_senders,
   reject_rbl_client zen.spamhaus.org,
   reject_rbl_client bl.spamcop.net,
   check_policy_service unix:postgrey/socket,
   reject
```

## edit /etc/postfix/master.cf and add the following line at the end
```
policy-spf  unix  -       n       n       -       0       spawn
   user=nobody argv=/usr/libexec/postfix/policyd-spf
```

## Todo
* use couchdb views to get kazoo faxboxes configuration into postfix
* edit domains and permitteed users from kazoo
* `postmap /etc/postfix/kz_smtp_domains`
* `postmap /etc/postfix/kz_allowed_senders`
* `postfix reload`
* put into a bash script
* add it to a cron table
* handle 304 Not Modified responses
