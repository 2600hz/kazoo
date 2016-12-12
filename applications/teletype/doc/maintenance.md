
# Overview

Most operators will be interested in whether emails are sending or not.

## See Receipts

    #> sup teletype_maintenance receipts
      |                                Call or Msg ID | Receipt                                       | To                             | From                           | Time
    1 |              40c4bcd636a9539f2a4dc2673958e378 | Queued as 9EDEB840B11                         | recipient@recipient.tld        | no_reply@kazoo.tld             | 2015-01-16_23-32-48
    2 |              560a2618f1d6def1444b455bc7a50f9f | Queued as E0338840B11                         | recipient@recipient.tld        | no_reply@kazoo.tld             | 2015-01-16_23-32-49
    3 |              19a56c7f364f9e0a49b93a4f1dbc843a | Queued as 75362840B11                         | recipient@recipient.tld        | no_reply@kazoo.tld             | 2015-01-16_23-49-08
    4 |              ca6d835975d41d54411af94ea3ad0d30 | Queued as 34930840B11                         | recipient@recipient.tld        | no_reply@kazoo.tld             | 2015-01-16_23-51-12
    ok

If the relay is a postfix running locally, grep /var/log/mail.log (or equivalent) for the receipt ID.
