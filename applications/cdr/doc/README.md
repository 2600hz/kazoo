# CDR - *Call Detail Records*

When calls finish, the CDR app receives the `CHANNEL_DESTROY` event, massages some of the values, then stores the CDR to the account's MODB.
