## BLF on mailbox via \*98{mailbox number}

You can enable this feature system-wide (for all the accounts):
```bash
sup kapps_config set_default callflow dialog_subscribed_mwi_prefix *98
```

Or per account:
```bash
# TODO: test this command
sup kapps_account_config set $AccountID callflow dialog_subscribed_mwi_prefix *98
```
