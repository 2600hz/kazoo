#Top Up feature

##Enable top:

1. Create/Update `topup` document in the `system_config` database and set `enable` field to `true`.
2. Then you need to enable top up on a per account basis.

Add a `topup` object to your account document. You can set 2 values `amount` & `threshold`.

```
"topup": {
    "amount": 100,
    "threshold": 10
}
```

## Maintenance

You can enable/disable top up with these sup commands:

* `sup kazoo_transactions_maintenance disable_top_up`
* `sup kazoo_transactions_maintenance enable_top_up`


This will not set top up for any accounts, just for the system (step 1).