Kazoo development scripts
=========================

```sh
# Starts Kazoo and provide Erlang console
scripts/dev/kazoo.sh

# Runs Kazoo sup wrapper
scripts/dev/sup.sh
```

Hot load code
=============
In Kazoo Erlang shell:
```erlang
> lager:set_loglevel(lager_console_backend, info).
> sync:go().
```
