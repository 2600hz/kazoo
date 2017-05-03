# Kazoo development scripts

## Starts Kazoo and provide Erlang console

```shell
./scripts/dev/kazoo.sh
```

## Runs Kazoo sup wrapper

```shell
./scripts/dev/sup.sh
```

## Hot load code

In Kazoo Erlang shell:

```erlang
lager:set_loglevel(lager_console_backend, info).
sync:go().
```
