# Kazoo Caches

The `kz_cache` interface exposes an LRU cache for storing Erlang terms in an ETS table.

This is heavily used to cache document lookups from CouchDB but is generally useful.

## Features of the cache

### Expiring entries

When stored with `{'expires', timeout()}` where `timeout()` is in seconds or `'infinity'`, the cache entry will stay in the cache until after `timeout()` seconds have elapsed.

!!! note
    The cache will check for expired entries, by default, based on the `EXPIRE_PERIOD_MS` macro in [the header](https://github.com/2600hz/kazoo/blob/master/core/kazoo_caches/src/kz_caches.hrl). Caches can be started with alternate expiration timeouts. Lower timeouts mean more work being done to expire caches but entries are evicted closer to the expected timeout; higher timeouts mean less work but an expired cache entry may exist after expiration.

### Self-flushing

The cache has a `gen_listener` process that can subscribe for document-change notices via AMQP (like when a document is successfully saved/deleted from CouchDB). This allows the cache to be programmatically flushed for entries that are no longer valid.

### Callbacks

Cache entries can have associated callbacks for various stages in the cache entry's lifecycle.

Include a callback function when storing a key/value pair:

```erlang
kz_cache:store_local(CacheName, <<"key">>, <<"value">>, [{'callback', fun some_callback/3}]).
```

The callback function will receive three arguments:

```erlang
some_callback(Key, Value, CallbackReason).
```

`CallbackReason` can be one of `'expire'`, `'erase'`, `'flush'`, `'timeout'`, or `'store'`.

#### Timeout

Monitor timer expires. Currently this isn't possible to be reached as the only monitor keys are for the `wait_for_` functions.

#### Expire

Cache entries that have expired

#### Erasure

Cache entries are erased

#### Flush

Cache entries are flushed

#### Store

A cache entry is stored

## Code layout

### `kz_cache`

API module

### `kz_cache_lru`

Enforces the expiration of cache entries

### `kz_cache_ets`

ETS table manipulations

### `kz_cache_callbacks`

Callbacks executed on various events in the cache

### `kz_cache_listener`

AMQP listener for document changes, if configured

### `kz_cache_nodes`

`kz_nodes` listener for new/expiring nodes

### `kz_cache_callbacks`

Callbacks processor module

## ETS architecture

### Main ETS table

Tracks the cache entry, expiration time, etc

### Monitor ETS table

Certain situations where processes can monitor a cache key (`wait_for_*` functions).

### Pointer ETS table

Tracks the AMQP bindings associated with the key for automatic flushing.

## Cache Strategies

See what strategy a node is using:

```shell
sup kazoo_data_maintenance cache_strategy
             strategy: none
  stampede mitigation: false
          async store: false
```

### None

This strategy does not use stampede mitigation and uses a blocking store operation.

```shell
sup kazoo_data_maintenance set_cache_strategy none
```

### Async

This strategy does not use stampede mitigation and uses a non-blocking store operation.

```shell
sup kazoo_data_maintenance set_cache_strategy async
```

### Stampede

This strategy uses stampede mitigation and uses a blocking store operation

```shell
sup kazoo_data_maintenance set_cache_strategy stampede
```

### Stampede Async

When the cache entry is missing, the database operation will proceed. However, when it comes time to cache the value, the key is locked until the store operation is successful. When a competing process tries to cache its value and sees the locked key, it will noop the store and continue on with life.

Essentially, the `winning` process will lock-then-async-store the cache value. All other processes will noop-store and continue, thus avoiding messages to the cache process mailbox.

```shell
sup kazoo_data_maintenance set_cache_strategy stampede_async
```

## Todo

[ ] Move callback processing to alternative module
[ ] Monitor `kz_cache` mailbox during load tests (maybe dbg?)
