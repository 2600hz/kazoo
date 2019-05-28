# Load Testing Caching strategies

## What the load test does

See [`kzs_loadtest`](https://github.com/2600hz/kazoo/blob/master/core/kazoo_data/src/kzs_loadtest.erl). The general idea is:

1. For each caching strategy:
  0. flush the document cache
  1. select a subset of doc IDs from the `system_schemas` database
  2. `spawn_monitor` `N` workers
      1. Choose a random doc ID and use `{'ok', _} = kz_datamgr:open_cache_doc(<<"system_schemas">>, DocId).`
  3. wait for all workers to finish, collecting their run times (or noting if a worker failed).
  4. print results for the caching strategy.

## TL;DR

The various strategies allowed will have different impacts on the local Erlang node and the database cluster.

The table below shows that for N processes accessing a nonexistent key, there will be:
- 1..N database requests
- 1..N store messages to the cache process
- potential for the calling process to crash

| Strategy       | DB operations | Store messages | Crash |
| --------       | ------------- | -------------- | ----- |
| none           | N             | N              | yes   |
| async          | N             | N              | no    |
| stampede       | 1             | 1              | no    |
| stampede_async | N             | 1              | no    |

Briefly:

- `none`: stampedes on a non-existing cache key will result in database calls for every request until the cache is populated. It will also cause mailbox pressure on the cache process as the `stores` come in from the requests. This can backup / crash the processes requesting the data (due to `store` being a blocking operation).
- `async`: similar to `none`, stampeding processes will result in database calls for every request. However, the processes won't block waiting for the `store` to complete. Mailbox pressure will still exist on the cache process.
- `stampede`: stampedes on a non-existing cache key will result in a single database request. All other processes will block until that request is populated in the cache. A single `store` will be put in the cache's mailbox. However, the `monitor` table will see some activity as each process that blocks on the key writes an entry to the `monitor` table.
- `stampede_async`: stampedes will result in database requests for each process, similar to `none` and `async`. However, the `store` will first try to lock the key; if successful the process will issue the an async `store`. If the process fails to get the lock, it will ignore storing the value and continue.

## Scaled Document ID Results

```erlang
35> kzs_loadtest:start(10).
using 1 document IDs
starting none...
  finished in 9ms
  min/mean/max: 6 < 7 < 9 (78)
  percentiles: 8(50) 9(75) 9(90) 9(95) 9(99)
  calls: fetch_doc: 10 store_local: 10
starting async...
  finished in 8ms
  min/mean/max: 6 < 6 < 8 (68)
  percentiles: 7(50) 7(75) 7(90) 8(95) 8(99)
  calls: fetch_doc: 10 store_local_async: 10
starting stampede...
  finished in 4ms
  min/mean/max: 4 < 4 < 4 (40)
  percentiles: 4(50) 4(75) 4(90) 4(95) 4(99)
  calls: fetch_doc: 1 store_local: 1
starting stampede_async...
  finished in 8ms
  min/mean/max: 6 < 6 < 8 (69)
  percentiles: 7(50) 8(75) 8(90) 8(95) 8(99)
  calls: fetch_doc: 10 store_local_async: 1
ok
36> kzs_loadtest:start(100).
using 1 document IDs
starting none...
  finished in 54ms
  min/mean/max: 11 < 35 < 52 (3528)
  percentiles: 35(50) 47(75) 50(90) 51(95) 52(99)
  calls: fetch_doc: 100 store_local: 100
starting async...
  finished in 51ms
  min/mean/max: 8 < 31 < 49 (3175)
  percentiles: 33(50) 39(75) 45(90) 47(95) 48(99)
  calls: fetch_doc: 100 store_local_async: 100
starting stampede...
  finished in 11ms
  min/mean/max: 6 < 7 < 10 (762)
  percentiles: 8(50) 8(75) 9(90) 9(95) 9(99)
  calls: fetch_doc: 1 store_local: 1
starting stampede_async...
  finished in 54ms
  min/mean/max: 12 < 35 < 52 (3524)
  percentiles: 33(50) 43(75) 50(90) 51(95) 51(99)
  calls: fetch_doc: 100 store_local_async: 1
ok
37> kzs_loadtest:start(1000).
using 3 document IDs
starting none...
  finished in 122ms
  min/mean/max: 0 < 14 < 110 (14473)
  percentiles: 1(50) 2(75) 74(90) 91(95) 108(99)
  calls: fetch_doc: 212 store_local: 212
starting async...
  finished in 265ms
  min/mean/max: 0 < 75 < 250 (75566)
  percentiles: 49(50) 151(75) 214(90) 233(95) 242(99)
  calls: fetch_doc: 555 store_local_async: 555
starting stampede...
  finished in 52ms
  min/mean/max: 0 < 9 < 34 (9905)
  percentiles: 7(50) 19(75) 27(90) 29(95) 32(99)
  calls: fetch_doc: 3 store_local: 3
starting stampede_async...
  finished in 225ms
  min/mean/max: 0 < 36 < 197 (36651)
  percentiles: 11(50) 67(75) 106(90) 119(95) 186(99)
  calls: fetch_doc: 510 store_local_async: 3
ok
38> kzs_loadtest:start(10000).
using 30 document IDs
starting none...
  finished in 838ms
  min/mean/max: 0 < 38 < 517 (389602)
  percentiles: 0(50) 1(75) 14(90) 432(95) 467(99)
  calls: fetch_doc: 963 store_local: 963
starting async...
  finished in 410ms
  min/mean/max: 0 < 1 < 127 (15693)
  percentiles: 0(50) 0(75) 0(90) 1(95) 52(99)
  calls: fetch_doc: 284 store_local_async: 284
starting stampede...
  finished in 419ms
  min/mean/max: 0 < 6 < 119 (61304)
  percentiles: 0(50) 0(75) 26(90) 52(95) 83(99)
  calls: fetch_doc: 30 store_local: 30
starting stampede_async...
  finished in 491ms
  min/mean/max: 0 < 4 < 259 (41007)
  percentiles: 0(50) 0(75) 1(90) 29(95) 93(99)
  calls: fetch_doc: 572 store_local_async: 30
ok
```

## Fixed document ID results

```erlang
39> kzs_loadtest:start(10).
using 3 document IDs
starting none...
  finished in 9ms
  min/mean/max: 5 < 6 < 8 (65)
  percentiles: 7(50) 7(75) 8(90) 8(95) 8(99)
  calls: fetch_doc: 10 store_local: 10
starting async...
  finished in 7ms
  min/mean/max: 5 < 5 < 7 (57)
  percentiles: 6(50) 6(75) 6(90) 7(95) 7(99)
  calls: fetch_doc: 10 store_local_async: 10
starting stampede...
  finished in 5ms
  min/mean/max: 4 < 4 < 5 (41)
  percentiles: 4(50) 4(75) 4(90) 5(95) 5(99)
  calls: fetch_doc: 3 store_local: 3
starting stampede_async...
  finished in 7ms
  min/mean/max: 4 < 5 < 6 (55)
  percentiles: 6(50) 6(75) 6(90) 6(95) 6(99)
  calls: fetch_doc: 10 store_local_async: 3
ok
40> kzs_loadtest:start(100).
using 3 document IDs
starting none...
  finished in 47ms
  min/mean/max: 13 < 31 < 44 (3134)
  percentiles: 29(50) 39(75) 43(90) 43(95) 44(99)
  calls: fetch_doc: 100 store_local: 100
starting async...
  finished in 45ms
  min/mean/max: 9 < 27 < 41 (2713)
  percentiles: 26(50) 36(75) 39(90) 39(95) 40(99)
  calls: fetch_doc: 100 store_local_async: 100
starting stampede...
  finished in 6ms
  min/mean/max: 3 < 4 < 6 (412)
  percentiles: 4(50) 4(75) 5(90) 5(95) 6(99)
  calls: fetch_doc: 3 store_local: 3
starting stampede_async...
  finished in 44ms
  min/mean/max: 10 < 26 < 41 (2672)
  percentiles: 27(50) 35(75) 38(90) 40(95) 40(99)
  calls: fetch_doc: 100 store_local_async: 3
ok
41> kzs_loadtest:start(1000).
using 3 document IDs
starting none...
  finished in 200ms
  min/mean/max: 0 < 51 < 174 (51773)
  percentiles: 3(50) 117(75) 137(90) 149(95) 170(99)
  calls: fetch_doc: 407 store_local: 407
starting async...
  finished in 229ms
  min/mean/max: 0 < 41 < 202 (41632)
  percentiles: 21(50) 70(75) 99(90) 112(95) 199(99)
  calls: fetch_doc: 550 store_local_async: 550
starting stampede...
  finished in 64ms
  min/mean/max: 0 < 16 < 46 (16796)
  percentiles: 15(50) 31(75) 40(90) 42(95) 43(99)
  calls: fetch_doc: 3 store_local: 3
starting stampede_async...
  finished in 209ms
  min/mean/max: 0 < 37 < 190 (37064)
  percentiles: 5(50) 67(75) 112(90) 128(95) 175(99)
  calls: fetch_doc: 448 store_local_async: 3
ok
42> kzs_loadtest:start(10000).
using 3 document IDs
starting none...
  finished in 208ms
  min/mean/max: 0 < 0 < 83 (3674)
  percentiles: 0(50) 0(75) 0(90) 0(95) 3(99)
  calls: fetch_doc: 62 store_local: 62
starting async...
  finished in 576ms
  min/mean/max: 0 < 4 < 243 (41277)
  percentiles: 0(50) 1(75) 1(90) 29(95) 85(99)
  calls: fetch_doc: 590 store_local_async: 590
starting stampede...
  finished in 381ms
  min/mean/max: 0 < 3 < 70 (31947)
  percentiles: 0(50) 0(75) 1(90) 31(95) 58(99)
  calls: fetch_doc: 3 store_local: 3
starting stampede_async...
  finished in 356ms
  min/mean/max: 0 < 1 < 112 (16907)
  percentiles: 0(50) 0(75) 1(90) 2(95) 64(99)
  calls: fetch_doc: 275 store_local_async: 3
ok
```
