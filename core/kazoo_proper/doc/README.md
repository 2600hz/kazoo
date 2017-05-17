# Kazoo PropEr

[PropEr](http://proper.softlab.ntua.gr/) quickcheck models for various parts of Kazoo.

## Crossbar

I like to set console level to critical (chatty otherwise): `kazoo_maintenance:console_level(critical).`

After each quickcheck, I like to clean soft-deleted account docs from the `accounts` database: `kt_cleanup:cleanup_soft_deletes(<<"accounts">>).`

### [Phone Numbers](src/pqc_cb_phone_numbers.erl)

Tests the [`phone_numbers`](../../applications/crossbar/doc/phone_numbers.md) API

Sequential tests: `proper:quickcheck(pqc_cb_phone_numbers:correct()).`
Parallel tests: `proper:quickcheck(pqc_cb_phone_numbers:correct_parallel()).`

Cleanup deleted account docs: `kt_cleanup:cleanup_soft_deletes(<<"accounts">>).`
