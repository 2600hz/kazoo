/*
Section: Kazoo Number Manager
Title: Carriers
Language: en-US
Version: 4.0
*/

# Overview

The `knm_carriers` module provides the interface to the enabled carrier modules, allowing upper-level applications a single interface to interact with carriers. The primary API concerns searching for, acquiring, and disconnecting numbers from the underlying carrier modules, as well as standardizing the results (including errors) across all carrier modules.

## Behaviour

The `knm_gen_carrier` module provides an Erlang behaviour which all carrier modules must implement.

There are five callbacks:

1. `find_numbers/3`
2. `acquire_number/1`
3. `disconnect_number/1`
4. `should_lookup_cnam/0`
5. `is_number_billable/1`

### `find_numbers/3`

The arguments are:

1. A prefix: used to search the carrier's stock of numbers.
2. The quantity: how many numbers matching that prefix to search for
3. Options list: various flags the carrier module can use to refine searching

The result will be a list of `#knm_number{}` records wrapped in an 'ok' tuple or an error tuple.

### `acquire_number/1`

The argument is the `#knm_number{}` record representing the number desired.

The result is the modified `#knm_number{}` record or an exception is thrown on error.

### `disconnect_number/1`

The argument is the `#knm_number{}` record representing the number to be disconnected.

The result is the modified `#knm_number{}` record or an exception is thrown on error.

### `should_lookup_cnam/0`

Returns a boolean for whether this carrier's numbers should be queried for CNAM.

### `is_number_billable/1`

The argument is the `#knm_number{}` record.

The result is a boolean, representing whether the number should be billed.
