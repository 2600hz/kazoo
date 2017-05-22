/*
Section: Kazoo Number Manager
Title: Kazoo Number Manager
Language: en-US
Version: 4.0
*/

# Overview

The Kazoo number manager aims for the following:

1. Provide a consistent interface to number management
1. Abstract concepts of carriers, providers, converters, etc, to allow a more easily pluggable system
1. Simplify how numbers move from various states and between accounts

## Interface

### Numbers

The main entry point for number management is the `knm_number` module and its underlying `#knm_number{}` record.
From here, numbers can be fetched, created, moved, assigned, and more.

#### Options

The `knm_number` API functions often take a second argument of options.
These are encapsulated in the `knm_number_options` module, including a handy `-type` for better dialyzing.

#### Bulk operations

When you want to perform operations on a list of numbers, `knm_numbers` provides a nice way to do that, versus iterating over the list yourself.

### Phone Numbers

The `knm_phone_number` module (and `#knm_phone_number{}` record) represent the number document stored in the database and control logic for changing fields of the number document, as well as accessors for various fields.
The module/record are not meant to be used outside of the KNM library; instead it is wrapped in the `#knm_number{}` record and accessed via other modules.

### Carriers

The `knm_carriers` module provides the interface to the enabled carrier modules, allowing upper-level applications a single interface to interact with carriers.
The primary API concerns searching for, acquiring, and disconnecting numbers from the underlying carrier modules, as well as standardizing the results (including errors) across all carrier modules.

The `knm_gen_carrier` module is the Erlang behaviour which all carrier modules must implement.

### Providers

The `knm_providers` module provides the interface to feature provider modules.
Just as the `knm_carriers` module abstracts the carrier, `knm_proviers` abstracts the provider modules, allowing providers to be added or removed without needing knowledge of which provider modules are in play.

The `knm_gen_provider` module is the Erlang behaviour which all provider modules must implement.

#### White-listing feature providers

A reseller can edit the list of features available to its sub-accounts by modifying the `allowed_features` account config list in its account DB.
This list defaults to `["cnam", "e911", "failover", "port", "prepend"]`.

Other providers can be specified in the same place (see below).

* `cnam_provider` (string): should be a CNAM provider module (defaults to `"knm_cnam_notifier"`).
* `e911_provider` (string): should be an emergency services provider module (defaults to `"knm_dash_e911"`).

So in order for a reseller's sub-accounts to be able to use Telnyx's E911 feature, one must:

1. Have `"e911"` in the `allowed_features` list, and
1. Set `e911_provider` to `"knm_telnyx_e911"`

Then, these accounts' numbers which have an `"e911"` public field will be querying Telnyx' E911 services.

### Converters

The `knm_converters` module abstracts how numbers are normalized, classified, and reconciled.
The primary implementation is the regex-based system, but there are plans to integrate [libphonenumber](https://github.com/googlei18n/libphonenumber) as another backend.

### Services

Activating features, numbers, calculating charges - anything involving an account's service plan and phone numbers - is processed through `knm_services`.

### Locality

The `knm_locality` is a generic HTTP client for fetching locality data associated with a number.
The module will POST to the configured URL appended with "/locality/metadata', sending a JSON payload of `{"data":["DID1", "DID2"...]}`.

The response is expected to be a JSON payload of `{"status":"success","data":{...}}` where `"status"="success"` indicates the query was successful (any other value is treated as an error) and `"data"` is a JSON object with the locality information of each supplied DID.

### Errors

The `knm_errors` module takes various error returns from the knm modules and converts it to a normalized JSON error response for use by the higher-level code.

### Config

The `knm_config` module provides accessors for various system- and account-config values.
