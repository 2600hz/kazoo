# Overview

The Kazoo number manager aims for the following:

1. Provide a consistent interface to number management
1. Abstract concepts of carriers, providers, converters, etc, to allow a more easily pluggable system
1. Simplify how numbers move from various states and between accounts


## Interface

### Numbers

The main entry point for number management is the `knm_number` module and its underlying `#knm_number{}` record.
From here, numbers can be fetched, created, moved, assigned, and more.

A number document should look something like this:

```json
{
   "_id": "+15556668743",
   "_rev": "6-8bed97b8a5bf5e89528a6d1b04bc0427",
   "pvt_db_name": "numbers%2F%2B1555",
   "pvt_state": "in_service",
   "pvt_ported_in": false,
   "pvt_module_name": "knm_bandwidth2",
   "pvt_modified": 63661152901,
   "pvt_created": 63658650265,
   "pvt_type": "number",
   "prepend": {
       "name": "My prepended name",
       "enabled": true
   },
   "cnam": {
       "inbound_lookup": true,
       "display_name": "My display name"
   },
   "pvt_assigned_to": "009afc511c97b2ae693c6cc4920988e8",
   "pvt_features": {
       "prepend": {
           "name": "My prepended name",
           "enabled": true
       },
       "inbound_cnam": {
           "inbound_lookup": true
       },
       "outbound_cnam": {
           "display_name": "My display name"
       }
   }
}
```

Numbers are stored in prefixed databases: `+15556668743` is stored in `numbers/+1555` with this id.
When a number is assigned to an account, this document is copied from the numbers database into the account's database.
When ownership changes to another account or when the number goes `available`, this account document is deleted.

Note: the number document in the numbers DB is the source of truth, above the one in account DBs.

Note: while a number document should exist in at most one account database, it is possible that a bug left some number documents in the wrong account databases.
Use the Sync button from the owning account, to delete these number documents from the wrong account database.

Run `sup kazoo_numbers_maintenance migrate {ACCOUNT_ID}` or a full migration to make sure these documents are all in sync on `{ACCOUNT_ID}` or throughout your system.
Do note though that this operation can take a long time, depending on the amount of accounts and numbers in your system.

#### Options

The `knm_number` API functions often take a second argument of options.
These are encapsulated in the `knm_number_options` module, including a handy `-type` for better type checking.


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
Just as the `knm_carriers` module abstracts the carrier, `knm_providers` abstracts the provider modules, allowing providers to be added or removed without needing knowledge of which provider modules are in play.

The `knm_gen_provider` module is the Erlang behaviour which all provider modules must implement.


#### White-listing feature providers

Number features (such as `E911`, `CNAM`, `failover`) can be whitelisted and blacklisted.

One can set the lists in this hierarchical order (if set, one takes precedence over the next, number document first):

1. On the number document (`undefined` by default)
1. On the number's reseller account document (`undefined` by default)
1. In `system_config/number_manager`:
    * if `system_config/number_manager/providers` is set it will be used as the default list of allowed features
    * otherwise, the defaults of `"features"."allow"` are used
    * Note: local numbers are the only exception (see note below)

Each of these documents can both have a list of features allowed for use and forbidden from use:

1. On number document: they are the root fields `"pvt_features_allowed"` and `"pvt_features_denied"`
1. On reseller account document: the fields `"features"."allow"` and `"features"."deny"` under the `number_manager` category
1. In `system_config/number_manager`: the fields `"features"."allow"` and `"features"."deny"`

By default, here is what the `system_config/number_manager` document looks like:

```json
       ...
       "features": {
           "allow": [
               "carrier_name",
               "failover",
               "force_outbound",
               "prepend",
               "ringback"
           ]
       },
       ...
```

To make sure your system has these defaults, run [`sup kazoo_numbers_maintenance reset_allowed_features_to_defaults_on_system_config`](./maintenance.md#reset_allowed_features_to_defaults_on_system_config).

On migration, `sup kazoo_numbers_maintenance ensure_adminonly_features_are_reachable` is run. This ensures that admins have access to admin-only features and also keeps the possible manual settings of system_config.

There are more maintenance commands to manage number features for the different documents listed above.

##### On local numbers

Numbers that have `knm_local` (or `knm_mdn`) as a carrier are forbidden to have these `external` features:

* `CNAM` i.e. features named `"cnam"`, `"inbound_cnam"`, `"outbound_cnam"`
* `E911` i.e. the feature named `"e911"`
* `port`: the `"port"` feature only concerns numbers that were ported into the system

Note: if you want to set one or more of these features on numbers local to the system, you can either:

* change their carrier from `knm_local` to `knm_inventory` using the `"rename_carrier"` feature (cleanest)
* set the `system_config/number_manager/local_feature_override` boolean flag to `true` (fastest)


##### On admin-only features

Some features can only be edited or used by admins of the system:

* `"carrier_name"`: allows an admin to change numbers' carrier module name


#### Change feature provider modules

Features such as `E911` and `CNAM` can be provisioned by different providers (Telnyx', Vitelity's, ...).
These provider modules can be configured on the reseller document.

Here are their defaults and possible values:

* `"cnam_provider"`: `"knm_cnam_notifier"` (other possible values: `"knm_telnyx_cnam"`, "`knm_vitelity_cnam`")
* `"e911_provider"`: `"knm_dash_e911"` (other possible values: `"knm_telnyx_e911"`, "`knm_vitelity_e911`")

So in order for a reseller's sub-accounts to be able to use Telnyx's E911 feature, one must:

1. Be allowed to edit the `"e911"` feature
1. Set `"e911_provider"` to `"knm_telnyx_e911"`

Then, these accounts' numbers which have an `"e911"` public field will be querying Telnyx' E911 services.


### Converters

The `knm_converters` module abstracts how numbers are normalized, classified, and reconciled.
The primary implementation is the regex-based system, but there are plans to integrate [libphonenumber](https://github.com/googlei18n/libphonenumber) as another backend.


### Services

Activating features, numbers, calculating charges -- anything involving an account's service plan and phone numbers -- is processed through `knm_services`.


### Locality

The `knm_locality` is a generic HTTP client for fetching locality data associated with a number.
The module will POST to the configured URL appended with "/locality/metadata', sending a JSON payload of `{"data":["DID1", "DID2"...]}`.

The response is expected to be a JSON payload of `{"status":"success","data":{...}}` where `"status"="success"` indicates the query was successful (any other value is treated as an error) and `"data"` is a JSON object with the locality information of each supplied DID.


### Errors

The `knm_errors` module takes various error returns from the knm modules and converts it to a normalized JSON error response for use by the higher-level code.


### Config

The `knm_config` module provides accessors for various system- and account-config values.
