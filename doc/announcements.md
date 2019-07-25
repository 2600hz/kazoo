# Announcements for Kazoo

This file will serve as a reference point for upcoming announcements, both of the temporal nature (this library will be deprecated in 6 months) and version-specific (upgrading from X to Y will require A, B, and C).

## Versions

### 5.0

The 5.0 release will start Kazoo's official support for OTP 21 ([21.3](http://www.erlang.org/news/127) currently being the preferred version).

1. The big change for Erlang code is the deprecation of using `erlang:get_stacktrace()`. There is a target in the root Makefile `make check_stacktrace` that will update uses of `get_stacktrace()` from `try/catch` clauses. Please ensure any private code is adjusted accordingly.

2. Community-supported and deprecated apps will be moved out of Kazoo and into a [kazoo-community](https://github.com/kazoo-community) organization. De-factor maintainers of the community apps have been added to remove 2600Hz from blocking PR and code management for those apps. Tooling will continue to be improved for those apps (and 3rd party apps in general). If you would like to take on a maintainer's role for any of the kazoo-community apps, let us know!

3. Dependencies have been re-evaluated, updated, or removed as necessary. Please check that your use of them is still available. We're thinking on how community/private apps can include unique dependencies within themselves without impacting core Kazoo's dependency list.

### 4.3

1. The Kazoo services have been significantly refactored.  This has resulted in changes to the APIs related to services (prior service_plans), ledgers and transactions as well as the documents in the services database and service plans.  See the documentation in `core/kazoo_services/doc` for more information.

2. `kz_datamgr:ensure_saved` deprecated

When multiple processes are updating a document, `ensure_saved` could result in data loss, particularly if an earlier version deleted a key; the newer version would revert the key's existence. At first we tried to calculate the diff from the current version of the doc against the changed doc but it wasn't possible to tell a deleted key in the current version from an added key in the changed version.

Instead, we've moved to using `kz_datamgr:update_doc` and adjusted it to take an option list instead of the JSON properties to update/create/extra-update. Now, when a conflict occurs on save, we can fetch the current version and re-apply the updates and save again.

3. `kzd_accounts:save/2` moved to `kzd_accounts:update/2`

The old `save/2` took an updater function and tried to save the result. Because we want to be sure the changes are saved both to the account DB and the `accounts` DB, we need to be able to apply the update to the `accounts` DB version independently of the account DB version.

4. New parameters were added to the account, user and device documents to set the asserted identity.  These parameters are currently free-form but will be strictly verified by default in the future!

### 4.2

1.  Erlang Version Support

    Starting with Kazoo 4.2 Erlang support will target 19+ and will not be backward compatible with prior Erlang versions.

2. Time

    In accordance with the new [time correction](http://erlang.org/doc/apps/erts/time_correction.html) work in Erlang 19+, cleanup of [kz_time](https://github.com/2600hz/kazoo/blob/master/core/kazoo_stdlib/src/kz_time.erl) has been done to ensure Kazoo uses the proper time functions.

    The big change (that should be mostly transparent) is that `kz_time:now_s/0` returns Gregorian seconds instead of Unix Epoch seconds. The majority of code either doesn't care or expected Gregorian seconds, so this change should have minimal impact on existing code. If you need a Unix timestamp, `kz_time:current_unix_tstamp/0` is what you want.

3. System Teletype Templates

    Starting with Kazoo 4.2 Teletype templates are using their own Teletype specific Email configuration from system configuration. Previously some properties like `from`, `to`, `cc`, `bcc`, etc... were read from `notify.{TEMPLATE_ID}` documents in `system_config` database to initialize the system templates. This has been changed to read from `notification.{TEMPLATE_ID}` which it's the place actual Teletype templates are saved.

    If you directly made configurations to these documents, you need to re-configure them in the Teletype templates documents.

    > **Note:** This change is not affecting users which are using the Notify application

    > **Note:** This only applied to templates from **system**, not account's specific templates

    > **Note:** Those parameters are the default values, that means if Teletype can't find the value in the notification payload it receives or account's template then it falls back to these system values (if necessary)

4. Crossbar Load View

    In order to reduce pagination problems, increase maintainability and standardizing Crossbar view operations on multiple databases and handling huge number of documents properly, [`crossbar_doc:load_view/3,4,5,6`](https://github.com/2600hz/kazoo/blob/873dc106c7a7330393201207eddc365837c3dbe6/applications/crossbar/src/crossbar_doc.erl#L15) has been deprecated in favor of new module `crossbar_view`. Please migrate your current Crossbar modules or write your new modules to use this new Crossbar view functionality.

    Starting with Kazoo 4.2, helper functions for creating range view options in [`cb_module_utils`](https://github.com/2600hz/kazoo/blob/873dc106c7a7330393201207eddc365837c3dbe6/applications/crossbar/src/modules/cb_modules_util.erl#L23-L26) has been removed. Instead several options has been introduced in `crossbar_view` to generating correct range view options according to query string parameters or module's options and requested sort direction.

    Crossbar View module introduce new functions to simple load view (`load/2,3`), ranged load (`load_range/2,3`) and ranged load from MODBs (`load_modb/2,3`). It has several ways to configure the `startkey` and `endkey` and the time range and a new generic way to return chunked base response.

### 4.1

1.  `kz_util` refactoring

    We are starting to break kz\_util up into more appropriately-named modules. There is a script that will take care of migrating existing code, \`scripts/kz\_util\_diaspora.bash\`. This is run as part of CircleCI (under the \`make code\_checks\` target) as well.

2.  ACDc Crossbar endpoints moved

    cb\_queues and cb\_agents have been moved to be part of ACDc's modules. Please know that you will need ACDc present if you wish to use ACDc's Crossbar endpoints in a particular Erlang VM.

3. The configuration /etc/kazoo/core/vm.args should no longer be modified locally

    Changes to vm.args is resulting in .rpmnew files that will keep kazoo from starting - simply overwrite the vm.args with vm.args.rpmnew

    Also, you should no longer edit vm.args all parameters are now pulled from config.ini.

4. Kamailio Auto Discovery of FreeSWITCH servers

    Kamailio will automatically manage the dispatcher list, drawn from the list of FreeSWITCH servers connected to ecallmgr.  To check the current status use the command: kazoo-Kamailio status

    > **Note:** Dbtext, /etc/kazoo/kamailio/dbtext, is no longer used.  You can override the automatic discovery and set the dispatcher list manually but you must use the SQL interface: KazooDB

    > **Note:** Make sure your Kamailio is properly federated on a multi-zone cluster to avoid inter zone call looping

### 4.0

1.  Erlang Version Support

    Starting with Kazoo 4.0 Erlang support will target 18+ and will not be backward compatible with prior Erlang versions.

2.  Consistent naming (removal of references to whistle)

    All instances of whistle or variations such as wh&ensp;have been replaced with kazoo or kz\\\_. This will include all maintenance functions as well as init.d/systemd scripts. There is a script that can be used to assist with renaming and changing references in code `scripts/wh-to-kz.sh`.

3.  Number Manager

    Upgrading to 4.0 will shift number management from `core/whistle_number_manager` to `core/kazoo_number_manager`. The upgrade process *should* be seamless from the user's perspective, with some exceptions: \* API calls for attachments and port requests all go to `cb_port_requests`

4.  CouchDB

    Upgrading will change the way Kazoo interacts with CouchDB (including deprecating using BigCouch and recommending CouchDB!). For most operations, nothing will be noticeably different.

5.  Authorizing-ID

    Currently, inbound calls from carriers don't have a Authorizing-ID but if the device has a redirection the CDR will have the Authorizing-ID header. This will change in version 4 as we believe the inbound leg should not have the Authorizing-ID set, so the CDR for inbound call (leg a) will not have the Authorizing-ID. The b-leg will have the Authorizing-ID set to the device that redirect the call. Restrictions will still be applied based on the device that redirected the call.

6.  Auth Token

    The `token_auth` database always had a lot of pressure in it, with known problems in size due to the nature of BigCouch/CouchDB, requiring intensive use of compactor which also led to intensive disk usage and network communication for synchronizing the db cluster.

    We decided to move to a cryptography-based solution by adopting [JSON web tokens](https://jwt.io/) which will allow the drop of `token_auth` database. The generated token can be safely validated by any node without requiring the usage of a database-persisted token.

    As a consequence of this change, the auth token included in REST API responses has increased in size. If you were validating the length of the auth\_token key (or X-Auth-Token header in the response headers) you may need to remove or update that check (we recommend removing it!).

7.  Webhooks

    Webhook trigger for federated messages is dropped. We now require that at least one instance of the webhooks app is running in each zone for the webhook to be triggered.

    Added new option `include_subaccounts` which will process the hook for subaccount events.

8.  Blacklist changes

    In the 3.x series, a `raw_numbers` attribute was allowed, ostensibly to match anonymous caller IDs. This has been removed and a more proper solution has been put in place. You can still add anonymous caller ID to your blacklists, or you can configure the account/system to block them via the `block_anonymous_caller_id` key:

        # Account
        #> sup kapps_account_config set {ACCOUNT_ID} stepswitch block_anonymous_caller_id true
        # System
        #> sup kapps_config set_default stepswitch block_anonymous_caller_id true

9.  Voicemail Messages

    Starting with Kazoo 4.0 all new voicemail messages goes into modb. All Kazoo Administrators need to migrate their voicemail messages from Kazoo version 3.22 to MODB. There is maintenance command for this transition. For more information about this change please see documentation for the new kazoo\\\_voicemail core application and crossbar voicemail documentation.

10. Removing socket.io support from Websockets

    The Blackhole application providing websocket support currently utilizes the socket.io client libraries. Due to the poor support for this server side in Erlang as well as the judgment that this provides little benefit it has been removed. The websockets now provide messaging without the socket.io overhead. More documentation will be available shortly. Please note that we still consider websockets beta functionality.

11. System media has been moved

    The media prompts that we used to keep in the code repository for historical reasons has been moved the [kazoo-sounds](https://github.com/2600hz/kazoo-sounds) repository.

12. Moved `cf_endpoint` to core

    The Callflow modules `cf_endpoint` and `cf_attributes` were used by multiple applications creating undesired dependencies between applications. This was done to avoid copy-pasting common code but is a clear indication that this functionality belongs in core. We have removed these modules from Callflows and moved them to <https://github.com/2600hz/kazoo/tree/master/core/kazoo\_endpoint>.

13. Moved `knm_sip` to inside `kzsip_uri`

    The `knm_sip` module shares a lot of functionality with the `kazoo_sip` core application. Its innards have thus been moved.


### 3.22

1.  FreeSWITCH 1.4.26+ / 1.6+

    To upgrade to Kazoo-FreeSWITCH 1.4.26+ or FreeSWITCH 1.6+ you must set the system\\\_config/ecallmgr parameter `use_kazoo_dptools` to TRUE. Failure to do so may cause inconsistent channel information among ecallmgr nodes connected to the same FreeSWITCH instance. If you are using Kazoo-FreeSWITCH 1.4.x builds prior to .26 this parameter should be left as its default, FALSE.

2.  Monster UI Enabled Applications

    The enabled Monster UI applications were moved from an object on the account document to its own document with the id `apps_store`. When you run `sup kapps_maintenance migrate` this will automatically preform this operation but until it is complete users logging into Monster UI may not see their previously enabled applications. Once the migration is complete the change should be transparent to end-users.

3.  Company Directory PDF

    If you plan to support the new API to download the company directory as a PDF you will need to install `htmldoc` on any server running crossbar.

4.  Default WebRTC Port change

    The default ports that Kamailio listens to for the WebRTC websocket have changed, this was due to a port conflict on all-in-one installs with TLS enabled Kazoo APIs. The standard HTTP websocket port was 8080 and is now 5064. The TLS HTTP websocket port was 8443 and is now 5065. If you would like to continue using the old ports please update "/etc/kazoo/kamailio/local.cfg" after an update to kazoo-configs 3.22.12+


## Upcoming


### May 2016

1.  Deprecating `deps/mochiweb`

    Most operations have been moved to the Cowboy or Cowlib projects. We will formally remove mochiweb from `deps/`. If you maintain code apart from Kazoo that uses mochiweb, please either covert to equivalent functionality with Cowboy/Cowlib or plan how you'll build your custom code with your own dependency of mochiweb.

2.  Deprecating `deps/exmpp`

    exmpp library has problems restarting. it will be replaced by `deps/escalus`

3.  Deprecating `deps/ejson`

    ejson used for json encode, decode will be replaced by `deps/jiffy`

4.  Deprecating `deps/ibrowse`

    `ibrowse` will be replaced by `core/kazoo_web/kz_http` which is using Erlang `httpc`. `kz_http` is the new HTTP client module now and the previous `kz_http` module is renamed to `kz_http_util`.

    If you maintain code apart from Kazoo that uses `ibrowse`, please either covert to equivalent functionality with `kz_http=/=httpc` or plan how you'll build your custom code with your own dependency of `ibrowse`.
