# Scripts

This is the scripts directory, where we place scripts of various types to help with various activities. :)

Let's get a little more concrete though.


## apps-process-count.sh

A simple script to query the Erlang VMs process count

```shell
./scripts/apps-process-count.sh
10
```

## apps_of_app.escript

Calculates application interdependencies, correcting .app.src files as necessary. Also can be used to detect circular references.

For now, we just calculate app files in `applications/` since `core/` is a tangled mess right now (and is typically installed as one lump package anyway).

### Create .dot file of application dependencies

```shell
ERL_LIBS=deps:core:applications ./scripts/apps_of_app.escript -d
cwd: {ok,"/home/james/local/git/2600hz/kazoo"}
o: [dot]
a: []
................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................wrote DOT file to /tmp/kazoo_project.dot
```

Then use your favorite program to convert the .dot file to an image.

To just do a single application's dependencies: `ERL_LIBS=deps:core:applications ./scripts/apps_of_app.escript -d {APPLICATION}`

### Find cycles in application dependencies

```shell
ERL_LIBS=deps:core:applications ./scripts/apps_of_app.escript -c
cwd: {ok,"/home/james/local/git/2600hz/kazoo"}
o: [circle]
a: []
finding circular dependencies ................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................ done
cycle through braintree: [braintree,kazoo_config,kazoo_services,braintree]
cycle through crossbar: [crossbar,kazoo_apps,kazoo_proper,crossbar]
cycle through kazoo: [kazoo,kazoo_config,kazoo]
cycle through kazoo_amqp: [kazoo_amqp,kazoo_data,kazoo_amqp]
cycle through kazoo_apps: [kazoo_apps,kazoo_call,kazoo_apps]
cycle through kazoo_attachments: [kazoo_attachments,kazoo_data,
                                  kazoo_attachments]
cycle through kazoo_auth: [kazoo_auth,kazoo_data,kazoo_attachments,kazoo_auth]
cycle through kazoo_bindings: [kazoo_bindings,kazoo,kazoo_documents,
                               kazoo_apps,kazoo_bindings]
cycle through kazoo_caches: [kazoo_caches,kazoo,kazoo_config,kazoo_caches]
cycle through kazoo_call: [kazoo_call,kazoo_apps,kazoo_call]
cycle through kazoo_config: [kazoo_config,kazoo,kazoo_config]
cycle through kazoo_couch: [kazoo_couch,kazoo,kazoo_documents,kazoo_apps,
                            kazoo_proper,tasks,kazoo_couch]
cycle through kazoo_csv: [kazoo_csv,kazoo,kazoo_data,kazoo_modb,kazoo_csv]
cycle through kazoo_data: [kazoo_data,kazoo,kazoo_data]
cycle through kazoo_documents: [kazoo_documents,kazoo,kazoo_documents]
cycle through kazoo_endpoint: [kazoo_endpoint,kazoo_call,kazoo_endpoint]
cycle through kazoo_etsmgr: [kazoo_etsmgr,kazoo,kazoo_documents,kazoo_apps,
                             kazoo_proper,crossbar,kazoo_token_buckets,
                             kazoo_etsmgr]
cycle through kazoo_globals: [kazoo_globals,kazoo_amqp,kazoo_globals]
cycle through kazoo_ips: [kazoo_ips,kazoo_config,kazoo_services,kazoo_ips]
cycle through kazoo_ledgers: [kazoo_ledgers,kazoo_config,kazoo_services,
                              kazoo_ledgers]
cycle through kazoo_media: [kazoo_media,kazoo_config,kazoo_call,kazoo_media]
cycle through kazoo_modb: [kazoo_modb,kazoo_apps,kazoo_modb]
cycle through kazoo_number_manager: [kazoo_number_manager,kazoo_apps,
                                     kazoo_number_manager]
cycle through kazoo_oauth: [kazoo_oauth,kazoo,kazoo_documents,kazoo_apps,
                            kazoo_proper,crossbar,kazoo_oauth]
cycle through kazoo_perf: [kazoo_perf,kazoo,kazoo_documents,kazoo_apps,
                           kazoo_proper,crossbar,kazoo_perf]
cycle through kazoo_proper: [kazoo_proper,kazoo_apps,kazoo_proper]
cycle through kazoo_schemas: [kazoo_schemas,kazoo,kazoo_config,kazoo_schemas]
cycle through kazoo_services: [kazoo_services,kazoo_config,kazoo_services]
cycle through kazoo_sip: [kazoo_sip,kazoo_stdlib,kazoo_documents,kazoo_apps,
                          kazoo_call,kazoo_endpoint,kazoo_sip]
cycle through kazoo_speech: [kazoo_speech,kazoo_config,kazoo_call,
                             kazoo_speech]
cycle through kazoo_stats: [kazoo_stats,kazoo_amqp,kazoo_globals,kazoo_apps,
                            kazoo_proper,tasks,kazoo_couch,kazoo_stats]
cycle through kazoo_stdlib: [kazoo_stdlib,kazoo_documents,kazoo_stdlib]
cycle through kazoo_tasks: [kazoo_tasks,kazoo_documents,kazoo_apps,
                            kazoo_proper,crossbar,kazoo_tasks]
cycle through kazoo_templates: [kazoo_templates,kazoo,kazoo_documents,
                                kazoo_apps,kazoo_proper,crossbar,
                                kazoo_templates]
cycle through kazoo_token_buckets: [kazoo_token_buckets,kazoo,kazoo_documents,
                                    kazoo_apps,kazoo_proper,crossbar,
                                    kazoo_token_buckets]
cycle through kazoo_transactions: [kazoo_transactions,kazoo_documents,
                                   kazoo_transactions]
cycle through kazoo_voicemail: [kazoo_voicemail,kazoo_apps,kazoo_voicemail]
cycle through kazoo_web: [kazoo_web,kazoo_stdlib,kazoo_documents,kazoo_web]
cycle through kazoo_xml: [kazoo_xml,kazoo_stdlib,kazoo_documents,kazoo_apps,
                          kazoo_number_manager,kazoo_xml]
cycle through tasks: [tasks,kazoo_apps,kazoo_proper,tasks]
```

## bump-copyright-year.sh

Python script to walk the supplied files and bumps the copyright year if appropriate.

```shell
./scripts/bump-copyright-year.sh [FILE]
```


## check-app-registered.sh

Checks Erlang applications for registered processes and compares that to the application's .app.src file.

```shell
./scripts/check-app-registered.sh [PATH/TO/APP]
```

For example, I set \`{registered, []} in callflow.app.src, then ran the script:

```shell
./scripts/check-app-registered.sh applications/callflow
cf_event_handler_sup, callflow_sup, cf_exe_sup
applications/callflow has no registered modules??
1 errors
1 errors in total
```

Now you have a listing of registered processes to put in your .app.src


## check-dialyzer.escript

An Erlang escript that dialyzes changed files. Run it using the makefile target 'dialyze' with the files to dialyze:

```shell
TO_DIALYZE=applications/callflow/ebin/callflow_sup.beam make dialyze
scanning "applications/callflow/ebin/callflow_sup.beam"
0 Dialyzer warnings
```

Typically \`TO\_DIALYZE\` would be a generated list of files.

Do note: this will only check the file itself for issues. To really leverage Dialyzer, you'll want to include remote project modules for Dialyzer to use as well.

## check-loglines.bash

Checks code for log lines that start with a capital letter.

## check-ref-docs.bash

Compares ref docs to their real version

## check-scripts-readme.bash

A quick script to check that all scripts in `$(ROOT)/scripts` are documented in this file!


## check-spelling.bash

Takes the misspellings.txt and checks for common mistakes.

Each line on the text file has the format `{correct}|{mispelt} [{misspelt} ...]`


## check-unstaged.bash

Checks if any unstaged changes are found in the repo and exits if so. Used in CircleCI to fail builds with unstaged changes after applying code checks, spell checking, etc.


## check-xref.escript

An Erlang escript for cross referencing (xref) calls to remote modules. Set \`TO\_XREF\` to ebin paths (or use the default):

```shell
make xref
Pass: global
Loading modules...
Running xref analysis...
Xref: listing undefined_function_calls
Xref: listing undefined_functions
Done
```

If there are any calls to nonexistent modules, or non-exported functions, you will get errors listed here.


## check-whitespace.sh

Removes trailing whitespaces from files

## circleci.bash

When building applications that don't live in the kazoo src tree, this script will configure CircleCI to run the necessary tests against Kazoo with the application added in.

## circleci-build-erlang.sh

Fetches kerl and installs configured Erlang version (used in CircleCI)


## code\_checks.bash

Checks source code files for various formatting expectations and exits if any are found.

```shell
./scripts/code_checks.bash applications/crossbar/src/cb_context.erl
Check for andalso/orelse dropped lines
Check for uses of module in lieu of ?MODULE
Check for TAB characters
Check for trailing whitespaces
```


## `code_checks.bash`

Checks source code for various style requirements of the project


## conn-to-apps.sh

Opens a remote shell to the kazoo\_apps@hostname VM.

```shell
./scripts/conn-to-apps.sh [{VM@HOSTNAME}, {LOCAL_SHELL@HOSTNAME}]
```


## conn-to-ecallmgr.sh

A convenience wrapper for connecting to ecallmgr@HOSTNAME via conn-to-apps.sh


## `convert_org_files.bash`

Script that is helpful when converting org files from 8.x to 9.x


## cover.escript

creates and sends coverage report for testing of codebase


## crash-apps.sh

Forces the running VM to halt, producing a crashdump, and exiting with status code 1 (as per the [docs](http://erldocs.com/18.0/erts/erlang.html?i=2&search=halt#halt/2)). Currently hard-coded the VM name to 'kazoo\_apps'


## crash-ecallmgr.sh

Same as crash-apps.sh but for the ecallmgr VM.


## dev-exec-mfa.sh

Runs M:F(A) on the node: \\#+INCLUDE "../dev-exec-mfa.sh" :lines "3-6"


## dev-start-apps.sh

Starts a VM with an interactive shell. {VM\_NAME} defaults to 'kazoo\_apps'

```shell
./scripts/dev-start-apps.sh {VM_NAME}
```


## dev-start-ecallmgr.sh

Defaults node name to 'ecallmgr'; otherwise the same as dev-start-apps.sh


## dev/kazoo.sh

When using releases, executes a release command against the running VM:

```shell
KAZOO_CONFIG=/etc/kazoo/core/config.ini ./scripts/dev/kazoo.sh {CMD}
```

{CMD} can be:

-   'attach': Attach to a running VM
-   'console': connect to the VM with an interactive shell
-   'escript': Run an escript under the node's environment
-   'eval': evaluates the string in the running VM
-   'foreground': start up the release in the foreground
-   'pid': get the OS pid of the VM
-   'ping': test aliveness of the VM
-   'reboot': restart the VM completely (new OS process)
-   'remote\_console': connect as a remote shell
-   'restart': restart the VM without exiting the OS process
-   'rpc': execute a remote procedure call
-   'rpcterms':
-   'start'/'start\_boot': start the VM
-   'stop': stop the VM
-   'unpack': Unpack a tar.gz for upgrade/downgrade/installation
-   'upgrade'*'downgrade'*'install': perform an upgrade/downgrade/installation


## dev/sup.sh

Runs the SUP escript against the running release


## dialyze-changed.bash

This script gets a diff set (against master) of .erl files from the current branch and dialyzes all changed files. You can include extra beam files on the end of the script (for things like gen\_listener, kz\_json, etc).

```shell
./scripts/dialyze-changed.bash core/kazoo/ebin/kz_json.beam
dialyzing changed files:
  Checking whether the PLT .kazoo.plt is up-to-date... yes
  Compiling some key modules to native code... done in 0m0.28s
  Proceeding with analysis...
  ...Issues Found...
  Unknown functions:
  ...Unknown functions...
  Unknown types:
  ...Unknown types...
 done in 0m6.69s
done (warnings were emitted)
```


## dialyze-usage.bash

Given a module name, such as 'props' or 'kz\_json', search core/applications for modules that make calls to the supplied module and dialyze those beam files looking for dialyzer complaints. You will likely see complaints unrelated to your supplied module - go ahead and fix those too if possible ;)

The more heavily utilized the module is, the longer this will take to run!

```shell
 ./scripts/dialyze-usage.bash kz_config
dialyzing usages of kz_config
  Checking whether the PLT .kazoo.plt is up-to-date... yes
  Proceeding with analysis...
kz_dataconfig.erl:26: Function connection/0 has no local return
kz_dataconfig.erl:27: The call kz_config:get('data','config',['bigcouch',...]) breaks the contract (section(),atom(),Default) -> kz_proplist() | Default
kz_dataconfig.erl:32: Function connection_options/1 will never be called
...
 done in 0m4.08s
done (warnings were emitted)
```


## ecallmgr-process-count.sh

Connects to the ecallmgr VM and outputs a count of running Erlang processes.


## `edocify.escript`

A script to fixes most issues which prevent EDoc to run properly.

> **Note:** This script needs [`ag`](https://github.com/ggreer/the_silver_searcher) command line to run!

```shell
edocify.escript
```

Sample Output:

```shell
$ scripts/edocify.escript
Edocify Kazoo...

* removing evil sepc+specs:  done
* rename and fix `@contributors' tags to '@author':  done
* remove @public tag:  done
* removing @spec from comments: .............................. done
* adding missing comments block after separator:  done
* escape code block for 'resource_exists' function crossbar modules:  done
* fix comment blocks with no @end:  done
* move first comment line to the same line as @doc:  done
* remove empty comment line after @doc: .......... done

Already EDocified! 🎉
```

## `empty_schema_descriptions.bash`

Checks JSON schemas for empty "description" properties and exit(1) if any are found


## `eunit_run.escript`

Run EUnit tests on specified module names, filtering test modules which has same as their normal module name like `fobar` and `fobar_tests` to avoid running the `fobar_tests` twice.

Usage:

```bash
cd core/kazoo_stdlib ## or your desire application or on the root kazoo project dir
KAZOO_CONFIG=../../rel/config-test.ini ERL_LIBS=../../deps:../../core:../../applications ../../scripts/eunit_run.escript --with-cover \
    --cover-project-name kazoo_stdlib --cover-report-dir cover \
    kz_binary kz_binary_tests kz_date kz_date_tests kz_json kz_json_tests kz_maps kz_maps_tests kz_mochinum kz_module kz_module_tests
    kz_term kz_term_tests kz_time kz_time_tests kz_types props props_tests

```

or simply:

```bash
cd core/kazoo_stdlib ## or your desire application or on the root kazoo project dir
make eunit # or make test
```


## `export_auth_token.bash`

Script for exporting `AUTH_TOKEN` and `ACCOUNT_ID` when doing Crossbar authentication. Handy when running curl commands to use `$AUTH_TOKEN` instead of the raw value (and for re-authing when auth token expires).

```bash
./scripts/export_auth_token.bash -h
Gets an auth token and outputs Bash script to export the following environment
variables:

    ACCOUNT_ID
    AUTH_TOKEN

Usage:

$(eval ./scripts/export_auth_token.bash [options])

Options:

-v:                      Print extra information (verbose)
-c CREDENTIALS_HASH      $(echo -n "username:password" | md5sum | cut -d ' ' -f 1)
-a ACCOUNT_NAME
-p PHONE_NUMBER
-r ACCOUNT_REALM
-k API_KEY
-s BASE_URL             Defaults to http://localhost:8000; include the http(s) scheme
```

## format-json.sh

Python script to format JSON files (like CouchDB views, JSON schemas) and write the formatted version back to the file. 'make apis' runs this as part of its instructions.

```shell
./scripts/format-json.sh path/to/file.json [path/to/other/file.json,...]
```


## generate-api-endpoints.escript

Builds the Crossbar reference docs in 'applications/crossbar/doc/ref'. Helps detect when Crossbar endpoints have changes to their functionality that is client-facing.

Also builds the [Swagger](http://swagger.io/) JSON file in applications/crossbar/priv/api/swagger.json


## generate-doc-schemas.sh

Updates crossbar docs with the schema table from the ref (auto-gen) version


## generate-fs-headers-hrl.escript

Parses the ecallmgr code looking for keys used to access values in the FreeSWITCH proplist and builds a header file at applications/ecallmgr/src/fs\_event\_filters.hrl for use when initializing mod\_kazoo.

## generate-kzd-builders.escript

Builds accessor modules (`.erl.src` files) in `core/kazoo_documents/src` from the API schemas. Changes to the `.erl.src` should be manually migrated to the corresponding `.erl` file.

## generate-schemas.escript

Parses the core/applications code looking for calls to kapps\_config (module used to access documents in the system\_config database) and building a base JSON schema file for each document found.

Also parses callflow's action modules looking for keys used to access values in the Data JSON object to build a base JSON schema file for each callflow action.

## `kzd_module_check.bash`

Check kazoo document auto-gen .src modules against the corresponding .erl files.

## `kz_diaspora.bash`

Script for updating Erlang code to account for functions that have moved modules.

-   kz\_util to alternative modules
-   kz\_json to kz\_doc for public/private fields


## `list-ext-deps.escript`

This escript gathers information from all `.beam` files in the filesystem tree specified by a list of directories provided to it on the command line, determines which external calls these files collectively make, and compares these calls with the applications provided by the Erlang runtime under which the script is running.

The end result is a list of OTP applications that this set of `.beam` files collectively make calls to (i.e. depend on).

* NOTE: The `.beam` files *must* be compiled with debug information for this script to be useful.

### Example

In this example, we find the names of all the Erlang applications which the `.beam` files in `applications/`, `core/`, and `deps` depend on.

```
$ scripts/list-ext-deps.escript core applications deps 2> /tmp/errors.log
common_test
compiler
crypto
erts
eunit
inets
kernel
mnesia
observer
public_key
runtime_tools
sasl
ssl
stdlib
syntax_tools
tools
xmerl
```


## `no_raw_json.escript`

Erlang has a handful of internal representations of JSON used by the various parses. The kz\_json module handles these details and Kazoo programmers should treat the data structure used as opaque. This script parses the codebase looking for instances where the opaqueness of the data structure is violated.


## rabbitmq-generic.sh

Wrapper for running rabbitmq script commands?


## rabbitmq-server.init

Init.d script for rabbitmq


## `reconcile_docs_to_index.bash`

Finds all docs in the repo and checks which are included in the [mkdocs.yml](/doc/mkdocs/mkdocs.yml) index


## setup-dev.sh

Script to setup a dev environment including:

-   Symlink SUP to /usr/bin
-   Symlink rabbitmq init.d script to /etc/init.d
-   Symlink kazoo init.d scripts to /etc/init.d
-   Reset RabbitMQ mnesia databases, logs
-   Setup users for rabbitmq and kazoo


## setup-git.sh

Setup the username/email to use in Git commits and other Git settings


## `setup_docs.bash`

Script for setting up a local environment for running the mkdocs-built docs site


## src2any.escript

Reads the .app.src file and writes a .src file?


## start-apps.sh

Starts a VM in the background with name kazoo\_apps


## start-ecallmgr.sh

Starts a VM in the background with name ecallmgr


## state-of-docs.sh

Searches for undocumented APIs and reports percentage of doc coverage.

```shell
./scripts/state-of-docs.sh
```

    Undocumented API endpoints:
    > DELETE /v2/templates/{TEMPLATE_NAME}
    > PUT /v2/templates/{TEMPLATE_NAME}
    > GET /v2/sup/{MODULE}
    > GET /v2/accounts/{ACCOUNT_ID}/agents
    > GET /v2/accounts/{ACCOUNT_ID}/agents/stats
    > GET /v2/accounts/{ACCOUNT_ID}/agents/status
    > POST /v2/accounts/{ACCOUNT_ID}/agents/status/{USER_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/agents/status/{USER_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/queue_status
    > POST /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/queue_status
    > GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/status
    > POST /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/status
    > GET /v2/accounts/{ACCOUNT_ID}/alerts
    > PUT /v2/accounts/{ACCOUNT_ID}/alerts
    > DELETE /v2/accounts/{ACCOUNT_ID}/alerts/{ALERT_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/alerts/{ALERT_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/blacklists
    > PUT /v2/accounts/{ACCOUNT_ID}/blacklists
    > GET /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
    > PATCH /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/bulk
    > POST /v2/accounts/{ACCOUNT_ID}/bulk
    > PUT /v2/accounts/{ACCOUNT_ID}/cccps
    > PUT /v2/accounts/{ACCOUNT_ID}/cccps/{CCCP_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/cccps/{CCCP_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/cccps/{CCCP_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/cccps/{CCCP_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/cdrs/summary
    > PUT /v2/accounts/{ACCOUNT_ID}/clicktocall
    > PATCH /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect
    > POST /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect
    > GET /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/history
    > GET /v2/accounts/{ACCOUNT_ID}/conferences
    > PUT /v2/accounts/{ACCOUNT_ID}/conferences
    > PATCH /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants
    > GET /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants/{PARTICIPANT_ID}
    > PATCH /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/connectivity
    > DELETE /v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}
    > PATCH /v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/directories
    > POST /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
    > PATCH /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/faxboxes
    > PUT /v2/accounts/{ACCOUNT_ID}/faxboxes
    > DELETE /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
    > PATCH /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/freeswitch
    > PUT /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates
    > GET /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates
    > GET /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates/{TEMPLATE_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates/{TEMPLATE_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates/{TEMPLATE_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates/{TEMPLATE_ID}/image
    > GET /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates/{TEMPLATE_ID}/image
    > DELETE /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates/{TEMPLATE_ID}/image
    > GET /v2/accounts/{ACCOUNT_ID}/hotdesks
    > GET /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates
    > PUT /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates
    > GET /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates/{TEMPLATE_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates/{TEMPLATE_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates/{TEMPLATE_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates/{TEMPLATE_ID}/image
    > POST /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates/{TEMPLATE_ID}/image
    > DELETE /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates/{TEMPLATE_ID}/image
    > GET /v2/accounts/{ACCOUNT_ID}/menus
    > PUT /v2/accounts/{ACCOUNT_ID}/menus
    > PATCH /v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/metaflows
    > DELETE /v2/accounts/{ACCOUNT_ID}/metaflows
    > POST /v2/accounts/{ACCOUNT_ID}/metaflows
    > PUT /v2/accounts/{ACCOUNT_ID}/onboard
    > GET /v2/accounts/{ACCOUNT_ID}/parked_calls
    > POST /v2/accounts/{ACCOUNT_ID}/presence
    > GET /v2/accounts/{ACCOUNT_ID}/presence/report-{REPORT_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/presence/{EXTENSION}
    > PUT /v2/accounts/{ACCOUNT_ID}/queues/eavesdrop
    > PUT /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/eavesdrop
    > POST /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster
    > GET /v2/accounts/{ACCOUNT_ID}/rate_limits
    > DELETE /v2/accounts/{ACCOUNT_ID}/rate_limits
    > POST /v2/accounts/{ACCOUNT_ID}/rate_limits
    > GET /v2/accounts/{ACCOUNT_ID}/resource_selectors
    > GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/rules
    > POST /v2/accounts/{ACCOUNT_ID}/resource_selectors/rules
    > DELETE /v2/accounts/{ACCOUNT_ID}/resource_selectors/{UUID}
    > GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/{UUID}
    > POST /v2/accounts/{ACCOUNT_ID}/resource_selectors/{UUID}
    > PUT /v2/accounts/{ACCOUNT_ID}/resource_templates
    > GET /v2/accounts/{ACCOUNT_ID}/resource_templates
    > POST /v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}
    > PATCH /v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/service_plans/reconciliation
    > POST /v2/accounts/{ACCOUNT_ID}/service_plans/synchronization
    > GET /v2/accounts/{ACCOUNT_ID}/services/plan
    > POST /v2/accounts/{ACCOUNT_ID}/services/status
    > GET /v2/accounts/{ACCOUNT_ID}/services/status
    > PUT /v2/accounts/{ACCOUNT_ID}/signup
    > POST /v2/accounts/{ACCOUNT_ID}/signup/{THING}
    > PUT /v2/accounts/{ACCOUNT_ID}/sms
    > GET /v2/accounts/{ACCOUNT_ID}/sms/{SMS_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/sms/{SMS_ID}
    > PATCH /v2/accounts/{ACCOUNT_ID}/storage
    > DELETE /v2/accounts/{ACCOUNT_ID}/storage
    > PUT /v2/accounts/{ACCOUNT_ID}/storage
    > POST /v2/accounts/{ACCOUNT_ID}/storage
    > PUT /v2/accounts/{ACCOUNT_ID}/storage/plans
    > GET /v2/accounts/{ACCOUNT_ID}/storage/plans
    > PATCH /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/output
    > PUT /v2/accounts/{ACCOUNT_ID}/temporal_rules
    > POST /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
    > PATCH /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets
    > GET /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets
    > POST /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
    > PATCH /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
    > GET /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
    > DELETE /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
    > DELETE /v2/accounts/{ACCOUNT_ID}/whitelabel
    > PUT /v2/accounts/{ACCOUNT_ID}/whitelabel
    > POST /v2/accounts/{ACCOUNT_ID}/whitelabel
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel
    > POST /v2/accounts/{ACCOUNT_ID}/whitelabel/icon
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/icon
    > POST /v2/accounts/{ACCOUNT_ID}/whitelabel/logo
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/logo
    > POST /v2/accounts/{ACCOUNT_ID}/whitelabel/welcome
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/welcome
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}/icon
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}/logo
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}/welcome
    > GET /v2/sup/{MODULE}/{FUNCTION}
    > GET /v2/sup/{MODULE}/{FUNCTION}/{ARGS}
    > DELETE /v2/auth/links
    > GET /v2/about
    > GET /v2/auth/links
    > GET /v2/auth/tokeninfo
    > GET /v2/templates
    > POST /v2/auth/links
    > PUT /v2/auth/authorize
    > PUT /v2/auth/callback
    > PUT /v2/ip_auth
    > PUT /v2/shared_auth

    349 / 526 ( 66% documented )

    Documented but not matching any allowed_method:
    > DELETE /v2/notifications/{NOTIFICATION_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/about
    > GET /v2/accounts/{ACCOUNT_ID}/descendants/port_requests
    > PATCH /v2/accounts/{ACCOUNT_ID}/descendants/webhooks
    > DELETE /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/access_lists
    > GET /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/channels
    > GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/cdrs
    > GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/channels
    > GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/devices
    > GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/recordings
    > GET /v1/accounts
    > GET /v2/channels
    > GET /v2/notifications
    > GET /v2/phone_numbers
    > GET /v2/resource_selectors/rules
    > GET /v2/search
    > GET /v2/search/multi
    > GET /v2/tasks
    > GET /v2/webhooks
    > GET /v2/websockets
    > POST /v2/resource_selectors/rules
    > POST /v2/whitelabel/domains

## state-of-edoc.escript

Searches for undocumented source files:

```shell
./scripts/state-of-edoc.escript
```

    State of EDoc
    ...........................................[more_dots]

    These files don't have documentations in module header:
    -- applications/acdc/src/acdc_agent_listener.erl
    -- applications/acdc/src/acdc_agent_maintenance.erl
    -- applications/acdc/src/acdc_agent_sup.erl
    -- applications/acdc/src/acdc_agent_util.erl
    -- applications/acdc/src/acdc_agents_sup.erl
    -- applications/acdc/src/acdc_announcements.erl
    -- applications/acdc/src/acdc_announcements_sup.erl
    -- applications/acdc/src/acdc_app.erl
    -- applications/acdc/src/acdc_eavesdrop.erl
    -- core/kazoo_xml/src/kz_xml.erl
    [more_files]

    These functions in files don't have any documentations:
    -- applications/acdc/src/acdc_agent_handler.erl [undocumented functions: 11/11 (%100)]
    -- applications/acdc/src/acdc_agent_maintenance.erl [undocumented functions: 5/5 (%100)]
    -- applications/acdc/src/acdc_queue_thief.erl [undocumented functions: 1/1 (%100)]
    -- applications/acdc/src/acdc_stats.erl [undocumented functions: 33/33 (%100)]
    -- applications/acdc/src/acdc_stats_util.erl [undocumented functions: 7/7 (%100)]
    -- applications/acdc/src/acdc_sup.erl [undocumented functions: 2/2 (%100)]
    -- applications/acdc/src/acdc_util.erl [undocumented functions: 15/15 (%100)]
    -- applications/cccp/src/cccp_shared_listener.erl [undocumented functions: 1/9 (%11)]
    -- applications/ecallmgr/src/ecallmgr_fs_conferences_shared.erl [undocumented functions: 1/9 (%11)]
    -- applications/webhooks/src/webhooks_listener.erl [undocumented functions: 1/9 (%11)]
    -- core/amqp_cron/src/amqp_cron_task.erl [undocumented functions: 1/9 (%11)]
    -- core/amqp_leader/src/amqp_leader_listener.erl [undocumented functions: 1/9 (%11)]
    -- core/braintree/src/braintree_customer.erl [undocumented functions: 2/21 (%9)]
    [more_files]


    Processed 1277 files
    Files without documentations in module header: 769/1277 (%60)
    Files with undocumented functions: 1021/1277 (%79)

## `sync_mkdocs_pages.sh`

A script to missing pages (MakrDown files) from `mkdocs.yml` to other YAML files. Flag those files which are deleted or rename.

## `sync_to_remote.bash`

```bash
HOST="server.com" ERL_FILES="path/to/source.erl" BEAM_PATH="/tmp/beams" ./scripts/sync_to_remote.bash
```

Takes the provided Erlang files, finds their .beam and syncs those to the remote server provided.

-   `ERL_FILES`: which source files to sync (the changed files (against master) are used by default).
-   `HOST`: The Host to use for the scp command
-   `BEAM_PATH`: Where on the Host to put the beam files

## `sync_to_release.bash`

Useful in conjunction with `sync_to_remote`. Takes .beam files in a directory and moves them into a release, into the proper application ebin, and reloads them in the default VMs

-   `BEAMS`: Path to beam files, defaults to `/tmp/beams/*.beam`
-   `DEST`: Path to the release's lib/ directory, defaults to `/opt/kazoo/lib`

## tags.escript

Generates a [TAGS file](https://www.emacswiki.org/emacs/EmacsTags) based on all Erlang code in the code path.

## update-the-types.sh

Used to search the code looking for deprecated Erlang functions and types and replace them with the newer versions as appropriate


## validate-js.sh

Processes JSON files:

-   Checks that \_id matches the file name in schema files
-   Checks map functions in CouchDB views for 'Object.keys' usage


## validate-schemas.sh

Validate JSON schema files:

```shell
./scripts/validate-schemas.sh applications/crossbar/priv/couchdb/schemas/
Traceback (most recent call last):
  File "/usr/local/bin/jsonschema", line 11, in <module>
    sys.exit(main())
  File "/usr/local/lib/python2.7/dist-packages/jsonschema/cli.py", line 67, in main
    sys.exit(run(arguments=parse_args(args=args)))
  File "/usr/local/lib/python2.7/dist-packages/jsonschema/cli.py", line 74, in run
    validator.check_schema(arguments["schema"])
  File "/usr/local/lib/python2.7/dist-packages/jsonschema/validators.py", line 83, in check_schema
    raise SchemaError.create_from(error)
jsonschema.exceptions.SchemaError: u'true' is not of type u'boolean'

Failed validating u'type' in schema[u'properties'][u'properties'][u'additionalProperties'][u'properties'][u'required']:
    {u'default': False, u'type': u'boolean'}

On instance[u'properties'][u'lists'][u'required']:
    u'true'

Bad schema: /home/pete/wefwefwef/kazoo.git/applications/crossbar/priv/couchdb/schemas/callflows.lookupcidname.json
{
    "$schema": "http://json-schema.org/draft-03/schema#",
    "_id": "callflows.lookupcidname",
    "description": "Validator for the Lookup callflow element",
    "properties": {
        "lists": {
            "default": [],
            "description": "Array of list ids",
            "items": {
                "type": "string"
            },
            "required": "true",
            "type": "array"
        }
    },
    "required": true,
    "type": "object"
}

Run again with:
./scripts/validate-schemas.sh /home/pete/wefwefwef/kazoo.git/applications/crossbar/priv/couchdb/schemas/callflows.lookupcidname.json
```


## validate-swagger.sh

Validate Swagger file using online validator

```shell
./scripts/validate-swagger.sh
```

      % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                     Dload  Upload   Total   Spent    Left  Speed
    100  2973  100  2973    0     0   4945      0 --:--:-- --:--:-- --:--:--  4938

Here no errors were generated.


## `validate_mkdocs.py`

Parses the mkdocs.yml and looks for nonexistent docs


## `wh_to_kz.sh`

Part of the great rename, converts Whistle-related names to Kazoo-specific names


## `make-swag.sh`

Generate API clients in multiple languages from the Swagger file.

## `next_version`

returns next release based on branch & tags.

## `increment_version.bash`

routine to increment a version
