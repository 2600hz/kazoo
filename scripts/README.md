# Scripts

This is the scripts directory, where we place scripts of various types to help with various activities. :)

Let's get a little more concrete though.


## apps-process-count.sh

A simple script to query the Erlang VMs process count

```shell
./scripts/apps-process-count.sh
10
```


## bump-copyright-year.sh

Python script to walk the supplied files and bumps the copyright year if appropriate. \\#+BEGIN\_EXAMPLE ./scripts/bump-copyright-year.sh [FILE] \\#+END\_SRC


## check-app-registered.sh

Checks Erlang applications for registered processes and compares that to the application's .app.src file. \\#+BEGIN\_EXAMPLE ./scripts/check-app-registered.sh [PATH/TO/APP] \\#+END\_SRC

For example, I set \`{registered, []} in callflow.app.src, then ran the script: \\#+BEGIN\_EXAMPLE ./scripts/check-app-registered.sh applications/callflow cf\_event\_handler\_sup, callflow\_sup, cf\_exe\_sup applications/callflow has no registered modules?? 1 errors 1 errors in total \\#+END\_SRC Now you have a listing of registered processes to put in your .app.src


## check-dialyzer.escript

An Erlang escript that dialyzes changed files. Run it using the makefile target 'dialyze' with the files to dialyze: \\#+BEGIN\_EXAMPLE TO\_DIALYZE=applications/callflow/ebin/callflow\_sup.beam make dialyze scanning "applications/callflow/ebin/callflow\_sup.beam" 0 Dialyzer warnings \\#+END\_SRC

Typically \`TO\_DIALYZE\` would be a generated list of files.

Do note: this will only check the file itself for issues. To really leverage Dialyzer, you'll want to include remote project modules for Dialyzer to use as well.


## check-release-startup.sh

Creates a release, starts it, and issues some commands to test that the release starts up and appears to be running


## check-scripts-readme.bash

A quick script to check that all scripts in `$(ROOT)/scripts` are documented in this file!


## check-xref.escript

An Erlang escript for cross referencing (xref) calls to remote modules. Set \`TO\_XREF\` to ebin paths (or use the default): \\#+BEGIN\_EXAMPLE make xref Pass: global Loading modules&#x2026; Running xref analysis&#x2026; Xref: listing undefined\_function\_calls Xref: listing undefined\_functions Done \\#+END\_SRC If there are any calls to non-existant modules, or non-exported functions, you will get errors listed here.


## circleci-build-erlang.sh

Fetches kerl and installs configured Erlang version (used in CircleCI)


## code\_checks.bash

Checks source code files for various formatting expectations and exits if any are found. \\#+BEGIN\_EXAMPLE ./scripts/code\_checks.bash applications/crossbar/src/cb\_context.erl Check for andalso/orelse dropped lines Check for uses of module in lieu of ?MODULE Check for TAB characters Check for trailing whitespaces \\#+END\_SRC


## `code_checks.bash`

Checks source code for various style requirements of the project


## conn-to-apps.sh

Opens a remote shell to the kazoo\_apps@hostname VM. \\#+BEGIN\_EXAMPLE ./scripts/conn-to-apps.sh [{VM@HOSTNAME}, {LOCAL\_SHELL@HOSTNAME}] \\#+END\_SRC


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

Starts a VM with an interactive shell. {VM\_NAME} defaults to 'kazoo\_apps' \\#+BEGIN\_EXAMPLE shell ./scripts/dev-start-apps.sh {VM\_NAME} \\#+END\_SRC


## dev-start-ecallmgr.sh

Defaults node name to 'ecallmgr'; otherwise the same as dev-start-apps.sh


## dev/kazoo.sh

When using releases, executes a release command against the running VM: \\#+BEGIN\_EXAMPLE shell KAZOO\_CONFIG=/etc/kazoo/core/config.ini ./scripts/dev/kazoo.sh {CMD} \\#+END\_SRC

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

\#+BEGIN\_EXAMPLE shell ./scripts/dialyze-changed.bash core/kazoo/ebin/kz\_json.beam dialyzing changed files: Checking whether the PLT .kazoo.plt is up-to-date&#x2026; yes Compiling some key modules to native code&#x2026; done in 0m0.28s Proceeding with analysis&#x2026; &#x2026;Issues Found&#x2026; Unknown functions: &#x2026;Unknown functions&#x2026; Unknown types: &#x2026;Unknown types&#x2026; done in 0m6.69s done (warnings were emitted) \\#+END\_SRC


## dialyze-usage.bash

Given a module name, such as 'props' or 'kz\_json', search core/applications for modules that make calls to the supplied module and dialyze those beam files looking for dialyzer complaints. You will likely see complaints unrelated to your supplied module - go ahead and fix those too if possilbe ;)

The more heavily utilized the module is, the longer this will take to run!

\#+BEGIN\_EXAMPLE shell ./scripts/dialyze-usage.bash kz\_config dialyzing usages of kz\_config Checking whether the PLT .kazoo.plt is up-to-date&#x2026; yes Proceeding with analysis&#x2026; kz\_dataconfig.erl:26: Function connection/0 has no local return kz\_dataconfig.erl:27: The call kz\_config:get('data','config',['bigcouch',&#x2026;]) breaks the contract (section(),atom(),Default) -> kz\_proplist() | Default kz\_dataconfig.erl:32: Function connection\_options/1 will never be called &#x2026; done in 0m4.08s done (warnings were emitted) \\#+END\_SRC


## ecallmgr-process-count.sh

Connects to the ecallmgr VM and outputs a count of running Erlang processes.


## `export_auth_token.bash`

Script for exporting `AUTH_TOKEN` and `ACCOUNT_ID` when doing Crossbar authentication. Handy when running curl commands to use `$AUTH_TOKEN` instead of the raw value (and for re-authing when auth token expires).


## format-json.sh

Python script to format JSON files (like CouchDB views, JSON schemas) and write the formatted version back to the file. 'make apis' runs this as part of its instructions. \\#+BEGIN\_EXAMPLE shell ./scripts/format-json.sh path/to/file.json [path/to/other/file.json,&#x2026;] \\#+END\_SRC


## generate-api-endpoints.escript

Builds the Crossbar reference docs in 'applications/crossbar/doc/ref'. Helps detect when Crossbar endpoints have changes to their functionality that is client-facing.

Also builds the [Swagger](http://swagger.io/) JSON file in applications/crossbar/priv/api/swagger.json


## generate-doc-schemas.sh

Updates crossbar docs with the schema table from the ref (auto-gen) version


## generate-fs-headers-hrl.escript

Parses the ecallmgr code looking for keys used to access values in the FreeSWITCH proplist and builds a header file at applications/ecallmgr/src/fs\_event\_filters.hrl for use when initializing mod\_kazoo.


## generate-schemas.escript

Parses the core/applications code looking for calls to kapps\_config (module used to access documents in the system\_config database) and building a base JSON schema file for each document found.

Also parses callflow's action modules looking for keys used to access values in the Data JSON object to build a base JSON schema file for each callflow action.


## `kz_util_diaspora.bash`

Script for updating Erlang code to account for functions that used to be in kz\_util and are now moved to alternative modules.


## `no_raw_json.escript`

Erlang has a handful of internal representations of JSON used by the various parses. The kz\_json module handles these details and Kazoo programmers should treat the data structure used as opaque. This script parses the codebase looking for instances where the opaqueness of the data structure is violated.


## rabbitmq-generic.sh

Wrapper for running rabbitmq script commands?


## rabbitmq-server.init

Init.d script for rabbitmq


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

Searches for undocumented APIs and reports percentage of doc coverage. \\#+BEGIN\_EXAMPLE shell ./scripts/state-of-docs.sh Undocumented API endpoints: > PATCH /v2/accounts/{ACCOUNT\_ID}/configs/{CONFIG\_ID} > PUT /v2/accounts/{ACCOUNT\_ID}/onboard > DELETE /v2/accounts/{ACCOUNT\_ID}/faxboxes/{FAXBOX\_ID} > GET /v2/system\_configs/{SYSTEM\_CONFIG\_ID}/{NODE} > GET /v2/accounts/{ACCOUNT\_ID}/acls > GET /v2/accounts/{ACCOUNT\_ID}/blacklists > PUT /v2/accounts/{ACCOUNT\_ID}/temporal\_rules\_sets > GET /v2/accounts/{ACCOUNT\_ID}/blacklists/{BLACKLIST\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/braintree/credits > PUT /v2/accounts/{ACCOUNT\_ID}/global\_provisioner\_templates > POST /v2/accounts/{ACCOUNT\_ID}/whitelabel/logo > DELETE /v2/accounts/{ACCOUNT\_ID}/configs/{CONFIG\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/whitelabel/welcome > PATCH /v2/accounts/{ACCOUNT\_ID}/temporal\_rules\_sets/{TEMPORAL\_RULE\_SET} > GET /v2/accounts/{ACCOUNT\_ID}/local\_provisioner\_templates/{TEMPLATE\_ID} > PUT /v2/google\_auth > PUT /v2/accounts/{ACCOUNT\_ID}/cccps/{CCCP\_ID} > PUT /v2/accounts/{ACCOUNT\_ID}/resource\_selectors/name/{SELECTOR\_NAME}/resource/{RESOURCE\_ID} > DELETE /v2/accounts/{ACCOUNT\_ID}/resource\_templates/{RESOURCE\_TEMPLATE\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/whitelabel/{WHITELABEL\_DOMAIN}/icon > POST /v2/accounts/{ACCOUNT\_ID}/cccps/{CCCP\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/alerts > POST /v2/accounts/{ACCOUNT\_ID}/access\_lists > GET /v2/accounts/{ACCOUNT\_ID}/storage > POST /v2/accounts/{ACCOUNT\_ID}/presence > POST /v2/accounts/{ACCOUNT\_ID}/temporal\_rules\_sets/{TEMPORAL\_RULE\_SET} > GET /v2/accounts/{ACCOUNT\_ID}/sms/{SMS\_ID} > DELETE /v2/accounts/{ACCOUNT\_ID}/access\_lists > GET /v2/accounts/{ACCOUNT\_ID}/access\_lists > GET /v2/accounts/{ACCOUNT\_ID}/resource\_selectors > POST /v2/accounts/{ACCOUNT\_ID}/agents/{USER\_ID}/queue\_status > POST /v2/accounts/{ACCOUNT\_ID}/resource\_selectors/name/{SELECTOR\_NAME}/resource/{RESOURCE\_ID} > PUT /v2/accounts/{ACCOUNT\_ID}/resource\_templates > GET /v2/accounts/{ACCOUNT\_ID}/braintree/cards > DELETE /v2/accounts/{ACCOUNT\_ID}/bulk > GET /v2/shared\_auth > GET /v2/accounts/{ACCOUNT\_ID}/braintree/addresses > GET /v2/accounts/{ACCOUNT\_ID}/queues/stats > GET /v2/accounts/{ACCOUNT\_ID}/cccps/{CCCP\_ID} > DELETE /v2/accounts/{ACCOUNT\_ID}/alerts/{ALERT\_ID} > DELETE /v2/accounts/{ACCOUNT\_ID}/cccps/{CCCP\_ID} > GET /v2/system\_configs > GET /v2/accounts/{ACCOUNT\_ID}/whitelabel > GET /v2/accounts/{ACCOUNT\_ID}/queues/{QUEUE\_ID} > DELETE /v2/accounts/{ACCOUNT\_ID}/whitelabel > GET /v2/accounts/{ACCOUNT\_ID}/rate\_limits > GET /v2/accounts/{ACCOUNT\_ID}/templates > PUT /v2/accounts/{ACCOUNT\_ID}/blacklists > POST /v2/accounts/{ACCOUNT\_ID}/braintree/customer > GET /v2/accounts/{ACCOUNT\_ID}/local\_provisioner\_templates/{TEMPLATE\_ID}/image > PUT /v2/accounts/{ACCOUNT\_ID}/temporal\_rules > GET /v2/accounts/{ACCOUNT\_ID}/freeswitch > GET /v2/accounts/{ACCOUNT\_ID}/faxboxes/{FAXBOX\_ID} > DELETE /v2/accounts/{ACCOUNT\_ID}/global\_provisioner\_templates/{TEMPLATE\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/resource\_selectors > POST /v2/accounts/{ACCOUNT\_ID}/queues/{QUEUE\_ID} > PUT /v2/accounts/{ACCOUNT\_ID}/configs/{CONFIG\_ID} > PUT /v2/accounts/{ACCOUNT\_ID}/queues/eavesdrop > GET /v2/accounts/{ACCOUNT\_ID}/menus/{MENU\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/menus/{MENU\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/braintree/customer > GET /v2/accounts/{ACCOUNT\_ID}/whitelabel/welcome > PUT /v2/accounts/{ACCOUNT\_ID}/braintree/credits > PUT /v2/accounts/{ACCOUNT\_ID}/templates/{TEMPLATE\_NAME} > PATCH /v2/accounts/{ACCOUNT\_ID}/queues/{QUEUE\_ID} > DELETE /v2/accounts/{ACCOUNT\_ID}/storage > DELETE /v2/accounts/{ACCOUNT\_ID}/queues/{QUEUE\_ID}/roster > GET /v2/accounts/{ACCOUNT\_ID}/temporal\_rules\_sets > GET /v2/accounts/{ACCOUNT\_ID}/queues > PUT /v2/accounts/{ACCOUNT\_ID}/storage > DELETE /v2/accounts/{ACCOUNT\_ID}/braintree/addresses/{ADDRESS\_ID} > PUT /v2/accounts/{ACCOUNT\_ID}/cccps > GET /v2/accounts/{ACCOUNT\_ID}/configs/{CONFIG\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/conferences/{CONFERENCE\_ID}/participants/{PARTICIPANT\_ID} > PUT /v2/accounts/{ACCOUNT\_ID}/storage/plans > GET /v2/accounts/{ACCOUNT\_ID}/local\_provisioner\_templates > POST /v2/accounts/{ACCOUNT\_ID}/queues/{QUEUE\_ID}/roster > DELETE /v2/accounts/{ACCOUNT\_ID}/sms/{SMS\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/local\_provisioner\_templates/{TEMPLATE\_ID} > DELETE /v2/system\_configs/{SYSTEM\_CONFIG\_ID}/{NODE} > GET /v2/accounts/{ACCOUNT\_ID}/services/plan > POST /v2/accounts/{ACCOUNT\_ID}/resource\_templates/{RESOURCE\_TEMPLATE\_ID} > PUT /v2/accounts/{ACCOUNT\_ID}/braintree/addresses > POST /v2/accounts/{ACCOUNT\_ID}/blacklists/{BLACKLIST\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/agents/{USER\_ID}/status > GET /v2/accounts/{ACCOUNT\_ID}/resource\_templates > PATCH /v2/accounts/{ACCOUNT\_ID}/connectivity/{CONNECTIVITY\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/storage/plans/{STORAGE\_PLAN\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/whitelabel/icon > GET /v2/accounts/{ACCOUNT\_ID}/agents/{USER\_ID}/queue\_status > PUT /v2/accounts/{ACCOUNT\_ID}/whitelabel > DELETE /v2/accounts/{ACCOUNT\_ID}/storage/plans/{STORAGE\_PLAN\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/temporal\_rules/{TEMPORAL\_RULE\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/storage/plans > GET /v2/accounts/{ACCOUNT\_ID}/braintree/transactions/{TRANSACTION\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/global\_provisioner\_templates/{TEMPLATE\_ID}/image > POST /v2/system\_configs/{SYSTEM\_CONFIG\_ID} > DELETE /v2/accounts/{ACCOUNT\_ID}/resource\_selectors/name/{SELECTOR\_NAME}/resource/{RESOURCE\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/agents/{USER\_ID} > PUT /v2/accounts/{ACCOUNT\_ID}/queues/{QUEUE\_ID}/eavesdrop > POST /v2/accounts/{ACCOUNT\_ID}/signup/{THING} > GET /v2/accounts/{ACCOUNT\_ID}/resource\_selectors/name/{SELECTOR\_NAME}/resource/{RESOURCE\_ID} > GET /v2/sup/{MODULE}/{FUNCTION} > PUT /v2/accounts/{ACCOUNT\_ID}/queues > GET /v2/accounts/{ACCOUNT\_ID}/metaflows > PUT /v2/accounts/{ACCOUNT\_ID}/faxes/inbox/{FAX\_ID} > DELETE /v2/accounts/{ACCOUNT\_ID}/local\_provisioner\_templates/{TEMPLATE\_ID} > DELETE /v2/system\_configs/{SYSTEM\_CONFIG\_ID} > DELETE /v2/token\_auth > DELETE /v2/accounts/{ACCOUNT\_ID}/rate\_limits > PUT /v2/accounts/{ACCOUNT\_ID}/braintree/transactions > GET /v2/accounts/{ACCOUNT\_ID}/agents/stats > POST /v2/accounts/{ACCOUNT\_ID}/storage/plans/{STORAGE\_PLAN\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/connectivity/{CONNECTIVITY\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/faxboxes > GET /v2/accounts/{ACCOUNT\_ID}/global\_provisioner\_templates/{TEMPLATE\_ID} > DELETE /v2/accounts/{ACCOUNT\_ID}/blacklists/{BLACKLIST\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/menus > GET /v2/accounts/{ACCOUNT\_ID}/whitelabel/logo > GET /v2/accounts/{ACCOUNT\_ID}/cccps > GET /v2/accounts/{ACCOUNT\_ID}/agents/status > POST /v2/accounts/{ACCOUNT\_ID}/bulk > POST /v2/accounts/{ACCOUNT\_ID}/service\_plans/synchronization > GET /v2/accounts/{ACCOUNT\_ID}/temporal\_rules\_sets/{TEMPORAL\_RULE\_SET} > GET /v2/accounts/{ACCOUNT\_ID}/whitelabel/icon > DELETE /v2/accounts/{ACCOUNT\_ID}/menus/{MENU\_ID} > PUT /v2/ubiquiti\_auth > PUT /v2/accounts/{ACCOUNT\_ID}/braintree/cards > DELETE /v2/accounts/{ACCOUNT\_ID}/queues/{QUEUE\_ID} > PUT /v2/accounts/{ACCOUNT\_ID}/local\_provisioner\_templates > GET /v2/accounts/{ACCOUNT\_ID}/whitelabel/{WHITELABEL\_DOMAIN}/welcome > GET /v2/accounts/{ACCOUNT\_ID}/global\_provisioner\_templates/{TEMPLATE\_ID}/image > POST /v2/accounts/{ACCOUNT\_ID}/resource\_selectors/resource/{RESOURCE\_ID}/name/{SELECTOR\_NAME} > GET /v2/accounts/{ACCOUNT\_ID}/hotdesks > GET /v2/accounts/{ACCOUNT\_ID}/temporal\_rules > GET /v2/accounts/{ACCOUNT\_ID}/apps\_link/authorize > PATCH /v2/accounts/{ACCOUNT\_ID}/blacklists/{BLACKLIST\_ID} > GET /v2/sup/{MODULE}/{FUNCTION}/{ARGS} > GET /v2/accounts/{ACCOUNT\_ID}/braintree/transactions > GET /v2/accounts/{ACCOUNT\_ID}/temporal\_rules/{TEMPORAL\_RULE\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/configs/{CONFIG\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/global\_provisioner\_templates > PUT /v2/accounts/{ACCOUNT\_ID}/alerts > PUT /v2/accounts/{ACCOUNT\_ID}/sms > GET /v2/about > POST /v2/accounts/{ACCOUNT\_ID}/braintree/cards/{CARD\_ID} > DELETE /v2/accounts/{ACCOUNT\_ID}/metaflows > POST /v2/accounts/{ACCOUNT\_ID}/global\_provisioner\_templates/{TEMPLATE\_ID} > GET /v2/system\_configs/{SYSTEM\_CONFIG\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/local\_provisioner\_templates/{TEMPLATE\_ID}/image > PATCH /v2/accounts/{ACCOUNT\_ID}/menus/{MENU\_ID} > PATCH /v2/accounts/{ACCOUNT\_ID}/faxboxes/{FAXBOX\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/contact\_list > GET /v2/accounts/{ACCOUNT\_ID}/bulk > GET /v2/accounts/{ACCOUNT\_ID}/sms > POST /v2/accounts/{ACCOUNT\_ID}/rate\_limits > DELETE /v2/accounts/{ACCOUNT\_ID}/local\_provisioner\_templates/{TEMPLATE\_ID}/image > GET /v2/accounts/{ACCOUNT\_ID}/resource\_templates/{RESOURCE\_TEMPLATE\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/whitelabel/{WHITELABEL\_DOMAIN}/logo > GET /v2/token\_auth > POST /v2/system\_configs/{SYSTEM\_CONFIG\_ID}/{NODE} > PUT /v2/ip\_auth > POST /v2/accounts/{ACCOUNT\_ID}/agents/{USER\_ID}/status > POST /v2/accounts/{ACCOUNT\_ID}/whitelabel > GET /v2/accounts/{ACCOUNT\_ID}/queues/{QUEUE\_ID}/roster > GET /v2/accounts/{ACCOUNT\_ID}/dialplans > DELETE /v2/accounts/{ACCOUNT\_ID}/templates/{TEMPLATE\_NAME} > DELETE /v2/accounts/{ACCOUNT\_ID}/connectivity/{CONNECTIVITY\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/agents > POST /v2/accounts/{ACCOUNT\_ID}/storage > GET /v2/accounts/{ACCOUNT\_ID}/alerts/{ALERT\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/connectivity > GET /v2/accounts/{ACCOUNT\_ID}/whitelabel/{WHITELABEL\_DOMAIN} > PATCH /v2/accounts/{ACCOUNT\_ID}/resource\_templates/{RESOURCE\_TEMPLATE\_ID} > PUT /v2/accounts/{ACCOUNT\_ID}/signup > GET /v2/accounts/{ACCOUNT\_ID}/presence/report-{REPORT\_ID} > DELETE /v2/accounts/{ACCOUNT\_ID}/temporal\_rules/{TEMPORAL\_RULE\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/faxboxes/{FAXBOX\_ID} > PUT /v2/accounts/{ACCOUNT\_ID}/faxboxes > POST /v2/accounts/{ACCOUNT\_ID}/braintree/addresses/{ADDRESS\_ID} > PUT /v2/shared\_auth > DELETE /v2/accounts/{ACCOUNT\_ID}/braintree/cards/{CARD\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/service\_plans/reconciliation > GET /v2/sup/{MODULE} > PUT /v2/system\_configs > DELETE /v2/accounts/{ACCOUNT\_ID}/temporal\_rules\_sets/{TEMPORAL\_RULE\_SET} > PUT /v2/accounts/{ACCOUNT\_ID}/connectivity > PUT /v2/api\_auth > PATCH /v2/accounts/{ACCOUNT\_ID}/temporal\_rules/{TEMPORAL\_RULE\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/braintree/addresses/{ADDRESS\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/connectivity/{CONNECTIVITY\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/metaflows > DELETE /v2/accounts/{ACCOUNT\_ID}/global\_provisioner\_templates/{TEMPLATE\_ID}/image > GET /v2/accounts/{ACCOUNT\_ID}/braintree/cards/{CARD\_ID} > POST /v2/accounts/{ACCOUNT\_ID}/agents/status/{USER\_ID} > GET /v2/accounts/{ACCOUNT\_ID}/agents/status/{USER\_ID} > PUT /v2/accounts/{ACCOUNT\_ID}/menus

318 / 517 ( 61% documented )

Documented but not matching any actual API endpoint: > GET /v2/channels > GET /v2/notifications > GET /v2/search/multi > POST /v2/resource\_selectors > GET /v2/accounts/{ACCOUNT\_ID}/users/{USER\_ID}/cdrs > GET /v1/accounts > GET /v2/webhooks > GET /v2/accounts/{ACCOUNT\_ID}/users/{USER\_ID}/devices > GET /v2/phone\_numbers > GET /v2/accounts/{ACCOUNT\_ID}/users/{USER\_ID}/channels > GET /v2/tasks > GET /v2/accounts/{ACCOUNT\_ID}/users/{USER\_ID}/recordings > PATCH /v2/accounts/{ACCOUNT\_ID}/descendants/webhooks > GET /v2/accounts/{ACCOUNT\_ID}/descendants/port\_requests > GET /v2/resource\_selectors > GET /v2/websockets > POST /v2/whitelabel/domains > GET /v2/search > GET /v2/accounts/{ACCOUNT\_ID}/about > GET /v2/accounts/{ACCOUNT\_ID}/devices/{DEVICE\_ID}/channels \\#+END\_SRC


## update-the-types.sh

Used to search the code looking for deprecated Erlang functions and types and replace them with the newer versions as appropriate


## validate-js.sh

Processes JSON files:

-   Checks that \_id matches the file name in schema files
-   Checks map functions in CouchDB views for 'Object.keys' usage


## validate-swagger.sh

Validate Swagger file using online validator \\#+BEGIN\_EXAMPLE shell ./scripts/validate-swagger.sh % Total % Received % Xferd Average Speed Time Time Time Current Dload Upload Total Spent Left Speed 100 2973 100 2973 0 0 4945 0 &#x2013;:&#x2013;:&#x2013; &#x2013;:&#x2013;:&#x2013; &#x2013;:&#x2013;:&#x2013; 4938 Swagger file validation errors: 2 { "messages": [ "malformed or unreadable swagger supplied" ], "schemaValidationMessages": [ { "domain": "validation", "instance": { "pointer": "/definitions/allotments" }, "keyword": "additionalProperties", "level": "error", "message": "object instance has properties which are not allowed by the schema: [\\"patternProperties\\"]", "schema": { "loadingURI": "<http://swagger.io/v2/schema.json>#", "pointer": "/definitions/schema" } }, { "domain": "validation", "instance": { "pointer": "/definitions/domain\_hosts" }, "keyword": "additionalProperties", "level": "error", "message": "object instance has properties which are not allowed by the schema: [\\"patternProperties\\"]", "schema": { "loadingURI": "<http://swagger.io/v2/schema.json>#", "pointer": "/definitions/schema" } }, { "domain": "validation", "instance": { "pointer": "/definitions/metaflow" }, "keyword": "additionalProperties", "level": "error", "message": "object instance has properties which are not allowed by the schema: [\\"oneOf\\"]", "schema": { "loadingURI": "<http://swagger.io/v2/schema.json>#", "pointer": "/definitions/schema" } }, { "domain": "validation", "instance": { "pointer": "/definitions/metaflow\_children" }, "keyword": "additionalProperties", "level": "error", "message": "object instance has properties which are not allowed by the schema: [\\"patternProperties\\"]", "schema": { "loadingURI": "<http://swagger.io/v2/schema.json>#", "pointer": "/definitions/schema" } }, { "domain": "validation", "instance": { "pointer": "/definitions/storage" }, "keyword": "additionalProperties", "level": "error", "message": "object instance has properties which are not allowed by the schema: [\\"patternProperties\\"]", "schema": { "loadingURI": "<http://swagger.io/v2/schema.json>#", "pointer": "/definitions/schema" } }, { "domain": "validation", "instance": { "pointer": "/definitions/storage.attachments" }, "keyword": "additionalProperties", "level": "error", "message": "object instance has properties which are not allowed by the schema: [\\"patternProperties\\"]", "schema": { "loadingURI": "<http://swagger.io/v2/schema.json>#", "pointer": "/definitions/schema" } }, { "domain": "validation", "instance": { "pointer": "/definitions/storage.connection.couchdb" }, "keyword": "additionalProperties", "level": "error", "message": "object instance has properties which are not allowed by the schema: [\\"definitions\\"]", "schema": { "loadingURI": "<http://swagger.io/v2/schema.json>#", "pointer": "/definitions/schema" } }, { "domain": "validation", "instance": { "pointer": "/definitions/storage.connections" }, "keyword": "additionalProperties", "level": "error", "message": "object instance has properties which are not allowed by the schema: [\\"patternProperties\\"]", "schema": { "loadingURI": "<http://swagger.io/v2/schema.json>#", "pointer": "/definitions/schema" } }, { "domain": "validation", "instance": { "pointer": "/definitions/storage.plan.database" }, "keyword": "additionalProperties", "level": "error", "message": "object instance has properties which are not allowed by the schema: [\\"definitions\\"]", "schema": { "loadingURI": "<http://swagger.io/v2/schema.json>#", "pointer": "/definitions/schema" } } ] } FIX THESE ISSUES \\#+END\_SRC


## `validate_mkdocs.py`

Parses the mkdocs.yml and looks for non-existent docs


## `wh_to_kz.sh`

Part of the great rename, converts Whistle-related names to Kazoo-specific names
