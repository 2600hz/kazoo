- [Scripts](#orgheadline36)
  - [apps-process-count.sh](#orgheadline1)
  - [bump-copyright-year.sh](#orgheadline2)
  - [check-app-registered.sh](#orgheadline3)
  - [check-dialyzer.escript](#orgheadline4)
  - [check-release-startup.sh](#orgheadline5)
  - [check-xref.escript](#orgheadline6)
  - [code\_checks.bash](#orgheadline7)
  - [conn-to-apps.sh](#orgheadline8)
  - [conn-to-ecallmgr.sh](#orgheadline9)
  - [crash-apps.sh](#orgheadline10)
  - [crash-ecallmgr.sh](#orgheadline11)
  - [dev-exec-mfa.sh](#orgheadline12)
  - [dev-start-apps.sh](#orgheadline13)
  - [dev-start-ecallmgr.sh](#orgheadline14)
  - [dev/kazoo.sh](#orgheadline15)
  - [dev/sup.sh](#orgheadline16)
  - [dialyze-usage.bash](#orgheadline17)
  - [ecallmgr-process-count.sh](#orgheadline18)
  - [format-json.sh](#orgheadline19)
  - [generate-api-endpoints.escript](#orgheadline20)
  - [generate-fs-headers-hrl.escript](#orgheadline21)
  - [generate-schemas.escript](#orgheadline22)
  - [no\_raw\_json.escript](#orgheadline23)
  - [rabbitmq-generic.sh](#orgheadline24)
  - [rabbitmq-server.init](#orgheadline25)
  - [setup-dev.sh](#orgheadline26)
  - [setup-git.sh](#orgheadline27)
  - [src2any.escript](#orgheadline28)
  - [start-apps.sh](#orgheadline29)
  - [start-ecallmgr.sh](#orgheadline30)
  - [state-of-docs.sh](#orgheadline31)
  - [update-the-types.sh](#orgheadline32)
  - [validate-js.sh](#orgheadline33)
  - [validate-swagger.sh](#orgheadline34)
  - [wh\_to\_kz.sh](#orgheadline35)

# Scripts<a id="orgheadline36"></a>

This is the scripts directory, where we place scripts of various types to help with various activities. :)

Let's get a little more concrete though.

## apps-process-count.sh<a id="orgheadline1"></a>

A simple script to query the Erlang VMs process count

    ./scripts/apps-process-count.sh
    10

## bump-copyright-year.sh<a id="orgheadline2"></a>

Python script to walk the supplied files and bumps the copyright year if appropriate.

    ./scripts/bump-copyright-year.sh [FILE]

## check-app-registered.sh<a id="orgheadline3"></a>

Checks Erlang applications for registered processes and compares that to the application's .app.src file.

    ./scripts/check-app-registered.sh [PATH/TO/APP]

For example, I set \`{registered, []} in callflow.app.src, then ran the script:

    ./scripts/check-app-registered.sh applications/callflow
    cf_event_handler_sup, callflow_sup, cf_exe_sup
    applications/callflow has no registered modules??
    1 errors
    1 errors in total

Now you have a listing of registered processes to put in your .app.src

## check-dialyzer.escript<a id="orgheadline4"></a>

An Erlang escript that dialyzes changed files. Run it using the makefile target 'dialyze' with the files to dialyze:

    TO_DIALYZE=applications/callflow/ebin/callflow_sup.beam make dialyze
    scanning "applications/callflow/ebin/callflow_sup.beam"
    0 Dialyzer warnings

Typically \`TO\_DIALYZE\` would be a generated list of files.

Do note: this will only check the file itself for issues. To really leverage Dialyzer, you'll want to include remote project modules for Dialyzer to use as well.

## check-release-startup.sh<a id="orgheadline5"></a>

Creates a release, starts it, and issues some commands to test that the release starts up and appears to be running

## check-xref.escript<a id="orgheadline6"></a>

An Erlang escript for cross referencing (xref) calls to remote modules. Set \`TO\_XREF\` to ebin paths (or use the default):

    make xref
    Pass: global
    Loading modules...
    Running xref analysis...
    Xref: listing undefined_function_calls
    Xref: listing undefined_functions
    Done

If there are any calls to non-existant modules, or non-exported functions, you will get errors listed here.

## code\_checks.bash<a id="orgheadline7"></a>

Checks source code files for various formatting expectations and exits if any are found.

    ./scripts/code_checks.bash applications/crossbar/src/cb_context.erl
    Check for andalso/orelse dropped lines
    Check for uses of module in lieu of ?MODULE
    Check for TAB characters
    Check for trailing whitespaces

## conn-to-apps.sh<a id="orgheadline8"></a>

Opens a remote shell to the kazoo\_apps@hostname VM.

    ./scripts/conn-to-apps.sh [{VM@HOSTNAME}, {LOCAL_SHELL@HOSTNAME}]

## conn-to-ecallmgr.sh<a id="orgheadline9"></a>

A convenience wrapper for connecting to ecallmgr@HOSTNAME via conn-to-apps.sh

## crash-apps.sh<a id="orgheadline10"></a>

Forces the running VM to halt, producing a crashdump, and exiting with status code 1 (as per the [docs](http://erldocs.com/18.0/erts/erlang.html?i=2&search=halt#halt/2)). Currently hard-coded the VM name to 'kazoo\_apps'

## crash-ecallmgr.sh<a id="orgheadline11"></a>

Same as crash-apps.sh but for the ecallmgr VM.

## dev-exec-mfa.sh<a id="orgheadline12"></a>

Runs M:F(A) on the node: \\#+INCLUDE "../dev-exec-mfa.sh" :lines "3-6"

## dev-start-apps.sh<a id="orgheadline13"></a>

Starts a VM with an interactive shell. {VM\_NAME} defaults to 'kazoo\_apps'

    ./scripts/dev-start-apps.sh {VM_NAME}

## dev-start-ecallmgr.sh<a id="orgheadline14"></a>

Defaults node name to 'ecallmgr'; otherwise the same as dev-start-apps.sh

## dev/kazoo.sh<a id="orgheadline15"></a>

When using releases, executes a release command against the running VM:

    KAZOO_CONFIG=/etc/kazoo/core/config.ini ./scripts/dev/kazoo.sh {CMD}

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
-   'upgrade'/'downgrade'/'install': perform an upgrade/downgrade/installation

## dev/sup.sh<a id="orgheadline16"></a>

Runs the SUP escript against the running release

## dialyze-usage.bash<a id="orgheadline17"></a>

Given a module name, such as 'props' or 'kz\_json', search core/applications for modules that make calls to the supplied module and dialyze those beam files looking for dialyzer complaints. You will likely see complaints unrelated to your supplied module - go ahead and fix those too if possilbe ;)

The more heavily utilized the module is, the longer this will take to run!

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

## ecallmgr-process-count.sh<a id="orgheadline18"></a>

Connects to the ecallmgr VM and outputs a count of running Erlang processes.

## format-json.sh<a id="orgheadline19"></a>

Python script to format JSON files (like CouchDB views, JSON schemas) and write the formatted version back to the file. 'make apis' runs this as part of its instructions.

    ./scripts/format-json.sh path/to/file.json [path/to/other/file.json,...]

## generate-api-endpoints.escript<a id="orgheadline20"></a>

Builds the Crossbar reference docs in 'applications/crossbar/doc/ref'. Helps detect when Crossbar endpoints have changes to their functionality that is client-facing.

Also builds the [Swagger](http://swagger.io/) JSON file in applications/crossbar/priv/api/swagger.json

## generate-fs-headers-hrl.escript<a id="orgheadline21"></a>

Parses the ecallmgr code looking for keys used to access values in the FreeSWITCH proplist and builds a header file at applications/ecallmgr/src/fs\_event\_filters.hrl for use when initializing mod\_kazoo.

## generate-schemas.escript<a id="orgheadline22"></a>

Parses the core/applications code looking for calls to kapps\_config (module used to access documents in the system\_config database) and building a base JSON schema file for each document found.

Also parses callflow's action modules looking for keys used to access values in the Data JSON object to build a base JSON schema file for each callflow action.

## no\_raw\_json.escript<a id="orgheadline23"></a>

Erlang has a handful of internal representations of JSON used by the various parses. The kz\_json module handles these details and Kazoo programmers should treat the data structure used as opaque. This script parses the codebase looking for instances where the opaqueness of the data structure is violated.

## rabbitmq-generic.sh<a id="orgheadline24"></a>

Wrapper for running rabbitmq script commands?

## rabbitmq-server.init<a id="orgheadline25"></a>

Init.d script for rabbitmq

## setup-dev.sh<a id="orgheadline26"></a>

Script to setup a dev environment including:

-   Symlink SUP to /usr/bin
-   Symlink rabbitmq init.d script to /etc/init.d
-   Symlink kazoo init.d scripts to /etc/init.d
-   Reset RabbitMQ mnesia databases, logs
-   Setup users for rabbitmq and kazoo

## setup-git.sh<a id="orgheadline27"></a>

Setup the username/email to use in Git commits and other Git settings

## src2any.escript<a id="orgheadline28"></a>

Reads the .app.src file and writes a .src file?

## start-apps.sh<a id="orgheadline29"></a>

Starts a VM in the background with name kazoo\_apps

## start-ecallmgr.sh<a id="orgheadline30"></a>

Starts a VM in the background with name ecallmgr

## state-of-docs.sh<a id="orgheadline31"></a>

Searches for undocumented APIs and reports percentage of doc coverage.

    ./scripts/state-of-docs.sh
    Undocumented API endpoints:
    > PATCH /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/onboard
    > DELETE /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
    > GET /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}
    > GET /v2/accounts/{ACCOUNT_ID}/acls
    > GET /v2/accounts/{ACCOUNT_ID}/blacklists
    > PUT /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets
    > GET /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/braintree/credits
    > PUT /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates
    > POST /v2/accounts/{ACCOUNT_ID}/whitelabel/logo
    > DELETE /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/whitelabel/welcome
    > PATCH /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
    > GET /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates/{TEMPLATE_ID}
    > PUT /v2/google_auth
    > PUT /v2/accounts/{ACCOUNT_ID}/cccps/{CCCP_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}/icon
    > POST /v2/accounts/{ACCOUNT_ID}/cccps/{CCCP_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/alerts
    > POST /v2/accounts/{ACCOUNT_ID}/access_lists
    > GET /v2/accounts/{ACCOUNT_ID}/storage
    > POST /v2/accounts/{ACCOUNT_ID}/presence
    > POST /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
    > GET /v2/accounts/{ACCOUNT_ID}/sms/{SMS_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/access_lists
    > GET /v2/accounts/{ACCOUNT_ID}/access_lists
    > GET /v2/accounts/{ACCOUNT_ID}/resource_selectors
    > POST /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/queue_status
    > POST /v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/resource_templates
    > GET /v2/accounts/{ACCOUNT_ID}/braintree/cards
    > DELETE /v2/accounts/{ACCOUNT_ID}/bulk
    > GET /v2/shared_auth
    > GET /v2/accounts/{ACCOUNT_ID}/braintree/addresses
    > GET /v2/accounts/{ACCOUNT_ID}/queues/stats
    > GET /v2/accounts/{ACCOUNT_ID}/cccps/{CCCP_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/alerts/{ALERT_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/cccps/{CCCP_ID}
    > GET /v2/system_configs
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel
    > GET /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/whitelabel
    > GET /v2/accounts/{ACCOUNT_ID}/rate_limits
    > GET /v2/accounts/{ACCOUNT_ID}/templates
    > PUT /v2/accounts/{ACCOUNT_ID}/blacklists
    > POST /v2/accounts/{ACCOUNT_ID}/braintree/customer
    > GET /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates/{TEMPLATE_ID}/image
    > PUT /v2/accounts/{ACCOUNT_ID}/temporal_rules
    > GET /v2/accounts/{ACCOUNT_ID}/freeswitch
    > GET /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates/{TEMPLATE_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/resource_selectors
    > POST /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/queues/eavesdrop
    > GET /v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/braintree/customer
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/welcome
    > PUT /v2/accounts/{ACCOUNT_ID}/braintree/credits
    > PUT /v2/accounts/{ACCOUNT_ID}/templates/{TEMPLATE_NAME}
    > PATCH /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/storage
    > DELETE /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster
    > GET /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets
    > GET /v2/accounts/{ACCOUNT_ID}/queues
    > PUT /v2/accounts/{ACCOUNT_ID}/storage
    > DELETE /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/cccps
    > GET /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants/{PARTICIPANT_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/storage/plans
    > GET /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates
    > POST /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster
    > DELETE /v2/accounts/{ACCOUNT_ID}/sms/{SMS_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates/{TEMPLATE_ID}
    > DELETE /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}
    > GET /v2/accounts/{ACCOUNT_ID}/services/plan
    > POST /v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/braintree/addresses
    > POST /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/status
    > GET /v2/accounts/{ACCOUNT_ID}/resource_templates
    > PATCH /v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/whitelabel/icon
    > GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/queue_status
    > PUT /v2/accounts/{ACCOUNT_ID}/whitelabel
    > DELETE /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/storage/plans
    > GET /v2/accounts/{ACCOUNT_ID}/braintree/transactions/{TRANSACTION_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates/{TEMPLATE_ID}/image
    > POST /v2/system_configs/{SYSTEM_CONFIG_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/eavesdrop
    > POST /v2/accounts/{ACCOUNT_ID}/signup/{THING}
    > GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}
    > GET /v2/sup/{MODULE}/{FUNCTION}
    > PUT /v2/accounts/{ACCOUNT_ID}/queues
    > GET /v2/accounts/{ACCOUNT_ID}/metaflows
    > PUT /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates/{TEMPLATE_ID}
    > DELETE /v2/system_configs/{SYSTEM_CONFIG_ID}
    > DELETE /v2/token_auth
    > DELETE /v2/accounts/{ACCOUNT_ID}/rate_limits
    > PUT /v2/accounts/{ACCOUNT_ID}/braintree/transactions
    > GET /v2/accounts/{ACCOUNT_ID}/agents/stats
    > POST /v2/accounts/{ACCOUNT_ID}/storage/plans/{STORAGE_PLAN_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/faxboxes
    > GET /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates/{TEMPLATE_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/menus
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/logo
    > GET /v2/accounts/{ACCOUNT_ID}/cccps
    > GET /v2/accounts/{ACCOUNT_ID}/agents/status
    > POST /v2/accounts/{ACCOUNT_ID}/bulk
    > POST /v2/accounts/{ACCOUNT_ID}/service_plans/synchronization
    > GET /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/icon
    > DELETE /v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}
    > PUT /v2/ubiquiti_auth
    > PUT /v2/accounts/{ACCOUNT_ID}/braintree/cards
    > DELETE /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}/welcome
    > GET /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates/{TEMPLATE_ID}/image
    > POST /v2/accounts/{ACCOUNT_ID}/killio/call
    > POST /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/name/{SELECTOR_NAME}
    > GET /v2/accounts/{ACCOUNT_ID}/hotdesks
    > GET /v2/accounts/{ACCOUNT_ID}/temporal_rules
    > GET /v2/accounts/{ACCOUNT_ID}/apps_link/authorize
    > PATCH /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
    > GET /v2/sup/{MODULE}/{FUNCTION}/{ARGS}
    > GET /v2/accounts/{ACCOUNT_ID}/braintree/transactions
    > GET /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates
    > PUT /v2/accounts/{ACCOUNT_ID}/alerts
    > PUT /v2/accounts/{ACCOUNT_ID}/sms
    > GET /v2/about
    > POST /v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/metaflows
    > POST /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates/{TEMPLATE_ID}
    > GET /v2/system_configs/{SYSTEM_CONFIG_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates/{TEMPLATE_ID}/image
    > PATCH /v2/accounts/{ACCOUNT_ID}/menus/{MENU_ID}
    > PATCH /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/contact_list
    > GET /v2/accounts/{ACCOUNT_ID}/bulk
    > GET /v2/accounts/{ACCOUNT_ID}/sms
    > POST /v2/accounts/{ACCOUNT_ID}/rate_limits
    > DELETE /v2/accounts/{ACCOUNT_ID}/local_provisioner_templates/{TEMPLATE_ID}/image
    > GET /v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}/logo
    > GET /v2/token_auth
    > POST /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}
    > PUT /v2/ip_auth
    > POST /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/status
    > POST /v2/accounts/{ACCOUNT_ID}/whitelabel
    > GET /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster
    > GET /v2/accounts/{ACCOUNT_ID}/dialplans
    > DELETE /v2/accounts/{ACCOUNT_ID}/templates/{TEMPLATE_NAME}
    > DELETE /v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/agents
    > POST /v2/accounts/{ACCOUNT_ID}/storage
    > PUT /v2/accounts/{ACCOUNT_ID}/killio/call
    > GET /v2/accounts/{ACCOUNT_ID}/alerts/{ALERT_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/connectivity
    > GET /v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}
    > PATCH /v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/signup
    > GET /v2/accounts/{ACCOUNT_ID}/presence/report-{REPORT_ID}
    > DELETE /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/faxboxes
    > POST /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}
    > PUT /v2/shared_auth
    > DELETE /v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/service_plans/reconciliation
    > GET /v2/sup/{MODULE}
    > PUT /v2/system_configs
    > DELETE /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
    > PUT /v2/accounts/{ACCOUNT_ID}/connectivity
    > PUT /v2/api_auth
    > PATCH /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/metaflows
    > DELETE /v2/accounts/{ACCOUNT_ID}/global_provisioner_templates/{TEMPLATE_ID}/image
    > GET /v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}
    > POST /v2/accounts/{ACCOUNT_ID}/agents/status/{USER_ID}
    > GET /v2/accounts/{ACCOUNT_ID}/agents/status/{USER_ID}
    > PUT /v2/accounts/{ACCOUNT_ID}/menus

    318 / 517 ( 61% documented )

    Documented but not matching any actual API endpoint:
    > GET /v2/channels
    > GET /v2/notifications
    > GET /v2/search/multi
    > POST /v2/resource_selectors
    > GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/cdrs
    > GET /v1/accounts
    > GET /v2/webhooks
    > GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/devices
    > GET /v2/phone_numbers
    > GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/channels
    > GET /v2/tasks
    > GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/recordings
    > PATCH /v2/accounts/{ACCOUNT_ID}/descendants/webhooks
    > GET /v2/accounts/{ACCOUNT_ID}/descendants/port_requests
    > GET /v2/resource_selectors
    > GET /v2/websockets
    > POST /v2/whitelabel/domains
    > GET /v2/search
    > GET /v2/accounts/{ACCOUNT_ID}/about
    > GET /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/channels

## update-the-types.sh<a id="orgheadline32"></a>

Used to search the code looking for deprecated Erlang functions and types and replace them with the newer versions as appropriate

## validate-js.sh<a id="orgheadline33"></a>

Processes JSON files:

-   Checks that \_id matches the file name in schema files
-   Checks map functions in CouchDB views for 'Object.keys' usage

## validate-swagger.sh<a id="orgheadline34"></a>

Validate Swagger file using online validator

    ./scripts/validate-swagger.sh
      % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                     Dload  Upload   Total   Spent    Left  Speed
    100  2973  100  2973    0     0   4945      0 --:--:-- --:--:-- --:--:--  4938
    Swagger file validation errors: 2
    {
        "messages": [
            "malformed or unreadable swagger supplied"
        ],
        "schemaValidationMessages": [
            {
                "domain": "validation",
                "instance": {
                    "pointer": "/definitions/allotments"
                },
                "keyword": "additionalProperties",
                "level": "error",
                "message": "object instance has properties which are not allowed by the schema: [\"patternProperties\"]",
                "schema": {
                    "loadingURI": "http://swagger.io/v2/schema.json#",
                    "pointer": "/definitions/schema"
                }
            },
            {
                "domain": "validation",
                "instance": {
                    "pointer": "/definitions/domain_hosts"
                },
                "keyword": "additionalProperties",
                "level": "error",
                "message": "object instance has properties which are not allowed by the schema: [\"patternProperties\"]",
                "schema": {
                    "loadingURI": "http://swagger.io/v2/schema.json#",
                    "pointer": "/definitions/schema"
                }
            },
            {
                "domain": "validation",
                "instance": {
                    "pointer": "/definitions/metaflow"
                },
                "keyword": "additionalProperties",
                "level": "error",
                "message": "object instance has properties which are not allowed by the schema: [\"oneOf\"]",
                "schema": {
                    "loadingURI": "http://swagger.io/v2/schema.json#",
                    "pointer": "/definitions/schema"
                }
            },
            {
                "domain": "validation",
                "instance": {
                    "pointer": "/definitions/metaflow_children"
                },
                "keyword": "additionalProperties",
                "level": "error",
                "message": "object instance has properties which are not allowed by the schema: [\"patternProperties\"]",
                "schema": {
                    "loadingURI": "http://swagger.io/v2/schema.json#",
                    "pointer": "/definitions/schema"
                }
            },
            {
                "domain": "validation",
                "instance": {
                    "pointer": "/definitions/storage"
                },
                "keyword": "additionalProperties",
                "level": "error",
                "message": "object instance has properties which are not allowed by the schema: [\"patternProperties\"]",
                "schema": {
                    "loadingURI": "http://swagger.io/v2/schema.json#",
                    "pointer": "/definitions/schema"
                }
            },
            {
                "domain": "validation",
                "instance": {
                    "pointer": "/definitions/storage.attachments"
                },
                "keyword": "additionalProperties",
                "level": "error",
                "message": "object instance has properties which are not allowed by the schema: [\"patternProperties\"]",
                "schema": {
                    "loadingURI": "http://swagger.io/v2/schema.json#",
                    "pointer": "/definitions/schema"
                }
            },
            {
                "domain": "validation",
                "instance": {
                    "pointer": "/definitions/storage.connection.couchdb"
                },
                "keyword": "additionalProperties",
                "level": "error",
                "message": "object instance has properties which are not allowed by the schema: [\"definitions\"]",
                "schema": {
                    "loadingURI": "http://swagger.io/v2/schema.json#",
                    "pointer": "/definitions/schema"
                }
            },
            {
                "domain": "validation",
                "instance": {
                    "pointer": "/definitions/storage.connections"
                },
                "keyword": "additionalProperties",
                "level": "error",
                "message": "object instance has properties which are not allowed by the schema: [\"patternProperties\"]",
                "schema": {
                    "loadingURI": "http://swagger.io/v2/schema.json#",
                    "pointer": "/definitions/schema"
                }
            },
            {
                "domain": "validation",
                "instance": {
                    "pointer": "/definitions/storage.plan.database"
                },
                "keyword": "additionalProperties",
                "level": "error",
                "message": "object instance has properties which are not allowed by the schema: [\"definitions\"]",
                "schema": {
                    "loadingURI": "http://swagger.io/v2/schema.json#",
                    "pointer": "/definitions/schema"
                }
            }
        ]
    }
    FIX THESE ISSUES

## wh\_to\_kz.sh<a id="orgheadline35"></a>

Part of the great rename, converts Whistle-related names to Kazoo-specific names