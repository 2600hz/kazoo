
/*
Section: Crossbar
Title: AAA
Language: en-US
*/

A AAA is a module to support settings for AAA servers and a set of dictionaries for them.

## AAA document structure

AAA configuration document can be stored as part of an account or in the system configuration database.

An example AAA configuration document (mostly self-explaining):

    {
        "aaa_mode": "on",
        "servers": [
            {
                "enabled": true,
                "name": "server1",
                "address": "127.0.0.1",
                "port": 1812,
                "secret": "secret1",
                "aaa_engine": "radius",
                "dicts": ["dictionary_3gpp", "dictionary"],
                "avp": "strict",
                "retries": 3,
                "timeout": 5000
            },
            {
                "enabled": false,
                "name": "server2",
                "address": "127.0.0.1",
                "port": 1812,
                "secret": "secret2",
                "aaa_engine": "radius",
                "dicts": ["dictionary_3gpp", "dictionary", "dictionary1"],
                "avp": "strict",
                "retries": 5,
                "timeout": 10000
            },
            {
                "enabled": true,
                "name": "server3",
                "address": "127.0.0.1",
                "port": 1813,
                "secret": "secret3",
                "aaa_engine": "diameter",
                "dicts": ["dictionary", "dictionary2"],
                "avp": "strict",
                "retries": 2,
                "timeout": 20000
            },
            {
                "enabled": false,
                "name": "server4",
                "address": "127.0.0.1",
                "port": 1813,
                "secret": "secret4",
                "aaa_engine": "diameter",
                "dicts": ["dictionary_3gpp", "dictionary", "dictionary2a"],
                "avp": "custom",
                "retries": 3,
                "timeout": 3000
            }
        ],
        "authentication": ["server1", "server2"],
        "authorization": ["server1", "server2"],
        "accounting": ["server3", "server4"],
        "workers": 5,
        "nas_address": "127.0.0.1",
        "nas_port": "2000",
        "authz_apps": [
            "circlemaker",
            "jonny5"
        ],
        "authn_avp_translation": [
            {
                "attribute": "User-Name",
                "request_key": "User-Name",
                "request_value_regexp": "^(.*)$",
                "attr_value_regexp": "^(.*)$"
            },
            {
                "attribute": "User-Password",
                "request_key": "User-Password",
                "request_value_regexp": "^(.*)$",
                "attr_value_regexp": "^(.*)$"
            }
        ],
        "authz_avp_translation": [
            {
                "attribute": "User-Name",
                "request_key": "User-Name",
                "request_value_regexp": "^(.*)$",
                "attr_value_regexp": "^(.*)$"
            },
            {
                "attribute": "User-Password",
                "request_key": "User-Password",
                "request_value_regexp": "^(.*)$",
                "attr_value_regexp": "^(.*)$"
            }
        ],
        "accounting_avp_translation": [
            {
                "attribute": "User-Name",
                "request_key": "User-Name",
                "request_value_regexp": "^(.*)$",
                "attr_value_regexp": "^(.*)$"
            },
            {
                "attribute": "User-Password",
                "request_key": "User-Password",
                "request_value_regexp": "^(.*)$",
                "attr_value_regexp": "^(.*)$"
            },
            {
                "cast": "string_to_integer",
                "attribute": "Acct-Delay-Time",
                "request_key": "Acct-Delay-Time",
                "request_value_regexp": "^(.*)$",
                "attr_value_regexp": "^(.*)$"
            },
            {
                "cast": "string_to_integer",
                "attribute": "Acct-Session-Time",
                "request_key": "Billing-Seconds",
                "request_value_regexp": "^(.*)$",
                "attr_value_regexp": "^(.*)$"
            }
        ],
        "block_accounting": [
            {
                "channel": [
                "inbound",
                "internal"
                ]
            }
        ],
        "block_authz": [
            {
                "channel": [
                  "inbound",
                  "external"
                ]
            },
            {
                "channel": [
                  "inbound",
                  "internal"
                ]
            }
        ]
    }

An example AAA configuration document with 'inherit' configuration for an account which uses AAA configuration of the parent account:

    {
        "aaa_mode": "inherit",
        "servers": [],
        "authentication": [],
        "authorization": [],
        "accounting": [],
        "workers": 5,
        "nas_address": "127.0.0.1",
        "nas_port": "2000",
        "authz_apps": [],
        "authz_avp_translation": [],
        "authn_avp_translation": [],
        "accounting_avp_translation": []
    }

### Fields list:

* `aaa_mode`: A mode used for this entity (an account or default system configuration). Required.
    1. `off` - an request to an AAA-server is disabled for this entity
    2. `on` - an request to an AAA-server is enabled for this entity according settings in the AAA document for the entity
    3. `inherit` - an request to an AAA-server is enabled for this entity according settings in the AAA document for the entity. If no AAA servers for the entity (the `servers` list entry is empty), then an AAA document for parent entity will be used.
* `servers`: A list of AAA servers will be used for an related operations with this entity. Required.
* `servers -> enabled`: Required.
    1. `true` - this server will be used for AAA operations.
    2. `false` - this server won't be used for AAA operations.
* `servers -> name`: An unique server name (used in the document as its ID). Required.
* `servers -> address`: An address of the server. Required.
* `servers -> port`: An port of the server. Required.
* `servers -> secret`: An secret value (in terms of the RADIUS protocol) for this server. Required.
* `servers -> aaa_engine`: Required.
    1. `radius` - this server is RADIUS server.
    2. `diameter` - this server is DIAMETER server ( **not implemented yet** ).
* `servers -> dicts`: Dictionaries list will be used in AAA operations. All dictionaries in the list should be existing. Required.
* `servers -> avp`:
    1. `strict` - strict check of an AVP (Attribute-Value Pair) in an dictionary. Only AVP existing in dictionaries will be sent to AAA-servers.
    2. `custom` - any AVPs are allowed without any checks ( **not implemented yet** ).
* `servers -> retries`: A number of retries when make a request to the server.
* `servers -> timeout`: A timeout value (in milliseconds) when make a request to the server.
* `authentication`: A list of servers name will be used for all authentication operations. Required.
* `authorization`: A list of servers name will be used for all authorization operations. Required.
* `accounting`: A list of servers name will be used for all accounting operations. Required.
* `workers`: A number of worker threads which will process requests simultaneously. Required.
* `nas_address`: NAS address for this account. Required.
* `nas_port`: NAS port for this account. Required.
* `authz_apps`: List of the applications will be used as authz providers (e.g. jonny5, circlemaker). Required.
* `authn_avp_translation`: This section describes translation from internal authn message representation to RADIUS message. Required.
* `authn_avp_translation -> cast`: Key used in a situation when need to convert value type of an internal message to another type of the a RADIUS request.
    1. `string_to_integer` - need to convert string value of an internal message into integer value of the a RADIUS request.
* `authn_avp_translation -> request_key`: A key name of the the internal authn message. This key name will be translated to corresponding attribute name on a request processing. Required.
* `authn_avp_translation -> attribute`: An attribute name in the dictionary. This attribute name will be translated to corresponding key name on a response processing. Required.
* `authn_avp_translation -> request_value_regexp`: Regular expression used for extracting needed part of the request value. Required part of the value can be extracted using round brackets (regex group). Required.
* `authn_avp_translation -> attr_value_regexp`: Regular expression used for extracting needed part of the response AVP value. Required part of the value can be extracted using round brackets (regex group). Required.
* `authz_avp_translation`: This section describes translation from internal authz message representation to RADIUS message. Required.
* `authz_avp_translation -> request_key`: A key name of the the internal authz message. This key name will be translated to corresponding attribute name on a request processing. Required.
* `authz_avp_translation -> attribute`: An attribute name in the dictionary. This attribute name will be translated to corresponding key name on a response processing. Required.
* `authz_avp_translation -> request_value_regexp`: Regular expression used for extracting needed part of the request value. Required part of the value can be extracted using round brackets (regex group). Required.
* `authz_avp_translation -> attr_value_regexp`: Regular expression used for extracting needed part of the response AVP value. Required part of the value can be extracted using round brackets (regex group). Required.
* `accounting_avp_translation`: This section describes translation from internal accounting message representation to RADIUS message. Required.
* `accounting_avp_translation -> request_key`: A key name of the the internal accounting message. This key name will be translated to corresponding attribute name on a request processing. Required.
* `accounting_avp_translation -> attribute`: An attribute name in the dictionary. This attribute name will be translated to corresponding key name on a response processing. Required.
* `accounting_avp_translation -> request_value_regexp`: Regular expression used for extracting needed part of the request value. Required part of the value can be extracted using round brackets (regex group). Required.
* `accounting_avp_translation -> attr_value_regexp`: Regular expression used for extracting needed part of the response AVP value. Required part of the value can be extracted using round brackets (regex group). Required.
* `block_accounting`: Section responsible for avoiding accounting operations via Circlemaker, so no requests will be sent to RADIUS server.
* `block_accounting -> channel`: Entry described type of channel which will be blocked
    1.1. `inbound` - inbound channel
    1.2. `outbound` - outbound channel
    2.1. `internal` - internal channel
    2.2. `external` - external channel
* `block_authz`: Section responsible for avoiding authz operations via Circlemaker. All authz operations of the blocked types will be bypassed and authorized without request to RADIUS server.
* `block_authz-> channel`: Entry described type of channel which will be blocked
    1.1. `inbound` - inbound channel
    1.2. `outbound` - outbound channel
    2.1. `internal` - internal channel
    2.2. `external` - external channel

## Dictionary document structure

Example dictionary document:

    {
       "name": "dictionary_juniper",
       "owner": "5157b7a3b843668a6baf83f6fa5e0bf4",
       "value": [
           {
               "val": {
                   "vid": 2636,
                   "id": 5,
                   "val": 1,
                   "name": "Juniper_Deny_Configuration"
               }
           },
           {
               "attr": {
                   "vid": 2636,
                   "id": 5,
                   "type": "string",
                   "name": "Juniper_Deny_Configuration",
                   "enc": "no"
               }
           },
           {
               "attr": {
                   "vid": 2636,
                   "id": 4,
                   "type": "string",
                   "name": "Juniper_Allow_Configuration",
                   "enc": "no"
               }
           },
           {
               "attr": {
                   "vid": 2636,
                   "id": 3,
                   "type": "string",
                   "name": "Juniper_Deny_Commands",
                   "enc": "no"
               }
           },
           {
               "attr": {
                   "vid": 2636,
                   "id": 2,
                   "type": "string",
                   "name": "Juniper_Allow_Commands",
                   "enc": "no"
               }
           },
           {
               "attr": {
                   "vid": 2636,
                   "id": 1,
                   "type": "string",
                   "name": "Juniper_Local_User_Name",
                   "enc": "no"
               }
           },
           {
               "vendor": {
                   "Juniper": 2636
               }
           }
       ]
    }

### Fields list:

* `name`: A dictionary name.
* `owner`: An account ID of the dictionary's owner.
* `value`: List of the dictionary entries.
* `servers -> vendor`: Vendor description entry. It's a JSON pair of the vendor's name as string and vendor's ID as integer.
* `servers -> attr`: Attribute entry
* `servers -> attr -> vid`: Vendor ID of the attribute
* `servers -> attr -> id`: ID of the attribute
* `servers -> attr -> type`: Type of the attribute
* `servers -> attr -> name`: Name of the attribute
* `servers -> attr -> enc`: If this attribute value is encoded
* `servers -> val`: Value entry
* `servers -> val -> vid`: Vendor ID of the value
* `servers -> val -> id`: ID of the value
* `servers -> val -> val`: Value as integer
* `servers -> val -> name`: Value description

All dictionaries are stored in the aaa_dicts database.

For a server there is a record like:

    "dicts": ["dictionary2a", "dictionary", "dictionary_3gpp"]

Search for the first entry of the AVP works in this list from left to right.

## Usage in crossbar

Using Crossbar to work with AAA documents and AAA dictionaries is very simple:

## API for the account's AAA document:

* GET - Gets the current AAA document.
* PUT - Create the AAA document.
* POST - Updates the AAA document.
* PATCH - Updates the AAA document partially.

### Account's AAA settings URI

`/v2/accounts/{ACCOUNT_ID}/aaa`

#### GET - Gets the current AAA document (error if no AAA docs for this account):

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa

#### PUT - Creates the AAA document:

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa -d '{"data":{"aaa_mode":"on","servers":[{"enabled":true,"name":"server1","address":"127.0.0.1","port":1812,"secret":"secret1","aaa_engine":"radius","dicts":["dictionary_3gpp", "dictionary"],"avp":"strict","retries":3,"timeout":5000}],"authentication":[],"authorization":["server1"],"accounting":[],"workers":5,"nas_address":"127.0.0.1","nas_port":"2000","authz_apps": ["circlemaker","jonny5"],"authz_avp_translation": [{"attribute": "User-Name","request_key": "User-Name","request_value_regexp": "^(.*)$","attr_value_regexp": "^(.*)$"},{"attribute": "User-Password","request_key": "User-Password","request_value_regexp": "^(.*)$","attr_value_regexp": "^(.*)$"}]}}'

#### POST - Updates the AAA document:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa -d '{"data":{"aaa_mode":"on","servers":[{"enabled":true,"name":"server1","address":"127.0.0.1","port":1812,"secret":"secret1","aaa_engine":"radius","dicts":["dictionary_3gpp", "dictionary"],"avp":"strict","retries":3,"timeout":5000}],"authentication":[],"authorization":["server1"],"accounting":[],"workers":5,"nas_address":"127.0.0.1","nas_port":"2000","authz_apps": ["circlemaker","jonny5"],"authz_avp_translation": [{"attribute": "User-Name","request_key": "User-Name","request_value_regexp": "^(.*)$","attr_value_regexp": "^(.*)$"},{"attribute": "User-Password","request_key": "User-Password","request_value_regexp": "^(.*)$","attr_value_regexp": "^(.*)$"}]}}'

#### PATCH - Updates the AAA document partially:

    curl -v -X PATCH -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa -d '{"data":{"aaa_mode":"off","authorization":["server1"],"workers":10}}'

## API for the system configuration's AAA document:

* GET - Gets the current AAA document.
* PUT - Create the AAA document.
* POST - Updates the AAA document.
* PATCH - Updates the AAA document partially.

### System configuration's AAA settings URI

`/v1/system_configs/circlemaker`

#### GET - Gets the current AAA document (error if no AAA docs for this account):

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/system_configs/circlemaker

#### PUT - Creates the AAA document:

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v1/system_configs/circlemaker -d '{"data":{"aaa_mode":"on","servers":[{"enabled":true,"name":"server1","address":"127.0.0.1","port":1812,"secret":"secret1","aaa_engine":"radius","dicts":["dictionary_3gpp", "dictionary"],"avp":"strict","retries":3,"timeout":5000}],"authentication":[],"authorization":["server1"],"accounting":[],"workers":5,"nas_address":"127.0.0.1","nas_port":"2000","authz_apps": ["circlemaker","jonny5"],"authz_avp_translation": [{"attribute": "User-Name","request_key": "User-Name","request_value_regexp": "^(.*)$","attr_value_regexp": "^(.*)$"},{"attribute": "User-Password","request_key": "User-Password","request_value_regexp": "^(.*)$","attr_value_regexp": "^(.*)$"}]}}'

#### POST - Updates the AAA document:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v1/system_configs/circlemaker -d '{"data":{"aaa_mode":"on","servers":[{"enabled":true,"name":"server1","address":"127.0.0.1","port":1812,"secret":"secret1","aaa_engine":"radius","dicts":["dictionary_3gpp", "dictionary"],"avp":"strict","retries":3,"timeout":5000}],"authentication":[],"authorization":["server1"],"accounting":[],"workers":5,"nas_address":"127.0.0.1","nas_port":"2000","authz_apps": ["circlemaker","jonny5"],"authz_avp_translation": [{"attribute": "User-Name","request_key": "User-Name","request_value_regexp": "^(.*)$","attr_value_regexp": "^(.*)$"},{"attribute": "User-Password","request_key": "User-Password","request_value_regexp": "^(.*)$","attr_value_regexp": "^(.*)$"}]}}'

#### PATCH - Updates the AAA document partially:

    curl -v -X PATCH -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/system_configs/circlemaker -d '{"data":{"aaa_mode":"off","authorization":["server1"],"workers":10}}'

## Managing AAA dictionaries:

* GET - Gets the current dictionary(s).
* PUT - Add an dictionary.
* POST - Updates an dictionary.
* DELETE - Removes an dictionary.

### AAA dictionaries URI

`/v2/accounts/{ACCOUNT_ID}/aaa/dictionary`

#### GET - Fetch all dictionaries:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa/dictionary

#### GET - Fetch an single dictionary:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa/dictionary/{ID}

#### PUT - Add an account dictionary:

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa/dictionary -d '{"data":{"name": "dictionary_juniper","owner": "5157b7a3b843668a6baf83f6fa5e0bf4","value": [{"attr": {"vid": 2636,"id": 5,"type": "string","name": "Juniper_Deny_Configuration","enc": "no"}},{"vendor": {"Juniper": 2636}}]}}'

#### POST - Update an account dictionary:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa/dictionary/{ID} -d '{"data":{"name": "dictionary_juniper","owner": "5157b7a3b843668a6baf83f6fa5e0bf4","value": [{"attr": {"vid": 2636,"id": 5,"type": "string","name": "Juniper_Deny_Configuration","enc": "yes"}},{"vendor": {"Juniper": 1111}}]}}'

#### DELETE - Remove an account dictionary:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa/dictionary/{ID}

Dictionaries of the system configuration can't be changed using API. They loaded once on start and after this they can't be changed.

After any changes in the aaa_dicts database (PUT/POST/DELETE methods) the circlemaker app should be restarted using the following command:

    sup whapps_controller restart_app circlemaker

This action is needed for reloading all dictionaries to the memory. All dictionaries are stored in memory to provide fast fetch of an AVPs.
