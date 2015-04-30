/*
Section: Crossbar
Title: AAA
Language: en-US
*/

A AAA is a module to support settings for AAA servers and a set of dictionaries for them.

## AAA document structure

The structure of AAA configuration document is next:

    {
        "aaa_mode":"on",
        "servers":[
            {
                "enabled":true
                "name":"server1",
                "address":"127.0.0.1",
                "port":1812,
                "secret":"secret1",
                "aaa_engine":"radius",
                "dicts":["dictionary_3gpp", "dictionary"],
                "avp":"strict",
                "retries":3,
                "timeout":5000
            },
            {
                "enabled":true
                "name":"server2",
                "address":"127.0.0.1",
                "port":1812,
                "secret":"secret2",
                "aaa_engine":"radius",
                "dicts":["dictionary_3gpp", "dictionary", "dictionary1"],
                "avp":"strict",
                "retries":5,
                "timeout":10000
            },
            ...
            ],
        "authentication":["server1", "server2"],
        "authorization":["server1", "server2"],
        "accounting":["server3", "server4"]
    }

### Fields list:

* `aaa_mode`: A mode used for this entity (an account or default system configuration).
    1. `on` - an request to an AAA-server is disabled for this entity
    2. `off` - an request to an AAA-server is enabled for this entity according settings in the AAA document for the entity
    3. `inherit` - an request to an AAA-server is enabled for this entity according settings in the AAA document for the entity. If no AAA document for the entity then an AAA document for parent entity will be used
* `servers`: A list of AAA servers will be used for an related operations with this entity.
* `servers -> enabled`:
    1. `true` - this server will be used for AAA operations.
    2. `false` - this server won't be used for AAA operations.
* `servers -> name`: An unique server name (used in the document as its ID).
* `servers -> address`: An address of the server.
* `servers -> port`: An port of the server.
* `servers -> secret`: An secret value (in terms of the RADIUS protocol) for this server.
* `servers -> aaa_engine`:
    1. `radius` - this server is RADIUS server.
    2. `diameter` - this server is DIAMETER server.
* `servers -> dicts`: A server names list will be used in AAA operations. All names in the list should exist.
* `servers -> avp`:
    1. `strict` - strict check of an AVP (Attribute-Value Pair) in an dictionary.
    2. `custom` - any AVPs are allowed.
* `servers -> retries`: A number of retries when make a request to the server.
* `servers -> timeout`: A timeout value (in milliseconds) when make a request to the server.
* `servers -> authentication`: A list of servers name will be used for all authentication operations.
* `servers -> authorization`: A list of servers name will be used for all authorization operations.
* `servers -> accounting`: A list of servers name will be used for all accounting operations.

## Dictionary document structure

The structure of AAA configuration document is next:

    {
       "name": "dictionary_juniper",
       "owner": "system_config",
       "value": [
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
       ],
       "pvt_type": "aaa_dict"
    }

## Usage in crossbar

Using Crossbar to work with AAA documents and AAA dictionaries is very simple:

## For the AAA documents:

* GET - Gets the current AAA document.
* PUT - Sets the AAA document.
* PATCH - Updates the AAA document partially.

### Account AAA settings URI

`/v2/accounts/{ACCOUNT_ID}/aaa`

#### GET - Gets the current AAA document:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa

#### PUT - Updates the AAA document:

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa -d '{"data":{"aaa_mode":"off","servers":{"enabled":false,"name":"unique_server_name","address":"127.0.0.1","port":1812,"secret":"example_secret","aaa_engine":"radius","dicts":["dictionary_3gpp","dictionary"],"avp":"strict","retries":3,"timeout":5000},"authentication":["unique_server_name"],"authorization":["unique_server_name"],"accounting":["unique_server_name"]}}'

#### PATCH - Updates the AAA document partially:

    curl -v -X PATCH -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa  -d '<dicts data>'


## For the AAA dictionaries:

* GET - Gets the current dictionary(s).
* PUT - Add an dictionary.
* POST - Updates an dictionary.
* DELETE - Removes an dictionary.

### Account AAA dictionaries URI

`/v2/accounts/{ACCOUNT_ID}/aaa/dict`

#### GET - Fetch an account dictionaries:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa/dict

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa/dict/{ID}

#### PUT - Add an account dictionary:

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa/dict -d '<dicts data>'

#### POST - Update an account dictionary:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa/dict/{ID} -d '<dicts data>'

#### DELETE - Remove an account dictionary:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/aaa/dict/{ID}
