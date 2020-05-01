# Functions

## About Functions

Functions enable dynamic call processing based on customized serverless function and callflow document as input.

Pivot allows you to receive a per-call HTTP request to decide what to do, generally to access data sources not available to KAZOO. However, if your business logic is self-contained and side-effect free (and not directly representable via the callflow JSON structure), functions allow you to create that custom logic and have it run within the KAZOO infrastructure. This minimizes the time spent processing the function (as it is run "locally") and doesn't require you to have to stand up separate infrastructure to handle the HTTP request and generate the JSON response.

#### Schema

Functions that javascript function to process a callflow document.



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`enabled` | Is the function enabled and running | `boolean()` | `true` | `false` | `supported`
`function_js` | Serverless function in Javascript | `string()` |   | `true` | `supported`
`language_pack` | The language and version are required for the function runtime. | `string()` | `javascript` | `false` | `supported`
`name` | A descriptive name for the function. | `string()` |   | `true` | `supported`



## User-defined Functions

Effectively, KAZOO gives you `function(call) {...}` and asks you to supply the `...`. You have the `call` object containing the same [request parameters](applications/pivot/doc/requests/) as you would in Pivot (such as `call['To']` or `call['Call-ID']`).

### Example 1: Say Hi!

```json
{"name":"say hi"
 ,"function_js":"return {'module':'tts', 'data':{'text':'Thanks for calling us ' + call['Caller-ID-Name']}};"
}
```

All calls to this function would then run the TTS engine to say hi to the caller using the supplied Caller ID Name.

### Example 2: Dates

```json
{"name":"fun day"
 ,"function_js":"var d = new Date(); var days = ['Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday']; return {'module':'tts', 'data':{'text':days[d.getDay()] + ' fun day!'}};"
}
```

### Javascript functionality available

As of now, [ECMA-262 5th edition (“ES5”) of the language](https://docs.couchdb.org/en/stable/best-practices/jsdevel.html), is supported.

## Fetch available functions on the system

Depending on the version of the KAZOO system running, the available functions may differ. Use this API to query the system for available functions.

> GET /v2/functions

```shell
curl -v -X GET \
    -H "Content-Type:application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN} \
    http://{SERVER}:8000/v2/functions
```

## Get sample payloads of all function events

> GET /v2/functions/samples

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/functions/samples
```

### Example

**Request:**

```shell
curl -H 'Content-Type: application/json' 'http://{SERVER}:8000/v2/functions/samples'
```
## List functions

> GET /v2/accounts/{ACCOUNT_ID}/functions

Any functions with *disable_reason* in the summary has been auto-disabled.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions
```

## Create function

> PUT /v2/accounts/{ACCOUNT_ID}/functions

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {
      "name":"Say hi!"
      ,"function_js":"return {"module":"tts", "data":{"text": "Thanks for calling " + call['To']}};"
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions
```

## Get details of the function

> GET /v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}
```

## Update the function

> POST /v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}
```


## Patch function

> PATCH /v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}

You can also patch an existing function:

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"enabled":true}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}
```

## Delete a function

> DELETE /v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}
```


## Administration

### Enabling the API endpoint

To add the module to your auto-started list of API endpoints:

```shell
sup crossbar_maintenance start_module cb_functions
```

If you just want to play with it on a local node but not persist the endpoint on restart:

```shell
sup cb_functions init
```

### CouchDB

We leverage CouchDB's [show function](http://guide.couchdb.org/editions/1/en/show.html) to provide hosting and processing of the user's Javascript function.

The client will create a "function" document containing the Javascript function as a string. The `show` function [`functions/wh`](https://github.com/2600hz/kazoo/blob/master/applications/functions/priv/couchdb/views/functionsdb-functions.json) will be run with the "function"'s doc id and the call will be placed in the request's query string parameters.

The `wh` show function will create a new function (using Javascript's `new Function()`) from the stringified version on the doc and pass it the query string object from the `req` argument. The user's function should return the new flow JSON (as you would using Pivot).

#### Work around to enable `new Function()` in CouchDB

Javascript's `new Function()`, which is similar to `eval`, is used to parse a piece of Javascript code in a string into a function object, which is invoked with the call object to return the new flow JSON. It is considered more secure than `eval` since the `new Function()` code runs in a separate scope.

However, both `eval` and `new Function()` are blocked by Content Security Policy in CouchDB (see this [commit](https://github.com/apache/couchdb/commit/1cd5852a758bf79c1860dae8d69e103d2e0611a4). A workaround has to be applied to enable `new Function()`.

##### CouchDB 2.3
Make sure `--eval` is explicitly added under query_servers of etc/local.ini.

```ini
[query_servers]
COUCHDB_QUERY_SERVER_JAVASCRIPT="<absolute path>/bin/couchjs --eval <absolute path>/share/server/main.js"
```

If CouchDB is started by dev/run from the CouchDB repo, please update `dev/run` with
```
qs_javascript = toposixpath("%s --eval %s" % (couchjs, mainjs))
```

##### CouchDB 2.2 or earlier versions or BigCouch

Make sure `--eval` is explicitly added under query_servers of etc/local.ini.

```
[query_servers]
javascript = "<absolute path>/bin/couchjs --eval <absolute path>/share/server/main.js"
```

##### Security

It is recommended that you do not run the `eval`/`new Function()` enabled CouchDB instance in your main DB cluster.

Fortunately, KAZOO allows you to define alternate CouchDB connections for specific databases or database classifications. Since all `function` documents are stored in the `functions` aggregate database, you can create a connection just for the `functions` database that points to a entirely separated CouchDB server or cluster:

Create a 32-char UUID: `> UUID = kz_binary:rand_hex(16).` => `"9ee5860b4d717907dfc63274af0d06e9"`

Create the connection and storage plan:

```json
{
  "connections": {
    "{UUID}": {
      "driver": "kazoo_couch",
      "name": "functions-connection",
      "settings": {
        "ip": "localhost",
        "port": 55984
      }
    }
  },
  "plan": {
    "aggregate": {
      "connection": "{UUID}",
      "database": {
        "names": [
          "functions"
        ]
      },
      "types": {
        "function": {
          "connection": "{UUID}"
        }
      }
    }
  }
}
```

Apply that to the global storage plan:

```shell
curl -v -X PATCH -H "X-Auth-Token: {ADMIN_AUTH_TOKEN}" http://localhost:8000/v2/storage \
-d '{"plan":{"aggregate":{"connection":"{UUID}","database":{"names":["functions"]},"types":{"function":{"connection":"{UUID}"}}}},"connections":{"{UUID}":{"driver":"kazoo_couch","name":"functions-connection","settings":{"ip":"localhost","port":55984}}}}'
```

## Engineering

### Other design options

Besides CouchDB's show function, we could directly communicate with `couchjs` that is CouchDB's external Javascript engine. The benefit is only to expose the `eval` risk to `Functions` instead of all `views` and `show` functions in CouchDB.

[Erlang_V8](https://github.com/strange/erlang_v8) has been toyed with to accommodate Javascript.

In the long term, to support a range of languages, we might look into Docker based web server to bootstrap functions implemented by the language of choice.
