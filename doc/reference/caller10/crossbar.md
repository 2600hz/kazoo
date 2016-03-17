### Crossbar

If this is your first time encountering Crossbar, you may want to read over the [basic overview](https://github.com/2600hz/kazoo/blob/slate/applications/crossbar/doc/basics.md) to learn about how URIs are chunked and routed to various endpoint modules.

#### Anatomy of a Request

Imagine an engine turning a gear, which turns a gear, which turns still more gears. Crossbar functions similarly. At the root, the [Cowboy HTTP server](http://ninenines.eu/docs/en/cowboy/HEAD/guide/introduction/) is the engine, driving the interaction between client and server, request and response. It, in turn, spins the [REST handler](http://ninenines.eu/docs/en/cowboy/HEAD/guide/rest_handlers/) gear. This then powers Crossbar's [api_resource](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/api_resource.erl) gear, which fans execution out to a wide range of gears, depending on the request made. Follow so far? Now, the gears up to and including api_resource.erl are there to make it easier for developing and modifying resource endpoints for use in Crossbar. We'll quickly look at the major "gears" you can hook your resource module onto and then build the "contests" resource module for a practical example of the basic [CRUD](https://en.wikipedia.org/wiki/CRUD) operations needed.

#### Resource Gears

* Authentication
  The authentication (or authn) gear asks if the client is who they say they are. [cb_token_auth](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/modules/cb_token_auth.erl#L91-119) (session token module) provides a good example of authenticating the client.
* Authorization
  The authorization (or authz) gear asks if the client is allowed to access the resource. There are some [system-wide resources](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/modules/cb_simple_authz.erl#L24-29) that only the top-level account should access, and obviously access to accounts a requestor does not belong to should not be allowed. See the [cb_simple_authz.erl](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/modules/cb_simple_authz.erl#L41-L67) module for an example.
* Allowed Methods
  This gear asks, given the path tokens that are passed to this module, what HTTP methods (GET, PUT, POST, etc) are allowed. For basic CRUD, for instance, a request with no extra data (/contests for instance) will typically allow GET and PUT, while a request with an ID (/contests/contest_id) will allow GET, POST, and DELETE. [Skeleton example](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/modules/cb_skels.erl#L98-L103).
* Resource Exists
  This gear asks, does this request point to a resource that should exist? Taking the basic CRUD, we would expect /contests and /contests/contest_id to exist, but would not expect /contests/contest_id/start_time to exist. Now, whether contest_id actually exists is another matter, handled later in the validate stage. [Skeleton example](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/modules/cb_skels.erl#L114-L117).
* Content Types Provided and Accepted
  By default, Crossbar accepts and provides JSON-formatted data payloads. There are times, however, where Crossbar accepts alternative data formats (like uploading music on hold files), and where Crossbar will send alternative data formats back to the client (downloading a voicemail).
  Again, based on the path tokens passed in, you can alter the content types provided and accepted. Read more about [HTTP Request headers](https://en.wikipedia.org/wiki/List_of_HTTP_headers#Requests) and why client applications should set them properly.
  * Content Types Provided
  This listing will match against the client's Accept header. If missing, Crossbar will assume "application/json". If the client wants to receive audio/mp3 (a voicemail, say), it will set the header "Accept: audio/mp3" in the HTTP request. Your module will then need to allow that content type (and any others your module will provide) in the `content_types_provided` function(s). See the [cb_media module](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/modules/cb_media.erl#L157-L179) where it determines the Content-Type of the requested media file and sets the `content_types_provided` accordingly.
  The other parameter in the `content_types_provided` format (`to_json`, `to_binary`, etc) are the encoders for the response. See the [api_resource](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/api_resource.erl#L646) module for the available encoders. You can also supply your own `to_*` functions: (bind for)[https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/modules/cb_cdrs.erl#L96] and (output)[https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/modules/cb_cdrs.erl#L100-L107] the format requested.
  * Content Types Accepted
  This listing will match against the client's Content-Type header. If missing, Crossbar will assume "application/json". If the client is providing a request other than a JSON payload, it must set the Content-Type header appropriately. This is typically a more static listing of MIME types, as seen again in the cb_media module.
  Similar to the content types provided, content types accepted requires another parameter, the decoding function ("from_json", "from_binary", etc). See the api_resource module for the available decoders.
Languages Provided
If you plan to provide more than just English responses (and we hope our international community does!), you can define what languages your module will provide (matched against a request's "Accept-Language" header). Skeleton example.
Charsets Provided
If you have alternative character sets you'd like to provide (like UTF-8 or UTF-16), set them here. They will be matched against the request's "Accept-Charset" (otherwise the first in the list will be used). Skeleton example.
Encodings Provided
If you provide alternative encodings, maybe a different compression engine, set the encodings your module will provide here. They will be matched against the request's "Accept-Encoding". Read more about HTTP compression. Skeleton example of setting encodings.
Validate
The validation gear allows the resource module to check the request's data payload to ensure it meets the appropriate criteria. Typically on PUT and POST the resource module will use the JSON schema of the resource to validate the request's payload to see if it conforms to the schema. For GET requests, the resource module will either do a lookup for a listing of all resource instances, or check whether the provided ID exists (similarly for DELETE). Generally speaking, any data on the payload not validated by the schema is automatically stored (with the exception of private fields, covered later) should the saving of the payload be successful (meaning Crossbar does not strip non-private fields it can't validate). The skeleton example shows how, based on the path tokens provided and the HTTP method used, different validation routines are invoked.
Another task of the validate gear is to ready the request to be executed (loading the appropriate data into the Context record).
Billing
If your module will invoke a paid-feature and you need to communicate that to your backend billing system, this gear is where you'll tie that in. See, for instance, billing for devices.
Execute
The execute gear takes the data loaded during the previous stages (most notably the validate stage) and executes the HTTP verb. In typical CRUD, the PUT would save a new document, a POST would update and existing document, and a DELETE would remove an existing document. GETs don't have an execute stage as it is expected that the validate stage has loaded the data necessary for creating a response. You can see in the skeleton example how you define methods based on the HTTP verbs.
Etag
Set the Etag header. This is automatically handled (if possible, and usually done for you - see crossbar_doc), otherwise left blank. Skeleton example.
Expires
Set the time for when the resource(s) returned will expire (the client should consider the response stale after this time). Skeleton example.
Finish Request
This gear is available after the response is on its way to the client, allowing the resource module to do any cleanup or updating necessary. Skeleton example.
Creating the contests resource
Now that we've seen an overview of the available gears we can hook our resource into, let's see about actually building the contests resource.
The first thing to do is copy the cb_skels.erl and rename it to cb_contests.erl (remember, by convention we use the plural of the resource name for the module name).
2600hz/kazoo$ cd applications/crossbar/
2600hz/kazoo/applications/crossbar$ cp src/modules/cb_skels.erl src/modules/cb_contests.erl
Next, search/replace skels with contests, then skel with contest:
2600hz/kazoo/applications/crossbar$ sed -i 's/skels/contests/g' src/modules/cb_contests.erl
2600hz/kazoo/applications/crossbar$ sed -i 's/skel/contest/g' src/modules/cb_contests.erl
Run make and the module should compile successfully (there should be a cb_contests.beam in the ebin/ folder).
2600hz/kazoo/applications/crossbar$ make
...make output...
2600hz/kazoo/applications/crossbar$ ls ebin/cb_contests.beam
ebin/cb_contests.beam
Congrats, you've just created your first CRUD-y resource! Let's load the beam into the running VM and start the endpoint.
2600hz/kazoo$ sup whistle_maintenance hotload cb_contests
ok
2600hz/kazoo$ sup crossbar_maintenance start_module cb_contests
ok
2600hz/kazoo$ sup crossbar_maintenance running_modules
[cb_about,...,cb_contests,...] # note: your output may vary here, just verify cb_contests is in the list
cb_contests in detail
First, some cleanup
A lot of the functions in cb_contests just set the defaults. Most can be removed without loss of functionality. The typical ones to not include are:
authenticate
authorize
content_types_provided
content_types_accepted
languages_provided
charsets_provided
encodings_provided
billing
etag
expires
finish_request
Remove the exports that correlate to these events, as well as the function definitions later in the module.
init/0
The init function serves to register your module to the crossbar_bindings server. The bindings server allows a module to register functions to be called when a particular routing key is fired. The routing keys allowed are nearly identical to what RabbitMQ/AMQP allow; however, in practice Crossbar modules typically only use the '*' wildcard segment or explicit segment matching.
See below for more about routing keys.
The init/0 is only run when the module is loaded into the running Crossbar system (at boot, or via crossbar_maintenance:start_module/1). It can also be used to initialize databases or other initialization tasks.
Be sure to remove the bind calls to the events removed above.
allowed_methods/{0,1}
We define two versions of the allowed_methods, one to handle /contests (that is, "contests" with no path tokens), and one to handle /contests/contest_id (that is, "contests" with one token, "contest_id").
/contests is accessible via an HTTP GET (listing of contests) or PUT (create a new contest).
/contests/contest_id is accessible via GET (details of contest), POST (edit contest), or DELETE (remove contest).
resource_exists/{0,1}
We have already determined that /contests and /contests/contest_id are existing resource paths, so return true for both cases.
validate/{1, 2}
Now things change a bit. The first argument to these functions will be the cb_context record associated with the request. Any subsequent arguments will be the path tokens provided in the request.
/contests will result in validate(Context) being called.
/contests/contest_id will result in validate(Context, ContestId) being called.
validate/{1,2} uses the HTTP verb to determine what action to take (summary, create, read, update).
put/1
Because a PUT is only valid for /contests, we only need to define an arity-1 version (with the cb_context as the argument). Since validate/1 has been successful, we know that the to-be-created contest is loaded up in the cb_context record and ready for saving, so the only thing needing doing is calling crossbar_doc:save(Context). This will save the contest to the account database and load the saved version (with auto-generated id and rev) into the cb_context record, ready to build the response.
post/2
Because a POST is only valid for /contests/contest_id, post(Context, ContestId) is called. Similarly to put/1, because the validate/2 took care of loading the existing contest doc, merging the request's contest payload, and validating the result, we know Context is ready for saving; post/2 just needs to execute the save.
delete/2
The validate/2 counterpart will have tried to read in the contest identified; if request execution reaches this function, Context is loaded with the doc to "delete". It is important to remember that deletes via crossbar_doc:delete/1 are "soft-deletes", so-called because they actually only set a private field indicating the document is no longer accessible.
create/1
This function will validate that the request data conforms to the JSON schema. cb_context:validate_request_data/3 takes the schema name, the Context, and a callback function to run if the request data passes validation.
read/2
This function will attempt to load the document with id Id into the cb_context record.
update/2
Similar to create/1, this will first validate the request data and if successful, will merge the request data with the existing document (done in the callback function).
summary/1
By convention, resources have a design document in the account databases named after the resource, with, at a minimum, a view named "crossbar_listing". This view will list existing resource documents with the id as the key, and the value as the summary version of the resource. Once loaded, each view result will be mapped over using the supplied callback (normalize_view_results/2 in this case).
on_successful_validation/2
Here we see what happens when the request data is successfully verified. If a PUT (or new resource creation), the Id is undefined, and the first clause will update the to-be-saved document with the pvt_type field (and others if desired).
If the request is a POST (or updating an existing document), the crossbar_doc:load_merge/2 will handle loading the current version of the resource, and merging the new values from the request.
normalize_view_results/2
Run on each result from a view lookup. JObj will an element from the view listing, while Acc will be the list of JSON objects returned by the normalizer. In this instance, the function merely prepends the value of the "value" key (another JSON object typically) to the accumulator.
The Context record
Crossbar threads a record throughout the request, typically bound to the Context variable, and should provide you with all the information you will need to process a given request. Refer to the cb_context.erl module for the various accessor functions.
Other important Crossbar modules
crossbar_doc.erl
This is a wrapper around the whistle_couch library, and provides a simplified interface for getting documents, view results, attachments, etc. The module also takes care of updating the cb_context record along the way.
crossbar_util.erl
When you need to set custom responses, crossbar_util has the helpers you need to set the response code, response headers, response body, etc.
crossbar_bindings.erl
The bindings server, routing the various stages of a request to the interested modules.
bind/3 binds a Module/Function pair to a key (similar to AMQP's binding keys). The arity of the Module:Function called will depend on the number of path tokens associated with the resource requested.
map/2 takes a routing key and a payload and maps the payload over any matching Module/Function pairs bound to a matching binding key. You will get a list of each matching Module/Function's responses.
fold/2 takes a routing key and a payload, and folds the payload through the matching Module/Function pairs (meaning if the first matching pair modifies the payload, the new payload is passed along to the next matching pair). You will get the final version of the payload back.
There are also some pretty nifty PropEr tests in this module, to help with the fuzzy testing of the binding/routing engine, if you're into that kind of stuff.
crossbar_cleanup.erl
The cleanup server, responsible for general DB maintenance. It cleans up soft-deleted documents, heard voicemails past a certain age, and more. It also provides bindings for your module to hook into should you want to do periodic maintenance tasks too. For instance, if you have an hourly maintenance task you could do the following in your init/0:
cb_my_module.erl
-module(cb_my_module).
-export([init/0
         ,hourly_cleanup/0
                 ]).

                  init() ->
                      crossbar_bindings:bind(crossbar_cleanup:binding_hour(), 'cb_my_module', 'hourly_cleanup').

                       hourly_cleanup() ->
                           lager:debug("hey, has it been an hour already?").
                           Every hour, your hourly_cleanup/0 will be called (as long as it is properly exported, of course!).
                           You can also bind for the periodic DB cleanup events. Crossbar classifies databases in one of four ways: account, account_mod, system, and other. If you bind for one or more database types, the arity-1 version of your Module/Function pair will be called, with the database name as the only argument.
                           init() ->
                               crossbar_bindings:bind(crossbar_cleanup:binding_account(), 'cb_my_module', 'account_db_cleanup').

                                account_db_cleanup(AccountDb) ->
                                    lager:debug("periodic cleanup of ~s commencing", [AccountDb]).
                                    Resource Schemas
                                    The JSON schemas allow Crossbar to validate the fields that Crossbar (and Kazoo in general) care about. All other non-private fields are stored and supplied on request unmodified. If a backend application is to use a field in a document, it should be part of the JSON schema (never trust user input!).
                                    Crossbar has a host of schemas you can use as reference. The easiest way to load your own schema is to put the resources.json into the schemas folder and run 'sup crossbar_maintenance refresh_schemas'. It is recommended that, if you need to update a schema, you do so on the disk version and run the refresh routine again.
                                    Make sure you have the contests.json schema from the beginning, save it into the schemas directory as contests.json, and run the refresh command. Now navigate to your Futon interface for the system_schemas and check that the contests schema is properly loaded.
                                    Core applications used
                                    See the core utils page for more, but the primary utilities used in Crossbar are:
                                    wh_json/props
                                    wh_util
                                    kz_buckets
                                    Building the view
                                    The last bit left is to create the design document for the account to make querying the list of contests do-able. First, the design document:
                                    applications/crossbar/priv/couchdb/account/contests.json
                                    {
                                        "_id": "_design/contests"
                                            ,"language": "javascript"
                                                ,"views": {
                                                        "crossbar_listing": {
                                                                    "map": "function(doc) { if (doc.pvt_type != 'contest' || doc.pvt_deleted) return; emit(doc._id, {'id': doc._id, 'name':doc.name}); }"
                                                                            }
                                                                                }
                                                                                }
                                                                                Save this document to $KAZOO/applications/crossbar/priv/couchdb/account/ as contests.json.
                                                                                Now we need to load the view into the account databases.
                                                                                2600hz/kazoo$ sup whapps_maintenance refresh_account_dbs
                                                                                This could take a while. Verify the view has been loaded in account database by navigating to http://localhost:5984/_utils/database.html?{account_db}/_design/contests/_view/crossbar_listing
                                                                                Now we're ready to test the API!
                                                                                Let's play with the new API
                                                                                First, generate an authentication token to start playing with the API! See the Generating an Authentication Token for how to do that. We will use placeholders of {account_id} and {auth_token} in the cURL examples. Please substitute your appropriate values! On to the cURL!
                                                                                # optional step
                                                                                alias pp="python -mjson.tool"

                                                                                 # Fetch all contests
                                                                                 2600hz/kazoo$ curl -v -X GET -H "X-Auth-Token: {auth_token}" http://thinky64.2600hz.com:8000/v1/accounts/{account_id}/contests | pp
                                                                                 ... Request/Response headers ...
                                                                                 {
                                                                                     "auth_token": "{auth_token}",
                                                                                         "data": [],
                                                                                             "request_id": "b0b95aa6edc1649687f5818046b70ae6",
                                                                                                 "revision": "undefined",
                                                                                                     "status": "success"
                                                                                                     }
                                                                                                     As you can see from the "data" portion, there are no contests in this account yet. Let's remedy that.
                                                                                                     # Create a basic contest
                                                                                                     # Starting at 9am Feb 26th, using the erlang shell
                                                                                                     # erl> calendar:datetime_to_gregorian_seconds({{2014,2,26},{9,0,0}}).
                                                                                                     # 63560624400

                                                                                                     2600hz/kazoo$ curl -v -X PUT -H "X-Auth-Token: {auth_token}" -H "Content-Type: application/json" http://thinky64.2600hz.com:8000/v1/accounts/{account_id}/contests -d '{"data":{"name":"First Contest", "start_time":63560624400}}' | pp
                                                                                                     ... Request/Response headers ...
                                                                                                     {
                                                                                                         "auth_token": "{auth_token}",
                                                                                                             "data": {
                                                                                                                     "winning_caller_number": {
                                                                                                                                 "required": "Field is required but missing"
                                                                                                                                         }
                                                                                                                                             },
                                                                                                                                                 "error": "400",
                                                                                                                                                     "message": "invalid data",
                                                                                                                                                         "request_id": "fc52be00acea348d7f6f4f161d444211",
                                                                                                                                                             "status": "error"
                                                                                                                                                             }
                                                                                                                                                             Oops! We forgot a field, winning_caller_number, from the payload. Let's try again.
                                                                                                                                                             2600hz/kazoo$ curl -v -X PUT -H "X-Auth-Token: {auth_token}" -H "Content-Type: application/json" http://thinky64.2600hz.com:8000/v1/accounts/{account_id}/contests -d '{"data":{"name":"First Contest", "start_time":63560624400, "winning_caller_number":9}}' | pp
                                                                                                                                                             ... Request/Response headers ...
                                                                                                                                                             {
                                                                                                                                                                 "auth_token": "{auth_token}",
                                                                                                                                                                     "data": {
                                                                                                                                                                             "id": "7e5c820c59b1c8c8e20f36e3d34f5581",
                                                                                                                                                                                     "name": "First Contest",
                                                                                                                                                                                             "start_time": 63560624400,
                                                                                                                                                                                                     "winning_caller_number": 9
                                                                                                                                                                                                         },
                                                                                                                                                                                                             "request_id": "d87d774f25d1f19d9ebb5948871bcd98",
                                                                                                                                                                                                                 "revision": "1-ad92a1b4feb1acfbb6768a7e2f8e4c9e",
                                                                                                                                                                                                                     "status": "success"
                                                                                                                                                                                                                     }
                                                                                                                                                                                                                     Nice. Take note of the "id" field (there's also a Location header in the response with the full URI for the resource).
                                                                                                                                                                                                                     It is also important to note the "request_id" (see as well the X-Request-ID response header); use that value to grep the Kazoo logs (typically /var/log/2600hz-platform.log) to see the decision-making used during the request.
                                                                                                                                                                                                                     Let's verify both that we can access the contest directly and that it shows up in the summary view now.
                                                                                                                                                                                                                     2600hz/kazoo$ curl -v -X GET -H "X-Auth-Token: 1499eb50d6a0e039907866b6ef08bdc0" http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/contests/7e5c820c59b1c8c8e20f36e3d34f5581 | pp
                                                                                                                                                                                                                     ... Request/Response headers ...
                                                                                                                                                                                                                     {
                                                                                                                                                                                                                         "auth_token": "{auth_token}",
                                                                                                                                                                                                                             "data": {
                                                                                                                                                                                                                                     "id": "7e5c820c59b1c8c8e20f36e3d34f5581",
                                                                                                                                                                                                                                             "name": "First Contest",
                                                                                                                                                                                                                                                     "start_time": 63560624400,
                                                                                                                                                                                                                                                             "winning_caller_number": 9
                                                                                                                                                                                                                                                                 },
                                                                                                                                                                                                                                                                     "request_id": "4477b35dce603b48eb51d8a3539e5b60",
                                                                                                                                                                                                                                                                         "revision": "1-ad92a1b4feb1acfbb6768a7e2f8e4c9e",
                                                                                                                                                                                                                                                                             "status": "success"
                                                                                                                                                                                                                                                                             }
                                                                                                                                                                                                                                                                             2600hz/kazoo$ curl -v -X GET -H "X-Auth-Token: {auth_token}" http://thinky64.2600hz.com:8000/v1/accounts/{account_id}/contests | pp
                                                                                                                                                                                                                                                                             ... Request/Response headers ...
                                                                                                                                                                                                                                                                             {
                                                                                                                                                                                                                                                                                 "auth_token": "{auth_token}",
                                                                                                                                                                                                                                                                                     "data": [
                                                                                                                                                                                                                                                                                             {
                                                                                                                                                                                                                                                                                                         "id": "7e5c820c59b1c8c8e20f36e3d34f5581",
                                                                                                                                                                                                                                                                                                                     "name": "First Contest"
                                                                                                                                                                                                                                                                                                                             }
                                                                                                                                                                                                                                                                                                                                 ],
                                                                                                                                                                                                                                                                                                                                     "request_id": "85065f0a3ab22e7fc9cc1319adb17d94",
                                                                                                                                                                                                                                                                                                                                         "revision": "da2074c5274616f384b631f724b6fb18",
                                                                                                                                                                                                                                                                                                                                             "status": "success"
                                                                                                                                                                                                                                                                                                                                             }
                                                                                                                                                                                                                                                                                                                                             You should be able to perform all the necessary CRUD operations now. Congratulations, you've built a Crossbar module, loaded it into a running system, built a view, and successfully used the new resource API via cURL.
                                                                                                                                                                                                                                                                                                                                             Miscellaneous topics
                                                                                                                                                                                                                                                                                                                                             Securing with SSL or use HAProxy instead
                                                                                                                                                                                                                                                                                                                                             Account hierarchy
                                                                                                                                                                                                                                                                                                                                             Maintenance module
                                                                                                                                                                                                                                                                                                                                             Doc change AMQP events
                                                                                                                                                                                                                                                                                                                                             OAuth ???
                                                                                                                                                                                                                                                                                                                                             Others ???
