%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_skels).

-export([init/0
         ,authenticate/1
         ,authorize/1
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,content_types_provided/1
         ,content_types_accepted/1
         ,languages_provided/1
         ,charsets_provided/1
         ,encodings_provided/1
         ,validate/1, validate/2
         ,billing/1
         ,get/1, get/2
         ,put/1, put/2
         ,post/1, post/2
         ,delete/1, delete/2
         ,etag/1
         ,expires/1
         ,finish_request/1
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"skels/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.skels">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.skels">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.skels">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.skels">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.languages_provided.skels">>, ?MODULE, 'languages_provided'),
    _ = crossbar_bindings:bind(<<"*.charsets_provided.skels">>, ?MODULE, 'charsets_provided'),
    _ = crossbar_bindings:bind(<<"*.encodings_provided.skels">>, ?MODULE, 'encodings_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.skels">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.billing">>, ?MODULE, 'billing'),
    _ = crossbar_bindings:bind(<<"*.execute.get.skels">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.skels">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.skels">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.skels">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"*.etag.skels">>, ?MODULE, 'etag'),
    _ = crossbar_bindings:bind(<<"*.expires.skels">>, ?MODULE, 'expires'),
    _ = crossbar_bindings:bind(<<"*.finish_request">>, ?MODULE, 'finish_request').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'false'.
authenticate(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> 'false'.
authorize(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods() | [].
-spec allowed_methods(path_token()) -> http_methods() | [].
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /skels => []
%%    /skels/foo => [<<"foo">>]
%%    /skels/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be using to respond (matched against
%% client's accept header)
%% Of the form {atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(#cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be requiring (matched to the client's
%% Content-Type header
%% Of the form {atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(cb_context:context()) -> cb_context:context().
content_types_accepted(#cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you provide alternative languages, return a list of languages and optional
%% quality value:
%% [<<"en">>, <<"en-gb;q=0.7">>, <<"da;q=0.5">>]
%% @end
%%--------------------------------------------------------------------
-spec languages_provided(cb_context:context()) -> cb_context:context().
languages_provided(#cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you provide alternative charsets, return a list of charsets and optional
%% quality value:
%% [<<"iso-8859-5">>, <<"unicode-1-1;q=0.8">>]
%% @end
%%--------------------------------------------------------------------
-spec charsets_provided(cb_context:context()) -> cb_context:context().
charsets_provided(#cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you provide alternative encodings, return a list of encodings and optional
%% quality value:
%% [<<"gzip;q=1.0">>, <<"identity;q=0.5">>, <<"*;q=0">>]
%% @end
%%--------------------------------------------------------------------
-spec encodings_provided(cb_context:context()) -> cb_context:context().
encodings_provided(#cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /skels mights load a list of skel objects
%% /skels/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    create(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    update(Id, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id) ->
    read(Id, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you handle billing-related calls, this callback will allow you to
%% execute those.
%% @end
%%--------------------------------------------------------------------
-spec billing(cb_context:context()) -> cb_context:context().
billing(#cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is a GET, execute necessary code to fulfill the GET
%% request. Generally, this will involve stripping pvt fields and loading
%% the resource into the resp_data, resp_headers, etc...
%% @end
%%--------------------------------------------------------------------
-spec get(cb_context:context()) -> cb_context:context().
-spec get(cb_context:context(), path_token()) -> cb_context:context().
get(#cb_context{}=Context) ->
    Context.
get(#cb_context{}=Context, _) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(#cb_context{}=Context) ->
    crossbar_doc:save(Context).
put(#cb_context{}=Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(#cb_context{}=Context) ->
    crossbar_doc:save(Context).
post(#cb_context{}=Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context()) -> cb_context:context().
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(#cb_context{}=Context) ->
    crossbar_doc:delete(Context).
delete(#cb_context{}=Context, _) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you want to manipulate the etag header, change it here in the cb_context{}
%% @end
%%--------------------------------------------------------------------
-spec etag(cb_context:context()) -> cb_context:context().
etag(#cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the expires header
%% @end
%%--------------------------------------------------------------------
-spec expires(cb_context:context()) -> cb_context:context().
expires(#cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% The response has gone out, do some cleanup of your own here.
%% @end
%%--------------------------------------------------------------------
-spec finish_request(cb_context:context()) -> cb_context:context().
finish_request(#cb_context{}=Context) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(#cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"skels">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, #cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"skels">>, Context, OnSuccess).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', #cb_context{doc=JObj}=Context) ->
    Context#cb_context{doc=wh_json:set_value(<<"pvt_type">>, <<"skel">>, JObj)};
on_successful_validation(Id, #cb_context{}=Context) ->
    crossbar_doc:load_merge(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
