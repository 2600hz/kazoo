%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Crossnar API for skel.
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2
        ,etag/1
        ,expires/1
        ,finish_request/1
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"skels/crossbar_listing">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
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
    _ = crossbar_bindings:bind(<<"*.execute.patch.skels">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.skels">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"*.etag.skels">>, ?MODULE, 'etag'),
    _ = crossbar_bindings:bind(<<"*.expires.skels">>, ?MODULE, 'expires'),
    _ = crossbar_bindings:bind(<<"*.finish_request">>, ?MODULE, 'finish_request').

%%------------------------------------------------------------------------------
%% @doc Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'false'.
authenticate(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) -> 'false'.
authorize(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_Thing) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /skels => []
%%    /skels/foo => [<<"foo">>]
%%    /skels/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc What content-types will the module be using to respond (matched against
%% client's accept header).
%% Of the form `{atom(), [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}'
%% @end
%%------------------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc What content-types will the module be requiring (matched to the client's
%% Content-Type header.
%% Of the form `{atom(), [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}'
%% @end
%%------------------------------------------------------------------------------
-spec content_types_accepted(cb_context:context()) -> cb_context:context().
content_types_accepted(Context) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc If you provide alternative languages, return a list of languages and optional
%% quality value.
%%
%% e.g.: `[<<"en">>, <<"en-gb;q=0.7">>, <<"da;q=0.5">>]'
%% @end
%%------------------------------------------------------------------------------
-spec languages_provided(cb_context:context()) -> cb_context:context().
languages_provided(Context) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc If you provide alternative charsets, return a list of charsets and optional
%% quality value.
%%  e.g. `[<<"iso-8859-5">>, <<"unicode-1-1;q=0.8">>]'
%% @end
%%------------------------------------------------------------------------------
-spec charsets_provided(cb_context:context()) -> cb_context:context().
charsets_provided(Context) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc If you provide alternative encodings, return a list of encodings and optional
%% quality value.
%% e.g. : `[<<"gzip;q=1.0">>, <<"identity;q=0.5">>, <<"*;q=0">>]'
%% @end
%%------------------------------------------------------------------------------
-spec encodings_provided(cb_context:context()) -> cb_context:context().
encodings_provided(Context) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /skels might load a list of skel objects
%% /skels/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_skels(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_skel(Context, Id, cb_context:req_verb(Context)).

-spec validate_skels(cb_context:context(), http_method()) -> cb_context:context().
validate_skels(Context, ?HTTP_GET) ->
    summary(Context);
validate_skels(Context, ?HTTP_PUT) ->
    create(Context).

-spec validate_skel(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_skel(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_skel(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_skel(Context, Id, ?HTTP_PATCH) ->
    validate_patch(Id, Context);
validate_skel(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

%%------------------------------------------------------------------------------
%% @doc If you handle billing-related calls, this callback will allow you to
%% execute those.
%% @end
%%------------------------------------------------------------------------------
-spec billing(cb_context:context()) -> cb_context:context().
billing(Context) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%------------------------------------------------------------------------------
%% @doc If you want to manipulate the etag header, change it here in the cb_context{}
%% @end
%%------------------------------------------------------------------------------
-spec etag(cb_context:context()) -> cb_context:context().
etag(Context) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc Set the expires header
%% @end
%%------------------------------------------------------------------------------
-spec expires(cb_context:context()) -> cb_context:context().
expires(Context) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc The response has gone out, do some cleanup of your own here.
%% @end
%%------------------------------------------------------------------------------
-spec finish_request(cb_context:context()) -> cb_context:context().
finish_request(Context) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"skels">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"skel">>)).

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"skels">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Update-merge an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec validate_patch(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    crossbar_doc:patch_and_validate(Id, Context, fun update/2).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_view:load(Context, ?CB_LIST, [{'mapper', fun normalize_view_results/2}]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), <<"skel">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"skel">>)).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%%
%% This is a simple normalizer function, for which you can just use this as
%% mapper option:
%%
%% ```
%%     {'mapper', crossbar_view:get_id_fun()}
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_doc:id(JObj)|Acc].
