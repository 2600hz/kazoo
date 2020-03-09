%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Metaflows execute on top of a call
%%% /accounts/{account_id}/metaflows - manip account metaflows
%%% /accounts/{account_id}/users/{user_id}/metaflows - manip user's metaflows
%%% /accounts/{account_id}/devices/{device_id}/metaflows - manip user's metaflows
%%%
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_metaflows).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ,post/1
        ,delete/1
        ]).

-include("crossbar.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.metaflows">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.metaflows">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.metaflows">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.metaflows">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.metaflows">>, ?MODULE, 'delete').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /metaflows => []
%%    /metaflows/foo => [<<"foo">>]
%%    /metaflows/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /metaflows might load a list of metaflow objects
%% /metaflows/123 might load the metaflow object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_metaflows(Context, cb_context:req_verb(Context)).

-spec validate_metaflows(cb_context:context(), http_method()) -> cb_context:context().
validate_metaflows(Context, ?HTTP_GET) ->
    validate_get_metaflows(thing_doc(Context));
validate_metaflows(Context, ?HTTP_POST) ->
    cb_context:validate_request_data(<<"metaflows">>, Context, fun validate_set_metaflows/1);
validate_metaflows(Context, ?HTTP_DELETE) ->
    validate_delete_metaflows(thing_doc(Context)).

-spec thing_doc(cb_context:context()) -> cb_context:context().
thing_doc(Context) ->
    thing_doc(Context, cb_context:req_nouns(Context)).

-spec thing_doc(cb_context:context(), req_nouns()) -> cb_context:context().
thing_doc(Context, [{<<"metaflows">>, []}, {<<"devices">>, ThingId} | _]) ->
    load_thing_doc(Context, ThingId, kzd_devices:type());
thing_doc(Context, [{<<"metaflows">>, []}, {<<"users">>, ThingId} | _]) ->
    load_thing_doc(Context, ThingId, kzd_users:type());
thing_doc(Context, [{<<"metaflows">>, []}, {<<"accounts">>, ThingId}]) ->
    load_thing_doc(Context, ThingId, kzd_accounts:type());
thing_doc(Context, _) ->
    crossbar_util:response_bad_identifier(<<"account, user or device ID is missing from url">>, Context).

load_thing_doc(Context, ThingId, Type) ->
    lager:debug("loading metaflows from '~s': '~s'", [Type, ThingId]),
    crossbar_doc:load(ThingId, Context, ?TYPE_CHECK_OPTION(Type)).

-spec validate_get_metaflows(cb_context:context()) -> cb_context:context().
validate_get_metaflows(Context) ->
    validate_get_metaflows(Context, cb_context:resp_status(Context)).

-spec validate_get_metaflows(cb_context:context(), crossbar_status()) -> cb_context:context().
validate_get_metaflows(Context, 'success') ->
    Doc = cb_context:doc(Context),
    Metaflows = kz_json:get_value(<<"metaflows">>, Doc, kz_json:new()),
    crossbar_util:response(Metaflows, Context);
validate_get_metaflows(Context, _) ->
    Context.

-spec validate_delete_metaflows(cb_context:context()) -> cb_context:context().
validate_delete_metaflows(Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            Doc = cb_context:doc(Context),
            crossbar_util:response(kz_json:new()
                                  ,cb_context:set_doc(Context
                                                     ,kz_json:delete_key(<<"metaflows">>, Doc)
                                                     )
                                  );
        _ ->
            Context
    end.

-spec validate_set_metaflows(cb_context:context()) ->
          cb_context:context().
validate_set_metaflows(Context) ->
    lager:debug("metaflow data is valid, setting on thing"),
    Metaflows = cb_context:doc(Context),
    Context1 = thing_doc(Context),
    validate_set_metaflows(Context1, Metaflows, cb_context:resp_status(Context1)).

-spec validate_set_metaflows(cb_context:context(), kz_json:object(), crossbar_status()) ->
          cb_context:context().
validate_set_metaflows(Context, Metaflows, 'success') ->
    Doc = kz_json:set_value(<<"metaflows">>, Metaflows, cb_context:doc(Context)),
    crossbar_util:response(Metaflows, cb_context:set_doc(Context, Doc));
validate_set_metaflows(Context, _, _) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    Doc = cb_context:doc(Context),
    after_post(crossbar_doc:save(Context), kz_doc:type(Doc)).

-spec after_post(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
after_post(Context, DocType) ->
    after_post(Context, DocType, cb_context:resp_status(Context)).

-spec after_post(cb_context:context(), kz_term:ne_binary(), crossbar_status()) -> cb_context:context().
after_post(Context, <<"account">>, 'success') ->
    {'ok', _} = kzd_accounts:save_accounts_doc(cb_context:doc(Context)),
    lager:debug("saved, returning the metaflows"),
    crossbar_util:response(kz_json:get_value(<<"metaflows">>, cb_context:doc(Context))
                          ,Context
                          );
after_post(Context, _, 'success') ->
    lager:debug("saved, returning the metaflows"),
    crossbar_util:response(kz_json:get_value(<<"metaflows">>, cb_context:doc(Context))
                          ,Context
                          );
after_post(Context, _DocType, _RespStatus) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    after_delete(crossbar_doc:save(Context)).

-spec after_delete(cb_context:context()) -> cb_context:context().
after_delete(Context) ->
    after_delete(Context, cb_context:resp_status(Context)).

-spec after_delete(cb_context:context(), crossbar_status()) -> cb_context:context().
after_delete(Context, 'success') ->
    crossbar_util:response(kz_json:new(), Context);
after_delete(Context, _RespStatus) ->
    Context.
