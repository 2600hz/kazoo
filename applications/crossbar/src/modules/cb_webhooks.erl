%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% Handle CRUD operations for WebHooks
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_webhooks).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"webhooks/crossbar_listing">>).

-define(PATH_TOKEN_ATTEMPTS, <<"attempts">>).

-define(ATTEMPTS_BY_ACCOUNT, <<"webhooks/attempts_by_time_listing">>).
-define(ATTEMPTS_BY_HOOK, <<"webhooks/attempts_by_hook_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = couch_mgr:db_create(?KZ_WEBHOOKS_DB),
    _ = couch_mgr:revise_doc_from_file(?KZ_WEBHOOKS_DB, 'crossbar', <<"views/webhooks.json">>),
    _ = couch_mgr:revise_doc_from_file(?WH_SCHEMA_DB, 'crossbar', <<"schemas/webhooks.json">>),

    _ = crossbar_bindings:bind(<<"*.allowed_methods.webhooks">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.webhooks">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.webhooks">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.webhooks">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.webhooks">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.webhooks">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(?PATH_TOKEN_ATTEMPTS) ->
    [?HTTP_GET];
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(_Id, ?PATH_TOKEN_ATTEMPTS) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_Id, ?PATH_TOKEN_ATTEMPTS) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB));
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    create(cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB)).

validate(Context, ?PATH_TOKEN_ATTEMPTS) ->
    summary_attempts(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB));
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    update(Id, cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB));
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id) ->
    read(Id, cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB)).

validate(Context, Id, ?PATH_TOKEN_ATTEMPTS) ->
    summary_attempts(Context, Id).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB)).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB)).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(cb_context:set_account_db(Context, ?KZ_WEBHOOKS_DB)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"webhooks">>, Context, OnSuccess).

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
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"webhooks">>, Context, OnSuccess).

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

summary_attempts(Context) ->
    summary_attempts(Context, 'undefined').
summary_attempts(Context, 'undefined') ->
    ViewOptions = [{'endkey', [cb_context:account_id(Context), 0]}
                   ,{'startkey', [cb_context:account_id(Context), wh_json:new()]}
                   ,{'limit', 15}
                   ,'include_docs'
                   ,'descending'
                  ],
    summary_attempts_fetch(Context, ViewOptions, ?ATTEMPTS_BY_ACCOUNT);
summary_attempts(Context, HookId) ->
    ViewOptions = [{'endkey', [cb_context:account_id(Context), HookId, 0]}
                   ,{'startkey', [cb_context:account_id(Context), HookId, wh_json:new()]}
                   ,{'limit', 15}
                   ,'include_docs'
                   ,'descending'
                  ],
    summary_attempts_fetch(Context, ViewOptions, ?ATTEMPTS_BY_HOOK).

summary_attempts_fetch(Context, ViewOptions, View) ->
    Db = wh_util:format_account_mod_id(cb_context:account_id(Context), wh_util:current_tstamp()),

    crossbar_doc:load_view(View
                           ,ViewOptions
                           ,cb_context:set_account_db(Context, Db)
                           ,fun normalize_attempt_results/2
                          ).

normalize_attempt_results(JObj, Acc) ->
    Doc = wh_json:get_value(<<"doc">>, JObj),
    Timestamp = wh_json:get_value(<<"pvt_created">>, Doc),
    [wh_json:delete_keys([<<"id">>, <<"_id">>]
                        ,wh_json:set_value(<<"timestamp">>, Timestamp, Doc)
                        )
     | Acc
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                       ,wh_json:set_values([{<<"pvt_type">>, <<"webhook">>}
                                            ,{<<"pvt_account_id">>, cb_context:account_id(Context)}
                                           ], cb_context:doc(Context))
                      );
on_successful_validation(Id, Context) ->
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
