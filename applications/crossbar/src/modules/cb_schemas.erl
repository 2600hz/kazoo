%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_schemas).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,authorize/1
        ,authenticate/1
        ,validate/1, validate/2, validate/3
        ]).

-include("crossbar.hrl").

-define(VALIDATION_PATH_TOKEN, <<"validation">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.schemas">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.schemas">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.validate.schemas">>, ?MODULE, 'validate'),
    ok.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_nouns(cb_context:req_nouns(Context)).

-spec authorize_nouns(req_nouns()) -> boolean().
authorize_nouns([{<<"schemas">>,_}]) ->
    lager:debug("authorizing request to fetch schema(s)"),
    'true';
authorize_nouns(_) -> 'false'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate_nouns(cb_context:req_nouns(Context)).

authenticate_nouns([{<<"schemas">>,_}]) ->
    lager:debug("authenticating request to fetch schema(s)"),
    'true';
authenticate_nouns(_) -> 'false'.

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
    [?HTTP_GET].
allowed_methods(_SchemaName) ->
    [?HTTP_GET].
allowed_methods(_SchemaName, ?VALIDATION_PATH_TOKEN) ->
    [?HTTP_PUT].

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
resource_exists() ->  'true'.
resource_exists(_SchemaName) -> 'true'.
resource_exists(_SchemaName, ?VALIDATION_PATH_TOKEN) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    lager:debug("load summary of schemas from ~s", [?KZ_SCHEMA_DB]),
    summary(cb_context:set_account_db(Context, ?KZ_SCHEMA_DB)).

validate(Context, Id) ->
    read(Id, cb_context:set_account_db(Context, ?KZ_SCHEMA_DB)).

validate(Context, Id, ?VALIDATION_PATH_TOKEN) ->
    cb_context:validate_request_data(Id, Context, fun on_success/1).

-spec on_success(cb_context:context()) -> cb_context:context().
on_success(Context) ->
    cb_context:set_resp_data(Context, cb_context:doc(Context)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION_ANY).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Context1 = crossbar_doc:load_docs(Context, fun normalize_view_results/2),
    cb_context:set_resp_data(
      Context1
                            ,lists:sort(cb_context:resp_data(Context1))
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    case kz_doc:id(JObj) of
        <<"_design/", _/binary>> -> Acc;
        ID -> [ID | Acc]
    end.
