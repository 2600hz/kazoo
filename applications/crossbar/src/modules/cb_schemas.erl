%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for skel documents
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

-include("../crossbar.hrl").

-define(VALIDATION_PATH_TOKEN, <<"validation">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.schemas">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.schemas">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"*.validate.schemas">>, ?MODULE, validate).

authorize(#cb_context{req_nouns=[{<<"schemas">>,_}]}) ->
    lager:debug("authorizing request to fetch schema(s)"),
    true.

authenticate(#cb_context{req_nouns=[{<<"schemas">>,_}]}) ->
    lager:debug("authenticating request to fetch schema(s)"),
    true.

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
allowed_methods(_) ->
    [?HTTP_GET].
allowed_methods(_, ?VALIDATION_PATH_TOKEN) ->
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
resource_exists() ->
    true.
resource_exists(_) ->
    true.
resource_exists(_, ?VALIDATION_PATH_TOKEN) ->
    true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(#cb_context{}) -> #cb_context{}.
-spec validate(#cb_context{}, path_token()) -> #cb_context{}.
-spec validate(#cb_context{}, path_token(), path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    lager:debug("load summary of schemas from ~s", [?WH_SCHEMA_DB]),
    summary(Context#cb_context{db_name = ?WH_SCHEMA_DB}).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, Context#cb_context{db_name = ?WH_SCHEMA_DB}).

validate(#cb_context{}=Context, Id, ?VALIDATION_PATH_TOKEN) ->
    OnSuccess = fun(#cb_context{doc=J}=C) -> C#cb_context{resp_data=J} end,
    cb_context:validate_request_data(Id, Context, OnSuccess).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), #cb_context{}) -> #cb_context{}.
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(#cb_context{}) -> #cb_context{}.
summary(Context) ->
    crossbar_doc:load_docs(Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    case wh_json:get_value(<<"id">>, JObj) of
        <<"_design/", _/binary>> -> Acc;
        ID -> [ID | Acc]
    end.
