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

-include_lib("crossbar/include/crossbar.hrl").

-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(VALIDATION_PATH_TOKEN, <<"validation">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.schemas">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.schemas">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.schemas">>, ?MODULE, validate).

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
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
-spec allowed_methods/2 :: (path_token(), path_token()) -> http_methods().
allowed_methods() ->
    ['GET'].
allowed_methods(_) ->
    ['GET'].
allowed_methods(_, ?VALIDATION_PATH_TOKEN) ->
    ['PUT'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
-spec resource_exists/2 :: (path_token(), path_token()) -> 'true'.
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
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    lager:debug("load summary of schemas from ~s", [?WH_SCHEMA_DB]),
    summary(Context#cb_context{db_name = ?WH_SCHEMA_DB}).

validate(#cb_context{req_verb = <<"get">>}=Context, Id) ->
    read(Id, Context#cb_context{db_name = ?WH_SCHEMA_DB}).

validate(#cb_context{req_data=Data}=Context, Id, ?VALIDATION_PATH_TOKEN) ->
    case wh_json_validator:is_valid(Data, Id) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, _JObj} ->
            crossbar_util:response(<<"data passed validation">>, Context)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary/1 :: (#cb_context{}) -> #cb_context{}.
summary(Context) ->
    crossbar_doc:load_docs(Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    case wh_json:get_value(<<"id">>, JObj) of
        <<"_design/", _/binary>> -> Acc;
        ID -> [ID | Acc]
    end.
