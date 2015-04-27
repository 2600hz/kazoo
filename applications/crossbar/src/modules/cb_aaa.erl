%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module contains logic for accessing to an RADIUS/DIAMETER
%%%  dictionaries and configuration documents via HTTP REST API.
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cb_aaa).

-export([init/0
    , allowed_methods/0, allowed_methods/1, allowed_methods/2
    , resource_exists/0, resource_exists/1, resource_exists/2
    , validate/1, validate/2, validate/3
    , put/1, put/2
    , post/3
    , patch/2
    , delete/3
    , load_dict_list/1, load_dict/3]).

-include("../crossbar.hrl").
-include_lib("circlemaker/src/circlemaker_defs.hrl").

-define(CB_LIST_DICTS, <<"aaa/fetch_dicts">>).

-define(AAA_RESOURCE, <<"aaa">>).
-define(AAA_DICT_RESOURCE, <<"aaa_dict">>).
-define(DICT_RESOURCE, <<"dict">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.aaa">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.aaa">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.aaa">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.aaa">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.aaa">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.aaa">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.aaa">>, ?MODULE, 'delete').

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
allowed_methods(?DICT_RESOURCE) ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(?DICT_RESOURCE, _DictID) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE, ?HTTP_PATCH].

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
    'true'.
resource_exists(?DICT_RESOURCE) ->
    'true'.
resource_exists(?DICT_RESOURCE, _) ->
    'true'.

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
    validate_config(Context, cb_context:req_verb(Context)).

validate_config(Context, ?HTTP_GET) ->
    load_aaa_config(Context);
validate_config(Context, ?HTTP_PUT) ->
    validate_request(Context).

validate(Context, ?DICT_RESOURCE) ->
    validate_dict(Context, ?DICT_RESOURCE, cb_context:req_verb(Context)).

validate_dict(Context, ?DICT_RESOURCE, ?HTTP_GET) ->
    load_dict_list(Context);
validate_dict(Context, ?DICT_RESOURCE, ?HTTP_PUT) ->
    validate_request(Context, ?DICT_RESOURCE, 'undefined').

validate(Context, ?DICT_RESOURCE, DictId) ->
    validate_dict(Context, ?DICT_RESOURCE, DictId, cb_context:req_verb(Context)).

validate_dict(Context, ?DICT_RESOURCE, DictId, ?HTTP_GET) ->
    load_dict(Context, ?DICT_RESOURCE, DictId);
validate_dict(Context, ?DICT_RESOURCE, DictId, ?HTTP_POST) ->
    validate_request(Context, ?DICT_RESOURCE, DictId);
validate_dict(Context, ?DICT_RESOURCE, DictId, ?HTTP_PATCH) ->
    validate_patch(load_dict(Context, ?DICT_RESOURCE, DictId), ?DICT_RESOURCE);
validate_dict(Context, ?DICT_RESOURCE, DictId, ?HTTP_DELETE) ->
    load_dict(Context, ?DICT_RESOURCE, DictId).


-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, ?DICT_RESOURCE, _DocId) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).
put(Context, ?DICT_RESOURCE) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, ?DICT_RESOURCE, _DocID) ->
    crossbar_doc:delete(Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _DocID) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load account config
%% @end
%%--------------------------------------------------------------------
-spec load_aaa_config(cb_context:context()) -> cb_context:context().
load_aaa_config(Context) ->
    crossbar_doc:load(?AAA_RESOURCE, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Validate request for AAA configuration document for an account 
%% @end
%%--------------------------------------------------------------------
-spec validate_request(cb_context:context()) -> cb_context:context().
validate_request(Context) ->
    check_aaa_schema(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Validate request for an dictionary of an account 
%% @end
%%--------------------------------------------------------------------
-spec validate_request(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate_request(Context, ?DICT_RESOURCE, DictId) ->
    check_aaa_schema(Context, DictId).

-spec check_aaa_schema(cb_context:context()) -> cb_context:context().
check_aaa_schema(Context) ->
    OnSuccess = fun(C) -> on_successful_validation(C) end,
    cb_context:validate_request_data(?AAA_RESOURCE, Context, OnSuccess).

-spec on_successful_validation(cb_context:context()) -> cb_context:context().
on_successful_validation(Context) ->
    Doc = cb_context:req_data(Context),
    Res = crossbar_doc:load_merge(?AAA_RESOURCE, Context),
    case {cb_context:resp_status(Res), cb_context:resp_error_code(Res)} of
        {'success', _} ->
            Res;
        {'error', 404} ->
            cb_context:set_doc(Context, wh_json:set_values([{<<"pvt_type">>, ?AAA_RESOURCE}, {<<"_id">>, ?AAA_RESOURCE}, {<<"id">>, ?AAA_RESOURCE}], Doc));
        {_, _} ->
            Context
    end.

-spec check_aaa_schema(cb_context:context(), ne_binary()) -> cb_context:context().
check_aaa_schema(Context, DictId) ->
    OnSuccess = fun(C) -> on_successful_validation(C, DictId) end,
    NewReqData = wh_json:set_values([{<<"owner">>, cb_context:account_id(Context)}], cb_context:req_data(Context)),
    Context1 = cb_context:set_req_data(Context, NewReqData),
    Context2 = cb_context:set_account_db(Context1, ?WH_AAA_DICTS_DB),
    cb_context:validate_request_data(?AAA_DICT_RESOURCE, Context2, OnSuccess).

-spec on_successful_validation(cb_context:context(), atom()) -> cb_context:context(); (cb_context:context(), ne_binary()) -> cb_context:context().
on_successful_validation(Context, 'undefined') ->
    Doc1 = wh_json:set_values([{<<"pvt_type">>, ?AAA_DICT_RESOURCE}], cb_context:doc(Context)),
    cb_context:set_doc(Context, Doc1);
on_successful_validation(Context, DictId) when is_binary(DictId) ->
    crossbar_doc:load_merge(DictId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Support PATCH - merge vmbox document with request data
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(cb_context:context(), ne_binary()) -> cb_context:context().
validate_patch(Context, DictId)  when is_binary(DictId) ->
    case cb_context:resp_status(Context) of
        'success' ->
            PatchJObj = wh_doc:public_fields(cb_context:req_data(Context)),
            VmJObj = wh_json:merge_jobjs(PatchJObj, cb_context:doc(Context)),
            OnValidateReqDataSuccess = fun(C) -> crossbar_doc:load_merge(DictId, C) end,
            cb_context:validate_request_data(?AAA_RESOURCE, cb_context:set_req_data(Context, VmJObj), OnValidateReqDataSuccess);
        _Status -> Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_filter_view(ne_binary()) -> cb_context:context().
normalize_filter_view(AccountId) ->
    fun(JObj, Acc) ->
        case wh_json:get_value(<<"owner">>, wh_json:get_value(<<"value">>, JObj)) of
            AccountId -> [JObj|Acc];
            _ -> Acc
        end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec load_dict_list(cb_context:context()) -> cb_context:context().
load_dict_list(Context) ->
    Context1 = cb_context:set_account_db(Context, ?WH_AAA_DICTS_DB),
    crossbar_doc:load_view(?CB_LIST_DICTS, [], Context1, normalize_filter_view(cb_context:account_id(Context))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a dict document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_dict(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
load_dict(Context, ?DICT_RESOURCE, DictId) when is_binary(DictId) ->
    Context1 = cb_context:set_account_db(Context, ?WH_AAA_DICTS_DB),
    crossbar_doc:load(DictId, Context1).
