%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, Umojo
%%% @doc
%%% Speech IVR module
%%%
%%% Handle client requests for Speech IVR callflow fragments
%%%
%%% @end
%%% @contributors
%%%   Jon Blanton
%%%-------------------------------------------------------------------
-module(cb_speech_ivr).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
        ]).

-include_lib("crossbar/include/crossbar.hrl").

-define(MOD_CONFIG_CAT, <<"speech_ivr">>).
-define(CB_LIST, <<"speech_ivr/crossbar_listing">>).
-define(DIRECTORY_LOOKUP, <<"speech_ivr/directory_lookup">>).
-define(EXPAND_KEYS, [<<"menu_id">>, <<"directory_id">>]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.speech_ivr">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.speech_ivr">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.speech_ivr">>, ?MODULE, validate),
    crossbar_bindings:bind(<<"v1_resource.finish_request.*.speech_ivr">>, ?MODULE, reconcile_services).

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
allowed_methods() ->
    ['GET'].

-spec allowed_methods/1 :: (path_token()) -> http_methods().
allowed_methods(_) ->
    ['GET'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
resource_exists() -> true.

-spec resource_exists/1 :: (path_token()) -> 'true'.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    load_speech_ivr_summary(Context).

-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context, DocId) ->
    load_speech_ivr(DocId, Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of speech IVRs
%% @end
%%--------------------------------------------------------------------
-spec load_speech_ivr_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_speech_ivr_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results_key/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a speech IVR callfow fragment from the database, ids resolved
%% @end
%%--------------------------------------------------------------------
-spec load_speech_ivr/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_speech_ivr(DocId, Context) ->
    case crossbar_doc:load_view(?CB_LIST, [{<<"key">>, DocId}], Context, fun normalize_view_results_value/2) of
        #cb_context{resp_status=success, resp_data=[[Data, CallflowId] | _]}=Context1 ->
            Data1 = wh_json:set_value(<<"callflow_id">>, CallflowId, Data),
            lager:debug("expanding speech ivr data"),
            expand_speech_ivr_data(Context1#cb_context{resp_data=Data1});
            %TODO: Expand directory here
        Else ->
            lager:debug("not expanding speech ivr data"),
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view using keys
%% @end
%%--------------------------------------------------------------------
-spec expand_speech_ivr_data/1 :: (#cb_context{}) -> #cb_context{}.
expand_speech_ivr_data(#cb_context{resp_data=Data}=Context) ->
    case collect_ids(Data) of
        [_|_]=Ids ->
            lager:debug("expanding 1 or more IDs"),
            inject_docs(Ids, Context);
        [] ->
            lager:debug("no IDs found"),
            Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Collects the value, specifically id, of particular fields
%% @end
%%--------------------------------------------------------------------
-spec collect_ids/1 :: (wh_json:json_object()) -> [ne_binary(), ...] | [].
collect_ids(Data) ->
    wh_json:foldl(fun(Key, Value, Acc) ->
                      case lists:member(Key, ?EXPAND_KEYS) of
                          true -> [Value | Acc];
                          false -> Acc
                      end
                  end
                  ,[]
                  ,Data
                 ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Injects documents into the response data of a given context
%% @end
%%--------------------------------------------------------------------
-spec inject_docs/2 :: ([ne_binary(), ...], #cb_context{}) -> #cb_context{}.
inject_docs(Ids, #cb_context{db_name=Db, resp_data=Data}=Context) ->
    case couch_mgr:all_docs(Db, [{<<"keys">>, Ids}, {<<"include_docs">>, <<"true">>}]) of
        {ok, Results} ->
            lager:debug("got doc(s), injecting now"),
            Data1 = lists:foldl(fun(Doc, Acc) ->
                                    wh_json:set_value(wh_json:get_value(<<"pvt_type">>, Doc)
                                                      ,crossbar_doc:public_fields(Doc)
                                                      ,Acc
                                                     )
                                end
                                ,Data
                                ,[wh_json:get_value(<<"doc">>, Result) || Result <- Results]
                               ),
            Context#cb_context{resp_data=Data1};
        _Else ->
            lager:debug("failed inject doc(s)"),
            Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view using keys
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results_key/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results_key(JObj, Acc) ->
    [wh_json:get_value(<<"key">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view using keys
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results_value/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results_value(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
