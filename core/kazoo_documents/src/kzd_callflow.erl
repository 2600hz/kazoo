%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz
%%% @doc
%%% Callflow document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_callflow).

-export([new/0
        ,type/0
        ,numbers/1, set_numbers/2
        ,patterns/1, set_patterns/2
        ,is_feature_code/1
        ,flow/1, flow/2
        ,set_flow/2
        ,validate/1
        ,validate_flow/1
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(CONFIG_CAT, <<"crossbar">>).

-define(FEATURE_CODE, <<"featurecode">>).
-define(PVT_TYPE, <<"callflow">>).
-define(FLOW, <<"flow">>).
-define(NUMBERS, <<"numbers">>).
-define(PATTERNS, <<"patterns">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec new() -> doc().
new() ->
    kz_json:from_list([{<<"pvt_type">>, type()}]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec type() -> ne_binary().
type() -> ?PVT_TYPE.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec numbers(doc()) -> ne_binaries().
numbers(Doc) ->
    kz_json:get_value(?NUMBERS, Doc, []).

-spec set_numbers(doc(), ne_binaries()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value(?NUMBERS, Numbers, Doc).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec patterns(doc()) -> ne_binaries().
patterns(Doc) ->
    kz_json:get_value(?PATTERNS, Doc, []).

-spec set_patterns(doc(), ne_binaries()) -> doc().
set_patterns(Doc, Patterns) ->
    kz_json:set_value(?PATTERNS, Patterns, Doc).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_feature_code(doc()) -> boolean().
is_feature_code(Doc) ->
    kz_json:get_value(?FEATURE_CODE, Doc, 'false') =/= 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec flow(doc()) -> api_object().
-spec flow(doc(), Default) -> kz_json:object() | Default.
flow(Doc) ->
    flow(Doc, 'undefined').
flow(Doc, Default) ->
    kz_json:get_json_value(?FLOW, Doc, Default).

-spec set_flow(doc(), kz_json:object()) -> doc().
set_flow(Doc, Flow) ->
    kz_json:set_value(?FLOW, Flow, Doc).

-spec validate(doc()) -> {'ok', doc()} |
                         {'error', kz_json_schema:validation_errors()}.
validate(Doc) ->
    case kz_json_schema:validate(<<"callflows">>, Doc) of
        {'ok', ValidatedDoc} -> validate_flow(ValidatedDoc);
        {'error', Errors} ->
            {'error', kz_json_schema:errors_to_jobj(Errors)}
    end.

-spec validate_flow(doc()) -> {'ok', doc()} |
                              {'error', kz_json_schema:validation_errors()}.
validate_flow(Doc) ->
    case validate_flow_elements(flow(Doc)) of
        {Flow, []} -> {'ok', set_flow(Doc, Flow)};
        {_Flow, Errors} ->
            {'error', Errors}
    end.

-type validate_acc() :: {kz_json:object(), [jesse_error:error_reason()]}.
-spec validate_flow_elements(kz_json:object()) -> validate_acc().
-spec validate_flow_elements(kz_json:object(), validate_acc(), ne_binaries()) -> validate_acc().
validate_flow_elements(Flow) ->
    validate_flow_elements(Flow, {Flow, []}, []).

-ifdef(TEST).
validate_flow_elements(Flow, {_RootFlow, ErrorsSoFar}=Acc, Path) ->
    case kz_json_schema:validate(<<"callflows.action">>
                                ,Flow
                                ,[{'schema_loader_fun', fun kz_json_schema:fload/1}
                                 ,{'allowed_errors', 'infinity'}
                                 ])
    of
        {'ok', ValidatedFlow} ->
            Module = kz_json:get_binary_value(<<"module">>, ValidatedFlow),
            Data = kz_json:get_json_value(<<"data">>, ValidatedFlow),
            Children = kz_json:get_json_value(<<"children">>, ValidatedFlow, kz_json:new()),

            validate_action_schema(Module, Data, Children, Acc, Path);
        {'error', Errors} ->
            {Flow, update_error_key_paths(Path, Errors) ++ ErrorsSoFar}
    end.
-else.
validate_flow_elements(Flow, {_RootFlow, ErrorsSoFar}=Acc, Path) ->
    case kz_json_schema:validate(<<"callflows.action">>, Flow) of
        {'ok', ValidatedFlow} ->
            Module = kz_json:get_binary_value(<<"module">>, ValidatedFlow),
            Data = kz_json:get_json_value(<<"data">>, ValidatedFlow),
            Children = kz_json:get_json_value(<<"children">>, ValidatedFlow, kz_json:new()),

            validate_action_schema(Module, Data, Children, Acc, Path);
        {'error', Errors} ->
            {Flow, update_error_key_paths(Path, Errors) ++ ErrorsSoFar}
    end.
-endif.

-spec validate_action_schema(api_binary(), api_object(), kz_json:object(), validate_acc(), ne_binaries()) ->
                                    validate_acc().
validate_action_schema(Module, Data, Children, {RootFlow, ErrorsSoFar}, Path) ->
    {UpdatedData, Errors} =
        case validate_action_schema(Module, Data) of
            {'true', ValidatedData} -> {ValidatedData, ErrorsSoFar};
            {'false', ActionErrors} ->
                {Data, update_error_key_paths(Path, ActionErrors) ++ ErrorsSoFar}
        end,

    UpdatedRootFlow = kz_json:set_value(lists:reverse([<<"data">>|Path])
                                       ,UpdatedData
                                       ,RootFlow
                                       ),

    kz_json:foldl(fun(Branch, BranchFlow, Acc) ->
                          validate_flow_elements(BranchFlow
                                                ,Acc
                                                ,[Branch, <<"children">> | Path]
                                                )
                  end
                 ,{UpdatedRootFlow, Errors}
                 ,Children
                 ).

-spec update_error_key_paths(ne_binaries(), [jesse_error:error_reason()]) ->
                                    [jesse_error:error_reason()].
update_error_key_paths(Path, ActionErrors) ->
    [update_error_key_path(Path, Error) || Error <- ActionErrors].

-spec update_error_key_path(ne_binaries(), jesse_error:error_reason()) ->
                                   jesse_error:error_reason().
update_error_key_path(Path, {'data_invalid', SchemaJObj, Error, Value, Key}) ->
    {'data_invalid', SchemaJObj, Error, Value, lists:reverse(Key ++ Path)}.

-ifdef(TEST).
validate_action_schema(Module, Data) ->
    validate_action_schema(Module, Data, {'ok', ?SCHEMA_JOBJ}).
-else.
-spec validate_action_schema(ne_binary(), kz_json:object()) ->
                                    {'true', kz_json:object()} |
                                    {'false', kz_json_schema:validation_errors()}.
validate_action_schema(Module, Data) ->
    Schema = <<"callflows.", Module/binary>>,
    validate_action_schema(Module, Data, kz_json_schema:load(Schema)).
-endif.

validate_action_schema(_Module, Data, {'error', 'not_found'}) ->
    {'true', Data};
validate_action_schema(Module, Data, {'ok', SchemaJObj}) ->
    Strict = should_validate_strictly(),
    case kz_json_schema:validate(SchemaJObj, kz_json:public_fields(Data)) of
        {'ok', ValidatedData} ->
            validate_action(Module, kz_json_schema:add_defaults(ValidatedData, SchemaJObj), SchemaJObj);
        {'error', Errors} when Strict ->
            {'false', Errors};
        {'error', Errors} ->
            maybe_fix_js_types(Module, Data, SchemaJObj, Errors)
    end.

-ifdef(TEST).
should_validate_strictly() -> 'false'.
-else.
-spec should_validate_strictly() -> boolean().
should_validate_strictly() ->
    kapps_config:get_is_true(?CONFIG_CAT, <<"schema_strict_validation">>, 'false').
-endif.

validate_action(<<"record_call">>, Data, SchemaJObj) ->
    Max = kz_media_util:max_recording_time_limit(),
    TimeLimit = kz_json:get_integer_value(<<"time_limit">>, Data),
    try kz_json:get_value(<<"action">>, Data) =:= <<"start">>
             andalso TimeLimit > Max
    of
        'false' -> {'true', Data};
        'true' ->
            lager:debug("the requested time limit is too damn high"),
            {'false', [{'data_invalid'
                       ,kz_json:set_value(<<"maximum">>, Max, SchemaJObj)
                       ,'not_maximum'
                       ,TimeLimit
                       ,[<<"time_limit">>]
                       }]
            }
    catch
        _E:_R ->
            lager:debug("failed to get integer from data: ~s: ~p", [_E, _R]),
            {'false', [{'data_invalid', SchemaJObj, 'wrong_type', TimeLimit, [<<"time_limit">>]}]}
    end;
validate_action(_Module, Data, _SchemaJObj) ->
    {'true', Data}.

-spec maybe_fix_js_types(ne_binary(), kz_json:object(), kz_json:object(), kz_json_schema:validation_errors()) ->
                                {'true', kz_json:object()} |
                                {'false', kz_json_schema:validation_errors()}.
maybe_fix_js_types(Module, Data, SchemaJObj, Errors) ->
    case maybe_fix_js_types(Data, Errors) of
        {'false', _} -> {'false', Errors};
        {'true', FixedData} -> validate_action_schema(Module, FixedData, {'ok', SchemaJObj})
    end.

maybe_fix_js_types(Data, Errors) ->
    lists:foldl(fun maybe_fix_js_type/2, {'false', Data}, Errors).

maybe_fix_js_type({'data_invalid', SchemaJObj, 'wrong_type', Value, Key}, {_Updated, Data}=Acc) ->
    case kz_json:get_value(<<"type">>, SchemaJObj) of
        <<"integer">> -> maybe_fix_js_integer(Key, Value, Data);
        _Type -> Acc
    end;
maybe_fix_js_type(_Error, Acc) -> Acc.

-spec maybe_fix_js_integer(kz_json:path(), kz_json:json_term(), kz_json:object()) ->
                                  {boolean(), kz_json:object()}.
maybe_fix_js_integer(Key, Value, Data) ->
    try kz_util:to_integer(Value) of
        V -> {'true', kz_json:set_value(maybe_fix_index(Key), V, Data)}
    catch
        _E:_R ->
            lager:debug("error converting value to integer ~p : ~p : ~p"
                       ,[Value, _E, _R]
                       ),
            {'false', Data}
    end.

-spec maybe_fix_index(kz_json:path() | kz_json:path()) -> kz_json:path().
maybe_fix_index(Keys)
  when is_list(Keys) ->
    lists:map(fun(K) when is_integer(K) ->
                      K + 1;
                 (K) -> K
              end
             ,Keys
             );
maybe_fix_index(Key) ->
    Key.
