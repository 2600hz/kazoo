%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2022, 2600Hz
%%% @doc Callflow document manipulation
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_callflow).

-export([new/0
        ,type/0
        ,numbers/1, set_numbers/2
        ,patterns/1, set_patterns/2
        ,is_feature_code/1
        ,flow/1, flow/2
        ,prepend_preflow/2
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new() -> doc().
new() ->
    kz_json:from_list([{<<"pvt_type">>, type()}]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec numbers(doc()) -> kz_term:ne_binaries().
numbers(Doc) ->
    kz_json:get_value(?NUMBERS, Doc, []).

-spec set_numbers(doc(), kz_term:ne_binaries()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value(?NUMBERS, Numbers, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec patterns(doc()) -> kz_term:ne_binaries().
patterns(Doc) ->
    kz_json:get_value(?PATTERNS, Doc, []).

-spec set_patterns(doc(), kz_term:ne_binaries()) -> doc().
set_patterns(Doc, Patterns) ->
    kz_json:set_value(?PATTERNS, Patterns, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_feature_code(doc()) -> boolean().
is_feature_code(Doc) ->
    kz_json:get_value(?FEATURE_CODE, Doc, 'false') =/= 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec flow(doc()) -> kz_term:api_object().
flow(Doc) ->
    flow(Doc, 'undefined').

-spec flow(doc(), Default) -> kz_json:object() | Default.
flow(Doc, Default) ->
    kz_json:get_json_value(?FLOW, Doc, Default).

-spec set_flow(doc(), kz_json:object()) -> doc().
set_flow(Doc, Flow) ->
    kz_json:set_value(?FLOW, Flow, Doc).

-spec validate(doc()) -> {'ok', doc()} |
          {'error', kz_json_schema:validation_errors()}.
validate(Doc) ->
    case kz_json_schema:validate(<<"callflows">>, Doc) of
        {'ok', _}= OK -> OK;
        {'error', Errors} -> {'error', kz_json_schema:errors_to_jobj(Errors)}
    end.

-ifdef(TEST).
-spec validate_flow(doc()) -> {'ok', doc()} | {'error', list()}.
validate_flow(Doc) ->
    case kz_json_schema:validate(<<"callflows.action">>
                                ,flow(Doc)
                                ,[{'schema_loader_fun', fun schema_test_load/1}
                                 ,{'allowed_errors', 'infinity'}
                                 ]) of
        {'ok', JObj} -> {ok, set_flow(Doc, JObj)};
        {'error', Errors} -> {'error', Errors}
    end.

schema_test_load(Schema) when not is_binary(Schema) ->
    schema_test_load(kz_term:to_binary(Schema));
schema_test_load(<<"file://", Schema/binary>>) -> schema_test_load(Schema);
schema_test_load(<<"callflows.test">> = Schema) ->
    {ok, kz_json:insert_value(<<"id">>, Schema, ?SCHEMA_JOBJ)};
schema_test_load(Schema) ->
    kz_json_schema:fload(Schema).
-else.
-spec validate_flow(doc()) -> {'ok', doc()} | {'error', kz_json_schema:validation_errors()}.
validate_flow(Doc) ->
    case kz_json_schema:validate(<<"callflows.action">>, flow(Doc)) of
        {'ok', JObj} -> {ok, set_flow(Doc, JObj)};
        {'error', Errors} -> {'error', kz_json_schema:errors_to_jobj(Errors)}
    end.
-endif.

-spec prepend_preflow(doc(), kz_term:ne_binary()) -> doc().
prepend_preflow(Callflow, PreflowId) ->
    AmendedFlow =
        kz_json:from_list([{<<"module">>, <<"callflow">>}
                          ,{<<"data">>, kz_json:from_list([{<<"id">>, PreflowId}])}
                          ,{<<"children">>, kz_json:from_list([{<<"_">>, flow(Callflow)}])}
                          ]),
    set_flow(Callflow, AmendedFlow).
