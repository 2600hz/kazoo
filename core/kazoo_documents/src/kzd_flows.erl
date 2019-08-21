%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_flows).

-export([new/0]).
-export([featurecode/1, featurecode/2, set_featurecode/2]).
-export([featurecode_name/1, featurecode_name/2, set_featurecode_name/2]).
-export([featurecode_number/1, featurecode_number/2, set_featurecode_number/2]).
-export([flow/1, flow/2, set_flow/2]).
-export([metaflow/1, metaflow/2, set_metaflow/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([patterns/1, patterns/2, set_patterns/2]).

-export([type/0
        ,is_feature_code/1
        ,validate/1
        ,validate_flow/1
        ,prepend_preflow/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"callflows">>).

-define(PVT_TYPE, <<"callflow">>).

-spec new() -> doc().
new() ->
    kz_doc:update_pvt_parameters(kz_json_schema:default_object(?SCHEMA)
                                ,'undefined'
                                ,[{'type', type()}
                                 ,{'now', kz_time:now_s()}
                                 ]
                                ).

-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec featurecode(doc()) -> kz_term:api_object().
featurecode(Doc) ->
    featurecode(Doc, 'undefined').

-spec featurecode(doc(), Default) -> kz_json:object() | Default.
featurecode(Doc, Default) ->
    kz_json:get_json_value([<<"featurecode">>], Doc, Default).

-spec set_featurecode(doc(), kz_json:object()) -> doc().
set_featurecode(Doc, Featurecode) ->
    kz_json:set_value([<<"featurecode">>], Featurecode, Doc).

-spec featurecode_name(doc()) -> kz_term:api_ne_binary().
featurecode_name(Doc) ->
    featurecode_name(Doc, 'undefined').

-spec featurecode_name(doc(), Default) -> kz_term:ne_binary() | Default.
featurecode_name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"featurecode">>, <<"name">>], Doc, Default).

-spec set_featurecode_name(doc(), kz_term:ne_binary()) -> doc().
set_featurecode_name(Doc, FeaturecodeName) ->
    kz_json:set_value([<<"featurecode">>, <<"name">>], FeaturecodeName, Doc).

-spec featurecode_number(doc()) -> kz_term:api_ne_binary().
featurecode_number(Doc) ->
    featurecode_number(Doc, 'undefined').

-spec featurecode_number(doc(), Default) -> kz_term:ne_binary() | Default.
featurecode_number(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"featurecode">>, <<"number">>], Doc, Default).

-spec set_featurecode_number(doc(), kz_term:ne_binary()) -> doc().
set_featurecode_number(Doc, FeaturecodeNumber) ->
    kz_json:set_value([<<"featurecode">>, <<"number">>], FeaturecodeNumber, Doc).

-spec flow(doc()) -> kz_term:api_object().
flow(Doc) ->
    flow(Doc, 'undefined').

-spec flow(doc(), Default) -> kz_json:object() | Default.
flow(Doc, Default) ->
    kz_json:get_json_value([<<"flow">>], Doc, Default).

-spec set_flow(doc(), kz_json:object()) -> doc().
set_flow(Doc, Flow) ->
    kz_json:set_value([<<"flow">>], Flow, Doc).

-spec metaflow(doc()) -> kz_term:api_object().
metaflow(Doc) ->
    metaflow(Doc, 'undefined').

-spec metaflow(doc(), Default) -> kz_json:object() | Default.
metaflow(Doc, Default) ->
    kz_json:get_json_value([<<"metaflow">>], Doc, Default).

-spec set_metaflow(doc(), kz_json:object()) -> doc().
set_metaflow(Doc, Metaflow) ->
    kz_json:set_value([<<"metaflow">>], Metaflow, Doc).

-spec numbers(doc()) -> kz_term:ne_binaries().
numbers(Doc) ->
    numbers(Doc, []).

-spec numbers(doc(), Default) -> kz_term:ne_binaries() | Default.
numbers(Doc, Default) ->
    kz_json:get_list_value([<<"numbers">>], Doc, Default).

-spec set_numbers(doc(), kz_term:ne_binaries()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value([<<"numbers">>], Numbers, Doc).

-spec patterns(doc()) -> kz_term:ne_binaries().
patterns(Doc) ->
    patterns(Doc, []).

-spec patterns(doc(), Default) -> kz_term:ne_binaries() | Default.
patterns(Doc, Default) ->
    kz_json:get_list_value([<<"patterns">>], Doc, Default).

-spec set_patterns(doc(), kz_term:ne_binaries()) -> doc().
set_patterns(Doc, Patterns) ->
    kz_json:set_value([<<"patterns">>], Patterns, Doc).

-spec is_feature_code(doc()) -> boolean().
is_feature_code(Doc) ->
    featurecode(Doc, 'false') =/= 'false'.

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
        {'ok', JObj} -> {'ok', set_flow(Doc, JObj)};
        {'error', Errors} -> {'error', Errors}
    end.

schema_test_load(Schema) when not is_binary(Schema) ->
    schema_test_load(kz_term:to_binary(Schema));
schema_test_load(<<"file://", Schema/binary>>) -> schema_test_load(Schema);
schema_test_load(<<"callflows.test">> = Schema) ->
    {'ok', kz_json:insert_value(<<"id">>, Schema, ?SCHEMA_JOBJ)};
schema_test_load(Schema) ->
    kz_json_schema:fload(Schema).
-else.
-spec validate_flow(doc()) -> {'ok', doc()} | {'error', kz_json_schema:validation_errors()}.
validate_flow(Doc) ->
    case kz_json_schema:validate(<<"callflows.action">>, flow(Doc)) of
        {'ok', JObj} -> {'ok', set_flow(Doc, JObj)};
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
