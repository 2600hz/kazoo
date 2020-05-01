%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_callflows).

-export([new/0]).
-export([featurecode/1, featurecode/2, set_featurecode/2]).
-export([featurecode_name/1, featurecode_name/2, set_featurecode_name/2]).
-export([featurecode_number/1, featurecode_number/2, set_featurecode_number/2]).
-export([flags/1, flags/2, set_flags/2]).
-export([flow/1, flow/2, set_flow/2]).
-export([metaflow/1, metaflow/2, set_metaflow/2]).
-export([name/1, name/2, set_name/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([patterns/1, patterns/2, set_patterns/2]).
-export([ringback/1, ringback/2, set_ringback/2]).
-export([ringback_early/1, ringback_early/2, set_ringback_early/2]).
-export([ringback_transfer/1, ringback_transfer/2, set_ringback_transfer/2]).

-export([type/0
        ,is_feature_code/1
        ,validate/1
        ,validate_flow/1
        ,prepend_preflow/2

         %% Dynamically added per-call
        ,capture_group/1, capture_group/2, set_capture_group/2
        ,capture_groups/1, capture_groups/2, set_capture_groups/2
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

-spec flags(doc()) -> kz_term:api_ne_binaries().
flags(Doc) ->
    flags(Doc, 'undefined').

-spec flags(doc(), Default) -> kz_term:ne_binaries() | Default.
flags(Doc, Default) ->
    kz_json:get_list_value([<<"flags">>], Doc, Default).

-spec set_flags(doc(), kz_term:ne_binaries()) -> doc().
set_flags(Doc, Flags) ->
    kz_json:set_value([<<"flags">>], Flags, Doc).

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

-spec name(doc()) -> kz_term:api_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> binary() | Default.
name(Doc, Default) ->
    kz_json:get_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

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

-spec ringback(doc()) -> kz_term:api_object().
ringback(Doc) ->
    ringback(Doc, 'undefined').

-spec ringback(doc(), Default) -> kz_json:object() | Default.
ringback(Doc, Default) ->
    kz_json:get_json_value([<<"ringback">>], Doc, Default).

-spec set_ringback(doc(), kz_json:object()) -> doc().
set_ringback(Doc, Ringback) ->
    kz_json:set_value([<<"ringback">>], Ringback, Doc).

-spec ringback_early(doc()) -> kz_term:api_binary().
ringback_early(Doc) ->
    ringback_early(Doc, 'undefined').

-spec ringback_early(doc(), Default) -> binary() | Default.
ringback_early(Doc, Default) ->
    kz_json:get_binary_value([<<"ringback">>, <<"early">>], Doc, Default).

-spec set_ringback_early(doc(), binary()) -> doc().
set_ringback_early(Doc, RingbackEarly) ->
    kz_json:set_value([<<"ringback">>, <<"early">>], RingbackEarly, Doc).

-spec ringback_transfer(doc()) -> kz_term:api_binary().
ringback_transfer(Doc) ->
    ringback_transfer(Doc, 'undefined').

-spec ringback_transfer(doc(), Default) -> binary() | Default.
ringback_transfer(Doc, Default) ->
    kz_json:get_binary_value([<<"ringback">>, <<"transfer">>], Doc, Default).

-spec set_ringback_transfer(doc(), binary()) -> doc().
set_ringback_transfer(Doc, RingbackTransfer) ->
    kz_json:set_value([<<"ringback">>, <<"transfer">>], RingbackTransfer, Doc).

-spec capture_group(doc()) -> kz_term:api_ne_binary().
capture_group(Doc) ->
    capture_group(Doc, 'undefined').

-spec capture_group(doc(), Default) -> kz_term:ne_binary() | Default.
capture_group(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"capture_group">>], Doc, Default).

-spec set_capture_group(doc(), kz_term:ne_binary()) -> doc().
set_capture_group(Doc, CaptureGroup) ->
    kz_json:set_value([<<"capture_group">>], CaptureGroup, Doc).

-spec capture_groups(doc()) -> kz_term:api_object().
capture_groups(Doc) ->
    capture_groups(Doc, 'undefined').

-spec capture_groups(doc(), Default) -> kz_json:object() | Default.
capture_groups(Doc, Default) ->
    kz_json:get_json_value([<<"capture_groups">>], Doc, Default).

-spec set_capture_groups(doc(), kz_json:object()) -> doc().
set_capture_groups(Doc, CaptureGroups) ->
    kz_json:set_value([<<"capture_groups">>], CaptureGroups, Doc).
