%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_functions).

%% API requests
-export([summary/2
        ,create/3
        ,fetch/3
        ,update/3
        ,patch/4
        ,delete/3
        ]).

-export([seq/0
        ,cleanup/0
        ,new_function/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, functions_url(AccountId)).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_functions:doc()) -> pqc_cb_api:response().
create(API, AccountId, FunctionJObj) ->
    Envelope = pqc_cb_api:create_envelope(FunctionJObj),
    pqc_cb_crud:create(API, functions_url(AccountId), Envelope).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, FunctionId) ->
    pqc_cb_crud:fetch(API, function_url(AccountId, FunctionId)).

-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_function:doc()) -> pqc_cb_api:response().
update(API, AccountId, FunctionJObj) ->
    Envelope = pqc_cb_api:create_envelope(FunctionJObj),
    pqc_cb_crud:update(API, function_url(AccountId, kz_doc:id(FunctionJObj)), Envelope).

-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch(API, AccountId, FunctionId, PatchJObj) ->
    Envelope = pqc_cb_api:create_envelope(PatchJObj),
    pqc_cb_crud:patch(API, function_url(AccountId, FunctionId), Envelope).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, FunctionId) ->
    pqc_cb_crud:delete(API, function_url(AccountId, FunctionId)).

-spec functions_url(kz_term:ne_binary()) -> string().
functions_url(AccountId) ->
    pqc_cb_crud:collection_url(AccountId, <<"functions">>).

-spec function_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
function_url(AccountId, FunctionId) ->
    pqc_cb_crud:entity_url(AccountId, <<"functions">>, FunctionId).

-spec seq() -> 'ok'.
seq() ->
    Mods = crossbar_config:autoload_modules(),
    case lists:member(<<"cb_functions">>, Mods) of
        'true' -> seq_run();
        'false' ->
            lager:info("cb_functions and couch --eval must be enabled to run this test")
    end.

seq_run() ->
    API = pqc_cb_api:init_api(['crossbar'], ['cb_functions']),
    AccountId = create_account(API),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptySummaryResp]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySummaryResp)),

    FunctionJObj = new_function(),
    CreateResp = create(API, AccountId, FunctionJObj),
    lager:info("created function ~s", [CreateResp]),
    CreatedFunction = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    FunctionId = kz_doc:id(CreatedFunction),

    Patch = kz_json:from_list([{<<"custom">>, <<"value">>}]),
    PatchResp = patch(API, AccountId, FunctionId, Patch),
    lager:info("patched to ~s", [PatchResp]),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    [SummaryFunction] = kz_json:get_list_value(<<"data">>, kz_json:decode(SummaryResp)),
    FunctionId = kz_doc:id(SummaryFunction),

    run_function(FunctionId),

    DeleteResp = delete(API, AccountId, FunctionId),
    lager:info("delete resp: ~s", [DeleteResp]),

    EmptyAgain = summary(API, AccountId),
    lager:info("empty summary resp: ~s", [EmptyAgain]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptyAgain)),

    cleanup(API),
    lager:info("FINISHED FUNCTION SEQ").

-spec cleanup() -> 'ok'.
cleanup() ->
    _ = pqc_cb_accounts:cleanup_accounts(?ACCOUNT_NAMES),
    cleanup_system().

cleanup(API) ->
    lager:info("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _ = pqc_cb_api:cleanup(API),
    cleanup_system().

cleanup_system() -> 'ok'.

-spec create_account(pqc_cb_api:state()) -> kz_term:ne_binary().
create_account(API) ->
    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    kz_json:get_ne_binary_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)).

-spec new_function() -> kzd_functions:doc().
new_function() ->
    kz_doc:public_fields(
      kz_json:exec_first([{fun kzd_functions:set_name/2, kz_binary:rand_hex(4)}
                         ,{fun kzd_functions:set_function_js/2, <<"return {'action':'tts', 'data':{'text':'got it ' + call['To']}};">>}
                         ]
                        ,kzd_functions:new()
                        )
     ).

-define(RESP, kz_json:from_list([{<<"action">>,<<"tts">>}
                                ,{<<"data">>,kz_json:from_list([{<<"text">>,<<"got it to">>}])}
                                ])
       ).
run_function(FunctionId) ->
    Routines = [{fun kapps_call:set_to/2, <<"to@nodomain">>}
               ,{fun kapps_call:set_from/2, <<"from@nodomain">>}
               ,{fun kapps_call:set_call_id/2, kz_binary:rand_hex(12)}
               ,{fun kapps_call:set_caller_id_number/2, <<"123456">>}
               ,{fun kapps_call:set_caller_id_name/2, <<"from">>}
               ],
    Call = kapps_call:exec(Routines, kapps_call:new()),
    {'ok', Resp} = cf_function:run_function(FunctionId, Call),
    lager:info("ran ~s: ~p", [FunctionId, Resp]),
    'true' = kz_json:are_equal(Resp, ?RESP).
