%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_phone_numbers).
-behaviour(proper_statem).

%% Crossbar API test functions
-export([summary/2
        ,list_number/3
        ,cleanup_numbers/2
        ,add_number/3
        ,activate_number/3
        ,remove_number/3
        ]).

-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
        ,correct_parallel/0
        ]).

-export([seq/0
        ,cleanup/0
        ]).

-include_lib("proper/include/proper.hrl").
-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<"accountone">>]).
-define(PHONE_NUMBERS, [<<"+12345678901">>]).

-spec cleanup_numbers(pqc_cb_api:state(), kz_term:ne_binaries()) -> 'ok'.
cleanup_numbers(_API, Numbers) ->
    _ = knm_ops:delete(Numbers, [{'auth_by',  <<"system">>}]),
    'ok'.

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    pqc_cb_crud:summary(API, numbers_url(AccountId)).

-spec list_number(pqc_cb_api:state(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
list_number(_API, 'undefined', _Number) -> ?FAILED_RESPONSE;
list_number(API, AccountId, Number) ->
    URL = number_url(AccountId, Number),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = [#expectation{response_codes = [200, 404]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec add_number(pqc_cb_api:state(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
add_number(_API, 'undefined', _Number) -> ?FAILED_RESPONSE;
add_number(API, AccountId, Number) ->
    add_number(API, AccountId, Number, kz_json:new()).

-spec add_number(pqc_cb_api:state(), kz_term:api_ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          pqc_cb_api:response().
add_number(API, AccountId, Number, RequestData) ->
    URL = number_url(AccountId, Number),
    RequestHeaders = pqc_cb_api:request_headers(API),
    RequestEnvelope  = pqc_cb_api:create_envelope(RequestData
                                                 ,kz_json:from_list([{<<"accept_charges">>, 'true'}])
                                                 ),
    Expectations = [#expectation{response_codes = [201, 404, 409]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec remove_number(pqc_cb_api:state(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
remove_number(_API, 'undefined', _Number) -> ?FAILED_RESPONSE;
remove_number(API, AccountId, Number) ->
    URL = number_url(AccountId, Number),
    RequestHeaders = pqc_cb_api:request_headers(API),
    Expectations = [#expectation{response_codes = [200, 404]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:delete/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec activate_number(pqc_cb_api:state(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
activate_number(_API, 'undefined', _Number) -> ?FAILED_RESPONSE;
activate_number(API, AccountId, Number) ->
    URL = number_url(AccountId, Number, "activate"),
    RequestHeaders = pqc_cb_api:request_headers(API),
    RequestEnvelope  = pqc_cb_api:create_envelope(kz_json:new(), kz_json:from_list([{<<"accept_charges">>, 'true'}])),

    Expectations = [#expectation{response_codes = [201, 404, 500]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec numbers_url(kz_term:ne_binary()) -> string().
numbers_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "phone_numbers"]
               ,"/"
               ).

-spec number_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
number_url(AccountId, Number) ->
    string:join([pqc_cb_accounts:account_url(AccountId)
                ,"phone_numbers", kz_term:to_list(kz_http_util:urlencode(Number))
                ]
               ,"/"
               ).

-spec number_url(kz_term:ne_binary(), kz_term:ne_binary(), string()) -> string().
number_url(AccountId, Number, PathToken) ->
    string:join([pqc_cb_accounts:account_url(AccountId)
                ,"phone_numbers", kz_term:to_list(kz_http_util:urlencode(Number))
                ,PathToken
                ]
               ,"/"
               ).

-spec correct() -> any().
correct() ->
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   timer:sleep(1000),
                   {History, Model, Result} = run_commands(?MODULE, Cmds),
                   pqc_cb_accounts:cleanup_accounts(pqc_kazoo_model:api(Model), ?ACCOUNT_NAMES),

                   ?WHENFAIL(io:format("Final Model : ~p~nFailing Cmds: ~p~n"
                                      ,[pqc_kazoo_model:pp(Model), zip(Cmds, History)]
                                      )
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

-spec correct_parallel() -> any().
correct_parallel() ->
    ?FORALL(Cmds
           ,parallel_commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   {Sequential, Parallel, Result} = run_parallel_commands(?MODULE, Cmds),
                   pqc_cb_accounts:cleanup_accounts(?ACCOUNT_NAMES),

                   ?WHENFAIL(io:format("S: ~p~nP: ~p~n", [Sequential, Parallel])
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    API = pqc_cb_api:authenticate(),
    pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    pqc_kazoo_model:new(API).

-spec command(any()) -> proper_types:type().
command(Model) ->
    command(Model, pqc_kazoo_model:has_accounts(Model)).

-spec command(any(), boolean()) -> proper_types:type().
command(Model, 'false') ->
    pqc_cb_accounts:command(Model, name());
command(Model, 'true') ->
    API = pqc_kazoo_model:api(Model),
    AccountId = pqc_cb_accounts:symbolic_account_id(Model, name()),

    oneof([{'call', ?MODULE, 'list_number', [API, AccountId, phone_number()]}
          ,{'call', ?MODULE, 'add_number', [API, AccountId, phone_number()]}
          ,{'call', ?MODULE, 'activate_number', [API, AccountId, phone_number()]}
          ,{'call', ?MODULE, 'remove_number', [API, AccountId, phone_number()]}
          ,pqc_cb_accounts:command(Model, name())
           %% ,{'call', ?MODULE, 'reserve_number', [API, name(), phone_number()]}
          ]).

name() ->
    elements(?ACCOUNT_NAMES).

phone_number() ->
    elements(?PHONE_NUMBERS).

-spec next_state(pqc_kazoo_model:model(), any(), any()) -> pqc_kazoo_model:model().
next_state(Model, APIResp, {'call', _, 'create_account', _Args}=Call) ->
    pqc_cb_accounts:next_state(Model, APIResp, Call);
next_state(Model
          ,APIResp
          ,{'call', _, 'add_number', [_API, AccountId, Number]}
          ) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:does_account_exist/2, [AccountId]}
                           ,{fun pqc_kazoo_model:is_number_missing_in_account/3, [AccountId, Number]}
                           ,{fun pqc_kazoo_model:add_number_to_account/4, [AccountId, Number, APIResp]}
                           ]
                          );
next_state(Model
          ,_APIResp
          ,{'call', _, 'remove_number', [_API, AccountId, Number]}
          ) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:does_account_exist/2, [AccountId]}
                           ,{fun pqc_kazoo_model:is_number_in_account/3, [AccountId, Number]}
                           ,{fun pqc_kazoo_model:remove_number_from_account/2, [Number]}
                           ]
                          );
next_state(Model
          ,APIResp
          ,{'call', _, 'activate_number', [_API, AccountId, Number]}
          ) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:does_account_exist/2, [AccountId]}
                           ,{fun pqc_kazoo_model:is_number_in_account/3, [AccountId, Number]}
                           ,{fun pqc_kazoo_model:transition_number_state/3, [Number, APIResp]}
                           ]
                          );
next_state(Model
          ,_APIResp
          ,{'call', _, 'list_number', [_API, _AccountId, _Number]}
          ) ->
    Model.

-spec postcondition(pqc_kazoo_model:model(), any(), any()) -> boolean().
postcondition(Model
             ,{'call', _, 'create_account', [_API, _Name]}=Call
             ,APIResult
             ) ->
    pqc_cb_accounts:postcondition(Model, Call, APIResult);
postcondition(Model
             ,{'call', _, 'list_number', [_API, AccountId, Number]}
             ,APIResult
             ) ->
    case pqc_kazoo_model:does_account_exist(Model, AccountId) of
        'false' -> ?FAILED_RESPONSE =:= APIResult;
        'true' ->
            case pqc_kazoo_model:number_data(Model, Number) of
                'undefined' ->
                    404 =:= pqc_cb_response:error_code(APIResult);
                #{'number_state' := NumberState
                 ,'account_id' := AccountId
                 } ->
                    NumberState =:= pqc_cb_response:number_state(APIResult);
                _N ->
                    404 =:= pqc_cb_response:error_code(APIResult)
            end
    end;
postcondition(Model
             ,{'call', _, 'add_number', [_API, AccountId, Number]}
             ,APIResult
             ) ->
    case pqc_kazoo_model:does_account_exist(Model, AccountId) of
        'false' -> ?FAILED_RESPONSE =:= APIResult;
        'true' ->
            case pqc_kazoo_model:number_data(Model, Number) of
                #{'account_id' := AccountId
                 ,'number_state' := _CurrentState
                 } ->
                    409 =:= pqc_cb_response:error_code(APIResult)
                        andalso <<"number_exists">> =:= pqc_cb_response:message(APIResult);
                #{} ->
                    %% if in another account
                    409 =:= pqc_cb_response:error_code(APIResult);
                'undefined' ->
                    case <<"success">> =:= pqc_cb_response:status(APIResult) of
                        'true' -> 'reserved' =:= pqc_cb_response:number_state(APIResult);
                        'false' -> 500 =:= pqc_cb_response:error_code(APIResult)
                    end
            end
    end;
postcondition(Model
             ,{'call', _, 'remove_number', [_API, AccountId, Number]}
             ,APIResult
             ) ->
    case pqc_kazoo_model:does_account_exist(Model, AccountId) of
        'false' -> ?FAILED_RESPONSE =:= APIResult;
        'true' ->
            case pqc_kazoo_model:number_data(Model, Number) of
                #{'account_id' := AccountId} ->
                    <<"success">> =:= pqc_cb_response:status(APIResult)
                        andalso 'deleted' =:= pqc_cb_response:number_state(APIResult);
                #{} ->
                    404 =:= pqc_cb_response:error_code(APIResult);
                'undefined' ->
                    404 =:= pqc_cb_response:error_code(APIResult)
            end
    end;
postcondition(Model
             ,{'call', _, 'activate_number', [_API, AccountId, Number]}
             ,APIResult
             ) ->
    case pqc_kazoo_model:does_account_exist(Model, AccountId) of
        'false' -> ?FAILED_RESPONSE =:= APIResult;
        'true' ->
            case pqc_kazoo_model:number_data(Model, Number) of
                #{'number_state' := NumberState
                 ,'account_id' := AccountId
                 } when 'reserved' =:= NumberState
                        orelse 'in_service' =:= NumberState
                        ->
                    'in_service' =:= pqc_cb_response:number_state(APIResult);
                #{} ->
                    <<"error">> =:= pqc_cb_response:status(APIResult);
                'undefined' ->
                    404 =:= pqc_cb_response:error_code(APIResult)
            end
    end.

-spec precondition(pqc_kazoo_model:model(), any()) -> boolean().
precondition(_Model, _Call) -> 'true'.

-spec seq() -> 'ok'.
seq() ->
    _ = init(),
    seq_kzoo_41().

seq_kzoo_41() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),
    lager:info("created account ~s", [AccountId]),

    EmptySummaryResp = summary(API, AccountId),
    lager:info("empty summary: ~s", [EmptySummaryResp]),
    'true' = kz_json:is_empty(kz_json:get_json_value([<<"data">>, <<"numbers">>], kz_json:decode(EmptySummaryResp))),

    PhoneNumber = hd(?PHONE_NUMBERS),

    CreateResp = add_number(API, AccountId, PhoneNumber, kz_json:from_list([{<<"carrier_name">>, <<"knm_other">>}])),
    lager:info("create resp ~p", [CreateResp]),
    NumberDoc = kz_json:get_json_value(<<"data">>, kz_json:decode(CreateResp)),
    <<PhoneNumber/binary>> = kz_doc:id(NumberDoc),

    SummaryResp = summary(API, AccountId),
    lager:info("summary resp: ~s", [SummaryResp]),
    SummaryJObj = kz_json:get_json_value([<<"data">>, <<"numbers">>, PhoneNumber], kz_json:decode(SummaryResp)),
    'false' = kz_json:is_empty(SummaryJObj),

    RemoveResp = remove_number(API, AccountId, PhoneNumber),
    lager:info("removed resp: ~s", [RemoveResp]),

    EmptyAgainResp = summary(API, AccountId),
    lager:info("empty again: ~s", [EmptyAgainResp]),
    EmptyData = kz_json:get_json_value([<<"data">>, <<"numbers">>], kz_json:decode(EmptyAgainResp)),
    'true' = kz_json:is_empty(EmptyData),

    cleanup(API).

init() ->
    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_phone_numbers_v2']
        ],
    lager:info("INIT FINISHED").

-spec cleanup() -> any().
cleanup() ->
    lager:info("CLEANUP ALL THE THINGS"),
    kz_data_tracing:clear_all_traces(),
    cleanup(pqc_cb_api:authenticate()).

-spec cleanup(pqc_cb_api:state()) -> any().
cleanup(API) ->
    lager:info("CLEANUP TIME, EVERYBODY HELPS"),
    lager:info("~p", [API]),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _D = knm_numbers:delete(?PHONE_NUMBERS, [{'auth_by', pqc_cb_api:auth_account_id(API)}]),
    lager:info("deleted numbers ~p", [_D]),
    _ = pqc_cb_api:cleanup(API),
    'ok'.
