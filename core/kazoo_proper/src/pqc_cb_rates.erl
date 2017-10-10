-module(pqc_cb_rates).
-behaviour(proper_statem).

-export([seq/0, init/0
        ,cleanup/0, cleanup/1
        ]).

-export([upload_rate/2, upload_csv/2
        ,rate_did/3
        ,delete_rate/2
        ,get_rate/2
        ,get_rates/1, get_rates/2

        ,create_service_plan/2
        ,assign_service_plan/3
        ,rate_account_did/3
        ]).

-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
        ,correct_parallel/0
        ]).

-include_lib("proper/include/proper.hrl").
-include("kazoo_proper.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(RATE_ID, <<"XX-1222">>).
-define(RATEDECK_NAMES, [?KZ_RATES_DB, <<"custom">>]).
-define(PHONE_NUMBERS, [<<"+12223334444">>]).
-define(ACCOUNT_NAMES, [<<"account_for_rates">>]).

-define(GLOBAL_COST, 1).
-define(ACCOUNT_COST, 4).

-spec rate_doc(ne_binary() | proper_types:type(), number() | proper_types:type()) ->
                      kzd_rate:doc().
rate_doc(RatedeckId, Cost) ->
    kzd_rate:from_map(#{<<"prefix">> => <<"1222">>
                       ,<<"rate_cost">> => Cost
                       ,<<"ratedeck_id">> => RatedeckId
                       ,<<"direction">> => <<"inbound">>
                       }
                     ).

-spec upload_rate(pqc_cb_api:state(), kzd_rate:doc()) -> {'ok', api_ne_binary()}.
upload_rate(API, RateDoc) ->
    ?INFO("uploading rate ~p", [RateDoc]),
    CSV = kz_csv:from_jobjs([RateDoc]),
    upload_csv(API, CSV, kzd_rate:ratedeck(RateDoc)).

-spec upload_csv(pqc_cb_api:state(), iodata()) ->
                        {'ok', api_ne_binary()}.
-spec upload_csv(pqc_cb_api:state(), iodata(), api_ne_binary()) ->
                        {'ok', api_ne_binary()}.
upload_csv(API, CSV) ->
    upload_csv(API, CSV, 'undefined').

upload_csv(API, CSV, _RatedeckId) ->
    CreateResp = pqc_cb_tasks:create(API, "category=rates&action=import", CSV),
    TaskId = kz_json:get_ne_binary_value([<<"data">>, <<"_read_only">>, <<"id">>]
                                        ,kz_json:decode(CreateResp)
                                        ),
    _ExecResp = pqc_cb_tasks:execute(API, TaskId),

    _DelResp = wait_for_task(API, TaskId),

    {'ok', TaskId}.

-spec create_service_plan(pqc_cb_api:state(), ne_binary() | proper_types:type()) ->
                                 'ok' | {'error', any()}.
create_service_plan(API, RatedeckId) ->
    RatesResp = get_rates(API, RatedeckId),
    case kz_json:get_list_value(<<"data">>, kz_json:decode(RatesResp), []) of
        [] ->
            ?INFO("no rates in ratedeck ~s, not creating service plan", [RatedeckId]),
            {'error', 'no_ratedeck'};
        _Rates ->
            ?INFO("creating service plan for ~s", [RatedeckId]),
            case pqc_cb_service_plans:create_service_plan(API, ratedeck_service_plan(RatedeckId)) of
                {'ok', _} -> 'ok';
                {'error', 'conflict'} -> 'ok';
                Error -> Error
            end
    end.

-spec assign_service_plan(pqc_cb_api:state(), ne_binary() | proper_types:type(), ne_binary()) ->
                                 pqc_cb_api:response().
assign_service_plan(_API, 'undefined', _RatedeckId) ->
    ?INFO("no account to assign ~s to", [_RatedeckId]),
    ?FAILED_RESPONSE;
assign_service_plan(API, AccountId, RatedeckId) ->
    ?INFO("attempting to assign service plan for ~s to ~s", [RatedeckId, AccountId]),
    ServicePlanId = service_plan_id(RatedeckId),
    pqc_cb_service_plans:assign_service_plan(API, AccountId, ServicePlanId).

-spec rate_account_did(pqc_cb_api:state(), ne_binary() | proper_types:type(), ne_binary()) ->
                              api_integer().
rate_account_did(_API, 'undefined', _DID) ->
    ?INFO("account doesn't exist to rate DID ~p", [_DID]),
    ?FAILED_RESPONSE;
rate_account_did(API, AccountId, DID) ->
    ?INFO("rating DID ~p against account ~p", [DID, AccountId]),
    URL = string:join([pqc_cb_accounts:account_url(AccountId), "rates", "number", kz_term:to_list(DID)], "/"),
    make_rating_request(API, URL).

-spec ratedeck_service_plan(ne_binary() | kzd_rate:doc()) -> kzd_service_plan:doc().
ratedeck_service_plan(<<_/binary>> = RatedeckId) ->
    Plan = kz_json:from_list([{<<"ratedeck">>
                              ,kz_json:from_list([{RatedeckId, kz_json:new()}])
                              }
                             ]),
    Funs = [{fun kzd_service_plan:set_plan/2, Plan}],

    lists:foldl(fun({F, V}, Acc) -> F(Acc, V) end
               ,kz_json:from_list([{<<"_id">>, service_plan_id(RatedeckId)}
                                  ,{<<"pvt_type">>, <<"service_plan">>}
                                  ,{<<"name">>, <<RatedeckId/binary, " Ratedeck Service Plan">>}
                                  ])
               ,Funs
               );
ratedeck_service_plan(RateDoc) ->
    ratedeck_service_plan(kzd_rate:ratedeck(RateDoc)).

service_plan_id(RatedeckId) ->
    <<"plan_ratedeck_", RatedeckId/binary>>.

wait_for_task(API, TaskId) ->
    GetResp = pqc_cb_tasks:fetch(API, TaskId),
    case kz_json:get_value([<<"data">>, <<"_read_only">>, <<"status">>]
                          ,kz_json:decode(GetResp)
                          )
    of
        <<"success">> -> pqc_cb_tasks:delete(API, TaskId);
        _Status ->
            timer:sleep(1000),
            wait_for_task(API, TaskId)
    end.

-spec delete_rate(pqc_cb_api:state(), ne_binary() | kzd_rate:doc()) -> pqc_cb_api:response().
delete_rate(API, <<_/binary>>=RatedeckId) ->
    delete_rate(API, ?RATE_ID, RatedeckId);
delete_rate(API, RateDoc) ->
    delete_rate(API, ?RATE_ID, kzd_rate:ratedeck(RateDoc)).

-spec delete_rate(pqc_cb_api:state(), ne_binary(), ne_binary()) -> pqc_cb_api:response().
delete_rate(API, ID, <<_/binary>>=RatedeckId) ->
    ?INFO("deleting rate ~s from ~s", [ID, RatedeckId]),

    URL = rate_url(ID, RatedeckId),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:delete/2
                           ,URL ++ "&should_soft_delete=false"
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec get_rate(pqc_cb_api:state(), kzd_rate:doc()) -> pqc_cb_api:response().
get_rate(API, RateDoc) ->
    ID = kz_doc:id(RateDoc),

    ?INFO("getting rate info for ~s in ~s", [ID, kzd_rate:ratedeck(RateDoc)]),

    URL = rate_url(ID, kzd_rate:ratedeck(RateDoc)),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:get/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec get_rates(pqc_cb_api:state()) -> pqc_cb_api:response().
-spec get_rates(pqc_cb_api:state(), ne_binary()) -> pqc_cb_api:response().
get_rates(API) ->
    get_rates(API, ?KZ_RATES_DB).
get_rates(API, RatedeckId) ->
    ?INFO("getting rates for ratedeck ~s", [RatedeckId]),
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,rates_url() ++ "?ratedeck_id=" ++ kz_term:to_list(RatedeckId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec rate_did(pqc_cb_api:state(), ne_binary(), ne_binary()) -> api_integer().
rate_did(API, RatedeckId, DID) ->
    ?INFO("rating DID ~s using ~s", [DID, RatedeckId]),
    URL = rate_number_url(RatedeckId, DID),

    make_rating_request(API, URL).

-spec make_rating_request(pqc_cb_api:state(), string()) -> api_integer().
make_rating_request(API, URL) ->
    RequestHeaders = pqc_cb_api:request_headers(API),

    Resp = pqc_cb_api:make_request([200, 500]
                                  ,fun kz_http:get/2
                                  ,URL
                                  ,RequestHeaders
                                  ),
    RespJObj = kz_json:decode(Resp),
    case kz_json:get_ne_binary_value(<<"status">>, RespJObj) of
        <<"error">> -> 'undefined';
        <<"success">> -> kz_json:get_float_value([<<"data">>, <<"Rate">>], RespJObj)
    end.

rates_url() ->
    string:join([pqc_cb_api:v2_base_url(), "rates"], "/").

rate_number_url(RatedeckId, DID) ->
    rate_did_url(pqc_cb_api:v2_base_url(), DID) ++ "?ratedeck_id=" ++ kz_term:to_list(RatedeckId).

rate_url(ID, RatedeckId) ->
    string:join([pqc_cb_api:v2_base_url(), "rates", kz_term:to_list(ID)], "/")
        ++ "?ratedeck_id=" ++ kz_term:to_list(RatedeckId).

rate_did_url(Base, DID) ->
    string:join([Base, "rates", "number", kz_term:to_list(kz_http_util:urlencode(DID))], "/").

-spec correct() -> any().
correct() ->
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   timer:sleep(1000),
                   try run_commands(?MODULE, Cmds) of
                       {History, Model, Result} ->
                           cleanup(pqc_kazoo_model:api(Model)),
                           ?WHENFAIL(io:format("Final Model:~n~p~n~nFailing Cmds:~n~p~n"
                                              ,[pqc_kazoo_model:pp(Model), zip(Cmds, History)]
                                              )
                                    ,aggregate(command_names(Cmds), Result =:= 'ok')
                                    )
                   catch
                       _E:_R ->
                           ST = erlang:get_stacktrace(),
                           io:format("exception running commands: ~s:~p~n", [_E, _R]),
                           [io:format("~p~n", [S]) || S <- ST],
                           cleanup(),
                           'false'
                   end

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
                   cleanup(),

                   ?WHENFAIL(io:format("S: ~p~nP: ~p~n", [Sequential, Parallel])
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

-spec init() -> 'ok'.
init() ->
    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar', 'hotornot', 'tasks']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_tasks', 'cb_rates', 'cb_accounts']
        ],
    ?INFO("INIT FINISHED").

-spec cleanup() -> any().
-spec cleanup(pqc_cb_api:state()) -> any().
cleanup() ->
    ?INFO("CLEANUP ALL THE THINGS"),
    kz_data_tracing:clear_all_traces(),
    pqc_cb_service_plans:cleanup(),
    cleanup(pqc_cb_api:authenticate()).

cleanup(API) ->
    ?INFO("CLEANUP TIME, EVERYBODY HELPS"),
    _ = [?MODULE:delete_rate(API, RatedeckId) || RatedeckId <- ?RATEDECK_NAMES],
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _DelSPs = [pqc_cb_service_plans:delete_service_plan(API, service_plan_id(RatedeckId)) || RatedeckId <- ?RATEDECK_NAMES],

    pqc_cb_api:cleanup(API).

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    init(),
    API = pqc_cb_api:authenticate(),
    ?INFO("state initialized to ~p", [API]),
    pqc_kazoo_model:new(API).

-spec seq() -> any().
seq() ->
    init(),
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    try
        RateDoc = rate_doc(<<"custom">>, 1),

        _Up = ?MODULE:upload_rate(API, RateDoc),
        ?INFO("upload: ~p~n", [_Up]),

        _Get = ?MODULE:get_rate(API, RateDoc),
        ?INFO("get: ~p~n", [_Get]),

        _Rated = ?MODULE:rate_did(API, kzd_rate:ratedeck(RateDoc), hd(?PHONE_NUMBERS)),
        ?INFO("rated: ~p~n", [_Rated]),

        _SP = ?MODULE:create_service_plan(API, kzd_rate:ratedeck(RateDoc)),
        ?INFO("created sp: ~p~n", [_SP]),

        AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
        AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

        case is_binary(AccountId) of
            'true' -> ?INFO("created account ~s~n", [AccountId]);
            'false' ->
                ?INFO("failed to get account id from ~s~n", [AccountResp]),
                throw('no_account_id')
        end,

        RatedeckId = kzd_rate:ratedeck(RateDoc),

        _Assigned = ?MODULE:assign_service_plan(API, AccountId, kzd_rate:ratedeck(RateDoc)),
        case kz_json:get_value([<<"data">>, <<"plan">>, <<"ratedeck">>, RatedeckId]
                              ,kz_json:decode(_Assigned)
                              )
        of
            'undefined' ->
                ?ERROR("failed to assign service plan for ~s to account ~s", [RatedeckId, AccountId]),
                throw('no_plan');
            _ ->
                ?INFO("assigned service plan to account: ~p~n", [_Assigned])
        end,

        _AcctRated = ?MODULE:rate_account_did(API, AccountId, hd(?PHONE_NUMBERS)),
        ?INFO("rated ~s in account ~s: ~p~n", [hd(?PHONE_NUMBERS), AccountId, _AcctRated]),

        _Deleted = ?MODULE:delete_rate(API, RateDoc),
        ?INFO("deleted: ~p~n", [_Deleted])
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            ?INFO("crashed ~s: ~p~n", [_E, _R]),
            io:format("crashed ~s: ~p~n", [_E, _R]),
            [begin
                 ?INFO("s: ~p~n", [S]),
                 io:format("s: ~p~n", [S])
             end
             || S <- ST
            ]
    after
        cleanup(API),
        io:format("done: ~p~n", [API])
    end.

-spec command(any()) -> proper_types:type().
command(Model) ->
    API = pqc_kazoo_model:api(Model),

    AccountName = account_name(),
    AccountId = pqc_cb_accounts:symbolic_account_id(Model, AccountName),

    RateDoc = rate_doc(ratedeck_id(), rate_cost()),

    oneof([{'call', ?MODULE, 'upload_rate', [API, RateDoc]}
          ,{'call', ?MODULE, 'delete_rate', [API, ratedeck_id()]}
          ,{'call', ?MODULE, 'get_rate', [API, RateDoc]}
          ,{'call', ?MODULE, 'rate_did', [API, ratedeck_id(), phone_number()]}
          ,pqc_cb_accounts:command(Model, AccountName)
          ,{'call', ?MODULE, 'create_service_plan', [API, ratedeck_id()]}
          ,{'call', ?MODULE, 'assign_service_plan', [API, AccountId, ratedeck_id()]}
          ,{'call', ?MODULE, 'rate_account_did', [API, AccountId, phone_number()]}
          ]).

ratedeck_id() ->
    oneof(?RATEDECK_NAMES).

rate_cost() ->
    range(1,10).

phone_number() ->
    elements(?PHONE_NUMBERS).

account_name() ->
    oneof(?ACCOUNT_NAMES).

-spec next_state(pqc_kazoo_model:model(), any(), any()) -> pqc_kazoo_model:model().
next_state(Model, APIResp, {'call', _, 'create_account', _Args}=Call) ->
    pqc_cb_accounts:next_state(Model, APIResp, Call);
next_state(Model
          ,_APIResp
          ,{'call', _, 'upload_rate', [_API, RateDoc]}
          ) ->
    Ratedeck = kzd_rate:ratedeck(RateDoc, ?KZ_RATES_DB),
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:is_rate_missing/3, [Ratedeck, RateDoc]}
                           ,{fun pqc_kazoo_model:add_rate_to_ratedeck/3, [Ratedeck, RateDoc]}
                           ]);
next_state(Model
          ,_APIResp
          ,{'call', ?MODULE, 'get_rate', [_API, _RateDoc]}
          ) ->
    Model;
next_state(Model
          ,_APIResp
          ,{'call', _, 'delete_rate', [_API, RatedeckId]}
          ) ->
    RateDoc = rate_doc(RatedeckId, 0),
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:does_rate_exist/3, [RatedeckId, RateDoc]}
                           ,{fun pqc_kazoo_model:remove_rate_from_ratedeck/3, [RatedeckId, RateDoc]}
                           ]);
next_state(Model
          ,_APIResp
          ,{'call', _, 'rate_did', [_API, _RatedeckId, _PhoneNumber]}
          ) ->
    Model;
next_state(Model
          ,_APIResp
          ,{'call', ?MODULE, 'create_service_plan', [_API, RatedeckId]}
          ) ->
    ServicePlan = ratedeck_service_plan(RatedeckId),
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:does_ratedeck_exist/2, [RatedeckId]}
                           ,{fun pqc_kazoo_model:add_service_plan/2, [ServicePlan]}
                           ]
                          );
next_state(Model
          ,_APIResp
          ,{'call', ?MODULE, 'assign_service_plan', [_API, AccountId, RatedeckId]}
          ) ->
    PlanId = service_plan_id(RatedeckId),
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:does_account_exist/2, [AccountId]}
                           ,{fun pqc_kazoo_model:does_service_plan_exist/2, [PlanId]}
                           ,{fun pqc_kazoo_model:add_service_plan/3, [AccountId, ratedeck_service_plan(RatedeckId)]}
                           ]
                          );
next_state(Model
          ,_APIResp
          ,{'call', ?MODULE, 'rate_account_did', [_API, _AccountId, _DID]}
          ) ->
    Model.

-spec precondition(pqc_kazoo_model:model(), any()) -> boolean().
precondition(_Model, _Call) -> 'true'.

-spec postcondition(pqc_kazoo_model:model(), any(), any()) -> boolean().
postcondition(Model, Call, APIResult) ->
    case postcondition1(Model, Call, APIResult) of
        'true' -> 'true';
        'false' ->
            ?INFO("postcondition failed for ~p", [Call]),
            'false'
    end.

postcondition1(Model, {'call', _, 'create_account', _Args}=Call, APIResult) ->
    pqc_cb_accounts:postcondition(Model, Call, APIResult);
postcondition1(_Model
              ,{'call', _, 'upload_rate', [_API, _RateDoc]}
              ,{'ok', _TaskId}
              ) ->
    'true';
postcondition1(Model
              ,{'call', ?MODULE, 'get_rate', [_API, RateDoc]}
              ,FetchResp
              ) ->
    RatedeckId = kzd_rate:ratedeck(RateDoc),
    case pqc_kazoo_model:is_rate_missing(Model, RatedeckId, RateDoc) of
        'true' ->
            404 =:= kz_json:get_integer_value(<<"error">>, kz_json:decode(FetchResp));
        'false' ->
            Data = kz_json:get_json_value(<<"data">>, kz_json:decode(FetchResp), kz_json:new()),
            kz_json:all(fun({K, V}) ->
                                F = kz_term:to_atom(K),
                                V =:= kzd_rate:F(Data)
                        end
                       ,RateDoc
                       )
    end;
postcondition1(Model
              ,{'call', _, 'delete_rate', [_API, RatedeckId]}
              ,APIResult
              ) ->
    RateDoc = rate_doc(RatedeckId, 0),
    case pqc_kazoo_model:does_rate_exist(Model, RatedeckId, RateDoc) of
        'true' ->
            Resp = kz_json:decode(APIResult),
            <<"success">> =:= kz_json:get_ne_binary_value(<<"status">>, Resp)
                andalso kz_json:is_true([<<"data">>, <<"_read_only">>, <<"deleted">>], Resp);
        'false' ->
            404 =:= kz_json:get_integer_value(<<"error">>, kz_json:decode(APIResult))
    end;
postcondition1(Model
              ,{'call', _, 'rate_did', [_API, RatedeckId, PhoneNumber]}
              ,APIResult
              ) ->
    matches_cost(Model, RatedeckId, PhoneNumber, APIResult);
postcondition1(Model
              ,{'call', ?MODULE, 'create_service_plan', [_API, RatedeckId]}
              ,APIResult
              ) ->
    case pqc_kazoo_model:does_ratedeck_exist(Model, RatedeckId) of
        'true' ->
            ?INFO("ratedeck ~s exists, creating service plan should succeed: ~p"
                 ,[RatedeckId, APIResult]
                 ),
            'ok' =:= APIResult;
        'false' ->
            ?INFO("ratedeck ~s does not exist, creating service plan should fail: ~p"
                 ,[RatedeckId, APIResult]
                 ),
            {'error', 'no_ratedeck'} =:= APIResult
    end;
postcondition1(_Model
              ,{'call', ?MODULE, 'assign_service_plan', [_API, 'undefined', _RatedeckId]}
              ,?FAILED_RESPONSE
              ) ->
    ?INFO("not assigning ratedeck ~s to undefined account", [_RatedeckId]),
    'true';
postcondition1(Model
              ,{'call', ?MODULE, 'assign_service_plan', [_API, _AccountId, RatedeckId]}
              ,APIResult
              ) ->
    PlanId = service_plan_id(RatedeckId),
    case pqc_kazoo_model:does_service_plan_exist(Model, PlanId) of
        'true' ->
            ?INFO("model has service plan ~s, is assigned to account ~s: ~s", [PlanId, _AccountId, APIResult]),
            'undefined' =/=
                kz_json:get_value([<<"data">>, <<"plan">>, <<"ratedeck">>, RatedeckId]
                                 ,kz_json:decode(APIResult)
                                 );
        'false' ->
            ?INFO("model does not have service plan ~s, API should not have it listed: ~s", [PlanId, APIResult]),
            'undefined' =:=
                kz_json:get_value([<<"data">>, <<"plan">>, <<"ratedeck">>, RatedeckId]
                                 ,kz_json:decode(APIResult)
                                 )
    end;
postcondition1(_Model
              ,{'call', ?MODULE, 'rate_account_did', [_API, 'undefined', _DID]}
              ,?FAILED_RESPONSE
              ) ->
    'true';
postcondition1(Model
              ,{'call', ?MODULE, 'rate_account_did', [_API, AccountId, DID]}
              ,APIResult
              ) ->
    matches_service_plan_cost(Model, AccountId, DID, APIResult).

matches_service_plan_cost(Model, AccountId, DID, APIResult) ->
    case pqc_kazoo_model:has_service_plan_rate_matching(Model, AccountId, DID) of
        {'true', Cost} when is_number(APIResult) ->
            ?INFO("model rates ~s against account ~s as ~p, got ~p in API"
                 ,[DID, AccountId, Cost, wht_util:dollars_to_units(APIResult)]
                 ),
            Cost =:= wht_util:dollars_to_units(APIResult);
        {'true', _Cost} ->
            ?INFO("model rates ~s against account ~s as ~p, but got ~p in API"
                 ,[DID, AccountId, _Cost, APIResult]
                 ),
            'false';
        'false' ->
            ?INFO("model has no rate for ~s against ~s, got ~p from API"
                 ,[DID, AccountId, APIResult]
                 ),
            'undefined' =:= APIResult
    end.

matches_cost(Model, RatedeckId, DID, APIResult) ->
    case pqc_kazoo_model:has_rate_matching(Model, RatedeckId, DID) of
        {'true', Cost} when is_number(APIResult) ->
            ?INFO("model rates ~s as ~p, got ~p in API"
                 ,[DID, Cost, wht_util:dollars_to_units(APIResult)]
                 ),
            Cost =:= wht_util:dollars_to_units(APIResult);
        {'true', _Cost} ->
            ?INFO("model rates ~s as ~p, but got ~p in API"
                 ,[DID, _Cost, APIResult]
                 ),
            'false';
        'false' ->
            ?INFO("model has no rate for ~s, got ~p from API"
                 ,[DID, APIResult]
                 ),
            'undefined' =:= APIResult
    end.
