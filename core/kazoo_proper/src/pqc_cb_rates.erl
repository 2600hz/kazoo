-module(pqc_cb_rates).
-behaviour(proper_statem).

-export([seq/0]).

-export([upload_rate/2
        ,rate_account_did/3
        ,rate_global_did/2
        ,delete_rate/2
        ,get_rate/2
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
-include_lib("kazoo/include/kz_databases.hrl").

-define(ACCOUNT_NAMES, [<<"rated_account">>]).
-define(PHONE_NUMBERS, [<<"+12223334444">>]).

-define(GLOBAL_COST, 1).
-define(ACCOUNT_COST, 4).

-define(GLOBAL_RATE
       ,kzd_rate:from_map(#{<<"prefix">> => <<"1222">>
                           ,<<"rate_cost">> => ?GLOBAL_COST
                           }
                         )
       ).
-define(ACCOUNT_RATE(AccountId)
       ,lists:foldl(fun({K,V}, M) -> maps:update_with(K, fun(_) -> V end, V, M) end
                   ,?GLOBAL_RATE
                    [{<<"rate_cost">>, ?ACCOUNT_COST}
                    ,{<<"ratedeck_name">>, AccountId}
                    ]
                   )
       ).

-spec upload_rate(cb_pqc_api:state(), kz_json:object()) -> {'ok', ne_binary()}.
upload_rate(API, RateDoc) ->
    CSV = kz_csv:from_jobjs([RateDoc]),

    CreateResp = pqc_cb_tasks:create(API, "category=rates&action=import", CSV),
    TaskId = kz_json:get_value([<<"data">>, <<"_read_only">>, <<"id">>], kz_json:decode(CreateResp)),
    _ExecResp = pqc_cb_tasks:execute(API, TaskId),
    _DelResp = wait_for_task(API, TaskId),

    {'ok', TaskId}.

wait_for_task(API, TaskId) ->
    GetResp = pqc_cb_tasks:fetch(API, TaskId),
    case kz_json:get_value([<<"data">>, <<"_read_only">>, <<"status">>]
                          ,kz_json:decode(GetResp)
                          )
    of
        <<"success">> ->
            io:format('user', "task ~s completed~n", [TaskId]),
            pqc_cb_tasks:delete(API, TaskId);
        _Status ->
            timer:sleep(1000),
            wait_for_task(API, TaskId)
    end.

-spec delete_rate(cb_pqc_api:state(), kz_json:object()) -> 'ok'.
delete_rate(API, RateDoc) ->
    ID = kz_doc:id(RateDoc),
    URL = rate_url(ID),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:delete/2
                           ,URL ++ "?should_soft_delete=false"
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec get_rate(cb_pqc_api:state(), kz_json:object()) -> 'ok'.
get_rate(API, RateDoc) ->
    ID = kz_doc:id(RateDoc),
    URL = rate_url(ID),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:get/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec rate_account_did(cb_pqc_api:state(), api_ne_binary(), ne_binary()) -> integer() | 'undefined'.
rate_account_did(_API, 'undefined', _DID) -> 'undefined';
rate_account_did(API, AccountId, DID) ->
    rate_did(API, rate_url(AccountId, DID)).

rate_did(API, URL) ->
    RequestHeaders = pqc_cb_api:request_headers(API),

    Resp = pqc_cb_api:make_request([200, 500]
                                  ,fun kz_http:get/2
                                  ,URL
                                  ,RequestHeaders
                                  ),
    RespJObj = kz_json:decode(Resp),
    case kz_json:get_ne_binary_value(<<"status">>, RespJObj) of
        <<"error">> ->
            io:format("failed to find rate for DID: ~s~n", [Resp]),
            'undefined';
        <<"success">> -> kz_json:get_integer_value([<<"data">>, <<"Rate">>], RespJObj)
    end.

-spec rate_global_did(cb_pqc_api:state(), ne_binary()) -> pos_integer() | 'undefined'.
rate_global_did(API, DID) ->
    rate_did(API, rate_url(DID)).

rate_url(AccountId, DID) ->
    rate_did_url(pqc_cb_accounts:account_url(AccountId), DID).

rate_url(<<"XX-", _/binary>>=ID) ->
    string:join([pqc_cb_api:v2_base_url(), "rates", kz_term:to_list(ID)], "/");
rate_url(DID) ->
    rate_did_url(pqc_cb_api:v2_base_url(), DID).

rate_did_url(Base, DID) ->
    string:join([Base, "rates", "number", kz_term:to_list(kz_http_util:urlencode(DID))], "/").

-spec correct() -> any().
correct() ->
    init(),
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   {History, Model, Result} = run_commands(?MODULE, Cmds),

                   cleanup(pqc_kazoo_model:api(Model)),

                   ?WHENFAIL(io:format("Final Model : ~p~nFailing Cmds: ~p~n"
                                      ,[Model, zip(Cmds, History)]
                                      )
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

-spec correct_parallel() -> any().
correct_parallel() ->
    init(),
    ?FORALL(Cmds
           ,parallel_commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   {Sequential, Parallel, Result} = run_parallel_commands(?MODULE, Cmds),
                   cleanup(pqc_cb_api:authenticate()),

                   ?WHENFAIL(io:format("S: ~p~nP: ~p~n", [Sequential, Parallel])
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

init() ->
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar', 'hotornot', 'tasks']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_tasks', 'cb_rates', 'cb_accounts']
        ].

cleanup(API) ->
    pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    kt_cleanup:cleanup_soft_deletes(<<"accounts">>),
    ?MODULE:delete_rate(API, ?GLOBAL_RATE).

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    API = pqc_cb_api:authenticate(),
    cleanup(API),
    pqc_kazoo_model:new(API).

-spec seq() -> any().
seq() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),
    _Up = ?MODULE:upload_rate(API, ?GLOBAL_RATE),
    io:format("upload: ~p~n", [_Up]),

    _Get = ?MODULE:get_rate(API, ?GLOBAL_RATE),
    io:format("get: ~p~n", [_Get]),

    _Rated = ?MODULE:rate_global_did(API, hd(?PHONE_NUMBERS)),
    io:format("rated: ~p~n", [_Rated]),

    _Deleted = ?MODULE:delete_rate(API, ?GLOBAL_RATE),
    io:format("deleted: ~p~n", [_Deleted]).

-spec command(any()) -> proper_types:type().
-spec command(any(), boolean()) -> proper_types:type().
command(Model) ->
    command(Model, 'true').
%% command(Model, pqc_kazoo_model:has_accounts(Model)).

%% command(Model, 'false') ->
%%     {'call', 'pqc_cb_accounts', 'create_account', [pqc_kazoo_model:api(Model), name()]};
command(Model, 'true') ->
    API = pqc_kazoo_model:api(Model),
    AccountId = {'call', 'pqc_kazoo_model', 'account_id_by_name', [Model, name()]},

    oneof([{'call', ?MODULE, 'upload_rate', [API, rate(AccountId)]}
           %% ,{'call', ?MODULE, 'rate_account_did', [API, AccountId, phone_number()]}
          ,{'call', ?MODULE, 'rate_global_did', [API, phone_number()]}
           %% ,{'call', 'pqc_cb_accounts', 'create_account', [pqc_kazoo_model:api(Model), name()]}
          ]).

rate('undefined') -> ?GLOBAL_RATE;
rate(_AccountId) -> ?GLOBAL_RATE.
%% oneof([?ACCOUNT_RATE(AccountId), ?GLOBAL_RATE]).

name() ->
    elements(?ACCOUNT_NAMES).

phone_number() ->
    elements(?PHONE_NUMBERS).

-spec next_state(pqc_kazoo_model:model(), any(), any()) -> pqc_kazoo_model:model().
next_state(Model
          ,APIResp
          ,{'call', _, 'create_account', [_API, Name]}
          ) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:is_account_missing/2, [Name]}
                           ,{fun pqc_kazoo_model:add_account/3, [Name, APIResp]}
                           ]);
next_state(Model
          ,_APIResp
          ,{'call', _, 'upload_rate', [_API, RateDoc]}
          ) ->
    case kz_json:get_value(<<"account_id">>, RateDoc) of
        'undefined' ->
            pqc_util:transition_if(Model
                                  ,[{fun pqc_kazoo_model:is_system_rate_missing/2, [RateDoc]}
                                   ,{fun pqc_kazoo_model:add_rate_to_system/2, [RateDoc]}
                                   ]);
        AccountId ->
            pqc_util:transition_if(Model
                                  ,[{fun pqc_kazoo_model:does_account_exist/2, [AccountId]}
                                   ,{fun pqc_kazoo_model:is_account_rate_missing/3, [AccountId, RateDoc]}
                                   ,{fun pqc_kazoo_model:add_rate_to_account/3, [AccountId, RateDoc]}
                                   ])
    end;
next_state(Model
          ,_APIResp
          ,{'call', _, 'rate_account_did', [_API, _AccountId, _PhoneNumber]}
          ) ->
    Model;
next_state(Model
          ,_APIResp
          ,{'call', _, 'rate_global_did', [_API, _PhoneNumber]}
          ) ->
    Model.

-spec precondition(pqc_kazoo_model:model(), any()) -> boolean().
precondition(_Model, _Call) -> 'true'.

-spec postcondition(pqc_kazoo_model:model(), any(), any()) -> boolean().
postcondition(Model
             ,{'call', _, 'create_account', [_API, Name]}
             ,APIResult
             ) ->
    case pqc_kazoo_model:account_id_by_name(Model, Name) of
        'undefined' ->
            'undefined' =/= pqc_cb_response:account_id(APIResult);
        _AccountId ->
            500 =:= pqc_cb_response:error_code(APIResult)
    end;
postcondition(_Model
             ,{'call', _, 'upload_rate', [_API, _RateDoc]}
             ,{'ok', _TaskId}
             ) ->
    'true';
postcondition(Model
             ,{'call', _, 'rate_global_did', [_API, PhoneNumber]}
             ,APIResult
             ) ->
    matches_global_cost(Model, PhoneNumber, APIResult);
postcondition(_Model
             ,{'call', _, 'rate_account_did', [_API, 'undefined', _PhoneNumber]}
             ,'undefined'
             ) ->
    'true';
postcondition(Model
             ,{'call', _, 'rate_account_did', [_API, AccountId, PhoneNumber]}
             ,APIResult
             ) ->
    matches_account_cost(Model, AccountId, PhoneNumber, APIResult).

matches_account_cost(Model, AccountId, PhoneNumber, APIResult) ->
    case pqc_kazoo_model:has_account_rate_matching(Model, AccountId, PhoneNumber) of
        'true' -> APIResult =:= ?ACCOUNT_COST;
        'false' -> matches_global_cost(Model, PhoneNumber, APIResult)
    end.

matches_global_cost(Model, PhoneNumber, APIResult) ->
    io:format('user', "does model have rate for ~p: ~p~n~p == ~p~n"
             ,[PhoneNumber, pqc_kazoo_model:ratedeck(Model), APIResult, ?GLOBAL_COST]
             ),
    case pqc_kazoo_model:has_system_rate_matching(Model, PhoneNumber) of
        'true' -> APIResult =:= ?GLOBAL_COST;
        'false' -> APIResult =:= 'undefined'
    end.
