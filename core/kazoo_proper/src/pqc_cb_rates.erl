-module(pqc_cb_rates).
-behaviour(proper_statem).

-export([]).

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

-define(ACCOUNT_NAMES, [<<"rated_account">>]).
-define(PHONE_NUMBERS, [<<"+12223334444">>]).

-define(GLOBAL_COST, 1).
-define(ACCOUNT_COST, 4).

-define(GLOBAL_RATE
       ,kz_json:from_list([{<<"prefix">>, <<"1222">>}
                          ,{<<"rate_cost">>, ?GLOBAL_COST}
                          ]
                         )
       ).
-define(ACCOUNT_RATE(AccountId)
       ,kz_json:set_values([{<<"account_id">>, AccountId}
                           ,{<<"rate_cost">>, ?ACCOUNT_COST}
                           ]
                          ,?GLOBAL_RATE
                          )
       ).

-spec correct() -> any().
correct() ->
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   {History, Model, Result} = run_commands(?MODULE, Cmds),

                   pqc_cb_accounts:cleanup_accounts(pqc_kazoo_model:api(Model), ?ACCOUNT_NAMES),

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
-spec command(any(), boolean()) -> proper_types:type().
command(Model) ->
    command(Model, pqc_kazoo_model:has_accounts(Model)).

command(Model, 'false') ->
    {'call', 'pqc_cb_accounts', 'create_account', [pqc_kazoo_model:api(Model), name()]};
command(Model, 'true') ->
    API = pqc_kazoo_model:api(Model),
    AccountId = {'call', 'pqc_kazoo_model', 'account_id_by_name', [Model, name()]},

    oneof([{'call', ?MODULE, 'upload_account_rate', [API, AccountId, ?ACCOUNT_RATE(AccountId)]}
          ,{'call', ?MODULE, 'upload_global_rate', [API, ?GLOBAL_RATE]}
          ,{'call', ?MODULE, 'rate_account_did', [API, AccountId, phone_number()]}
          ,{'call', ?MODULE, 'rate_global_did', [API, phone_number()]}
          ,{'call', 'pqc_cb_accounts', 'create_account', [pqc_kazoo_model:api(Model), name()]}
           %% ,{'call', ?MODULE, 'reserve_number', [API, name(), phone_number()]}
          ]).

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
          ,{'call', _, 'upload_account_rate', [_API, AccountId, RateDoc]}
          ) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:does_account_exist/2, [AccountId]}
                           ,{fun pqc_kazoo_model:is_account_rate_missing/3, [AccountId, RateDoc]}
                           ,{fun pqc_kazoo_model:add_rate_to_account/3, [AccountId, RateDoc]}
                           ]);
next_state(Model
          ,_APIResp
          ,{'call', _, 'upload_global_rate', [_API, RateDoc]}
          ) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:is_system_rate_missing/2, [RateDoc]}
                           ,{fun pqc_kazoo_model:add_rate_to_system/2, [RateDoc]}
                           ]);
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
             ,{'call', _, 'upload_account_rate', [_API, _AccountId, _RateDoc]}
             ,APIResult
             ) ->
    APIResult =:= 'ok';
postcondition(_Model
             ,{'call', _, 'upload_global_rate', [_API, _RateDoc]}
             ,APIResult
             ) ->
    APIResult =:= 'ok';
postcondition(Model
             ,{'call', _, 'rate_global_did', [_API, PhoneNumber]}
             ,APIResult
             ) ->
    matches_global_cost(Model, PhoneNumber, APIResult);
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
    case pqc_kazoo_model:has_system_rate_matching(Model, PhoneNumber) of
        'true' -> APIResult =:= ?GLOBAL_COST;
        'false' -> APIResult =:= 'undefined'
    end.
