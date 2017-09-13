-module(pqc_cb_ips).
-behaviour(proper_statem).

-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
        ,correct_parallel/0
        ]).

-export([cleanup/0, cleanup/1]).

-include_lib("proper/include/proper.hrl").
-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<"account_for_ips">>]).

-spec cleanup() -> any().
-spec cleanup(pqc_cb_api:state()) -> any().
cleanup() ->
    ?INFO("CLEANUP ALL THE THINGS"),
    kz_data_tracing:clear_all_traces(),
    pqc_cb_service_plans:cleanup(),
    cleanup(pqc_cb_api:authenticate()).

cleanup(API) ->
    ?INFO("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),

    pqc_cb_api:cleanup(API).

-spec command(any()) -> proper_types:type().
command(Model) ->
    _API = pqc_kazoo_model:api(Model),

    AccountName = account_name(),
    _AccountId = pqc_cb_accounts:symbolic_account_id(Model, AccountName),

    oneof([pqc_cb_accounts:command(Model, AccountName)]).

account_name() ->
    oneof(?ACCOUNT_NAMES).

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    API = pqc_cb_api:authenticate(),
    ?INFO("state initialized to ~p", [API]),
    pqc_kazoo_model:new(API).

-spec next_state(pqc_kazoo_model:model(), any(), any()) -> pqc_kazoo_model:model().
next_state(Model, APIResp, {'call', _, 'create_account', _Args}=Call) ->
    pqc_cb_accounts:next_state(Model, APIResp, Call).

-spec precondition(pqc_kazoo_model:model(), any()) -> boolean().
precondition(_Model, _Call) -> 'true'.

-spec postcondition(pqc_kazoo_model:model(), any(), any()) -> boolean().
postcondition(Model, {'call', _, 'create_account', _Args}=Call, APIResult) ->
    pqc_cb_accounts:postcondition(Model, Call, APIResult).

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
