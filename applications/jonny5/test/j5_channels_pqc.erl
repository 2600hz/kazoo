%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @author James Aimonetti
%%% @doc PropEr testing of the j5_channels cache
%%% jonny5 instances can recv authz_req, CHANNEL_CREATE,
%%% broadcasted authz_resp, and CHANNEL_DESTROY events.  authz_req is
%%% sent in round-robin to running instances authz_resp is recv by all
%%% instances CHANNEL_CREATE/DESTROY are sent to every instance
%%%
%%% We should be able to model the state of a j5_channels instance to
%%% determine if it maintains the proper listing of channels and their
%%% authz status, avoiding stuck channels.
%%% @end
%%%-----------------------------------------------------------------------------
-module(j5_channels_pqc).
-ifdef(PROPER).

-behaviour(proper_statem).

-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
        ]).

-export([authz_resp/2
        ,channel_create/1
        ,channel_destroy/1
        ]).

-export([run_counterexample/0
        ,run_counterexample/1
        ]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("kazoo_events/include/kz_hooks.hrl").

-define(J5_CHANNELS, j5_channels).

%% {UUID => {IsAuthzd, IsDestroyed}}
-type authz_channels() :: #{kz_term:ne_binary() => {boolean(), boolean()}}.

-define(ACCOUNT_ID, <<"d2554b642d0082432e7a14546424cc1f">>).
-define(RESELLER_ID, <<"605d2d931165bbfe161c51e1626f93d8">>).

-spec authz_resp(kz_term:ne_binary(), boolean()) -> 'ok'.
authz_resp(CallId, IsAuthorized) ->
    ?J5_CHANNELS:handle_authz_resp(
       kz_json:from_list([{<<"Is-Authorized">>, kz_term:to_binary(IsAuthorized)}
                         ,{<<"Account-ID">>, ?ACCOUNT_ID}
                         ,{<<"Account-Billing">>, <<"flat_rate">>}
                         ,{<<"Reseller-ID">>, ?RESELLER_ID}
                         ,{<<"Reseller-Billing">>, <<"flat_rate">>}
                         ,{<<"Call-Direction">>, <<"outbound">>}
                         ,{<<"Call-ID">>, CallId}
                         ,{<<"Msg-ID">>, CallId}
                          | kz_api:default_headers(<<"authz">>, <<"authz_resp">>, <<?MODULE_STRING>>, <<"1">>)
                         ])
      ,[]
      ).

-spec channel_destroy(kz_term:ne_binary()) -> 'ok'.
channel_destroy(CallId) ->
    Event = kz_json:from_list(
              [{<<"Call-ID">>, CallId}
              ,{<<"Account-ID">>, ?ACCOUNT_ID}
              ,{<<"Call-Direction">>, <<"outbound">>}
              ,{<<"Event-Category">>, <<"call_event">>}
              ,{<<"Event-Name">>, <<"CHANNEL_DESTROY">>}
              ]
             ),
    whereis(?J5_CHANNELS) ! ?HOOK_EVT(?ACCOUNT_ID, <<"CHANNEL_DESTROY">>, Event),
    timer:sleep(10),
    'ok'.

-spec channel_create(kz_term:ne_binary()) -> 'ok'.
channel_create(CallId) ->
    Event = kz_json:from_list(
              [{<<"Call-ID">>, CallId}
              ,{<<"Account-ID">>, ?ACCOUNT_ID}
              ,{<<"Call-Direction">>, <<"outbound">>}
              ,{<<"Event-Category">>, <<"call_event">>}
              ,{<<"Event-Name">>, <<"CHANNEL_CREATE">>}
              ,{<<"To-Uri">>, <<"2600@twentysixhundred.com">>}
              ]
             ),
    whereis(?J5_CHANNELS) ! ?HOOK_EVT(?ACCOUNT_ID, <<"CHANNEL_CREATE">>, Event),
    timer:sleep(10),
    'ok'.

run_counterexample() ->
    run_counterexample(proper:counterexample()).

run_counterexample('undefined') -> 'undefined';
run_counterexample([SeqSteps]) ->
    run_counterexample(SeqSteps, initial_state()).

run_counterexample(SeqSteps, State) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(?MODULE),

    ?J5_CHANNELS:start_link(),

    try lists:foldl(fun transition_if/2
                   ,{1, State}
                   ,SeqSteps
                   )
    catch
        'throw':T -> {'throw', T}
    after
        ?J5_CHANNELS:stop()
    end.

transition_if({'set', _Var, Call}, {Step, State}) ->
    {'call', M, F, As} = Call,
    Resp = erlang:apply(M, F, As),
    io:format("~w: ~w -> ~w~n", [Step, Call, Resp]),
    print_state(State),

    case postcondition(State, Call, Resp) of
        'true' ->
            {Step+1, next_state(State, Resp, Call)};
        'false' ->
            io:format("failed on step ~p~n", [Step]),
            throw({'failed_postcondition', State, Call, Resp})
    end.

print_state(Channels) ->
    io:format("  channels: ~p~n", [Channels]).

correct() ->
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   kz_util:put_callid(?MODULE),
                   ?J5_CHANNELS:start_link(),
                   {History, State, Result} = run_commands(?MODULE, Cmds),
                   ?J5_CHANNELS:stop(),
                   ?WHENFAIL(io:format("Final State: ~p\nFailing Cmds: ~p\n"
                                      ,[State, zip(Cmds, History)]
                                      )
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

-spec initial_state() -> authz_channels().
initial_state() -> #{}.

command(Channels) when map_size(Channels) =:= 0 ->
    oneof([{'call', ?MODULE, 'channel_create', [call_id()]}
          ,{'call', ?J5_CHANNELS, 'total_calls', [?ACCOUNT_ID]}
          ,{'call', ?J5_CHANNELS, 'flush', []}
          ]);
command(Channels) ->
    oneof([{'call', ?MODULE, 'authz_resp', [call_id(Channels), is_authz()]}
          ,{'call', ?MODULE, 'channel_destroy', [call_id(Channels)]}
          ,{'call', ?J5_CHANNELS, 'total_calls', [?ACCOUNT_ID]}
          ]).

%% static for now
call_id() -> kz_binary:rand_hex(6).
call_id(Channels) ->
    [{CallId, _} | _] = kz_term:shuffle_list(maps:to_list(Channels)),
    CallId.

%% toggle whether to consider the channel authz'd
is_authz() -> boolean().

next_state(Channels
          ,_V %% symbolic or concrete resp
          ,{'call', ?MODULE, 'authz_resp', [CallId, IsAuthz]}
          ) ->
    case maps:get(CallId, Channels, 'undefined') of
        'undefined' -> Channels#{CallId => {IsAuthz, 'false'}};
        {'undefined', 'false'} -> Channels#{CallId => {IsAuthz, 'false'}};
        {_IsAuthz, _IsDestroyed} -> Channels
    end;
next_state(Channels
          ,_V %% symbolic or concrete resp
          ,{'call', ?MODULE, 'channel_create', [CallId]}
          ) ->
    case maps:get(CallId, Channels, 'undefined') of
        'undefined' -> Channels#{CallId => {'undefined', 'false'}};
        {_IsAuthz, 'false'} -> Channels
    end;
next_state(Channels
          ,_V %% symbolic or concrete resp
          ,{'call', ?MODULE, 'channel_destroy', [CallId]}
          ) ->
    case maps:get(CallId, Channels, 'undefined') of
        'undefined' -> Channels#{CallId => {'undefined', 'true'}};
        {IsAuthz, 'false'} -> Channels#{CallId => {IsAuthz, 'true'}};
        {_IsAuthz, 'true'} -> Channels
    end;
next_state(Channels, _V, {'call', ?J5_CHANNELS, 'total_calls', [?ACCOUNT_ID]}) ->
    Channels;
next_state(_Channels, _V, {'call', ?J5_CHANNELS, 'flush', []}) ->
    #{}.

precondition(Channels, {'call', ?MODULE, 'channel_destroy', [CallId]}) ->
    case maps:get(CallId, Channels, 'undefined') of
        {_, 'true'} -> 'false';
        _ -> 'true'
    end;
precondition(_Channels, _Call) -> 'true'.

postcondition(_Channels
             ,{'call', ?MODULE, 'authz_resp', [_CallId, _IsAuthz]}
             ,_Result %% concrete result
             ) ->
    'true';
postcondition(_Channels
             ,{'call', ?MODULE, 'channel_create', [_CallId]}
             ,_Result
             ) ->
    'true';
postcondition(_Channels
             ,{'call', ?MODULE, 'channel_destroy', [_CallId]}
             ,_Result
             ) ->
    'true';
postcondition(Channels
             ,{'call', ?J5_CHANNELS, 'total_calls', [?ACCOUNT_ID]}
             ,Result
             ) ->
    count_channels(Channels) =:= Result;
postcondition(_Channels
             ,{'call', ?J5_CHANNELS, 'flush', []}
             ,_Result
             ) ->
    'true'.

count_channels(Channels) ->
    maps:fold(fun count_channel/3, 0, Channels).

count_channel(_CallId, {'true', 'false'}, Count) -> Count + 1;
count_channel(_CallId, {'false', 'false'}, Count) -> Count + 1;
count_channel(_CallId, {'undefined', 'false'}, Count) -> Count + 1;
count_channel(_CallId, _Meta, Count) -> Count.

-endif.
