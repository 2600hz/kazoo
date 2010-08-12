-module(ecallmgr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_call/1, end_call/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Ref, Args), {Ref, {I, start_link, [{Ref, Args}]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ecallmgr_sup:start_link().

stop(_State) ->
    ok.

start_call(Args) ->
    Ref = make_ref(),
    io:format("ECALLMGR_APP: Ref: ~p Args: ~p~n", [Ref, Args]),
    ChildSpec = ?CHILD(ecallmgr_call, worker, Ref, Args),
    io:format("ECALLMGR_APP: ChildSpec: ~p~n", [ChildSpec]),
    _Res = supervisor:start_child(ecallmgr_call_sup, ChildSpec),
    io:format("ECALLMGR_APP: Res: ~p~n", [_Res]).

end_call(Ref) ->
    supervisor:terminate_child(ecallmgr_call_sup, Ref),
    supervisor:delete_child(ecallmgr_call_sup, Ref).
