-module(bh_limit).
-include("blackhole.hrl").
-export([init/0, connection/3, message/3]).

init() ->
    blackhole_bindings:bind(<<"message">>, ?MODULE, 'message'),
    blackhole_bindings:bind(<<"authenticated">>, ?MODULE, 'connection').

connection(Context=#bh_context{auth_account_id=AuthAccountId, websocket_pid=WsPid}, <<"authenticate">>, _JMsg) ->
    lager:debug("check connections per account limit: ~p", [AuthAccountId]),
    try
        blackhole_limit:account(AuthAccountId),
        blackhole_util:send_success(WsPid, <<"successfully authenticated">>),
        Context
    catch
        _:_ -> {'error', <<"account connections limit">>}
    end.

message(Context=#bh_context{websocket_pid=Pid}, _Cmd, _JMsg) ->
    lager:debug("check message rate per connection limit: ~p", [Pid]),
    try
        blackhole_limit:conn_rate(Pid),
        Context
    catch
        _:_ -> {'error', <<"message rate limit">>}
    end.
