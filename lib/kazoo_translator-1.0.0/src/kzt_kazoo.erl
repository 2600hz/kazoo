%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% The Kazoo version of the Pivot APIs
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%
%%% @todo
%%%-------------------------------------------------------------------
-module(kzt_kazoo).

-export([does_recognize/1, exec/2, req_params/1]).

-include("kzt.hrl").

-spec does_recognize/1 :: (string()) -> {'true', term()} | 'false'.
does_recognize(Cmds) ->
    try wh_json:decode(wh_util:to_binary(Cmds)) of
        JObjs -> {true, JObjs}
    catch
        _E:_R ->
            lager:debug("don't recognize: ~s ~p", [_E, _R]),
            false
    end.

-spec exec/2 :: (whapps_call:call(), wh_json:objects()) -> exec_return().
exec(Call, JObjs) ->
    try exec_response(Call, JObjs) of
        Resp -> Resp
    catch
        _C:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to exec: ~p: ~p", [_C, _R]),
            _ = [lager:debug("st: ~p", [S]) || S <- ST],
            {stop, kzt_util:update_call_status(Call, ?STATUS_FAILED)}
    end.

exec_response(Call, JObjs) ->
    {ok, Call}.

-spec req_params/1 :: (whapps_call:call()) -> wh_proplist().
req_params(Call) ->
    [{<<"CallSid">>, whapps_call:call_id(Call)}
     ,{<<"AccountSid">>, whapps_call:account_id(Call)}
     ,{<<"From">>, whapps_call:from_user(Call)}
     ,{<<"To">>, whapps_call:to_user(Call)}
     ,{<<"CallStatus">>, whapps_call:kvs_fetch(<<"call_status">>, ?STATUS_RINGING, Call)}
     ,{<<"ApiVersion">>, <<"2010-04-01">>}
     ,{<<"Direction">>, <<"inbound">>}
     ,{<<"CallerName">>, whapps_call:caller_id_name(Call)}
    ].
