%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_authz_win).

-export([handle_req/2]).

-include("jonny5.hrl").

-spec handle_req/2 :: (wh_json:json_object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    true = wapi_authz:win_v(JObj),
    wh_util:put_callid(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case wh_cache:peek_local(?JONNY5_CACHE, ?MONITOR_CALL(CallId)) of
        {error, not_found} -> ok;
        {ok, V} ->
            io:format("SHOULD MONITOR ~s:~n~p~n~p~n", [CallId, V, JObj])
    end.
