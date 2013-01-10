%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Handle route requests off AMQP
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ts_route_req).

-export([init/0, handle_req/2]).

-include("ts.hrl").

init() ->
    ok.

-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> {'ok', pid()} | 'ok'.
handle_req(ApiJObj, _Options) ->
    true = wapi_route:req_v(ApiJObj),
    CallID = wh_json:get_value(<<"Call-ID">>, ApiJObj),
    put(callid, CallID),

    case {wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], ApiJObj), wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], ApiJObj)} of
        {AcctID, undefined} when is_binary(AcctID) ->
            %% Coming from carrier (off-net)
            lager:info("offnet call starting"),
            ts_offnet_sup:start_handler(<<"offnet-", CallID/binary>>, ApiJObj);
        {AcctID, AuthID} when is_binary(AcctID) andalso is_binary(AuthID) ->
            %% Coming from PBX (on-net); authed by Registrar or ts_auth
            lager:info("onnet call starting"),
            ts_onnet_sup:start_handler(<<"onnet-", CallID/binary>>, ApiJObj);
        {_AcctID, _AuthID} ->
            lager:info("error in routing: AcctID: ~s AuthID: ~s", [_AcctID, _AuthID])
    end.
