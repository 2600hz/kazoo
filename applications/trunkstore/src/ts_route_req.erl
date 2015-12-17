%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%% Handle route requests off AMQP
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ts_route_req).

-export([init/0
         ,handle_req/2
        ]).

-include("ts.hrl").

init() -> 'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(ApiJObj, _Options) ->
    'true' = wapi_route:req_v(ApiJObj),
    CallID = wh_json:get_value(<<"Call-ID">>, ApiJObj),
    wh_util:put_callid(CallID),
    lager:info("received a request asking if trunkstore can route this call"),
    case {wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], ApiJObj)
          ,wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], ApiJObj)
         }
    of
        {AcctID, 'undefined'} when is_binary(AcctID) ->
            %% Coming from carrier (off-net)
            lager:info("call began from outside the network"),
            ts_offnet_sup:start_handler(<<"offnet-", CallID/binary>>, ApiJObj);
        {AcctID, AuthID} when is_binary(AcctID) andalso is_binary(AuthID) ->
            %% Coming from PBX (on-net); authed by Registrar or ts_auth
            lager:info("call began on the network"),
            ts_onnet_sup:start_handler(<<"onnet-", CallID/binary>>, ApiJObj);
        {_AcctID, _AuthID} ->
            lager:debug("insufficient information available to lookup routing, ignoring")
    end.
