%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
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

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_req(kz_json:object(), kz_proplist()) -> any().
handle_req(ApiJObj, _Options) ->
    kz_util:put_callid(ApiJObj),
    'true' = kapi_route:req_v(ApiJObj),
    CallID = kz_json:get_value(<<"Call-ID">>, ApiJObj),
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, ApiJObj, kz_json:new()),
    lager:info("received request ~s asking if trunkstore can route this call", [kapi_route:fetch_id(ApiJObj)]),
    case {kz_json:get_value(<<"Account-ID">>, CCVs)
         ,kz_json:get_first_defined([<<"Authorizing-ID">>, <<"Referred-By">>, <<"Redirected-By">>], CCVs)
         }
    of
        {AcctID, 'undefined'} when is_binary(AcctID) ->
            %% Coming from carrier (off-net)
            lager:info("call with fetch-id ~s began from outside the network", [kapi_route:fetch_id(ApiJObj)]),
            ts_offnet_sup:start_handler(<<"offnet-", CallID/binary>>, ApiJObj);
        {AcctID, AuthID} when is_binary(AcctID)
                              andalso is_binary(AuthID) ->
            %% Coming from PBX (on-net); authed by Registrar or ts_auth
            lager:info("call with fetch-id ~s began on the network", [kapi_route:fetch_id(ApiJObj)]),
            ts_onnet_sup:start_handler(<<"onnet-", CallID/binary>>, ApiJObj);
        {_AcctID, _AuthID} ->
            lager:debug("insufficient information available to lookup routing for ~s, ignoring", [kapi_route:fetch_id(ApiJObj)])
    end.
