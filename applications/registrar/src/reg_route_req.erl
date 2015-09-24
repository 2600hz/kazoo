%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% Look up IP for authorization/replaying of route_req
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(reg_route_req).

-export([init/0
         ,handle_route_req/2
        ]).

-include("reg.hrl").

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_route_req(wh_json:object(), wh_proplist()) -> any().
handle_route_req(JObj, _Props) ->
    'true' = wapi_route:req_v(JObj),
    wh_util:put_callid(JObj),

    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    case wh_json:get_ne_value(<<"Account-ID">>, CCVs) of
        'undefined' -> maybe_replay_route_req(JObj, CCVs);
        _AccountId -> 'ok'
    end.

-spec maybe_replay_route_req(wh_json:object(), wh_json:object()) -> 'ok'.
-spec maybe_replay_route_req(wh_json:object(), wh_json:object(), api_binary()) -> 'ok'.
maybe_replay_route_req(JObj, CCVs) ->
    maybe_replay_route_req(JObj, CCVs, wh_json:get_value(<<"From-Network-Addr">>, JObj)).

maybe_replay_route_req(_JObj, _CCVs, 'undefined') ->
    lager:debug("failing to reply route req with no IP to use");
maybe_replay_route_req(JObj, CCVs, IP) ->
    case reg_authn_req:lookup_account_by_ip(IP) of
        {'ok', AccountCCVs} ->
            lager:debug("route req was missing account information, loading from IP ~s and replaying", [IP]),
            wapi_route:publish_req(
              wh_json:set_value(<<"Custom-Channel-Vars">>
                                ,wh_json:set_values(AccountCCVs, CCVs)
                                ,JObj
                               )
             );
        {'error', _E} ->
            lager:debug("failed to find account information from IP ~s, not replaying route req", [IP])
    end.
