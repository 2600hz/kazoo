%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Look up IP for authorization/replaying of route_req
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(reg_route_req).

-export([init/0
        ,handle_route_req/2
        ,lookup_account_by_ip/1
        ]).

-include("reg.hrl").

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_route_req(kz_json:object(), kz_term:proplist()) -> any().
handle_route_req(JObj, _Props) ->
    'true' = kapi_route:req_v(JObj),
    kz_log:put_callid(JObj),

    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
    case kz_json:get_ne_value(<<"Account-ID">>, CCVs) of
        'undefined' -> maybe_replay_route_req(JObj, CCVs);
        _AccountId -> 'ok'
    end.

-spec maybe_replay_route_req(kz_json:object(), kz_json:object()) -> 'ok'.
maybe_replay_route_req(JObj, CCVs) ->
    maybe_replay_route_req(JObj, CCVs, kz_json:get_value(<<"From-Network-Addr">>, JObj)).

-spec maybe_replay_route_req(kz_json:object(), kz_json:object(), kz_term:api_binary()) -> 'ok'.
maybe_replay_route_req(_JObj, _CCVs, 'undefined') ->
    lager:debug("failing to reply route req with no IP to use");
maybe_replay_route_req(JObj, CCVs, IP) ->
    case lookup_account_by_ip(IP) of
        {'ok', AccountCCVs} ->
            lager:debug("route req was missing account information, loading from IP ~s and replaying", [IP]),
            kapi_route:publish_req(
              kz_json:set_value(<<"Custom-Channel-Vars">>
                               ,kz_json:set_values(AccountCCVs, CCVs)
                               ,JObj
                               )
             );
        {'error', _E} ->
            lager:debug("failed to find account information from IP ~s, not replaying route req", [IP])
    end.


%%------------------------------------------------------------------------------
%% @doc lookup auth by IP in cache/database and return the result
%% @end
%%------------------------------------------------------------------------------
-spec lookup_account_by_ip(kz_term:ne_binary()) ->
                                  {'ok', kz_term:proplist()} |
                                  {'error', 'not_found'}.
lookup_account_by_ip(IP) ->
    lager:debug("looking up IP: ~s in db ~s", [IP, ?KZ_SIP_DB]),
    kapps_util:get_ccvs_by_ip(IP).
