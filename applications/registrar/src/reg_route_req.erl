%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% Look up IP for authorization/replaying of route_req
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(reg_route_req).

-export([init/0, handle_route_req/2]).

-include("reg.hrl").

init() -> whapps_maintenance:refresh(?WH_SIP_DB).

-spec handle_route_req(wh_json:object(), wh_proplist()) -> any().
handle_route_req(JObj, _Props) ->
    'true' = wapi_route:req_v(JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    case wh_json:get_ne_value(<<"Account-ID">>, CCVs) of
        'undefined' ->  maybe_replay_route_req(JObj);
        _AccountId -> 'ok'
    end.

-spec maybe_replay_route_req(wh_json:object()) -> any().
maybe_replay_route_req(JObj) ->
    case wh_json:get_value(<<"From-Network-Addr">>, JObj) of
        'undefined' -> 'ok';
        IP -> maybe_replay_route_req(JObj, IP)
    end.

-spec maybe_replay_route_req(wh_json:object(), api_binary()) -> any().
maybe_replay_route_req(JObj, IP) ->
    lager:debug("trying to see if this route req is an auth-by-ip'd device: ~s", [IP]),
    ViewOptions = [{'key', IP}],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup_by_ip">>, ViewOptions) of
        {'ok', []} -> lager:debug("no entry in ~s for IP ~s", [?WH_SIP_DB, IP]);
        {'ok', [Doc|_]} -> replay_route_req(JObj, Doc);
        {'error', _E} -> lager:debug("failed to lookup by ip: ~s: ~p", [IP, _E])
    end.

-spec replay_route_req(wh_json:object(), wh_json:object()) -> any().
replay_route_req(JObj, Doc) ->
    AccountID = wh_json:get_value([<<"value">>,  <<"account_id">>], Doc),
    OwnerID = wh_json:get_value([<<"value">>, <<"owner_id">>], Doc),
    AuthType = wh_json:get_value([<<"value">>, <<"authorizing_type">>], Doc, <<"anonymous">>),
    lager:debug("adding account ~s and owner ~s to ccvs", [AccountID, OwnerID]),
    CCVs = wh_json:set_values(
             props:filter_undefined(
               [{<<"Account-ID">>, AccountID}
                ,{<<"Owner-ID">>, OwnerID}
                ,{<<"Authorizing-ID">>, wh_json:get_value(<<"id">>, Doc)}
                ,{<<"Authorizing-Type">>, AuthType}
               ])
             ,wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())),
    lager:debug("replaying route_req"),
    wapi_route:publish_req(wh_json:set_value(<<"Custom-Channel-Vars">>, CCVs, JObj)).
