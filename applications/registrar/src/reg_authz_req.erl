%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Handle authz_req messages
%%% @end
%%% @contributors
%%%   David Singer (cando)
%%%-------------------------------------------------------------------
-module(reg_authz_req).

-export([init/0
         ,handle_req/2
	]).

-include("reg.hrl").

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_authz:authz_req_v(JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),

    case wh_json:get_ne_value(<<"Account-ID">>, CCVs) of
	'undefined' ->
            handle_missing_account_id(JObj, CCVs);
        _AccountId -> 'ok'
    end.

-spec handle_missing_account_id(wh_json:object(), wh_json:object()) -> 'ok'.
-spec handle_missing_account_id(wh_json:object(), wh_json:object(), api_binary()) -> 'ok'.
handle_missing_account_id(JObj, CCVs) ->
    wh_util:put_callid(JObj),
    handle_missing_account_id(JObj, CCVs, wh_json:get_value(<<"From-Network-Addr">>, JObj)).

handle_missing_account_id(_JObj, _CCVs, 'undefined') ->
    lager:debug("failed to find account information since there was no IP to use");
handle_missing_account_id(JObj, CCVs, IP) ->
    case reg_authn_req:lookup_account_by_ip(IP) of
        {'ok', AccountCCVs} ->
            lager:debug("authz request was missing account information, loading from IP ~s and replaying", [IP]),
            wapi_authz:publish_authz_req(
              wh_json:set_value(<<"Custom-Channel-Vars">>
                                ,wh_json:set_values(AccountCCVs, CCVs)
                                ,JObj
                               )
             );
        {'error', _E} ->
            lager:debug("failed to find account information from IP ~s, not replaying authz req", [IP])
    end.
