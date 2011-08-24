%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle authz_req AMQP message
%%% @end
%%% Created : 22 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(authz_req).

-export([init/0, handle_req/2]).

-include("jonny5.hrl").

init() ->
    {ok, Accts} = couch_mgr:get_results(<<"accounts">>, <<"accounts/listing_by_id">>, []),
    ?LOG_SYS("Preloading ~b accounts", [length(Accts)]),
    _ = [ jonny5_acct_sup:start_proc(wh_json:get_value(<<"id">>, AcctJObj)) || AcctJObj <- Accts],
    {ok, TSAccts} = couch_mgr:get_results(<<"ts">>, <<"LookUpDID/DIDsByAcct">>, []), %% crappy way, make new view
    ?LOG_SYS("Preloading ~b trunkstore accounts", [length(TSAccts)]),
    [ jonny5_acct_sup:start_proc(wh_json:get_value(<<"id">>, TSAcctJObj)) || TSAcctJObj <- TSAccts].

handle_req(JObj, Props) ->
    CPid = props:get_value(cache, Props),
    true = wh_api:authz_req_v(JObj),
    put(callid, wh_json:get_value(<<"Call-ID">>, JObj)),

    ?LOG("Authorize ~s can make the call to ~s", [wh_json:get_value(<<"From">>, JObj), wh_json:get_value(<<"To">>, JObj)]),

    AuthZResp = case {wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj)
		      ,wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], JObj)
		     } of
		    {AcctID, undefined} when is_binary(AcctID) ->
			%% Coming from carrier (off-net)
			?LOG("Authorize inbound call"),
			j5_acctmgr:authz_trunk(AcctID, JObj, inbound, CPid);
		    {AcctID, AuthID} when is_binary(AcctID) andalso is_binary(AuthID) ->
			%% Coming from PBX (on-net); authed by Registrar
			?LOG("Authorize outbound call"),
			j5_acctmgr:authz_trunk(AcctID, JObj, outbound, CPid);
		    {_AcctID, _AuthID} ->
			?LOG("Error in authorization: AcctID: ~s AuthID: ~s", [_AcctID, _AuthID]),
			undefined
		end,
    send_resp(JObj, AuthZResp).

send_resp(_JObj, undefined) ->
    ?LOG_END("No response for authz");
send_resp(JObj, {AuthzResp, CCV}) ->
    ?LOG_SYS("AuthzResp: ~s", [AuthzResp]),
    ?LOG_SYS("CCVs: ~p", [CCV]),

    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),

    Prop = [{<<"Is-Authorized">>, wh_util:to_binary(AuthzResp)}
	     ,{<<"Custom-Channel-Vars">>, {struct, CCV}}
	     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
	     ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
	     | wh_api:default_headers(<<>>, <<"dialplan">>, <<"authz_resp">>, ?APP_NAME, ?APP_VSN)
	    ],

    {ok, JSON} = wh_api:authz_resp(Prop),
    ?LOG_END("Sending authz resp: ~s", [JSON]),
    amqp_util:targeted_publish(RespQ, JSON, <<"application/json">>).
