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
    j5_util:preload_accounts(),
    j5_util:preload_trunkstrore().

handle_req(JObj, _Props) ->
    case wh_api:authz_req_v(JObj) of
	false -> throw({failed_api_validation, JObj});
	true -> ?LOG("Valid authz_req")
    end,

    wh_util:put_callid(JObj),

    ?LOG("Authorize ~s can make the call to ~s", [wh_json:get_value(<<"From">>, JObj), wh_json:get_value(<<"To">>, JObj)]),

    AuthZResp = case {wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj)
		      ,wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], JObj)
		     } of
		    {AcctID, undefined} when is_binary(AcctID) ->
			%% Coming from carrier (off-net)
			?LOG("Authorize inbound call"),
			j5_acctmgr:authz_trunk(AcctID, JObj, inbound);
		    {AcctID, AuthID} when is_binary(AcctID) andalso is_binary(AuthID) ->
			%% Coming from PBX (on-net); authed by Registrar
			?LOG("Authorize outbound call"),
			j5_acctmgr:authz_trunk(AcctID, JObj, outbound);
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
