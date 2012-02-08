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
    _ = j5_util:preload_accounts(),
    j5_util:preload_trunkstore().

handle_req(JObj, _Props) ->
    case wapi_authz:req_v(JObj) of
        false -> throw({failed_api_validation, JObj});
        true -> ?LOG("valid authz_req")
    end,

    wh_util:put_callid(JObj),

    E164 = get_dest_number(JObj),

    ?LOG("authorize ~s can make the call to ~s", [wh_json:get_value(<<"From">>, JObj), E164]),

    AuthZResp = case {wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj)
                      ,wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], JObj)
                     } of
                    {AcctID, undefined} when is_binary(AcctID) ->
                        %% Coming from carrier (off-net)
                        ?LOG("trying to authorize inbound call"),
                        j5_acctmgr:authz_trunk(AcctID, JObj, inbound);
                    {AcctID, AuthID} when is_binary(AcctID) andalso is_binary(AuthID) ->
                        %% Coming from PBX (on-net); authed by Registrar
                        ?LOG("trying to authorize outbound call"),
                        j5_acctmgr:authz_trunk(AcctID, JObj, outbound);
                    {_AcctID, _AuthID} ->
                        case wh_number_manager:lookup_account_by_number(E164) of
                            {ok, AcctID, _} ->
                                ?LOG("found account id ~s for ~s for inbound call", [AcctID, E164]),
                                j5_acctmgr:authz_trunk(AcctID, JObj, inbound);
                            _ ->
                                ?LOG("error in finding authorization: AcctID: ~s AuthID: ~s", [_AcctID, _AuthID]),
                                undefined
                        end
                end,
    send_resp(JObj, AuthZResp).

send_resp(_JObj, undefined) ->
    ?LOG_END("No response for authz");
send_resp(JObj, {AuthzResp, CCV}) ->
    ?LOG_SYS("sending authz response ~s", [AuthzResp]),

    Resp = [{<<"Is-Authorized">>, wh_util:to_binary(AuthzResp)}
            ,{<<"Custom-Channel-Vars">>, wh_json:from_list(props:delete(<<"Server-ID">>, CCV))}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            ,{<<"Server-ID">>, props:get_value(<<"Server-ID">>, CCV)}
            ,{<<"App-Name">>, ?APP_NAME}
            ,{<<"App-Version">>, ?APP_VERSION}
           ],
    wapi_authz:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the e164 format of the inbound number
%% @end
%%--------------------------------------------------------------------
-spec get_dest_number/1 :: (wh_json:json_object()) -> ne_binary().
get_dest_number(JObj) ->
    User = case whapps_config:get(<<"stepswitch">>, <<"inbound_user_field">>, <<"Request">>) of
               <<"To">> ->
                   case binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>) of
                       [<<"nouser">>, _] ->
                           [ReqUser, _] = binary:split(wh_json:get_value(<<"Request">>, JObj), <<"@">>),
                           ReqUser;
                       [ToUser, _] ->
                           ToUser
                   end;
               _ ->
                   case binary:split(wh_json:get_value(<<"Request">>, JObj), <<"@">>) of
                       [<<"nouser">>, _] ->
                           [ReqUser, _] = binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>),
                           ReqUser;
                       [ToUser, _] ->
                           ToUser
                   end
           end,
    wnm_util:to_e164(User).
