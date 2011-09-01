%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle authn_req messages
%%% @end
%%% Created : 19 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(authn_req).

-export([init/0, handle_req/2]).

-include("reg.hrl").

init() ->
    ok.

-spec handle_req/2 :: (JObj, Props) -> no_return() when
      JObj :: json_object(),
      Props :: proplist().
handle_req(JObj, Props) ->
    Cache = props:get_value(cache, Props),
    Queue = props:get_value(queue, Props),

    ?LOG_START("received SIP authentication request"),

    AuthU = wh_json:get_value(<<"Auth-User">>, JObj),
    AuthR0 = wh_json:get_value(<<"Auth-Realm">>, JObj),

    AuthR = case wh_util:is_ipv4(AuthR0) orelse wh_util:is_ipv6(AuthR0) of
                true ->
                    [_ToUser, ToDomain] = binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>),
                    ?LOG("auth-realm (~s) not a hostname, trying To-domain (~s)", [AuthR0, ToDomain]),
                    ToDomain;
                false ->
                    AuthR0
            end,

    %% crashes if not found, no return necessary
    {ok, AuthJObj} = reg_util:lookup_auth_user(AuthU, AuthR, Cache),

    AuthId = wh_json:get_value([<<"doc">>, <<"_id">>], AuthJObj),

    AccountId = case wh_json:get_value([<<"doc">>, <<"pvt_account_id">>], AuthJObj) of
		    undefined ->
                        case wh_json:get_value([<<"doc">>, <<"pvt_account_db">>], AuthJObj) of
                            undefined -> undefined;
                            AcctDb -> whapps_util:get_db_name(AcctDb, raw)
                        end;
                    AcctId -> AcctId
		end,

    CCVs = [CCV || {_, V}=CCV <- [{<<"Username">>, AuthU}
                                  ,{<<"Realm">>, AuthR}
                                  ,{<<"Account-ID">>, AccountId}
                                  ,{<<"Inception">>, <<"on-net">>}
                                  ,{<<"Authorizing-ID">>, AuthId}
                                 ],
		   V =/= undefined],

    Defaults = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
		,{<<"Custom-Channel-Vars">>, {struct, CCVs}}
		| wh_api:default_headers(Queue % serverID is not important, though we may want to define it eventually
					      ,wh_json:get_value(<<"Event-Category">>, JObj)
					      ,<<"authn_resp">>
					      ,?APP_NAME
					      ,?APP_VERSION)],

    {ok, Payload} = authn_response(wh_json:get_value(<<"value">>, AuthJObj), Defaults),
    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    reg_util:send_resp(Payload, RespQ).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% determine if the user was known and send a reply if so
%% @end
%%-----------------------------------------------------------------------------
-spec authn_response/2 :: (AuthnResp, Prop) -> {ok, iolist()} | {error, string()} when
      AuthnResp :: json_object() | integer(),
      Prop :: proplist().
authn_response(?EMPTY_JSON_OBJECT, _) ->
    ?LOG_END("user is unknown");
authn_response(AuthInfo, Prop) ->
    Data = lists:umerge(auth_specific_response(AuthInfo), Prop),
    ?LOG_END("sending SIP authentication reply, with credentials"),
    wh_api:authn_resp(Data).


%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% create a auth response proplist to send back when the user is known
%% @end
%%-----------------------------------------------------------------------------
-spec auth_specific_response/1 :: (AuthInfo) -> proplist() when
      AuthInfo :: json_object() | integer().
auth_specific_response(AuthInfo) ->
    Method = list_to_binary(string:to_lower(binary_to_list(wh_json:get_value(<<"method">>, AuthInfo, <<"password">>)))),
    [{<<"Auth-Password">>, wh_json:get_value(<<"password">>, AuthInfo)}
     ,{<<"Auth-Method">>, Method}
     ,{<<"Event-Name">>, <<"authn_resp">>}
     ,{<<"Access-Group">>, wh_json:get_value(<<"access_group">>, AuthInfo, <<"ignore">>)}
     ,{<<"Tenant-ID">>, wh_json:get_value(<<"tenant_id">>, AuthInfo, <<"ignore">>)}
    ].

