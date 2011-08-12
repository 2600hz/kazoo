%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Respond to Authentication requests
%%% @end
%%% Created : 31 Aug 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_auth).

%% API
-export([handle_req/1]).

-include("ts.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Give Prop, the Auth API request, create the API response JSON
%% @end
%%--------------------------------------------------------------------
-spec handle_req/1 :: (JObj) -> {ok, iolist()} | {error, string()} when
      JObj :: json_object().
handle_req(JObj) ->
    AuthU = wh_json:get_value(<<"Auth-User">>, JObj),
    AuthR0 = wh_json:get_value(<<"Auth-Realm">>, JObj),

    %% if we're authing, it's an outbound call; no auth means carrier authed by ACL, hence inbound
    %% until we introduce IP-based auth
    Direction = <<"outbound">>,

    AuthR = case ts_util:is_ipv4(AuthR0) of
		true ->
		    [_ToUser, ToDomain] = binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>),
		    ?LOG("Auth-Realm (~s) not a hostname, trying To-Domain (~s)", [AuthR0, ToDomain]),
		    ToDomain;
		false ->
		    AuthR0
	    end,

    {ok, AuthJObj} = lookup_user(AuthU, AuthR),

    AcctID = wh_json:get_value(<<"id">>, AuthJObj),

    Defaults = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
		,{<<"Custom-Channel-Vars">>, {struct, [
						       {<<"Direction">>, Direction}
						       ,{<<"Username">>, AuthU}
						       ,{<<"Realm">>, AuthR}
						       ,{<<"Account-ID">>, AcctID}
						       ,{<<"Authorizing-ID">>, AcctID}
						      ]
					     }}
		| wh_api:default_headers(<<>> % serverID is not important, though we may want to define it eventually
					      ,wh_json:get_value(<<"Event-Category">>, JObj)
					      ,<<"authn_resp">>
					      ,?APP_NAME
					      ,?APP_VERSION)],

    response(AuthJObj, Defaults).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec lookup_user/2 :: (Name, Realm) -> {ok, json_object()} | {error, user_not_found} when
      Name :: binary(),
      Realm :: binary().
lookup_user(Name, Realm) ->
    case couch_mgr:get_results(?TS_DB, ?TS_VIEW_USERAUTHREALM, [{<<"key">>, [Realm, Name]}]) of
	{error, _}=E -> E;
	{ok, []} -> {error, user_not_found};
	{ok, [{struct, _}=User|_]} ->
	    Auth = wh_json:get_value(<<"value">>, User),
	    {ok, wh_json:set_value(<<"id">>, wh_json:get_value(<<"id">>, User), Auth)}
    end.

-spec response/2 :: (AuthJObj, Prop) -> {ok, iolist()} | {error, string()} when
      AuthJObj :: json_object(),
      Prop :: proplist().
response(?EMPTY_JSON_OBJECT, Prop) ->
    Data = lists:umerge(specific_response(403), Prop),
    wh_api:authn_resp(Data);
response(AuthJObj, Prop) ->
    Data = lists:umerge(specific_response(AuthJObj), Prop),
    wh_api:authn_resp(Data).

-spec specific_response/1 :: (AuthJObj) -> proplist() when
      AuthJObj :: json_object() | 403.
specific_response({struct, _}=AuthJObj) ->
    Method = wh_util:to_binary(string:to_lower(wh_util:to_list(wh_json:get_value(<<"auth_method">>, AuthJObj)))),
    [{<<"Auth-Password">>, wh_json:get_value(<<"auth_password">>, AuthJObj)}
     ,{<<"Auth-Method">>, Method}
     ,{<<"Event-Name">>, <<"authn_resp">>}
     ,{<<"Access-Group">>, wh_json:get_value(<<"Access-Group">>, AuthJObj, <<"ignore">>)}
     ,{<<"Tenant-ID">>, wh_json:get_value(<<"Tenant-ID">>, AuthJObj, <<"ignore">>)}
    ];
specific_response(403) ->
    [{<<"Auth-Method">>, <<"error">>}
     ,{<<"Auth-Password">>, <<"403 Forbidden">>}
     ,{<<"Access-Group">>, <<"ignore">>}
     ,{<<"Tenant-ID">>, <<"ignore">>}].
