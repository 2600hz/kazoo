%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Respond to Authentication requests
%%% @end
%%% Created : 31 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_auth).

%% API
-export([handle_req/1]).

-define(APP_NAME, <<"ts_responder.auth">>).
-define(APP_VERSION, <<"0.3.1">>).

-include("ts.hrl").

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Give Prop, the Auth API request, create the API response JSON
%% @end
%%--------------------------------------------------------------------
-spec(handle_req/1 :: (Prop :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
handle_req(Prop) ->
    AuthU = get_value(<<"Auth-User">>, Prop),
    AuthR = get_value(<<"Auth-Domain">>, Prop),

    %% [_FromUser, FromDomain] = binary:split(get_value(<<"From">>, Prop), <<"@">>),
    %% Direction = case is_inbound(FromDomain) of
    %% 		    true -> <<"inbound">>;
    %% 		    false -> <<"outbound">>
    %% 		end,
    Direction = <<"outbound">>, %% if we're authing, it's an outbound call; no auth means carrier authed by ACL, hence inbound

    {ok, AuthProp} = lookup_user(AuthU, AuthR),

    Defaults = [{<<"Msg-ID">>, get_value(<<"Msg-ID">>, Prop)}
		,{<<"Custom-Channel-Vars">>, {struct, [
						       {<<"Direction">>, Direction}
						       ,{<<"Username">>, AuthU}
						       ,{<<"Realm">>, AuthR}
						      ]
					     }}
		| whistle_api:default_headers(<<>> % serverID is not important, though we may want to define it eventually
					      ,get_value(<<"Event-Category">>, Prop)
					      ,<<"auth_resp">>
					      ,?APP_NAME
					      ,?APP_VERSION)],

    response(AuthProp, Defaults).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Inbound detection will likely be done in ACLs for carriers, so this function is more a place-holder
%% than something more meaningful. Auth will likely be bypassed for known carriers, and this function
%% will most likely return false everytime
%% -spec(is_inbound/1 :: (Domain :: binary()) -> false).
  %% is_inbound(Domain) ->
    %% IP = ts_util:find_ip(Domain),
    %% Options = [{"key", IP}],
    %% format_log(info, "TS_AUTH(~p): lookup_carrier using ~p(~p) in ~p~n", [self(), Domain, IP, ?TS_VIEW_CARRIERIP]),
    %% case couch_mgr:get_results(?TS_DB, ?TS_VIEW_CARRIERIP, Options) of
    %% 	{error, not_found} ->
    %% 	    format_log(info, "TS_AUTH(~p): No Carrier matching ~p(~p)~n", [self(), Domain, IP]),
    %% 	    false;
    %% 	{error,  db_not_reachable} ->
    %% 	    format_log(info, "TS_AUTH(~p): No DB accessible~n", [self()]),
    %% 	    false;
    %% 	{error, view_not_found} ->
    %% 	    format_log(info, "TS_AUTH(~p): View ~p missing~n", [self(), ?TS_VIEW_CARRIERIP]),
    %% 	    false;
    %% 	{ok, []} ->
    %% 	    format_log(info, "TS_AUTH(~p): No Carrier matching ~p(~p)~n", [self(), Domain, IP]),
    %% 	    false;
    %% 	{ok, [{struct, ViewProp} | _Rest]} ->
    %% 	    format_log(info, "TS_AUTH(~p): Carrier found for ~p(~p)~n~p~n", [self(), Domain, IP, ViewProp]),
    %% 	    true;
    %% 	_Else ->
    %% 	    format_log(error, "TS_AUTH(~p): Got something unexpected during inbound check~n~p~n", [self(), _Else]),
    %% 	    false
    %% end.

-spec(lookup_user/2 :: (Name :: binary(), Realm :: binary()) -> tuple(ok, proplist()) | tuple(error, string())).
lookup_user(Name, Realm) ->
    case couch_mgr:get_results(?TS_DB, ?TS_VIEW_USERAUTHREALM, [{<<"key">>, [Realm, Name]}]) of
	{error, _}=E -> E;
	{ok, []} -> {error, "No user/realm found"};
	{ok, [{struct, User}|_]} ->
	    {struct, Auth} = props:get_value(<<"value">>, User),
	    {ok, Auth}
    end.

-spec(response/2 :: (AuthInfo :: proplist() | integer(), Prop :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
response([], Prop) ->
    Data = lists:umerge(specific_response(403), Prop),
    whistle_api:auth_resp(Data);
response(AuthInfo, Prop) ->
    Data = lists:umerge(specific_response(AuthInfo), Prop),
    whistle_api:auth_resp(Data).

-spec(specific_response/1 :: (AuthInfo :: proplist() | integer()) -> proplist()).
specific_response(AuthInfo) when is_list(AuthInfo) ->
    Method = list_to_binary(string:to_lower(binary_to_list(get_value(<<"auth_method">>, AuthInfo)))),
    [{<<"Auth-Password">>, get_value(<<"auth_password">>, AuthInfo)}
     ,{<<"Auth-Method">>, Method}
     ,{<<"Event-Name">>, <<"auth_resp">>}
     ,{<<"Access-Group">>, get_value(<<"Access-Group">>, AuthInfo, <<"ignore">>)}
     ,{<<"Tenant-ID">>, get_value(<<"Tenant-ID">>, AuthInfo, <<"ignore">>)}
    ];
specific_response(403) ->
    [{<<"Auth-Method">>, <<"error">>}
     ,{<<"Auth-Password">>, <<"403 Forbidden">>}
     ,{<<"Access-Group">>, <<"ignore">>}
     ,{<<"Tenant-ID">>, <<"ignore">>}].
