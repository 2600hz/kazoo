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
-define(APP_VERSION, <<"0.3.0">>).

-include("ts.hrl").

-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Give Prop, the Auth API request, create the API response JSON
%% @end
%%--------------------------------------------------------------------
-spec(handle_req/1 :: (Prop :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
handle_req(Prop) ->
    [_FromUser, FromDomain] = binary:split(get_value(<<"From">>, Prop), <<"@">>),
    {AuthU, AuthD} = {get_value(<<"Auth-User">>, Prop), get_value(<<"Auth-Domain">>, Prop)},

    Direction = case is_inbound(FromDomain) of
		    true -> <<"inbound">>;
		    false -> <<"outbound">>
		end,

    ViewInfo = case lookup_user(AuthU, AuthD) of
		   {ok, View} -> View;
		   {error, _E}=Error ->
		       format_log(error, "TS_AUTH(~p): Failed to lookup user ~p@~p~n", [self(), AuthU, AuthD]),
		       Error
	       end,

    Defaults = [{<<"Msg-ID">>, get_value(<<"Msg-ID">>, Prop)}
		,{<<"Custom-Channel-Vars">>, {struct, [
						       {<<"Direction">>, Direction}
						       ,{<<"Auth-User">>, AuthU}
						      ]
					     }}
		| whistle_api:default_headers(<<>> % serverID is not important, though we may want to define it eventually
					      ,get_value(<<"Event-Category">>, Prop)
					      ,<<"auth_resp">>
					      ,?APP_NAME
					      ,?APP_VERSION)],

    Info = case ViewInfo of
	       {error, Reason} ->
		   format_log(error, "TS_AUTH(~p): Sending a 500 for error: ~p~n", [self(), Reason]),
		   500;
	       [] ->
		   case Direction of
		       <<"inbound">> ->
			   %% send urgent email or alert for Phantom Number
			   format_log(error, "TS_AUTH(~p): Alert! ~p@~p was routed to us~n", [self(), AuthU, AuthD]);
		       <<"outbound">> ->
			   %% unauthed user trying to make calls, alert
			   format_log(error, "TS_AUTH(~p): Sending a 403 - unauthed user trying to make a call~n", [self()])
		   end,
		   403;
	       _ ->
		   format_log(error, "TS_AUTH(~p): Sending a 200 w/ ~p~n", [self(), ViewInfo]),
		   ViewInfo
	   end,
    response(Info, Defaults).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Inbound detection will likely be done in ACLs for carriers, so this function is more a place-holder
%% than something more meaningful. Auth will likely be bypassed for known carriers, and this function
%% will most likely return false everytime
-spec(is_inbound/1 :: (Domain :: binary()) -> boolean()).
is_inbound(Domain) ->
    IP = ts_util:find_ip(Domain),
    Options = [{"key", IP}],
    format_log(info, "TS_AUTH(~p): lookup_carrier using ~p(~p) in ~p~n", [self(), Domain, IP, ?TS_VIEW_CARRIERIP]),
    case couch_mgr:get_results(?TS_DB, ?TS_VIEW_CARRIERIP, Options) of
	{error, not_found} ->
	    format_log(info, "TS_AUTH(~p): No Carrier matching ~p(~p)~n", [self(), Domain, IP]),
	    false;
	{error,  db_not_reachable} ->
	    format_log(info, "TS_AUTH(~p): No DB accessible~n", [self()]),
	    false;
	{error, view_not_found} ->
	    format_log(info, "TS_AUTH(~p): View ~p missing~n", [self(), ?TS_VIEW_CARRIERIP]),
	    false;
	{ok, []} ->
	    format_log(info, "TS_AUTH(~p): No Carrier matching ~p(~p)~n", [self(), Domain, IP]),
	    false;
	{ok, [{struct, ViewProp} | _Rest]} ->
	    format_log(info, "TS_AUTH(~p): Carrier found for ~p(~p)~n~p~n", [self(), Domain, IP, ViewProp]),
	    true;
	_Else ->
	    format_log(error, "TS_AUTH(~p): Got something unexpected during inbound check~n~p~n", [self(), _Else]),
	    false
    end.

-spec(lookup_user/2 :: (Name :: binary(), Domain :: binary()) -> tuple(ok | error, proplist() | string())).
lookup_user(Name, _Domain) ->
    format_log(info, "TS_AUTH(~p): Skipping checking ~s@~s, just ~s for now~n", [self(), Name, _Domain, Name]),
    lookup_user(Name).

    %% commented out until we actually care about the domain - currently hardcoded in ecallmgr/FS
    %% format_log(info, "Finding IP for ~p~n", [Domain]),
    %% IP = ts_util:find_ip(Domain),
    %% format_log(info, "Finding Options~n", []),
    %% Options = [{"key", IP}],
    %% format_log(info, "TS_AUTH(~p): lookup_user with ~p and ~p(~p) in ~p.~p~n"
    %% 	       ,[self(), Name, Domain, IP, ?TS_DB, ?TS_VIEW_IPAUTH]),
    %% format_log(info, "Getting results~n", []),
    %% case couch_mgr:get_results(?TS_DB, ?TS_VIEW_IPAUTH, Options) of
    %% 	false ->
    %% 	    format_log(error, "TS_AUTH(~p): No ~p view found while looking up ~p(~p)~n"
    %% 		       ,[self(), ?TS_VIEW_IPAUTH, Domain, IP]),
    %% 	    {error, "No view found."};
    %% 	[] ->
    %% 	    format_log(info, "TS_AUTH(~p): No Domain matching ~p(~p)~n", [self(), Domain, IP]),
    %% 	    lookup_user(Name);
    %% 	[{ViewProp} | _Rest] ->
    %% 	    format_log(info, "TS_AUTH(~p): Retrieved by ~p(~p)~n", [self(), Domain, IP]),
    %% 	    {ok, ViewProp};
    %% 	_Else ->
    %% 	    format_log(error, "TS_AUTH(~p): Got something unexpected~n~p~n", [self(), _Else]),
    %% 	    {error, "Unexpected error in lookup_user/2"}
    %% end.

-spec(lookup_user/1 :: (Name :: binary()) -> tuple(ok | error, proplist() | string())).
lookup_user(Name) ->
    Options = [{"key", Name}],
    case couch_mgr:get_results(?TS_DB, ?TS_VIEW_USERAUTH, Options) of
	{error, _} ->
	    format_log(error, "TS_AUTH(~p): No ~p view found while looking up ~p~n", [self(), ?TS_VIEW_USERAUTH, Name]),
	    {error, "No view found."};
	{ok, []} ->
	    format_log(info, "TS_AUTH(~p): No Name matching ~p~n", [self(), Name]),
	    [];
	{ok, [{struct, ViewProp} | _Rest]} ->
	    format_log(info, "TS_AUTH(~p): Retrieved by username ~p~n", [self(), Name]),
	    {ok, ViewProp};
	_Else ->
	    format_log(error, "TS_AUTH(~p): Got something unexpected~n~p~n", [self(), _Else]),
	    {error, "Unexpeced error in lookup_user/1"}
    end.

-spec(response/2 :: (RespData :: proplist() | integer(), Prop :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
response(ViewInfo, Prop) ->
    Data = lists:umerge(specific_response(ViewInfo), Prop),
    whistle_api:auth_resp(Data).

-spec(specific_response/1 :: (ViewInfo :: proplist() | integer()) -> proplist()).
specific_response(ViewInfo) when is_list(ViewInfo) ->
    {struct, Info} = get_value(<<"value">>, ViewInfo),
    Method = list_to_binary(string:to_lower(binary_to_list(get_value(<<"auth_method">>, Info)))),
    [{<<"Auth-Password">>, get_value(<<"auth_password">>, Info)}
     ,{<<"Auth-Method">>, Method}
     ,{<<"Event-Name">>, <<"auth_resp">>}
     ,{<<"Access-Group">>, get_value(<<"Access-Group">>, Info, <<"ignore">>)}
     ,{<<"Tenant-ID">>, get_value(<<"Tenant-ID">>, Info, <<"ignore">>)}
    ];
specific_response(500) ->
    [{<<"Auth-Method">>, <<"error">>}
     ,{<<"Auth-Password">>, <<"500 Internal Error">>}
     ,{<<"Access-Group">>, <<"ignore">>}
     ,{<<"Tenant-ID">>, <<"ignore">>}];
specific_response(403) ->
    [{<<"Auth-Method">>, <<"error">>}
     ,{<<"Auth-Password">>, <<"403 Forbidden">>}
     ,{<<"Access-Group">>, <<"ignore">>}
     ,{<<"Tenant-ID">>, <<"ignore">>}].
