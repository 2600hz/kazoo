%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Respond to Route requests
%%% @end
%%% Created : 31 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_route).

%% API
-export([handle_req/1]).

-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("ts.hrl").

-define(APP_NAME, <<"ts_responder.route">>).
-define(APP_VERSION, <<"0.1">>).

%%%===================================================================
%%% API
%%%===================================================================
-spec(handle_req/1 :: (ApiProp :: proplist()) -> {ok, iolist()} | {error, string()}).
handle_req(ApiProp) ->
    format_log(info, "TS_ROUTE(~p): Prop: ~p~n", [self(), ApiProp]),
    case get_value(<<"Custom-Channel-Vars">>, ApiProp) of
	undefined ->
	    {error, "No Custom Vars"};
	{struct, []} ->
	    {error, "Empty Custom Vars"};
	{struct, CCVs} ->
	    case get_value(<<"Direction">>, CCVs) of
		<<"outbound">> ->
		    outbound_handler(ApiProp);
		<<"inbound">> ->
		    inbound_handler(ApiProp)
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(inbound_handler/1 :: (ApiProp :: list()) -> {ok, iolist()} | {error, string()}).
inbound_handler(Prop) ->
    {error, "Inbound handler not implemented"}.

-spec(outbound_handler/1 :: (ApiProp :: list()) -> {ok, iolist()} | {error, string()}).
outbound_handler(Prop) ->
    Did = get_value(<<"Caller-ID-Number">>, Prop),
    Options = [{"key", Did}],
    case ts_couch:has_view(?TS_DB, ?TS_VIEW_DIDLOOKUP) andalso
	ts_couch:get_results(?TS_DB, ?TS_VIEW_DIDLOOKUP, Options) of
	false ->
	    format_log(error, "TS_ROUTE(~p): No ~p view found while looking up ~p~n"
		       ,[self(), ?TS_VIEW_DIDLOOKUP, Did]),
	    {error, "No DIDLOOKUP view"};
	[] ->
	    format_log(info, "TS_ROUTE(~p): No DID matching ~p~n", [self(), Did]),
	    {error, "No matching DID"};
	[{ViewProp} | _Rest] ->
	    format_log(info, "TS_ROUTE(~p): DID found for ~p~n~p~n", [self(), Did, ViewProp]),
	    {struct, Value} = mochijson2:decode(get_value(<<"value">>, ViewProp)),
	    {struct, DidOptions} = get_value(<<"DID_Opts">>, Value),
	    process_routing(DidOptions, Prop);
	_Else ->
	    format_log(error, "TS_ROUTE(~p): Got something unexpected~n~p~n", [self(), _Else]),
	    {error, "Unexpected error in outbound_handler"}
    end.

-spec(process_routing/2 :: (DidProp :: proplist(), ApiProp :: proplist()) -> {ok, iolist()} | {error, string()}).
process_routing(DidProp, ApiProp) ->
    Flags = outbound_features(set_flags(DidProp, ApiProp)),
    format_log(info, "TS_ROUTE(~p): Flags set after outbound: ~p~n", [self(), Flags]),
    case ts_credit:check(Flags) of
	{ok, Flags1} ->
	    %% call may proceed
	    1;
	{error, Error} ->
	    %% play recording about being out of credit
	    2
    end,
    {error, "Thus far and no farther"}.

outbound_features(Flags) ->
    Features = [ts_e911, ts_t38],
    lists:foldl(fun(Mod, Flags0) ->
			Mod:process_flags(Flags0)
		end, Flags, Features).

set_flags(DidProp, ApiProp) ->
    [ToUser, ToDomain] = binary:split(get_value(<<"To">>, ApiProp), <<"@">>),
    [FromUser, FromDomain] = binary:split(get_value(<<"From">>, ApiProp), <<"@">>),

    {struct, FailOpts} = get_value(<<"FailOver">>, DidProp, {struct, []}),
    {struct, CallerIDOpts} = get_value(<<"CallerID">>, DidProp, {struct, []}),
    {struct, Opts} = get_value(<<"Options">>, DidProp),

    E911 = case get_value(<<"E911">>, Opts) of
	       undefined -> [];
	       false -> [];
	       0 -> [];
	       1 ->
		   {struct, E911Opts} = get_value(<<"E911_Info">>, DidProp, {struct, []}),
		   E911Opts
	   end,

    #route_flags{
	     to_user = ToUser
	     ,to_domain = ToDomain
	     ,from_user = FromUser
	     ,from_domain = FromDomain
	     ,failover = FailOpts
	     ,callerid = {get_value(<<"CName">>, CallerIDOpts, ""), get_value(<<"CNum">>, CallerIDOpts, "")}
	     ,e911 = E911
	     ,payphone = (get_value(<<"PayPhoneAccess">>, Opts) =:= 1)
	     %,callerid_default
	     %,e911_default
	     %,fax
	     %,flat_rate_enabled
	     %,codecs = [] :: list()
	    }.

-spec(specific_response/2 :: (Code :: integer(), Routes :: proplist()) -> proplist()).
specific_response(200, Routes) ->
    [{<<"Routes">>, {struct, Routes}}
     ,{<<"Method">>, <<"bridge">>}
    ];
specific_response(404, _Routes) ->
    [{<<"Routes">>, []}
     ,{<<"Method">>, <<"error">>}
     ,{<<"Route-Error-Code">>, <<"404">>}
     ,{<<"Route-Error-Message">>, <<"Not Found">>}
    ];
specific_response(503, _Routes) ->
    [{<<"Routes">>, []}
     ,{<<"Method">>, <<"error">>}
     ,{<<"Route-Error-Code">>, <<"503">>}
     ,{<<"Route-Error-Message">>, <<"Insufficient Credit">>}
    ].
