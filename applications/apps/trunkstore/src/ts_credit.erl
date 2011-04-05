%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Check user's account for appropriate credit
%%% @end
%%% Created : 20 Sep 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_credit).

-behaviour(gen_server).

-include("ts.hrl").

%% API
-export([start_link/0, check/1, force_rate_refresh/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-compile(export_all).

-define(SERVER, ?MODULE).
-define(REFRESH_RATE, 43200000). % 1000ms * 60s * 60m * 12h = Every twelve hours

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

check(Flags) ->
    gen_server:call(?MODULE, {check, Flags}).

force_rate_refresh() ->
    ?MODULE ! refresh,
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    timer:send_after(1000, ?MODULE, refresh),
    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Req, _From, []=R) ->
    format_log(info, "TS_CREDIT(~p): No rate information for Req ~p~n", [self(), Req]),
    {reply, no_rate_information, R};
handle_call({check, Flags}, From, Rates) ->
    spawn(fun() ->
		  wh_timer:start("ts_credit"),
		  gen_server:reply(From, set_rate_flags(Flags, Rates)),
		  wh_timer:stop("ts_Credit")
	  end),
    {noreply, Rates}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(refresh, OldRates) ->
    case get_current_rates() of
	{ok, Rates} ->
	    {noreply, Rates};
	{error, _Err} ->
	    format_log(error, "TS_CREDIT(~p): Error getting rates: ~p~n", [self(), _Err]),
	    {noreply, OldRates}
    end;
handle_info({document_changes, DocID, Changes}, Rates) ->
    format_log(info, "TS_CREDIT(~p): Changes on ~p. ~p~n", [self(), DocID, Changes]),
    CurrRev = get_value(<<"_rev">>, Rates),
    ChangedRates = lists:foldl(fun(ChangeProp, Rs) ->
				       NewRev = get_value(<<"rev">>, ChangeProp),
				       case NewRev of
					   undefined -> Rs;
					   CurrRev -> Rs;
					   _ ->
					       {ok, NewRates} = get_current_rates(),
					       NewRates
				       end
			       end, Rates, Changes),
    format_log(info, "TS_CREDIT(~p): Changed rates from ~p to ~p~n", [self(), CurrRev, get_value(<<"_rev">>, ChangedRates)]),
    {noreply, ChangedRates};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    couch_mgr:rm_change_handler(?TS_DB, ?TS_RATES_DOC),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_current_rates() ->
    case couch_mgr:open_doc(?TS_DB, ?TS_RATES_DOC) of
	{error, not_found} ->
	    format_log(info, "TS_CREDIT(~p): No document(~p) found~n", [self(), ?TS_RATES_DOC]),
	    {error, "No matching rates"};
	{error, db_not_reachable} ->
	    format_log(info, "TS_CREDIT(~p): No host set for couch. Call couch_mgr:set_host/1~n", [self()]),
	    {error, "No database host"};
	{ok, []} ->
	    format_log(info, "TS_CREDIT(~p): No Rates defined~n", [self()]),
	    {error, "No matching rates"};
	{ok, {struct, Rates}} when is_list(Rates) ->
	    format_log(info, "TS_CREDIT(~p): Rates pulled. Rev: ~p~n", [self(), get_value(<<"_rev">>, Rates)]),
	    couch_mgr:add_change_handler(?TS_DB, ?TS_RATES_DOC),
	    {ok, lists:map(fun process_rates/1, Rates)}
    end.

process_rates({<<"_id">>, _}=ID) -> ID;
process_rates({<<"_rev">>, _}=Rev) -> Rev;
process_rates({RouteName, {struct, RouteOptions}}) ->
    RoutesRegexStrs = get_value(<<"routes">>, RouteOptions, []),
    {struct, Options} = get_value(<<"options">>, RouteOptions, {struct, []}),
    ROs0 = proplists:delete(<<"routes">>, RouteOptions),
    {RouteName, [{<<"routes">>, lists:map(fun(Str) -> {ok, R} = re:compile(Str), R end, RoutesRegexStrs)}
		 ,{<<"options">>, Options}
		 | proplists:delete(<<"options">>, ROs0)]}.

set_rate_flags(Flags, Rates) ->
    Dir = Flags#route_flags.direction,
    User = Flags#route_flags.to_user,
    Rates0 = proplists:delete(<<"_rev">>, proplists:delete(<<"_id">>, Rates)),
    Rates1 = lists:filter(fun({_RateName, RateData}) ->
				  lists:member(Dir, get_value(<<"direction">>, RateData)) andalso
				      lists:any(fun(Regex) ->
							re:run(User, Regex) =/= nomatch
						end, get_value(<<"routes">>, RateData))
			  end, Rates0),
    wh_timer:tick("post first filter"),
    %% Filter on Options - All flag options must be in Rate options
    Rates2 = lists:filter(fun({_RateName, RateData}) ->
				  options_match(Flags#route_flags.route_options, get_value(<<"options">>, RateData, []))
			  end, Rates1),
    wh_timer:tick("post second filter"),

    case lists:usort(fun sort_rates/2, Rates2) of
	[] ->
	    wh_timer:tick("post usort empty"),
	    format_log(error, "TS_CREDIT(~p): No Rate found for ~p~n", [self(), User]),
	    {error, no_route_found};
	[{RateName, RateData} | _] ->
	    wh_timer:tick("post usort data found"),
	    format_log(info, "TS_CREDIT(~p): Rate to use ~p~n", [self(), RateName]),

	    case ts_acctmgr:reserve_trunk(Flags#route_flags.account_doc_id, Flags#route_flags.callid
					  ,(Flags#route_flags.rate * Flags#route_flags.rate_minimum + Flags#route_flags.surcharge)) of
		{ok, flat_rate} ->
		    {ok, set_flat_flags(Flags, Dir)};
		{ok, per_min} ->
		    {ok, set_rate_flags(Flags, Dir, RateData, RateName)};
		{error, entry_exists}=E ->
		    format_log(error, "TS_CREDIT(~p): Failed to reserve trunk for already existing call-id~n", [self()]),
		    E;
		{error, no_funds}=E1 ->
		    format_log(error, "TS_CREDIT(~p): No funds/flat-rate trunks to route call over.~n", [self()]),
		    E1;
		{error, no_account}=E2 ->
		    format_log(error, "TS_CREDIT(~p): No account id passed.~n", [self()]),
		    E2;
		{error, no_callid}=E3 ->
		    format_log(error, "TS_CREDIT(~p): No call id passed.~n", [self()]),
		    E3
	    end
    end.

%% Return true of RateA has higher weight than RateB
sort_rates({_RNameA, RateDataA}, {_RNameB, RateDataB}) ->
    ts_util:constrain_weight(get_value(<<"weight">>, RateDataA, 1)) >= ts_util:constrain_weight(get_value(<<"weight">>, RateDataB, 1)).

set_rate_flags(Flags, <<"inbound">>=In, RateData, RateName) ->
    format_log(info, "TS_CREDIT.set_rate_flags(~p): ~p~n", [In, RateName]),
    Flags#route_flags{
      rate = whistle_util:to_float(get_value(<<"rate_cost">>, RateData))
      ,rate_increment = whistle_util:to_integer(get_value(<<"rate_increment">>, RateData))
      ,rate_minimum = whistle_util:to_integer(get_value(<<"rate_minimum">>, RateData))
      ,surcharge = whistle_util:to_float(get_value(<<"rate_surcharge">>, RateData, 0))
      ,rate_name = RateName
      ,flat_rate_enabled = false
     };
set_rate_flags(Flags, <<"outbound">>=Out, RateData, RateName) ->
    format_log(info, "TS_CREDIT.set_rate_flags(~p): ~p~n", [Out, RateName]),
    Flags#route_flags{
      rate = whistle_util:to_float(get_value(<<"rate_cost">>, RateData))
      ,rate_increment = whistle_util:to_integer(get_value(<<"rate_increment">>, RateData))
      ,rate_minimum = whistle_util:to_integer(get_value(<<"rate_minimum">>, RateData))
      ,surcharge = whistle_util:to_float(get_value(<<"rate_surcharge">>, RateData, 0))
      ,rate_name = RateName
      ,flat_rate_enabled = false
     }.

set_flat_flags(Flags, <<"inbound">>=In) ->
    format_log(info, "TS_CREDIT.set_flat_flags for ~p~n", [In]),
    Flags#route_flags{
      rate = 0.0
      ,rate_increment = 0
      ,rate_minimum = 0
      ,surcharge = 0.0
      ,rate_name = <<>>
      ,flat_rate_enabled = true
     };
set_flat_flags(Flags, <<"outbound">>=Out) ->
    format_log(info, "TS_CREDIT.set_flat_flags for ~p~n", [Out]),
    Flags#route_flags{
      rate = 0.0
      ,rate_increment = 0
      ,rate_minimum = 0
      ,surcharge = 0.0
      ,rate_name = <<>>
      ,flat_rate_enabled = true
     }.

%% match options set in Flags to options available in Rate
%% All options set in Flags must be set in Rate to be usable
-spec(options_match/2 :: (RouteOptions :: list(binary()), RateOptions :: list(binary())) -> boolean()).
options_match(RouteOptions, RateOptions) ->
    format_log(info, "TS_CREDIT.options_match:~nDID Flags: ~p~nRoute Options: ~p~n", [RouteOptions, RateOptions]),
    lists:all(fun(Opt) -> get_value(Opt, RateOptions, false) =/= false end, RouteOptions).
