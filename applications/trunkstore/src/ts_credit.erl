%%%-------------------------------------------------------------------
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
    timer:send_interval(?REFRESH_RATE, refresh),
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
handle_call({check, Flags}, _From, Rates) ->
    {reply, set_rate_flags(Flags, Rates), Rates}.

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
    case ts_couch:open_doc(?TS_DB, ?TS_RATES_DOC) of
	not_found ->
	    format_log(info, "TS_CREDIT(~p): No document(~p) found~n", [self(), ?TS_RATES_DOC]),
	    {error, "No matching rates"};
	{error, unhandled_call} ->
	    format_log(info, "TS_CREDIT(~p): No host set for couch. Call ts_couch:set_host/1~n", [self()]),
	    {error, "No database host"};
	[] ->
	    format_log(info, "TS_CREDIT(~p): No Rates defined~n", [self()]),
	    {error, "No matching rates"};
	Rates when is_list(Rates) ->
	    format_log(info, "TS_CREDIT(~p): Rates pulled. Rev: ~p~n", [self(), get_value(<<"_rev">>, Rates)]),
	    {ok, lists:map(fun process_rates/1, Rates)};
	Error ->
	    format_log(error, "TS_CREDIT(~p): Fail ~p~n", [self(), Error]),
	    {error, "Unknown error occurred"}
    end.

process_rates({<<"_id">>, _}=ID) ->
    ID;
process_rates({<<"_rev">>, _}=Rev) ->
    Rev;
process_rates({RouteName, {RouteOptions}}) ->
    RoutesRegexStrs = get_value(<<"routes">>, RouteOptions, []),
    {Options} = get_value(<<"options">>, RouteOptions, {[]}),
    ROs0 = proplists:delete(<<"routes">>, RouteOptions),
    {RouteName, [{<<"routes">>, lists:map(fun(Str) -> {ok, R} = re:compile(Str), R end, RoutesRegexStrs)}
		 ,{<<"options">>, Options}
		 | proplists:delete(<<"options">>, ROs0)]}.

set_rate_flags(Flags, Rates) ->
    Dir = Flags#route_flags.direction,
    User = case Dir of
	       <<"outbound">> -> Flags#route_flags.to_user;
	       <<"inbound">> -> Flags#route_flags.from_user
	   end,
    Rates0 = proplists:delete(<<"_rev">>, proplists:delete(<<"_id">>, Rates)),
    Rates1 = lists:filter(fun({_RateName, RateData}) ->
				  lists:member(Dir, get_value(<<"direction">>, RateData)) andalso
				      lists:any(fun(Regex) ->
							re:run(User, Regex) =/= nomatch
						end, get_value(<<"routes">>, RateData))
			  end, Rates0),
    %% Filter on Options - All flag options must be in Rate options
    Rates2 = lists:filter(fun({_RateName, RateData}) ->
				  format_log(info, "TS_CREDIT.options_match: Filter ~p~n", [_RateName]),
				  options_match(Flags#route_flags.route_options, get_value(<<"options">>, RateData, []))
			  end, Rates1),

    case Rates2 of
	[] ->
	    format_log(error, "TS_CREDIT(~p): No Rate found for ~p~n", [self(), User]),
	    {error, no_route_found};
	[{RateName, RateData} | _Rest] ->
	    format_log(info, "TS_CREDIT(~p): Rate to use ~p~n", [self(), RateName]),

	    %% trunks available in flags (flat_rate_enabled) and the carrier has flatrate available as well
	    UseFlatRate = Flags#route_flags.flat_rate_enabled andalso get_value(<<"flatrate">>, RateData, false),

	    case UseFlatRate of
		true ->
		    {ok, set_flat_flags(Flags, Dir)};
		false ->
		    Flags0 = set_rate_flags(Flags, Dir, RateData),
		    case has_credit(Flags0, Dir) of
			true ->
			    {ok, Flags0};
			false ->
			    %% Raise Holy Hell, or play music saying no credit
			    %% returns us to ts_responder to catch the error
			    {error, lacking_credit}
		    end
	    end
    end.

%% can they be on the phone for at least a minute?
has_credit(Flags, <<"inbound">>) ->
    Flags#route_flags.credit_available > (Flags#route_flags.inbound_rate * Flags#route_flags.inbound_rate_minimum);
has_credit(Flags, <<"outbound">>) ->
    Flags#route_flags.credit_available > (Flags#route_flags.outbound_rate * Flags#route_flags.outbound_rate_minimum).

set_rate_flags(Flags, <<"inbound">>=In, RateData) ->
    format_log(info, "TS_CREDIT.set_rate_flags(~p): ~p~n", [In, RateData]),
    Flags#route_flags{
      inbound_rate = get_value(<<"rate_cost">>, RateData)
      ,inbound_rate_increment = get_value(<<"rate_increment">>, RateData)
      ,inbound_rate_minimum = get_value(<<"rate_minimum">>, RateData)
      ,inbound_surcharge = get_value(<<"rate_surcharge">>, RateData)
     };
set_rate_flags(Flags, <<"outbound">>=Out, RateData) ->
    format_log(info, "TS_CREDIT.set_rate_flags(~p): ~p~n", [Out, RateData]),
    Flags#route_flags{
      outbound_rate = get_value(<<"rate_cost">>, RateData)
      ,outbound_rate_increment = get_value(<<"rate_increment">>, RateData)
      ,outbound_rate_minimum = get_value(<<"rate_minimum">>, RateData)
      ,outbound_surcharge = get_value(<<"rate_surcharge">>, RateData)
     }.

set_flat_flags(Flags, <<"inbound">>=In) ->
    format_log(info, "TS_CREDIT.set_flat_flags for ~p~n", [In]),
    Flags#route_flags{
      inbound_rate = 0.0
      ,inbound_rate_increment = 0
      ,inbound_rate_minimum = 0
      ,inbound_surcharge = 0.0
     };
set_flat_flags(Flags, <<"outbound">>=Out) ->
    format_log(info, "TS_CREDIT.set_flat_flags for ~p~n", [Out]),
    Flags#route_flags{
      outbound_rate = 0.0
      ,outbound_rate_increment = 0
      ,outbound_rate_minimum = 0
      ,outbound_surcharge = 0.0
     }.

%% match options set in Flags to options available in Rate
%% All options set in Flags must be set in Rate to be usable
-spec(options_match/2 :: (RouteOptions :: tuple(list()) | list(), RateOptions :: list()) -> boolean()).
options_match({RouteOptions}, RateOptions) ->
    options_match(RouteOptions, RateOptions);
options_match(RouteOptions, RateOptions) ->
    format_log(info, "TS_CREDIT.options_match:~nDID Flags: ~p~nRoute Options: ~p~n", [RouteOptions, RateOptions]),
    lists:all(fun(Opt) -> get_value(Opt, RateOptions, false) =/= false end, RouteOptions).
