%%%-------------------------------------------------------------------
%%% File    : j5_balacne_crawler.erl
%%% Description : Jonny5 module for disconnect calls when account 
%%% balance drops below zero
%%%-------------------------------------------------------------------
-module(j5_balance_crawler).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-export([maybe_disconnect_account/1]).
-export([delayed_hangup/2]).

-include("jonny5.hrl").

-record(state, {}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    wh_util:put_callid(?MODULE),
    self() ! 'crawl_accounts',
    {'ok', #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = {'error', 'not_implemented'},
    {'reply', Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info('crawl_accounts', _State) ->
    IsEnabled = whapps_config:get_is_true(?APP_NAME, <<"crawl_for_zero_balance">>, true),
    case j5_channels:accounts() of
        [] ->
            self() ! 'next_cycle',
            {'noreply', []};
        Accounts when IsEnabled -> 
            lager:debug("check accounts for zero balance"),
            self() ! 'next_account',
            {'noreply', Accounts};
        _ ->
            self() ! 'next_cycle',
            {'noreply', []}
    end;

handle_info('next_account', []) ->
    self() ! 'next_cycle',
    {'noreply', []};

handle_info('next_account', [Account|Accounts]) ->
    maybe_disconnect_account(Account),
    Delay = whapps_config:get_integer(?APP_NAME, <<"interaccount_delay">>, 10),
    erlang:send_after(Delay, self(), 'next_account'),
    {'noreply', Accounts, 'hibernate'};

handle_info('next_cycle', _State) ->
    Cycle = whapps_config:get_integer(?APP_NAME, <<"balance_crawler_cycle">>, 60000),
    erlang:send_after(Cycle, self(), 'crawl_accounts'),
    {'noreply', [], 'hibernate'};

handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]),
    'ok'.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%-------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec maybe_disconnect_account(ne_binary()) -> 'ok'.
maybe_disconnect_account(AccountId) -> 
    Limits = j5_limits:get(AccountId),
    DisconnectActiveCalls = j5_limits:disconnect_active_calls(Limits),
    case wh_util:is_true(DisconnectActiveCalls) of
        'false' -> 
            'ok';
        'true' -> 
            case j5_per_minute:maybe_credit_available(0, Limits, 'true') of
                'true' -> 
                    lager:debug("account ~p balance is ok",[AccountId]),
                    'ok';
                'false' -> 
                    disconnect_account(AccountId)
            end
    end.

-spec disconnect_account(ne_binary()) -> 'ok'.
disconnect_account(AccountId) ->
    case j5_channels:per_minute(AccountId) of
        Count when Count > 0 -> 
            lager:debug("account ~p have ~p per-minute calls, trying disconnect this calls",[AccountId, Count]),
            maybe_disconnect_channels(j5_channels:account(AccountId));
        _ -> 
            lager:debug("account ~p dont have any per-minute call",[AccountId]),
            'ok'
    end.
 
-spec maybe_disconnect_channels(j5_channels:channels()) -> 'ok'.
maybe_disconnect_channels([]) -> 'ok';
maybe_disconnect_channels([Channel|Channels]) ->
    Props = j5_channels:to_props(Channel),
    case props:get_binary_value(<<"Account-Billing">>, Props) =:= <<"per_minute">> 
         orelse props:get_binary_value(<<"Reseller-Billing">>, Props) =:= <<"per_minute">>
    of
        'false' -> maybe_disconnect_channels(Channels);
        'true' -> 
            disconnect_channel(Props),
            maybe_disconnect_channels(Channels)
    end.

-spec disconnect_channel(wh_proplist()) -> 'ok'.
disconnect_channel(Props) ->
    CallId =  props:get_ne_binary_value(<<"Call-ID">>, Props),
    HangupDelay = get_hangup_delay(Props),
    lager:debug("call id ~p, account billing ~p, reseller billing ~p",[CallId, props:get_binary_value(<<"Account-Billing">>, Props), props:get_binary_value(<<"Reseller-Billing">>, Props)]),
     try_disconnect_call(CallId, HangupDelay).

-spec get_hangup_delay(wh_proplist()) -> integer().
get_hangup_delay(Props) ->
    case props:get_integer_value(<<"Answered-Timestamp">>, Props) of
        'undefined' -> 0;
        AnsweredTimestamp ->
            CallTime = wh_util:current_tstamp() - AnsweredTimestamp,
            RateMinimum = props:get_integer_value(<<"Rate-Minimum">>, Props),
            RateIncrement = props:get_integer_value(<<"Rate-Increment">>, Props),
            MaxCallTime = ((CallTime div RateIncrement) + 1) * RateIncrement,
            case CallTime < RateMinimum of
                'true' -> RateMinimum - CallTime - 1;
                'false' when MaxCallTime > CallTime -> MaxCallTime - CallTime - 1;
                'false' -> 0
            end
    end.

-spec delayed_hangup(ne_binary(), integer()) -> 'ok'.
delayed_hangup(CallId, HangupDelay) ->
    timer:sleep(HangupDelay * 1000),
    send_hangup_req(CallId).

-spec send_hangup_req(ne_binary()) -> 'ok'.
send_hangup_req(CallId) ->
    API = [{<<"Call-ID">>, CallId}
           ,{<<"Action">>, <<"hangup">>}
           ,{<<"Data">>, wh_json:new()}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("attempting to hangup ~s", [CallId]),
    wh_amqp_worker:cast(API, fun wapi_metaflow:publish_req/1).

-spec try_disconnect_call(ne_binary(), integer()) -> 'ok'.
try_disconnect_call(CallId, 0) -> send_hangup_req(CallId);
try_disconnect_call(CallId, HangupDelay) ->
    lager:debug("disconnect call ~p delayed to ~p seconds",[CallId, HangupDelay]),
    _ = spawn(?MODULE, 'delayed_hangup', [CallId, HangupDelay]),
    'ok'.

