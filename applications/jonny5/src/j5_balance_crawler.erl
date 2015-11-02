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

-export([sleep_and_cast/3]).
-export([maybe_disconnect_account/1]).
-export([delayed_hangup/2]).

-include("jonny5.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_CRAWLER_CYCLE, 60000).
-define(DEFAULT_INTERACCOUNT_DELAY, 10).
-define(OVERLOAD_DELAY, 10000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    case whapps_config:get_is_true(?APP_NAME, <<"balance_crawler_enabled">>, false) of
        'true' -> gen_server:start_link(?MODULE, [], []);
        'false' -> 'ignore'
    end.

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
    sleep_and_cast(0,'crawl_accounts'),
    {'ok', []}.

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
handle_cast('crawl_accounts', []) ->
    Cycle = whapps_config:get_integer(?APP_NAME, <<"balance_crawler_cycle_ms">>, ?DEFAULT_CRAWLER_CYCLE),
    spawn_sleep_and_cast(Cycle, 'crawl_accounts'),
    case j5_channels:accounts() of
        [] ->
            {'noreply', [], 'hibernate'};
        Accounts ->
            lager:debug("check ~p account(s) for zero balance", [length(Accounts)]),
            sleep_and_cast(0, 'next_account'),
            {'noreply', Accounts}
    end;

%%--------------------------------------------------------------------------------------------
%% If crawler still checking account from previous cycle, warn and try again after some delay
%%--------------------------------------------------------------------------------------------
handle_cast('crawl_accounts', [_|_] = Accounts) ->
    lager:warning("delay next crawler cycle for ~p ms, ~p account(s) to go. Please tune crawler settings", [?OVERLOAD_DELAY, length(Accounts)]),
    spawn_sleep_and_cast(?OVERLOAD_DELAY, 'crawl_accounts'),
    {'noreply', Accounts};

handle_cast('next_account', []) ->
    {'noreply', [], 'hibernate'};

handle_cast('next_account', [Account|Accounts]) ->
    maybe_disconnect_account(Account),
    Delay = whapps_config:get_integer(?APP_NAME, <<"balance_crawler_interaccount_delay_ms">>, ?DEFAULT_INTERACCOUNT_DELAY),
    sleep_and_cast(Delay, 'next_account'),
    {'noreply', Accounts};

handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
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
-spec sleep_and_cast(integer(), atom()) -> 'ok'.
-spec sleep_and_cast(pid(), integer(), atom()) -> 'ok'.
sleep_and_cast(Delay, Message) ->
    sleep_and_cast(self(), Delay, Message).
sleep_and_cast(Pid, Delay, Message) when is_integer(Delay) andalso Delay > 0 ->
    timer:sleep(Delay),
    gen_server:cast(Pid, Message);
sleep_and_cast(Pid, _Delay, Message) ->
    gen_server:cast(Pid, Message).

-spec spawn_sleep_and_cast(integer(), atom()) -> 'ok'.
spawn_sleep_and_cast(Delay, Message) ->
    _ = spawn(?MODULE, sleep_and_cast, [self(), Delay, Message]),
    'ok'.

-spec maybe_disconnect_account(ne_binary()) -> 'ok'.
maybe_disconnect_account(AccountId) ->
    Limits = j5_limits:get(AccountId),
    DisconnectActiveCalls = j5_limits:disconnect_active_calls(Limits),
    case wh_util:is_true(DisconnectActiveCalls) of
        'false' -> 'ok';
        'true' ->
            case j5_per_minute:maybe_credit_available(0, Limits, 'true') of
                'true' -> 'ok';
                'false' -> disconnect_account(AccountId)
            end
    end.

-spec disconnect_account(ne_binary()) -> 'ok'.
disconnect_account(AccountId) ->
    case j5_channels:per_minute(AccountId) of
        Count when Count > 0 ->
            lager:debug("account ~p has ~p per-minute calls, disconnect them",[AccountId, Count]),
            maybe_disconnect_channels(j5_channels:account(AccountId));
        _ ->
            lager:debug("account ~p doesn't have any per-minute call",[AccountId]),
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
    HangupDelay = case whapps_config:get_is_true(?APP_NAME, <<"balance_crawler_delayed_hangup">>, true) of
                      'true' -> get_hangup_delay(Props);
                      'false' -> 0
                  end,
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
    timer:sleep(HangupDelay * ?MILLISECONDS_IN_SECOND),
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
try_disconnect_call(CallId, HangupDelay) when is_integer(HangupDelay) andalso HangupDelay > 0 ->
    lager:debug("disconnect call ~p delayed to ~p seconds",[CallId, HangupDelay]),
    _ = spawn(?MODULE, 'delayed_hangup', [CallId, HangupDelay]),
    'ok';
try_disconnect_call(CallId, _HangupDelay) -> send_hangup_req(CallId).

