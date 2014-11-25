%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%% Implementation of a token bucket as gen_server
%%%   https://en.wikipedia.org/wiki/Token_bucket#The_token_bucket_algorithm
%%%
%%% Bucket = start_link(100, 10), % start with 100 tokens, add 10 per second (on the second in bulk)
%%% consume(Bucket, 10) => 'true'
%%% consume(Bucket, 1000) => 'false'
%%%
%%% Metered = start_link(100, 10, 'false'), % start with 100 tokens, add 1 per 100ms instead
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_token_bucket).

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/3, start_link/4
         ,stop/1
         ,consume/2
         ,consume_until/2
         ,credit/2
         ,tokens/1
         ,set_name/2
         ,default_fill_time/0
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("kz_buckets.hrl").

-define(FILL_TIME, whapps_config:get_integer(<<"token_buckets">>, <<"tokens_fill_time">>, <<"second">>)).
-define(TOKEN_FILL_TIME, 'fill_er_up').

-type fill_rate_time() :: 'second' | 'minute' | 'hour' | 'day'.

-export_type([fill_rate_time/0]).

-record(state, {max_tokens = 100 :: pos_integer() % max size of bucket
                ,tokens = 0 :: non_neg_integer() % current count
                ,fill_rate = 1 :: pos_integer() % tokens/fill_rate_time() added
                ,fill_rate_time = 'second' :: fill_rate_time()
                ,fill_ref :: reference()
                ,fill_as_block = 'true' :: boolean()
               }).

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
-spec start_link(pos_integer(), pos_integer()) -> startlink_ret().
-spec start_link(pos_integer(), pos_integer(), boolean()) -> startlink_ret().
start_link(Max, FillRate) -> start_link(Max, FillRate, 'true').
start_link(Max, FillRate, FillBlock) ->
    start_link(Max, FillRate, FillBlock, default_fill_time()).
start_link(Max, FillRate, FillBlock, FillTime) when is_integer(FillRate), FillRate > 0,
                                                    is_integer(Max), Max > 0,
                                                    is_boolean(FillBlock),
                                                    (FillTime =:= 'second'
                                                     orelse FillTime =:= 'minute'
                                                     orelse FillTime =:= 'hour'
                                                     orelse FillTime =:= 'day'
                                                    )
                                                    ->
    gen_server:start_link(?MODULE, [Max, FillRate, FillBlock, FillTime], []).

-spec stop(pid()) -> 'ok'.
stop(Srv) ->
    gen_server:cast(Srv, 'stop').

-spec consume(pid(), non_neg_integer()) -> boolean().
consume(_Srv, Tokens) when is_integer(Tokens) andalso Tokens =< 0 ->
    'true';
consume(Srv, Tokens) when is_integer(Tokens) andalso Tokens > 0 ->
    gen_server:call(Srv, {'consume', Tokens}).

-spec consume_until(pid(), non_neg_integer()) -> boolean().
consume_until(_Srv, Tokens) when is_integer(Tokens) andalso Tokens =< 0 ->
    'true';
consume_until(Srv, Tokens) when is_integer(Tokens) andalso Tokens > 0 ->
    gen_server:call(Srv, {'consume_until', Tokens}).

-spec credit(pid(), pos_integer()) -> 'ok'.
credit(Srv, Tokens) when is_integer(Tokens) andalso Tokens > 0 ->
    gen_server:cast(Srv, {'credit', Tokens}).

-spec tokens(pid()) -> non_neg_integer().
tokens(Srv) -> gen_server:call(Srv, {'tokens'}).

-spec set_name(pid(), ne_binary()) -> 'ok'.
set_name(Srv, Name) -> gen_server:cast(Srv, {'name', Name}).

-spec default_fill_time() -> fill_rate_time().
-spec default_fill_time(api_binary()) -> fill_rate_time().
default_fill_time() ->
    default_fill_time(?FILL_TIME).

default_fill_time(<<"day">>) -> 'day';
default_fill_time(<<"hour">>) -> 'hour';
default_fill_time(<<"minute">>) -> 'minute';
default_fill_time(_) -> 'second'.


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
init([Max, FillRate, FillBlock, FillTime]) ->
    wh_util:put_callid(?MODULE),
    lager:debug("starting token bucket with ~b max, filling at ~b/~s, in a block: ~s"
                ,[Max, FillRate,FillTime, FillBlock]
               ),
    {'ok', #state{max_tokens=Max
                  ,fill_rate=FillRate
                  ,fill_rate_time=FillTime
                  ,fill_ref=start_fill_timer(FillRate, FillBlock, FillTime)
                  ,fill_as_block=FillBlock
                  ,tokens=Max
                 }
     ,?INACTIVITY_TIMEOUT_MS
    }.

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
handle_call({'consume', Req}, _From, #state{tokens=Current}=State) ->
    case Current - Req of
        N when N >= 0 ->
            lager:debug("consumed ~p, ~p left", [Req, N]),
            {'reply', 'true', State#state{tokens=N}, ?INACTIVITY_TIMEOUT_MS};
        _ ->
            lager:debug("not enough tokens (~p) to consume ~p", [Current, Req]),
            {'reply', 'false', State, ?INACTIVITY_TIMEOUT_MS}
    end;
handle_call({'consume_until', Req}, _From, #state{tokens=Current}=State) ->
    case Current - Req of
        N when N >= 0 ->
            lager:debug("consumed ~p, ~p left", [Req, N]),
            {'reply', 'true', State#state{tokens=N}, ?INACTIVITY_TIMEOUT_MS};
        _N ->
            lager:debug("not enough tokens (~p) to consume ~p, zeroing out tokens", [Current, Req]),
            {'reply', 'false', State#state{tokens=0}, ?INACTIVITY_TIMEOUT_MS}
    end;
handle_call({'tokens'}, _From, #state{tokens=Current}=State) ->
    {'reply', Current, State, ?INACTIVITY_TIMEOUT_MS};
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call: ~p", [_Request]),
    {'reply', 'ok', State, ?INACTIVITY_TIMEOUT_MS}.

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
handle_cast({'credit', Req}, #state{tokens=Current
                                    ,max_tokens=Max
                                   }=State) ->
    case Current + Req of
        N when N > Max ->
            lager:debug("credit of ~p tokens overfills, setting to ~p", [Req, Max]),
            {'noreply', State#state{tokens=Max}, ?INACTIVITY_TIMEOUT_MS};
        N ->
            lager:debug("crediting ~p tokens, now at ~p", [Req, N]),
            {'noreply', State#state{tokens=N}, ?INACTIVITY_TIMEOUT_MS}
    end;
handle_cast({'name', Name}, State) ->
    wh_util:put_callid(Name),
    lager:debug("updated name to ~s", [Name]),
    {'noreply', State, ?INACTIVITY_TIMEOUT_MS};
handle_cast('stop', State) ->
    lager:debug("asked to stop"),
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, ?INACTIVITY_TIMEOUT_MS}.

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
handle_info('timeout', State) ->
    lager:debug("inactivity timeout reached, going down"),
    {'stop', 'normal', State};
handle_info({'timeout', Ref, ?TOKEN_FILL_TIME}, #state{max_tokens=Max
                                                       ,tokens=Current
                                                       ,fill_rate=FillRate
                                                       ,fill_rate_time=FillTime
                                                       ,fill_ref=Ref
                                                       ,fill_as_block=FillBlock
                                                     }=State) ->
    {'noreply'
     ,State#state{tokens=add_tokens(Max, Current, FillRate, FillBlock, FillTime)
                  ,fill_ref=start_fill_timer(FillRate, FillBlock, FillTime)
                 }
     ,?INACTIVITY_TIMEOUT_MS
    };
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, ?INACTIVITY_TIMEOUT_MS}.

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
    lager:debug("token bucket going down: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% fill_as_block:
%%   add all tokens (up to max_tokens) on the second when true
%%   if false, start timer to try to add 1s/fill_rate * 1000 ms
%%   If fill_rate is 10 tokens/s, (1s / 10 tokens/s) * 1000ms = 100ms
%%   If fill_rate is 100 tokens/s, (1s / 100 tokens/s) * 1000ms = 10ms
%%   Maxes out at 1ms

-spec fill_time_in_ms(fill_rate_time() | pos_integer()) -> pos_integer().
fill_time_in_ms('second') -> 1000;
fill_time_in_ms('minute') -> ?MILLISECONDS_IN_MINUTE;
fill_time_in_ms('hour') -> ?MILLISECONDS_IN_HOUR;
fill_time_in_ms('day') -> ?MILLISECONDS_IN_DAY;
fill_time_in_ms(N) when is_integer(N) -> N.

-spec start_fill_timer(pos_integer(), boolean(), fill_rate_time() | pos_integer()) -> reference().
start_fill_timer(_FillRate, 'true', FillTime) ->
    start_fill_timer(fill_time_in_ms(FillTime));
start_fill_timer(FillRate, 'false', FillTime) ->
    start_fill_timer(fill_timeout(FillRate, fill_time_in_ms(FillTime))).

-spec fill_timeout(pos_integer(), pos_integer()) -> pos_integer().
fill_timeout(FillRate, FillTime) ->
    case FillTime div FillRate of
        N when N < 1 -> 1;
        N -> N
    end.

-spec start_fill_timer(pos_integer()) -> reference().
start_fill_timer(Timeout) ->
    erlang:start_timer(Timeout, self(), ?TOKEN_FILL_TIME).

-spec add_tokens(pos_integer(), non_neg_integer(), pos_integer(), boolean(), fill_rate_time() | pos_integer()) ->
                        non_neg_integer().
add_tokens(Max, Count, FillRate, 'true', _FillTime) ->
    constrain(Max, Count, FillRate);
add_tokens(Max, Count, FillRate, 'false', FillTime) ->
    FillTimeout = fill_timeout(FillRate, fill_time_in_ms(FillTime)),
    FillShard = FillTimeout div FillRate,

    constrain(Max, Count, FillShard).

-spec constrain(pos_integer(), pos_integer(), pos_integer()) -> pos_integer().
constrain(Max, Count, Inc) ->
    case Count+Inc of
        N when N > Max -> Max;
        N -> N
    end.

-ifdef(TEST).

-endif.
