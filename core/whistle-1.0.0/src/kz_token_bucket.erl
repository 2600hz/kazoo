%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
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
         ,consume/2
         ,tokens/1
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").

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
    start_link(Max, FillRate, FillBlock, 'second').
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

-spec consume(pid(), pos_integer()) -> boolean().
consume(Srv, Tokens) when is_integer(Tokens) andalso Tokens > 0 ->
    gen_server:call(Srv, {'consume', Tokens}).

-spec tokens(pid()) -> non_neg_integer().
tokens(Srv) -> gen_server:call(Srv, {'tokens'}).

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
    lager:debug("starting token bucket with ~b max, filling at ~b/~s, in a block: ~s"
                ,[Max, FillRate,FillTime, FillBlock]
               ),
    {'ok', #state{max_tokens=Max
                  ,fill_rate=FillRate
                  ,fill_rate_time=FillTime
                  ,fill_ref=start_fill_timer(FillRate, FillBlock, FillTime)
                  ,fill_as_block=FillBlock
                  ,tokens=Max
                 }}.

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
            {'reply', 'true', State#state{tokens=N}};
        _ ->
            lager:debug("not enough tokens (~p) to consume ~p", [Current, Req]),
            {'reply', 'false', State}
    end;
handle_call({'tokens'}, _From, #state{tokens=Current}=State) ->
    {'reply', Current, State};
handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

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
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

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
handle_info({'timeout', Ref, ?TOKEN_FILL_TIME}, #state{max_tokens=Max
                                                       ,tokens=Current
                                                       ,fill_rate=FillRate
                                                       ,fill_rate_time=FillTime
                                                       ,fill_ref=Ref
                                                       ,fill_as_block=FillBlock
                                                     }=State) ->
    {'noreply', State#state{tokens=add_tokens(Max, Current, FillRate, FillBlock, FillTime)
                            ,fill_ref=start_fill_timer(FillRate, FillBlock, FillTime)
                           }};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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

fill_time_in_ms('second') -> 1000;
fill_time_in_ms('minute') -> ?MILLISECONDS_IN_MINUTE;
fill_time_in_ms('hour') -> ?MILLISECONDS_IN_HOUR;
fill_time_in_ms('day') -> ?MILLISECONDS_IN_DAY;
fill_time_in_ms(N) when is_integer(N) -> N.

start_fill_timer(_FillRate, 'true', FillTime) ->
    start_fill_timer(fill_time_in_ms(FillTime));
start_fill_timer(FillRate, 'false', FillTime) ->
    start_fill_timer(fill_timeout(FillRate, fill_time_in_ms(FillTime))).

fill_timeout(FillRate, FillTime) ->
    case FillTime div FillRate of
        N when N < 1 -> 1;
        N -> N
    end.

start_fill_timer(Timeout) -> erlang:start_timer(Timeout, self(), ?TOKEN_FILL_TIME).

add_tokens(Max, Count, FillRate, 'true', _FillTime) ->
    constrain(Max, Count, FillRate);
add_tokens(Max, Count, FillRate, 'false', FillTime) ->
    FillTimeout = fill_timeout(FillRate, fill_time_in_ms(FillTime)),
    FillShard = FillTimeout div FillRate,
    constrain(Max, Count, FillShard).

constrain(Max, Count, Inc) ->
    case Count+Inc of
        N when N > Max -> Max;
        N -> N
    end.

-ifdef(TEST).

-endif.
