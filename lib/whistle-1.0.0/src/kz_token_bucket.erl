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
-export([start_link/2, start_link/3
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

-record(state, {max_tokens = 100 :: pos_integer() % max size of bucket
                ,tokens = 0 :: non_neg_integer() % current count
                ,fill_rate = 1 :: pos_integer() % tokens/sec added
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
start_link(Max, FillRate, FillBlock) when FillRate > 0,
                                          Max > 0,
                                          is_boolean(FillBlock) ->
    gen_server:start_link(?MODULE, [Max, FillRate, FillBlock], []).

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
init([Max, FillRate, FillBlock]) ->
    {'ok', #state{max_tokens=Max
                  ,fill_rate=FillRate
                  ,fill_ref=start_fill_timer(FillRate, FillBlock)
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
        N when N >= 0 -> {'reply', 'true', State#state{tokens=N}};
        _ -> {'reply', 'false', State}
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
                                                      ,fill_ref=Ref
                                                      ,fill_as_block=FillBlock
                                                     }=State) ->
    {'noreply', State#state{tokens=add_tokens(Max, Current, FillRate, FillBlock)
                            ,fill_ref=start_fill_timer(FillRate, FillBlock)
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

start_fill_timer(_FillRate, 'true') -> start_fill_timer(1000);
start_fill_timer(FillRate, 'false') ->
    start_fill_timer(fill_timeout(FillRate)).

fill_timeout(FillRate) ->
    case 1000 div FillRate of
        N when N < 1 -> 1;
        N -> N
    end.

start_fill_timer(Timeout) -> erlang:start_timer(Timeout, self(), ?TOKEN_FILL_TIME).

add_tokens(Max, Count, FillRate, 'true') ->
    constrain(Max, Count, FillRate);
add_tokens(Max, Count, FillRate, 'false') ->
    FillTimeout = fill_timeout(FillRate),
    FillShard = FillRate div FillTimeout,
    constrain(Max, Count, FillShard).

constrain(Max, Count, Inc) ->
    case Count+Inc of
        N when N > Max -> Max;
        N -> N
    end.

-ifdef(TEST).

-endif.
