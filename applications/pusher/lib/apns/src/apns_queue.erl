%%%-------------------------------------------------------------------
%%% @author Manuel Rubio <manuel@altenwald.com>
%%% @copyright (C) 2014 Altenwald Solutions, S.L.
%%% @doc apns4erl queue for recover in failure
%%% @end
%%%-------------------------------------------------------------------
-module(apns_queue).
-author('Manuel Rubio <manuel@altenwald.com>').

-include("apns.hrl").

-behaviour(gen_server).

-define(DEFAULT_MAX_ENTRIES, 1000).

-export([
    start_link/0,
    stop/1,

    in/2,
    fail/2,

    % callbacks
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    queue = queue:new() :: queue(),
    max_entries = ?DEFAULT_MAX_ENTRIES :: pos_integer()
}).

-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc  Stops the connection
-spec stop(QID :: pid()) -> ok.
stop(QID) ->
  gen_server:cast(QID, stop).

-spec in(QID :: pid(), Msg :: apns:msg()) -> ok.
in(QID, Msg) ->
    gen_server:cast(QID, {in, Msg}).

-spec fail(QID :: pid(), ID :: binary()) -> [apns:msg()].
fail(QID, ID) ->
    gen_server:call(QID, {fail, ID}).

%% @hidden
-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server implementation, a.k.a.: callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, #state{}}.

%% @hidden
-spec handle_cast(stop | term(), state()) -> {noreply, state()} | {stop, normal | {error, term()}, state()}.
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({in, Msg}, #state{max_entries=MaxEntries,queue=OldQueue}=State) ->
    Queue = case MaxEntries =< queue:len(OldQueue) of
        true -> queue:liat(OldQueue);
        false -> OldQueue
    end,
    {noreply, State#state{queue = queue:in(Msg, Queue)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
-spec handle_call(X::term(), reference(), state()) -> {reply, {Failed::apns:msg(), RestToRetry::[apns:msg()]}, state()}.
handle_call({fail, ID}, _From, #state{queue=Queue}=State) ->
    {reply, recover_fail(ID, Queue), State#state{queue=queue:new()}};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @hidden
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server implementation, a.k.a.: callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec recover_fail(ID::binary(), Queue::queue()) -> {apns:msg(), [apns:msg()]}.
%@hidden
recover_fail(ID, Queue) ->
    Now = apns:expiry(0),
    List = queue:to_list(queue:filter(fun
        (#apns_msg{expiry=Expiry}) -> Expiry > Now
    end, Queue)),
    DropWhile = fun(#apns_msg{id=I}) -> I =/= ID end,
    case lists:dropwhile(DropWhile, List) of
        [Failed|RestToRetry] -> {Failed, RestToRetry};
        [] -> {undefined, []}
    end.
