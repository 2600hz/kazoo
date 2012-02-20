%%%-------------------------------------------------------------------
%%% @author Jesper Louis andersen <>
%%% @copyright (C) 2011, Jesper Louis andersen
%%% @doc
%%%
%%% @end
%%% Created : 11 Nov 2011 by Jesper Louis andersen <>
%%%-------------------------------------------------------------------
-module(queue_srv).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/0, len/0, in/1, in/2, is_empty/0,
         out/0, out/1, pout/0,
         is_queue/0, to_list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { mod, q }).

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
start_link(Mod) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Mod], []).

stop() ->
    call(stop).

call(M) ->
    gen_server:call(?SERVER, M, infinity).

in(I) ->
    call({in, I}).

in(I, P) ->
    call({in, I, P}).

len() ->
    call(len).

is_empty() ->
    call(is_empty).

is_queue() ->
    call(is_queue).

to_list() ->
    call(to_list).

out() ->
    call(out).

out(P) ->
    call({out, P}).

pout() ->
    call(pout).

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
init([Mod]) ->
    {ok, #state{ mod = Mod,
                 q = Mod:new() }}.

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
handle_call(stop, _F, S) ->
    {stop, normal, ok, S};
handle_call({in, Item}, _F, #state { q = Q, mod = M } = S) ->
    NQ = M:in(Item, Q),
    {reply, ok, S#state { q = NQ }};
handle_call({in, Item, Prio}, _F, #state { q = Q, mod = M } = S) ->
    NQ = M:in(Item, Prio, Q),
    {reply, ok, S#state { q = NQ }};
handle_call({out, P},         _F, #state { q = Q, mod = M } = S) ->
    {R, NQ} = M:out(P, Q),
    {reply, R, S#state { q = NQ }};
handle_call(Ty, _F, #state { q = Q, mod = M } = S) when Ty == out;
                                                        Ty == pout ->
    {R, NQ} = M:Ty(Q),
    {reply, R, S#state { q = NQ }};
handle_call(Ty, _F, #state { q = Q, mod = M } = S) when Ty == is_queue;
                                                        Ty == is_empty;
                                                        Ty == len;
                                                        Ty == to_list ->
    R = M:Ty(Q),
    {reply, R, S};
handle_call(Req, From, State) ->
    error_logger:info_report([{handle_call, Req, From, State}]),
    Reply = ok,
    {reply, Reply, State}.

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
