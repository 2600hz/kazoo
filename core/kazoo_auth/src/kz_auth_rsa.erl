%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_auth_rsa).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, gen_rsa/2, max_jobs/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_MAXJOBS, 4).

-record(state, {port, requests=[], queue = queue:new(), limit}).

%%%===================================================================
%%% API
%%%===================================================================

stop() ->
    gen_server:call(?SERVER, stop).

gen_rsa(Bits, E) when is_integer(Bits), Bits > 0, E band 1 =:= 1 ->
    gen_server:call(?SERVER, {gen_rsa, Bits, E}, infinity).

max_jobs(Limit) when is_integer(Limit)
                     andalso Limit > 0 ->
    gen_server:call(?SERVER, {set_limit, Limit}, infinity).

start_link() ->
    Limit = case application:get_env(kazoo_auth, max_jobs) of
                undefined -> ?DEFAULT_MAXJOBS;
                {ok, Int} when is_integer(Int)
                               andalso Int > 0 -> Int
            end,
    gen_server:start_link({local, ?SERVER}, ?MODULE, Limit, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Limit) when is_integer(Limit)
                 andalso Limit > 0 ->
    Port = kz_auth_rsa_drv:open(),
    {ok, #state{port = Port}}.

handle_call(stop, _From, #state{} = State) ->
    {stop, normal, ok, State};
handle_call({gen_rsa, _Bits, _E} = Req, From, #state{} = State) ->
    Ref = erlang:phash2(make_ref()),
    Job = {Ref, From, Req},
    NewState0 = add_to_queue(Job, State),
    NewState1 = process_queue(NewState0),
    {noreply, NewState1};
handle_call({set_limit, Limit}, _From, #state{} = State)
  when is_integer(Limit)
       andalso Limit > 0 ->
    NewState = State#state{limit = Limit},
    {noreply, NewState};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, Ref, Data}, #state{port = Port, requests = Reqs} = State) ->
    case lists:keytake(Ref, 1, Reqs) of
        {value, {Ref, From}, NewRequests} ->
            gen_server:reply(From, {ok, Data}),
            NewState = process_queue(State#state{requests = NewRequests}),
            {noreply, NewState};
        false -> {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port} = _State) ->
    ok = kz_auth_rsa_drv:close(Port).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_to_queue(Job, #state{queue = Queue} = State) ->
    NewQueue = queue:in(Job, Queue),
    State#state{queue = NewQueue}.

process_queue(#state{requests = Reqs, queue = Q, limit = Limit} = State) ->
    case Reqs =:= []
        orelse length(Reqs) > Limit
    of
        true ->
            case queue:out(Q) of
                {{value, Job}, NewQ} ->
                    NewState = State#state{queue = NewQ},
                    start_job(Job, NewState);
                {empty, Q} -> State
            end;
        false -> State
    end.

start_job({Ref, From, {gen_rsa, Bits, E}},
          #state{port = Port, requests = Requests} = State) ->
    case kz_auth_rsa_drv:gen_rsa(Port, Ref, Bits, E) of
        ok ->
            NewRequests = [{Ref, From}|Requests],
            State#state{requests = NewRequests};
        {error, _ErrNo} = Reply ->
            gen_server:reply(From, Reply),
            State
    end.
