%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%% Trigger jobs for execution
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_tasks_trigger).
-behaviour(gen_server).

-export([start_link/0]).
-export([status/0]).

%%% gen_server callbacks
-export([init/1
        ,handle_cast/2
        ,handle_call/3
        ,handle_info/2
        ,code_change/3
        ,terminate/2
        ]).

-include("tasks.hrl").

-define(SERVER, {'via', 'kz_globals', ?MODULE}).

-record(state, {minute_ref = minute_timer() :: reference()
               ,hour_ref = hour_timer() :: reference()
               ,day_ref = day_timer() :: reference()
               }).
-type state() :: #state{}.


%%%===================================================================
%%% API
%%%===================================================================

-spec status() -> kz_proplist().
status() ->
    gen_server:call(?SERVER, 'status').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    case gen_server:start_link(?SERVER, ?MODULE, [], []) of
        {'error', {'already_started', Pid}} ->
            'true' = link(Pid),
            {'ok', Pid};
        Other -> Other
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    _ = process_flag('trap_exit', 'true'),
    kz_util:put_callid(?MODULE),
    lager:debug("started ~s", [?MODULE]),
    {'ok', #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call('status', _From, #state{minute_ref = Minute
                                   ,hour_ref = Hour
                                   ,day_ref = Day
                                   }=State) ->
    Timers = [{'minute', erlang:read_timer(Minute)}
             ,{'hour', erlang:read_timer(Hour)}
             ,{'day', erlang:read_timer(Day)}
             ],
    {'reply', Timers, State};

handle_call(_Request, _From, State) ->
    lager:debug("unhandled call ~p from ~p", [_Request, _From]),
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'EXIT', _Pid, normal}, State) ->
    lager:debug("job ~p terminated normally", [_Pid]),
    {noreply, State};
handle_info({'EXIT', _Pid, _Reason}, State) ->
    lager:error("job ~p crashed: ~p", [_Pid, _Reason]),
    {noreply, State};

handle_info({timeout, Ref, _Msg}, #state{minute_ref = Ref}=State) ->
    spawn_jobs(Ref, ?TRIGGER_MINUTE),
    {'noreply', State#state{minute_ref = minute_timer()}};

handle_info({timeout, Ref, _Msg}, #state{hour_ref = Ref}=State) ->
    spawn_jobs(Ref, ?TRIGGER_HOUR),
    {'noreply', State#state{hour_ref = hour_timer()}};

handle_info({timeout, Ref, _Msg}, #state{day_ref = Ref}=State) ->
    spawn_jobs(Ref, ?TRIGGER_DAY),
    {'noreply', State#state{day_ref = day_timer()}};

handle_info(_Info, State) ->
    lager:debug("unhandled message ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?MODULE, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec minute_timer() -> reference().
minute_timer() ->
    erlang:start_timer(?MILLISECONDS_IN_MINUTE, self(), ok).

-spec hour_timer() -> reference().
hour_timer() ->
    erlang:start_timer(?MILLISECONDS_IN_HOUR, self(), ok).

-spec day_timer() -> reference().
day_timer() ->
    erlang:start_timer(?MILLISECONDS_IN_DAY, self(), ok).

-spec spawn_jobs(reference(), ne_binary()) -> ok.
spawn_jobs(Ref, Binding) ->
    CallId = make_callid(Ref, Binding),
    _Pid = erlang:spawn_link(fun () ->
                                     _ = kz_util:put_callid(CallId),
                                     tasks_bindings:map(Binding, [])
                             end),
    lager:debug("binding ~s triggered ~p via ~p", [Binding, _Pid, Ref]).

-spec make_callid(reference(), ne_binary()) -> ne_binary().
make_callid(Ref, Binding) ->
    Key = lists:last(binary:split(Binding, <<$.>>, [global])),
    Id = ref_to_id(Ref),
    <<"task_", Key/binary, "_", Id/binary>>.

ref_to_id(Ref) ->
    Bin = list_to_binary(io_lib:format("~p", [Ref])),
    Start = <<"#Ref<">>,
    StartSize = byte_size(Start),
    Size = byte_size(Bin) - StartSize - 1,
    <<Start:StartSize/binary, Id:Size/binary, ">">> = Bin,
    Id.

%%% End of Module.
