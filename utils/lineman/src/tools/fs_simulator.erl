%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%------------------------------------------------------------------
-module(fs_simulator).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-export([register_event_handler/1]).
-export([binding_handler/1]).
-export([api_handler/1]).
-export([events_handler/1]).

-define(SERVER, ?MODULE).
-define(FS_STATUS, <<"UP 0 years, 18 days, 13 hours, 41 minutes, 1 second, 777 milliseconds, 964 microseconds\nFreeSWITCH is ready\n147 session(s) since startup\n0 session(s) 0/200\n5000 session(s) max\nmin idle cpu 0.00/43.00\n">>).
-define(FS_ZERO_CHANNELS, <<"\n0 total.\n">>).

-record(state, {}).

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
    gen_server:start_link({local, fs_simulator}, ?SERVER, [], []).

%%%===================================================================
%%% gen_listener callbacks
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
    lager:debug("starting fs simulator"),
    case lineman_util:get_target("ecallmgr") of
        {error, Reason} -> 
            lager:info("failed to connect to the ecallmgr node: ~p", [Reason]),
            {stop, Reason};
        {ok, Target} ->
            _ = lineman_bindings:bind(<<"fs_event.request.register_event_handler">>, ?MODULE, register_event_handler),
            _ = lineman_bindings:bind(<<"fs_event.request.bind.*">>, ?MODULE, binding_handler),
            _ = lineman_bindings:bind(<<"fs_event.request.api.status">>, ?MODULE, api_handler),
            _ = lineman_bindings:bind(<<"fs_event.request.api.show">>, ?MODULE, api_handler),
            _ = lineman_bindings:bind(<<"fs_event.request.events">>, ?MODULE, events_handler),
            connect_simulator(Target),
            {ok, #state{}, 0}
    end.

register_event_handler(Pid) ->
    lager:info("registered event handler", []),
    Pid ! ok.

binding_handler({Pid, _Binding}) ->
    lager:info("adding binding for ~s", [_Binding]),
    Pid ! ok.

api_handler({Pid, show, "channels"}) ->
    lager:info("responding to show channels request"),
    Pid ! {ok, ?FS_ZERO_CHANNELS};    
api_handler({Pid, status, []}) ->
    lager:info("responding to status request"),
    Pid ! {ok, ?FS_STATUS}.

events_handler({Pid, _Events}) ->
    lager:info("registering events handlers"),
    Pid ! ok.

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
handle_call(_Msg, _From, State) ->
    {noreply, State}.

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
handle_info({Pid, register_event_handler}, State) ->
    Route = list_to_binary(["fs_event.request.register_event_handler"]),
    lineman_bindings:map(Route, Pid),
    {noreply, State};
handle_info({Pid, bind, Type}, State) ->
    Route = list_to_binary(["fs_event.request.bind.", atom_to_list(Type)]),
    lineman_bindings:map(Route, {Pid, Type}),
    {noreply, State};
handle_info({Pid, send, Term}, State) ->
    Route = list_to_binary(["fs_event.request.send"]),
    lineman_bindings:map(Route, {Pid, Term}),
    {noreply, State};
handle_info({Pid, fetch_reply, FetchID, Reply}, State) ->
    Route = list_to_binary(["fs_event.request.fetch_reply.", FetchID]),
    lineman_bindings:map(Route, {Pid, FetchID, Reply}), 
    {noreply, State};
handle_info({Pid, api, Cmd, Args}, State) ->
    Route = list_to_binary(["fs_event.request.api.", atom_to_list(Cmd)]),
    lineman_bindings:map(Route, {Pid, Cmd, Args}), 
    {noreply, State};
handle_info({Pid, bgapi, Cmd, Args}, State) ->
    Route = list_to_binary(["fs_event.request.bgapi.", Cmd]),
    lineman_bindings:map(Route, {Pid, Cmd, Args}), 
    {noreply, State};
handle_info({Pid, event, Events}, State) ->
    Route = list_to_binary(["fs_event.request.events"]),
    lineman_bindings:map(Route, {Pid, Events}), 
    {noreply, State};
handle_info({Pid, session_event, Events}, State) ->
    Route = list_to_binary(["fs_event.request.session_events"]),
    lineman_bindings:map(Route, {Pid, Events}), 
    {noreply, State};
handle_info({Pid, nixevent, Events}, State) ->
    Route = list_to_binary(["fs_event.request.nixevents"]),
    lineman_bindings:map(Route, {Pid, Events}), 
    {noreply, State};
handle_info({Pid, session_nixevent, Events}, State) ->
    Route = list_to_binary(["fs_event.request.session_nixevents"]),
    lineman_bindings:map(Route, {Pid, Events}), 
    {noreply, State};
handle_info({Pid, noevents}, State) ->
    Route = list_to_binary(["fs_event.request.noevents"]),
    lineman_bindings:map(Route, Pid), 
    {noreply, State};
handle_info({Pid, session_noevents}, State) ->
    Route = list_to_binary(["fs_event.request.session_noevents"]),
    lineman_bindings:map(Route, Pid), 
    {noreply, State};
handle_info({Pid, close, Reason}, State) ->
    Route = list_to_binary(["fs_event.request.close"]),
    lineman_bindings:map(Route, {Pid, Reason}), 
    lineman_bindings:flush(<<"fs_event.request.#">>),
    {noreply, State};
handle_info({Pid, sendevent, 'CUSTOM', SubClassName, Headers}, State) ->
    Route = list_to_binary(["fs_event.request.sendevent.custom.", SubClassName]),
    lineman_bindings:map(Route, {Pid, SubClassName, Headers}),
    {noreply, State};
handle_info({Pid, sendevent, EventName, Headers}, State) ->
    Route = list_to_binary(["fs_event.request.sendevent.", EventName]),
    lineman_bindings:map(Route, {Pid, EventName, Headers}),
    {noreply, State};
handle_info({Pid, sendmsg, UUID, Headers}, State) ->
    Route = list_to_binary(["fs_event.request.sendmsg.", UUID]),
    lineman_bindings:map(Route, {Pid, UUID, Headers}),
    {noreply, State};
handle_info({Pid, getpid}, State) ->
    Pid ! {ok, self()},
    {noreply, State};
handle_info({Pid, handlecall, UUID}, State) ->
    Route = list_to_binary(["fs_event.request.handlecall.", UUID]),
    lineman_bindings:map(Route, {Pid, UUID}),
    {noreply, State};
handle_info({Pid, handlecall, UUID, Process}, State) ->
    Route = list_to_binary(["fs_event.request.handlecall.", UUID]),
    lineman_bindings:map(Route, {Pid, UUID, Process}),
    {noreply, State};
handle_info({Pid, start_handler, Type}, State) ->
    Route = list_to_binary(["fs_event.request.start_handler"]),
    lineman_bindings:map(Route, {Pid, Type}),
    {noreply, State};
handle_info(_Info, State) ->
    lager:info("Unknown fs event: ~p", [_Info]),
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate/2 :: (term(), #state{}) -> 'ok'.
terminate(_Reason, _State) ->
    lager:info("fs simulator termination ~p", [_Reason]).

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
connect_simulator(Target) ->
    c:nl(freeswitch),
    R = rpc:call(Target, ecallmgr_fs_handler, add_fs_node, [erlang:node()]),
    lager:info("rpc call result: ~p", [R]).
