%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%------------------------------------------------------------------
-module(lineman_tool_freeswitch).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include_lib("lineman/src/lineman.hrl").

-define(SERVER, ?MODULE).

-record(state, {event_handlers = []
                ,bindings = []
                ,events = sets:new()
                ,session_events = sets:new()
                ,default_event_headers = []
                ,static_binding_results = []
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
    {ok, #state{}}.

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
handle_call(reset, _From, State) ->
    {reply, ok, reset(State)};
handle_call({parameter, "default-event-headers", Parameter}, _From, State) -> 
    DefaultEventHeaders = lineman_util:get_xml_element_content(Parameter),
    lager:debug("loaded default event headers: ~p", [DefaultEventHeaders]),
    {reply, ok, State#state{default_event_headers=DefaultEventHeaders}};
handle_call({parameter, "bind", Parameter}, _From, #state{static_binding_results=SBRs}=State) -> 
    Binding = wh_util:to_binary(lineman_util:get_xml_attribute_value("binding", Parameter)),
    Args = wh_util:to_list(lineman_util:get_xml_attribute_value("args", Parameter, [])),
    Value = case lineman_util:get_xml_attribute_value("clean", Parameter) of
                "false" -> lineman_util:get_xml_element_content(Parameter, false);
                _Else -> lineman_util:get_xml_element_content(Parameter)
            end,
    lager:debug("added static binding result for ~s with arg '~s'", [Binding, Args, Value]),
    Pid = spawn_link(?MODULE, static_responder, [Binding, Args, Value]),
    {reply, ok, State#state{static_responder=[Pid|SBRs]}};
handle_call({parameter, "connect", Parameter}, _From, State) -> 
    Node = lineman_util:get_xml_attribute_value("node", Parameter, "ecallmgr"),
    Host = lineman_util:get_xml_attribute_value("host", Parameter, net_adm:localhost()),
    DefaultCookie = lineman_util:try_get_cookie_from_vmargs(?ECALL_VM_ARGS),
    Cookie = lineman_util:get_xml_attribute_value("cookie", Parameter, DefaultCookie),
    lager:info("attempting to connect fs simulator to target '~s@~s' with cookie '~s'", [Node, Host, Cookie]),
    case lineman_util:try_connect_to_target(Node, Cookie, Host) of
        {ok, Target} -> 
            c:nl(freeswitch),
            spawn(fun() ->
                          R = rpc:call(Target, ecallmgr_fs_handler, add_fs_node, [erlang:node()]),
                          lager:info("rpc call result: ~p", [R])
                  end),
            {reply, ok, State};
        {error, _}=E ->
            {reply, E, reset(State)}
    end;
handle_call(_Msg, _From, State) ->
    {reply, {error, not_implemented}, State}.

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
handle_info({Pid, register_event_handler}, #state{event_handlers=EventHandlers}=State) ->
    Pid ! ok,
    Ref = erlang:monitor(process, Pid),
    lager:debug("registered event handler ~p", [Pid]),
    {noreply, State#state{event_handlers=[{Pid, Ref}|EventHandlers]}};
handle_info({Pid, bind, Type}, #state{bindings=Bindings}=State) ->
    Pid ! ok,
    lager:debug("added binding '~s' to ~p", [Type, Pid]),
    {noreply, State#state{bindings=[{Type, Pid}|Bindings]}};
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
handle_info({Pid, event, AddEvents}, #state{events=Events}=State) ->
    Pid ! ok,
    NewEvents = lists:foldr(fun(Event, E) -> 
                                    lager:debug("added event binding: ~s", [Event]),
                                    sets:add_element(Event, E) 
                            end, Events, AddEvents),
    {noreply, State#state{events=NewEvents}};
handle_info({Pid, session_event, Events}, State) ->
    Route = list_to_binary(["fs_event.request.session_events"]),
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
-spec reset/1 :: (#state{}) -> #state{}.
reset(State) ->
    lager:info("reseting state", []),
    State.
