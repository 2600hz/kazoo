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
-export([set_parameter/2]).
-export([prepare/1]).
-export([execute/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include_lib("lineman/src/lineman.hrl").

-define(SERVER, fs_simulator).

-record(state, {event_handler = undefined
                ,bindings = dict:new()
                ,events = sets:new()
                ,session_events = dict:new()
                ,default_event_headers = []
                ,static_responses = dict:new()
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec set_parameter/2 :: (string(), #xmlElement{}) -> 'ok'.
set_parameter(Name, Parameter) ->
    gen_server:call(?SERVER, {parameter, Name, Parameter}).

-spec prepare/1 :: (#xmlElement{}) -> 'ok'.
prepare(_) -> 'ok'.

-spec execute/1 :: (#xmlElement{}) -> 'ok'.
execute(_) -> 'ok'.

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


%%events_handler(Pid, Args) ->
%%    lineman_bindings:bind(<<"fs_event.request.events">>, ?MODULE, events_handler).

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
handle_call({parameter, "default-event-headers", Parameter}, _From, State) -> 
    DefaultEventHeaders = lineman_util:get_xml_element_content(Parameter),
    lager:debug("loaded default event headers: ~p", [DefaultEventHeaders]),
    {reply, ok, State#state{default_event_headers=DefaultEventHeaders}};
handle_call({parameter, "bind", Parameter}, _From, #state{static_responses=StaticResponses}=State) -> 
    Binding = wh_util:to_binary(lineman_util:get_xml_attribute_value("binding", Parameter)),
    Args = wh_util:to_list(lineman_util:get_xml_attribute_value("args", Parameter, [])),
    Value = case lineman_util:get_xml_attribute_value("clean", Parameter) of
                "false" -> lineman_util:get_xml_element_content(Parameter, false);
                _Else1 -> lineman_util:get_xml_element_content(Parameter)
            end,
    Msg = case lineman_util:get_xml_attribute_value("error", Parameter) of
              "true" -> {error, Value};
              _Else2  -> {ok, Value}
          end,
    lager:info("added static binding result for ~s with arg '~s'", [Binding, Args]),
    {reply, ok, State#state{static_responses=dict:store({Binding, Args}, Msg, StaticResponses)}};
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

handle_cast({Pid, session_event, Events}, State) ->
    Route = list_to_binary(["fs_event.request.session_events"]),
    lineman_bindings:map(Route, {Pid, Events}),     
    {noreply, State};
handle_cast({Pid, session_nixevent, Events}, State) ->
    Route = list_to_binary(["fs_event.request.session_nixevents"]),
    lineman_bindings:map(Route, {Pid, Events}), 
    {noreply, State};
handle_cast({Pid, session_noevents}, State) ->
    Route = list_to_binary(["fs_event.request.session_noevents"]),
    lineman_bindings:map(Route, Pid), 
    {noreply, State};

handle_cast({Pid, close, Reason}, State) ->
    Route = list_to_binary(["fs_event.request.close"]),
    lineman_bindings:map(Route, {Pid, Reason}), 
    lineman_bindings:flush(<<"fs_event.request.#">>),
    {noreply, State};

handle_cast({Pid, sendevent, 'CUSTOM', SubClassName, Headers}, State) ->
    Route = list_to_binary(["fs_event.request.sendevent.custom.", SubClassName]),
    lineman_bindings:map(Route, {Pid, SubClassName, Headers}),
    {noreply, State};
handle_cast({Pid, sendevent, EventName, Headers}, State) ->
    Route = list_to_binary(["fs_event.request.sendevent.", EventName]),
    lineman_bindings:map(Route, {Pid, EventName, Headers}),
    {noreply, State};

handle_cast({Pid, sendmsg, UUID, Headers}, State) ->
    Route = list_to_binary(["fs_event.request.sendmsg.", UUID]),
    lineman_bindings:map(Route, {Pid, UUID, Headers}),
    {noreply, State};

handle_cast({Pid, handlecall, UUID}, State) ->
    Route = list_to_binary(["fs_event.request.handlecall.", UUID]),
    lineman_bindings:map(Route, {Pid, UUID}),
    {noreply, State};

handle_cast({Pid, handlecall, UUID, Process}, State) ->
    Route = list_to_binary(["fs_event.request.handlecall.", UUID]),
    lineman_bindings:map(Route, {Pid, UUID, Process}),
    {noreply, State};

handle_cast({Pid, start_handler, Type}, State) ->
    Route = list_to_binary(["fs_event.request.start_handler"]),
    lineman_bindings:map(Route, {Pid, Type}),
    {noreply, State};

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
%%-------------------------------------------------------------------
handle_info({Pid, <<"freeswitch.register_event_handler">>, []}, State) ->
    Pid ! {ok, accepted},
    lager:info("registered event handler ~p", [Pid]),
    {noreply, State#state{event_handler=Pid}};
handle_info({Pid, <<"freeswitch.getpid">>, []}, State) ->
    Pid ! {ok, self()},
    {noreply, State};
handle_info({Pid, <<"freeswitch.bind.", Type/binary>>, []}, #state{bindings=Bindings}=State) ->
    Pid ! {ok, accepted},
    lager:info("added binding '~s' to ~p", [Type, Pid]),
    {noreply, State#state{bindings=dict:store(Type, Pid, Bindings)}};
handle_info({Pid, <<"freeswitch.event">>, AddEvents}, #state{events=Events}=State) ->
    Pid ! {ok, accepted},
    {noreply, State#state{events=add_events(AddEvents, Events)}};
handle_info({Pid, <<"freeswitch.nixevent">>, RemoveEvents}, #state{events=Events}=State) ->
    Pid ! {ok, accepted},
    {noreply, State#state{events=remove_events(RemoveEvents, Events)}};
handle_info({Pid, <<"freeswitch.noevents">>, []}, State) ->
    Pid ! {ok, accepted},
    {noreply, State#state{events=sets:new()}};
handle_info({Pid, Route, Term}, #state{static_responses=StaticResponses}=State) ->
    lager:info("received '~s': ~p", [Route, Term]),
    case dict:find({Route, Term}, StaticResponses) of
        {ok, Value} -> 
            lager:info("sending static response for '~s'", [Route]),
            Pid ! Value;
        error ->
            lager:info("running map", []),
            lineman_bindings:map(Route, {Pid, Term})
    end,
    {noreply, State};
handle_info(reset, State) ->
    {noreply, reset(State)};
handle_info(_Info, State) ->
    lager:info("Unknown info message: ~p", [_Info]),
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

add_events(['CUSTOM'|AddEvents], Events) ->
    add_custom_events(AddEvents, Events);
add_events([AddEvent|AddEvents], Events) ->
    lager:info("added binding for event '~s'", [AddEvent]),
    add_events(AddEvents, sets:add_element(AddEvent, Events));
add_events([], Events) ->
    Events.

add_custom_events([AddEvent|AddEvents], Events) ->
    lager:info("added binding for event 'CUSTOM ~s'", [AddEvent]),
    add_custom_events(AddEvents, sets:add_element({'CUSTOM', AddEvent}, Events));
add_custom_events([], Events) ->
    Events.

remove_events(['CUSTOM'|RemoveEvents], Events) ->
    remove_custom_events(RemoveEvents, Events);
remove_events([RemoveEvent|RemoveEvents], Events) ->
    lager:info("removed binding for event '~s'", [RemoveEvent]),
    remove_events(RemoveEvents, sets:del_element(RemoveEvent, Events));
remove_events([], Events) ->
    Events.

remove_custom_events([RemoveEvent|RemoveEvents], Events) ->
    lager:info("removed binding for event 'CUSTOM ~s'", [RemoveEvent]),
    remove_custom_events(RemoveEvents, sets:del_element({'CUSTOM', RemoveEvent}, Events));
remove_custom_events([], Events) ->
    Events.
