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
-export([prepare/2]).
-export([execute/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include_lib("lineman/src/lineman.hrl").

-define(SERVER, fs_simulator).

-record(state, {event_handler
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

-spec prepare/2 :: (xml(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
prepare(_Xml, Workorder) -> Workorder.

-spec execute/2 :: (xml(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
execute(Xml, Workorder) ->
    Action = lineman_util:xml_attribute("action", Xml),
    execute_action(Action, Xml),
    Workorder.

-spec execute_action/2 :: (ne_binary(), #xmlElement{}) -> 'ok'.
execute_action(<<"fetch">>, Step) ->
    Module = lineman_util:xml_atom_attribute("fetch-module", Step),
    Key = lineman_util:xml_attribute("fetch-key", Step),
    Property = lineman_util:xml_attribute("fetch-property", Step),
    Value = lineman_util:xml_attribute("fetch-value", Step),
    Id = lineman_util:xml_attribute("fetch-id", Step),
    CallId = lineman_util:xml_attribute("call-id", Step),
    Event = [CallId | lineman_util:xml_content(Step)],
    gen_server:cast(?SERVER, {fetch, Module, Key, Property, Value, Id, Event});
execute_action(<<"publish">>, Step) ->
    CallId = lineman_util:xml_attribute("call-id", Step),
    Event = lineman_util:xml_content(Step),
    gen_server:cast(?SERVER, {event, CallId, Event}).

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
handle_call({parameter, "default-event-headers", Parameter}, _From, State) -> 
    DefaultEventHeaders = lineman_util:xml_content(Parameter),
    lager:debug("loaded default event headers: ~p", [DefaultEventHeaders]),
    {reply, ok, State#state{default_event_headers=DefaultEventHeaders}};
handle_call({parameter, "bind", Parameter}, _From, #state{static_responses=StaticResponses}=State) -> 
    Binding = lineman_util:xml_attribute("binding", Parameter),
    Args = lineman_util:xml_string_attribute("args", Parameter, []),
    Value = case lineman_util:xml_attribute("clean", Parameter) of
                <<"false">> -> lineman_util:xml_content(Parameter, false);
                _Else1 -> lineman_util:xml_content(Parameter)
            end,
    Msg = case lineman_util:xml_attribute("error", Parameter) of
              <<"true">> -> {error, Value};
              _Else2  -> {ok, Value}
          end,
    lager:debug("added static binding result for ~s with arg '~s'", [Binding, Args]),
    {reply, ok, State#state{static_responses=dict:store({Binding, Args}, Msg, StaticResponses)}};
handle_call({parameter, "connect", Parameter}, _From, State) -> 
    Node = lineman_util:xml_string_attribute("node", Parameter, "ecallmgr"),
    Host = lineman_util:xml_string_attribute("host", Parameter, net_adm:localhost()),
    DefaultCookie = lineman_util:try_get_cookie_from_vmargs(?ECALL_VM_ARGS),
    Cookie = lineman_util:xml_atom_attribute("cookie", Parameter, DefaultCookie),
    lager:info("attempting to connect fs simulator to target '~s@~s' with cookie '~s'", [Node, Host, Cookie]),
    case lineman_util:try_connect_to_target(Node, Cookie, Host) of
        {ok, Target} -> 
            c:nl(freeswitch),
            spawn(fun() ->
                          R = rpc:call(Target, ecallmgr_fs_nodes, add, [erlang:node()]),
                          lager:debug("rpc call result: ~p", [R])
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
handle_cast({event, CallId, Event}, #state{events=Events, event_handler=EventHandler
                                           ,default_event_headers=DefaultHeaders}=State) ->
    Class = proplists:get_value(<<"Event-Name">>, Event),
    Subclass = proplists:get_value(<<"Event-Subclass">>, Event),
    UUID = proplists:get_value(<<"call-id">>, Event, CallId),
    case sets:is_element({Class, Subclass}, Events) of
        false -> ok;
        true ->
            lager:debug("sending event ~s ~s to ~p", [Class, Subclass, EventHandler]),
            EventHandler ! {event, [UUID | DefaultHeaders ++ Event]}
    end,
    {noreply, State};
handle_cast({fetch, Module, _Key, _Property, _Value, _Id, _Event}=Req, #state{bindings=Bindings}=State) ->
    case dict:find(wh_util:to_binary(Module), Bindings) of
        error -> ok;
        {ok, Pids} ->
            lager:debug("sending fetch request for ~s", [Module]),
            [Pid ! Req || Pid <- Pids, is_pid(Pid)]
    end,
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
    {noreply, State#state{bindings=dict:append(Type, Pid, Bindings)}};
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
    case dict:find({Route, Term}, StaticResponses) of
        {ok, Value} -> 
            lager:info("sending static response for '~s'", [Route]),
            Pid ! Value;
        error -> lineman_bindings:map(Route, Term)
    end,
    {noreply, State};
handle_info(reset, State) ->
    {noreply, reset(State)};
handle_info(_Info, State) ->
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
    Class = list_to_binary(atom_to_list(AddEvent)),
    lager:info("added binding for event '~s'", [Class]),
    add_events(AddEvents, sets:add_element({Class, undefined}, Events));
add_events([], Events) ->
    Events.

add_custom_events([AddEvent|AddEvents], Events) ->
    Subclass = list_to_binary(atom_to_list(AddEvent)),
    lager:info("added binding for event 'CUSTOM ~s'", [Subclass]),
    add_custom_events(AddEvents, sets:add_element({<<"CUSTOM">>, Subclass}, Events));
add_custom_events([], Events) ->
    Events.

remove_events(['CUSTOM'|RemoveEvents], Events) ->
    remove_custom_events(RemoveEvents, Events);
remove_events([RemoveEvent|RemoveEvents], Events) ->
    Class = list_to_binary(atom_to_list(RemoveEvent)),
    lager:info("removed binding for event '~s'", [Class]),
    remove_events(RemoveEvents, sets:del_element({Class, undefined}, Events));
remove_events([], Events) ->
    Events.

remove_custom_events([RemoveEvent|RemoveEvents], Events) ->
    Subclass = list_to_binary(atom_to_list(RemoveEvent)),
    lager:info("removed binding for event 'CUSTOM ~s'", [Subclass]),
    remove_custom_events(RemoveEvents, sets:del_element({<<"CUSTOM">>, Subclass}, Events));
remove_custom_events([], Events) ->
    Events.
