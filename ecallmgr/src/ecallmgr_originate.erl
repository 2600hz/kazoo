%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%    Karl Anderson
%%%    James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_originate).

-behaviour(gen_listener).

-export([start_link/2]).
-export([handle_originate_execute/2]).
-export([handle_call_events/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-record(state, {node :: atom()
                ,server_id :: api_binary()
                ,originate_req = wh_json:new() :: wh_json:object()
                ,uuid :: api_binary()
                ,action :: api_binary()
                ,dialstrings :: api_binary()
                ,queue = <<>> :: binary() 
                ,control_pid :: 'undefined' | pid()
                ,tref :: 'undefined' | reference()
               }).

-define(BINDINGS, [{self, []}]).
-define(RESPONDERS, [{{?MODULE, handle_originate_execute}, [{<<"dialplan">>, <<"originate_execute">>}]}
                     ,{{?MODULE, handle_call_events}, [{<<"call_event">>, <<"*">>}]}
                    ]).
-define(QUEUE_NAME, <<"">>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(ORIGINATE_PARK, <<"&park()">>).
-define(REPLY_TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link/2 :: (atom(), wh_json:object()) -> startlink_ret().
start_link(Node, JObj) ->
    gen_listener:start_link(?MODULE
                            ,[{bindings, ?BINDINGS}
                              ,{responders, ?RESPONDERS}
                              ,{queue_name, ?QUEUE_NAME}
                              ,{queue_options, ?QUEUE_OPTIONS}
                              ,{consume_options, ?CONSUME_OPTIONS}
                             ]
                            ,[Node, JObj]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call_events/2 :: (wh_json:object(), wh_proplist()) -> 'ok'.
handle_call_events(JObj, Props) ->
    Srv = props:get_value(server, Props),
    case props:get_value(uuid, Props) =:=  wh_json:get_value(<<"Call-ID">>, JObj)
        andalso wh_json:get_value(<<"Event-Name">>, JObj)
    of
        <<"CHANNEL_EXECUTE_COMPLETE">> ->
            case wh_json:get_value(<<"Application-Name">>, JObj) of
                <<"bridge">> ->
                    gen_listener:cast(Srv, {bridge_execute_complete, JObj});
                _Else -> ok
            end;
        <<"CHANNEL_DESTROY">> ->
            gen_listener:cast(Srv, {channel_destroy, JObj});
        _Else -> ok
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_originate_execute/2 :: (wh_json:object(), wh_proplist()) -> 'ok'.
handle_originate_execute(JObj, Props) ->
    true = wapi_dialplan:originate_execute_v(JObj),
    Srv = props:get_value(server, Props),
    UUID = props:get_value(uuid, Props),
    lager:debug("recv originate_execute for ~s", [UUID]),
    _ = case wh_json:get_ne_value(<<"Server-ID">>, JObj) of
            undefined -> ok;
            ServerId ->
                gen_listener:cast(Srv, {update_server_id, ServerId})
        end,
    wh_cache:store_local(?ECALLMGR_UTIL_CACHE, {UUID, start_listener}, true),
    gen_listener:cast(Srv, {originate_execute}).

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
init([Node, JObj]) ->
    _ = wh_util:put_callid(JObj),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    bind_to_events(freeswitch:version(Node), Node),
    case wapi_resource:originate_req_v(JObj) of
        false ->
            Error = <<"originate failed to execute as JObj did not validate">>,
            publish_error(Error, undefined, JObj, ServerId),
            {stop, normal};
        true ->
            Self = self(),
            spawn(fun() ->
                          Q = gen_listener:queue_name(Self),
                          gen_server:cast(Self, {queue_name, Q})
                  end),
            {ok, #state{node=Node, originate_req=JObj, server_id=ServerId}}
    end.

bind_to_events({ok, <<"mod_kazoo", _/binary>>}, Node) ->
    ok = freeswitch:event(Node, ['CUSTOM', 'loopback::bowout']);
bind_to_events(_, Node) ->
    gproc:reg({p, l, {call_event, Node, <<"loopback::bowout">>}}).

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
handle_call(_Request, _From, State) ->
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
handle_cast({queue_name, Q}, State) ->
    lager:debug("starting originate request", []),
    gen_listener:cast(self(), {get_originate_action}),
    {noreply, State#state{queue=Q}};

handle_cast({update_server_id, ServerId}, State) ->
    {noreply, State#state{server_id=ServerId}, hibernate};

handle_cast({maybe_update_node, Node}, #state{node=Node}=State) ->
    {noreply, State};
handle_cast({maybe_update_node, Node}, #state{node=_OldNode}=State) ->
    lager:debug("updating node from ~s to ~s", [_OldNode, Node]),
    {noreply, State#state{node=Node}, hibernate};

handle_cast({create_uuid}, #state{node=Node, originate_req=JObj}=State) ->
    lager:debug("creating a new uuid", []),
    {noreply, State#state{uuid=create_uuid(JObj, Node)}, hibernate};

handle_cast({get_originate_action}, #state{originate_req=JObj}=State) ->
    gen_listener:cast(self(), {build_originate_args}),
    ApplicationName = wh_json:get_value(<<"Application-Name">>, JObj),
    {noreply, State#state{action=get_originate_action(ApplicationName, JObj)}};

handle_cast({build_originate_args}, #state{uuid=undefined, action = ?ORIGINATE_PARK}=State) ->
    gen_listener:cast(self(), {create_uuid}),
    gen_listener:cast(self(), {build_originate_args}),
    {noreply, State};
handle_cast({build_originate_args}, #state{originate_req=JObj, uuid=UUID, action = ?ORIGINATE_PARK}=State) ->
    gen_listener:cast(self(), {originate_ready}),
    Endpoints = [wh_json:set_value(<<"origination_uuid">>, UUID, Endpoint)
                 || Endpoint <- wh_json:get_ne_value(<<"Endpoints">>, JObj, [])
                ],
    {noreply, State#state{dialstrings=build_originate_args(?ORIGINATE_PARK, Endpoints, JObj)}};    
handle_cast({build_originate_args}, #state{originate_req=JObj, action = Action}=State) ->
    gen_listener:cast(self(), {originate_execute}),
    Endpoints = wh_json:get_ne_value(<<"Endpoints">>, JObj, []),
    {noreply, State#state{dialstrings=build_originate_args(Action, Endpoints, JObj)}};

handle_cast({originate_ready}, #state{originate_req=JObj, node=Node, uuid=UUID
                                      ,queue=Q, server_id=ServerId}=State) ->
    lager:debug("preemptively starting call control process", []),
    case ecallmgr_call_sup:start_control_process(Node, UUID, ServerId) of
        {ok, CtrlPid} ->
            CtrlQ = ecallmgr_call_control:queue_name(CtrlPid),
            publish_originate_ready(CtrlQ, UUID, JObj, Q, ServerId),
            {noreply, State#state{control_pid=CtrlPid
                                  ,tref=erlang:send_after(?REPLY_TIMEOUT, self(), {abandon_originate})}};
        _Else ->
            Error = <<"failed to preemptively start a call control process">>,
            publish_error(Error, UUID, JObj, ServerId),
            {stop, normal, State}
    end;

handle_cast({originate_execute}, #state{tref=TRef}=State) when is_reference(TRef) ->
    gen_listener:cast(self(), {originate_execute}),
    _ = erlang:cancel_timer(TRef),
    {noreply, State#state{tref=undefined}};
handle_cast({originate_execute}, #state{dialstrings=Dialstrings, node=Node, originate_req=JObj
                                        ,uuid=UUID, server_id=ServerId, control_pid=CtrlPid}=State) ->
    case originate_execute(Node, Dialstrings) of
        {ok, _} when is_pid(CtrlPid) ->
            lager:debug("originate completed"),
            {stop, normal, State#state{control_pid=undefined}};
        {ok, CallId} ->
            put(callid, CallId),
            lager:debug("originate is executing, waiting for completion", []),
            erlang:monitor_node(Node, true),
            bind_to_call_events(CallId),
            {noreply, State#state{uuid=CallId}};
        {error, Error} ->
            _ = publish_error(Error, UUID, JObj, ServerId),
            {stop, normal, State}
    end;

handle_cast({bridge_execute_complete, JObj}, #state{server_id=ServerId}=State) ->
    Resp = wh_json:set_values([{<<"Event-Category">>, <<"resource">>}
                               ,{<<"Event-Name">>, <<"originate_resp">>}
                              ], JObj),
    lager:debug("received bridge complete event, sending originate response", []),
    wapi_resource:publish_originate_resp(ServerId, Resp),
    {stop, normal, State};

handle_cast({channel_destroy, JObj}, #state{server_id=ServerId}=State) ->
    Resp = wh_json:set_values([{<<"Event-Category">>, <<"resource">>}
                               ,{<<"Event-Name">>, <<"originate_resp">>}
                              ], JObj),
    lager:debug("received channel destroy event, sending originate response", []),
    wapi_resource:publish_originate_resp(ServerId, Resp),
    {stop, normal, State};
    
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
handle_info({event, [_ | Props]}, #state{uuid=OldUUID}=State) ->
    case props:get_value(<<"Event-Subclass">>, Props, props:get_value(<<"Event-Name">>, Props)) of
        <<"loopback::bowout">> -> 
            case  props:get_value(<<"Resigning-UUID">>, Props) =:= OldUUID of
                false -> {noreply, State};
                true ->
                    NewUUID = props:get_value(<<"Acquired-UUID">>, Props),
                    _ = update_uuid(OldUUID, NewUUID),
                    {noreply, State#state{uuid=NewUUID}}
            end;
        _ -> {noreply, State}
    end;
handle_info({abandon_originate}, #state{tref=undefined}=State) ->
    %% Cancelling a timer does not guarantee that the message has not
    %% already been delivered to the message queue.
    {noreply, State};
handle_info({abandon_originate}, #state{originate_req=JObj, uuid=UUID, server_id=ServerId}=State) ->
    Error = <<"Failed to receive valid originate_execute in time">>,
    _ = publish_error(Error, UUID, JObj, ServerId),
    {stop, normal, State};

handle_info({nodedown, _}, #state{originate_req=JObj, uuid=UUID, server_id=ServerId, node=Node}=State) ->
    erlang:monitor_node(Node, false),
    Error = <<"lost connection to freeswitch node">>,
    _ = publish_error(Error, UUID, JObj, ServerId),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{uuid=UUID}) ->
    {reply, [{uuid, UUID}]}.

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
terminate(_Reason, #state{control_pid=CtrlPid}) when is_pid(CtrlPid) ->
    lager:debug("stop abandoned call controll process ~p", [CtrlPid]),
    ecallmgr_call_control:stop(CtrlPid),
    lager:debug("originate termination: ~p", [_Reason]);
terminate(_Reason, _State) ->
    lager:debug("originate termination: ~p", [_Reason]).

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
-spec get_originate_action/2 :: (ne_binary(), wh_json:object()) -> ne_binary().
get_originate_action(<<"fax">>, JObj) ->
    lager:debug("got originate with action fax"),
    Data = wh_json:get_value(<<"Application-Data">>, JObj),
    <<"&txfax(${http_get(", Data/binary, ")})">>;
get_originate_action(<<"transfer">>, JObj) ->
    lager:debug("got originate with action transfer"),
    case wh_json:get_value([<<"Application-Data">>, <<"Route">>], JObj) of
        undefined -> <<"error">>;
        Route ->
            list_to_binary(["'m:^:", get_unset_vars(JObj), "transfer:", wnm_util:to_e164(Route), " XML context_2' inline"])
    end;
get_originate_action(<<"bridge">>, JObj) ->
    lager:debug("got originate with action bridge"),
    Data = wh_json:get_value(<<"Application-Data">>, JObj),

    case ecallmgr_util:build_channel(Data) of
        {error, _} -> <<"error">>;
        {ok, Channel} ->
            list_to_binary(["'m:^:", get_unset_vars(JObj), "bridge:", Channel, "' inline"])
    end;
get_originate_action(<<"eavesdrop">>, JObj) ->
    lager:debug("got originate with action eavesdrop"),
    case ecallmgr_fs_nodes:channel_node(wh_json:get_value(<<"Eavesdrop-Call-ID">>, JObj)) of
        {error, _} -> <<"error">>;
        {ok, N} ->
            gen_listener:cast(self(), {maybe_update_node, N}),
            get_eavesdrop_action(JObj)
    end;
get_originate_action(_, _) ->
    lager:debug("got originate with action park"),
    ?ORIGINATE_PARK.

get_eavesdrop_action(JObj) ->
    {CallId, Group} = case wh_json:get_value(<<"Eavesdrop-Group-ID">>, JObj) of
                          undefined -> {wh_json:get_value(<<"Eavesdrop-Call-ID">>, JObj), <<>>};
                          ID -> {<<"all">>, <<"eavesdrop_require_group=", ID/binary, ",">>}
                      end,
    case wh_json:get_value(<<"Eavesdrop-Mode">>, JObj) of
        <<"whisper">> -> <<Group/binary, "queue_dtmf:w2@500,eavesdrop:", CallId/binary, " inline">>;
        <<"full">> -> <<Group/binary, "queue_dtmf:w3@500,eavesdrop:", CallId/binary, " inline">>;
        <<"listen">> -> <<Group/binary, "eavesdrop:", CallId/binary, " inline">>;
        undefined -> <<Group/binary, "eavesdrop:", CallId/binary, " inline">>
    end.

-spec build_originate_args/3 :: (ne_binary(), wh_json:objects(), wh_json:object()) -> ne_binary().
build_originate_args(Action, Endpoints, JObj) ->
    lager:debug("building originate command arguments", []),
    DialSeparator = case wh_json:get_value(<<"Dial-Endpoint-Method">>, JObj, <<"single">>) of
                        <<"simultaneous">> when length(Endpoints) > 1 -> <<",">>;
                        _Else -> <<"|">>
                    end,
    DialStrings = ecallmgr_util:build_bridge_string(Endpoints, DialSeparator),
    BillingId = wh_util:rand_hex_binary(16),
    J = wh_json:set_values([{[<<"Custom-Channel-Vars">>, <<"Billing-ID">>], BillingId}
                            ,{<<"Loopback-Bowout">>, <<"true">>}
                           ], JObj),
    list_to_binary([ecallmgr_fs_xml:get_channel_vars(J), DialStrings, " ", Action]).

-spec originate_execute/2 :: (atom(), ne_binary()) -> {'ok', ne_binary()} |
                                                      {'error', ne_binary()}.
originate_execute(Node, Dialstrings) ->
    {ok, BGApiID} = freeswitch:bgapi(Node, 'originate', wh_util:to_list(Dialstrings)),

    receive
        {bgok, BGApiID, <<"+OK ", UUID/binary>>} ->
            {ok, wh_util:strip_binary(binary:replace(UUID, <<"\n">>, <<>>))};
        {bgok, BGApiID, Error} ->
            lager:debug("something other than +OK from FS: ~s", [Error]),
            {error, Error};
        {bgerror, BGApiID, Error} ->
            lager:debug("received an originate error: ~s", [Error]),
            {error, Error}
    after 120000 ->
            lager:debug("originate timed out on ~s", [Node]),
            {error, <<"Originate timed out waiting for the switch to reply">>}
    end.

-spec bind_to_call_events/1 :: (ne_binary()) -> 'ok'.
bind_to_call_events(CallId) ->
    lager:debug("binding to call events for ~s", [CallId]),
    Options = [{callid, CallId}
               ,{restrict_to, [events]}
              ],
    gen_listener:add_binding(self(), call, Options).

-spec unbind_from_call_events/0 :: () -> 'ok'.
unbind_from_call_events() ->
    lager:debug("unbind from call events", []),
    gen_listener:rm_binding(self(), call, []).


-spec update_uuid/2 :: (ne_binary(), ne_binary()) -> 'ok'.
update_uuid(OldUUID, NewUUID) ->
    put(callid, NewUUID),
    lager:debug("updating call id from ~s to ~s", [OldUUID, NewUUID]),
    unbind_from_call_events(),
    bind_to_call_events(NewUUID),
    ok.

-spec create_uuid/1 :: (atom()) -> ne_binary().
-spec create_uuid/2 :: (wh_json:object(), atom()) -> ne_binary().

create_uuid(Node) ->
    case freeswitch:api(Node, 'create_uuid', " ") of
        {ok, UUID} -> 
            put(callid, UUID),
            UUID;
        {error, _E} -> 
            lager:debug("unable to get a uuid from ~s: ~p", [Node, _E]),
            wh_util:rand_hex_binary(18);
        timeout -> 
            lager:debug("unable to get a uuid from ~s: timeout", [Node]),
            wh_util:rand_hex_binary(18)
    end.

create_uuid(JObj, Node) ->
    case wh_json:get_ne_value(<<"Outbound-Call-ID">>, JObj) of
        undefined -> create_uuid(Node);
        CallId -> 
            put(callid, CallId),
            CallId
    end.

-spec get_unset_vars/1 :: (wh_json:object()) -> string().
get_unset_vars(JObj) ->
    %% Refactor (Karl wishes he had unit tests here for you to use)
    ExportProps = [{K, <<>>} || K <- wh_json:get_value(<<"Export-Custom-Channel-Vars">>, JObj, [])],
    Export = [K || KV <- lists:foldr(fun ecallmgr_fs_xml:get_channel_vars/2
                                     ,[]
                                     ,[{<<"Custom-Channel-Vars">>, wh_json:from_list(ExportProps)}]
                                    ),
                   ([K, _] = string:tokens(binary_to_list(KV), "=")) =/= undefined
             ],
    case [[$u,$n,$s,$e,$t,$: | K] || KV <- lists:foldr(fun ecallmgr_fs_xml:get_channel_vars/2, [], wh_json:to_proplist(JObj))
                                         ,not lists:member(begin [K, _] = string:tokens(binary_to_list(KV), "="), K end, Export)] of
        [] -> "";
        Unset -> [string:join(Unset, "^"), "^"]
    end.

-spec publish_error/4 :: (ne_binary(), 'undefined' | ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
publish_error(Error, UUID, Request, ServerId) ->
    lager:debug("originate error: ~s", [Error]),
    E = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request)}
         ,{<<"Call-ID">>, UUID}
         ,{<<"Request">>, Request}
         ,{<<"Error-Message">>, Error}
         | wh_api:default_headers(<<"error">>, <<"originate_resp">>, ?APP_NAME, ?APP_VERSION)
        ],
    wh_api:publish_error(ServerId, props:filter_undefined(E)).

-spec publish_originate_ready/5 :: (ne_binary(), ne_binary(), wh_json:object(), ne_binary(), ne_binary()) -> 'ok'.
publish_originate_ready(CtrlQ, UUID, Request, Q, ServerId) ->
    lager:debug("originate command is ready, waiting for originate_execute", []),
    Props = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, UUID)}
             ,{<<"Call-ID">>, UUID}
             ,{<<"Control-Queue">>, CtrlQ}
             | wh_api:default_headers(Q, <<"dialplan">>, <<"originate_ready">>, ?APP_NAME, ?APP_VERSION)
            ],    
    wapi_dialplan:publish_originate_ready(ServerId, Props).
