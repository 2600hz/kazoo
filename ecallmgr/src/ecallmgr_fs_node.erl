%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, VoIP INC
%%% @doc
%%% Manage a FreeSWITCH node and its resources
%%% @end
%%% Created : 11 Nov 2010 by James Aimonetti <james@2600hz.org>
-module(ecallmgr_fs_node).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]).
-export([resource_consume/3, show_channels/1, fs_node/1, uuid_exists/2
         ,uuid_dump/2, hostname/1
        ]).
-export([distributed_presence/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-record(state, {
          node = 'undefined' :: atom()
          ,stats = #node_stats{} :: #node_stats{}
          ,options = [] :: proplist()
         }).

-define(SERVER, ?MODULE).

-define(YR_TO_MICRO(Y), wh_util:to_integer(Y)*365*24*3600*1000000).
-define(DAY_TO_MICRO(D), wh_util:to_integer(D)*24*3600*1000000).
-define(HR_TO_MICRO(Hr), wh_util:to_integer(Hr)*3600*1000000).
-define(MIN_TO_MICRO(Min), wh_util:to_integer(Min)*60*1000000).
-define(SEC_TO_MICRO(Sec), wh_util:to_integer(Sec)*1000000).
-define(MILLI_TO_MICRO(Mil), wh_util:to_integer(Mil)*1000).

-define(FS_TIMEOUT, 5000).

%% keep in sync with wh_api.hrl OPTIONAL_CHANNEL_QUERY_REQ_HEADERS
-define(CALL_STATUS_HEADERS, [<<"Unique-ID">>, <<"Call-Direction">>, <<"Caller-Caller-ID-Name">>, <<"Caller-Caller-ID-Number">>
                                  ,<<"Caller-Network-Addr">>, <<"Caller-Destination-Number">>, <<"FreeSWITCH-Hostname">>
                             ]).
-define(CALL_STATUS_MAPPING, lists:zip(?CALL_STATUS_HEADERS, [<<"Call-ID">> | wapi_channel_query:optional_headers()])).

-spec resource_consume/3 :: (pid(), ne_binary(), json_object()) -> {'resource_consumed', binary(), binary(), integer()} |
                                                                   {'resource_error', binary() | 'error'}.
resource_consume(FsNodePid, Route, JObj) ->
    FsNodePid ! {resource_consume, self(), Route, JObj},
    receive Resp -> Resp
    after   10000 -> {resource_error, timeout}
    end.

-spec distributed_presence/3 :: (pid(), ne_binary(), proplist()) -> 'ok'.
distributed_presence(Srv, Type, Event) ->
    gen_server:cast(Srv, {distributed_presence, Type, Event}).

-spec show_channels/1 :: (pid()) -> [proplist(),...] | [].
show_channels(Srv) ->
    gen_server:call(Srv, show_channels).

-spec hostname/1 :: (pid()) -> fs_api_ret().
hostname(Srv) ->
    gen_server:call(Srv, hostname).

-spec fs_node/1 :: (pid()) -> atom().
fs_node(Srv) ->
    gen_server:call(Srv, fs_node).

-spec uuid_exists/2 :: (pid(), ne_binary()) -> boolean().
uuid_exists(Srv, UUID) ->
    gen_server:call(Srv, {uuid_exists, UUID}).

-spec uuid_dump/2 :: (pid(), ne_binary()) -> {'ok', proplist()} | {'error', ne_binary()} | 'timeout'.
uuid_dump(Srv, UUID) ->
    gen_server:call(Srv, {uuid_dump, UUID}).

-spec start_link/1 :: (atom()) -> {'ok', pid()} | {'error', term()}.
start_link(Node) ->
    gen_server:start_link(?SERVER, [Node, []], []).

-spec start_link/2 :: (atom(), proplist()) -> {'ok', pid()} | {'error', term()}.
start_link(Node, Options) ->
    gen_server:start_link(?SERVER, [Node, Options], []).

init([Node, Options]) ->
    put(callid, wh_util:to_binary(Node)),
    ?LOG_SYS("starting new fs node"),

    process_flag(trap_exit, true),

    Stats = #node_stats{started = erlang:now()},
    erlang:monitor_node(Node, true),
    {ok, #state{node=Node, stats=Stats, options=Options}, 0}.

-spec handle_call/3 :: (term(), {pid(), reference()}, #state{}) -> {'noreply', #state{}} | {'reply', atom(), #state{}}.
handle_call(hostname, _From, #state{node=Node}=State) ->
    [_, Hostname] = binary:split(wh_util:to_binary(Node), <<"@">>),
    {reply, {ok, Hostname}, State};
handle_call({uuid_exists, UUID}, From, #state{node=Node}=State) ->
    spawn(fun() ->
                  case freeswitch:api(Node, uuid_exists, wh_util:to_list(UUID)) of
                      {'ok', Result} -> ?LOG(UUID, "Result of uuid_exists: ~s", [Result]), gen_server:reply(From, wh_util:is_true(Result));
                      _ -> ?LOG(UUID, "Failed to get result from uuid_exists", []), gen_server:reply(From, false)
                  end
          end),
    {noreply, State};
handle_call({uuid_dump, UUID}, From, #state{node=Node}=State) ->
    spawn(fun() ->
                  case freeswitch:api(Node, uuid_dump, wh_util:to_list(UUID)) of
                      {'ok', Result} -> 
                          Props = ecallmgr_util:eventstr_to_proplist(Result),
                          gen_server:reply(From, {ok, Props});
                      Error -> 
                          ?LOG(UUID, "failed to get result from uuid_dump", []),
                          gen_server:reply(From, Error)
                  end
          end),
    {noreply, State};
handle_call(fs_node, _From, #state{node=Node}=State) ->
    {reply, Node, State};
handle_call(show_channels, From, #state{node=Node}=State) ->
    spawn(fun() ->
                  case freeswitch:api(Node, show, "channels") of
                      {ok, Rows} -> gen_server:reply(From, convert_rows(Node, Rows));
                      _ -> gen_server:reply(From, [])
                  end
          end),
    {noreply, State}.

handle_cast({distributed_presence, Presence, Event}, #state{node=Node}=State) ->
    Headers = [{wh_util:to_list(K), wh_util:to_list(V)}
               || {K, V} <- lists:foldr(fun(Header, Props) ->
                                                proplists:delete(Header, Props)
                                        end, Event, ?FS_DEFAULT_HDRS)
              ],
    EventName = wh_util:to_atom(Presence, true), %% Presence = <<"PRESENCE_", Type/binary>>
    _ = freeswitch:sendevent(Node, EventName, Headers),
    {noreply, State};

handle_cast(_Req, State) ->
    {noreply, State}.

%% If we start up while there are active channels, we'll have negative active_channels in our stats.
%% The first clause fixes that situation
handle_info(Msg, #state{stats=#node_stats{created_channels=Cr, destroyed_channels=De}=Stats}=S) when De > Cr ->
    handle_info(Msg, S#state{stats=Stats#node_stats{created_channels=De, destroyed_channels=De}});

handle_info({event, [undefined | Data]}, #state{stats=Stats, node=Node}=State) ->
    case props:get_value(<<"Event-Name">>, Data) of
        <<"PRESENCE_", _/binary>> = EvtName ->
            _ = case props:get_value(<<"Distributed-From">>, Data) of
                    undefined ->
                        Headers = [{<<"Distributed-From">>, wh_util:to_binary(Node)} | Data],
                        [distributed_presence(Srv, EvtName, Headers)
                         || Srv <- ecallmgr_fs_sup:node_handlers(),
                            Srv =/= self()
                        ];
                    _Else ->
                        ok
                end,
            {noreply, State, hibernate};
        <<"HEARTBEAT">> ->
            {noreply, State#state{stats=Stats#node_stats{last_heartbeat=erlang:now()}}, hibernate};
        <<"CUSTOM">> ->
            spawn(fun() -> process_custom_data(Data) end),
            {noreply, State, hibernate};
        _ ->
            {noreply, State, hibernate}
    end;

handle_info({event, [UUID | Data]}, #state{node=Node, stats=#node_stats{created_channels=Cr, destroyed_channels=De}=Stats}=State) ->
    case props:get_value(<<"Event-Name">>, Data) of
        <<"CHANNEL_CREATE">> ->
            ?LOG(UUID, "received channel create event", []),
            spawn(fun() -> ecallmgr_call_control:add_leg(Data) end),
            {noreply, State#state{stats=Stats#node_stats{created_channels=Cr+1}}, hibernate};
        <<"CHANNEL_DESTROY">> ->
            ChanState = props:get_value(<<"Channel-State">>, Data),
            case ChanState of
                <<"CS_NEW">> -> % ignore
                    ?LOG(UUID, "ignoring channel destroy because of CS_NEW", []),
                    {noreply, State, hibernate};
                <<"CS_DESTROY">> ->
                    ?LOG(UUID, "received channel destroyed", []),
                    spawn(fun() -> ecallmgr_call_control:rm_leg(Data) end),
                    {noreply, State#state{stats=Stats#node_stats{destroyed_channels=De+1}}, hibernate}
            end;
        <<"CHANNEL_HANGUP_COMPLETE">> ->
            spawn(fun() -> put(callid, UUID), ecallmgr_call_cdr:new_cdr(UUID, Data) end),
            {noreply, State};
        <<"PRESENCE_", _/binary>> = EvtName ->
            _ = case props:get_value(<<"Distributed-From">>, Data) of
                    undefined ->
                        Headers = [{<<"Distributed-From">>, wh_util:to_binary(Node)}|Data],
                        [distributed_presence(Srv, EvtName, Headers)
                         || Srv <- ecallmgr_fs_sup:node_handlers(),
                            Srv =/= self()
                        ];
                    _Else ->
                        ok
                end,
            {noreply, State, hibernate};
        <<"CUSTOM">> ->
            spawn(fun() -> process_custom_data(Data) end),
            {noreply, State};
        _ ->
            {noreply, State}
    end;

handle_info({resource_request, Pid, <<"audio">>, ChanOptions}
            ,#state{options=Opts, stats=#node_stats{created_channels=Cr, destroyed_channels=De}}=State) ->
    ActiveChan = Cr - De,
    MaxChan = props:get_value(max_channels, Opts),
    AvailChan =  MaxChan - ActiveChan,
    Utilized =  round(ActiveChan / MaxChan * 100),

    MinReq = props:get_value(min_channels_requested, ChanOptions),
    FSHandlerPid = self(),
    spawn(fun() -> channel_request(Pid, FSHandlerPid, AvailChan, Utilized, MinReq) end),
    {noreply, State};
handle_info({resource_consume, Pid, Route, JObj}, #state{node=Node, options=Opts, stats=#node_stats{created_channels=Cr, destroyed_channels=De}}=State) ->
    ActiveChan = Cr - De,
    MaxChan = props:get_value(max_channels, Opts, 1),
    AvailChan =  MaxChan - ActiveChan,

    spawn(fun() -> originate_channel(Node, Pid, Route, AvailChan, JObj) end),
    {noreply, State};

handle_info({update_options, NewOptions}, State) ->
    {noreply, State#state{options=NewOptions}, hibernate};

handle_info({diagnostics, Pid}, #state{stats=Stats}=State) ->
    spawn(fun() -> diagnostics(Pid, Stats) end),
    {noreply, State};

handle_info(timeout, #state{stats=Stats, node=Node}=State) ->
    {foo, Node} ! register_event_handler,
    receive
        ok ->
            ?LOG("event handler registered"),
            Res = run_start_cmds(Node),
            spawn(fun() -> print_api_responses(Res) end),

            NodeData = extract_node_data(Node),
            Active = get_active_channels(Node),

            ok = freeswitch:event(Node, ['CHANNEL_CREATE', 'CHANNEL_DESTROY', 'HEARTBEAT', 'CHANNEL_HANGUP_COMPLETE'
                                         ,'PRESENCE_IN', 'PRESENCE_OUT', 'PRESENCE_PROBE'
                                         ,'CUSTOM', 'sofia::register'
                                        ]),
            ?LOG("bound to switch events"),

            {noreply, State#state{stats=(Stats#node_stats{
                                           created_channels = Active
                                           ,fs_uptime = props:get_value(uptime, NodeData, 0)
                                          })}, hibernate};
        {error, Reason} ->
            ?LOG("error when trying to register event handler: ~p", [Reason]),
            {stop, Reason, State};
        timeout ->
            ?LOG("timeout when trying to register event handler"),
            {stop, timeout, State}
    after ?FS_TIMEOUT ->
            ?LOG("fs timeout when trying to register event handler"),
            {stop, timeout, State}
    end;

handle_info({'EXIT', _Pid, noconnection}, State) ->
    ?LOG("noconnection received for node, pid: ~p", [_Pid]),
    {stop, normal, State};

handle_info({nodedown, Node}, #state{node=Node}=State) ->
    ?LOG("nodedown received"),
    {stop, normal, State};

handle_info(nodedown, State) ->
    ?LOG("nodedown received"),
    {stop, normal, State};

handle_info(_Msg, State) ->
    ?LOG("unhandled message: ~p", [_Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ?LOG_SYS("fs node ~p termination", [_Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec originate_channel/5 :: (Node, Pid, Route, AvailChan, JObj) -> no_return() when
      Node :: atom(),
      Pid :: pid(),
      Route :: binary() | list(),
      AvailChan :: integer(),
      JObj :: json_object().
originate_channel(Node, Pid, Route, AvailChan, JObj) ->
    ChannelVars = ecallmgr_fs_xml:get_channel_vars(JObj),
    Action = get_originate_action(wh_json:get_value(<<"Application-Name">>, JObj), JObj),
    OrigStr = binary_to_list(list_to_binary([ChannelVars, Route, " ", Action])),
    ?LOG_SYS("originate ~s on node ~s", [OrigStr, Node]),
    _ = ecallmgr_util:fs_log(Node, "whistle originating call: ~s", [OrigStr]),
    Result = freeswitch:bgapi(Node, originate, OrigStr),
    case Result of
        {ok, JobId} ->
            receive
                {bgok, JobId, X} ->
                    CallID = erlang:binary_part(X, {4, byte_size(X)-5}),
                    ?LOG_START(CallID, "originate on ~s with job id ~s received bgok ~s", [Node, JobId, X]),
                    CtlQ = start_call_handling(Node, CallID),
                    Pid ! {resource_consumed, CallID, CtlQ, AvailChan-1};
                {bgerror, JobId, Y} ->
                    ErrMsg = erlang:binary_part(Y, {5, byte_size(Y)-6}),
                    ?LOG_SYS("~s failed to originate, ~p", [Node, ErrMsg]),
                    Pid ! {resource_error, ErrMsg}
            after
                9000 ->
                    ?LOG_SYS("~s originate timed out", [Node]),
                    Pid ! {resource_error, timeout}
            end;
        {error, Y} ->
            ErrMsg = erlang:binary_part(Y, {5, byte_size(Y)-6}),
            ?LOG_SYS("~s failed to originate ~p", [Node, ErrMsg]),
            Pid ! {resource_error, ErrMsg};
        timeout ->
            ?LOG_SYS("~s originate timed out", [Node]),
            Pid ! {resource_error, timeout}
    end.

-spec start_call_handling/2 :: (atom(), ne_binary()) -> ne_binary() | {'error', 'amqp_error'}.
start_call_handling(Node, UUID) ->
    try
        {ok, CtlPid} = ecallmgr_call_sup:start_control_process(Node, UUID),
        {ok, _} = ecallmgr_call_sup:start_event_process(Node, UUID),

        ecallmgr_call_control:queue_name(CtlPid)
    catch
        _:_ -> {error, amqp_error}
    end.

-spec diagnostics/2 :: (pid(), tuple()) -> proplist().
diagnostics(Pid, Stats) ->
    Resp = ecallmgr_diagnostics:get_diagnostics(Stats),
    Pid ! Resp.

channel_request(Pid, FSHandlerPid, AvailChan, Utilized, MinReq) ->
    ?LOG("requested for ~p channels with ~p avail", [MinReq, AvailChan]),
    case MinReq > AvailChan of
        true -> Pid ! {resource_response, FSHandlerPid, []};
        false -> Pid ! {resource_response, FSHandlerPid, [{node, FSHandlerPid}
                                                          ,{available_channels, AvailChan}
                                                          ,{percent_utilization, Utilized}
                                                         ]}
    end.

-spec extract_node_data/1 :: (atom()) -> [{'cpu',string()} |
                                        {'sessions_max',integer()} |
                                        {'sessions_per_thirty',integer()} |
                                        {'sessions_since_startup',integer()} |
                                        {'uptime',number()}
                                        ,...].
extract_node_data(Node) ->
    {ok, Status} = freeswitch:api(Node, status),
    Lines = string:tokens(wh_util:to_list(Status), [$\n]),
    process_status(Lines).

-spec process_status/1 :: ([nonempty_string(),...]) -> [{'cpu',string()} |
                                                        {'sessions_max',integer()} |
                                                        {'sessions_per_thirty',integer()} |
                                                        {'sessions_since_startup',integer()} |
                                                        {'uptime',number()}
                                                        ,...].
process_status([Uptime, _, SessSince, Sess30, SessMax, CPU]) ->
    process_status([Uptime, SessSince, Sess30, SessMax, CPU]);
process_status(["UP " ++ Uptime, SessSince, Sess30, SessMax, CPU]) ->
    {match, [[Y],[D],[Hour],[Min],[Sec],[Milli],[Micro]]} = re:run(Uptime, "([\\d]+)", [{capture, [1], list}, global]),
    UpMicro = ?YR_TO_MICRO(Y) + ?DAY_TO_MICRO(D) + ?HR_TO_MICRO(Hour) + ?MIN_TO_MICRO(Min)
        + ?SEC_TO_MICRO(Sec) + ?MILLI_TO_MICRO(Milli) + wh_util:to_integer(Micro),
    {match, SessSinceNum} = re:run(SessSince, "([\\d]+)", [{capture, [1], list}]),
    {match, Sess30Num} = re:run(Sess30, "([\\d]+)", [{capture, [1], list}]),
    {match, SessMaxNum} = re:run(SessMax, "([\\d]+)", [{capture, [1], list}]),
    {match, CPUNum} = re:run(CPU, "([\\d\.]+)", [{capture, [1], list}]),

    [{uptime, UpMicro}
     ,{sessions_since_startup, wh_util:to_integer(lists:flatten(SessSinceNum))}
     ,{sessions_per_thirty, wh_util:to_integer(lists:flatten(Sess30Num))}
     ,{sessions_max, wh_util:to_integer(lists:flatten(SessMaxNum))}
     ,{cpu, lists:flatten(CPUNum)}
    ].

process_custom_data(Data) ->
    put(callid, props:get_value(<<"call-id">>, Data)),
    Subclass = props:get_value(<<"Event-Subclass">>, Data),
    case Subclass of
        <<"sofia::register">> ->
            ?LOG("received registration event"),
            publish_register_event(Data);
        <<"sofia::transfer">> ->
            ?LOG("received transfer event"),
            process_transfer_event(props:get_value(<<"Type">>, Data), Data);
        _ ->
            ok
    end.

publish_register_event(Data) ->
    ApiProp = lists:foldl(fun(K, Api) ->
                                  case props:get_value(wh_util:binary_to_lower(K), Data) of
                                      undefined ->
                                          case props:get_value(K, Data) of
                                              undefined -> Api;
                                              V -> [{K, V} | Api]
                                          end;
                                      V -> [{K, V} | Api]
                                  end
                          end
                          ,[{<<"Event-Timestamp">>, round(wh_util:current_tstamp())}
                            ,{<<"Call-ID">>, get(callid)}
                            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)]
                          ,wapi_registration:success_keys()),
    ?LOG("sending successful registration"),
    wapi_registration:publish_success(ApiProp).

-spec process_transfer_event/2 :: (ne_binary(), proplist()) -> ok.
process_transfer_event(<<"BLIND_TRANSFER">>, Data) ->
    TransfererCtrlUUId = case props:get_value(<<"Transferor-Direction">>, Data) of
                             <<"inbound">> ->
                                 props:get_value(<<"Transferor-UUID">>, Data);
                             _ ->
                                 props:get_value(<<"Transferee-UUID">>, Data)
    end,
    case ecallmgr_call_control_sup:find_worker(TransfererCtrlUUId) of
        {ok, Pid1} -> 
            ?LOG(TransfererCtrlUUId, "sending transferer notice to ecallmgr_call_control ~p", [Pid1]),
            ecallmgr_call_control:transferer(Pid1, Data);
        {error, not_found} -> 
            ok
    end;
process_transfer_event(_, Data) ->
    TransfererCtrlUUId = case props:get_value(<<"Transferor-Direction">>, Data) of
                             <<"inbound">> ->
                                 props:get_value(<<"Transferor-UUID">>, Data);
                             _ ->
                                 props:get_value(<<"Transferee-UUID">>, Data)
    end,
    case ecallmgr_call_control_sup:find_worker(TransfererCtrlUUId) of
        {ok, Pid1} -> 
            ?LOG(TransfererCtrlUUId, "sending transferer notice to ecallmgr_call_control ~p", [Pid1]),
            ecallmgr_call_control:transferer(Pid1, Data);
        {error, not_found} -> 
            ok
    end,
    TransfereeCtrlUUId = props:get_value(<<"Replaces">>, Data),
    case ecallmgr_call_control_sup:find_worker(TransfereeCtrlUUId) of
        {ok, Pid2} -> 
            ?LOG(TransfereeCtrlUUId, "sending transferee notice to ecallmgr_call_control ~p", [Pid2]),
            ecallmgr_call_control:transferee(Pid2, Data);
        {error, not_found} ->
            ok
    end.    

get_originate_action(<<"transfer">>, JObj) ->
    case wh_json:get_value([<<"Application-Data">>, <<"Route">>], JObj) of
        undefined -> <<"error">>;
        Route ->
            list_to_binary(["'m:^:", get_unset_vars(JObj), "transfer:", wh_util:to_e164(Route), " XML context_2' inline"])
    end;
get_originate_action(<<"bridge">>, JObj) ->
    Data = wh_json:get_value(<<"Application-Data">>, JObj),
    case ecallmgr_fs_xml:build_route(Data, wh_json:get_value(<<"Invite-Format">>, Data)) of
        {error, timeout} -> <<"error">>;
        EndPoint ->
            list_to_binary(["'m:^:", get_unset_vars(JObj), "bridge:", EndPoint, "' inline"])
    end;
get_originate_action(_, _) ->
    <<"&park()">>.

get_unset_vars(JObj) ->
    %% Refactor (Karl wishes he had unit tests here for you to use)
    ExportProps = [{K, <<>>} || K <- wh_json:get_value(<<"Export-Custom-Channel-Vars">>, JObj, [])],

    Export = [K || KV <- lists:foldr(fun ecallmgr_fs_xml:get_channel_vars/2, [], [{<<"Custom-Channel-Vars">>, ExportProps}])
                       ,([K, _] = string:tokens(binary_to_list(KV), "=")) =/= undefined],
    case [[$u,$n,$s,$e,$t,$: | K] || KV <- lists:foldr(fun ecallmgr_fs_xml:get_channel_vars/2, [], wh_json:to_proplist(JObj))
                               ,not lists:member(begin [K, _] = string:tokens(binary_to_list(KV), "="), K end, Export)] of
        [] -> "";
        Unset ->
            [string:join(Unset, "^"), "^"]
    end.

-spec run_start_cmds/1 :: (atom()) -> [fs_api_ret(),...].
run_start_cmds(Node) ->
    [freeswitch:api(Node, wh_util:to_atom(ApiCmd, true), wh_util:to_list(ApiArg))
     || {ApiCmd, ApiArg} <- ecallmgr_config:fetch(fs_cmds, []) %% replace with AMQP call to get list of startup commands
    ].

-spec get_active_channels/1 :: (atom()) -> integer().
get_active_channels(Node) ->
    case freeswitch:api(Node, show, "channels") of
        {ok, Chans} ->
            {ok, R} = re:compile("([\\d+])"),
            {match, Match} = re:run(Chans, R, [{capture, [1], list}]),
            wh_util:to_integer(lists:flatten(Match));
        _ ->
            0
    end.

-spec convert_rows/2 :: (atom(), binary()) -> [proplist(),...] | [].
convert_rows(_, <<"\n0 total.\n">>) ->
    ?LOG("No channels up"),
    [];
convert_rows(Node, RowsBin) ->
    [_|Rows] = binary:split(RowsBin, <<"\n">>, [global]),
    return_rows(Node, Rows, []).

-spec return_rows/3 :: (atom(), [binary(),...] | [], [proplist(),...] | []) -> [proplist(),...] | [].
return_rows(Node, [<<>>|Rs], Acc) ->
    return_rows(Node, Rs, Acc);
return_rows(Node, [R|Rs], Acc) ->
    ?LOG("R: ~s", [R]),
    case binary:split(R, <<",">>) of
        [_Total] ->
            ?LOG("Total: ~s", [_Total]),
            return_rows(Node, Rs, Acc);
        [UUID|_] ->
            ?LOG("UUID: ~s", [UUID]),
            {ok, Dump} = freeswitch:api(Node, uuid_dump, wh_util:to_list(UUID)),
            DumpProp = ecallmgr_util:eventstr_to_proplist(Dump),

            %% Pull wanted data from the converted DUMP proplist
            Prop = [{AMQPKey, props:get_value(FSKey, DumpProp)} || {FSKey, AMQPKey} <- ?CALL_STATUS_MAPPING],
            return_rows(Node, Rs, [ Prop | Acc ])
    end;
return_rows(_Node, [], Acc) -> Acc.

print_api_responses(Res) ->
    ?LOG_SYS("Start cmd results:"),
    _ = [ print_api_response(ApiRes) || ApiRes <- Res],
    ?LOG_SYS("End cmd results").

print_api_response({ok, Res}) ->
    [ ?LOG_SYS("~s", [Row]) || Row <- binary:split(Res, <<"\n">>, [global]), Row =/= <<>> ];
print_api_response(Other) ->
    ?LOG_SYS("~p", [Other]).
