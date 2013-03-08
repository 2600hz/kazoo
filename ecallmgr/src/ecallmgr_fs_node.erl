%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz INC
%%% @doc
%%% Manage a FreeSWITCH node and its resources
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_node).

-behaviour(gen_listener).

-export([start_link/1, start_link/2]).
-export([handle_reload_acls/2]).
-export([handle_reload_gtws/2]).
-export([sync_channels/1
         ,sync_conferences/1
        ]).
-export([show_channels/1]).
-export([fs_node/1]).
-export([hostname/1]).
-export([process_event/3]).
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
                ,options = [] :: wh_proplist()
               }).

-define(RESPONDERS, [{{?MODULE, 'handle_reload_acls'}
                      ,[{<<"switch_event">>, <<"reload_acls">>}]
                     }
                     ,{{?MODULE, 'handle_reload_gtws'}
                       ,[{<<"switch_event">>, <<"reload_gateways">>}]
                      }
                    ]).
-define(BINDINGS, [{'switch', []}]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(SERVER, ?MODULE).

-define(YR_TO_MICRO(Y), wh_util:to_integer(Y)*365*24*3600*1000000).
-define(DAY_TO_MICRO(D), wh_util:to_integer(D)*24*3600*1000000).
-define(HR_TO_MICRO(Hr), wh_util:to_integer(Hr)*3600*1000000).
-define(MIN_TO_MICRO(Min), wh_util:to_integer(Min)*60*1000000).
-define(SEC_TO_MICRO(Sec), wh_util:to_integer(Sec)*1000000).
-define(MILLI_TO_MICRO(Mil), wh_util:to_integer(Mil)*1000).

-define(FS_TIMEOUT, 5000).

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
-spec start_link(atom()) -> {'ok', pid()} | {'error', term()}.
-spec start_link(atom(), proplist()) -> {'ok', pid()} | {'error', term()}.

start_link(Node) -> start_link(Node, []).
start_link(Node, Options) ->
    gen_listener:start_link(?MODULE, [{'responders', ?RESPONDERS}
                                      ,{'bindings', ?BINDINGS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [Node, Options]).

-spec sync_channels(pid()) -> 'ok'.
sync_channels(Srv) -> gen_server:cast(Srv, {'sync_channels'}).

-spec sync_conferences(pid()) -> 'ok'.
sync_conferences(Srv) -> gen_server:cast(Srv, {'sync_conferences'}).

-spec hostname(pid()) -> api_binary().
hostname(Srv) ->
    case fs_node(Srv) of
        'undefined' -> 'undefined';
        Node ->
            [_, Hostname] = binary:split(wh_util:to_binary(Node), <<"@">>),
            Hostname
    end.

-spec handle_reload_acls(wh_json:object(), wh_proplist()) -> 'ok'.
handle_reload_acls(_JObj, Props) ->
    Node = props:get_value('node', Props),
    case freeswitch:bgapi(Node, 'reloadacl', "") of
        {'ok', Job} -> lager:debug("reloadacl command sent to ~s: JobID: ~s", [Node, Job]);
        {'error', _E} -> lager:debug("reloadacl failed with error: ~p", [_E]);
        'timeout' -> lager:debug("reloadacl failed with error: timeout")
    end.

-spec handle_reload_gtws(wh_json:object(), wh_proplist()) -> 'ok'.
handle_reload_gtws(_JObj, Props) ->
    Node = props:get_value('node', Props),
    Args = ["profile "
            ,?DEFAULT_FS_PROFILE
            ," rescan"
           ],
    case wh_util:is_true(ecallmgr_config:get(<<"process_gateways">>, 'false'))
        andalso freeswitch:bgapi(Node, 'sofia', lists:flatten(Args))
    of
        'false' -> 'ok';
        {'ok', Job} -> lager:debug("sofia ~s command sent to ~s: JobID: ~s", [Args, Node, Job]);
        {'error', _E} -> lager:debug("sofia ~s failed with error: ~p", [Args, _E]);
        'timeout' -> lager:debug("sofia ~s failed with error: timeout", [Args])
    end.

-spec fs_node(pid()) -> atom().
fs_node(Srv) ->
    case catch(gen_server:call(Srv, 'node', ?FS_TIMEOUT)) of
        {'EXIT', _} -> 'undefined';
        Else -> Else
    end.

-spec show_channels(pid() | atom()) -> wh_json:objects().
show_channels(Srv) when is_pid(Srv) -> show_channels(fs_node(Srv));
show_channels(Node) -> show_channels_as_json(Node).

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
init([Node, Options]) ->
    put('callid', Node),
    process_flag('priority', 'high'), %% Living dangerously!
    lager:info("starting new fs node listener for ~s", [Node]),
    case bind_to_events(props:get_value('client_version', Options), Node) of
        {'error', Reason} ->
            lager:critical("unable to establish node bindings: ~p", [Reason]),
            {'stop', Reason};
        ok ->
            gproc:reg({'p', 'l', 'fs_node'}),
            lager:debug("bound to switch events on node ~s", [Node]),
            sync_channels(self()),
            sync_conferences(self()),
            run_start_cmds(Node),
            {'ok', #state{node=Node, options=Options}}
    end.

bind_to_events(<<"mod_kazoo", _/binary>>, Node) ->
    case freeswitch:event(Node, ?FS_EVENTS) of
        'timeout' -> {'error', 'timeout'};
        Else -> Else
    end;
bind_to_events(_, Node) ->
    case freeswitch:register_event_handler(Node) of
        'timeout' -> {'error', 'timeout'};
        'ok' ->
            lager:debug("event handler registered on node ~s", [Node]),
            case freeswitch:event(Node, ?FS_EVENTS) of
                'timeout' -> {'error', 'timeout'};
                Else -> Else
            end;
        {'error', _}=E -> E
    end.

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
handle_call('node', _From, #state{node=Node}=State) ->
    {'reply', Node, State}.

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
-spec handle_cast(term(), #state{}) -> {'noreply', #state{}}.
handle_cast({'sync_channels'}, #state{node=Node}=State) ->
    Calls = [wh_json:get_value(<<"uuid">>, J)
             || J <- show_channels_as_json(Node)
            ],
    Msg = {'sync_channels', Node, props:filter_undefined(Calls)},
    gen_server:cast('ecallmgr_fs_nodes', Msg),
    {'noreply', State};
handle_cast({'sync_conferences'}, #state{node=Node}=State) ->
    ecallmgr_fs_conference:sync_conferences(Node),
    {'noreply', State};
handle_cast(_Req, State) ->
    {'noreply', State}.

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
handle_info({'event', [UUID | Data]}, #state{node=Node}=State) ->
    _ = spawn(?MODULE, 'process_event', [UUID, Data, Node]),
    {'noreply', State};
handle_info({'bgok', _Job, _Result}, State) ->
    lager:debug("job ~s finished successfully: ~p", [_Job, _Result]),
    {'noreply', State};
handle_info({'bgerror', _Job, _Result}, State) ->
    lager:debug("job ~s finished with an error: ~p", [_Job, _Result]),
    {'noreply', State};
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{node=Node}) ->
    {'reply', [{'node', Node}]}.

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
terminate(_Reason, #state{node=Node}) ->
    lager:info("node listener for ~s terminating: ~p", [Node, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec process_event(api_binary(), wh_proplist(), atom()) -> 'ok'.
process_event(UUID, Props, Node) ->
    EventName = props:get_value(<<"Event-Subclass">>, Props, props:get_value(<<"Event-Name">>, Props)),
    process_event(EventName, UUID, Props, Node).

-spec process_event(ne_binary(), api_binary(), wh_proplist(), atom()) -> any().
process_event(<<"CHANNEL_CREATE">> = EventName, UUID, Props, Node) ->
    _ = ecallmgr_fs_channel:new(Props, Node),
    _ = maybe_start_event_listener(Node, UUID),
    maybe_send_event(EventName, UUID, Props, Node);
process_event(<<"CHANNEL_DESTROY">> = EventName, UUID, Props, Node) ->
    _ = ecallmgr_fs_channel:destroy(Props, Node),
    _ = ecallmgr_fs_conference:participant_destroy(Node, UUID),
    maybe_send_event(EventName, UUID, Props, Node);
process_event(<<"CHANNEL_ANSWER">> = EventName, UUID, Props, Node) ->
    _ = ecallmgr_fs_channel:set_answered(UUID, 'true'),
    maybe_send_event(EventName, UUID, Props, Node);
process_event(<<"CHANNEL_BRIDGE">> = EventName, UUID, Props, Node) ->
    OtherLeg = get_other_leg(UUID, Props),
    _ = ecallmgr_fs_channel:set_bridge(UUID, OtherLeg),
    _ = ecallmgr_fs_channel:set_bridge(OtherLeg, UUID),
    maybe_send_event(EventName, UUID, Props, Node);
process_event(<<"CHANNEL_UNBRIDGE">> = EventName, UUID, Props, Node) ->
    OtherLeg = get_other_leg(UUID, Props),
    _ = ecallmgr_fs_channel:set_bridge(UUID, 'undefined'),
    _ = ecallmgr_fs_channel:set_bridge(OtherLeg, 'undefined'),
    maybe_send_event(EventName, UUID, Props, Node);
process_event(<<"CHANNEL_EXECUTE_COMPLETE">> = EventName, UUID, Props, Node) ->
    Data = props:get_value(<<"Application-Data">>, Props),
    _ = case props:get_value(<<"Application">>, Props) of
            <<"set">> -> process_channel_update(UUID, Data);
            <<"export">> -> process_channel_update(UUID, Data);
            <<"multiset">> -> process_channel_multiset(UUID, Data);
            _Else -> 'ok'
        end,
    maybe_send_event(EventName, UUID, Props, Node);
process_event(<<"conference::maintenance">> = EventName, UUID, Props, Node) ->
    _ = ecallmgr_fs_conference:event(Node, UUID, Props),
    maybe_send_event(EventName, UUID, Props, Node);
process_event(?CHANNEL_MOVE_RELEASED_EVENT_BIN, _, Props, Node) ->
    UUID = props:get_value(<<"old_node_channel_uuid">>, Props),
    gproc:send({'p', 'l', ?CHANNEL_MOVE_REG(Node, UUID)}
               ,?CHANNEL_MOVE_RELEASED_MSG(Node, UUID, Props)
              );
process_event(?CHANNEL_MOVE_COMPLETE_EVENT_BIN, _, Props, Node) ->
    UUID = props:get_value(<<"old_node_channel_uuid">>, Props),
    gproc:send({'p', 'l', ?CHANNEL_MOVE_REG(Node, UUID)}
               ,?CHANNEL_MOVE_COMPLETE_MSG(Node, UUID, Props)
              );
process_event(EventName, UUID, Props, Node) ->
    maybe_send_event(EventName, UUID, Props, Node).

-spec maybe_send_event(ne_binary(), api_binary(), wh_proplist(), atom()) -> any().
maybe_send_event(EventName, UUID, Props, Node) ->
    case wh_util:is_true(props:get_value(<<"variable_channel_is_moving">>, Props)) of
        'true' -> 'ok';
        'false' ->
            gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, {'event', [UUID | Props]}),
            maybe_send_call_event(UUID, Props, Node)
    end.

-spec maybe_send_call_event(api_binary(), wh_proplist(), atom()) -> any().
maybe_send_call_event('undefined', _, _) -> 'ok';
maybe_send_call_event(CallId, Props, Node) ->
    gproc:send({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, CallId)}, {'event', [CallId | Props]}).

-type cmd_result() :: {'ok', {atom(), nonempty_string()}, ne_binary()} |
                      {'error', {atom(), nonempty_string()}, ne_binary()} |
                      {'timeout', {atom(), ne_binary()}}.
-type cmd_results() :: [cmd_result(),...] | [].

-spec run_start_cmds(atom()) -> pid().
run_start_cmds(Node) ->
    spawn_link(fun() ->
                       timer:sleep(5000),
                       Cmds = ecallmgr_config:get(<<"fs_cmds">>, [], Node),
                       Res = process_cmds(Node, Cmds),
                       case lists:filter(fun was_not_successful_cmd/1, Res) of
                           [] -> 'ok';
                           Errs -> print_api_responses(Errs)
                       end
               end).

-spec process_cmds(atom(), wh_json:object() | ne_binaries()) -> cmd_results().
process_cmds(_, []) ->
    lager:info("no freeswitch commands to run, seems suspect. Is your ecallmgr connected to the same AMQP as the whapps running sysconf?"),
    [];
process_cmds(Node, Cmds) when is_list(Cmds) ->
    lists:foldl(fun(Cmd, Acc) -> process_cmd(Node, Cmd, Acc) end, [], Cmds);
process_cmds(Node, Cmds) ->
    case wh_json:is_json_object(Cmds) of
        'true' -> process_cmd(Node, Cmds, []);
        'false' ->
            lager:debug("recv something other than a list for fs_cmds: ~p", [Cmds]),
            timer:sleep(5000),
            run_start_cmds(Node)
    end.

-spec process_cmd(atom(), wh_json:object(), cmd_results()) -> cmd_results().
process_cmd(Node, JObj, Acc0) ->
    lists:foldl(fun({ApiCmd, ApiArg}, Acc) ->
                        process_cmd(Node, ApiCmd, ApiArg, Acc)
                end, Acc0, wh_json:to_proplist(JObj)).

-spec process_cmd(atom(), ne_binary(), ne_binary(), cmd_results()) -> cmd_results().
process_cmd(Node, ApiCmd0, ApiArg, Acc) ->
    process_cmd(Node, ApiCmd0, ApiArg, Acc, 'binary').
process_cmd(Node, ApiCmd0, ApiArg, Acc, ArgFormat) ->
    ApiCmd = wh_util:to_atom(ApiCmd0, ?FS_CMD_SAFELIST),
    case freeswitch:bgapi(Node, ApiCmd, format_args(ArgFormat, ApiArg)) of
        {'error', 'badarg'} when ArgFormat =:= 'binary' ->
            process_cmd(Node, ApiCmd0, ApiArg, Acc, 'list');
        {'ok', BGApiID} ->
            receive
                {'bgok', BGApiID, FSResp} ->
                    process_resp(ApiCmd, ApiArg, binary:split(FSResp, <<"\n">>, ['global']), Acc);
                {'bgerror', BGApiID, _} when ArgFormat =:= 'binary' ->
                    process_cmd(Node, ApiCmd0, ApiArg, Acc, 'list');
                {'bgerror', BGApiID, Error} -> [{'error', Error} | Acc]
            after 120000 ->
                    [{'timeout', {ApiCmd, ApiArg}} | Acc]
            end;
        {'error', _}=Error ->
            [Error | Acc]
    end.

format_args('list', Args) -> wh_util:to_list(Args);
format_args('binary', Args) -> wh_util:to_binary(Args).

process_resp(ApiCmd, ApiArg, [<<>>|Resps], Acc) ->
    process_resp(ApiCmd, ApiArg, Resps, Acc);
process_resp(ApiCmd, ApiArg, [<<"+OK Reloading XML">>|Resps], Acc) ->
    process_resp(ApiCmd, ApiArg, Resps, Acc);
process_resp(ApiCmd, ApiArg, [<<"+OK acl reloaded">>|Resps], Acc) ->
    process_resp(ApiCmd, ApiArg, Resps, Acc);
process_resp(ApiCmd, ApiArg, [<<"+OK ", Resp/binary>>|Resps], Acc) ->
    process_resp(ApiCmd, ApiArg, Resps, [{'ok', {ApiCmd, ApiArg}, Resp} | Acc]);
process_resp(ApiCmd, ApiArg, [<<"+OK">>|Resps], Acc) ->
    process_resp(ApiCmd, ApiArg, Resps, [{'ok', {ApiCmd, ApiArg}, <<"OK">>} | Acc]);
process_resp(ApiCmd, ApiArg, [<<"-ERR ", Err/binary>>|Resps], Acc) ->
    case was_bad_error(Err, ApiCmd, ApiArg) of
        'true' -> process_resp(ApiCmd, ApiArg, Resps, [{'error', {ApiCmd, ApiArg}, Err} | Acc]);
        'false' -> process_resp(ApiCmd, ApiArg, Resps, Acc)
    end;
process_resp(_, _, [], Acc) -> Acc.

was_bad_error(<<"[Module already loaded]">>, 'load', _) -> 'false';
was_bad_error(_E, _, _) -> 'true'.

was_not_successful_cmd({'ok', _}) -> 'false';
was_not_successful_cmd({'ok', _, _}) -> 'false';
was_not_successful_cmd(_) -> 'true'.

-spec print_api_responses(cmd_results()) -> 'ok'.
print_api_responses(Res) ->
    lager:debug("start cmd results:"),
    _ = [ print_api_response(ApiRes) || ApiRes <- lists:flatten(Res)],
    lager:debug("end cmd results").

-spec print_api_response(cmd_result()) -> 'ok'.
print_api_response({'ok', {Cmd, Args}, Res}) ->
    lager:info("ok: ~s(~s) => ~s", [Cmd, Args, Res]);
print_api_response({'error', {Cmd, Args}, Res}) ->
    lager:info("error: ~s(~s) => ~s", [Cmd, Args, Res]);
print_api_response({'timeout', {Cmd, Arg}}) ->
    lager:info("timeout: ~s(~s)", [Cmd, Arg]).

-spec show_channels_as_json(atom()) -> wh_json:objects().
show_channels_as_json(Node) ->
    case freeswitch:api(Node, 'show', "channels as delim |||") of
        {'ok', Lines} ->
            case binary:split(Lines, <<"\n">>, ['global']) of
                [<<>>|_] -> [];
                [Header|Rest] ->
                    Keys = binary:split(Header, <<"|||">>, ['global']),
                    [wh_json:from_list(lists:zip(Keys, Values))
                     || Line <- Rest,
                        ((Values = binary:split(Line, <<"|||">>, ['global'])) =/= [Line])
                    ]
            end;
        {'error', _} -> [];
        'timeout' -> []
    end.

-spec maybe_start_event_listener(atom(), ne_binary()) -> 'ok' | sup_startchild_ret().
maybe_start_event_listener(Node, UUID) ->
    case wh_cache:fetch_local(?ECALLMGR_UTIL_CACHE, {UUID, 'start_listener'}) of
        {'ok', 'true'} -> ecallmgr_call_sup:start_event_process(Node, UUID);
        _E -> 'ok'
    end.

-spec process_channel_multiset(ne_binary(), ne_binary()) -> any().
process_channel_multiset(UUID, Datas) ->
    [process_channel_update(UUID, Data)
     || Data <- binary:split(Datas, <<"|">>, ['global'])
    ].

-spec process_channel_update(ne_binary(), ne_binary()) -> any().
process_channel_update(UUID, Data) ->
    case binary:split(Data, <<"=">>) of
        [Var, Value] -> process_channel_update(UUID, Var, Value);
        _Else -> 'ok'
    end.

-spec process_channel_update(ne_binary(), ne_binary(), ne_binary()) -> any().
process_channel_update(UUID, <<"ecallmgr_", Var/binary>>, Value) ->
    Normalized = wh_util:to_lower_binary(binary:replace(Var, <<"-">>, <<"_">> , ['global'])),
    process_channel_update(UUID, Normalized, Value);
process_channel_update(UUID, <<"hold_music">>, _) ->
    ecallmgr_fs_channel:set_import_moh(UUID, 'false');
process_channel_update(UUID, Var, Value) ->
    try wh_util:to_atom(<<"set_", Var/binary>>) of
        Function ->
            Exports = ecallmgr_fs_channel:module_info('exports'),
            case lists:keysearch(Function, 1, Exports) of
                {'value', {_, 2}} -> ecallmgr_fs_channel:Function(UUID, Value);
                _Else -> 'ok'
            end
    catch
        _:_ -> 'ok'
    end.

get_other_leg(UUID, Props) ->
    get_other_leg(UUID, Props, props:get_value(<<"Other-Leg-Unique-ID">>, Props)).

get_other_leg(UUID, Props, 'undefined') ->
    maybe_other_bridge_leg(UUID
                           ,props:get_value(<<"Bridge-A-Unique-ID">>, Props)
                           ,props:get_value(<<"Bridge-B-Unique-ID">>, Props)
                          );
get_other_leg(_UUID, _Props, OtherLeg) -> OtherLeg.

maybe_other_bridge_leg(UUID, UUID, OtherLeg) -> OtherLeg;
maybe_other_bridge_leg(UUID, OtherLeg, UUID) -> OtherLeg.
