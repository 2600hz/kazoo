%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc Manage a FreeSWITCH node and its resources
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_node).
-behaviour(gen_listener).

-export([start_link/1, start_link/2]).
-export([handle_reload_acls/2]).
-export([handle_reload_gateways/2]).
-export([sync_channels/1
        ,sync_interfaces/1
        ,sync_interface/1, sync_interface/2
        ,sync_capabilities/1
        ]).
-export([sip_url/1, sip_url/2]).
-export([sip_external_ip/1, sip_external_ip/2]).
-export([fs_node/1]).
-export([hostname/1]).
-export([interface/1, interface/2]).
-export([interfaces/1]).
-export([instance_uuid/1]).
-export([fetch_timeout/0, fetch_timeout/1]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(UPTIME_S, kapps_config:get_integer(?APP_NAME, <<"fs_node_uptime_s">>, 600)).

-type interface() :: {kz_term:ne_binary(), kz_term:proplist()}.
-type interfaces() :: [interface()].

-define(DEFAULT_FS_COMMANDS, [kz_json:from_list([{<<"load">>, <<"mod_sofia">>}])
                             ,kz_json:from_list([{<<"reloadacl">>, <<>>}])
                             ]).
-define(FS_CMDS(Node), kapps_config:get_jsons(?APP_NAME, <<"fs_cmds">>, ?DEFAULT_FS_COMMANDS, Node)).

-define(DEFAULT_CAPABILITIES, [kz_json:from_list([{<<"module">>, <<"mod_conference">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"conference">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_channel_move">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"channel_move">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_http_cache">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"http_cache">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_dptools">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"dialplan">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_sofia">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"sip">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_spandsp">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"fax">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_flite">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"tts">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_freetdm">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"freetdm">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_skypopen">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"skype">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_dingaling">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"xmpp">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_skinny">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"skinny">>}
                                                 ])
                              ,kz_json:from_list([{<<"module">>, <<"mod_sms">>}
                                                 ,{<<"is_loaded">>, 'false'}
                                                 ,{<<"capability">>, <<"sms">>}
                                                 ])
                              ]).

-record(state, {node               :: atom()
               ,instance_uuid      :: kz_term:api_ne_binary()
               ,options = []       :: kz_term:proplist()
               ,interfaces = []    :: interfaces()
               ,start_cmds_pid_ref :: kz_term:api_pid_ref()
               }).
-type state() :: #state{}.

-define(RESPONDERS, [{{?MODULE, 'handle_reload_acls'}
                     ,[{<<"switch_event">>, <<"reload_acls">>}]
                     }
                    ,{{?MODULE, 'handle_reload_gateways'}
                     ,[{<<"switch_event">>, <<"reload_gateways">>}]
                     }
                    ,{'ecallmgr_fs_node_command'
                     ,[{<<"switch_event">>, <<"command">>}]
                     }
                    ]).
-define(BINDINGS(Node), [{'switch', [{'node', Node}
                                    ,{'restrict_to', ['reload_acls'
                                                     ,'reload_gateways'
                                                     ,'command'
                                                     ]
                                     }
                                    ,'federate'
                                    ]
                         }
                        ]).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-define(FS_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).

-define(REPLAY_REG_MAP,
        [{<<"Realm">>, <<"realm">>}
        ,{<<"Username">>, <<"reg_user">>}
        ,{<<"Network-IP">>, <<"network_ip">>}
        ,{<<"Network-Port">>, <<"network_port">>}
        ,{<<"FreeSWITCH-Hostname">>, <<"hostname">>}
        ,{<<"To-Host">>, <<"realm">>}
        ,{<<"To-User">>, <<"reg_user">>}
        ,{<<"From-Host">>, <<"realm">>}
        ,{<<"From-User">>, <<"reg_user">>}
        ,{<<"Call-ID">>, <<"token">>}
        ,{<<"Profile-Name">>, {fun replay_profile/1, <<"url">>}}
        ,{<<"Contact">>, {fun replay_contact/1, <<"url">>}}
        ,{<<"Expires">>, {fun replay_expires/1, <<"expires">>}}
        ]).

-type fs_node() :: atom() | kz_term:ne_binary() | pid().

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------

-spec start_link(atom()) -> kz_types:startlink_ret().
start_link(Node) -> start_link(Node, []).

-spec start_link(atom(), kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Node, Options) when is_atom(Node) ->
    QueueName = list_to_binary([kz_term:to_binary(Node)
                               ,"-"
                               ,kz_term:to_binary(?MODULE)
                               ]),

    gen_listener:start_link(?SERVER
                           ,[{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS(Node)}
                            ,{'queue_name', QueueName}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[Node, Options]
                           ).

-spec sync_channels(fs_node()) -> 'ok'.
sync_channels(Srv) ->
    gen_server:cast(find_srv(Srv), 'sync_channels').

-spec sync_interfaces(fs_node()) -> 'ok'.
sync_interfaces(Srv) ->
    gen_server:cast(find_srv(Srv), 'sync_interfaces').

-spec sync_interface(fs_node()) -> 'ok'.
sync_interface(Srv) ->
    sync_interface(Srv, <<?DEFAULT_FS_PROFILE>>).

-spec sync_interface(fs_node(), kz_term:ne_binary()) -> 'ok'.
sync_interface(Srv, Profile) ->
    gen_server:cast(find_srv(Srv), {'sync_interface', Profile}).

-spec sync_capabilities(fs_node()) -> 'ok'.
sync_capabilities(Srv) ->
    gen_server:cast(find_srv(Srv), 'sync_capabilities').

-spec hostname(fs_node()) -> kz_term:api_binary().
hostname(Srv) ->
    case fs_node(Srv) of
        'undefined' -> 'undefined';
        Node ->
            [_, Hostname] = binary:split(kz_term:to_binary(Node), <<"@">>),
            Hostname
    end.

-spec sip_url(fs_node()) -> kz_term:api_binary().
sip_url(Srv) ->
    sip_url(Srv, <<?DEFAULT_FS_PROFILE>>).

-spec sip_url(fs_node(), kz_term:ne_binary()) -> kz_term:api_binary().
sip_url(Srv, Profile) ->
    gen_server:call(find_srv(Srv), {'sip_url', Profile}).

-spec sip_external_ip(fs_node()) -> kz_term:api_binary().
sip_external_ip(Srv) ->
    sip_external_ip(Srv, <<?DEFAULT_FS_PROFILE>>).

-spec sip_external_ip(fs_node(), kz_term:ne_binary()) -> kz_term:api_binary().
sip_external_ip(Srv, Profile) ->
    gen_server:call(find_srv(Srv), {'sip_external_ip', Profile}).

-spec handle_reload_acls(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_reload_acls(JObj, Props) ->
    'true' = kapi_switch:reload_acls_v(JObj),

    Node = props:get_value('node', Props),
    case freeswitch:bgapi(Node, 'reloadacl', "") of
        {'ok', Job} -> lager:debug("reloadacl command sent to ~s: JobID: ~s", [Node, Job]);
        {'error', _E} -> lager:debug("reloadacl failed with error: ~p", [_E])
    end.

-spec handle_reload_gateways(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_reload_gateways(JObj, Props) ->
    'true' = kapi_switch:reload_gateways_v(JObj),

    Node = props:get_value('node', Props),
    Args = ["profile "
           ,?DEFAULT_FS_PROFILE
           ," rescan"
           ],
    case kapps_config:get_boolean(?APP_NAME, <<"process_gateways">>, 'false')
        andalso freeswitch:bgapi(Node, 'sofia', lists:flatten(Args))
    of
        'false' -> 'ok';
        {'ok', Job} -> lager:debug("sofia ~s command sent to ~s: JobID: ~s", [Args, Node, Job]);
        {'error', _E} -> lager:debug("sofia ~s failed with error: ~p", [Args, _E])
    end.

-spec fs_node(fs_node()) -> atom().
fs_node(Srv) ->
    case catch(gen_server:call(find_srv(Srv), 'node', ?FS_TIMEOUT)) of
        {'EXIT', _} -> 'undefined';
        Else -> Else
    end.

-spec find_srv(fs_node()) -> pid().
find_srv(Pid) when is_pid(Pid) -> Pid;
find_srv(Node) when is_binary(Node) -> find_srv(kz_term:to_atom(Node));
find_srv(Node) when is_atom(Node) ->
    ecallmgr_fs_node_sup:node_srv(ecallmgr_fs_sup:find_node(Node)).

-spec fetch_timeout() -> pos_integer().
fetch_timeout() ->
    kapps_config:get_integer(?APP_NAME, <<"fetch_timeout">>, ?DEFAULT_FETCH_TIMEOUT).

-spec fetch_timeout(fs_node()) -> pos_integer().
fetch_timeout(_Node) ->
    %% TODO: eventually expose this timeout via mod_kazoo and decrement a bit.
    fetch_timeout().

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | kz_term:proplist()]) -> {'ok', state()}.
init([Node, Options]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(Node),
    process_flag('priority', 'high'), %% Living dangerously!
    lager:info("starting new fs node listener for ~s", [Node]),
    gproc:reg({'p', 'l', 'fs_node'}),
    sync_channels(self()),
    PidRef = run_start_cmds(Node, Options),
    lager:debug("running start commands in ~p", [PidRef]),
    {'ok', #state{node=Node
                 ,options=Options
                 ,start_cmds_pid_ref=PidRef
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'sip_external_ip', Profile}, _, #state{interfaces=Interfaces}=State) ->
    ExternalIP = case props:get_value(Profile, Interfaces) of
                     'undefined' -> 'undefined';
                     Interface -> props:get_value(<<"Ext-SIP-IP">>, Interface)
                 end,
    {'reply', ExternalIP, State};
handle_call({'sip_url', Profile}, _, #state{interfaces=Interfaces}=State) ->
    SIPUrl = case props:get_value(Profile, Interfaces) of
                 'undefined' -> 'undefined';
                 Interface -> props:get_value(<<"URL">>, Interface)
             end,
    {'reply', SIPUrl, State};
handle_call('interfaces', _, #state{interfaces=[]}=State) ->
    {'reply', 'undefined', State};
handle_call('interfaces', _, #state{interfaces=Interfaces}=State) ->
    Resp = kz_json:from_list_recursive(Interfaces),
    {'reply', Resp, State};
handle_call('instance_uuid', _, #state{instance_uuid='undefined', node=Node}=State) ->
    {'ok', UUID} = freeswitch:api(Node, 'eval', "${Core-UUID}"),
    {'reply', UUID, State#state{instance_uuid=UUID}};
handle_call('instance_uuid', _, #state{instance_uuid=UUID}=State) ->
    {'reply', UUID, State};
handle_call('node', _, #state{node=Node}=State) ->
    {'reply', Node, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('sync_interfaces', #state{node=Node
                                     ,interfaces=Interfaces
                                     }=State) ->
    {'ok', UUID} = freeswitch:api(Node, 'eval', "${Core-UUID}"),
    {'noreply', State#state{instance_uuid=UUID, interfaces=node_interfaces(Node, Interfaces)}};
handle_cast('sync_capabilities', #state{node=Node}=State) ->
    _Pid = kz_util:spawn(fun probe_capabilities/1, [Node]),
    lager:debug("syncing capabilities in ~p", [_Pid]),
    {'noreply', State};
handle_cast('sync_channels', #state{node=Node}=State) ->
    Channels = [kz_json:get_value(<<"uuid">>, J)
                || J <- channels_as_json(Node)
               ],
    _ = ecallmgr_fs_channels:sync(Node, Channels),
    {'noreply', State};
handle_cast(_Req, State) ->
    lager:debug("unhandled cast: ~p", [_Req]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info('sync_interfaces', #state{node=Node
                                     ,interfaces=Interfaces
                                     }=State) ->
    {'noreply', State#state{interfaces=node_interfaces(Node, Interfaces)}};
handle_info({'bgok', _Job, _Result}, State) ->
    lager:debug("job ~s finished successfully: ~p", [_Job, _Result]),
    {'noreply', State};
handle_info({'bgerror', _Job, _Result}, State) ->
    lager:debug("job ~s finished with an error: ~p", [_Job, _Result]),
    {'noreply', State};
handle_info({'DOWN', Ref, 'process', Pid, _Reason}, #state{start_cmds_pid_ref={Pid, Ref}}=State) ->
    {'noreply', State#state{start_cmds_pid_ref='undefined'}};
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {stop, {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{node=Node
                          ,options=Options
                          }) ->
    {'reply', [{'node', Node}
              ,{'node_options', Options}
              ]
    }.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node=Node}) ->
    lager:info("node listener for ~s terminating: ~p", [Node, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-type cmd_result() :: {'ok', {atom(), nonempty_string()}, kz_term:ne_binary()} |
                      {'error', {atom(), nonempty_string()}, kz_term:ne_binary()} |
                      {'timeout', {atom(), kz_term:ne_binary()}}.
-type cmd_results() :: [cmd_result()] |
                       {'error', 'retry'}.

-spec run_start_cmds(atom(), kz_term:proplist()) -> kz_term:pid_ref().
run_start_cmds(Node, Options) when is_atom(Node) ->
    kz_util:spawn_monitor(fun run_start_cmds/3, [Node, Options, self()]).

-spec run_start_cmds(atom(), kz_term:proplist(), pid()) -> any().
run_start_cmds(Node, Options, Parent) when is_atom(Node) ->
    kz_util:put_callid(Node),
    timer:sleep(kapps_config:get_integer(?APP_NAME, <<"fs_cmds_wait_ms">>, 5 * ?MILLISECONDS_IN_SECOND, Node)),

    run_start_cmds(Node, Options, Parent, is_restarting(Node)).

-spec is_restarting(atom()) -> boolean().
is_restarting(Node) when is_atom(Node) ->
    case freeswitch:api(Node, 'status', <<>>) of
        {'ok', Status} ->
            [UP|_] = binary:split(Status, <<"\n">>),
            is_restarting_status(UP);
        _E ->
            lager:debug("failed to get status of node ~s: ~p", [Node, _E]),
            'false'
    end.

-spec is_restarting_status(kz_term:ne_binary()) -> boolean().
is_restarting_status(UP) ->
    case re:run(UP, <<"UP (\\d+) years, (\\d+) days, (\\d+) hours, (\\d+) minutes, (\\d+) seconds, (\\d+) milliseconds, (\\d+) microseconds">>, [{'capture', 'all_but_first', 'binary'}]) of
        {'match', [Years, Days, Hours, Minutes, Seconds, _Mille, _Micro]} ->
            Uptime = (kz_term:to_integer(Years) * ?SECONDS_IN_YEAR)
                + (kz_term:to_integer(Days) * ?SECONDS_IN_DAY)
                + (kz_term:to_integer(Hours) * ?SECONDS_IN_HOUR)
                + (kz_term:to_integer(Minutes) * ?SECONDS_IN_MINUTE)
                + kz_term:to_integer(Seconds),
            lager:debug("node has been up for ~b s (considered restarting: ~s)", [Uptime, Uptime < ?UPTIME_S]),
            Uptime < ?UPTIME_S;
        'nomatch' -> 'false'
    end.

-spec run_start_cmds(atom(), kz_term:proplist(), pid(), boolean() | kz_json:objects()) -> 'ok'.
run_start_cmds(Node, Options, Parent, 'true') when is_atom(Node) ->
    lager:debug("node ~s is considered restarting", [Node]),
    run_start_cmds(Node, Options, Parent, ?FS_CMDS(Node));
run_start_cmds(Node, Options, Parent, 'false') when is_atom(Node) ->
    lager:debug("node ~s is not considered restarting, trying reconnect cmds first", [Node]),
    Cmds = case kapps_config:get_jsons(?APP_NAME, <<"fs_reconnect_cmds">>, 'undefined') of
               'undefined' -> ?FS_CMDS(Node);
               ReconCmds -> ReconCmds
           end,
    run_start_cmds(Node, Options, Parent, Cmds);
run_start_cmds(Node, Options, Parent, Cmds) when is_atom(Node) ->
    Res = process_cmds(Node, Options, Cmds),

    case is_list(Res)
        andalso [R || R <- Res, was_not_successful_cmd(R)]
    of
        [] ->
            lager:debug("ask freeswitch ~s to get latest config", [Node]),
            freeswitch:config(Node),
            sync(Parent);
        'false' ->
            lager:debug("failed to run start commands, retrying"),
            run_start_cmds(Node, Options, Parent);
        Errs ->
            print_api_responses(Errs),
            sync(Parent)
    end.

-spec sync(pid()) -> any().
sync(Parent) ->
    sync_interfaces(Parent),
    sync_capabilities(Parent).

-spec process_cmds(atom(), kz_term:proplist(), kz_json:objects()) -> cmd_results().
process_cmds(_Node, _Options, []) ->
    lager:info("no freeswitch commands to run, seems suspect. Is your ecallmgr connected to the same AMQP as the kapps running sysconf?"),
    [];
process_cmds(Node, Options, Cmds) when is_list(Cmds) ->
    lists:foldl(fun(Cmd, Acc) -> process_cmd(Node, Options, Cmd, Acc) end, [], Cmds).

-spec process_cmd(atom(), kz_term:proplist(), kz_json:object(), cmd_results()) -> cmd_results().
process_cmd(Node, Options, JObj, Acc0) ->
    kz_json:foldl(fun(ApiCmd, ApiArg, Acc) ->
                          lager:debug("process ~s: ~s: ~s", [Node, ApiCmd, ApiArg]),
                          process_cmd(Node, Options, ApiCmd, ApiArg, Acc)
                  end
                 ,Acc0
                 ,JObj
                 ).

-spec process_cmd(atom(), kz_term:proplist(), kz_term:ne_binary(), kz_json:json_term(), cmd_results()) -> cmd_results().
process_cmd(Node, Options, ApiCmd0, ApiArg, Acc) ->
    process_cmd(Node, Options, ApiCmd0, ApiArg, Acc, 'binary').

-spec process_cmd(atom(), kz_term:proplist(), kz_term:ne_binary(), kz_json:json_term(), cmd_results(), 'list'|'binary') -> cmd_results().
process_cmd(Node, Options, ApiCmd0, ApiArg, Acc, ArgFormat) ->
    execute_command(Node, Options, ApiCmd0, ApiArg, Acc, ArgFormat).

-spec execute_command(atom(), kz_term:proplist(), kz_term:ne_binary(), kz_json:json_term(), cmd_results(), 'list'|'binary') ->
                             cmd_results().
execute_command(Node, Options, ApiCmd0, ApiArg, Acc, ArgFormat) ->
    ApiCmd = kz_term:to_atom(ApiCmd0, ?FS_CMD_SAFELIST),
    lager:debug("exec ~s on ~s", [ApiCmd, Node]),
    case freeswitch:bgapi(Node, ApiCmd, format_args(ArgFormat, ApiArg)) of
        {'ok', BGApiID} ->
            receive
                {'bgok', BGApiID, FSResp} ->
                    process_resp(ApiCmd, ApiArg, binary:split(FSResp, <<"\n">>, ['global']), Acc);
                {'bgerror', BGApiID, _} when ArgFormat =:= 'binary' ->
                    process_cmd(Node, Options, ApiCmd0, ApiArg, Acc, 'list');
                {'bgerror', BGApiID, Error} ->
                    process_resp(ApiCmd, ApiArg, binary:split(Error, <<"\n">>, ['global']), Acc)
            after 120 * ?MILLISECONDS_IN_SECOND ->
                    [{'timeout', {ApiCmd, ApiArg}} | Acc]
            end;
        {'error', _}=Error ->
            [Error | Acc]
    end.

-spec format_args('list'|'binary', kz_term:api_terms()) -> kz_term:api_terms().
format_args('list', Args) -> kz_term:to_list(Args);
format_args('binary', Args) -> kz_term:to_binary(Args).

-spec process_resp(atom(), kz_term:api_terms(), kz_term:ne_binaries(), cmd_results()) -> cmd_results().
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

-spec was_bad_error(kz_term:ne_binary(), atom(), any()) -> boolean().
was_bad_error(<<"[Module already loaded]">>, 'load', _) -> 'false';
was_bad_error(_E, _, _) -> 'true'.

-spec was_not_successful_cmd(cmd_result()) -> boolean().
was_not_successful_cmd({'ok', _, _}) -> 'false';
was_not_successful_cmd(_) -> 'true'.

-spec print_api_responses(cmd_results()) -> 'ok'.
print_api_responses(Res) ->
    lager:debug("start cmd results:"),
    lists:foreach(fun print_api_response/1, lists:flatten(Res)),
    lager:debug("end cmd results").

-spec print_api_response(cmd_result()) -> 'ok'.
print_api_response({'ok', {Cmd, Args}, Res}) ->
    lager:info("ok: ~s(~s) => ~s", [Cmd, Args, Res]);
print_api_response({'error', {Cmd, Args}, Res}) ->
    lager:info("error: ~s(~s) => ~s", [Cmd, Args, Res]);
print_api_response({'timeout', {Cmd, Arg}}) ->
    lager:info("timeout: ~s(~s)", [Cmd, Arg]).

-spec channels_as_json(atom()) -> kz_json:objects().
channels_as_json(Node) ->
    case freeswitch:api(Node, 'show', "channels as delim |||") of
        {'ok', Lines} ->
            case binary:split(Lines, <<"\n">>, ['global']) of
                [<<>>|_] -> [];
                [Header|Rest] ->
                    Keys = binary:split(Header, <<"|||">>, ['global']),
                    [kz_json:from_list(lists:zip(Keys, Values))
                     || Line <- Rest,
                        (Values = binary:split(Line, <<"|||">>, ['global'])) =/= [Line]
                    ]
            end;
        {'error', _} -> []
    end.

-spec probe_capabilities(atom()) -> 'ok'.
probe_capabilities(Node) ->
    probe_capabilities(Node, kapps_config:get_jsons(?APP_NAME, <<"capabilities">>, ?DEFAULT_CAPABILITIES)).

-spec probe_capabilities(atom(), kz_json:objects()) -> 'ok'.
probe_capabilities(Node, PossibleCapabilities) ->
    kz_util:put_callid(Node),
    F = fun(Capability) -> maybe_add_capability(Node, Capability) end,
    lists:foreach(F, PossibleCapabilities),
    lager:notice("capabilities sync complete").

-spec maybe_add_capability(atom(), kz_json:object()) -> any().
maybe_add_capability(Node, Capability) ->
    Module = kz_json:get_value(<<"module">>, Capability),
    lager:debug("probing ~s about ~s", [Node, Module]),
    case freeswitch:api(Node, 'module_exists', kz_term:to_binary(Module)) of
        {'ok', Maybe} ->
            case kz_term:is_true(Maybe) of
                'true' ->
                    lager:debug("adding capability of ~s", [Module]),
                    ecallmgr_fs_nodes:add_capability(Node, kz_json:set_value(<<"is_loaded">>, 'true', Capability));
                'false' ->
                    ecallmgr_fs_nodes:add_capability(Node, kz_json:set_value(<<"is_loaded">>, 'false', Capability))
            end;
        {'error', _E} ->
            lager:debug("failed to probe node ~s: ~p", [Node, _E])
    end.

-spec node_interfaces(atom(), interfaces()) -> interfaces().
node_interfaces(Node, CurrInterfaces) ->
    case ecallmgr_util:get_interface_properties(Node) of
        [] ->
            lager:debug("no interface properties available at the moment, will sync again"),
            _ = erlang:send_after(?MILLISECONDS_IN_SECOND, self(), 'sync_interfaces'),
            CurrInterfaces;
        Interfaces -> Interfaces
    end.

-spec interfaces(atom() | binary()) -> kz_term:api_object().
interfaces(Node) ->
    gen_server:call(find_srv(Node), 'interfaces').

-spec interface(atom() | binary()) -> kz_term:api_object().
interface(Node) ->
    interface(Node, <<?DEFAULT_FS_PROFILE>>).

-spec interface(atom() | binary(), kz_term:ne_binary()) -> kz_term:api_object().
interface(Node, Profile) ->
    gen_server:call(find_srv(Node), {'interface', Profile}).

-spec instance_uuid(atom() | binary()) -> kz_term:api_ne_binary().
instance_uuid(Node) ->
    gen_server:call(find_srv(Node), 'instance_uuid').
