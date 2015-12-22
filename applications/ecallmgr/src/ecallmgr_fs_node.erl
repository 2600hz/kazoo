%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz INC
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
-export([handle_reload_gateways/2]).
-export([sync_channels/1
         ,sync_interface/1
         ,sync_capabilities/1
         ,sync_registrations/1
        ]).
-export([sip_url/1]).
-export([sip_external_ip/1]).
-export([fs_node/1]).
-export([hostname/1]).
-export([interface/1]).
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

-define(UPTIME_S, ecallmgr_config:get_integer(<<"fs_node_uptime_s">>, 600)).

-record(interface, {name
                    ,domain_name
                    ,auto_nat
                    ,presence_hosts
                    ,dialplan
                    ,context
                    ,challenge_realm
                    ,rtp_ip
                    ,ext_rtp_ip
                    ,sip_ip
                    ,ext_sip_ip
                    ,url
                    ,bind_url
                    ,hold_music
                    ,outbound_proxy
                    ,codecs_in
                    ,codecs_out
                    ,tel_event
                    ,dtmf_mode
                    ,cng
                    ,session_to
                    ,max_dialog
                    ,no_media
                    ,late_neg
                    ,proxy_media
                    ,zrtp_passthru
                    ,aggressive_nat
                    ,stun_enabled
                    ,stun_auto_disabled
                    ,interface_props = []
                   }).
-type interface() :: #interface{}.

-define(DEFAULT_FS_COMMANDS, [wh_json:from_list([{<<"load">>, <<"mod_sofia">>}])
                              ,wh_json:from_list([{<<"reloadacl">>, <<>>}])
                             ]).

-define(DEFAULT_CAPABILITIES, [wh_json:from_list([{<<"module">>, <<"mod_conference">>}
                                                  ,{<<"is_loaded">>, 'false'}
                                                  ,{<<"capability">>, <<"conference">>}
                                                 ])
                               ,wh_json:from_list([{<<"module">>, <<"mod_channel_move">>}
                                                   ,{<<"is_loaded">>, 'false'}
                                                   ,{<<"capability">>, <<"channel_move">>}
                                                  ])
                               ,wh_json:from_list([{<<"module">>, <<"mod_http_cache">>}
                                                   ,{<<"is_loaded">>, 'false'}
                                                   ,{<<"capability">>, <<"http_cache">>}
                                                  ])
                               ,wh_json:from_list([{<<"module">>, <<"mod_dptools">>}
                                                   ,{<<"is_loaded">>, 'false'}
                                                   ,{<<"capability">>, <<"dialplan">>}
                                                  ])
                               ,wh_json:from_list([{<<"module">>, <<"mod_sofia">>}
                                                   ,{<<"is_loaded">>, 'false'}
                                                   ,{<<"capability">>, <<"sip">>}
                                                  ])
                               ,wh_json:from_list([{<<"module">>, <<"mod_spandsp">>}
                                                   ,{<<"is_loaded">>, 'false'}
                                                   ,{<<"capability">>, <<"fax">>}
                                                  ])
                               ,wh_json:from_list([{<<"module">>, <<"mod_flite">>}
                                                   ,{<<"is_loaded">>, 'false'}
                                                   ,{<<"capability">>, <<"tts">>}
                                                  ])
                               ,wh_json:from_list([{<<"module">>, <<"mod_freetdm">>}
                                                   ,{<<"is_loaded">>, 'false'}
                                                   ,{<<"capability">>, <<"freetdm">>}
                                                  ])
                               ,wh_json:from_list([{<<"module">>, <<"mod_skypopen">>}
                                                   ,{<<"is_loaded">>, 'false'}
                                                   ,{<<"capability">>, <<"skype">>}
                                                  ])
                               ,wh_json:from_list([{<<"module">>, <<"mod_dingaling">>}
                                                   ,{<<"is_loaded">>, 'false'}
                                                   ,{<<"capability">>, <<"xmpp">>}
                                                  ])
                               ,wh_json:from_list([{<<"module">>, <<"mod_skinny">>}
                                                   ,{<<"is_loaded">>, 'false'}
                                                   ,{<<"capability">>, <<"skinny">>}
                                                  ])
                               ,wh_json:from_list([{<<"module">>, <<"mod_sms">>}
                                                   ,{<<"is_loaded">>, 'false'}
                                                   ,{<<"capability">>, <<"sms">>}
                                                  ])
                              ]).

-record(state, {node :: atom()
                ,options = []             :: wh_proplist()
                ,interface = #interface{} :: interface()
                ,start_cmds_pid_ref       :: pid_ref() | 'undefined'
               }).

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
                                    ]
                         }
                        ]).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-define(SERVER, ?MODULE).

-define(YR_TO_MICRO(Y), wh_util:to_integer(Y)*365*24*3600*1000000).
-define(DAY_TO_MICRO(D), wh_util:to_integer(D)*24*3600*1000000).
-define(HR_TO_MICRO(Hr), wh_util:to_integer(Hr)*3600*1000000).
-define(MIN_TO_MICRO(Min), wh_util:to_integer(Min)*60*1000000).
-define(SEC_TO_MICRO(Sec), wh_util:to_integer(Sec)*1000000).
-define(MILLI_TO_MICRO(Mil), wh_util:to_integer(Mil)*1000).

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
         ,{<<"Profile-Name">>, {fun replay_profile/1, <<"url">> } }
         ,{<<"Contact">>, {fun replay_contact/1, <<"url">> } }
         ,{<<"Expires">>, {fun replay_expires/1, <<"expires">>}}
        ]).

-type fs_node() :: atom() | ne_binary() | pid().

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
-spec start_link(atom()) -> startlink_ret().
-spec start_link(atom(), wh_proplist()) -> startlink_ret().

start_link(Node) -> start_link(Node, []).
start_link(Node, Options) ->
    QueueName = <<(wh_util:to_binary(Node))/binary
                  ,"-"
                  ,(wh_util:to_binary(?MODULE))/binary
                >>,
    gen_listener:start_link(?MODULE, [{'responders', ?RESPONDERS}
                                      ,{'bindings', ?BINDINGS(Node)}
                                      ,{'queue_name', QueueName}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [Node, Options]).

-spec sync_channels(fs_node()) -> 'ok'.
sync_channels(Srv) ->
    gen_server:cast(find_srv(Srv), 'sync_channels').

-spec sync_interface(fs_node()) -> 'ok'.
sync_interface(Srv) ->
    gen_server:cast(find_srv(Srv), 'sync_interface').

-spec sync_capabilities(fs_node()) -> 'ok'.
sync_capabilities(Srv) ->
    gen_server:cast(find_srv(Srv), 'sync_capabilities').

-spec sync_registrations(fs_node()) -> 'ok'.
sync_registrations(Srv) ->
    gen_server:cast(find_srv(Srv), 'sync_registrations').

-spec hostname(pid()) -> api_binary().
hostname(Srv) ->
    case fs_node(Srv) of
        'undefined' -> 'undefined';
        Node ->
            [_, Hostname] = binary:split(wh_util:to_binary(Node), <<"@">>),
            Hostname
    end.

-spec sip_url(fs_node()) -> api_binary().
sip_url(Srv) ->
    gen_server:call(find_srv(Srv), 'sip_url').

-spec sip_external_ip(fs_node()) -> api_binary().
sip_external_ip(Srv) ->
    gen_server:call(find_srv(Srv), 'sip_external_ip').

-spec handle_reload_acls(wh_json:object(), wh_proplist()) -> 'ok'.
handle_reload_acls(JObj, Props) ->
    'true' = wapi_switch:reload_acls_v(JObj),

    Node = props:get_value('node', Props),
    case freeswitch:bgapi(Node, 'reloadacl', "") of
        {'ok', Job} -> lager:debug("reloadacl command sent to ~s: JobID: ~s", [Node, Job]);
        {'error', _E} -> lager:debug("reloadacl failed with error: ~p", [_E])
    end.

-spec handle_reload_gateways(wh_json:object(), wh_proplist()) -> 'ok'.
handle_reload_gateways(JObj, Props) ->
    'true' = wapi_switch:reload_gateways_v(JObj),

    Node = props:get_value('node', Props),
    Args = ["profile "
            ,?DEFAULT_FS_PROFILE
            ," rescan"
           ],
    case ecallmgr_config:get_boolean(<<"process_gateways">>, 'false')
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
find_srv(Node) when is_binary(Node) -> find_srv(wh_util:to_atom(Node));
find_srv(Node) when is_atom(Node) ->
    ecallmgr_fs_node_sup:node_srv(ecallmgr_fs_sup:find_node(Node)).

-spec fetch_timeout() -> pos_integer().
-spec fetch_timeout(fs_node()) -> pos_integer().
fetch_timeout() ->
    ecallmgr_config:get(<<"fetch_timeout">>, ?DEFAULT_FETCH_TIMEOUT).
fetch_timeout(_Node) ->
    %% TODO: eventually expose this timeout via mod_kazoo and decrement a bit.
    fetch_timeout().

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
    wh_util:put_callid(Node),
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
handle_call('sip_external_ip', _, #state{interface=Interface}=State) ->
    {'reply', Interface#interface.ext_sip_ip, State};
handle_call('sip_url', _, #state{interface=Interface}=State) ->
    {'reply', Interface#interface.url, State};
handle_call('interface_props', _, #state{interface=Interface}=State) ->
    {'reply', Interface#interface.interface_props, State};
handle_call('node', _, #state{node=Node}=State) ->
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
handle_cast('sync_interface', #state{node=Node
                                     ,interface=Interface
                                    }=State) ->
    {'noreply', State#state{interface=node_interface(Node, Interface)}};
handle_cast('sync_capabilities', #state{node=Node}=State) ->
    _Pid = wh_util:spawn(fun probe_capabilities/1, [Node]),
    lager:debug("syncing capabilities in ~p", [_Pid]),
    {'noreply', State};
handle_cast('sync_channels', #state{node=Node}=State) ->
    Channels = [wh_json:get_value(<<"uuid">>, J)
                || J <- channels_as_json(Node)
               ],
    _ = ecallmgr_fs_channels:sync(Node, Channels),
    {'noreply', State};
handle_cast('sync_registrations', #state{node=Node}=State) ->
    _Pid = wh_util:spawn(fun maybe_replay_registrations/1, [Node]),
    lager:debug("syncing registrations in ~p", [_Pid]),
    {'noreply', State};
handle_cast(_Req, State) ->
    lager:debug("unhandled cast: ~p", [_Req]),
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
handle_info('sync_interface', #state{node=Node
                                     ,interface=Interface
                                    }=State) ->
    {'noreply', State#state{interface=node_interface(Node, Interface)}};
handle_info({'bgok', _Job, _Result}, State) ->
    lager:debug("job ~s finished successfully: ~p", [_Job, _Result]),
    {'noreply', State};
handle_info({'bgerror', _Job, _Result}, State) ->
    lager:debug("job ~s finished with an error: ~p", [_Job, _Result]),
    {'noreply', State};
handle_info({'DOWN', Ref, 'process', Pid, _Reason}, #state{start_cmds_pid_ref={Pid, Ref}}=State) ->
    {'noreply', State#state{start_cmds_pid_ref='undefined'}};
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
-type cmd_result() :: {'ok', {atom(), nonempty_string()}, ne_binary()} |
                      {'error', {atom(), nonempty_string()}, ne_binary()} |
                      {'timeout', {atom(), ne_binary()}}.
-type cmd_results() :: [cmd_result()] |
                       {'error', 'retry'}.

-spec run_start_cmds(atom(), wh_proplist()) -> pid_ref().
run_start_cmds(Node, Options) ->
    Parent = self(),
    spawn_monitor(fun() ->
                          run_start_cmds(Node, Options, Parent)
                  end).

-spec run_start_cmds(atom(), wh_proplist(), pid()) -> any().
run_start_cmds(Node, Options, Parent) ->
    wh_util:put_callid(Node),
    timer:sleep(ecallmgr_config:get_integer(<<"fs_cmds_wait_ms">>, 5 * ?MILLISECONDS_IN_SECOND, Node)),

    run_start_cmds(Node, Options, Parent, is_restarting(Node)).

-spec is_restarting(atom()) -> boolean().
is_restarting(Node) ->
    case freeswitch:api(Node, 'status', <<>>) of
        {'ok', Status} ->
            [UP|_] = binary:split(Status, <<"\n">>),
            is_restarting_status(UP);
        _E ->
            lager:debug("failed to get status of node ~s: ~p", [Node, _E]),
            'false'
    end.

-spec is_restarting_status(ne_binary()) -> boolean().
is_restarting_status(UP) ->
    case re:run(UP, <<"UP (\\d+) years, (\\d+) days, (\\d+) hours, (\\d+) minutes, (\\d+) seconds, (\\d+) milliseconds, (\\d+) microseconds">>, [{'capture', 'all_but_first', 'binary'}]) of
        {'match', [Years, Days, Hours, Minutes, Seconds, _Mille, _Micro]} ->
            Uptime = (wh_util:to_integer(Years) * ?SECONDS_IN_YEAR)
                + (wh_util:to_integer(Days) * ?SECONDS_IN_DAY)
                + (wh_util:to_integer(Hours) * ?SECONDS_IN_HOUR)
                + (wh_util:to_integer(Minutes) * ?SECONDS_IN_MINUTE)
                + wh_util:to_integer(Seconds),
            lager:debug("node has been up for ~b s (considered restarting: ~s)", [Uptime, Uptime < ?UPTIME_S]),
            Uptime < ?UPTIME_S;
        'nomatch' -> 'false'
    end.

-spec run_start_cmds(atom(), wh_proplist(), pid(), boolean() | wh_json:objects()) -> 'ok'.
run_start_cmds(Node, Options, Parent, 'true') ->
    lager:debug("node ~s is considered restarting", [Node]),
    run_start_cmds(Node, Options, Parent
                   ,ecallmgr_config:get(<<"fs_cmds">>, ?DEFAULT_FS_COMMANDS, Node)
                  );
run_start_cmds(Node, Options, Parent, 'false') ->
    lager:debug("node ~s is not considered restarting, trying reconnect cmds first", [Node]),
    Cmds = case ecallmgr_config:get(<<"fs_reconnect_cmds">>) of
               'undefined' -> ecallmgr_config:get(<<"fs_cmds">>, ?DEFAULT_FS_COMMANDS, Node);
               ReconCmds -> ReconCmds
           end,
    run_start_cmds(Node, Options, Parent, Cmds);
run_start_cmds(Node, Options, Parent, Cmds) ->
    Res = process_cmds(Node, Options, Cmds),

    case is_list(Res) andalso lists:filter(fun was_not_successful_cmd/1, Res) of
        [] -> sync(Parent);
        'false' ->
            lager:debug("failed to run start commands, retrying"),
            run_start_cmds(Node, Options, Parent);
        Errs ->
            print_api_responses(Errs),
            sync(Parent)
    end.

-spec sync(pid()) -> any().
sync(Parent) ->
    sync_interface(Parent),
    sync_registrations(Parent),
    sync_capabilities(Parent).

-spec process_cmds(atom(), wh_proplist(), wh_json:object() | ne_binaries()) -> cmd_results().
process_cmds(_Node, _Options, []) ->
    lager:info("no freeswitch commands to run, seems suspect. Is your ecallmgr connected to the same AMQP as the whapps running sysconf?"),
    [];
process_cmds(Node, Options, Cmds) when is_list(Cmds) ->
    lists:foldl(fun(Cmd, Acc) -> process_cmd(Node, Options, Cmd, Acc) end, [], Cmds);
process_cmds(Node, Options, Cmds) ->
    case wh_json:is_json_object(Cmds) of
        'true' -> process_cmd(Node, Options, Cmds, []);
        'false' ->
            lager:debug("recv something other than a list for fs_cmds: ~p", [Cmds]),
            {'error', 'retry'}
    end.

-spec process_cmd(atom(), wh_proplist(), wh_json:object(), cmd_results()) -> cmd_results().
process_cmd(Node, Options, JObj, Acc0) ->
    wh_json:foldl(fun(ApiCmd, ApiArg, Acc) ->
                        lager:debug("process ~s: ~s: ~s", [Node, ApiCmd, ApiArg]),
                        process_cmd(Node, Options, ApiCmd, ApiArg, Acc)
                end, Acc0, JObj).

-spec process_cmd(atom(), wh_proplist(), ne_binary(), wh_json:json_term(), cmd_results()) -> cmd_results().
-spec process_cmd(atom(), wh_proplist(), ne_binary(), wh_json:json_term(), cmd_results(), 'list'|'binary') -> cmd_results().
process_cmd(Node, Options, ApiCmd0, ApiArg, Acc) ->
    process_cmd(Node, Options, ApiCmd0, ApiArg, Acc, 'binary').
process_cmd(Node, Options, ApiCmd0, ApiArg, Acc, ArgFormat) ->
    execute_command(Node, Options, ApiCmd0, ApiArg, Acc, ArgFormat).

-spec execute_command(atom(), wh_proplist(), ne_binary(), wh_json:json_term(), cmd_results(), 'list'|'binary') ->
                             cmd_results().
execute_command(Node, Options, ApiCmd0, ApiArg, Acc, ArgFormat) ->
    ApiCmd = wh_util:to_atom(ApiCmd0, ?FS_CMD_SAFELIST),
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

-spec format_args('list'|'binary', api_terms()) -> api_terms().
format_args('list', Args) -> wh_util:to_list(Args);
format_args('binary', Args) -> wh_util:to_binary(Args).

-spec process_resp(atom(), api_terms(), ne_binaries(), cmd_results()) -> cmd_results().
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

-spec was_bad_error(ne_binary(), atom(), any()) -> boolean().
was_bad_error(<<"[Module already loaded]">>, 'load', _) -> 'false';
was_bad_error(_E, _, _) -> 'true'.

-spec was_not_successful_cmd({'ok', any()} |
                             {'ok', any(), any()} |
                             any()
                            ) -> boolean().

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

-spec channels_as_json(atom()) -> wh_json:objects().
channels_as_json(Node) ->
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
        {'error', _} -> []
    end.

-spec interface_from_props(wh_proplist()) -> interface().
interface_from_props(Props) ->
    #interface{name=props:get_value(<<"Name">>, Props, ?DEFAULT_FS_PROFILE)
               ,domain_name=props:get_value(<<"DomainName">>, Props)
               ,auto_nat=props:get_is_true(<<"Auto-NAT">>, Props)
               ,presence_hosts=props:get_value(<<"PresHosts">>, Props)
               ,dialplan=props:get_value(<<"Dialplan">>, Props)
               ,context=props:get_value(<<"Context">>, Props)
               ,challenge_realm=props:get_value(<<"ChallengeRealm">>, Props)
               ,rtp_ip=props:get_value(<<"RTP-IP">>, Props)
               ,ext_rtp_ip=props:get_value(<<"Ext-RTP-IP">>, Props)
               ,sip_ip=props:get_value(<<"SIP-IP">>, Props)
               ,ext_sip_ip=props:get_value(<<"Ext-SIP-IP">>, Props)
               ,url=props:get_value(<<"URL">>, Props)
               ,bind_url=props:get_value(<<"BIND-URL">>, Props)
               ,hold_music=props:get_value(<<"HOLD_MUSIC">>, Props)
               ,outbound_proxy=props:get_value(<<"OUTBOUND-PROXY">>, Props)
               ,codecs_in=split_codes(<<"CODECSIN">>, Props)
               ,codecs_out=split_codes(<<"CODECSOUT">>, Props)
               ,tel_event=props:get_value(<<"TEL-EVENT">>, Props)
               ,dtmf_mode=props:get_value(<<"DTMF-MODE">>, Props)
               ,cng=props:get_value(<<"CNG">>, Props)
               ,session_to=props:get_value(<<"SESSION-TO">>, Props)
               ,max_dialog=props:get_value(<<"MAX-DIALOG">>, Props)
               ,no_media=props:get_is_true(<<"NOMEDIA">>, Props)
               ,late_neg=props:get_is_true(<<"LATE-NEG">>, Props)
               ,proxy_media=props:get_is_true(<<"PROXY-MEDIA">>, Props)
               ,zrtp_passthru=props:get_is_true(<<"ZRTP-PASSTHRU">>, Props)
               ,aggressive_nat=props:get_is_true(<<"AGGRESSIVENAT">>, Props)
               ,stun_enabled=props:get_is_true(<<"STUN-ENABLED">>, Props)
               ,stun_auto_disabled=props:get_is_true(<<"STUN-AUTO-DISABLE">>, Props)
               ,interface_props=Props
              }.

-spec split_codes(ne_binary(), wh_proplist()) -> ne_binaries().
split_codes(Key, Props) ->
    [Codec
     || Codec <- binary:split(props:get_value(Key, Props, <<>>), <<",">>, ['global'])
            ,not wh_util:is_empty(Codec)
    ].

-spec probe_capabilities(atom()) -> 'ok'.
-spec probe_capabilities(atom(), wh_json:objects()) -> 'ok'.
probe_capabilities(Node) ->
    probe_capabilities(Node, ecallmgr_config:get(<<"capabilities">>, ?DEFAULT_CAPABILITIES)).
probe_capabilities(Node, PossibleCapabilities) ->
    wh_util:put_callid(Node),
    lists:foreach(fun(Capability) ->
                            maybe_add_capability(Node, Capability)
                    end, PossibleCapabilities).

-spec maybe_add_capability(atom(), wh_json:object()) -> any().
maybe_add_capability(Node, Capability) ->
    Module = wh_json:get_value(<<"module">>, Capability),
    lager:debug("probing ~s about ~s", [Node, Module]),
    case freeswitch:api(Node, 'module_exists', wh_util:to_binary(Module)) of
        {'ok', Maybe} ->
            case wh_util:is_true(Maybe) of
                'true' ->
                    lager:debug("adding capability of ~s", [Module]),
                    ecallmgr_fs_nodes:add_capability(Node, wh_json:set_value(<<"is_loaded">>, 'true', Capability));
                'false' ->
                    ecallmgr_fs_nodes:add_capability(Node, wh_json:set_value(<<"is_loaded">>, 'false', Capability))
            end;
        {'error', _E} ->
            lager:debug("failed to probe node ~s: ~p", [Node, _E])
    end.

-spec maybe_replay_registrations(atom()) -> 'ok'.
maybe_replay_registrations(Node) ->
    wh_util:put_callid(Node),
    replay_registration(Node, get_registrations(Node)).

-spec replay_registration(atom(), [wh_proplist()]) -> 'ok'.
replay_registration(_Node, [[]]) -> 'ok';
replay_registration(_Node, []) -> 'ok';
replay_registration(Node, [Reg | Regs]) ->
    Payload = [{<<"FreeSWITCH-Nodename">>, wh_util:to_binary(Node)}
               ,{<<"Event-Timestamp">>, round(wh_util:current_tstamp())}
               | lists:map(fun({K,{F, V}}) when is_function(F,1) ->
                                   {K, F(props:get_value(V, Reg))};
                              ({K,V}) ->
                                   {K, props:get_value(V, Reg)}
                           end, ?REPLAY_REG_MAP)
               ++ wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    lager:debug("replaying registration: ~p",[Payload]),
    wh_amqp_worker:cast(Payload
                        ,fun wapi_registration:publish_success/1
                       ),
    replay_registration(Node, Regs).

-spec replay_profile(ne_binary()) -> ne_binary().
replay_profile(V) ->
    lists:nth(2, binary:split(V, <<"/">>, ['global'] ) ).

-spec replay_expires(ne_binary()) -> pos_integer().
replay_expires(V) ->
    wh_util:unix_seconds_to_gregorian_seconds(wh_util:to_integer(V)) -
        (wh_util:current_tstamp() +
             ecallmgr_config:get_integer(<<"expires_deviation_time">>, 0)
        ).

-spec replay_contact(ne_binary()) -> ne_binary().
replay_contact(V) ->
    <<"<", (lists:nth(3, binary:split(V, <<"/">>, ['global'] ) ))/binary, ">">>.

-spec get_registrations(atom()) -> [wh_proplist()].
get_registrations(Node) ->
    case freeswitch:api(Node, 'show', "registrations") of
        {'ok', Response} ->
            R = binary:replace(Response, <<" ">>, <<>>, ['global']),
            Lines = [binary:split(Line, <<",">>, ['global'])
                     || Line <- binary:split(R, <<"\n">>, ['global'])
                    ],
            get_registration_details(Lines);
        _Else -> [[]]
    end.

-spec get_registration_details(list()) -> [wh_proplist()].
get_registration_details([Header, _, _, _| _] = Lines) ->
    [begin
         {Res, _Total} = lists:mapfoldl(
                           fun(E, Acc) ->
                                   {{lists:nth(Acc, Header),E}, Acc+1}
                           end, 1, Row),
         Res
     end
     || Row <- lists:sublist(Lines, 2, length(Lines)-4)
    ];
get_registration_details(_List) -> [].

-spec node_interface(atom(), interface()) -> interface().
node_interface(Node, CurrInterface) ->
    case ecallmgr_util:get_interface_properties(Node) of
        [] ->
            lager:debug("no interface properties available at the moment, will sync again"),
            _ = erlang:send_after(?MILLISECONDS_IN_SECOND, self(), 'sync_interface'),
            CurrInterface;
        Props ->
            interface_from_props(Props)
    end.

-spec interface(atom() | binary()) -> wh_proplist().
interface(Node) ->
    gen_server:call(find_srv(Node), 'interface_props').
