%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% Create freeswitch offline configuration
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------

-module(crossbar_freeswitch).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).


-export([build_freeswitch/1]).

-export([reset/0]).

-include("crossbar.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").

-define(CALLFLOW_VIEW, <<"callflow/listing_by_number">>).
-define(DEVICES_VIEW, <<"devices/listing_by_owner">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".freeswitch">>).

-define(FS_DIALPLAN, 'freeswitch_dialplan').
-define(FS_CHATPLAN, 'freeswitch_chatplan').
-define(FS_DIRECTORY, 'freeswitch_directory').

-define(FS_DIRECTORY_REALM, 'freeswitch_directory_realm').
-define(FS_DIALPLAN_REALM, 'freeswitch_dialplan_realm').
-define(FS_CHATPLAN_REALM, 'freeswitch_chatplan_realm').

-define(DEFAULT_FS_TEMPLATES, [?FS_DIRECTORY]).
-define(DEFAULT_FS_INCLUDE_DIRECTORY_FILES, [?FS_DIALPLAN, ?FS_CHATPLAN]).

-define(FS_TEMPLATES, [?FS_DIRECTORY, ?FS_DIALPLAN, ?FS_CHATPLAN]).
-define(FS_REALM_TEMPLATES, [?FS_DIRECTORY_REALM]).
-define(FS_ALL_TEMPLATES, [?FS_DIRECTORY, ?FS_DIALPLAN, ?FS_CHATPLAN, ?FS_DIRECTORY_REALM]).

-define(AUTHN_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).

-record(state, {config = 'undefined' :: api_binary(),
                is_running = 'false' :: boolean(),
                monitor :: reference()
               }).

%% this shouldn't be here. we need to move this definition from
%% ecallmgr.hrl into the database or into whistle/include
-define(CHANNEL_VAR_PREFIX, "ecallmgr_").

%%%===================================================================
%%% API
%%%===================================================================
-spec reset() -> 'ok'.
reset() ->
    gen_server:cast(crossbar_sup:find_proc(?MODULE), 'reset').

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
init([]) ->
    process_flag(trap_exit, 'true'),
    _ = compile_templates(),
    _  = gen_server:cast(self(), 'periodic_build'),
    {'ok', #state{}}.

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
handle_call('current', _From, #state{config='undefined'}=State) ->
    {'reply', {'error', 'no_file'}, State};
handle_call('current', _From, #state{config=Config}=State) ->
    {'reply', {'ok', Config}, State};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

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
handle_cast('periodic_build', #state{is_running='true'}=State) ->
    {'noreply', State};
handle_cast('periodic_build', #state{is_running='false'}=State) ->
    {Pid, Monitor} = spawn_monitor(?MODULE, 'build_freeswitch', [self()]),
    lager:debug("started new freeswitch offline configuration builder ~p"
                ,[Pid]),
    {'noreply', State#state{is_running='true', monitor=Monitor}};
handle_cast({'completed', File}, #state{config=Config}=State) ->
    lager:debug("created new freeswitch offline configuration ~s"
                ,[File]),
    gen_server:cast(self(), {'delete', Config}),
    {'noreply', State#state{is_running='false', config=File}};
handle_cast({'delete', 'undefined'}, State) ->
    {'noreply', State};
handle_cast({'delete', File}, State) ->
    lager:debug("removing prior freeswitch offline configuration ~s"
                ,[File]),
    wh_util:delete_file(File),
    {'noreply', State};
handle_cast('reset', #state{config=Config}=State) ->
    lager:debug("resetting freeswitch state"),
    gen_server:cast(self(), {'delete', Config}),
    {'noreply', State#state{is_running='false'}};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
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
handle_info({'DOWN', MonitorRef, 'process', _, 'normal'}, #state{monitor=MonitorRef}=State) ->
    {'noreply', State#state{is_running='false'}};
handle_info({'DOWN', MonitorRef, _, _Pid, _Reason}, #state{monitor=MonitorRef}=State) ->
    lager:debug("freeswitch offline configuration builder ~p died unexpectedly: ~p"
                ,[_Pid, _Reason]),
    {'noreply', State#state{is_running='false'}};
handle_info(_Info, State) ->
    {'noreply', State}.

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
terminate(_Reason, _State) ->
    lager:debug("crossbar freeswitch terminating: ~p", [_Reason]).

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
-spec zip_directory(ne_binary()) -> string().
zip_directory(WorkDir0) ->
    WorkDir = wh_util:to_list(WorkDir0),
    ZipName = lists:concat([WorkDir, ".zip"]),
    Files = [wh_util:to_list(F) || F <- filelib:wildcard("*", WorkDir)],
    {'ok', _} = zip:zip(ZipName , Files, [{'cwd', WorkDir}]),
    ZipName.

-spec setup_directory() -> ne_binary().
setup_directory() ->
    TopDir = wh_util:rand_hex_binary(8),
    WorkRootDir = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"work_dir">>, <<"/tmp/">>),
    WorkDir = filename:join([WorkRootDir, TopDir]),
    wh_util:make_dir(WorkDir),
    Files = [{<<"directory">>, ?FS_DIRECTORY}
             ,{<<"chatplan">>, ?FS_CHATPLAN}
             ,{<<"dialplan">>, ?FS_DIALPLAN}
            ],
    Filter = whapps_config:get(?MOD_CONFIG_CAT, <<"files_to_include">>, ?DEFAULT_FS_INCLUDE_DIRECTORY_FILES),
    _ = [wh_util:make_dir(filename:join([WorkDir, Dir])) || {Dir, _} <- Files],
    _ = [wh_util:write_file(filename:join([WorkDir, D, xml_file_name(T)])
                            ,xml_file_from_config(T)
                           )
         || {D, T} <- Files, lists:member(wh_util:to_binary(T), Filter)
        ],
    put(<<"WorkDir">>,WorkDir),
    put(<<"Realms">>,[]),
    WorkDir.

-spec process_realms() -> 'ok'.
process_realms() ->
    Realms = get(<<"Realms">>),
    Templates = [{<<"directory">>, ?FS_DIRECTORY_REALM}
                 ,{<<"chatplan">>, ?FS_CHATPLAN_REALM}
                 ,{<<"dialplan">>, ?FS_DIALPLAN_REALM}
                ],
    Filter = whapps_config:get(?MOD_CONFIG_CAT, <<"realm_templates_to_process">>, ?FS_REALM_TEMPLATES),
    _ = [process_realms(Realms, D, T)
         || {D, T} <- Templates,
            lists:member(wh_util:to_binary(T), Filter)
        ],
    'ok'.

-spec process_realms(api_binaries(), ne_binary(), atom()) -> 'ok'.
process_realms('undefined', _Dir, _Module) -> 'ok';
process_realms([], _, _) -> 'ok';
process_realms([Realm | Realms], Dir, Module) ->
    process_realm(Realm, Dir, Module),
    process_realms(Realms, Dir, Module).

-spec process_realm(ne_binary(), ne_binary(), atom()) -> 'ok'.
process_realm(Realm, Dir, Module) ->
    Props = [{<<"realm">>, Realm}],
    WorkDir = get(<<"WorkDir">>),
    OutDir = filename:join([WorkDir, Dir]),
    wh_util:make_dir(OutDir),
    XMLFile = filename:join([OutDir, <<Realm/binary,".xml">>]),

    case render(Module, Props) of
        {'ok', Result} ->
            wh_util:write_file(XMLFile, Result),
            lager:debug("wrote file ~s", [XMLFile]);
        {'error', E} ->
            lager:debug("error rendering template ~s for realm ~s: ~p"
                        ,[Module, Realm, E]
                       )
    end.

-spec build_freeswitch(pid()) -> any().
build_freeswitch(Pid) ->
    WorkDir = setup_directory(),
    AllDBs = wnm_util:get_all_number_dbs(),
    _ = [crawl_numbers_db(Db) || Db <- AllDBs, is_number_db(Db)],
    process_realms(),
    File = zip_directory(WorkDir),
    del_dir(wh_util:to_list(WorkDir)),
    gen_server:cast(Pid, {'completed', File}).

-spec is_number_db(ne_binary()) -> boolean().
%% should this change (+) go into wnm_util ?
is_number_db(<<"numbers/+", _/binary>>) -> 'true';
is_number_db(<<"numbers%2f%2b", _/binary>>) -> 'true';
is_number_db(<<"numbers%2F%2B", _/binary>>) -> 'true';
is_number_db(_) -> 'false'.

-spec crawl_numbers_db(ne_binary()) -> 'ok'.
crawl_numbers_db(NumberDb) ->
    lager:debug("getting all numbers from ~s",[NumberDb]),
    Db = wh_util:to_binary(http_uri:encode(wh_util:to_list(NumberDb))),
    try couch_mgr:all_docs(Db) of
        {'ok', []} ->
            lager:debug("no number docs in ~s",[NumberDb]);
        {'ok', JObjs} ->
            Numbers = get_numbers(JObjs),
            maybe_export_numbers(Db, Numbers);
        {'error', _R} ->
            lager:debug("error getting number docs from ~s: ~p"
                        ,[NumberDb, _R]
                       )
    catch
         _E:_R ->
            lager:debug("~s getting number docs from ~s: ~p"
                        ,[_E, Db, _R])
    end.

-spec get_numbers(wh_json:objects()) -> ne_binaries().
get_numbers(JObjs) ->
    [Number
     || JObj <- JObjs
            ,case (Number = wh_doc:id(JObj)) of
                 <<"_design/", _/binary>> -> 'false';
                 _Else -> 'true'
             end
    ].

-spec maybe_export_numbers(ne_binary(), ne_binaries()) -> 'ok'.
maybe_export_numbers(_, []) -> 'ok';
maybe_export_numbers(Db, [Number|Numbers]) ->
    _ = case couch_mgr:open_doc(Db, Number) of
            {'ok', JObj} ->
                maybe_export_number(Number
                                    ,wh_json:get_value(?PVT_NUMBER_STATE, JObj)
                                    ,wh_json:get_value(<<"pvt_assigned_to">>, JObj)
                                   );
            {'error', _R} ->
                lager:debug("error fetching number ~s from ~d: ~p"
                            ,[Number, Db, _R]
                           )
        end,
    maybe_export_numbers(Db, Numbers).

-spec maybe_export_number(ne_binary(), api_binary(), api_binary()) -> 'ok'.
maybe_export_number(Number, ?NUMBER_STATE_IN_SERVICE, AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = [{'key', Number}
                   ,'include_docs'
                  ],
    %% TODO: This is not very DB friendly as we are iterating the numbers
    %%    and the callflows.  Once possible improvement might be to walk
    %%    the accounts, pulling all callflows and building for any assigned
    %%    reconcilable number (instead of walking the numbers).
    case couch_mgr:get_results(AccountDb, ?CALLFLOW_VIEW, ViewOptions) of
        {'ok', []} ->
            lager:debug("number ~s in service for account ~s but no callflows using it"
                        ,[Number, AccountId]
                       );
        {'ok', JObjs} ->
            Flows = [wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs],
            process_callflows(Number, AccountId, Flows);
        {'error', _R} ->
            lager:debug("unable to get callflows for number ~s in account ~s"
                        ,[Number, AccountId]
                       )
    end;
maybe_export_number(_, _, _) -> 'ok'.

-spec process_callflows(ne_binary(), ne_binary(), wh_json:objects()) -> 'ok'.
process_callflows(_, _, []) -> 'ok';
process_callflows(Number, AccountId, [JObj | JObjs]) ->
    FlowId = wh_doc:id(JObj),
    Flow = wh_json:get_value(<<"flow">>, JObj),
    lager:debug("processing callflow ~s in account ~s with number ~s"
                ,[FlowId, AccountId, Number]),
    process_callflow(Number, AccountId, Flow),
    process_callflows(Number, AccountId, JObjs).

-spec process_callflow(ne_binary(), ne_binary(), api_object()) -> 'ok'.
process_callflow(_, _, 'undefined') -> 'ok';
process_callflow(Number, AccountId, Flow) ->
    Module = wh_json:get_value(<<"module">>, Flow),
    Data = wh_json:get_value([<<"data">>,<<"id">>], Flow),
    Children = wh_json:get_value(<<"children">>, Flow),
    process_callflow(Number, AccountId, Module, Data),
    process_callflow(Number, AccountId
                     ,case wh_json:is_empty(Children) of
                          'true' -> 'undefined';
                          _ -> Children
                      end).

-spec process_callflow(ne_binary(), ne_binary(), ne_binary(), api_binary()) -> 'ok'.
process_callflow(_, _, _, 'undefined') -> 'ok';
process_callflow(Number, AccountId, <<"device">>, DeviceId) ->
    lager:debug("found device ~s associated with ~s"
                ,[DeviceId, Number]
               ),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, DeviceId) of
        {'ok', JObj } -> process_device(Number, AccountId, JObj);
        {'error', _R} ->
            lager:debug("unable to get device ~s from account ~s: ~p"
                        ,[DeviceId, AccountId, _R]
                       )
    end;
process_callflow(Number, AccountId, <<"user">>, UserId) ->
    lager:debug("found user ~s associated with ~s"
                ,[UserId, Number]
               ),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = [{'key', UserId}],
    case couch_mgr:get_results(AccountDb, ?DEVICES_VIEW, ViewOptions) of
        {'ok', JObjs} ->
            Devices = [wh_json:get_value([<<"value">>,<<"id">>], JObj)
                       || JObj <- JObjs
                      ],
            [process_callflow(Number, AccountId, <<"device">>, DeviceId)
             || DeviceId <- Devices
            ];
        {'error', _R} ->
            lager:debug("unable to get user ~s from account ~s: ~p"
                        ,[UserId, AccountId, _R]
                       )
    end;
process_callflow(_, _, _, _) -> 'ok'.

-spec process_device(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
process_device(Number, AccountId, JObj) ->
    AccountRealm = wh_util:get_account_realm(AccountId),
    Realm = kz_device:sip_realm(JObj, AccountRealm),
    Username = kz_device:sip_username(JObj),
    case query_registrar(Realm, Username) of
        {'ok', Auth} ->
            Props = props_for_rendering(Number, Username, Realm, Auth),
            render_templates(Number, AccountId, Username, Realm, Props),
            lager:debug("rendered templates");
        {'error', _R} ->
            lager:debug("unable to query registrar for credentails of ~s@~s in account ~s: ~p"
                        ,[Username, Realm, AccountId, _R]
                       )
    end.

-spec props_for_rendering(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> wh_proplist().
props_for_rendering(Number, Username, Realm, Auth) ->
    props:filter_empty(
      wh_json:recursive_to_proplist(
        normalize(
          wh_json:set_values(
            [{<<"effective_caller_id_number">>, Number}
             ,{<<"username">>, Username}
             ,{<<"realm">>, Realm}
             ,{<<"number">>, Number}
            ], Auth)))).

-spec normalize(wh_json:object()) -> wh_json:object().
normalize(JObj) ->
    JHeaders = wh_json:get_value(<<"Custom-SIP-Headers">>, JObj, []),
    JVariables = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, []),
    Headers = [[{<<"name">>, K}, {<<"value">>, V}]
               || {K, V} <- wh_json:to_proplist(JHeaders)
              ],
    Variables = [[{<<"name">>, <<?CHANNEL_VAR_PREFIX, K/binary>>}, {<<"value">>, V}]
                 || {K, V} <- wh_json:to_proplist(JVariables)
                ],
    wh_json:set_values([{<<"variables">>, Variables}
                        ,{<<"headers">>, Headers}
                       ],
                       wh_json:normalize(
                         wh_json:delete_keys(
                           [<<"Custom-SIP-Headers">>
                            ,<<"Custom-Channel-Vars">>
                           ], JObj)
                      )).

-spec render_templates(ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) -> 'ok'.
render_templates(Number, AccountId, Username, Realm, Props) ->
    Templates = [{"directory", ?FS_DIRECTORY}
                 ,{"chatplan", ?FS_CHATPLAN}
                 ,{"dialplan", ?FS_DIALPLAN}
                ],
    Filter = whapps_config:get(?MOD_CONFIG_CAT, <<"templates_to_process">>, ?DEFAULT_FS_TEMPLATES),
    _ = [render_template(Number, AccountId, Username, Realm, Props, D, T)
         || {D, T} <- Templates, lists:member(wh_util:to_binary(T), Filter)
        ],
    'ok'.

-spec render_template(ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist(), file:name_all(), atom()) -> 'ok'.
render_template(Number, AccountId, Username, Realm, Props, Dir, Module) ->
    maybe_accumulate_realm(lists:member(Realm, get(<<"Realms">>)), Realm),
    WorkDir = get(<<"WorkDir">>),
    OutDir = filename:join([WorkDir, Dir, Realm]),
    wh_util:make_dir(OutDir),
    XMLFile = filename:join([OutDir, <<Username/binary,".xml">>]),
    case render(Module, Props) of
        {'ok', Result} -> wh_util:write_file(XMLFile, Result);
        {'error', _R} ->
            lager:debug("unable to render template ~s for ~s in account ~s: ~p"
                        ,[Module, Number, AccountId, _R])
    end.

-spec maybe_accumulate_realm(boolean(), ne_binary()) -> any().
maybe_accumulate_realm('true', _) -> 'ok';
maybe_accumulate_realm('false', Realm) ->
    put(<<"Realms">>, [Realm | get(<<"Realms">>)]).

-spec query_registrar(ne_binary(), ne_binary()) -> {'ok', wh_json:object()}
                                                       | {'error', _}.
query_registrar(Realm, Username) ->
    FullUser = <<Username/binary, "@", Realm/binary>>,
    Req = [{<<"To">>, FullUser}
           ,{<<"From">>, FullUser}
           ,{<<"Auth-User">>, Username}
           ,{<<"Auth-Realm">>, Realm}
           ,{<<"Method">>, <<"REGISTER">>}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    whapps_util:amqp_pool_request(props:filter_undefined(Req)
                                  ,fun wapi_authn:publish_req/1
                                  ,fun wapi_authn:resp_v/1
                                  ,?AUTHN_TIMEOUT).

-spec template_file(atom()) -> string().
template_file(Module) ->
    filename:join([code:lib_dir('crossbar', 'priv')
                   ,"freeswitch"
                  ,template_file_name(Module)
                  ]).

-spec template_file_name(?FS_CHATPLAN | ?FS_DIALPLAN | ?FS_DIRECTORY | ?FS_DIRECTORY_REALM) -> string().
template_file_name(?FS_DIALPLAN) -> "dialplan_template.xml";
template_file_name(?FS_CHATPLAN) -> "chatplan_template.xml";
template_file_name(?FS_DIRECTORY) -> "directory_template.xml";
template_file_name(?FS_DIRECTORY_REALM) -> "directory_realm_template.xml".

-spec compile_templates() -> ['ok',...] | [].
compile_templates() ->
    [compile_template(wh_util:to_atom(T,'true')) || T <- ?FS_ALL_TEMPLATES].

-spec compile_template(atom()) -> 'ok'.
compile_template(Module) ->
    compile_template(Module, whapps_config:get_binary(?MOD_CONFIG_CAT, wh_util:to_binary(Module))).

-spec compile_template(atom(), api_binary()) -> 'ok'.
compile_template(Module, 'undefined') ->
    {'ok', Contents} = file:read_file(template_file(Module)),
    whapps_config:set(?MOD_CONFIG_CAT, wh_util:to_binary(Module), Contents),
    compile_template(Module, Contents);
compile_template(Module, Template) ->
    case erlydtl:compile_template(Template, Module, [{'out_dir', 'false'}]) of
        {'ok', _T} ->
            lager:debug("compiled template ~s", [_T]);
        {'ok', _T, _W} ->
            lager:debug("compiled template ~s with warnings: ~p", [_T, _W])
    end.

-spec render(atom(), wh_proplist()) -> {'ok', iolist()} | {'error', _}.
render(Module, Props) ->
    try Module:render(props:filter_empty(Props)) of
        {'ok', _}=OK -> OK
    catch
        _E:_R ->
            lager:debug("failed to render template ~s: ~p"
                        ,[Module, _R]),
            {'error', _E}
    end.

-spec xml_file(atom()) -> string().
xml_file(Module) ->
    filename:join([code:lib_dir('crossbar', 'priv')
                   ,"freeswitch"
                  ,xml_file_name(Module)
                  ]).

-spec xml_file_name(?FS_CHATPLAN | ?FS_DIALPLAN | ?FS_DIRECTORY) -> string().
xml_file_name(?FS_DIALPLAN) -> "dialplan.xml";
xml_file_name(?FS_CHATPLAN) -> "chatplan.xml";
xml_file_name(?FS_DIRECTORY) -> "directory.xml".

-spec xml_file_from_config(?FS_CHATPLAN | ?FS_DIALPLAN | ?FS_DIRECTORY) -> ne_binary().
-spec xml_file_from_config(?FS_CHATPLAN | ?FS_DIALPLAN | ?FS_DIRECTORY, ne_binary()) -> ne_binary().
xml_file_from_config(Module) ->
    KeyName = <<(wh_util:to_binary(Module))/binary,"_top_dir_file_content">>,
    xml_file_from_config(Module, KeyName).
xml_file_from_config(Module, KeyName) ->
    xml_file_from_config(Module, whapps_config:get_binary(?MOD_CONFIG_CAT, KeyName), KeyName).

-spec xml_file_from_config(atom(), api_binary(), ne_binary()) -> ne_binary().
xml_file_from_config(Module, 'undefined', KeyName) ->
    {'ok', Contents} = file:read_file(xml_file(Module)),
    whapps_config:set(?MOD_CONFIG_CAT, KeyName, Contents),
    Contents;
xml_file_from_config(_, Contents, _) -> Contents.

-spec del_dir(string()) -> 'ok'.
%% TODO: This should be moved to a wh_file helper
%%    when wh_util is cleaned-up
del_dir(Dir) ->
    lists:foreach(fun(D) ->
                          'ok' = file:del_dir(D)
                  end, del_all_files([Dir], [])).

-spec del_all_files(strings(), strings()) -> strings().
del_all_files([], EmptyDirs) -> EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
    {'ok', FilesInDir} = file:list_dir(Dir),
    {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                        Path = Dir ++ "/" ++ F,
                                        case filelib:is_dir(Path) of
                                            'true' ->
                                                {Fs, [Path | Ds]};
                                            'false' ->
                                                {[Path | Fs], Ds}
                                        end
                                end, {[],[]}, FilesInDir),
    _ = lists:foreach(fun(F) ->
                              'ok' = file:delete(F)
                      end, Files),
    del_all_files(T ++ Dirs, [Dir | EmptyDirs]).
