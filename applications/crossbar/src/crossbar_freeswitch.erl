%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Create freeswitch offline configuration
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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


-export([reset/0]).

-include("crossbar.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(SERVER, ?MODULE).

-define(CALLFLOW_VIEW, <<"callflows/listing_by_number">>).
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

-record(state, {config = 'undefined' :: kz_term:api_binary()
               ,is_running = 'false' :: boolean()
               ,monitor :: kz_term:api_reference()
               ,hourly_timer = hourly_timer() :: reference()
               }).
-type state() :: #state{}.

%% this shouldn't be here. we need to move this definition from
%% ecallmgr.hrl into the database or into kazoo/include
-define(CHANNEL_VAR_PREFIX, "ecallmgr_").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reset() -> 'ok'.
reset() ->
    gen_server:cast(crossbar_sup:find_proc(?SERVER), 'reset').

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link(?SERVER, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, 'true'),
    compile_templates(),
    _  = gen_server:cast(self(), 'periodic_build'),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('current', _From, #state{config='undefined'}=State) ->
    {'reply', {'error', 'no_file'}, State};
handle_call('current', _From, #state{config=Config}=State) ->
    {'reply', {'ok', Config}, State};

handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('periodic_build', #state{is_running='true'}=State) ->
    {'noreply', State};
handle_cast('periodic_build', #state{is_running='false'}=State) ->
    {Pid, Monitor} = kz_process:spawn_monitor(fun build_freeswitch/1, [self()]),
    lager:debug("started new freeswitch offline configuration builder ~p", [Pid]),
    {'noreply', State#state{is_running='true', monitor=Monitor}};

handle_cast({'completed', File}, #state{config=Config}=State) ->
    lager:debug("created new freeswitch offline configuration ~s", [File]),
    gen_server:cast(self(), {'delete', Config}),
    {'noreply', State#state{is_running='false', config=File}};

handle_cast({'delete', 'undefined'}, State) ->
    {'noreply', State};
handle_cast({'delete', File}, State) ->
    lager:debug("removing prior freeswitch offline configuration ~s", [File]),
    kz_util:delete_file(File),
    {'noreply', State};

handle_cast('reset', #state{config=Config}=State) ->
    lager:debug("resetting freeswitch state"),
    gen_server:cast(self(), {'delete', Config}),
    {'noreply', State#state{is_running='false'}};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'DOWN', MonitorRef, 'process', _, 'normal'}, #state{monitor=MonitorRef}=State) ->
    {'noreply', State#state{is_running='false'}};
handle_info({'DOWN', MonitorRef, _, _Pid, _Reason}, #state{monitor=MonitorRef}=State) ->
    lager:debug("freeswitch offline configuration builder ~p died unexpectedly: ~p"
               ,[_Pid, _Reason]),
    {'noreply', State#state{is_running='false'}};

handle_info({timeout, Ref, _Msg}, #state{hourly_timer = Ref}=State) ->
    _  = gen_server:cast(self(), 'periodic_build'),
    {'noreply', State#state{hourly_timer = hourly_timer()}};

handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of {@link init/1} and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("crossbar freeswitch terminating: ~p", [_Reason]).

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec hourly_timer() -> reference().
hourly_timer() ->
    erlang:start_timer(?MILLISECONDS_IN_HOUR, self(), ok).

-spec zip_directory(file:filename_all()) -> string().
zip_directory(WorkDir0) ->
    WorkDir = kz_term:to_list(WorkDir0),
    ZipName = lists:concat([WorkDir, ".zip"]),
    Files = [kz_term:to_list(F) || F <- filelib:wildcard("*", WorkDir)],
    {'ok', _} = zip:zip(ZipName , Files, [{'cwd', WorkDir}]),
    ZipName.

-spec setup_directory() -> file:filename_all().
setup_directory() ->
    TopDir = kz_binary:rand_hex(8),
    WorkRootDir = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"work_dir">>, <<"/tmp/">>),
    WorkDir = filename:join([WorkRootDir, TopDir]),
    kz_util:make_dir(WorkDir),
    Files = [{<<"directory">>, ?FS_DIRECTORY}
            ,{<<"chatplan">>, ?FS_CHATPLAN}
            ,{<<"dialplan">>, ?FS_DIALPLAN}
            ],
    Filter = kapps_config:get(?MOD_CONFIG_CAT, <<"files_to_include">>, ?DEFAULT_FS_INCLUDE_DIRECTORY_FILES),
    _ = [kz_util:make_dir(filename:join([WorkDir, Dir])) || {Dir, _} <- Files],
    _ = [kz_util:write_file(filename:join([WorkDir, D, xml_file_name(T)])
                           ,xml_file_from_config(T)
                           )
         || {D, T} <- Files,
            lists:member(kz_term:to_binary(T), Filter)
        ],
    _ = put(<<"WorkDir">>, WorkDir),
    _ = put(<<"Realms">>, []),
    WorkDir.

-spec process_realms() -> 'ok'.
process_realms() ->
    Realms = get(<<"Realms">>),
    Templates = [{<<"directory">>, ?FS_DIRECTORY_REALM}
                ,{<<"chatplan">>, ?FS_CHATPLAN_REALM}
                ,{<<"dialplan">>, ?FS_DIALPLAN_REALM}
                ],
    Filter = kapps_config:get(?MOD_CONFIG_CAT, <<"realm_templates_to_process">>, ?FS_REALM_TEMPLATES),
    _ = [process_realms(Realms, D, T)
         || {D, T} <- Templates,
            lists:member(kz_term:to_binary(T), Filter)
        ],
    'ok'.

-spec process_realms(kz_term:api_binaries(), kz_term:ne_binary(), atom()) -> 'ok'.
process_realms('undefined', _Dir, _Module) -> 'ok';
process_realms([], _, _) -> 'ok';
process_realms([Realm | Realms], Dir, Module) ->
    process_realm(Realm, Dir, Module),
    process_realms(Realms, Dir, Module).

-spec process_realm(kz_term:ne_binary(), kz_term:ne_binary(), atom()) -> 'ok'.
process_realm(Realm, Dir, Module) ->
    Props = [{<<"realm">>, Realm}],
    WorkDir = get(<<"WorkDir">>),
    OutDir = filename:join([WorkDir, Dir]),
    kz_util:make_dir(OutDir),
    XMLFile = filename:join([OutDir, <<Realm/binary,".xml">>]),
    case kz_template:render(Module, Props) of
        {'ok', Result} ->
            kz_util:write_file(XMLFile, Result),
            lager:debug("wrote file ~s", [XMLFile]);
        {'error', E} ->
            lager:debug("error rendering template ~s for realm ~s: ~p"
                       ,[Module, Realm, E])
    end.

-spec build_freeswitch(pid()) -> any().
build_freeswitch(Pid) ->
    WorkDir = setup_directory(),
    lists:foreach(fun crawl_numbers_db/1, knm_util:get_all_number_dbs()),
    process_realms(),
    File = zip_directory(WorkDir),
    kz_util:delete_dir(kz_term:to_list(WorkDir)),
    gen_server:cast(Pid, {'completed', File}).

-spec crawl_numbers_db(kz_term:ne_binary()) -> 'ok'.
crawl_numbers_db(NumberDb) ->
    lager:debug("getting all numbers from ~s",[NumberDb]),
    Db = kz_term:to_binary(http_uri:encode(kz_term:to_list(NumberDb))),
    try kz_datamgr:all_docs(Db) of
        {'ok', []} ->
            lager:debug("no number docs in ~s",[NumberDb]);
        {'ok', JObjs} ->
            Numbers = get_numbers(JObjs),
            maybe_export_numbers(Db, Numbers);
        {'error', _R} ->
            lager:debug("error getting number docs from ~s: ~p", [NumberDb, _R])
    catch
        _E:_R ->
            lager:debug("~s getting number docs from ~s: ~p", [_E, Db, _R])
    end.

-spec get_numbers(kz_json:objects()) -> kz_term:ne_binaries().
get_numbers(JObjs) ->
    [Number
     || JObj <- JObjs,
        case (Number = kz_doc:id(JObj)) of
            <<"_design/", _/binary>> -> 'false';
            _Else -> 'true'
        end
    ].

-spec maybe_export_numbers(kz_term:ne_binary(), kz_term:ne_binaries()) -> 'ok'.
maybe_export_numbers(_, []) -> 'ok';
maybe_export_numbers(Db, [Number|Numbers]) ->
    _ = case kz_datamgr:open_doc(Db, Number) of
            {'ok', JObj} ->
                maybe_export_number(Number
                                   ,kzd_phone_numbers:pvt_state(JObj)
                                   ,kzd_phone_numbers:pvt_assigned_to(JObj)
                                   );
            {'error', _R} ->
                lager:debug("error fetching number ~s from ~d: ~p", [Number, Db, _R])
        end,
    maybe_export_numbers(Db, Numbers).

-spec maybe_export_number(kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary()) -> 'ok'.
maybe_export_number(Number, ?NUMBER_STATE_IN_SERVICE, AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = [{'key', Number}
                  ,'include_docs'
                  ],
    %% TODO: This is not very DB friendly as we are iterating the numbers
    %%    and the callflows.  Once possible improvement might be to walk
    %%    the accounts, pulling all callflows and building for any assigned
    %%    reconcilable number (instead of walking the numbers).
    case kz_datamgr:get_results(AccountDb, ?CALLFLOW_VIEW, ViewOptions) of
        {'ok', []} ->
            lager:debug("number ~s in service for account ~s but no callflows using it"
                       ,[Number, AccountId]);
        {'ok', JObjs} ->
            Flows = [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs],
            process_callflows(Number, AccountId, Flows);
        {'error', _R} ->
            lager:debug("unable to get callflows for number ~s in account ~s"
                       ,[Number, AccountId])
    end;
maybe_export_number(_, _, _) -> 'ok'.

-spec process_callflows(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
process_callflows(_, _, []) -> 'ok';
process_callflows(Number, AccountId, [JObj | JObjs]) ->
    FlowId = kz_doc:id(JObj),
    Flow = kz_json:get_value(<<"flow">>, JObj),
    lager:debug("processing callflow ~s in account ~s with number ~s"
               ,[FlowId, AccountId, Number]),
    process_callflow(Number, AccountId, Flow),
    process_callflows(Number, AccountId, JObjs).

-spec process_callflow(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_object()) -> 'ok'.
process_callflow(_, _, 'undefined') -> 'ok';
process_callflow(Number, AccountId, Flow) ->
    Module = kz_json:get_value(<<"module">>, Flow),
    Data = kz_json:get_value([<<"data">>,<<"id">>], Flow),
    Children = kz_json:get_value(<<"children">>, Flow),
    process_callflow(Number, AccountId, Module, Data),
    process_callflow(Number, AccountId
                    ,case kz_json:is_empty(Children) of
                         'true' -> 'undefined';
                         _ -> Children
                     end).

-spec process_callflow(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) -> 'ok'.
process_callflow(_, _, _, 'undefined') -> 'ok';
process_callflow(Number, AccountId, <<"device">>, DeviceId) ->
    lager:debug("found device ~s associated with ~s", [DeviceId, Number]),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_cache_doc(AccountDb, DeviceId) of
        {'ok', JObj } -> process_device(Number, AccountId, JObj);
        {'error', _R} ->
            lager:debug("unable to get device ~s from account ~s: ~p"
                       ,[DeviceId, AccountId, _R])
    end;
process_callflow(Number, AccountId, <<"user">>, UserId) ->
    lager:debug("found user ~s associated with ~s", [UserId, Number]),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = [{'key', UserId}],
    case kz_datamgr:get_results(AccountDb, ?DEVICES_VIEW, ViewOptions) of
        {'ok', JObjs} ->
            Devices = [kz_json:get_value([<<"value">>,<<"id">>], JObj)
                       || JObj <- JObjs,
                          kz_json:is_false([<<"value">>, <<"hotdesked">>], JObj)
                      ],
            [process_callflow(Number, AccountId, <<"device">>, DeviceId)
             || DeviceId <- Devices
            ];
        {'error', _R} ->
            lager:debug("unable to get user ~s from account ~s: ~p"
                       ,[UserId, AccountId, _R])
    end;
process_callflow(_, _, _, _) -> 'ok'.

-spec process_device(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
process_device(Number, AccountId, JObj) ->
    AccountRealm = kzd_accounts:fetch_realm(AccountId),
    Realm = kzd_devices:sip_realm(JObj, AccountRealm),
    Username = kzd_devices:sip_username(JObj),
    case query_registrar(Realm, Username) of
        {'ok', Auth} ->
            Props = props_for_rendering(Number, Username, Realm, Auth),
            render_templates(Number, AccountId, Username, Realm, Props),
            lager:debug("rendered templates");
        {'error', _R} ->
            lager:debug("unable to query registrar for credentials of ~s@~s in account ~s: ~p"
                       ,[Username, Realm, AccountId, _R])
    end.

-spec props_for_rendering(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:proplist().
props_for_rendering(Number, Username, Realm, Auth) ->
    props:filter_empty(
      kz_json:recursive_to_proplist(
        normalize(
          kz_json:set_values([{<<"effective_caller_id_number">>, Number}
                             ,{<<"username">>, Username}
                             ,{<<"realm">>, Realm}
                             ,{<<"number">>, Number}
                             ]
                            ,Auth
                            )))).

-spec normalize(kz_json:object()) -> kz_json:object().
normalize(JObj) ->
    JHeaders = kz_json:get_value(<<"Custom-SIP-Headers">>, JObj, []),
    JVariables = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, []),
    Headers = [[{<<"name">>, K}, {<<"value">>, V}]
               || {K, V} <- kz_json:to_proplist(JHeaders)
              ],
    Variables = [[{<<"name">>, <<?CHANNEL_VAR_PREFIX, K/binary>>}, {<<"value">>, V}]
                 || {K, V} <- kz_json:to_proplist(JVariables)
                ],
    kz_json:set_values([{<<"variables">>, Variables}
                       ,{<<"headers">>, Headers}
                       ]
                      ,kz_json:normalize(
                         kz_json:delete_keys([<<"Custom-SIP-Headers">>
                                             ,<<"Custom-Channel-Vars">>
                                             ]
                                            ,JObj
                                            )
                        )).

-spec render_templates(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
render_templates(Number, AccountId, Username, Realm, Props) ->
    Templates = [{"directory", ?FS_DIRECTORY}
                ,{"chatplan", ?FS_CHATPLAN}
                ,{"dialplan", ?FS_DIALPLAN}
                ],
    Filter = kapps_config:get(?MOD_CONFIG_CAT, <<"templates_to_process">>, ?DEFAULT_FS_TEMPLATES),
    _ = [render_template(Number, AccountId, Username, Realm, Props, D, T)
         || {D, T} <- Templates, lists:member(kz_term:to_binary(T), Filter)
        ],
    'ok'.

-spec render_template(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), file:name_all(), atom()) -> 'ok'.
render_template(Number, AccountId, Username, Realm, Props, Dir, Module) ->
    maybe_accumulate_realm(lists:member(Realm, get(<<"Realms">>)), Realm),
    WorkDir = get(<<"WorkDir">>),
    OutDir = filename:join([WorkDir, Dir, Realm]),
    kz_util:make_dir(OutDir),
    XMLFile = filename:join([OutDir, <<Username/binary,".xml">>]),
    case kz_template:render(Module, Props) of
        {'ok', Result} -> kz_util:write_file(XMLFile, Result);
        {'error', _R} ->
            lager:debug("unable to render template ~s for ~s in account ~s: ~p"
                       ,[Module, Number, AccountId, _R])
    end.

-spec maybe_accumulate_realm(boolean(), kz_term:ne_binary()) -> any().
maybe_accumulate_realm('true', _) -> 'ok';
maybe_accumulate_realm('false', Realm) ->
    put(<<"Realms">>, [Realm | get(<<"Realms">>)]).

-spec query_registrar(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_json:object()}
                                                                       | {'error', any()}.
query_registrar(Realm, Username) ->
    FullUser = <<Username/binary, "@", Realm/binary>>,
    Req = [{<<"To">>, FullUser}
          ,{<<"From">>, FullUser}
          ,{<<"Auth-User">>, Username}
          ,{<<"Auth-Realm">>, Realm}
          ,{<<"Method">>, <<"REGISTER">>}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:call(props:filter_undefined(Req)
                       ,fun kapi_authn:publish_req/1
                       ,fun kapi_authn:resp_v/1
                       ,?AUTHN_TIMEOUT
                       ).

-spec template_file(atom()) -> string().
template_file(Module) ->
    filename:join([code:priv_dir(?APP), "freeswitch", template_file_name(Module)]).

-spec template_file_name(?FS_CHATPLAN | ?FS_DIALPLAN | ?FS_DIRECTORY | ?FS_DIRECTORY_REALM) -> string().
template_file_name(?FS_DIALPLAN) -> "dialplan_template.xml";
template_file_name(?FS_CHATPLAN) -> "chatplan_template.xml";
template_file_name(?FS_DIRECTORY) -> "directory_template.xml";
template_file_name(?FS_DIRECTORY_REALM) -> "directory_realm_template.xml".

-spec compile_templates() -> ok.
compile_templates() ->
    F = fun (T) -> compile_template(kz_term:to_atom(T, 'true')) end,
    lists:foreach(F, ?FS_ALL_TEMPLATES).

-spec compile_template(atom()) -> 'ok'.
compile_template(Module) ->
    compile_template(Module, kapps_config:get_binary(?MOD_CONFIG_CAT, kz_term:to_binary(Module))).

-spec compile_template(atom(), kz_term:api_binary()) -> 'ok'.
compile_template(Module, 'undefined') ->
    {'ok', Contents} = file:read_file(template_file(Module)),
    _ = kapps_config:set(?MOD_CONFIG_CAT, kz_term:to_binary(Module), Contents),
    compile_template(Module, Contents);
compile_template(Module, Template) ->
    _ = kz_template:compile(Template, Module),
    'ok'.

-spec xml_file(atom()) -> string().
xml_file(Module) ->
    filename:join([code:priv_dir(?APP), "freeswitch", xml_file_name(Module)]).

-spec xml_file_name(?FS_CHATPLAN | ?FS_DIALPLAN | ?FS_DIRECTORY) -> string().
xml_file_name(?FS_DIALPLAN) -> "dialplan.xml";
xml_file_name(?FS_CHATPLAN) -> "chatplan.xml";
xml_file_name(?FS_DIRECTORY) -> "directory.xml".

-spec xml_file_from_config(?FS_CHATPLAN | ?FS_DIALPLAN | ?FS_DIRECTORY) -> kz_term:ne_binary().
xml_file_from_config(Module) ->
    KeyName = <<(kz_term:to_binary(Module))/binary,"_top_dir_file_content">>,
    xml_file_from_config(Module, KeyName).

-spec xml_file_from_config(?FS_CHATPLAN | ?FS_DIALPLAN | ?FS_DIRECTORY, kz_term:ne_binary()) -> kz_term:ne_binary().
xml_file_from_config(Module, KeyName) ->
    xml_file_from_config(Module, kapps_config:get_binary(?MOD_CONFIG_CAT, KeyName), KeyName).

-spec xml_file_from_config(atom(), kz_term:api_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
xml_file_from_config(Module, 'undefined', KeyName) ->
    {'ok', Contents} = file:read_file(xml_file(Module)),
    _ = kapps_config:set(?MOD_CONFIG_CAT, KeyName, Contents),
    Contents;
xml_file_from_config(_, Contents, _) -> Contents.
