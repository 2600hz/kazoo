%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
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
-define(FS_TEMPLATES, [?FS_DIRECTORY, ?FS_DIALPLAN, ?FS_CHATPLAN]).

-define(AUTHN_TIMEOUT, 5000).

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

reset() ->
    lager:debug("resetting"),    
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
    compile_templates(),
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
    lager:debug("already building offline freeswitch configuration"),
    {'noreply', State};    
handle_cast('periodic_build', #state{is_running='false'}=State) ->
    Pid = spawn(?MODULE, 'build_freeswitch', [self()]),
    Monitor = erlang:monitor('process', Pid),
    {'noreply', State#state{is_running='true', monitor=Monitor}};
handle_cast({'completed', File}, #state{config=Config}=State) ->
    lager:debug("FREESWITCH COMPLETED ~s",[File]),
    gen_server:cast(self(), {'delete', Config}),
    {'noreply', State#state{is_running='false', config=File}};
handle_cast({'delete', 'undefined'}, State) ->
    {'noreply', State};
handle_cast({'delete', File}, State) ->
    file:delete(File),
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
handle_info({'DOWN', MonitorRef, 'process', Object, 'normal'}=A, #state{monitor=MonitorRef}=State) ->
    {'noreply', State#state{is_running='false'}};
handle_info({'DOWN', MonitorRef, Type, Object, Info}=A, #state{monitor=MonitorRef}=State) ->
    lager:debug("build freeswitch crashed ~p",[A]),
    {'noreply', State#state{is_running='false'}};
handle_info(_Info, State) ->
    lager:debug("FREESWITCH unhandled message: ~p", [_Info]),
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

-spec zip_directory(ne_binary()) -> ne_binary().
zip_directory(WorkDir) ->
    ZipName = lists:concat([wh_util:to_list(WorkDir),".zip"]),
    Files = [ wh_util:to_list(F) || F <- filelib:wildcard("*", WorkDir)],
    zip:zip(ZipName , Files, [{cwd, WorkDir}]),
    ZipName.


-spec setup_directory() -> ne_binary().
setup_directory() ->
    Dir = wh_util:rand_hex_binary(8),
    WorkRootDir = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"work_dir">>, <<"/tmp/">>),
    WorkDir = filename:join([WorkRootDir, Dir]), 
    file:make_dir(WorkDir),
    file:make_dir(filename:join([WorkDir, "dialplan"])),
    file:make_dir(filename:join([WorkDir, "chatplan"])),
    file:make_dir(filename:join([WorkDir, "directory"])),    
    put(<<"WorkDir">>,WorkDir),
    WorkDir.


-spec build_freeswitch(pid()) -> any().
build_freeswitch(Pid) ->
    lager:debug("START CREATING OFFLINE FREESWITCH"),
    WorkDir = setup_directory(),
    AllDBs = wnm_util:get_all_number_dbs(),
    lager:debug("ALL DBS ~p",[AllDBs]),
    _ = [crawl_numbers_db(Db) || Db <- AllDBs, is_number_db(Db)],
    File = zip_directory(WorkDir),
    del_dir(wh_util:to_list(WorkDir)),
    gen_server:cast(Pid, {'completed', File}).

%% should this change (+) go into wnm_util ?
is_number_db(<<"numbers/+", _/binary>>) -> 'true';
is_number_db(<<"numbers%2f%2b", _/binary>>) -> 'true';
is_number_db(<<"numbers%2F%2B", _/binary>>) -> 'true';
is_number_db(_) -> 'false'.

crawl_numbers_db(NumberDb) when not is_binary(NumberDb) ->
    crawl_numbers_db(wh_util:to_binary(NumberDb));
crawl_numbers_db(NumberDb) ->
    lager:debug("getting all number docs from ~s",[NumberDb]),
    Db = wh_util:to_binary(http_uri:encode(wh_util:to_list(NumberDb))),
    case couch_mgr:all_docs(Db) of
        {'error', _R} ->
            lager:debug("error [~p] getting number docs from ~s",[_R, NumberDb]);
        {'ok', []} ->
            lager:debug("no number docs in ~s",[NumberDb]);
        {'ok', JObjs} ->
            lager:debug("NUMBERS FROM ~s : ~p",[NumberDb, JObjs]),
            Numbers = [Number
                       || JObj <- JObjs
                              ,case (Number = wh_json:get_value(<<"id">>, JObj)) of
                                   <<"_design/", _/binary>> -> 'false';
                                   _Else -> 'true'
                               end
                      ],
            _ = maybe_export_numbers(Db, Numbers)
    end.

-spec maybe_export_numbers(ne_binary(), ne_binaries()) -> 'ok'.
maybe_export_numbers(_, []) -> 'ok';
maybe_export_numbers(Db, [Number|Numbers]) ->
    case couch_mgr:open_doc(Db, Number) of
        {'ok', JObj} ->
            maybe_export_number(Number
                                ,wh_json:get_value(<<"pvt_number_state">>, JObj)
                                ,wh_json:get_value(<<"pvt_assigned_to">>, JObj)
                               );
        {'error', E} ->
            lager:debug("error reading doc ~s from ~d")
    end,
    maybe_export_numbers(Db, Numbers).    

-spec maybe_export_number(ne_binary(), api_binary(), api_binary()) -> any().
maybe_export_number(_, _, 'undefined') -> 'ok';
maybe_export_number(Number, <<"in_service">>, AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = [{'key', Number}, 'include_docs'],
    case couch_mgr:get_results(AccountDb, ?CALLFLOW_VIEW, ViewOptions) of
        {'ok', []} ->
            lager:debug("number ~s in service for account ~s but no callflows using it.",[Number, AccountId]);
        {'ok', JObjs} ->
            Flows = [wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs],
            process_callflows(Number, AccountId, Flows);
        {'error', _} ->
            lager:debug("error getting callflows for number ~s in account ~s",[Number, AccountId])
    end;
maybe_export_number(_, _, _) -> 'ok'.

-spec process_callflows(ne_binary(), ne_binary(), wh_json:objects()) -> any().
process_callflows(_, _, []) -> 'ok';
process_callflows(Number, AccountId, [JObj | JObjs]) ->
    FlowId = wh_json:get_value(<<"_id">>, JObj),
    Flow = wh_json:get_value(<<"flow">>, JObj),
    lager:debug("start processing callflow ~s for number ~s",[FlowId, Number]),
    process_callflow(Number, AccountId, Flow),
    process_callflows(Number, AccountId, JObjs).

-spec process_callflow(ne_binary(), ne_binary(), api_object()) -> any().
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

-spec process_callflow(ne_binary(), ne_binary(), ne_binary(), api_binary()) -> any().
process_callflow(_, _, _, 'undefined') -> 'ok';
process_callflow(Number, AccountId, <<"device">>, DeviceId) ->
    lager:debug("DEVICE ~s/~p",[Number,DeviceId]),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, DeviceId) of
        {'ok', JObj } ->
            process_device(Number, AccountId, DeviceId, JObj);
        {'error', E} ->
            lager:debug("~s : error getting doc for device ~s in account ~s",[Number,DeviceId, AccountId])
    end;
process_callflow(Number, AccountId, <<"user">>, UserId) ->
    lager:debug("USERID ~s/~p",[Number, UserId]),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = [{'key', UserId}],
    case couch_mgr:get_results(AccountDb, ?DEVICES_VIEW, ViewOptions) of
        {'ok', JObjs} ->
            Devices = [wh_json:get_value([<<"value">>,<<"id">>], JObj) || JObj <- JObjs],
            [process_callflow(Number, AccountId, <<"device">>, DeviceId) || DeviceId <- Devices];
        _ -> 'ok'
    end;
process_callflow(_, _, _, _) -> 'ok'.

process_device(Number, AccountId, DeviceId, JObj) ->
    AccountRealm = wh_util:get_account_realm(AccountId),
    Realm = wh_json:get_value([<<"sip">>,<<"realm">>],JObj, AccountRealm),
    Username = wh_json:get_value([<<"sip">>,<<"username">>],JObj),
    case query_registrar(Realm, Username) of
        {'error', E} ->
            lager:debug("~s : error [~p] processing credentials for ~s@~s in account ~s"
                        ,[Number, E, Username, Realm, AccountId]);
        {'ok', Auth} ->
            Props = props_for_rendering(Number, AccountId, Username, Realm, JObj, Auth),
            render_templates(Number, AccountId, Username, Realm, Props)
    end.


props_for_rendering(Number, AccountId, Username, Realm, JObj, Auth) ->
    props:filter_empty(
      wh_json:recursive_to_proplist(
        normalize(
          wh_json:set_values(
            [{<<"effective_caller_id_number">>, Number}
            ,{<<"username">>, Username}
            ,{<<"realm">>, Realm}
            ,{<<"number">>, Number}
            ], Auth)))).
  
normalize(JObj) ->
    JHeaders = wh_json:get_value(<<"Custom-SIP-Headers">>, JObj, []),
    JVariables = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, []),
    Headers = [ [{<<"name">>,K},{<<"value">>, V}] || {K, V} <- wh_json:to_proplist(JHeaders)],
    Variables = [ [{<<"name">>,<<?CHANNEL_VAR_PREFIX, K/binary>>} ,{<<"value">>, V}] || {K, V} <- wh_json:to_proplist(JVariables)],
    wh_json:set_values([{<<"variables">>, Variables}
                        ,{<<"headers">>, Headers}
                       ],
                       wh_json:normalize(
                         wh_json:delete_keys(
                           [<<"Custom-SIP-Headers">>
                            ,<<"Custom-Channel-Vars">>
                           ], JObj)
                      )).

render_templates(Number, AccountId, Username, Realm, Props) ->
    lager:debug("~s : Rendering Templates [~s, ~s, ~s]", [Number, AccountId, Username, Realm]),
    Templates = [{"directory", ?FS_DIRECTORY}
                ,{"chatplan", ?FS_CHATPLAN}
                ,{"dialplan", ?FS_DIALPLAN}
                ],
    [ render_template(Number, AccountId, Username, Realm, Props, D, T) || {D, T} <- Templates].
    
render_template(Number, AccountId, Username, Realm, Props, Dir, Module) ->
    WorkDir = get(<<"WorkDir">>),
    OutDir = filename:join([WorkDir, Dir, Realm]),
    file:make_dir(OutDir),
    XMLFile = filename:join([OutDir, <<Username/binary,".xml">>]),
    case render(Module, Props) of
        {'ok', Result} ->
            file:write_file(XMLFile, Result);
        {'error', E} ->
            lager:debug("~s : error rendering template ~s for account ~s", [Number, Module, AccountId])
    end.

    

-spec query_registrar(ne_binary(), ne_binary()) -> {'ok', wh_json:object()}
                                                 | {'error', _}.
query_registrar(Realm, Username) ->
    FullUser = <<Username/binary, "@", Realm/binary>>,
    Req = [{<<"To">>, FullUser}
           ,{<<"From">>, FullUser}
           ,{<<"Auth-User">>, Username}
           ,{<<"Auth-Realm">>, Realm}
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

-spec template_file_name(atom()) -> string().
template_file_name(?FS_DIALPLAN) -> "dialplan_template.xml";
template_file_name(?FS_CHATPLAN) -> "chatplan_template.xml";
template_file_name(?FS_DIRECTORY) -> "directory_template.xml".


compile_templates() ->
    [ compile_template(wh_util:to_atom(T,'true'))    || T <- ?FS_TEMPLATES].
    
-spec compile_template(atom()) -> any().
compile_template(Module) ->
    compile_template(Module, whapps_config:get_binary(?MOD_CONFIG_CAT, wh_util:to_binary(Module))).
                
-spec compile_template(atom(), api_binary()) -> any().
compile_template(Module, 'undefined') ->
    {'ok', Contents} = file:read_file(template_file(Module)),
    whapps_config:set(?MOD_CONFIG_CAT, wh_util:to_binary(Module), Contents),
    compile_template(Module, Contents);
compile_template(Module, Template) ->
    {'ok', _R } = erlydtl:compile_template(Template, Module, [{'out_dir', 'false'}]).


render(Module, Props) ->
    try Module:render(props:filter_empty(Props)) of
        {'ok', Txt} = OK -> OK
    catch
        _T:_E -> {'error', _E}
    end.
    


    

%% should this go into wh_util ?
del_dir(Dir) ->
   lists:foreach(fun(D) ->
                    ok = file:del_dir(D)
                 end, del_all_files([Dir], [])).
del_all_files([], EmptyDirs) ->
   EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
   {ok, FilesInDir} = file:list_dir(Dir),
   {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                  Path = Dir ++ "/" ++ F,
                                  case filelib:is_dir(Path) of
                                     true ->
                                          {Fs, [Path | Ds]};
                                     false ->
                                          {[Path | Fs], Ds}
                                  end
                               end, {[],[]}, FilesInDir),
   lists:foreach(fun(F) ->
                         ok = file:delete(F)
                 end, Files),
   del_all_files(T ++ Dirs, [Dir | EmptyDirs]).

