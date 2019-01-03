%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_config_maint_listener).
-behaviour(gen_listener).

-export([start_link/0
        ,handle_req/2
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kazoo_maintenance.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(RESTRICTIONS, [kapi_maintenance:restrict_to_db(?KZ_CONFIG_DB)]).
-define(BINDINGS, [{'maintenance', [{'restrict_to', ?RESTRICTIONS}]}]).
-define(RESPONDERS, [{{?MODULE, 'handle_req'}
                     ,[{<<"maintenance">>, <<"req">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<?MODULE_STRING>>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ,{'declare_exchanges', [{?EXCHANGE_SYSCONF, ?TYPE_SYSCONF}]}
                            ]
                           ,[]
                           ).

-spec handle_req(kapi_maintenance:req(), kz_term:proplist()) -> 'ok'.
handle_req(MaintJObj, _Props) ->
    'true' = kapi_maintenance:req_v(MaintJObj),

    handle_refresh(MaintJObj, kz_json:get_value(<<"Database">>, MaintJObj)).

handle_refresh(MaintJObj, ?KZ_CONFIG_DB) ->
    Created = kz_datamgr:db_create(?KZ_CONFIG_DB),
    _ = kz_util:spawn(fun cleanup_invalid_notify_docs/0, []),
    _ = kz_util:spawn(fun delete_system_media_references/0, []),
    _ = kz_util:spawn(fun accounts_config_deprecate_timezone_for_default_timezone/0, []),
    send_resp(MaintJObj, Created).

-spec send_resp(kapi_mainteannce:req(), boolean()) -> 'ok'.
send_resp(MaintJObj, Created) ->
    Resp = [{<<"Code">>, code(Created)}
           ,{<<"Message">>, message(Created)}
           ,{<<"Msg-ID">>, kz_api:msg_id(MaintJObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_maintenance:publish_resp(kz_api:server_id(MaintJObj), Resp).

-spec code(boolean()) -> 200 | 500.
code('true') -> 200;
code('false') -> 500.

-spec message(boolean()) -> kz_term:ne_binary().
message('true') -> <<"Created ", (?KZ_CONFIG_DB)/binary>>;
message('false') -> <<"Did not create ", (?KZ_CONFIG_DB)/binary>>.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

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
-spec cleanup_invalid_notify_docs() -> 'ok'.
cleanup_invalid_notify_docs() ->
    _ = kz_datamgr:db_archive(<<"system_config">>),
    case kz_datamgr:all_docs(?KZ_CONFIG_DB, ['include_docs']) of
        {'ok', JObjs} -> cleanup_invalid_notify_docs(JObjs);
        {'error', _R} ->
            lager:warning("unable to fetch all system config docs: ~p", [_R])
    end.

-spec cleanup_invalid_notify_docs(kz_json:objects()) -> 'ok'.
cleanup_invalid_notify_docs([]) -> 'ok';
cleanup_invalid_notify_docs([JObj|JObjs]) ->
    Id = kz_json:get_value(<<"id">>, JObj),
    Doc = kz_json:get_value(<<"doc">>, JObj),
    _ = maybe_remove_invalid_notify_doc(kz_doc:type(Doc), Id, Doc),
    cleanup_invalid_notify_docs(JObjs).

-spec maybe_remove_invalid_notify_doc(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_remove_invalid_notify_doc(<<"notification">>, <<"notification", _/binary>>, _) -> 'ok';
maybe_remove_invalid_notify_doc(<<"notification">>, _, JObj) ->
    _ = kz_datamgr:del_doc(?KZ_CONFIG_DB, JObj),
    'ok';
maybe_remove_invalid_notify_doc(_Type, _Id, _Doc) -> 'ok'.


-spec delete_system_media_references() -> 'ok'.
delete_system_media_references() ->
    DocId = kz_call_response:config_doc_id(),
    case kz_datamgr:open_doc(?KZ_CONFIG_DB, DocId) of
        {'ok', CallResponsesDoc} ->
            delete_system_media_references(DocId, CallResponsesDoc);
        {'error', 'not_found'} -> 'ok'
    end.

-spec delete_system_media_references(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
delete_system_media_references(DocId, CallResponsesDoc) ->
    TheKey = <<"default">>,
    Default = kz_json:get_value(TheKey, CallResponsesDoc),

    case kz_json:map(fun remove_system_media_refs/2, Default) of
        Default -> 'ok';
        NewDefault ->
            io:format("updating ~s with stripped system_media references~n", [DocId]),
            NewCallResponsesDoc = kz_json:set_value(TheKey, NewDefault, CallResponsesDoc),
            _Resp = kz_datamgr:save_doc(?KZ_CONFIG_DB, NewCallResponsesDoc),
            'ok'
    end.

-spec remove_system_media_refs(kz_json:path(), kz_json:objects()) ->
                                      {kz_json:path(), kz_json:json_term()}.
remove_system_media_refs(HangupCause, Config) ->
    case kz_json:is_json_object(Config) of
        'false' -> {HangupCause, Config};
        'true' ->
            {HangupCause
            ,kz_json:foldl(fun remove_system_media_ref/3, kz_json:new(), Config)
            }
    end.

-spec remove_system_media_ref(kz_json:path(), kz_json:json_term(), kz_json:object()) ->
                                     kz_json:object().
remove_system_media_ref(Key, <<"/system_media/", Value/binary>>, Acc) -> kz_json:set_value(Key, Value, Acc);
remove_system_media_ref(Key, Value, Acc) -> kz_json:set_value(Key, Value, Acc).

%%------------------------------------------------------------------------------
%% @doc Remove system_config/accounts timezone key and use only
%% default_timezone
%% @end
%%------------------------------------------------------------------------------

-spec accounts_config_deprecate_timezone_for_default_timezone() -> 'ok'.
accounts_config_deprecate_timezone_for_default_timezone() ->
    case kz_datamgr:open_cache_doc(?KZ_CONFIG_DB, <<"accounts">>) of
        {'ok', AccountsConfig} ->
            accounts_config_deprecate_timezone_for_default_timezone(AccountsConfig);
        {'error', E} ->
            lager:warning("unable to fetch system_config/accounts: ~p", [E])
    end.

-spec accounts_config_deprecate_timezone_for_default_timezone(kz_json:object()) -> 'ok'.
accounts_config_deprecate_timezone_for_default_timezone(AccountsConfig) ->
    PublicFields = kz_doc:public_fields(AccountsConfig),
    case kz_json:get_keys(PublicFields) of
        [] -> 'ok';
        Keys ->
            MigratedConfig = deprecate_timezone_for_default_timezone(Keys, AccountsConfig),
            kz_datamgr:save_doc(?KZ_CONFIG_DB, MigratedConfig),
            'ok'
    end.

-spec deprecate_timezone_for_default_timezone(kz_json:keys(), kz_json:object()) ->
                                                     kz_json:object().
deprecate_timezone_for_default_timezone(Nodes, AccountsConfig) ->
    lists:foldl(fun deprecate_timezone_for_node/2, AccountsConfig, Nodes).

-spec deprecate_timezone_for_node(kz_json:key(), kz_json:object()) ->
                                         kz_json:object().
deprecate_timezone_for_node(Node, AccountsConfig) ->
    Timezone = kz_json:get_value([Node, <<"timezone">>], AccountsConfig),
    DefaultTimezone = kz_json:get_value([Node, <<"default_timezone">>], AccountsConfig),
    deprecate_timezone_for_node(Node, AccountsConfig, Timezone, DefaultTimezone).

-spec deprecate_timezone_for_node(kz_json:key(), kz_json:object(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) ->
                                         kz_json:object().
deprecate_timezone_for_node(_Node, AccountsConfig, 'undefined', _Default) ->
    AccountsConfig;
deprecate_timezone_for_node(Node, AccountsConfig, Timezone, 'undefined') ->
    io:format("setting default timezone to ~s for node ~s~n", [Timezone, Node]),
    kz_json:set_value([Node, <<"default_timezone">>]
                     ,Timezone
                     ,kz_json:delete_key([Node, <<"timezone">>], AccountsConfig)
                     );
deprecate_timezone_for_node(Node, AccountsConfig, _Timezone, _Default) ->
    kz_json:delete_key([Node, <<"timezone">>], AccountsConfig).
