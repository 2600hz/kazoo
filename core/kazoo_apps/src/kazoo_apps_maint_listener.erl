%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_apps_maint_listener).
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

-include("kazoo_apps.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
%% define what databases or classifications we're interested in
-define(RESTRICTIONS, [kapi_maintenance:restrict_to_db(?KZ_ACCOUNTS_DB)
                      ,kapi_maintenance:restrict_to_db(?KZ_DEDICATED_IP_DB)
                      ,kapi_maintenance:restrict_to_db(?KZ_TOKEN_DB)
                      ,kapi_maintenance:restrict_to_db(?KZ_ALERTS_DB)
                      ,kapi_maintenance:restrict_to_db(?KZ_PENDING_NOTIFY_DB)
                      ,kapi_maintenance:restrict_to_classification('system')

                      ,kapi_maintenance:restrict_to_views_db(?KZ_SIP_DB)
                      ,kapi_maintenance:restrict_to_views_db(?KZ_ACCOUNTS_DB)
                      ,kapi_maintenance:restrict_to_views_db(?KZ_DEDICATED_IP_DB)

                      ,kapi_maintenance:restrict_to_views_classification('modb')
                      ,kapi_maintenance:restrict_to_views_classification('account')
                      ]).
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
    handle_refresh(MaintJObj
                  ,kapi_maintenance:req_action(MaintJObj)
                  ,kapi_maintenance:req_database(MaintJObj)
                  ,kapi_maintenance:req_classification(MaintJObj)
                  ).

handle_refresh(MaintJObj, <<"refresh_views">>, Database, <<"modb">>) ->
    kazoo_modb:refresh_views(Database),
    send_resp(MaintJObj, 'true');
handle_refresh(MaintJObj, <<"refresh_views">>, Database, <<"account">>) ->
    refresh_account_db(Database),
    send_resp(MaintJObj, 'true');
handle_refresh(MaintJObj, <<"refresh_database">>, Database, _Class) ->
    refresh_database(MaintJObj, Database);
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_SIP_DB, _Class) ->
    Views = [kapps_util:get_view_json('kazoo_apps', ?MAINTENANCE_VIEW_FILE)],
    Updated = kapps_util:update_views(?KZ_SIP_DB, Views, 'true'),
    send_resp(MaintJObj, Updated);
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_ACCOUNTS_DB, _Class) ->
    Views = [kapps_util:get_view_json('kazoo_apps', ?MAINTENANCE_VIEW_FILE)
            ,kapps_util:get_view_json('kazoo_apps', ?ACCOUNTS_AGG_VIEW_FILE)
            ,kapps_util:get_view_json('kazoo_apps', ?SEARCH_VIEW_FILE)
            ],
    Updated = kapps_util:update_views(?KZ_ACCOUNTS_DB, Views, 'true'),
    send_resp(MaintJObj, Updated);
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_DEDICATED_IP_DB, _Class) ->
    Revised = kz_datamgr:revise_docs_from_folder(?KZ_DEDICATED_IP_DB
                                                ,'kazoo_ips'
                                                ,"views"
                                                ),
    send_resp(MaintJObj, Revised).

refresh_database(MaintJObj, Database) ->
    Created = kz_datamgr:db_create(Database),
    send_resp(MaintJObj, Created).

-type results() :: boolean() |
                   'ok'.

-spec send_resp(kapi_mainteannce:req(), results()) -> 'ok'.
send_resp(MaintJObj, Created) ->
    Resp = [{<<"Code">>, code(Created)}
           ,{<<"Message">>, message(Created)}
           ,{<<"Msg-ID">>, kz_api:msg_id(MaintJObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_maintenance:publish_resp(kz_api:server_id(MaintJObj), Resp).

code('true') -> 200;
code('ok') -> 200;
code('false') -> 500.

message('true') -> <<"Success">>;
message('ok') -> <<"Success">>;
message('false') -> <<"Failed">>.

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
-define(VIEW_NUMBERS_ACCOUNT, <<"_design/numbers">>).

-spec refresh_account_db(kz_term:ne_binary()) -> 'ok'.
refresh_account_db(Database) ->
    AccountDb = kz_util:format_account_id(Database, 'encoded'),
    AccountId = kz_util:format_account_id(Database, 'raw'),
    _ = remove_depreciated_account_views(AccountDb),
    _ = ensure_account_definition(AccountDb, AccountId),
    %% ?VIEW_NUMBERS_ACCOUNT gets updated/created in KNM maintenance
    AccountViews =
        case kz_datamgr:open_doc(AccountDb, ?VIEW_NUMBERS_ACCOUNT) of
            {error,_} ->
                lists:keydelete(?VIEW_NUMBERS_ACCOUNT, 1, kapps_maintenance:get_all_account_views());
            {ok, ViewJObj} ->
                ViewListing = {?VIEW_NUMBERS_ACCOUNT, ViewJObj},
                lists:keyreplace(?VIEW_NUMBERS_ACCOUNT, 1, kapps_maintenance:get_all_account_views(), ViewListing)
        end,
    _ = kapps_util:update_views(AccountDb, AccountViews, 'true'),
    _ = kazoo_number_manager_maintenance:update_number_services_view(AccountDb),
    kapps_account_config:migrate(AccountDb),
    _ = kazoo_bindings:map(kapps_maintenance:binding({'refresh_account', AccountDb}), AccountId),
    'ok'.

-spec remove_depreciated_account_views(kz_term:ne_binary()) -> 'ok'.
remove_depreciated_account_views(AccountDb) ->
    _ = kz_datamgr:del_doc(AccountDb, <<"_design/limits">>),
    _ = kz_datamgr:del_doc(AccountDb, <<"_design/sub_account_reps">>),
    'ok'.

-spec ensure_account_definition(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
ensure_account_definition(AccountDb, AccountId) ->
    case kz_datamgr:open_doc(AccountDb, AccountId) of
        {'error', 'not_found'} -> get_definition_from_accounts(AccountDb, AccountId);
        {'ok', _} -> 'ok'
    end.

-spec get_definition_from_accounts(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
get_definition_from_accounts(AccountDb, AccountId) ->
    case kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} -> kz_datamgr:ensure_saved(AccountDb, kz_doc:delete_revision(JObj));
        {'error', 'not_found'} ->
            io:format("    account ~s is missing its local account definition, and not in the accounts db~n"
                     ,[AccountId]),
            _ = kz_datamgr:db_archive(AccountDb),
            kapps_maintenance:maybe_delete_db(AccountDb)
    end.
