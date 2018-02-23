%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_service_sync).
-behaviour(gen_listener).

-export([start_link/0
        ,maintenance_req/2
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("tasks.hrl").
-include_lib("kazoo_services/include/kazoo_services.hrl").
-include_lib("kazoo_transactions/include/kazoo_transactions.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").

-define(SERVER, ?MODULE).
-define(SCAN_MSG, {'try_sync_service'}).

-record(state, {}).
-type state() :: #state{}.

-define(SHOULD_SYNC_SERVICES
       ,kapps_config:get_is_true(?CONFIG_CAT, <<"sync_services">>, 'false')
       ).

-define(SCAN_RATE
       ,kapps_config:get_non_neg_integer(?CONFIG_CAT, <<"scan_rate">>, 20 * ?MILLISECONDS_IN_SECOND)
       ).

-define(RESTRICTIONS, [kapi_maintenance:restrict_to_clean_services(<<"*">>)]).
-define(BINDINGS, [{'maintenance', [{'restrict_to', ?RESTRICTIONS}]}]).
-define(RESPONDERS, [{{?MODULE, 'maintenance_req'}
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

-spec maintenance_req(kapi_maintenance:req(), kz_term:proplist()) -> 'ok'.
maintenance_req(MaintJObj, _Props) ->
    'true' = kapi_maintenance:req_v(MaintJObj),
    process_maintenance_req(MaintJObj, kapi_maintenance:req_action(MaintJObj)).

process_maintenance_req(MaintJObj, <<"clean_services">>) ->
    AccountId = kapi_maintenance:req_database(MaintJObj),
    _ = kz_services:clean(AccountId),
    send_resp(MaintJObj, 'true').

-spec send_resp(kapi_mainteannce:req(), 'true') -> 'ok'.
send_resp(MaintJObj, Results) ->
    Resp = [{<<"Code">>, code(Results)}
           ,{<<"Message">>, message(Results)}
           ,{<<"Msg-ID">>, kz_api:msg_id(MaintJObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_maintenance:publish_resp(kz_api:server_id(MaintJObj), Resp).

code('true') -> 200.

message('true') -> <<"Success">>.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', #state{}}.
init([]) ->
    io:format("getting ~s sync_services~n", [?CONFIG_CAT]),
    lager:debug("getting ~s sync_services", [?CONFIG_CAT]),

    maybe_migrate_config(),

    case ?SHOULD_SYNC_SERVICES of
        'false' ->
            io:format("not starting sync services~n"),
            lager:debug("not starting sync services"),
            {'ok', #state{}};
        'true' ->
            _Ref = start_sync_service_timer(),
            io:format("started sync services ~p\n", [_Ref]),
            lager:debug("started sync services ~p", [_Ref]),
            {'ok', #state{}}
    end.

maybe_migrate_config() ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"sync_services">>) of
        'undefined' -> migrate_config();
        _ -> 'ok'
    end.

migrate_config() ->
    lager:info("migrating configs from services to tasks"),
    Keys = [<<"scan_rate">>, <<"sync_buffer_period">>, <<"sync_services">>],
    lists:foreach(fun migrate_config/1, Keys).

migrate_config(Key) ->
    case kapps_config:get(<<"services">>, Key) of
        'undefined' -> 'ok';
        Value ->
            lager:info("migrating ~s: ~p", [Key, Value]),
            kapps_config:set(?CONFIG_CAT, Key, Value)
    end.

-spec start_sync_service_timer() -> reference().
start_sync_service_timer() ->
    erlang:send_after(?SCAN_RATE, self(), ?SCAN_MSG).

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
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(?SCAN_MSG, State) ->
    _ = maybe_sync_service(),
    _ = maybe_clear_process_dictionary(),
    _Ref = start_sync_service_timer(),
    {'noreply', State, 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

-spec maybe_clear_process_dictionary() -> 'ok'.
maybe_clear_process_dictionary() ->
    lists:foreach(fun maybe_clear_dictionary_entry/1, get()).

-spec maybe_clear_dictionary_entry({any(), any()}) -> any().
maybe_clear_dictionary_entry({{'phone_number_doc', _AccountId}=Key, _Doc}) ->
    erase(Key);
maybe_clear_dictionary_entry(_) -> 'ok'.

-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("kazoo service sync terminating: ~p", [_Reason]).

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

-define(SYNC_BUFFER_PERIOD
       ,kapps_config:get_non_neg_integer(?CONFIG_CAT, <<"sync_buffer_period">>, 600)
       ).

-spec maybe_sync_service() -> kz_term:std_return().
maybe_sync_service() ->
    ViewOptions = [{'limit', 1}
                  ,'include_docs'
                  ,{'endkey', kz_time:now_s() - ?SYNC_BUFFER_PERIOD}
                  ],
    case kz_datamgr:get_results(?KZ_SERVICES_DB, <<"services/dirty">>, ViewOptions) of
        {'error', _}=E -> E;
        {'ok', [JObj]} -> bump_modified(kz_json:get_value(<<"doc">>, JObj));
        {'ok', _} -> {'error', 'no_dirty_services'}
    end.

-spec bump_modified(kz_json:object()) -> kz_term:std_return().
bump_modified(JObj) ->
    AccountId = kz_doc:account_id(JObj),
    Services = kz_services:reconcile_only(AccountId),
    'true' = (Services =/= 'false'),

    Values = [{?SERVICES_PVT_MODIFIED, kz_time:now_s()}
             ,{?SERVICES_PVT_REV, kz_doc:revision(JObj)}
             ],
    UpdatedServicesJObj = kz_json:set_values(Values, kz_services:to_json(Services)),
    case kz_datamgr:save_doc(?KZ_SERVICES_DB, UpdatedServicesJObj) of
        {'error', _}=E ->
            %% If we conflict or cant save the doc with a new modified timestamp
            %% then another process is probably handling it, move on
            E;
        {'ok', ServicesJObj} ->
            %% If we can change the timestamp then (since the view requires the
            %% modified time to be x mins in the past) we have gain exclusive
            %% control for x mins.... good luck!
            [RevNum, _] = binary:split(kz_doc:revision(ServicesJObj), <<"-">>),
            kz_util:put_callid(<<AccountId/binary, "-", RevNum/binary>>),
            lager:debug("start synchronization of services with bookkeepers"),
            maybe_follow_sync_billing_id(AccountId, ServicesJObj)
    end.

-spec maybe_follow_sync_billing_id(kz_term:ne_binary(), kzd_services:doc()) -> kz_term:std_return().
maybe_follow_sync_billing_id(AccountId, ServicesJObj) ->
    case kz_services:get_billing_id(AccountId, ServicesJObj) of
        AccountId -> kz_services:sync(AccountId, ServicesJObj);
        BillingId -> follow_sync_billing_id(BillingId, AccountId, ServicesJObj)
    end.

-spec follow_sync_billing_id(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:std_return().
follow_sync_billing_id(BillingId, AccountId, ServicesJObj) ->
    %% NOTE: First try to make the parent (to be billed) as dirty
    %%  if that is successful then mark the current service doc cleans
    case kz_services:mark_dirty(BillingId) of
        {'ok', _} ->
            lager:debug("following billing id ~s", [BillingId]),
            mark_clean(ServicesJObj);
        {'error', 'not_found'} ->
            maybe_update_billing_id(BillingId, AccountId, ServicesJObj);
        {'error', _R}=E ->
            lager:debug("unable to mark billing services ~s dirty: ~p", [BillingId, _R]),
            E
    end.

-spec mark_clean(kzd_services:doc()) -> kz_term:std_return().
mark_clean(ServicesJObj) ->
    kz_datamgr:save_doc(?KZ_SERVICES_DB, kzd_services:set_is_dirty(ServicesJObj, 'false')).


-spec maybe_update_billing_id(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:std_return().
maybe_update_billing_id(BillingId, AccountId, ServicesJObj) ->
    case kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, BillingId) of
        {'error', _} ->
            lager:debug("billing id ~s on ~s does not exist anymore, updating to bill self"
                       ,[BillingId, AccountId]
                       ),
            kz_datamgr:save_doc(?KZ_SERVICES_DB, kzd_services:set_billing_id(ServicesJObj, AccountId));
        {'ok', JObj} ->
            case kz_doc:is_soft_deleted(JObj) of
                'false' -> kz_services:reconcile(BillingId);
                'true' ->
                    lager:debug("billing id ~s on ~s was deleted, updating to bill self"
                               ,[BillingId, AccountId]
                               ),
                    kz_datamgr:save_doc(?KZ_SERVICES_DB, kzd_services:set_billing_id(ServicesJObj, AccountId))
            end
    end.
