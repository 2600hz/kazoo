%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(crossbar_maint_listener).
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

-include("crossbar.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(RESTRICTIONS, [kapi_maintenance:restrict_to_views_db(?KZ_CONFIG_DB)
                      ,kapi_maintenance:restrict_to_views_db(?KZ_SCHEMA_DB)
                      ,kapi_maintenance:restrict_to_views_db(?KZ_MEDIA_DB)
                      ,kapi_maintenance:restrict_to_views_db(?KZ_SIP_DB)
                      ,kapi_maintenance:restrict_to_views_db(?KZ_PORT_REQUESTS_DB)
                      ,kapi_maintenance:restrict_to_views_db(?KZ_ACDC_DB)
                      ,kapi_maintenance:restrict_to_views_db(?KZ_CCCPS_DB)
                      ,kapi_maintenance:restrict_to_views_db(?KZ_TOKEN_DB)
                      ,kapi_maintenance:restrict_to_views_db(?KZ_ALERTS_DB)
                      ,kapi_maintenance:restrict_to_views_db(?KZ_PENDING_NOTIFY_DB)
                      ,kapi_maintenance:restrict_to_views_db(?KZ_WEBHOOKS_DB)
                      ,kapi_maintenance:restrict_to_views_db(?KZ_OFFNET_DB)

                      ,kapi_maintenance:restrict_to_views_classification('ratedeck')
                      ]).
-define(BINDINGS, [{'maintenance', [{'restrict_to', ?RESTRICTIONS}]}]).
-define(RESPONDERS, [{{?MODULE, 'handle_req'}
                     ,[{<<"maintenance">>, <<"req">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<?MODULE_STRING>>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
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

-spec handle_req(kapi_maintenance:req(), kz_proplist()) -> 'ok'.
handle_req(MaintJObj, _Props) ->
    'true' = kapi_maintenance:req_v(MaintJObj),

    handle_refresh(MaintJObj
                  ,kapi_maintenance:req_action(MaintJObj)
                  ,kapi_maintenance:req_database(MaintJObj)
                  ,kapi_maintenance:req_classification(MaintJObj)
                  ).

-spec handle_refresh(kapi_maintenance:req(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_CONFIG_DB, _Class) ->
    Revised = kz_datamgr:revise_doc_from_file(?KZ_CONFIG_DB, 'crossbar', <<"views/system_configs.json">>),
    send_resp(MaintJObj, Revised);
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_SCHEMA_DB, _Class) ->
    kz_datamgr:suppress_change_notice(),
    Revised = kz_datamgr:revise_docs_from_folder(?KZ_SCHEMA_DB, 'crossbar', "schemas"),
    kz_datamgr:enable_change_notice(),
    send_resp(MaintJObj, Revised);
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_MEDIA_DB, _Class) ->
    Revised = kz_datamgr:revise_doc_from_file(?KZ_MEDIA_DB, 'crossbar', "account/media.json"),
    send_resp(MaintJObj, Revised);
handle_refresh(MaintJObj, <<"refresh_views">>, Database, <<"ratedeck">>) ->
    Revised = kz_datamgr:revise_doc_from_file(Database, 'crossbar', <<"views/rates.json">>),
    send_resp(MaintJObj, Revised);
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_SIP_DB, _Class) ->
    View = kapps_util:get_view_json('crossbar', <<"views/resources.json">>),
    case kapps_util:update_views(?KZ_SIP_DB, [View], 'true') of
        'true' -> send_resp(MaintJObj, {'ok', MaintJObj});
        'false' -> send_resp(MaintJObj, {'error', 'not_found'})
    end;
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_PORT_REQUESTS_DB, _Class) ->
    Revised = kz_datamgr:revise_doc_from_file(?KZ_PORT_REQUESTS_DB, 'crossbar', <<"views/port_requests.json">>),
    send_resp(MaintJObj, Revised);
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_ACDC_DB, _Class) ->
    Revised = kz_datamgr:revise_doc_from_file(?KZ_ACDC_DB, 'crossbar', <<"views/acdc.json">>),
    send_resp(MaintJObj, Revised);
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_CCCPS_DB, _Class) ->
    Revised = kz_datamgr:revise_doc_from_file(?KZ_CCCPS_DB, 'crossbar', <<"views/cccps.json">>),
    send_resp(MaintJObj, Revised);
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_TOKEN_DB, _Class) ->
    Revised = kz_datamgr:revise_doc_from_file(?KZ_TOKEN_DB, 'crossbar', "views/token_auth.json"),
    send_resp(MaintJObj, Revised);
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_ALERTS_DB, _Class) ->
    Revised = kz_datamgr:revise_doc_from_file(?KZ_ALERTS_DB, 'crossbar', "views/alerts.json"),
    send_resp(MaintJObj, Revised);
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_PENDING_NOTIFY_DB, _Class) ->
    Revised = kz_datamgr:revise_doc_from_file(?KZ_PENDING_NOTIFY_DB, 'crossbar', "views/pending_notify.json"),
    send_resp(MaintJObj, Revised);
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_WEBHOOKS_DB, _Class) ->
    Revised = kz_datamgr:revise_doc_from_file(?KZ_WEBHOOKS_DB, 'crossbar', <<"views/webhooks.json">>),
    send_resp(MaintJObj, Revised);
handle_refresh(MaintJObj, <<"refresh_views">>, ?KZ_OFFNET_DB, _Class) ->
    View = kapps_util:get_view_json('crossbar', <<"views/resources.json">>),
    kapps_util:update_views(?KZ_OFFNET_DB, [View], 'true'),
    send_resp(MaintJObj, 'true').

-type results() :: {'ok', kz_json:object()} |
                   kz_datamgr:data_error() |
                   boolean() | 'ok'.

-spec send_resp(kapi_mainteannce:req(), results()) -> 'ok'.
send_resp(MaintJObj, Revised) ->
    Resp = [{<<"Code">>, code(Revised)}
           ,{<<"Message">>, message(Revised)}
           ,{<<"Msg-ID">>, kz_api:msg_id(MaintJObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_maintenance:publish_resp(kz_api:server_id(MaintJObj), Resp).

-spec code(results()) -> 200 | 500.
code({'ok', _}) -> 200;
code('true') -> 200;
code('ok') -> 200;
code({'error', _}) -> 500.

-spec message(results()) -> ne_binary().
message({'ok', _}) -> <<"Revised crossbar docs/views">>;
message('ok') -> <<"Revised crossbar docs/views">>;
message({'error', E}) ->
    <<"Failed to revise docs/views: ", (kz_term:to_binary(E))/binary>>;
message('true') -> <<"Created database">>.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%%--------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Allows listener to pass options to handlers
%% @spec handle_event(JObj, State) -> {reply, Options}
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%% @spec terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
