%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_local_extension).

-behaviour(gen_listener).

-export([start_link/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("stepswitch.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").

-record(state, {number_props
                ,resource_req
                ,request_handler
                ,control_queue
                ,response_queue
                ,queue
                ,timeout}).

-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

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
start_link(Props, JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Bindings = [{'call', [{'callid', CallId}
                          ,{'restrict_to', ['events'
                                            ,'destroy_channel'
                                           ]}
                         ]}
                ,{'self', []}
               ],
    gen_listener:start_link(?MODULE, [{'bindings', Bindings}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [Props, JObj]).

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
init([Props, JObj]) ->
    wh_util:put_callid(JObj),
    case wh_json:get_ne_value(<<"Control-Queue">>, JObj) of
        'undefined' -> {'stop', 'normal'};
        ControlQ ->
            {'ok', #state{number_props=Props
                          ,resource_req=JObj
                          ,request_handler=self()
                          ,control_queue=ControlQ
                          ,response_queue=wh_json:get_ne_value(<<"Server-ID">>, JObj)
                          ,timeout=erlang:send_after(120000, self(), 'local_extension_timeout')
                         }}
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
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call: ~p", [_Request]),
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
handle_cast({'wh_amqp_channel', _}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    {'noreply', State#state{queue=Q}};
handle_cast({'gen_listener', {'is_consuming', 'true'}}, #state{control_queue=ControlQ}=State) ->
    'ok' = wapi_dialplan:publish_command(ControlQ, build_local_extension(State)),
    lager:debug("sent local extension command to ~s", [ControlQ]),
    {'noreply', State};
handle_cast({'local_extension_result', _Props}, #state{response_queue='undefined'}=State) ->
    {'stop', 'normal', State};
handle_cast({'local_extension_result', Props}, #state{response_queue=ResponseQ}=State) ->
    wapi_offnet_resource:publish_resp(ResponseQ, Props),
    {'stop', 'normal', State};
handle_cast({'bridged', CallId}, #state{timeout='undefined'}=State) ->
    lager:debug("channel bridged to ~s", [CallId]),
    {'noreply', State};
handle_cast({'bridged', CallId}, #state{timeout=TimerRef}=State) ->
    lager:debug("channel bridged to ~s, canceling timeout", [CallId]),
    _ = erlang:cancel_timer(TimerRef),
    {'noreply', State#state{timeout='undefined'}};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p~n", [_Msg]),
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
handle_info('local_extension_timeout', #state{timeout='undefined'}=State) ->
    {'noreply', State};
handle_info('local_extension_timeout', #state{response_queue=ResponseQ
                                              ,resource_req=JObj}=State) ->
    wapi_offnet_resource:publish_resp(ResponseQ, local_extension_timeout(JObj)),
    {'stop', 'normal', State#state{timeout='undefined'}};
handle_info(_Info, State) ->
    lager:debug("unhandled info: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(JObj, #state{request_handler=RequestHandler, resource_req=Request}) ->
    case whapps_util:get_event_type(JObj) of
        {<<"error">>, _} ->
            <<"execute_extension">> = wh_json:get_value([<<"Request">>, <<"Application-Name">>], JObj),
            lager:debug("channel execution error while waiting for execute extension: ~s"
                        ,[wh_util:to_binary(wh_json:encode(JObj))]),
            gen_listener:cast(RequestHandler, {'local_extension_result', local_extension_error(JObj, Request)});
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            lager:debug("channel was destroy while waiting for execute extension", []),
            gen_listener:cast(RequestHandler, {'local_extension_result', local_extension_success(Request)});
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>} ->
            <<"execute_extension">> = wh_json:get_value(<<"Application-Name">>, JObj),
            lager:debug("channel execute complete for execute extension", []),
            gen_listener:cast(RequestHandler, {'local_extension_result', local_extension_success(Request)});
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>} ->
            CallId = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
            gen_listener:cast(RequestHandler, {'bridged', CallId});
        _ -> 'ok'
    end,
    {'reply', []}.

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
    lager:debug("listener terminating: ~p", [_Reason]).

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
build_local_extension(#state{number_props=Props, resource_req=JObj, queue=Q}) ->
    {CIDNum, CIDName} = local_extension_caller_id(JObj),
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
    Number = props:get_value('number', Props),
    AccountId = props:get_value('account_id', Props),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    CCVUpdates = props:filter_undefined(
                   [{<<"Inception">>, <<"off-net">>}
                    ,{<<"Retain-CID">>, <<"true">>}
                    ,{<<"Global-Resource">>, <<"false">>}
                    ,{<<"Account-ID">>, AccountId}
                    ,{<<"Caller-ID-Number">>, CIDNum}
                    ,{<<"Caller-ID-Name">>, CIDName}
                    ,{<<"Callee-ID-Number">>, wh_util:to_binary(Number)}
                    ,{<<"Callee-ID-Name">>, get_account_name(Number, AccountId)}
                    ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
                   ]),    
    [{<<"Application-Name">>, <<"execute_extension">>}
     ,{<<"Reset">>, <<"true">>}
     ,{<<"Extension">>, Number}
     ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     ,{<<"Custom-Channel-Vars">>, wh_json:set_values(CCVUpdates, CCVs)}
     ,{<<"Fax-Identity-Number">>, wh_json:get_value(<<"Fax-Identity-Number">>, JObj, CIDNum)}
     ,{<<"Fax-Identity-Name">>, wh_json:get_value(<<"Fax-Identity-Name">>, JObj, CIDName)}
     | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
    ].

-spec get_account_name(ne_binary(), ne_binary()) -> ne_binary().
get_account_name(Number, AccountId) when is_binary(Number) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} -> wh_json:get_ne_value(<<"name">>, JObj, Number);
        _ -> Number
    end.

-spec local_extension_caller_id(wh_json:object()) -> {api_binary(), api_binary()}.
local_extension_caller_id(JObj) ->
    {wh_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                                ,<<"Emergency-Caller-ID-Number">>
                               ], JObj)
     ,wh_json:get_first_defined([<<"Outbound-Caller-ID-Name">>
                                 ,<<"Emergency-Caller-ID-Name">>
                                ], JObj)
    }.

-spec local_extension_timeout(wh_json:object()) -> wh_proplist().
local_extension_timeout(Request) ->
    lager:debug("attempt to connect to resources timed out"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, <<"local extension request timed out">>}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, Request)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec local_extension_error(wh_json:object(), wh_json:object()) -> wh_proplist().
local_extension_error(JObj, Request) ->
    lager:debug("error during outbound request: ~s", [wh_util:to_binary(wh_json:encode(JObj))]),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, wh_json:get_value(<<"Error-Message">>, JObj, <<"failed to process request">>)}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, Request)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec local_extension_success(wh_json:object()) -> wh_proplist().
local_extension_success(Request) ->
    lager:debug("local extension request successfully completed"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"SUCCESS">>}
     ,{<<"Response-Code">>, <<"sip:200">>}
     ,{<<"Resource-Response">>, wh_json:new()}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
