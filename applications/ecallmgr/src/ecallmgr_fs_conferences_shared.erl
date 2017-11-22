-module(ecallmgr_fs_conferences_shared).

-behaviour(gen_listener).

-export([start_link/0
        ,handle_dial_req/2
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(RESPONDERS, [{{?MODULE, 'handle_dial_req'}
                     ,[{<<"conference">>, <<"command">>}]
                     }
                    ]).
-define(BINDINGS, [{'conference', [{'restrict_to', [{'command', <<"*">>}]}
                                  ,'federate'
                                  ]}
                  ]).
-define(QUEUE_NAME, <<?MODULE_STRING>>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-type state() :: 'ok'.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?MODULE}, ?MODULE,
                            [{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    {'ok', 'ok'}.

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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Req, _From, State) ->
    lager:debug("unhandled call from ~p: ~p", [_From, _Req]),
    {'noreply', State}.

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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(_Msg, State) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("conferences listener going down: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) -> {'ok', State}.

-spec handle_dial_req(kapi_conference:doc(), kz_proplist()) -> 'ok'.
handle_dial_req(JObj, _Props) ->
    'true' = kapi_conference:dial_v(JObj),
    ConferenceId = kz_json:get_ne_binary_value(<<"Conference-ID">>, JObj),
    case node(ConferenceId) of
        {'error', 'not_found'} ->
            maybe_start_conference(JObj, ConferenceId);
        {'ok', ConferenceNode} ->
            exec_dial(ConferenceNode, ConferenceId, JObj)
    end.

-spec exec_dial(atom(), ne_binary(), kapi_conference:doc()) -> 'ok'.
exec_dial(ConferenceNode, ConferenceId, JObj) ->
    lager:info("conference ~s is running on ~s, dialing out", [ConferenceId, ConferenceNode]),
    try ecallmgr_conference_command:exec_cmd(ConferenceNode, ConferenceId, JObj) of
        {'ok', Resp} ->
            lager:info("starting dial resulted in ~s", [Resp]),
            send_success_resp(JObj, Resp);
        _E ->
            lager:info("failed to exec: ~p", [_E]),
            send_error_resp(JObj, <<"unknown failure">>)
    catch
        'throw':{'msg', E} ->
            send_error_resp(JObj, E)
    end.

-spec send_success_resp(kapi_conference:doc(), ne_binary()) -> 'ok'.
send_success_resp(JObj, Resp) ->
    JobId =
        case re:run(Resp, <<"([\\w-]{36})">>, ['ungreedy', {'capture', 'all_but_first', 'binary'}]) of
            {'match', [UUID|_]} -> UUID;
            _ -> 'undefined'
        end,

    publish_resp(JObj, [{<<"Job-ID">>, JobId}
                       ,{<<"Message">>, <<"dialing endpoints">>}
                       ,{<<"Status">>, <<"success">>}
                       ]).

-spec send_error_resp(kapi_conference:doc(), ne_binary()) -> 'ok'.
send_error_resp(JObj, Error) ->
    publish_resp(JObj, [{<<"Status">>, <<"error">>}
                       ,{<<"Message">>, Error}
                       ]).

-spec publish_resp(kapi_conference:doc(), kz_proplist()) -> 'ok'.
publish_resp(JObj, BaseResp) ->
    Resp = BaseResp
        ++ [{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kz_amqp_worker:cast(Resp, fun(P) -> kapi_conference:publish_dial_resp(kz_api:server_id(JObj), P) end).

-spec maybe_start_conference(kapi_conference:doc(), ne_binary()) -> 'ok'.
maybe_start_conference(JObj, ConferenceId) ->
    lager:info("conference ~s is not running yet", [ConferenceId]),
    case find_media_server(kz_json:get_ne_binary_value(<<"Target-Call-ID">>, JObj), kz_api:node(JObj)) of
        'undefined' -> lager:info("no node found for the dial command, ignoring");
        MediaServer ->
            lager:info("starting conference ~s on ~s and dialing out", [ConferenceId, MediaServer]),
            exec_dial(MediaServer, ConferenceId, JObj)
    end.

-spec find_media_server(api_ne_binary(), ne_binary()) -> atom().
find_media_server('undefined', IssuerNode) ->
    IssuerNodeInfo = kz_nodes:node_to_json(IssuerNode),
    MyZone = kz_config:zone('binary'),

    case kz_json:get_ne_binary_value(<<"zone">>, IssuerNodeInfo) of
        MyZone -> choose_random_media_server();
        _IssuerZone ->
            lager:info("issuer ~s is in zone ~s, ignoring request", [IssuerNode, _IssuerZone]),
            'undefined'
    end;
find_media_server(TargetCallId, IssuerNode) ->
    case ecallmgr_fs_channel:node(TargetCallId) of
        {'ok', Node} -> Node;
        {'error', 'not_found'} ->
            lager:info("failed to find node of target call-id ~s, querying cluster", [TargetCallId]),
            case query_cluster_for_call(TargetCallId) of
                {'ok', StatusJObjs} ->
                    find_media_server_from_statuses(TargetCallId, IssuerNode, StatusJObjs);
                _E ->
                    lager:info("failed to query for ~s: ~p", [TargetCallId, _E]),
                    find_media_server('undefined', IssuerNode)
            end
    end.

-spec find_media_server_from_statuses(ne_binary(), ne_binary(), kz_json:objects()) -> atom().
find_media_server_from_statuses(TargetCallId, IssuerNode, []) ->
    lager:info("no one has record of ~s", [TargetCallId]),
    find_media_server('undefined', IssuerNode);
find_media_server_from_statuses(TargetCallId, IssuerNode, [Status|Statuses]) ->
    case kz_json:get_ne_binary_value([<<"Channels">>, TargetCallId, <<"Media-Node">>], Status) of
        'undefined' -> find_media_server_from_statuses(TargetCallId, IssuerNode, Statuses);
        MediaServer ->
            lager:info("found ~s on ~s", [TargetCallId, MediaServer]),
            case lists:filter(fun(MS) -> kz_term:to_binary(MS) =:= MediaServer end
                             ,ecallmgr_fs_nodes:connected()
                             )
            of
                [] ->
                    lager:info("media server ~s is not managed by us, not starting conference"
                              ,[MediaServer]
                              ),
                    'undefined';
                [MS] ->
                    lager:info("media server ~s is managed by us!", [MediaServer]),
                    MS
            end
    end.

-spec query_cluster_for_call(ne_binary()) -> {'ok', kz_json:objects()} |
                                             {'error', any()}.
query_cluster_for_call(CallId) ->
    Req = [{<<"Call-ID">>, CallId}
          ,{<<"Fields">>, <<"all">>}
          ,{<<"Active-Only">>, 'true'}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    kz_amqp_worker:call_collect(Req
                               ,fun kapi_call:publish_query_channels_req/1
                               ,{'ecallmgr', fun kapi_call:query_channels_resp_v/1}
                               ).

-spec choose_random_media_server() -> atom().
choose_random_media_server() ->
    [Server|_] = kz_term:shuffle_list(ecallmgr_fs_nodes:connected()),
    Server.
