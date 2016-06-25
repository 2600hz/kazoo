%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(pusher_listener).

-behaviour(gen_listener).

-export([start_link/0
         ,handle_push/2
         ,handle_reg_success/2
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include_lib("pusher/src/pusher.hrl").
-include_lib("kazoo_sip/include/kzsip_uri.hrl").

-define(SERVER, ?MODULE).

-record(state, {subs_pid :: pid()
                ,subs_ref :: reference()
               }).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'self', []}
                   ,{'pusher', []}
                   ,{'registration', [{'restrict_to',['reg_success']}]}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_push'}
                      ,[{<<"notification">>, <<"push_req">>}]
                     }
                     ,{{?MODULE, 'handle_reg_success'}
                       ,[{<<"directory">>, <<"reg_success">>}]
                      }
                    ]).

-define(QUEUE_NAME, <<"pusher_shared_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%===================================================================
%%% API
%%%===================================================================

-spec handle_push(kz_json:object(), kz_proplist()) -> 'ok'.
handle_push(JObj, _Props) ->
    Token = kz_json:get_value(<<"Token-ID">>, JObj),
    TokenType = kz_json:get_value(<<"Token-Type">>, JObj),
    Module = kz_util:to_atom(<<"pm_",TokenType/binary>> , 'true'),

    lager:debug("pushing for token ~s(~s) to module ~s", [Token, TokenType, Module]),

    kz_cache:store_local(?CACHE_NAME
                         ,Token
                         ,JObj
                         ,[{'expires', 20}]
                        ),
    gen_server:cast(Module, {'push', JObj}).

-spec handle_reg_success(kz_json:object(), kz_proplist()) -> 'ok'.
handle_reg_success(JObj, _Props) ->
    UserAgent = kz_json:get_value(<<"User-Agent">>, JObj),
    UserAgentProperties = pusher_util:user_agent_push_properties(UserAgent),

    maybe_process_reg_success(UserAgentProperties, JObj).

-spec maybe_process_reg_success(api_object(), kz_json:object()) -> 'ok'.
-spec maybe_process_reg_success(api_binary(), kz_json:object(), kz_json:object(), kz_proplist()) -> 'ok'.
maybe_process_reg_success('undefined', _JObj) -> 'ok';
maybe_process_reg_success(UA, JObj) ->
    OriginalContact = kz_json:get_value(<<"Original-Contact">>, JObj),

    [#uri{opts=A, ext_opts=B}] = kzsip_uri:uris(OriginalContact),
    Params = A ++ B,

    TokenKey = kz_json:get_value(?TOKEN_KEY, UA),
    Token = props:get_value(TokenKey, Params),
    maybe_process_reg_success(Token, kz_json:set_value(<<"Token-Proxy">>, ?TOKEN_PROXY_KEY, UA) , JObj, Params).

maybe_process_reg_success('undefined', _UA, _JObj, _Params) -> 'ok';
maybe_process_reg_success(Token, UA, JObj, Params) ->
    case kz_cache:fetch_local(?CACHE_NAME, Token) of
        {'error', 'not_found'} -> maybe_update_push_token(UA, JObj, Params);
        {'ok', TokenJObj} -> send_reply(Token, TokenJObj)
    end.

-spec maybe_update_push_token(kz_json:object(), kz_json:object(), kz_proplist()) -> 'ok'.
-spec maybe_update_push_token(api_binary(), api_binary(), kz_json:object(), kz_json:object(), kz_proplist()) -> 'ok'.
maybe_update_push_token(UA, JObj, Params) ->
    AccountId = kz_json:get_first_defined([[<<"Custom-Channel-Vars">>, <<"Account-ID">>]
                                           ,<<"Account-ID">>
                                          ], JObj),
    AuthorizingId = kz_json:get_first_defined([[<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>]
                                               ,<<"Authorizing-ID">>
                                              ], JObj),

    maybe_update_push_token(AccountId, AuthorizingId, UA, JObj, Params).

maybe_update_push_token('undefined', _AuthorizingId, _UA, _JObj, _Params) -> 'ok';
maybe_update_push_token(_AccountId, 'undefined', _UA, _JObj, _Params) -> 'ok';
maybe_update_push_token(AccountId, AuthorizingId, UA, JObj, Params) ->
    AccountDb = kz_util:format_account_db(AccountId),
    case kz_datamgr:open_cache_doc(AccountDb, AuthorizingId) of
        {'ok', Doc} ->
            Push = kz_json:get_value(<<"push">>, Doc),
            case build_push(UA, JObj, Params, kz_json:new()) of
                Push -> lager:debug("push exists: ~p", [Push]);
                NewPush ->
                    {'ok', _} = kz_datamgr:save_doc(AccountDb, kz_json:set_value(<<"push">>, NewPush, Doc)),
                    lager:debug("setting push object for ~s: ~s: ~p", [AccountId, AuthorizingId, NewPush])
            end;
        {'error', _} -> lager:debug("failed to open ~s in ~s", [AuthorizingId, AccountId])
    end.

-spec build_push(kz_json:object(), kz_json:object(), kz_proplist(), kz_json:object()) ->
                        kz_json:object().
build_push(UA, JObj, Params, InitialAcc) ->
    kz_json:foldl(
      fun(K, V, Acc) ->
              build_push_fold(K, V, Acc, JObj, Params)
      end, InitialAcc, UA).

-spec build_push_fold(kz_json:key(), kz_json:json_term(), kz_json:object(), kz_json:object(), kz_proplist()) -> kz_json:object().
build_push_fold(K, V, Acc, JObj, Params) ->
    case props:get_value(V, Params) of
        'undefined' ->
            case kz_json:get_value(V, JObj) of
                'undefined' -> Acc;
                V1 -> kz_json:set_value(K, V1, Acc)
            end;
        V2 -> kz_json:set_value(K, V2, Acc)
    end.

-spec send_reply(ne_binary(), kz_json:object()) -> 'ok'.
send_reply(Token, JObj) ->
    kz_cache:erase_local(?CACHE_NAME, Token),
    Queue = kz_json:get_value(<<"Server-ID">>, JObj),
    Payload = [{<<"Token-ID">>, Token}
               ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    lager:debug("sending pusher reply to ~s: ~p", [Queue, Payload]),
    kz_amqp_worker:cast(Payload, fun(P) -> kapi_pusher:publish_targeted_push_resp(Queue, P) end).

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], []).

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
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(?MODULE),
    lager:debug("pusher_listener started"),
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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'kz_amqp_channel',{'new_channel',_IsNew}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue',_Queue}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'push', JObj}, State) ->
    lager:debug("HANDLE_PUSH_push ~p", [JObj]),
    {'noreply', State};
handle_cast({'reg',JObj}, State) ->
    lager:debug("handle_cast_reg ~p",[JObj]),
    {'noreply', State};
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'DOWN', _Ref, 'process', _Pid, _R}, State) ->
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> handle_event_ret().
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
    lager:debug("pusher listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
