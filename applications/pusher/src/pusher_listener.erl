%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(pusher_listener).

-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-export([handle_push/2, handle_reg_success/2]).

-include("pusher.hrl").
-include("gen_listener_spec.hrl").

-include_lib("nksip/include/nksip.hrl").

-record(state, {subs_pid :: pid()
                ,subs_ref :: reference()
               }).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'self', []}
                   ,{'pusher', []}
                   ,{'registration', [{'restrict_to',['reg_success']}]}
                  ]).
-define(RESPONDERS, [
                     {{?MODULE, 'handle_push'}
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

-spec handle_push(wh_json:object(), wh_proplist()) -> 'ok'.
handle_push(JObj, _Props) ->
    Token = wh_json:get_value(<<"Token-ID">>, JObj),
    TokenType = wh_json:get_value(<<"Token-Type">>, JObj),
    Module = wh_util:to_atom(<<"pm_",TokenType/binary>> , 'true'),
    wh_cache:store_local(?PUSHER_CACHE
                         ,Token
                         ,JObj
                         ,[{'expires', 20}]
                        ),
    gen_server:cast(Module, {'push', JObj}).

-spec handle_reg_success(wh_json:object(), wh_proplist()) -> 'ok'.
handle_reg_success(JObj, _Props) ->
    UserAgent = wh_json:get_value(<<"User-Agent">>, JObj),
    UserAgentProperties = pusher_util:user_agent_push_properties(UserAgent),
    maybe_process_reg_success(UserAgentProperties, JObj).

maybe_process_reg_success('undefined', _) -> 
    'ok';
maybe_process_reg_success(UA, JObj) -> 
    OriginalContact = wh_json:get_value(<<"Original-Contact">>, JObj),
    [#uri{opts=A, ext_opts=B}] = nksip_parse_uri:uris(OriginalContact),
    Params = A ++ B,
    TokenKey = wh_json:get_value(?TOKEN_KEY, UA),
    Token = props:get_value(TokenKey, Params),
    maybe_process_reg_success(Token, wh_json:set_value(<<"Token-Proxy">>, ?TOKEN_PROXY_KEY, UA) , JObj, Params).

maybe_process_reg_success('undefined', _, _, _) -> 
    'ok';
maybe_process_reg_success(Token, UA, JObj, Params) ->
    case wh_cache:fetch_local(?PUSHER_CACHE, Token) of
        {'error', 'not_found'} -> maybe_update_push_token(Token, UA, JObj, Params);
        {'ok', TokenJObj} -> send_reply(Token, TokenJObj)
    end.
    
maybe_update_push_token(Token, UA, JObj, Params) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AuthorizingId = wh_json:get_value(<<"Authorizing-ID">>, JObj),
    maybe_update_push_token(AccountId, AuthorizingId, Token, UA, JObj, Params).

maybe_update_push_token('undefined', _AuthorizingId, _Token, _UA, _JObj, _Params) -> 'ok';
maybe_update_push_token(_AccountId, 'undefined', _Token, _UA, _JObj, _Params) -> 'ok';
maybe_update_push_token(AccountId, AuthorizingId, _Token, UA, JObj, Params) ->
    AccountDb = wh_util:format_account_db(AccountId),
    case couch_mgr:open_cache_doc(AccountDb, AuthorizingId) of
        {'ok', Doc} ->
            Push = wh_json:get_value(<<"push">>, Doc),
            NewPush = build_push(UA, JObj, Params, wh_json:new()),
            case Push =:= NewPush of
                'false' -> couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"push">>, NewPush, Doc));
                'true' -> 'ok'
            end;
        {'error', _} -> 'ok'
    end,
    'ok'.

build_push(UA, JObj, Params, InitialAcc) ->
    wh_json:foldl(
      fun(K, V, Acc)->
              case props:get_value(V, Params) of
                  'undefined' ->
                      case wh_json:get_value(V, JObj) of
                          'undefined' -> Acc;
                          V1 -> wh_json:set_value(K, V1, Acc)
                      end;
                  V2 -> wh_json:set_value(K, V2, Acc)
              end
      end, InitialAcc, UA).

send_reply(Token, JObj) ->
    wh_cache:erase_local(?PUSHER_CACHE, Token),
    Queue = wh_json:get_value(<<"Server-ID">>, JObj),
    Payload = [{<<"Token-ID">>, Token}
              ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    wh_amqp_worker:cast(Payload, fun(P) -> wapi_pusher:publish_targeted_push_resp(Queue, P) end).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                      %%,{'basic_qos', 1}                % only needed if prefetch controls
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
init([]) ->
    put('callid', ?MODULE),
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

handle_cast({'wh_amqp_channel',{'new_channel',_IsNew}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue',_Queue}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'push',JObj}, State) ->
	lager:debug("HANDLE_PUSH_push ~p",[JObj]),

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
handle_event(_JObj, _State) ->
    {reply, []}.

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
    lager:debug("pusher listener terminating: ~p", [_Reason]).

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
