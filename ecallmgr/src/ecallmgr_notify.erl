%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% Notify-type requests, like MWI updates, received and processed here
%%% @end
%%% Created :  18 Jul 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_notify).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([send_mwi/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MWI_BODY, "Messages-Waiting: ~s~nVoice-Message: ~b/~b (~b/~b)~nMessage-Account: ~s~n").

-include("ecallmgr.hrl").

-record(state, {amqp_q  = <<>> :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec send_mwi/4 :: (User, Realm, New, Saved) -> no_return() when
      User :: string() | binary(),
      Realm :: string() | binary(),
      New :: integer() | binary(),
      Saved :: integer() | binary().
send_mwi(User, Realm, New, Saved) ->
    JObj = {struct, [{<<"Notify-User">>, wh_util:to_binary(User)}
                    ,{<<"Notify-Realm">>, wh_util:to_binary(Realm)}
                    ,{<<"Messages-New">>, wh_util:to_binary(New)}
                    ,{<<"Messages-Saved">>, wh_util:to_binary(Saved)}
                    | wh_api:default_headers(<<>>, <<"notify">>, <<"mwi">>, ?APP_NAME, ?APP_VERSION)
                   ]},
    process_req({<<"notify">>, <<"mwi">>}, JObj, #state{}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ?LOG_SYS("starting new ecallmgr notify process"),
    {ok, #state{}, 0}.

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
    {reply, ok, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(timeout, #state{amqp_q = <<>>}=State) ->
    try
        {ok, Q} = start_amqp(),
        {noreply, State#state{amqp_q=Q}, hibernate}
    catch
        _:_ ->
            ?LOG_SYS("attempting to connect AMQP again in ~b ms", [?AMQP_RECONNECT_INIT_TIMEOUT]),
            {ok, _} = timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
            {noreply, State}
    end;

handle_info({amqp_reconnect, T}, State) ->
    try
        {ok, NewQ} = start_amqp(),
        {noreply, State#state{amqp_q=NewQ}, hibernate}
    catch
        _:_ ->
            case T * 2 of
                Timeout when Timeout > ?AMQP_RECONNECT_MAX_TIMEOUT ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [?AMQP_RECONNECT_MAX_TIMEOUT]),
                    {ok, _} = timer:send_after(?AMQP_RECONNECT_MAX_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_MAX_TIMEOUT}),
                    {noreply, State};
                Timeout ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [Timeout]),
                    {ok, _} = timer:send_after(Timeout, {amqp_reconnect, Timeout}),
                    {noreply, State}
            end
    end;

handle_info({amqp_host_down, _}, State) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    {ok, _} = timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
    {noreply, State#state{amqp_q = <<>>}, hibernate};

handle_info({#'basic.deliver'{}, #amqp_msg{props = Props, payload = Payload}}, State) when
      Props#'P_basic'.content_type == <<"application/json">> ->
    spawn(fun() ->
                  JObj = mochijson2:decode(Payload),
                  put(callid, wh_json:get_value(<<"Call-ID">>, JObj, <<"000000000000">>)),
                  _ = process_req(get_event_type(JObj), JObj, State)
          end),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

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
    ?LOG_SYS("ecallmgr notify ~p termination", [_Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ensure the exhanges exist, build a queue, bind, and consume
%% @end
%%--------------------------------------------------------------------
-spec start_amqp/0 :: () -> tuple(error, amqp_error) | tuple(ok, binary()).
start_amqp() ->
    try
        true = is_binary(Q = amqp_util:new_queue()),
        #'basic.qos_ok'{} = amqp_util:basic_qos(1),
        _ = amqp_util:bind_q_to_callmgr(Q, ?KEY_SIP_NOTIFY),
        _ = amqp_util:basic_consume(Q),
        ?LOG_SYS("connected to AMQP"),
        {ok, Q}
    catch
        _:R ->
            ?LOG_SYS("failed to connect to AMQP ~p", [R]),
            {error, amqp_error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convience function for seperating the event category and name
%% @end
%%--------------------------------------------------------------------
-spec get_event_type/1 :: (JObj) -> tuple(binary(), binary()) when
      JObj :: json_object().
get_event_type(JObj) ->
    {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level wh_api request to retrieve a list of
%% conference memebers
%% @end
%%--------------------------------------------------------------------
-spec process_req/3 :: (MsgType, JObj, State) -> no_return() when
      MsgType :: tuple(binary(), binary()),
      JObj :: json_object(),
      State :: #state{}.
process_req({<<"notify">>, <<"mwi">>}, JObj, _) ->
    ?LOG_START("received notify mwi request"),
    true = wh_api:mwi_update_v(JObj),
    User = wh_json:get_value(<<"Notify-User">>, JObj),
    Realm  = wh_json:get_value(<<"Notify-Realm">>, JObj),
    case get_endpoint(User, Realm) of
        {error, timeout} ->
            ?LOG_END("mwi timed out looking up contact for ~s@~s", [User, Realm]);
        Endpoint ->
            NewMessages = wh_json:get_integer_value(<<"Messages-New">>, JObj, 0),
            Body = io_lib:format(?MWI_BODY, [case NewMessages of 0 -> "no"; _ -> "yes" end,
                                             NewMessages
                                             ,wh_json:get_integer_value(<<"Messages-Saved">>, JObj, 0)
                                             ,wh_json:get_integer_value(<<"Messages-Urgent">>, JObj, 0)
                                             ,wh_json:get_integer_value(<<"Messages-Urgent-Saved">>, JObj, 0)
                                             ,<<User/binary, "@", Realm/binary>>
                                            ]),
            ?LOG("created mwi notify body ~s", [Body]),
            Headers = [{"profile", "sipinterface_1"}
                       ,{"to-uri", Endpoint}
                       ,{"from-uri", "sip:2600hz@2600hz.com"}
                       ,{"event-str", "message-summary"}
                       ,{"content-type", "application/simple-message-summary"}
                       ,{"content-length", wh_util:to_list(length(Body))}
                       ,{"body", lists:flatten(Body)}
                      ],
            {ok, Node} = ecallmgr_fs_handler:request_node(<<"audio">>),
            Resp = freeswitch:sendevent(Node, 'NOTIFY', Headers),
            ?LOG("sending MWI update to ~s (~p)", [Node, Resp])
    end;
process_req(_, _, _) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Request the current registration contact string for the user/realm
%% replacing everything before the first '@' with:
%% 'sip:{User}'
%% @end
%%--------------------------------------------------------------------
-spec get_endpoint/2 :: (User, Realm) -> tuple(error, timeout) | string() when
      User :: binary(),
      Realm :: binary().
get_endpoint(User, Realm) ->
    case ecallmgr_registrar:lookup(Realm, User, [<<"Contact">>]) of
        [{<<"Contact">>, Contact}] ->
            RURI = binary:replace(re:replace(Contact, "^[^\@]+", User, [{return, binary}]), <<">">>, <<"">>),
            wh_util:to_list(<<"sip:", (RURI)/binary>>);
        {error, timeout}=E ->
            E
    end.
