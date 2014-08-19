%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnip_pkg_presence).

-behaviour(gen_server).

-export([start_link/0
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("omnipresence.hrl").

-record(state, {}).

-record(call, {call_id     :: api_binary()
               ,direction  :: api_binary()
               ,state      :: api_binary()
               ,to         :: api_binary()
              }).

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
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).


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
    ensure_template(),
    lager:debug("omnipresence event presence package started"),
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

handle_cast({'gen_listener',{'created_queue',_Queue}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'omnipresence',{'subscribe_notify', <<"presence">>, User, #omnip_subscription{}=Subscription}}, State) ->
    [Username, Realm] = binary:split(User, <<"@">>),
    Props = [{<<"user">>, Username}, {<<"realm">>, Realm}],
    spawn(fun() -> send_update(User, Props, [Subscription]) end),
    {'noreply', State};
handle_cast({'omnipresence',{'resubscribe_notify', <<"presence">>, User, #omnip_subscription{}=Subscription}}, State) ->
    [Username, Realm] = binary:split(User, <<"@">>),
    Props = [{<<"user">>, Username}, {<<"realm">>, Realm}],
    spawn(fun() -> send_update(User, Props, [Subscription]) end),
    {'noreply', State};
handle_cast({'omnipresence',{'presence_update', JObj}}, State) ->
    spawn(fun() -> presence_event(JObj) end),
    {'noreply', State};
handle_cast({'omnipresence',{'channel_event', JObj}}, State) ->
    EventType = wh_json:get_value(<<"Event-Name">>, JObj),
    spawn(fun() -> channel_event(EventType, JObj) end),
    {'noreply', State};
handle_cast({'omnipresence', _}, State) ->
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

-spec channel_event(ne_binary(), wh_json:object()) -> 'ok'.
channel_event(<<"CHANNEL_CREATE">>, JObj) -> handle_new_channel(JObj);
channel_event(<<"CHANNEL_ANSWER">>, JObj) -> handle_answered_channel(JObj);
channel_event(<<"CHANNEL_DESTROY">>, JObj) -> handle_destroyed_channel(JObj);
channel_event(<<"CHANNEL_CONNECTED">>, JObj) -> handle_connected_channel(JObj);
channel_event(<<"CHANNEL_DISCONNECTED">>, JObj) -> handle_disconnected_channel(JObj);
channel_event(_, _JObj) -> 'ok'.

-spec handle_new_channel(wh_json:object()) -> 'ok'.
handle_new_channel(JObj) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("received channel create, checking for subscribers"),
    handle_update(JObj, ?PRESENCE_RINGING).

-spec handle_answered_channel(wh_json:object()) -> any().
handle_answered_channel(JObj) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("received channel answer, checking for subscribers"),
    handle_update(JObj, ?PRESENCE_ANSWERED).

-spec handle_destroyed_channel(wh_json:object()) -> any().
handle_destroyed_channel(JObj) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("received channel destroy, checking for subscribers"),
    handle_update(JObj, ?PRESENCE_HANGUP).

-spec handle_disconnected_channel(wh_json:object()) -> any().
handle_disconnected_channel(JObj) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("channel has been disconnected, checking status of channel on the cluster"),
    handle_destroyed_channel(JObj).

-spec handle_connected_channel(wh_json:object()) -> 'ok'.
handle_connected_channel(_JObj) ->
    'ok'.

-spec presence_event(wh_json:object()) -> 'ok'.
presence_event(JObj) ->
    State = wh_json:get_value(<<"State">>, JObj),
    maybe_handle_presence_state(State, JObj).
    
-spec maybe_handle_presence_state(api_binary(), wh_json:object()) -> 'ok'.
maybe_handle_presence_state(<<"online">>=State, JObj) ->
    handle_update(State, JObj);
maybe_handle_presence_state(_, _JObj) -> 'ok'.

-spec handle_update(ne_binary(), wh_json:object()) -> any().
handle_update(State, JObj) ->
    To = wh_json:get_first_defined([<<"To">>, <<"Presence-ID">>], JObj),
    From = wh_json:get_value(<<"From">>, JObj),
    [ToUsername, ToRealm] = binary:split(To, <<"@">>),
    [FromUsername, FromRealm] = binary:split(From, <<"@">>),
    Direction = wh_json:get_lower_binary(<<"Call-Direction">>, JObj),
    {User, Props} =
        case Direction =:= <<"inbound">> of
            'true' ->
                {From, props:filter_undefined(
                       [{<<"destination">>, ToUsername}
                        ,{<<"state">>, State}
                        ,{<<"direction">>, <<"initiator">>}
                        ,{<<"uuid">>, wh_json:get_value(<<"Call-ID">>, JObj)}
                        ,{<<"user">>, FromUsername}
                        ,{<<"realm">>, FromRealm}
                       ])};
            'false' ->
                {To, props:filter_undefined(
                       [{<<"destination">>, FromUsername}
                        ,{<<"state">>, State}
                        ,{<<"direction">>, <<"recipient">>}
                        ,{<<"uuid">>, wh_json:get_value(<<"Call-ID">>, JObj)}
                        ,{<<"user">>, ToUsername}
                        ,{<<"realm">>, ToRealm}
                       ])}
            end,
    maybe_send_update(User, Props).
    
-spec maybe_send_update(ne_binary(), wh_proplist()) -> 'ok'.   
maybe_send_update(User, Props) ->
    case omnip_subscriptions:find_subscriptions(?PRESENCE_EVENT, User) of
        {'ok', Subs} ->
            send_update(User, Props, Subs);
        {'error', 'not_found'} ->
            lager:debug("no ~s subscriptions for ~s",[?PRESENCE_EVENT, User])
    end.

-spec send_update(ne_binary(), wh_proplist(), subscriptions()) -> 'ok'.
send_update(User, Props, Subscriptions) ->
    Body = build_body(User, Props),
    Options = [{body, Body}
               ,{content_type, <<"application/pidf+xml">>}
               ,{subscription_state, active}
               ],
    [nksip_uac:notify(SubscriptionId,
                      Options ++ [{contact, Contact},{route, [Proxy]}])
                    || #omnip_subscription{subscription_id=SubscriptionId
                                           ,contact=Contact
                                           ,proxy_route=Proxy} <- Subscriptions
                                                  , SubscriptionId =/= 'undefined'].

-spec get_user_channels(ne_binary()) -> list().
get_user_channels(User) ->
    [Username, Realm] = binary:split(User, <<"@">>),
    Payload = [{<<"Username">>, Username}
               ,{<<"Realm">>, Realm}
               ,{<<"Active-Only">>, 'false'}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_request(Payload
                                       ,fun wapi_call:publish_query_user_channels_req/1
                                       ,fun wapi_call:query_user_channels_resp_v/1
                                      )
    of
        {'ok', Resp} ->
            case wh_json:get_value(<<"Channels">>, Resp) of
                'undefined' -> [];
                Channels ->  wh_json:to_proplist(Channels)
            end;
        {'error', _E} ->
            lager:debug("query user channels for ~s failed : ~p", [User, _E]),
            []
    end.

-spec map_state(ne_binary() | boolean()) -> ne_binary().
map_state('false') -> <<"early">>;
map_state('true') -> <<"confirmed">>;
map_state(Other) -> Other.

-spec map_direction(ne_binary()) -> ne_binary().
map_direction(<<"inbound">>) -> <<"initiator">>;
map_direction(<<"outbound">>) -> <<"recipient">>;
map_direction(Other) -> Other.

-spec normalize_variables(wh_proplist()) -> wh_proplist().
normalize_variables(Props) ->
    [{wh_json:normalize_key(K), V} || {K, V} <- Props ].

-spec props_to_call(wh_proplist()) -> #call{} | 'undefined'.
props_to_call(Props) ->
    case props:is_defined(<<"uuid">>, Props) of
        'true' ->
            #call{call_id = props:get_value(<<"uuid">>, Props) 
                  ,direction = map_direction(props:get_value(<<"direction">>, Props))
                  ,state = map_state(props:get_first_defined([<<"state">>, <<"answered">>], Props))
                  ,to = props:get_value(<<"destination">>, Props)
                 };
        'false' -> 'undefined'
    end.

-spec build_channels(ne_binary(), wh_proplist()) -> wh_proplist().
build_channels(User, Props) ->    
    Channels = [props_to_call(Channel) || Channel  <- get_user_channels(User)],
    case props_to_call(Props) of
        'undefined' ->
            Channels;
        UUID ->
            [ UUID | [ Channel || Channel  <- Channels, Channel#call.call_id =/= UUID#call.call_id] ]
    end.


-spec build_variables(ne_binary(), wh_proplist()) -> ne_binary().
build_variables(User, Props) ->
    case build_channels(User, Props) of
        [] -> normalize_variables(Props);
        Channels -> normalize_variables(props:set_value(<<"calls">>, Channels, Props))
    end.
    

-spec build_body(ne_binary(), wh_proplist()) -> ne_binary().
build_body(User, Props) ->
    Variables = build_variables(User, Props),
    {'ok', Text} = sub_package_presence:render(Variables),
    Body = wh_util:to_binary(Text),
    binary:replace(Body, <<"\n\n">>, <<"\n">>, [global]).


ensure_template() ->
    BasePath = code:lib_dir(omnipresence, priv),
    File = lists:concat([BasePath, "/packages/presence.xml"]),
    Mod = wh_util:to_atom(<<"sub_package_presence">>, 'true'),
    {'ok', _CompileResult} = erlydtl:compile(File, Mod, [{record_info, [{call, record_info(fields, call)}]}]).
