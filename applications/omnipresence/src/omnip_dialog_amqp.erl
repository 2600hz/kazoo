%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnip_dialog_amqp).
-behaviour(gen_server).

-export([start_link/0
        ,reset_blf/1
        ,reset_user_blf/1
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

-define(SERVER, ?MODULE).

-define(DEFAULT_VM_NUMBER, <<"*98">>).
-define(VM_NUMBER_KEY, <<"dialog_subscribed_mwi_prefix">>).
-define(VM_NUMBER(A), kapps_account_config:get_global(A, ?CONFIG_CAT, ?VM_NUMBER_KEY, ?DEFAULT_VM_NUMBER)).

-record(state, {}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

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
    lager:debug("omnipresence event dialog amqp package started"),
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

handle_cast({'gen_listener',{'created_queue',_Queue}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'omnipresence',{'channel_event', JObj}}, State) ->
    kz_util:put_callid(JObj),
    EventType = kz_json:get_value(<<"Event-Name">>, JObj),
    _ = kz_util:spawn(fun channel_event/2, [EventType, JObj]),
    {'noreply', State};
handle_cast({'omnipresence',{'presence_update', JObj}}, State) ->
    kz_util:put_callid(JObj),
    _ = kz_util:spawn(fun presence_event/1, [JObj]),
    {'noreply', State};
handle_cast({'omnipresence',{'mwi_update', JObj}}, State) ->
    kz_util:put_callid(JObj),
    _ = kz_util:spawn(fun mwi_event/1, [JObj]),
    {'noreply', State};
handle_cast({'omnipresence',{'presence_reset', JObj}}, State) ->
    kz_util:put_callid(JObj),
    _ = kz_util:spawn(fun presence_reset/1, [JObj]),
    {'noreply', State};
handle_cast({'omnipresence',{'probe', <<"dialog">> = Package, User,
                             #omnip_subscription{call_id=CallId}=_Subscription
                            }}, State) ->
    kz_util:put_callid(CallId),
    omnip_util:request_probe(Package, User),
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
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
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
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec channel_event(ne_binary(), kz_json:object()) -> 'ok'.
channel_event(<<"CHANNEL_CREATE">>, JObj) -> handle_new_channel(JObj);
channel_event(<<"CHANNEL_ANSWER">>, JObj) -> handle_answered_channel(JObj);
channel_event(<<"CHANNEL_DESTROY">>, JObj) -> handle_destroyed_channel(JObj);
channel_event(<<"CHANNEL_CONNECTED">>, JObj) -> handle_connected_channel(JObj);
channel_event(<<"CHANNEL_DISCONNECTED">>, JObj) -> handle_disconnected_channel(JObj);
channel_event(_, _JObj) -> 'ok'.

-spec handle_new_channel(kz_json:object()) -> 'ok'.
handle_new_channel(JObj) ->
    'true' = kapi_call:event_v(JObj),
    kz_util:put_callid(JObj),
    lager:debug("received channel create, checking for dialog subscribers"),
    handle_update(JObj, ?PRESENCE_RINGING).

-spec handle_answered_channel(kz_json:object()) -> 'ok'.
handle_answered_channel(JObj) ->
    'true' = kapi_call:event_v(JObj),
    kz_util:put_callid(JObj),
    lager:debug("received channel answer, checking for subscribers"),
    handle_update(JObj, ?PRESENCE_ANSWERED).

-spec handle_destroyed_channel(kz_json:object()) -> 'ok'.
handle_destroyed_channel(JObj) ->
    'true' = kapi_call:event_v(JObj),
    kz_util:put_callid(JObj),
    lager:debug("received channel destroy, checking for dialog subscribers"),
    handle_update(JObj, ?PRESENCE_HANGUP).

-spec handle_disconnected_channel(kz_json:object()) -> 'ok'.
handle_disconnected_channel(JObj) ->
    'true' = kapi_call:event_v(JObj),
    kz_util:put_callid(JObj),
    lager:debug("channel has been disconnected, checking status of channel on the cluster"),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    case kapps_call_command:b_channel_status(CallId) of
        {'ok', _} ->
            lager:info("call '~s' is still active, ignoring disconnect", [CallId]);
        _Else ->
            lager:info("call '~s' is no longer active, sending hangup", [CallId]),
            handle_destroyed_channel(JObj)
    end.

-spec handle_connected_channel(kz_json:object()) -> 'ok'.
handle_connected_channel(_JObj) ->
    'ok'.

-spec presence_event(kz_json:object()) -> 'ok'.
presence_event(JObj) ->
    State = kz_json:get_value(<<"State">>, JObj),
    maybe_handle_presence_state(JObj, State).

-spec maybe_handle_presence_state(kz_json:object(), api_binary()) -> 'ok'.
maybe_handle_presence_state(_JObj, <<"online">>) -> 'ok';
maybe_handle_presence_state(_JObj, <<"offline">>) -> 'ok';
maybe_handle_presence_state(JObj, ?PRESENCE_HANGUP=State) ->
    handle_update(JObj, State, 10);
maybe_handle_presence_state(JObj, ?PRESENCE_RINGING=State) ->
    handle_update(JObj, State, 0);
maybe_handle_presence_state(JObj, State) ->
    handle_update(JObj, State, 0).

-spec handle_update(kz_json:object(), ne_binary()) -> 'ok'.
handle_update(JObj, ?PRESENCE_HANGUP) ->
    handle_update(JObj, ?PRESENCE_HANGUP, 10);
handle_update(JObj, ?PRESENCE_RINGING) ->
    handle_update(JObj, ?PRESENCE_RINGING, 120);
handle_update(JObj, ?PRESENCE_ANSWERED) ->
    handle_update(JObj, ?PRESENCE_ANSWERED, 36000);
handle_update(_JObj, _State) -> 'ok'.

-spec handle_update(kz_json:object(), ne_binary(), integer()) -> 'ok'.
handle_update(JObj, State, Expires) ->
    To = kz_json:get_first_defined([<<"To">>, <<"Presence-ID">>], JObj),
    From = kz_json:get_first_defined([<<"From">>, <<"Presence-ID">>], JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj, ?FAKE_CALLID(From)),
    TargetCallId = kz_json:get_value(<<"Target-Call-ID">>, JObj, CallId),
    kz_util:put_callid(TargetCallId),

    case omnip_util:are_valid_uris([To, From]) of
        'true' -> handle_update(JObj, State, From, To, Expires);
        'false' -> lager:warning("dialog handler ignoring update from ~s to ~s", [From, To])
    end.

-spec handle_update(kz_json:object(), ne_binary(), ne_binary(), ne_binary(), integer()) -> 'ok'.
handle_update(JObj, State, From, To, Expires) ->
    [ToUsername, ToRealm] = binary:split(To, <<"@">>),
    [FromUsername, FromRealm] = binary:split(From, <<"@">>),

    CallId = kz_json:get_value(<<"Call-ID">>, JObj, ?FAKE_CALLID(From)),
    TargetCallId = kz_json:get_value(<<"Target-Call-ID">>, JObj, CallId),
    SwitchURI = kz_json:get_value(<<"Switch-URI">>, JObj),
    Cookie = kz_util:rand_hex_binary(6),

    {User, Props} =
        case kz_json:get_lower_binary(<<"Call-Direction">>, JObj) of
            <<"inbound">> ->
                {From, props:filter_undefined(
                         [{<<"From">>, <<"sip:", From/binary>>}
                         ,{<<"From-User">>, FromUsername}
                         ,{<<"From-Realm">>, FromRealm}
                         ,{<<"From-Tag">>, kz_json:get_value(<<"To-Tag">>, JObj)}
                         ,{<<"To">>, <<"sip:", To/binary>>}
                         ,{<<"To-User">>, ToUsername}
                         ,{<<"To-Realm">>, ToRealm}
                         ,{<<"To-Tag">>, kz_json:get_value(<<"From-Tag">>, JObj)}
                         ,{<<"State">>, State}
                         ,{<<"Expires">>, Expires}
                         ,{<<"Flush-Level">>, kz_json:get_value(<<"Flush-Level">>, JObj)}
                         ,{<<"Direction">>, <<"initiator">>}
                         ,{<<"Call-ID">>, CallId}
                         ,{<<"Target-Call-ID">>, TargetCallId}
                         ,{<<"Switch-URI">>, SwitchURI}
                         ,{<<"Call-Cookie">>, Cookie}
                         ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
                         ,{<<"Event-Package">>, <<"dialog">>}
                         ,{<<"destination">>, ToUsername}
                         ,{<<"uuid">>, kz_json:get_value(<<"Call-ID">>, JObj)}
                         ,{<<"user">>, FromUsername}
                         ,{<<"realm">>, FromRealm}
                          | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                         ])
                };
            _Direction ->
                App = kz_json:get_value(<<"App-Name">>, JObj, ?APP_NAME),
                ToURI = build_update_to_uri(State, App, From, ToRealm, Cookie),
                {To, props:filter_undefined(
                       [{<<"From">>, <<"sip:", To/binary>>}
                       ,{<<"From-User">>, ToUsername}
                       ,{<<"From-Realm">>, ToRealm}
                       ,{<<"From-Tag">>, kz_json:get_value(<<"From-Tag">>, JObj)}
                       ,{<<"From-URI">>, ToURI}
                       ,{<<"To">>, ToURI}
                       ,{<<"To-URI">>, ToURI}
                       ,{<<"To-User">>, FromUsername}
                       ,{<<"To-Realm">>, FromRealm}
                       ,{<<"To-Tag">>, kz_json:get_value(<<"To-Tag">>, JObj)}
                       ,{<<"State">>, State}
                       ,{<<"Expires">>, Expires}
                       ,{<<"Flush-Level">>, kz_json:get_value(<<"Flush-Level">>, JObj)}
                       ,{<<"Direction">>, <<"recipient">>}
                       ,{<<"Call-ID">>, CallId}
                       ,{<<"Target-Call-ID">>, TargetCallId}
                       ,{<<"Switch-URI">>, SwitchURI}
                       ,{<<"Call-Cookie">>, Cookie}
                       ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
                       ,{<<"Event-Package">>, <<"dialog">>}
                       ,{<<"destination">>, FromUsername}
                       ,{<<"uuid">>, kz_json:get_value(<<"Call-ID">>, JObj)}
                       ,{<<"user">>, ToUsername}
                       ,{<<"realm">>, ToRealm}
                        | kz_api:default_headers(App, ?APP_VERSION)
                       ])
                }
        end,
    maybe_send_update(User, Props).

-spec build_update_to_uri(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
build_update_to_uri(State, App, From, Realm, Cookie) ->
    case kapps_config:get(?CONFIG_CAT, <<"use_fast_pickup_cookies">>, 'true') of
        'true' -> to_uri_cookie(State, App, From, Realm, Cookie);
        _Other -> to_uri(State, App, From, Realm, Cookie)
    end.

-spec to_uri(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
to_uri(?PRESENCE_RINGING, <<"park">>, From, _, _) ->
    <<"sip:", From/binary,";kazoo-pickup=true">>;
to_uri(_State, _, From, _, _) ->
    <<"sip:", From/binary>>.

-spec to_uri_cookie(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
to_uri_cookie(?PRESENCE_RINGING, _, _, Realm, Cookie) ->
    <<"sip:kfp+", Cookie/binary, "@", Realm/binary>>;
to_uri_cookie(?PRESENCE_ANSWERED, _, _, Realm, Cookie) ->
    <<"sip:kfp+", Cookie/binary, "@", Realm/binary>>;
to_uri_cookie(_State, _, From, _, _) ->
    <<"sip:", From/binary>>.

-spec maybe_send_update(ne_binary(), kz_proplist()) -> 'ok'.
maybe_send_update(User, Props) ->
    case omnip_subscriptions:get_stalkers(?DIALOG_EVENT, User) of
        {'ok', Stalkers} ->
            send_update(Stalkers, Props);
        {'error', 'not_found'} ->
            lager:debug("no ~s subscriptions for ~s",[?DIALOG_EVENT, User])
    end.

-spec send_update(binaries(), kz_proplist()) -> 'ok'.
send_update(Stalkers, Props) ->
    lager:debug("sending amqp dialog update state ~p for ~s/~s to ~p",
                [props:get_value(<<"State">>, Props)
                ,props:get_value(<<"From-User">>, Props)
                ,props:get_value(<<"To-User">>, Props)
                ,Stalkers
                ]),
    {'ok', Worker} = kz_amqp_worker:checkout_worker(),
    _ = [kz_amqp_worker:cast(Props
                            ,fun(P) -> kapi_omnipresence:publish_update(S, P) end
                            , Worker
                            )
         || S <- Stalkers
        ],
    kz_amqp_worker:checkin_worker(Worker).

-spec presence_reset(kz_json:object()) -> any().
presence_reset(JObj) ->
    User = <<(kz_json:get_value(<<"Username">>, JObj))/binary, "@", (kz_json:get_value(<<"Realm">>, JObj))/binary>>,
    reset_blf(User).

-spec reset_blf(ne_binary()) -> any().
reset_blf(User) ->
    Headers = [{<<"From">>, User}
              ,{<<"To">>, User}
              ,{<<"Flush-Level">>, 1}
              ,{<<"Call-ID">>, kz_util:to_hex_binary(crypto:hash('md5', User))}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    handle_update(kz_json:from_list(Headers), ?PRESENCE_HANGUP).

-spec reset_user_blf(ne_binary()) -> any().
reset_user_blf(User) ->
    case omnip_subscriptions:find_user_subscriptions(?DIALOG_EVENT, User) of
        {'ok', Subs} ->
            [reset_blf(SubUser) || #omnip_subscription{user=SubUser} <- Subs];
        _ -> 'ok'
    end.

-spec mwi_event(kz_json:object()) -> 'ok'.
mwi_event(JObj) ->
    To = kz_json:get_value(<<"To">>, JObj),
    [_, ToRealm] = binary:split(To, <<"@">>),
    {ok, AccountDb} = kapps_util:get_account_by_realm(ToRealm),
    AccountId = kz_util:format_account_id(AccountDb),
    State = case kz_json:get_integer_value(<<"Messages-New">>, JObj, 0) of
                0 -> ?PRESENCE_HANGUP;
                _ -> ?PRESENCE_ANSWERED
            end,
    Props = props:filter_undefined(
              [{<<"Presence-ID">>, <<(?VM_NUMBER(AccountId))/binary, To/binary>>}
              ,{<<"Flush-Level">>, 1}
              ,{<<"Call-Direction">>, <<"inbound">>}
              ]),
    handle_update(kz_json:from_list(Props), State, 0).
