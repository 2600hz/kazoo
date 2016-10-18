%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnip_presence_amqp).
-behaviour(gen_server).

-export([start_link/0
        ,set_presence_state/2
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
    lager:debug("omnipresence event presence amqp package started"),
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
handle_cast({'gen_listener',{'created_queue',_Queue}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'omnipresence',{'probe', <<"presence">> = Package, User,
                             #omnip_subscription{call_id=CallId}=_Subscription
                            }}, State) ->
    kz_util:put_callid(CallId),
    omnip_util:request_probe(Package, User),
    {'noreply', State};
handle_cast({'omnipresence',{'presence_update', JObj}}, State) ->
    kz_util:put_callid(JObj),
    _ = kz_util:spawn(fun presence_event/1, [JObj]),
    {'noreply', State};
handle_cast({'omnipresence',{'presence_reset', JObj}}, State) ->
    kz_util:put_callid(JObj),
    _ = kz_util:spawn(fun presence_reset/1, [JObj]),
    {'noreply', State};
handle_cast({'omnipresence',{'channel_event', JObj}}, State) ->
    kz_util:put_callid(JObj),
    EventType = kz_json:get_value(<<"Event-Name">>, JObj),
    _ = kz_util:spawn(fun channel_event/2, [EventType, JObj]),
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
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
    lager:debug("received channel create, checking for presence subscribers"),
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
    lager:debug("received channel destroy, checking for presence subscribers"),
    handle_update(JObj, ?PRESENCE_HANGUP).

-spec handle_disconnected_channel(kz_json:object()) -> 'ok'.
handle_disconnected_channel(JObj) ->
    'true' = kapi_call:event_v(JObj),
    kz_util:put_callid(JObj),
    lager:debug("channel has been disconnected, checking status of channel on the cluster"),
    handle_destroyed_channel(JObj).

-spec handle_connected_channel(kz_json:object()) -> 'ok'.
handle_connected_channel(_JObj) ->
    'ok'.

-spec presence_event(kz_json:object()) -> 'ok'.
presence_event(JObj) ->
    State = kz_json:get_value(<<"State">>, JObj),
    maybe_handle_presence_state(JObj, State).

-spec maybe_handle_presence_state(kz_json:object(), api_binary()) -> 'ok'.
maybe_handle_presence_state(JObj, <<"online">>=State) ->
    handle_update(JObj, State, 0);
maybe_handle_presence_state(JObj, <<"offline">>=State) ->
    handle_update(JObj, State, 0);
maybe_handle_presence_state(JObj, State) ->
    handle_update(kz_json:delete_keys([<<"From">>, <<"To">>], JObj), State, 0).

-spec handle_update(kz_json:object(), ne_binary()) -> 'ok'.
handle_update(JObj, ?PRESENCE_HANGUP) ->
    handle_update(JObj, ?PRESENCE_HANGUP, 0);
handle_update(JObj, ?PRESENCE_RINGING) ->
    handle_update(JObj, ?PRESENCE_RINGING, 120);
handle_update(JObj, ?PRESENCE_ANSWERED) ->
    handle_update(JObj, ?PRESENCE_ANSWERED, 36000);
handle_update(JObj, State) ->
    handle_update(JObj, State, 0).

-spec handle_update(kz_json:object(), ne_binary(), integer()) -> 'ok'.
handle_update(JObj, State, Expires) ->
    To = kz_json:get_first_defined([<<"To">>, <<"Presence-ID">>], JObj),
    From = kz_json:get_first_defined([<<"From">>, <<"Presence-ID">>], JObj),

    case omnip_util:are_valid_uris([To, From]) of
        'true' -> handle_update(JObj, State, From, To, Expires);
        'false' -> lager:warning("presence handler ignoring update from ~s to ~s", [From, To])
    end.

-spec handle_update(kz_json:object(), ne_binary(), ne_binary(), ne_binary(), integer()) -> 'ok'.
handle_update(JObj, State, From, To, Expires) ->
    [ToUsername, ToRealm] = binary:split(To, <<"@">>),
    [FromUsername, FromRealm] = binary:split(From, <<"@">>),
    Direction = kz_json:get_lower_binary(<<"Call-Direction">>, JObj),
    {User, Props} =
        case Direction =:= <<"inbound">> of
            'true' ->
                {From, props:filter_undefined(
                         [{<<"From">>, <<"sip:", From/binary>>}
                         ,{<<"From-User">>, FromUsername}
                         ,{<<"From-Realm">>, FromRealm}
                         ,{<<"To">>, <<"sip:", To/binary>>}
                         ,{<<"To-User">>, ToUsername}
                         ,{<<"To-Realm">>, ToRealm}
                         ,{<<"State">>, State}
                         ,{<<"Expires">>, Expires}
                         ,{<<"Direction">>, <<"initiator">>}
                         ,{<<"Call-ID">>, ?FAKE_CALLID(From)}
                         ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
                         ,{<<"Event-Package">>, <<"presence">>}
                         ,{<<"destination">>, ToUsername}
                         ,{<<"uuid">>, kz_json:get_value(<<"Call-ID">>, JObj)}
                         ,{<<"user">>, FromUsername}
                         ,{<<"realm">>, FromRealm}
                          | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                         ])
                };
            'false' ->
                {To, props:filter_undefined(
                       [{<<"From">>, <<"sip:", To/binary>>}
                       ,{<<"From-User">>, ToUsername}
                       ,{<<"From-Realm">>, ToRealm}
                       ,{<<"To">>, <<"sip:", From/binary>>}
                       ,{<<"To-User">>, FromUsername}
                       ,{<<"To-Realm">>, FromRealm}
                       ,{<<"To">>, <<"sip:", From/binary>>}
                       ,{<<"State">>, State}
                       ,{<<"Expires">>, Expires}
                       ,{<<"Direction">>, <<"recipient">>}
                       ,{<<"Call-ID">>, ?FAKE_CALLID(To)}
                       ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
                       ,{<<"Event-Package">>, <<"presence">>}
                       ,{<<"destination">>, FromUsername}
                       ,{<<"uuid">>, kz_json:get_value(<<"Call-ID">>, JObj)}
                       ,{<<"user">>, ToUsername}
                       ,{<<"realm">>, ToRealm}
                        | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                       ])
                }
        end,
    maybe_send_update(User, Props).

-spec maybe_send_update(ne_binary(), kz_proplist()) -> 'ok'.
maybe_send_update(User, Props) ->
    case omnip_subscriptions:get_stalkers(?PRESENCE_EVENT, User) of
        {'ok', Stalkers} ->
            send_update(Stalkers, Props);
        {'error', 'not_found'} ->
            lager:debug("no ~s subscriptions for ~s",[?PRESENCE_EVENT, User])
    end.

-spec send_update(binaries(), kz_proplist()) -> 'ok'.
send_update(Stalkers, Props) ->
    {'ok', Worker} = kz_amqp_worker:checkout_worker(),
    _ = [kz_amqp_worker:cast(Props
                            ,fun(P) -> kapi_omnipresence:publish_update(S, P) end
                            ,Worker
                            )
         || S <- Stalkers
        ],
    kz_amqp_worker:checkin_worker(Worker).

-spec presence_reset(kz_json:object()) -> any().
presence_reset(JObj) ->
    User = <<(kz_json:get_value(<<"Username">>, JObj))/binary, "@", (kz_json:get_value(<<"Realm">>, JObj))/binary>>,
    set_presence_state(User, ?PRESENCE_HANGUP).

-spec set_presence_state(ne_binary(), ne_binary()) -> 'ok'.
set_presence_state(PresenceId, State) ->
    Headers = [{<<"Presence-ID">>, PresenceId }],
    handle_update(kz_json:from_list(Headers), State, 0).
