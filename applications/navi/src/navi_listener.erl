%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Voyager Internet Ltd.
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Ben Partridge
%%%-------------------------------------------------------------------
-module(navi_listener).
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

-export([handle_new_voicemail/2
        ,handle_push_request_device/2
        ]).

-include("navi.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

-define(BINDINGS, [{'notifications', [{'restrict_to', ['new_voicemail']}]}
                  ,{'navi', [{'restrict_to', ['push_device']}]}
                  ,{'self', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_new_voicemail'}, [{<<"notification">>, <<"voicemail_new">>}]}
                    ,{{?MODULE, 'handle_push_request_device'}, [{<<"navi">>, <<"push_device">>}]}
                    ]).
-define(QUEUE_NAME, <<"navi_listener">>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%===================================================================
%%% API
%%%===================================================================

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
                                      %%,{basic_qos, 1}                % only needed if prefetch controls
                                     ], []).

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

%%--------------------------------------------------------------------
%% @private
%% @doc Determines who received the voicemail and which user that corresponds to, then sends notifications.
%% @spec handle_new_voicemail(Jobj, Props) -> ok
%%--------------------------------------------------------------------
-spec handle_new_voicemail(kz_json:object(), kz_proplist()) -> 'ok'.
handle_new_voicemail(JObj, _Props) ->
    %% Ensure object is actually of event we want
    'true' = kapi_notifications:voicemail_new_v(JObj),
    lager:debug("Handling New Voicemail push notification: ~s", [kz_json:encode(JObj)]),
    VoicemailBoxId = kz_json:get_value(<<"Voicemail-Box">>, JObj),
    Account = kz_json:get_value(<<"Account-ID">>, JObj),
    AccountDb = kz_util:format_account_db(Account),
    {'ok', Doc} = kz_datamgr:open_cache_doc(AccountDb, VoicemailBoxId),
    User = kzd_voicemail_box:owner_id(Doc),
    Msg = kz_term:to_binary(io_lib:format("You received a voicemail message from ~s", [create_caller_id_string(JObj)])),
    ExtraParameters = [{<<"metadata">>, kz_json:normalize(JObj)}],
    push_notifications(Account, User, <<"new_voicemail">>, Msg, ExtraParameters).

%%--------------------------------------------------------------------
%% @private
%% @doc Handles external requests to send push notifications
%% @spec handle_push_request_device(Jobj, Props) -> ok
%%--------------------------------------------------------------------
-spec handle_push_request_device(kz_json:object(), kz_proplist()) -> 'ok'.
handle_push_request_device(JObj, _Props) ->
    lager:debug("Navi received external push request"),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    DeviceId = kz_json:get_value(<<"Device-ID">>, JObj),
    Msg = kz_json:get_value(<<"Message">>, JObj),
    Metadata = kz_json:get_value(<<"Metadata">>, JObj),
    Event = kz_json:get_value(<<"Push-Topic">>, JObj),
    ExtraParameters = [{<<"metadata">>, kz_json:normalize(Metadata)}],
    push_notifications_device(AccountId, DeviceId, Event, Msg, ExtraParameters).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Gets the registrations for the user of the given type and pushes the notifications
-spec push_notifications(api_ne_binary(), api_ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> any().
push_notifications(Account, User, Event, Message, ExtraParameters) ->
    case get_user_notification_registrations(Account, User, Event) of
        [_|_]=Registrations ->
            lager:debug("Found ~p notification registrations for user: ~p", [Registrations, User]),
            [send_notification_to_device(Registration, Message, ExtraParameters) || Registration <- Registrations];
        [] -> lager:debug("No push notification registrations for user: ~p/~p", [Account, User])
    end.

%%% Gets the registration(s) for the device of the given type and pushes the notification to that device
-spec push_notifications_device(api_ne_binary(), api_ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> any().
push_notifications_device(Account, Device, Event, Message, ExtraParameters) ->
    %% There may be multiple push notification registrations for the one device if the user uses many accounts on one device
    %% without unregistering, but we only need to return one of them as that will still deliver the notification to the device.
    case get_user_notification_registrations_device(Account, Device, Event) of
        [Reg|_] ->
            lager:debug("Found ~p notification registration for device: ~p", [Reg, Device]),
            send_notification_to_device(Reg, Message, ExtraParameters);
        [] -> lager:debug("No push notification registrations for device: ~p/~p", [Account, Device])
    end.

%% Reads the view for the push registrations for a device
-spec get_user_notification_registrations_device(ne_binary(), ne_binary(), ne_binary()) -> kz_json:objects().
get_user_notification_registrations_device(Account, Device, Event) ->
    AccountDB = kz_util:format_account_db(Account),
    {'ok', Rows} = kz_datamgr:get_results(AccountDB, <<"push_notification_subscriptions/by_reg_by_subscription">>, [{'key', [Device, Event]}]),
    [kz_json:get_value(<<"value">>, Row) || Row <- Rows].

%%% Reads the view for all registered devices for a given user
-spec get_user_notification_registrations(api_ne_binary(), ne_binary(), ne_binary()) -> kz_json:objects().
get_user_notification_registrations(_Account, 'undefined', _Event) ->
    [];
get_user_notification_registrations(Account, User, Event) ->
    AccountDB = kz_util:format_account_db(Account),
    {'ok', Rows} = kz_datamgr:get_results(AccountDB, <<"push_notification_subscriptions/by_user_by_subscription">>, [{'key', [User, Event]}]),
    [kz_json:get_value(<<"value">>, Row) || Row <- Rows].

%%% Develops a human readable string for the person's voicemail or missed call notification
-spec create_caller_id_string(kz_json:object()) -> ne_binary().
create_caller_id_string(JObj) ->
    case {kz_json:get_value(<<"Caller-ID-Number">>, JObj), kz_json:get_value(<<"Caller-ID-Name">>, JObj)} of
        {'undefined', _} -> <<"someone">>;
        {Number, 'undefined'} -> Number;
        {Number, Name} -> kz_term:to_binary(io_lib:format("~s (~s)", [Name, Number]))
    end.

%%% Sets up the datastructure and calls the push notification service
-spec send_notification_to_device(kz_json:object(), ne_binary(), kz_proplist()) -> 'ok' | 'error'.
send_notification_to_device(Registration, Msg, ExtraParameters) ->
    lager:debug("Sending notification for registration: ~p", [Registration]),
    RegistrationId = kz_json:get_value(<<"notification_registration_id">>, Registration),
    AppName = kz_json:get_value(<<"app_name">>, Registration),
    NotificationType = kz_json:get_value(<<"notification_type">>, Registration),
    navi_module_sup:push(RegistrationId, AppName, NotificationType, Msg, ExtraParameters).
