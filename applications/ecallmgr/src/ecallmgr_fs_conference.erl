%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012 VoIP INC
%%% @doc
%%% Execute conference commands
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_conference).

-behaviour(gen_listener).

%% API
-export([start_link/1
         ,start_link/2
        ]).
-export([handle_command/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(RESPONDERS, [{{?MODULE, 'handle_command'}
                      ,[{<<"conference">>, <<"command">>}]
                     }
                    ]).
-define(BINDINGS, [{'conference', [{'restrict_to', ['command']}]}]).
%% This queue is used to round-robin conference commands among ALL the
%% conference listeners with the hopes that the one receiving the command
%% can send it to the focus (barring network connectivity)...
-define(QUEUE_NAME, <<"ecallmgr_fs_conference">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-record(state, {node = 'undefined' :: atom()
                ,options = [] :: wh_proplist()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_listener:start_link(?MODULE,
                            [{'responders', ?RESPONDERS}
                             ,{'bindings', ?BINDINGS}
                             ,{'queue_name', ?QUEUE_NAME}
                             ,{'queue_options', ?QUEUE_OPTIONS}
                             ,{'consume_options', ?CONSUME_OPTIONS}
                            ], [Node, Options]).

-spec handle_command(wh_json:object(), wh_proplist()) -> any().
handle_command(JObj, _Props) ->
    do_handle_command(JObj, 0).

-spec do_handle_command(wh_json:object(), 0..3) -> 'ok'.
do_handle_command(JObj, Tries) when Tries < 3 ->
    ConferenceId = wh_json:get_value(<<"Conference-ID">>, JObj),
    case ecallmgr_fs_conferences:node(ConferenceId) of
        {'error', 'not_found'} ->
            timer:sleep(50),
            do_handle_command(JObj, Tries + 1);
        {'ok', Node} -> exec(Node, ConferenceId, JObj)
    end;
do_handle_command(_, _) -> 'ok'.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([Node, Options]) ->
    put('callid', ?LOG_SYSTEM_ID),
    lager:info("starting new fs conference listener for ~s", [Node]),
    gen_server:cast(self(), 'bind_to_events'),
    ecallmgr_fs_conferences:sync_node(Node),
    {'ok', #state{node=Node, options=Options}}.

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
handle_cast('bind_to_events', #state{node=Node}=State) ->
    case gproc:reg({'p', 'l', {'event', Node, <<"conference::maintenance">>}}) of
        'true' -> {'noreply', State};
        'false' -> {'stop', 'gproc_badarg', State}
    end;
handle_cast({'gen_listener', {'created_queue', <<"ecallmgr_fs_conference">>}}, #state{node=Node}=State) ->
    Self = self(),
    ConsumerPid = wh_amqp_channel:consumer_pid(),
    spawn(fun() ->
                  %% This queue is used to round-robin conference commands
                  %% between listeners that have established connections
                  %% to the conference focus when the upstream whapp
                  %% knows the conference focus...
                  wh_amqp_channel:consumer_pid(ConsumerPid),
                  QueueName = wapi_conference:focus_queue_name(Node),
                  Options = [{'queue_options', [{'exclusive', 'false'}]}
                             ,{'consume_options', [{'exclusive', 'false'}]}
                            ],
                  Bindings= [{'self', []}],
                  gen_listener:add_queue(Self, QueueName, Options, Bindings)
          end),
    {'noreply', State};
handle_cast({'wh_amqp_channel',{'new_channel',_IsNew}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue',_QueueName}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
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
handle_info({'event', ['undefined' | Props]}, #state{node=Node}=State) ->
    Action = props:get_value(<<"Action">>, Props),
    _ = case process_conference_event(Action, Props, Node) of
            'stop' -> 'ok';
            'continue' -> send_conference_event(Action, Props);
            {'continue', CustomProps} ->
                send_conference_event(Action, Props, CustomProps)
        end,
    process_conference_event(Action, Props, Node),
    {'noreply', State};
handle_info({'event', [CallId | Props]}, #state{node=Node}=State) ->
    Action = props:get_value(<<"Action">>, Props),
    _ = case process_participant_event(Action, Props, Node, CallId) of
            'stop' -> 'ok';
            'continue' -> send_participant_event(Action, CallId, Props);
            {'continue', CustomProps} ->
                send_participant_event(Action, CallId, Props, CustomProps)
        end,
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all amqp messages
%%
%% @spec handle_event(JObj, Props) -> {reply, Props} |
%%                                    ignore
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
    lager:debug("ecallmgr conference listener terminating: ~p", [_Reason]).

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
-spec process_participant_event(ne_binary(), wh_proplist(), atom(), ne_binary()) ->
                                       {'continue', wh_proplist()} |
                                       'continue' |
                                       'stop'.
process_participant_event(<<"add-member">>, Props, Node, CallId) ->
    _ = ecallmgr_fs_conferences:participant_create(Props, Node, CallId),
    %% TODO: this can be removed once the whapps conf_participant is refactored
    _ = publish_new_participant_event(Props, Node),
    'continue';
process_participant_event(<<"del-member">>, Props, Node, CallId) ->
    _ = ecallmgr_fs_conferences:participant_destroy(CallId),
    %% TODO: this can be removed once the whapps conf_participant is refactored
    _ = publish_participant_destroy_event(Props, Node),
    'continue';
process_participant_event(<<"stop-talking">>, _, _, _) -> 'continue';
process_participant_event(<<"start-talking">>, _, _, _) -> 'continue';
process_participant_event(<<"mute-member">>, _, _, CallId) ->
    _ = ecallmgr_fs_conferences:participant_update(CallId, [{#participant.speak, 'false'}]),
    'continue';
process_participant_event(<<"unmute-member">>, _, _, CallId) ->
    _ = ecallmgr_fs_conferences:participant_update(CallId, [{#participant.speak, 'true'}]),
    'continue';
process_participant_event(<<"deaf-member">>, _, _, CallId) ->
    _ = ecallmgr_fs_conferences:participant_update(CallId, [{#participant.hear, 'false'}]),
    'continue';
process_participant_event(<<"undeaf-member">>, _, _, CallId) ->
    _ = ecallmgr_fs_conferences:participant_update(CallId, [{#participant.hear, 'true'}]),
    'continue';
process_participant_event(<<"hup-member">>, _, _, CallId) ->
    _ = ecallmgr_fs_conferences:participant_destroy(CallId),
    'continue';
process_participant_event(<<"kick-member">>, _, _, CallId) ->
    _ = ecallmgr_fs_conferences:participant_destroy(CallId),
    'continue';
process_participant_event(<<"mute-detect">>, _, _, _) -> 'stop';
process_participant_event(<<"dtmf">>, Props, _, _) ->
    %% Note: These are the digits dialed by the participant
    %%    not to be confused with dtmf-member
    {'continue', [{<<"DTMF-Digit">>, props:get_value(<<"DTMF-Key">>, Props)}
                  ,{<<"DTMF-Data">>, props:get_value(<<"Data">>, Props)} %% ??
                 ]};
process_participant_event(<<"energy-level">>, Props, _, CallId) ->
    Level = props:get_integer_value(<<"New-Level">>, Props, 0),
    ecallmgr_fs_conferences:participant_update(CallId, [{#participant.energy_level, Level}]),
    'stop';
process_participant_event(<<"volume-level">>, Props, _, CallId) ->
    Level = props:get_integer_value(<<"New-Level">>, Props, 0),
    ecallmgr_fs_conferences:participant_update(CallId, [{#participant.volume_level, Level}]),
    'stop';
process_participant_event(<<"gain-level">>, Props, _, CallId) ->
    Level = props:get_integer_value(<<"New-Level">>, Props, 0),
    ecallmgr_fs_conferences:participant_update(CallId, [{#participant.gain_level, Level}]),
    'stop';
process_participant_event(<<"energy-level-member">>, _, _, _) ->
    %% EnergyLevel = props:get_value(<<"Energy-Level">>, Props),
    'stop';
process_participant_event(<<"volume-in-member">>, _, _, _) ->
    %% VolumeLevel = props:get_value(<<"Volume-Level">>, Props),
    'stop';
process_participant_event(<<"volume-out-member">>, _, _, _) ->
    %% VolumeLevel = props:get_value(<<"Volume-Level">>, Props),
    'stop';
process_participant_event(<<"dtmf-member">>, _, _, _) ->
    %% Note: these are digits sent by the API or other means,
    %%    and not those dialed by the participant
    %% Digits = props:get_value(<<"Digits">>, props),
    'stop';
process_participant_event(<<"transfer">>, _, _, _) ->
    %% case props:get_value(<<"Dialplan">>, Props) of
    %%     'undefined' ->
    %%         OldConferenceName = props:get_value(<<"Old-Conference-Name">>, Props),
    %%         NewConferenceName = props:get_value(<<"New-Conference-Name">>, Props);
    %%     Dialplan -> 'stop'
    %% end,
    'stop';
process_participant_event(<<"execute_app">>, _, _, _) ->
    %% Application = props:get_value(<<"Application">>, Props),
    'stop';
process_participant_event(<<"play-file-member">> = Event, Props, _, _) ->
    {'continue', [{<<"Event-Name">>, <<"CHANNEL_EXECUTE">>}
                  ,{<<"whistle_event_name">>, <<"CHANNEL_EXECUTE">>}
                  ,{<<"Application">>, Event}
                  ,{<<"whistle_application_name">>, Event}
                  ,{<<"Application-Data">>, props:get_value(<<"File">>, Props)}
                  ,{<<"Asynchronous-Playback">>, props:get_is_true(<<"Async">>, Props)}
                 ]};
process_participant_event(<<"play-file-member-done">> = Event, Props, _, _) ->
    {'continue', [{<<"Event-Name">>, <<"CHANNEL_EXECUTE_COMPLETE">>}
                  ,{<<"whistle_event_name">>, <<"CHANNEL_EXECUTE_COMPLETE">>}
                  ,{<<"Application">>, Event}
                  ,{<<"whistle_application_name">>, Event}
                  ,{<<"Application-Data">>, props:get_value(<<"File">>, Props)}
                  ,{<<"Asynchronous-Playback">>, props:get_is_true(<<"Async">>, Props)}
                 ]};
process_participant_event(<<"speak-text-member">>, _, _, _) ->
    %% Text = props:get_value(<<"Text">>, Props),
    'stop';
process_participant_event(_, _, _, _) -> 'stop'.

-spec process_conference_event(ne_binary(), wh_proplist(), atom()) ->
                                      {'continue', wh_proplist()} |
                                      'continue' |
                                      'stop'.
process_conference_event(<<"conference-create">>, Props, Node) ->
    _ = ecallmgr_fs_conferences:create(Props, Node),
    'continue';
process_conference_event(<<"conference-destroy">>, Props, _) ->
    UUID = props:get_value(<<"Conference-Unique-ID">>, Props),
    _ = ecallmgr_fs_conferences:destroy(UUID),
    'continue';
process_conference_event(<<"floor-change">>, Props, _) ->
    UUID = props:get_value(<<"Conference-Unique-ID">>, Props),
    WithFloor = safe_integer_get(<<"New-ID">>, Props),
    LostFloor = safe_integer_get(<<"Old-ID">>, Props),
    _ = ecallmgr_fs_conferences:update(UUID, [{#conference.with_floor, WithFloor}
                                              ,{#conference.lost_floor, LostFloor}
                                             ]),
    {'continue', [{<<"With-Floor">>, ecallmgr_fs_conferences:participant_callid(UUID, WithFloor)}
                  ,{<<"Lost-Floor">>, ecallmgr_fs_conferences:participant_callid(UUID, LostFloor)}
                 ]};
process_conference_event(<<"play-file">> = Event, Props, _) ->
    {'continue', [{<<"Event-Name">>, <<"CHANNEL_EXECUTE">>}
                  ,{<<"whistle_event_name">>, <<"CHANNEL_EXECUTE">>}
                  ,{<<"Application">>, Event}
                  ,{<<"whistle_application_name">>, Event}
                  ,{<<"Application-Data">>, props:get_value(<<"File">>, Props)}
                  ,{<<"Asynchronous-Playback">>, props:get_is_true(<<"Async">>, Props)}
                 ]};
process_conference_event(<<"play-file-done">> = Event, Props, _) ->
    {'continue', [{<<"Event-Name">>, <<"CHANNEL_EXECUTE_COMPLETE">>}
                  ,{<<"whistle_event_name">>, <<"CHANNEL_EXECUTE_COMPLETE">>}
                  ,{<<"Application">>, Event}
                  ,{<<"whistle_application_name">>, Event}
                  ,{<<"Application-Data">>, props:get_value(<<"File">>, Props)}
                  ,{<<"Asynchronous-Playback">>, props:get_is_true(<<"Async">>, Props)}
                 ]};
process_conference_event(<<"lock">>, _, _) -> 'continue';
process_conference_event(<<"unlock">>, _, _) -> 'continue';
process_conference_event(<<"speak-text">>, _, _) ->
    %% Test = props:get_value(<<"Text">>, Props),
    'stop';
process_conference_event(<<"exit-sounds-on">>, _, _) -> 'stop';
process_conference_event(<<"exit-sounds-off">>, _, _) -> 'stop';
process_conference_event(<<"exit-sound-file-changed">>, _, _) -> 'stop';
process_conference_event(<<"enter-sounds-on">>, _, _) -> 'stop';
process_conference_event(<<"enter-sounds-off">>, _, _) -> 'stop';
process_conference_event(<<"enter-sound-file-changed">>, _, _) -> 'stop';
process_conference_event(<<"start-recording">> = Event, Props, _) ->
    {'continue', [{<<"Event-Name">>, <<"CHANNEL_EXECUTE">>}
                  ,{<<"whistle_event_name">>, <<"CHANNEL_EXECUTE">>}
                  ,{<<"Application">>, Event}
                  ,{<<"whistle_application_name">>, Event}
                  ,{<<"Application-Data">>, props:get_value(<<"Path">>, Props)}
                 ]};
process_conference_event(<<"pause-recording">>, _, _) ->
    %% Path = props:get_value(<<"Path">>, Props),
    %% OtherRecordings = props:get_is_true(<<"Other-Recordings">>, Props),
    'stop';
process_conference_event(<<"resume-recording">>, _, _) ->
    %% Path = props:get_value(<<"Path">>, Props),
    %% OtherRecordings = props:get_is_true(<<"Other-Recordings">>, Props),
    'stop';
process_conference_event(<<"stop-recording">> = Event, Props, _) ->
    {'continue', [{<<"Event-Name">>, <<"CHANNEL_EXECUTE_COMPLETE">>}
                  ,{<<"whistle_event_name">>, <<"CHANNEL_EXECUTE_COMPLETE">>}
                  ,{<<"Application">>, Event}
                  ,{<<"whistle_application_name">>, Event}
                  ,{<<"Application-Data">>, props:get_value(<<"Path">>, Props)}
                  ,{<<"Other-Recordings">>, props:get_is_true(<<"Other-Recordings">>, Props)}
                 ]};
process_conference_event(<<"bgdial-result">>, _, _) ->
    %% Result = props:get_value(<<"Result">>, Props),
    %% JobUUID = props:get_value(<<"Job-UUID">>, Props),
    'stop';
process_conference_event(_, _, _) -> 'stop'.

-spec send_conference_event(ne_binary(), wh_proplist()) -> 'ok'.
send_conference_event(Action, Props) ->
    send_conference_event(Action, Props, []).

-spec send_conference_event(ne_binary(), wh_proplist(), wh_proplist()) -> 'ok'.
send_conference_event(Action, Props, CustomProps) ->
    ConferenceName =  props:get_value(<<"Conference-Name">>, Props),
    Event = [{<<"Event">>, Action}
             ,{<<"Conference-ID">>, ConferenceName}
             ,{<<"Instance-ID">>, props:get_value(<<"Conference-Unique-ID">>, Props)}
             ,{<<"Custom-Channel-Vars">>, wh_json:from_list(ecallmgr_util:custom_channel_vars(Props))}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    Publisher = fun(P) -> wapi_conference:publish_conference_event(ConferenceName, P) end,
    _ = wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL, Event, Publisher),
    %% TODO: After KAZOO-27 is accepted the relay should not be necessary
    relay_event(Event ++ CustomProps ++ props:delete_keys([<<"Event-Name">>, <<"Event-Subclass">>], Props)).

-spec send_participant_event(ne_binary(), ne_binary(), wh_proplist()) -> 'ok'.
send_participant_event(Action, CallId, Props) ->
    send_participant_event(Action, CallId, Props, []).

-spec send_participant_event(ne_binary(), ne_binary(), wh_proplist(), wh_proplist()) -> 'ok'.
send_participant_event(Action, CallId, Props, CustomProps) ->
    ConferenceName =  props:get_value(<<"Conference-Name">>, Props),
    Event = [{<<"Event">>, Action}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Conference-ID">>, ConferenceName}
             ,{<<"Instance-ID">>, props:get_value(<<"Conference-Unique-ID">>, Props)}
             ,{<<"Participant-ID">>, props:get_integer_value(<<"Member-ID">>, Props, 0)}
             ,{<<"Floor">>, props:get_is_true(<<"Floor">>, Props, 'false')}
             ,{<<"Hear">>, props:get_is_true(<<"Hear">>, Props, 'true')}
             ,{<<"Speak">>, props:get_is_true(<<"Speak">>, Props, 'true')}
             ,{<<"Talking">>, props:get_is_true(<<"Talking">>, Props, 'false')}
             ,{<<"Current-Energy">>, props:get_integer_value(<<"Current-Energy">>, Props, 0)}
             ,{<<"Energy-Level">>, props:get_integer_value(<<"Energy-Level">>, Props, 0)}
             ,{<<"Video">>, props:get_is_true(<<"Video">>, Props, 'false')}
             ,{<<"Mute-Detect">>, props:get_is_true(<<"Must-Detect">>, Props, 'false')}
             ,{<<"Caller-ID-Name">>, props:get_value(<<"Caller-Caller-ID-Name">>, Props)}
             ,{<<"Caller-ID-Number">>, props:get_value(<<"Caller-Caller-ID-Number">>, Props)}
             ,{<<"Channel-Presence-ID">>, props:get_value(<<"Channel-Presence-ID">>, Props)}
             ,{<<"Custom-Channel-Vars">>, wh_json:from_list(ecallmgr_util:custom_channel_vars(Props))}
             %% NOTE: Participant-ID is depreciated, use call-id instead
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    %% TODO: After KAZOO-27 is accepted the relay should not be necessary and we can
    %%    use publish_participant_event
    %% Publisher = fun(P) -> wapi_conference:publish_participant_event(ConferenceName, P) end,
    %% _ = wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL, Event, Publisher),
    relay_event(Event ++ CustomProps ++ props:delete_keys([<<"Event-Name">>, <<"Event-Subclass">>], Props)).

-spec exec(atom(), ne_binary(), wh_json:object()) -> 'ok'.
exec(Focus, ConferenceId, JObj) ->
    App = wh_json:get_value(<<"Application-Name">>, JObj),
    lager:debug("asked to exec ~s for ~s", [App, ConferenceId]),
    case get_conf_command(App, Focus, ConferenceId, JObj) of
        {'error', _Msg}=E ->
            lager:debug("command ~s failed: ~s", [App, _Msg]),
            send_response(App, E, wh_json:get_value(<<"Server-ID">>, JObj), JObj);
        {'noop', Conference} ->
            send_response(App, {'noop', Conference}, wh_json:get_value(<<"Server-ID">>, JObj), JObj);
        {<<"play">>, AppData} ->
            Result =
                case wh_json:get_value(<<"Call-ID">>, JObj) of
                    'undefined' ->
                        Command = list_to_binary([ConferenceId, " play ", AppData]),
                        Focus =/= 'undefined' andalso lager:debug("execute on node ~s: conference ~s", [Focus, Command]),
                        lager:debug("api to ~s: conference ~s", [Focus, Command]),
                        freeswitch:api(Focus, 'conference', Command);
                    CallId ->
                        Command = wh_util:to_list(list_to_binary(["uuid:", CallId
                                                                  ," conference ", ConferenceId
                                                                  ," play ", AppData
                                                                 ])),
                        Focus =/= 'undefined' andalso lager:debug("execute on node ~s: conference ~s", [Focus, Command]),
                        lager:debug("api to ~s: expand ~s", [Focus, Command]),
                        freeswitch:api(Focus, 'expand', Command)
                end,
            send_response(App, Result, wh_json:get_value(<<"Server-ID">>, JObj), JObj);
        {AppName, AppData} ->
            Command = wh_util:to_list(list_to_binary([ConferenceId, " ", AppName, " ", AppData])),
            Focus =/= 'undefined' andalso lager:debug("execute on node ~s: conference ~s", [Focus, Command]),
            lager:debug("api to ~s: conference ~s", [Focus, Command]),
            Result = freeswitch:api(Focus, 'conference', Command),
            send_response(App, Result, wh_json:get_value(<<"Server-ID">>, JObj), JObj)
    end.

-spec get_conf_command(ne_binary(), atom(), ne_binary(), wh_json:object()) ->
                              {ne_binary(), binary()} |
                              {'error', ne_binary()} |
                              {'noop', wh_json:object()}.
%% The following conference commands can operate on the entire conference
get_conf_command(<<"participants">>, 'undefined', ConferenceId, _) ->
    {'error', <<"Non-Existant ID ", ConferenceId/binary>>};
get_conf_command(<<"participants">>, _Focus, ConferenceId, JObj) ->
    case wapi_conference:participants_req_v(JObj) of
        'false' -> {'error', <<"conference participants failed to execute as JObj did not validate.">>};
        'true' ->
            case ecallmgr_fs_conferences:participants(ConferenceId) of
                [] ->
                    {'error', <<"conference participants are not ready to be listed, or are none">>};
                Participants ->
                    JObjs = ecallmgr_fs_conferences:participants_to_json(Participants),
                    {'noop', wh_json:from_list([{<<"Participants">>, JObjs}])}
            end
    end;
get_conf_command(<<"lock">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:lock_v(JObj) of
        'false' ->
            {'error', <<"conference lock failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"lock">>, <<>>}
    end;
get_conf_command(<<"unlock">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:unlock_v(JObj) of
        'false' ->
            {'error', <<"conference unlock failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"unlock">>, <<>>}
    end;
get_conf_command(<<"record">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:record_v(JObj) of
        'false' ->
            {'error', <<"conference record failed to execute as JObj did not validate.">>};
        'true' ->
            MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
            RecordingName = ecallmgr_util:recording_filename(MediaName),
            {<<"recording">>, [<<"start ">>, RecordingName]}
    end;
get_conf_command(<<"recordstop">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:recordstop_v(JObj) of
        'false' -> {'error', <<"conference recordstop failed validation">>};
        'true' ->
            MediaName = ecallmgr_util:recording_filename(wh_json:get_binary_value(<<"Media-Name">>, JObj)),
            {<<"recording">>, [<<"stop ">>, MediaName]}
    end;
get_conf_command(<<"tones">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:tones_v(JObj) of
        'false' -> {'error', <<"conference tones failed to validate">>};
        'true' ->
            Tones = wh_json:get_value(<<"Tones">>, JObj, []),
            FSTones = [begin
                           Vol = case wh_json:get_value(<<"Volume">>, Tone) of
                                     'undefined' -> [];
                                     %% need to map V (0-100) to FS values
                                     V -> list_to_binary(["v=", wh_util:to_list(V), ";"])
                                 end,
                           Repeat = case wh_json:get_value(<<"Repeat">>, Tone) of
                                        'undefined' -> [];
                                        R -> list_to_binary(["l=", wh_util:to_list(R), ";"])
                                    end,
                           Freqs = string:join([ wh_util:to_list(V) || V <- wh_json:get_value(<<"Frequencies">>, Tone) ], ","),
                           On = wh_util:to_list(wh_json:get_value(<<"Duration-ON">>, Tone)),
                           Off = wh_util:to_list(wh_json:get_value(<<"Duration-OFF">>, Tone)),
                           wh_util:to_list(list_to_binary([Vol, Repeat, "%(", On, ",", Off, ",", Freqs, ")"]))
                       end || Tone <- Tones],
            Arg = [$t,$o,$n,$e,$_,$s,$t,$r,$e,$a,$m,$:,$/,$/ | string:join(FSTones, ";")],
            {<<"play">>, Arg}
    end;
%% The following conference commands can optionally specify a participant
get_conf_command(<<"play">>, _Focus, ConferenceId, JObj) ->
    case wapi_conference:play_v(JObj) of
        'false' ->
            {'error', <<"conference play failed to execute as JObj did not validate.">>};
        'true' ->
            UUID = wh_json:get_ne_value(<<"Call-ID">>, JObj, ConferenceId),
            Media = list_to_binary(["'", ecallmgr_util:media_path(wh_json:get_value(<<"Media-Name">>, JObj), UUID, JObj), "'"]),
            Args = case wh_json:get_binary_value(<<"Participant">>, JObj) of
                       'undefined' -> Media;
                       Participant -> list_to_binary([Media, " ", Participant])
                   end,
            {<<"play">>, Args}
    end;
get_conf_command(<<"stop_play">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:stop_play_v(JObj) of
        'false' ->
            {'error', <<"conference stop_play failed to execute as JObj did not validate.">>};
        'true' ->
            Affects = wh_json:get_binary_value(<<"Affects">>, JObj, <<"all">>),
            Args = case wh_json:get_binary_value(<<"Participant">>, JObj) of
                       undefined -> Affects;
                       Participant -> list_to_binary([Affects, " ", Participant])
                   end,
            {<<"stop">>, Args}
    end;
get_conf_command(Say, _Focus, _ConferenceId, JObj) when Say =:= <<"say">> orelse Say =:= <<"tts">> ->
    case wapi_conference:say_v(JObj) of
        'false' -> {'error', <<"conference say failed to validate">>};
        'true'->
            SayMe = wh_json:get_value(<<"Text">>, JObj),

            case wh_json:get_binary_value(<<"Participant">>, JObj) of
                'undefined' -> {<<"say">>, ["'", SayMe, "'"]};
                Id -> {<<"saymember">>, [Id, " '", SayMe, "'"]}
            end
    end;
%% The following conference commands require a participant
get_conf_command(<<"kick">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:kick_v(JObj) of
        'false' ->
            {'error', <<"conference kick failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"hup">>, wh_json:get_binary_value(<<"Participant">>, JObj, <<"last">>)}
    end;
get_conf_command(<<"mute_participant">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:mute_participant_v(JObj) of
        'false' ->
            {'error', <<"conference mute_participant failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"mute">>, wh_json:get_binary_value(<<"Participant">>, JObj, <<"last">>)}
    end;
get_conf_command(<<"deaf_participant">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:deaf_participant_v(JObj) of
        'false' ->
            {'error', <<"conference deaf_participant failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"deaf">>, wh_json:get_binary_value(<<"Participant">>, JObj)}
    end;
get_conf_command(<<"participant_energy">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:participant_energy_v(JObj) of
        'false' ->
            {'error', <<"conference participant_energy failed to execute as JObj did not validate.">>};
        'true' ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Energy-Level">>, JObj, <<"20">>)
                                  ]),
            {<<"energy">>, Args}
    end;
get_conf_command(<<"relate_participants">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:relate_participants_v(JObj) of
        'false' ->
            {'error', <<"conference relate_participants failed to execute as JObj did not validate.">>};
        'true' ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Other-Participant">>, JObj)
                                   ," ", relationship(wh_json:get_binary_value(<<"Relationship">>, JObj))
                                  ]),
            {<<"relate">>, Args}
    end;
get_conf_command(<<"set">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:set_v(JObj) of
        'false' ->
            {'error', <<"conference set failed to execute as JObj did not validate.">>};
        'true' ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Parameter">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Value">>, JObj)
                                  ]),
            {<<"set">>, Args}
    end;
get_conf_command(<<"undeaf_participant">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:undeaf_participant_v(JObj) of
        'false' ->
            {'error', <<"conference undeaf_participant failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"undeaf">>, wh_json:get_binary_value(<<"Participant">>, JObj)}
    end;
get_conf_command(<<"unmute_participant">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:unmute_participant_v(JObj) of
        'false' ->
            {'error', <<"conference unmute failed to execute as JObj did not validate.">>};
        'true' ->
            {<<"unmute">>, wh_json:get_binary_value(<<"Participant">>, JObj)}
    end;
get_conf_command(<<"participant_volume_in">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:participant_volume_in_v(JObj) of
        'false' ->
            {'error', <<"conference participant_volume_in failed to execute as JObj did not validate.">>};
        'true' ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Volume-In-Level">>, JObj, <<"0">>)
                                  ]),
            {<<"volume_in">>, Args}
    end;
get_conf_command(<<"participant_volume_out">>, _Focus, _ConferenceId, JObj) ->
    case wapi_conference:participant_volume_out_v(JObj) of
        'false' ->
            {'error', <<"conference participant_volume_out failed to execute as JObj did not validate.">>};
        'true' ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Volume-Out-Level">>, JObj, <<"0">>)
                                  ]),
            {<<"volume_out">>, Args}
    end;
get_conf_command(Cmd, _Focus, _ConferenceId, _JObj) ->
    lager:debug("unknown conference command ~s", [Cmd]),
    {'error', list_to_binary([<<"unknown conference command: ">>, Cmd])}.

-spec send_response(ne_binary(), tuple(), api_binary(), wh_json:object()) -> 'ok'.
send_response(<<"stop_play">>, {'ok', Res}, _Queue, Command) ->
    Evt = [{<<"Conference-Name">>, wh_json:get_value(<<"Conference-ID">>, Command)}
           ,{<<"Event-Date-Timestamp">>, wh_util:current_tstamp()}
           ,{<<"Action">>,<<"play-file-done">>}
           ,{<<"Event-Name">>, <<"CHANNEL_EXECUTE_COMPLETE">>}
           ,{<<"whistle_event_name">>, <<"CHANNEL_EXECUTE_COMPLETE">>}
           ,{<<"Application">>, <<"play-file-done">>}
           ,{<<"whistle_application_name">>, <<"play-file-done">>}
           ,{<<"Application-Data">>, Res}
          ],
    relay_event(Evt);
send_response(_, _, 'undefined', _) -> lager:debug("no server-id to respond");
send_response(_, {'ok', <<"Non-Existant ID", _/binary>> = Msg}, RespQ, Command) ->
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
             ,{<<"Error-Message">>, binary:replace(Msg, <<"\n">>, <<>>)}
             ,{<<"Request">>, Command}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    lager:debug("error in conference command: ~s", [Msg]),
    wapi_conference:publish_error(RespQ, Error);
send_response(<<"participants">>, {'noop', Conference}, RespQ, Command) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
            ,{<<"Participants">>, wh_json:get_value(<<"Participants">>, Conference, wh_json:new())}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_conference:publish_participants_resp(RespQ, Resp);
send_response(_, {'ok', Response}, RespQ, Command) ->
    case binary:match(Response, <<"not found">>) of
        'nomatch' -> 'ok';
        _Else ->
            Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
                     ,{<<"Error-Message">>, binary:replace(Response, <<"\n">>, <<>>)}
                     ,{<<"Request">>, Command}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            wapi_conference:publish_error(RespQ, Error)
    end;
send_response(_, {'error', Msg}, RespQ, Command) ->
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
             ,{<<"Error-Message">>, binary:replace(wh_util:to_binary(Msg), <<"\n">>, <<>>)}
             ,{<<"Request">>, Command}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    wapi_conference:publish_error(RespQ, Error);
send_response(_, 'timeout', RespQ, Command) ->
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
             ,{<<"Error-Message">>, <<"Node Timeout">>}
             ,{<<"Request">>, Command}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    wapi_conference:publish_error(RespQ, Error).

-spec relationship(ne_binary()) -> ne_binary().
relationship(<<"mute">>) -> <<"nospeak">>;
relationship(<<"deaf">>) -> <<"nohear">>;
relationship(_) -> <<"clear">>.

-spec safe_integer_get(any(), wh_proplist()) -> non_neg_integer().
safe_integer_get(Key, Props) ->
    safe_integer_get(Key, Props, 0).

-spec safe_integer_get(any(), wh_proplist(), any()) -> any().
safe_integer_get(Key, Props, Default) ->
    try props:get_integer_value(Key, Props, Default) of
        Value -> Value
    catch
        'error':'badarg' -> Default
    end.

-spec relay_event(wh_proplist()) -> 'ok'.
relay_event(Props) ->
    ConferenceName = props:get_value(<<"Conference-Name">>, Props),
    [relay_event(UUID, Node, Props)
     || #participant{uuid=UUID, node=Node} <- ecallmgr_fs_conferences:participants(ConferenceName)
    ],
    'ok'.

-spec relay_event(ne_binary(), atom(), wh_proplist()) -> 'ok'.
relay_event(UUID, Node, Props) ->
    EventName = props:get_first_defined([<<"Event">>, <<"Event-Name">>], Props),
    Application = props:get_first_defined([<<"Application">>, <<"Action">>], Props),
    lager:debug("relaying conf event ~s(~s) to ~s", [EventName, Application, UUID]),
    Payload = {'event', [UUID, {<<"Caller-Unique-ID">>, UUID} | Props]},
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, Payload),
    gproc:send({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, UUID)}, Payload).

%% TODO: this can be removed once the KAZOO-27 is accepted
-spec publish_new_participant_event(wh_proplist(), atom()) -> 'ok'.
publish_new_participant_event(Props, Node) ->
    ConferenceName = props:get_value(<<"Conference-Name">>, Props),
    Participants = ecallmgr_fs_conferences:participants(ConferenceName),
    Event = [{<<"Participants">>, ecallmgr_fs_conferences:participants_to_json(Participants)}
             ,{<<"Focus">>, wh_util:to_binary(Node)}
             ,{<<"Conference-ID">>, ConferenceName}
             ,{<<"Instance-ID">>, props:get_value(<<"Conference-Unique-ID">>, Props)}
             ,{<<"Switch-Hostname">>, props:get_value(<<"FreeSWITCH-Hostname">>, Props, wh_util:to_binary(Node))}
             ,{<<"Switch-URL">>, ecallmgr_fs_nodes:sip_url(Node)}
             ,{<<"Switch-External-IP">>, ecallmgr_fs_nodes:sip_external_ip(Node)}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    Publisher = fun(P) -> wapi_conference:publish_participants_event(ConferenceName, P) end,
    wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                        ,Event
                        ,Publisher
                       ).

%% TODO: this can be removed once the KAZOO-27 is accepted
-spec publish_participant_destroy_event(wh_proplist(), atom()) -> 'ok'.
publish_participant_destroy_event(Props, Node) ->
    ConferenceName = props:get_value(<<"Conference-Name">>, Props),
    Participants = ecallmgr_fs_conferences:participants(ConferenceName),
    Event = [{<<"Participants">>, ecallmgr_fs_conferences:participants_to_json(Participants)}
             ,{<<"Focus">>, wh_util:to_binary(Node)}
             ,{<<"Conference-ID">>, ConferenceName}
             ,{<<"Instance-ID">>, props:get_value(<<"Conference-Unique-ID">>, Props)}
             ,{<<"Switch-Hostname">>, props:get_value(<<"FreeSWITCH-Hostname">>, Props, wh_util:to_binary(Node))}
             ,{<<"Switch-URL">>, ecallmgr_fs_nodes:sip_url(Node)}
             ,{<<"Switch-External-IP">>, ecallmgr_fs_nodes:sip_external_ip(Node)}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    Publisher = fun(P) -> wapi_conference:publish_participants_event(ConferenceName, P) end,
    wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                        ,Event
                        ,Publisher
                       ).

