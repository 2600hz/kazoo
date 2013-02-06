%%%-------------------------------------------------------------------
%%% @copyright (C) 2013 2600Hz Inc
%%% @doc
%%% Conference participant process
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(conf_participant).

-behaviour(gen_listener).

-include("conference.hrl").

%% API
-export([start_link/1]).
-export([relay_amqp/2]).
-export([handle_participants_resp/2]).
-export([handle_conference_error/2]).
-export([handle_authn_req/2]).
-export([handle_route_req/2, handle_route_win/2]).

-export([consume_call_events/1]).
-export([conference/1, set_conference/2]).
-export([discovery_event/1, set_discovery_event/2]).
-export([call/1]).

-export([join_local/1]).

-export([mute/1, unmute/1, toggle_mute/1]).
-export([deaf/1, undeaf/1, toggle_deaf/1]).
-export([hangup/1]).
-export([dtmf/2]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-export([join_conference/3]).

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{{?MODULE, relay_amqp}
                      ,[{<<"call_event">>, <<"*">>}]
                     }
                     ,{{?MODULE, handle_participants_resp}
                       ,[{<<"conference">>, <<"participants_resp">>}]
                      }
                     ,{{?MODULE, handle_conference_error}
                       ,[{<<"conference">>, <<"error">>}]
                      }
                     ,{{?MODULE, handle_authn_req}
                       ,[{<<"directory">>, <<"authn_req">>}]
                      }
                     ,{{?MODULE, handle_route_req}
                       ,[{<<"dialplan">>, <<"route_req">>}]
                      }
                     ,{{?MODULE, handle_route_win}
                       ,[{<<"dialplan">>, <<"route_win">>}]
                      }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(participant, {participant_id = 0 :: non_neg_integer()
                      ,call :: whapps_call:call()
                      ,bridge
                      ,bridge_request
                      ,moderator = 'false' :: boolean()
                      ,muted = 'false' :: boolean()
                      ,deaf = 'false' :: boolean()
                      ,waiting_for_mod = 'false' :: boolean()
                      ,call_event_consumers = [] :: list()
                      ,in_conference = 'false' :: boolean()
                      ,join_attempts = 0 :: integer()
                      ,conference = whapps_conference:new()
                      ,discovery_event = wh_json:new()
                      ,last_dtmf = <<>> :: binary()
                     }).
-type participant() :: #participant{}.

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
start_link(Call) ->
    CallId = whapps_call:call_id(Call),
    Bindings = [{call, [{callid, CallId}]}
                ,{self, []}
               ],
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
                                      ,{bindings, Bindings}
                                      ,{queue_name, ?QUEUE_NAME}
                                      ,{queue_options, ?QUEUE_OPTIONS}
                                      ,{consume_options, ?CONSUME_OPTIONS}
                                     ], [Call]).

-spec conference(pid()) -> {ok, whapps_conference:conference()}.
conference(Srv) -> gen_listener:call(Srv, {get_conference}, 500).

-spec set_conference(whapps_conference:conference(), pid()) -> 'ok'.
set_conference(Conference, Srv) ->
    gen_listener:cast(Srv, {set_conference, Conference}).

-spec discovery_event(pid()) -> {ok, wh_json:object()}.
discovery_event(Srv) ->
    gen_listener:call(Srv, {get_discovery_event}, 500).

-spec set_discovery_event(wh_json:object(), pid()) -> 'ok'.
set_discovery_event(DiscoveryEvent, Srv) ->
    gen_listener:cast(Srv, {set_discovery_event, DiscoveryEvent}).

-spec call(pid()) -> {ok, whapps_call:call()}.
call(Srv) -> gen_listener:call(Srv, {get_call}, 500).

-spec join_local(pid()) -> 'ok'.
join_local(Srv) -> gen_listener:cast(Srv, join_local).

-spec mute(pid()) -> 'ok'.
mute(Srv) -> gen_listener:cast(Srv, mute).

-spec unmute(pid()) -> 'ok'.
unmute(Srv) -> gen_listener:cast(Srv, unmute).

-spec toggle_mute(pid()) -> 'ok'.
toggle_mute(Srv) -> gen_listener:cast(Srv, toggle_mute).

-spec deaf(pid()) -> 'ok'.
deaf(Srv) -> gen_listener:cast(Srv, deaf).

-spec undeaf(pid()) -> 'ok'.
undeaf(Srv) -> gen_listener:cast(Srv, undeaf).

-spec toggle_deaf(pid()) -> 'ok'.
toggle_deaf(Srv) -> gen_listener:cast(Srv, toggle_deaf).

-spec hangup(pid()) -> 'ok'.
hangup(Srv) -> gen_listener:cast(Srv, hangup).

-spec dtmf(pid(), ne_binary()) -> 'ok'.
dtmf(Srv, Digit) -> gen_listener:cast(Srv,{dtmf, Digit}).

-spec consume_call_events(pid()) -> 'ok'.
consume_call_events(Srv) -> gen_listener:cast(Srv, {add_consumer, self()}).

-spec relay_amqp(wh_json:object(), wh_proplist()) -> 'ok'.
relay_amqp(JObj, Props) ->
    _ = [whapps_call_command:relay_event(Pid, JObj)
         || Pid <- props:get_value(call_event_consumers, Props, [])
                ,is_pid(Pid)
        ],
    Digit = wh_json:get_value(<<"DTMF-Digit">>, JObj),
    case is_binary(Digit) andalso props:get_value(in_conference, Props, false) of
        false -> ok;
        true ->
            Srv = props:get_value(server, Props),
            dtmf(Srv, Digit)
    end.

-spec handle_participants_resp(wh_json:object(), wh_proplist()) -> 'ok'.
handle_participants_resp(JObj, Props) ->
    true = wapi_conference:participants_resp_v(JObj),
    Srv = props:get_value(server, Props),

    lager:debug("participants resp: ~p", [JObj]),

    Participants = wh_json:get_value(<<"Participants">>, JObj, wh_json:new()),
    gen_listener:cast(Srv, {sync_participant, Participants}).

-spec handle_conference_error(wh_json:object(), wh_proplist()) -> 'ok'.
handle_conference_error(JObj, Props) ->
    true = wapi_conference:conference_error_v(JObj),

    lager:debug("conference error: ~p", [JObj]),

    case wh_json:get_value([<<"Request">>, <<"Application-Name">>], JObj) of
        <<"participants">> ->
            Srv = props:get_value(server, Props),
            gen_listener:cast(Srv, {sync_participant, wh_json:new()});
        _Else ->
            ok
    end.

-spec handle_authn_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_authn_req(JObj, Props) ->
    true = wapi_authn:req_v(JObj),
    BridgeRequest = props:get_value(bridge_request, Props),
    case wh_json:get_value(<<"Method">>, JObj) =:= <<"INVITE">>
        andalso binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>) of
        [BridgeRequest, _] ->
            Srv = props:get_value(server, Props),
            gen_listener:cast(Srv, {authn_req, JObj});
        _Else -> ok
    end.

-spec handle_route_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_route_req(JObj, Props) ->
    true = wapi_route:req_v(JObj),
    BridgeRequest = props:get_value(bridge_request, Props),
    case binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>) of
        [BridgeRequest, _] ->
            Srv = props:get_value(server, Props),
            gen_listener:cast(Srv, {route_req, JObj});
        _Else -> ok
    end.

-spec handle_route_win(wh_json:object(), wh_proplist()) -> 'ok'.
handle_route_win(JObj, Props) ->
    true = wapi_route:win_v(JObj),
    Srv = props:get_value(server, Props),
    gen_listener:cast(Srv, {route_win, JObj}).

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
init([Call]) ->
    process_flag(trap_exit, true),
    put(callid, whapps_call:call_id(Call)),
    _ = get_my_queue(),
    {ok, #participant{call=Call}}.

get_my_queue() ->
    get_my_queue(self()).
get_my_queue(Srv) ->
    spawn(fun() ->
                  ControllerQ = gen_listener:queue_name(Srv),
                  gen_listener:cast(Srv, {controller_queue, ControllerQ})
          end).

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
handle_call({get_conference}, _, #participant{conference=Conf}=P) ->
    {reply, {ok, Conf}, P};
handle_call({get_discovery_event}, _, #participant{discovery_event=DE}=P) ->
    {reply, {ok, DE}, P};
handle_call({get_call}, _, #participant{call=Call}=P) ->
    {reply, {ok, Call}, P};
handle_call(_Request, _, P) ->
    {reply, {error, unimplemented}, P}.

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
handle_cast(hungup, Participant) ->
    {stop, {shutdown, hungup}, Participant};

handle_cast({controller_queue, undefined}, Participant) ->
    _ = get_my_queue(),
    {noreply, Participant};
handle_cast({controller_queue, Q}, #participant{call=Call}=P) ->
    {noreply, P#participant{call=whapps_call:set_controller_queue(Q, Call)}};

handle_cast({add_consumer, C}, #participant{call_event_consumers=Cs}=P) ->
    lager:debug("adding call event consumer ~p", [C]),
    link(C),
    {noreply, P#participant{call_event_consumers=[C|Cs]}};
handle_cast({remove_consumer, C}, #participant{call_event_consumers=Cs}=P) ->
    lager:debug("removing call event consumer ~p", [C]),
    {noreply, P#participant{call_event_consumers=[C1 || C1 <- Cs, C=/=C1]}};
handle_cast({set_conference, C}, P) ->
    lager:debug("received conference data for conference ~s", [whapps_conference:id(C)]),
    {noreply, P#participant{conference=C}};

handle_cast({set_discovery_event, DE}, #participant{}=Participant) ->
    {noreply, Participant#participant{discovery_event=DE}};

handle_cast(join_local, #participant{call=Call
                                     ,conference=Conference
                                    }=Participant) ->
    MyQ = whapps_call:controller_queue(Call),
    Conference1 = whapps_conference:set_controller_queue(MyQ, Conference),
    Self = self(),
    _ = spawn(?MODULE, join_conference, [Self, Call, Conference1]),
    {noreply, Participant#participant{conference=Conference1}};

handle_cast({sync_participant, Participants}, #participant{bridge='undefined'
                                                           ,call=Call
                                                          }=Participant) ->
    {noreply, sync_participant(Participants, Call, Participant)};
handle_cast({sync_participant, Participants}, #participant{bridge=Bridge}=Participant) ->
    {noreply, sync_participant(Participants, Bridge, Participant)};
handle_cast(play_member_entry, #participant{conference=Conference}=Participant) ->
    _ = whapps_conference:play_entry_tone(Conference) andalso
        whapps_conference_command:play(<<"tone_stream://%(200,0,500,600,700)">>, Conference),
    {noreply, Participant};
handle_cast(play_moderator_entry, #participant{conference=Conference}=Participant) ->
    _ = whapps_conference:play_entry_tone(Conference) andalso
        whapps_conference_command:play(<<"tone_stream://%(200,0,500,600,700)">>, Conference),
    {noreply, Participant};
handle_cast({dtmf, Digit}, #participant{last_dtmf = <<"*">>}=Participant) ->
    case Digit of
        <<"1">> -> toggle_mute(self());
        <<"2">> -> mute(self());
        <<"3">> -> unmute(self());
        <<"4">> -> toggle_deaf(self());
        <<"5">> -> deaf(self());
        <<"6">> -> undeaf(self());
        <<"#">> -> hangup(self());
        _Else -> ok
    end,
    {noreply, Participant#participant{last_dtmf = Digit}};
handle_cast({dtmf, Digit}, Participant) ->
    {noreply, Participant#participant{last_dtmf = Digit}};
handle_cast(mute, #participant{participant_id=ParticipantId
                               ,conference=Conference
                              }=Participant) ->
    lager:debug("received in-conference command, muting participant ~s", [ParticipantId]),
    whapps_conference_command:mute_participant(ParticipantId, Conference),
    whapps_conference_command:prompt(<<"conf-muted">>, ParticipantId, Conference),
    {noreply, Participant#participant{muted='true'}};

handle_cast(unmute, #participant{participant_id=ParticipantId
                                 ,conference=Conference
                                }=Participant) ->
    lager:debug("received in-conference command, unmuting participant ~s", [ParticipantId]),
    whapps_conference_command:unmute_participant(ParticipantId, Conference),
    whapps_conference_command:prompt(<<"conf-unmuted">>, ParticipantId, Conference),
    {noreply, Participant#participant{muted='false'}};

handle_cast(toggle_mute, #participant{muted='true'}=Participant) ->
    unmute(self()),
    {noreply, Participant};
handle_cast(toggle_mute, #participant{muted='false'}=Participant) ->
    mute(self()),
    {noreply, Participant};
handle_cast(deaf, #participant{participant_id=ParticipantId
                               ,conference=Conference
                              }=Participant) ->
    lager:debug("received in-conference command, making participant ~s deaf", [ParticipantId]),
    whapps_conference_command:deaf_participant(ParticipantId, Conference),
    whapps_conference_command:prompt(<<"conf-deaf">>, ParticipantId, Conference),
    {noreply, Participant#participant{deaf='true'}};
handle_cast(undeaf, #participant{participant_id=ParticipantId
                                 ,conference=Conference
                                }=Participant) ->
    lager:debug("received in-conference command, making participant ~s undeaf", [ParticipantId]),
    whapps_conference_command:undeaf_participant(ParticipantId, Conference),
    whapps_conference_command:prompt(<<"conf-undeaf">>, ParticipantId, Conference),
    {noreply, Participant#participant{deaf='false'}};
handle_cast(toggle_deaf, #participant{deaf='true'}=Participant) ->
    undeaf(self()),
    {noreply, Participant};
handle_cast(toggle_deaf, #participant{deaf='false'}=Participant) ->
    deaf(self()),
    {noreply, Participant};
handle_cast(hangup, #participant{call=Call}=Participant) ->
    lager:debug("received in-conference command, hangup participant"),
    whapps_call_command:hangup(Call),
    {noreply, Participant};
handle_cast(_Cast, Participant) ->
    lager:debug("unhandled cast: ~p", [_Cast]),
    {noreply, Participant}.

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
handle_info({'EXIT', Consumer, _R}, #participant{call_event_consumers=Consumers}=Participant) ->
    lager:debug("call event consumer ~p died: ~p", [Consumer, _R]),

    Cs = [C || C <- Consumers, C =/= Consumer],

    {noreply, Participant#participant{call_event_consumers=Cs}, hibernate};
handle_info(_Msg, Participant) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {noreply, Participant}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_event(JObj, #participant{call_event_consumers=Consumers
                                ,call=Call
                                ,in_conference=InConf
                                ,bridge_request=BridgeRequest
                               }) ->
    CallId = whapps_call:call_id_direct(Call),
    case {whapps_util:get_event_type(JObj), wh_json:get_value(<<"Call-ID">>, JObj)} of
        {{<<"call_event">>, <<"CHANNEL_HANGUP">>}, CallId} ->
            lager:debug("received channel hangup event, terminate"),
            gen_listener:cast(self(), hungup),
            {reply, [{call_event_consumers, Consumers}]};
        {{<<"call_detail">>, <<"cdr">>}, CallId} ->
            lager:debug("received channel cdr event, terminate"),
            gen_listener:cast(self(), hungup),
            ignore;
        {{<<"call_event">>, <<"CHANNEL_DESTROY">>}, CallId} ->
            lager:debug("received channel destry, terminate"),
            gen_listener:cast(self(), hungup),
            {reply, [{call_event_consumers, Consumers}]};
        {{<<"call_event">>, _}, EventCallId} when EventCallId =/= CallId ->
            lager:debug("received event from call ~s while relaying for ~s, dropping", [EventCallId, CallId]),
            ignore;
        {_Else, _} ->
            {reply, [{call_event_consumers, Consumers}
                     ,{in_conference, InConf}
                     ,{bridge_request, BridgeRequest}
                    ]}
    end.

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
terminate(_Reason, #participant{conference=Conference
                                ,in_conference=InConference
                               }) ->
    InConference andalso whapps_conference_command:play(<<"tone_stream://%(500,0,300,200,100,50,25)">>, Conference),
    lager:debug("conference participant execution has been stopped: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, Participant, _Extra) ->
    {ok, Participant}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec find_participant(wh_proplist(), ne_binary()) ->
                                    {'ok', wh_json:object()} |
                                    {'error', 'not_found'}.
find_participant([], _) -> {error, not_found};
find_participant([Participant|Participants], CallId) ->
    case wh_json:get_value(<<"Call-ID">>, Participant) of
        CallId -> {ok, Participant};
        _Else -> find_participant(Participants, CallId)
    end;
find_participant(_, _CallId) -> {error, not_found}.


-spec sync_participant(wh_json:object(), whapps_call:call(), participant()) ->
                              participant().
sync_participant(Participants, Call, #participant{in_conference='false'
                                                  ,conference=Conference
                                                  ,join_attempts=JoinAttempts
                                                  ,discovery_event=DE
                                                 }=Participant
                ) ->
    Moderator = whapps_conference:moderator(Conference),

    lager:debug("participants: ~p", [Participants]),

    case find_participant(wh_json:to_proplist(Participants), whapps_call:call_id(Call)) of
        {ok, JObj} when Moderator ->
            ParticipantId = wh_json:get_value(<<"Participant-ID">>, JObj),
            lager:debug("caller has joined the local conference as moderator ~s", [ParticipantId]),
            Deaf = not wh_json:is_true(<<"Hear">>, JObj),
            Muted = not wh_json:is_true(<<"Speak">>, JObj),
            gen_listener:cast(self(), play_moderator_entry),
            whapps_conference:moderator_join_muted(Conference) andalso gen_listener:cast(self(), mute),
            whapps_conference:moderator_join_deaf(Conference) andalso gen_listener:cast(self(), deaf),


            notify_requestor(whapps_call:controller_queue(Call)
                             ,ParticipantId
                             ,DE
                             ,whapps_conference:id(Conference)
                            ),

            Participant#participant{in_conference='true'
                                    ,muted=Muted
                                    ,deaf=Deaf
                                    ,participant_id=ParticipantId
                                   };
        {ok, JObj} ->
            ParticipantId = wh_json:get_value(<<"Participant-ID">>, JObj),
            lager:debug("caller has joined the local conference as member ~p", [ParticipantId]),

            Deaf = not wh_json:is_true(<<"Hear">>, JObj),
            Muted = not wh_json:is_true(<<"Speak">>, JObj),

            gen_listener:cast(self(), play_member_entry),
            whapps_conference:member_join_muted(Conference) andalso gen_listener:cast(self(), mute),
            whapps_conference:member_join_deaf(Conference) andalso gen_listener:cast(self(), deaf),

            notify_requestor(whapps_call:controller_queue(Call)
                             ,ParticipantId
                             ,DE
                             ,whapps_conference:id(Conference)
                            ),

            Participant#participant{in_conference='true'
                                    ,muted=Muted
                                    ,deaf=Deaf
                                    ,participant_id=ParticipantId
                                   };
        {error, not_found} when JoinAttempts > 15 ->
            lager:debug("too many attempts to discover the participant id, assuming call lost"),
            gen_listener:cast(self(), hungup),
            Participant#participant{join_attempts = JoinAttempts + 1};
        {error, not_found} ->
            lager:debug("caller not found in the list of conference participants, trying again"),
            timer:sleep(500),
            whapps_conference_command:participants(Conference),
            Participant#participant{join_attempts = JoinAttempts + 1}
    end;
sync_participant(Participants, Call, #participant{in_conference=true}=Participant) ->
    lager:debug("participants: ~p", [Participants]),

    case find_participant(wh_json:to_proplist(Participants), whapps_call:call_id(Call)) of
        {ok, JObj} ->
            ParticipantId = wh_json:get_value(<<"Participant-ID">>, JObj),
            lager:debug("caller has is still in the conference as participant ~s", [ParticipantId]),
            Deaf = not wh_json:is_true(<<"Hear">>, JObj),
            Muted = not wh_json:is_true(<<"Speak">>, JObj),
            Participant#participant{in_conference='true'
                                    ,muted=Muted
                                    ,deaf=Deaf
                                    ,participant_id=ParticipantId
                                   };
        {error, not_found} ->
            lager:debug("participant is not present in conference anymore, terminating"),
            gen_listener:cast(self(), hungup),
            Participant#participant{in_conference='false'}
    end.

-spec join_conference(pid(), whapps_call:call(), whapps_conference:conference()) -> 'ok'.
join_conference(Srv, Call, Conference) ->
    put(callid, whapps_call:call_id(Call)),
    consume_call_events(Srv),

    lager:debug("answering conference call"),
    _ = whapps_call_command:b_answer(Call),
    ConferenceId = whapps_conference:id(Conference),

    _ = whapps_conference:play_entry_prompt(Conference) andalso
        whapps_call_command:prompt(<<"conf-joining_conference">>, Call),

    lager:debug("caller is a moderation: ~s and playing entry: ~s"
                ,[whapps_conference:moderator(Conference)
                  ,whapps_conference:play_entry_prompt(Conference)
                 ]
               ),

    _ = whapps_call_command:b_conference(ConferenceId, 'false', 'false'
                                         ,whapps_conference:moderator(Conference)
                                         ,Call
                                        ),

    lager:debug("requesting conference participants"),
    whapps_conference_command:participants(Conference).

notify_requestor(MyQ, MyId, DiscoveryEvent, ConferenceId) ->
    case wh_json:get_value(<<"Server-ID">>, DiscoveryEvent) of
        'undefined' -> ok;
        <<>> -> ok;
        RequestorQ ->
            Resp = [{<<"Conference-ID">>, ConferenceId}
                    ,{<<"Participant-ID">>, MyId}
                    | wh_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
                   ],
            wapi_conference:publish_discovery_resp(RequestorQ, Resp)
    end.
