%%%-------------------------------------------------------------------
%%% @copyright (C) 2012 VoIP Inc
%%% @doc
%%% Conference participant process
%%% @end
%%% Created : 20 Feb 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(conf_participant).

-behaviour(gen_listener).

-include("conference.hrl").

%% API
-export([start_link/1]).
-export([relay_amqp/2]).
-export([handle_participants_resp/2]).
-export([consume_call_events/1]).
-export([conference/1, set_conference/2]).
-export([discovery_event/1, set_discovery_event/2]).
-export([call/1]).

-export([join_local/1]).
%%-export([join_remote/2]).

-export([mute/1, unmute/1, toggle_mute/1]).
-export([deaf/1, undeaf/1, toggle_deaf/1]).
-export([hangup/1]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{{?MODULE, relay_amqp}, [{<<"call_event">>, <<"*">>}]}
                     ,{{?MODULE, handle_participants_resp}, [{<<"conference">>, <<"participants_resp">>}]}
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(participant, {conference_id = undefined
                      ,participant_id = 0
                      ,controller_queue = undefined
                      ,call = undefined
                      ,bridge = undefined
                      ,moderator = false
                      ,muted = false
                      ,deaf = false
                      ,waiting_for_mod = false
                      ,call_event_consumers = []
                      ,self = self()
                      ,in_conference = false
                      ,conference = #conference{}
                      ,discovery_event = wh_json:new()
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

-spec conference/1 :: (pid()) -> {ok, #conference{}}.
conference(Srv) ->
    gen_server:call(Srv, {get_conference}, 500).

-spec set_conference/2 :: (#conference{}, pid()) -> 'ok'.
set_conference(Conference, Srv) ->
    gen_server:cast(Srv, {set_conference, Conference}).

-spec discovery_event/1 :: (pid()) -> {ok, wh_json:json_object()}.
discovery_event(Srv) ->
    gen_server:call(Srv, {get_discovery_event}, 500).

-spec set_discovery_event/2 :: (wh_json:json_object(), pid()) -> 'ok'.
set_discovery_event(DiscoveryEvent, Srv) ->
    gen_server:cast(Srv, {set_discovery_event, DiscoveryEvent}).

-spec call/1 :: (pid()) -> {ok, whapps_call:call()}.
call(Srv) ->
    gen_server:call(Srv, {get_call}, 500).

-spec join_local/1 :: (pid()) -> 'ok'.
join_local(Srv) ->
    gen_server:cast(Srv, join_local).
    
-spec mute/1 :: (pid()) -> ok.
mute(Srv) ->
    gen_server:cast(Srv, mute).

-spec unmute/1 :: (pid()) -> ok.
unmute(Srv) ->
    gen_server:cast(Srv, unmute).

-spec toggle_mute/1 :: (pid()) -> ok.
toggle_mute(Srv) ->
    gen_server:cast(Srv, toggle_mute).

-spec deaf/1 :: (pid()) -> ok.
deaf(Srv) ->
    gen_server:cast(Srv, deaf).

-spec undeaf/1 :: (pid()) -> ok.
undeaf(Srv) ->
    gen_server:cast(Srv, undeaf).

-spec toggle_deaf/1 :: (pid()) -> ok.
toggle_deaf(Srv) ->
    gen_server:cast(Srv, toggle_deaf).

-spec hangup/1 :: (pid()) -> ok.
hangup(Srv) ->
    gen_server:cast(Srv, hangup).

-spec consume_call_events/1 :: (pid()) -> ok.
consume_call_events(Srv) ->
    gen_server:cast(Srv, {add_consumer, self()}).    

-spec relay_amqp/2 :: (wh_json:json_object(), proplist()) -> ok.
relay_amqp(JObj, Props) ->
    [Pid ! {amqp_msg, JObj}
     || Pid <- props:get_value(call_event_consumers, Props, [])
            ,is_pid(Pid)
    ],
    Digit = wh_json:get_value(<<"DTMF-Digit">>, JObj),
    case is_binary(Digit) andalso props:get_value(in_conference, Props, false) of
        false -> ok;
        true ->
            Srv = props:get_value(server, Props),
            case Digit of
                <<"1">> -> mute(Srv);
                <<"2">> -> unmute(Srv);
                <<"0">> -> toggle_mute(Srv);
                <<"3">> -> deaf(Srv);
                <<"4">> -> undeaf(Srv);
                <<"*">> -> toggle_deaf(Srv);
                <<"#">> -> hangup(Srv);
                _Else -> ok
            end
    end.
    
-spec handle_participants_resp/2 :: (wh_json:json_object(), proplist()) -> ok.
handle_participants_resp(JObj, Props) ->
    Srv = props:get_value(server, Props),
    Participants = wh_json:get_value(<<"Participants">>, JObj, wh_json:new()),
    gen_server:cast(Srv, {sync_participant, Participants}).

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
    %% publish call status request
    process_flag(trap_exit, true),
    Self = self(),
    spawn(fun() ->
                  ControllerQ = gen_listener:queue_name(Self),
                  gen_server:cast(Self, {controller_queue, ControllerQ})
          end),
    {ok, #participant{call=Call}}.

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
handle_call({get_conference}, _From, #participant{conference=Conference}=Participant) ->
    {reply, {ok, Conference}, Participant};
handle_call({get_discovery_event}, _From, #participant{discovery_event=DiscoveryEvent}=Participant) ->
    {reply, {ok, DiscoveryEvent}, Participant};
handle_call({get_call}, _From, #participant{call=Call}=Participant) ->
    {reply, {ok, Call}, Participant};
handle_call(_Request, _From, Participant) ->
    Reply = {error, unimplemented},
    {reply, Reply, Participant}.

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
handle_cast({controller_queue, ControllerQ}, #participant{call=Call}=Participant) ->
    {noreply, Participant#participant{call=whapps_call:set_controller_queue(ControllerQ, Call)}};
handle_cast({add_consumer, Consumer}, #participant{call_event_consumers=Consumers}=Participant) ->
    link(Consumer),
    io:format("added consumer ~p~n", [Consumer]),
    {noreply, Participant#participant{call_event_consumers=[Consumer|Consumers]}};
handle_cast({remove_consumer, Consumer}, #participant{call_event_consumers=Consumers}=Participant) ->
    io:format("removed consumer ~p~n", [Consumer]),
    {noreply, Participant#participant{call_event_consumers=lists:filter(fun(C) -> C =/= Consumer end, Consumers)}};
handle_cast({set_conference, #conference{}=Conference}, Participant) ->
    {noreply, Participant#participant{conference=Conference}};
handle_cast({set_discovery_event, DiscoveryEvent}, Participant) ->
    {noreply, Participant#participant{discovery_event=DiscoveryEvent}};
handle_cast(join_local, #participant{call=Call, conference=#conference{id=ConferenceId}}=Participant) ->
    whapps_call_command:conference(ConferenceId, Call),
    Q = whapps_call:controller_queue(Call),
    Command = [{<<"Conference-ID">>, ConferenceId}
               | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_conference:publish_participants_req(ConferenceId, Command),    
    {noreply, Participant};
handle_cast({sync_participant, Participants}, #participant{conference=#conference{id=ConferenceId}
                                                           ,call=Call}=Participant) ->
    io:format("GOT ~p~n", [Participants]),
    case find_participant(wh_json:to_proplist(Participants), whapps_call:call_id(Call)) of
        {ok, JObj} ->
            ParticipantId = wh_json:get_value(<<"Participant-ID">>, JObj),            
            io:format("FOUND PARTICIPANT ID: ~p~n", [ParticipantId]),
            %% if not in_conference already play entry audio
            %% also sync deaf/mute state
            {noreply, Participant#participant{in_conference=true, participant_id=ParticipantId}};
        {error, not_found} ->
            %% if in_conference, terminate... not any more ;)
            timer:sleep(1000),
            Q = whapps_call:controller_queue(Call),
            Command = [{<<"Conference-ID">>, ConferenceId}
                       | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                      ],
            wapi_conference:publish_participants_req(ConferenceId, Command),    
            {noreply, Participant}
    end;
handle_cast(mute, #participant{call=Call, participant_id=ParticipantId
                              ,conference=#conference{id=ConferenceId}}=Participant) ->
    Q = whapps_call:controller_queue(Call),
    ?LOG("mute participant ~s", [ParticipantId]),
    Command = [{<<"Conference-ID">>, ConferenceId}
               ,{<<"Participant">>, ParticipantId}
               | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_conference:publish_mute_participant(ConferenceId, Command),
    %% play muted prompt
    {noreply, Participant#participant{muted=true}};
handle_cast(unmute, #participant{call=Call, participant_id=ParticipantId
                                 ,conference=#conference{id=ConferenceId}}=Participant) ->
    Q = whapps_call:controller_queue(Call),
    Command = [{<<"Conference-ID">>, ConferenceId}
               ,{<<"Participant">>, ParticipantId}
               | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_conference:publish_unmute_participant(ConferenceId, Command),
    %% play unmuted prompt
    {noreply, Participant#participant{muted=false}};
handle_cast(toggle_mute, #participant{muted=true}=Participant) ->
    unmute(self()),
    {noreply, Participant};
handle_cast(toggle_mute, #participant{muted=false}=Participant) ->
    mute(self()),
    {noreply, Participant};
handle_cast(deaf, #participant{call=Call, participant_id=ParticipantId
                               ,conference=#conference{id=ConferenceId}}=Participant) ->
    Q = whapps_call:controller_queue(Call),
    Command = [{<<"Conference-ID">>, ConferenceId}
               ,{<<"Participant">>, ParticipantId}
               | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_conference:publish_deaf_participant(ConferenceId, Command),
    %% play deaf prompt
    {noreply, Participant#participant{deaf=true}};
handle_cast(undeaf, #participant{call=Call, participant_id=ParticipantId
                                 ,conference=#conference{id=ConferenceId}}=Participant) ->
    Q = whapps_call:controller_queue(Call),
    Command = [{<<"Conference-ID">>, ConferenceId}
               ,{<<"Participant">>, ParticipantId}
               | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_conference:publish_undeaf_participant(ConferenceId, Command),
    %% play undeaf prompt
    {noreply, Participant#participant{deaf=false}};
handle_cast(toggle_deaf, #participant{deaf=true}=Participant) ->
    undeaf(self()),
    {noreply, Participant};
handle_cast(toggle_deaf, #participant{deaf=false}=Participant) ->
    deaf(self()),
    {noreply, Participant};
handle_cast(hangup, Participant) ->
    {noreply, Participant};
handle_cast(_, Participant) ->
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
    io:format("consumer ~p died: ~p~n", [Consumer, _R]),
    {noreply, Participant#participant{call_event_consumers=lists:filter(fun(C) -> C =/= Consumer end, Consumers)}};
handle_info(_, Participant) ->
    {noreply, Participant}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_event(JObj, #participant{call_event_consumers=Consumers, call=Call, self=Self, in_conference=InConf}) ->
    CallId = whapps_call:call_id_direct(Call),
    case {whapps_util:get_event_type(JObj), wh_json:get_value(<<"Call-ID">>, JObj)} of
        {{<<"call_event">>, <<"CHANNEL_HANGUP">>}, CallId} ->
            gen_server:cast(Self, hungup),
            {reply, [{call_event_consumers, Consumers}]};
        {{<<"call_detail">>, <<"cdr">>}, CallId} ->
            gen_server:cast(Self, hungup),
            ignore;
        {{<<"call_event">>, <<"CHANNEL_DESTROY">>}, CallId} ->
            gen_server:cast(Self, hungup),
            {reply, [{call_event_consumers, Consumers}]};
        {{<<"call_event">>, _}, EventCallId} when EventCallId =/= CallId ->
            ?LOG("received event from call ~s while relaying for ~s, dropping", [EventCallId, CallId]),
            ignore;
        {_Else, _} ->
            {reply, [{call_event_consumers, Consumers}, {in_conference, InConf}]}
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
terminate(_Reason, _Participant) ->
    ?LOG_END("conference participant execution has been stopped: ~p", [_Reason]),
    ok.

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
-spec find_participant/2 :: (proplist(), ne_binary()) -> {ok, wh_json:json_object()} |
                                                         {error, not_found}.
find_participant([], _) ->
    {error, not_found};
find_participant([{_, Participant}|Participants], CallId) ->
    case wh_json:get_value(<<"Call-ID">>, Participant) of
        CallId -> {ok, Participant};
        _Else -> find_participant(Participants, CallId)
    end.
        
