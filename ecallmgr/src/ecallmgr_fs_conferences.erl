%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Track FreeSWITCH conference information and provide accessors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_conferences).

-behaviour(gen_listener).

-export([start_link/0]).
-export([handle_search_req/2]).
-export([show_all/0
         ,new/2
         ,node/1
         ,destroy/2
         ,all/0, all/1, all/2
         ,event/3
         ,size/1, set_size/2
         ,xml_list_to_records/2
         ,fetch/1, fetch_full/1
         ,flush_node/1
         ,sync_conferences/1
        ]).
%% Participant-specific
-export([participants_list/1
         ,participants_uuids/1
         ,participant_record_to_json/1
         ,props_to_participant_record/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(CONFERENCES_TBL, 'ecallmgr_conferences').

-record(conference, {name :: api_binary() | '$1' | '_'
                     ,uuid :: api_binary() | '$1' | '_'
                     ,node :: atom() | '$1' | '$2' | '_'
                     ,participants = 0 :: non_neg_integer() | '_'
                     ,profile_name = <<"default">> :: ne_binary() | '_'
                     ,with_floor :: non_neg_integer() | '_' % which participant has the floor
                     ,lost_floor :: non_neg_integer() | '_' % which participant has lost the floor
                     ,running = 'true' :: boolean() | '_'
                     ,answered = 'true' :: boolean() | '_'
                     ,enforce_min = 'true' :: boolean() | '_'
                     ,dynamic = 'true' :: boolean() | '_'
                     ,exit_sound = 'true' :: boolean() | '_'
                     ,enter_sound = 'true' :: boolean() | '_'
                     ,run_time = 0 :: non_neg_integer() | '_'
                     ,switch_hostname :: api_binary() | '_'
                     ,switch_url :: api_binary() | '_'
                     ,switch_external_ip :: api_binary() | '_'
                    }).
-type conference() :: #conference{}.
-type conferences() :: [conference(),...] | [].

-record(participant, {uuid :: api_binary() | '$1' | '_'
                      ,node :: atom() | '$2' | '_'
                      ,conference_name :: api_binary() | '$1'| '_'
                      ,floor = 'false' :: boolean() | '_'
                      ,hear = 'true' :: boolean() | '_'
                      ,speak = 'true' :: boolean() | '_'
                      ,talking = 'false' :: boolean() | '_'
                      ,mute_detect = 'false' :: boolean() | '_'
                      ,member_id = 0 :: non_neg_integer() | '_'
                      ,member_type :: api_binary() | '_'
                      ,energy_level = 0 :: non_neg_integer() | '_'
                      ,current_energy = 0 :: non_neg_integer() | '_'
                      ,video = 'false' :: boolean() | '_'
                      ,is_moderator = 'false' :: boolean() | '_'
                     }).
-type participant() :: #participant{}.
-type participants() :: [participant(),...] | [].

-export_type([conference/0, conferences/0
              ,participant/0, participants/0
             ]).

-compile([{'no_auto_import', [node/1]}]).

-define(RESPONDERS, [{{?MODULE, 'handle_search_req'}
                      ,[{<<"conference">>, <<"search_req">>}]
                     }
                    ]).
-define(BINDINGS, [{'conference', [{'restrict_to', ['discovery']}]}]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

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
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?MODULE}, ?MODULE,
                            [{'responders', ?RESPONDERS}
                             ,{'bindings', ?BINDINGS}
                             ,{'queue_name', ?QUEUE_NAME}
                             ,{'queue_options', ?QUEUE_OPTIONS}
                             ,{'consume_options', ?CONSUME_OPTIONS}
                            ], []).

-spec show_all() -> wh_json:objects().
show_all() ->
    ets:foldl(fun(#conference{}=Conf, Acc) -> [record_to_json(Conf) | Acc];
                 (#participant{}=P, Acc) ->   [participant_record_to_json(P) | Acc]
              end, [], ?CONFERENCES_TBL).

-spec sync_conferences(atom()) -> 'ok'.
sync_conferences(Node) -> gen_server:cast(?MODULE, {'sync_conferences', Node}).

-spec new(atom(), wh_proplist()) -> 'ok'.
new(Node, Props) ->
    gen_server:call(?MODULE, {'new_conference', props_to_record(Props, Node)}).

-spec node(ne_binary()) ->
                  {'ok', atom()} |
                  {'error', 'not_found'} |
                  {'error', 'multiple_conferences', [atom(),...]}.
node(ConfName) ->
    case ets:lookup(?CONFERENCES_TBL, ConfName) of
        [#conference{node=Node}] -> {'ok', Node};
        [#conference{} | _]=Cs ->
            {'error', 'multiple_conferences'
             ,[N || #conference{node=N} <- Cs]
            };
        _ -> {'error', 'not_found'}
    end.

-type format() :: 'json' | 'record'.

-spec all() -> conferences().
-spec all(atom()) -> conferences().
-spec all(atom(), format()) -> conferences().
all() -> all('all', 'record').
all(Node) -> all(Node, 'record').

all('all', 'record') -> ets:match_object(?CONFERENCES_TBL, #conference{_='_'});
all(Node, 'record') -> ets:match_object(?CONFERENCES_TBL, #conference{node=Node, _='_'});
all('all', 'json') -> [record_to_json(C) || C <- all('all', 'record')];
all(Node, 'json') -> [record_to_json(C) || C <- all(Node, 'record')].

-spec destroy(atom(), wh_proplist()) -> 'ok'.
destroy(Node, Props) ->
    gen_server:call(?MODULE, {'conference_destroy', Node, props:get_value(<<"Conference-Name">>, Props)}).

-spec participant_destroy(atom(), ne_binary(), wh_proplist()) -> 'ok'.
participant_destroy(Node, UUID, Props) when is_list(Props) ->
    gen_server:call(?MODULE, {'participant_destroy', Node, UUID}),
    maybe_publish_participants_event(Node, UUID, Props).

size(ConfId) ->
    case ets:lookup(?CONFERENCES_TBL, ConfId) of
        [] -> {'error', 'invalid_conference'};
        [#conference{participants=P}] -> P
    end.
set_size(ConfId, Size) when is_integer(Size) ->
    gen_server:call(?MODULE, {'conference_update', ConfId, {#conference.participants, Size}}).

-spec fetch(ne_binary()) ->
                   {'ok', wh_json:object()} |
                   {'error', 'not_found'}.
fetch(ConfId) ->
    case ets:lookup(?CONFERENCES_TBL, ConfId) of
        [Conf] -> {'ok', record_to_json(Conf)};
        _Else -> {'error', 'not_found'}
    end.

-spec fetch_full(ne_binary()) ->
                        {'ok', wh_json:object()} |
                        {'error', 'not_found'}.
fetch_full(ConfId) ->
    case ets:lookup(?CONFERENCES_TBL, ConfId) of
        [Conf] -> add_participants_to_conference_json(ConfId, record_to_json(Conf));
        _Else -> {'error', 'not_found'}
    end.

-spec flush_node(atom()) -> 'ok'.
flush_node(Node) ->
    gen_server:cast(?MODULE, {'flush_node_conferences', Node}).

-spec participants_list(ne_binary()) -> wh_json:objects().
participants_list(ConfId) ->
    case ets:match_object(?CONFERENCES_TBL, #participant{conference_name=ConfId, _='_'}) of
        [] -> [];
        Participants ->
            [participant_record_to_json(Participant)
             || Participant <- Participants
            ]
    end.

-spec participants_uuids(ne_binary()) -> wh_json:objects().
participants_uuids(ConfId) ->
    ets:match(?CONFERENCES_TBL, #participant{conference_name=ConfId
                                             ,uuid='$1'
                                             ,node='$2'
                                             ,_='_'
                                            }).


participant_record_to_json(#participant{uuid=UUID
                                        ,conference_name=ConfName
                                        ,floor=Floor
                                        ,hear=Hear
                                        ,speak=Speak
                                        ,talking=Talking
                                        ,mute_detect=MuteDetect
                                        ,member_id=MemberId
                                        ,member_type=MemberType
                                        ,energy_level=EnergyLevel
                                        ,current_energy=CurrentEnergy
                                        ,video=Video
                                        ,is_moderator=IsMod
                                       }) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"Call-ID">>, UUID}
         ,{<<"Conference-Name">>, ConfName}
         ,{<<"Floor">>, Floor}
         ,{<<"Hear">>, Hear}
         ,{<<"Speak">>, Speak}
         ,{<<"Talking">>, Talking}
         ,{<<"Mute-Detect">>, MuteDetect}
         ,{<<"Participant-ID">>, MemberId}
         ,{<<"Participant-Type">>, MemberType}
         ,{<<"Energy-Level">>, EnergyLevel}
         ,{<<"Current-Energy">>, CurrentEnergy}
         ,{<<"Video">>, Video}
         ,{<<"Is-Moderator">>, IsMod}
        ])).

props_to_participant_record(Node, Props) ->
    #participant{node=Node
                 ,uuid=props:get_value(<<"Call-ID">>, Props, props:get_value(<<"Unique-ID">>, Props))
                 ,conference_name=props:get_value(<<"Conference-Name">>, Props)
                 ,floor=props:get_is_true(<<"Floor">>, Props, 'false')
                 ,hear=props:get_is_true(<<"Hear">>, Props, 'true')
                 ,speak=props:get_is_true(<<"Speak">>, Props, 'true')
                 ,talking=props:get_is_true(<<"Talking">>, Props, 'false')
                 ,mute_detect=props:get_is_true(<<"Mute-Detect">>, Props, 'false')
                 ,member_id=props:get_integer_value(<<"Participant-ID">>, Props, 0)
                 ,member_type=props:get_value(<<"Participant-Type">>, Props)
                 ,energy_level=props:get_integer_value(<<"Energy-Level">>, Props, 0)
                 ,current_energy=props:get_integer_value(<<"Current-Energy">>, Props, 0)
                 ,video=props:get_is_true(<<"Video">>, Props, 'false')
                 ,is_moderator=props:get_is_true(<<"Is-Moderator">>, Props, 'false')
                }.

-spec handle_search_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_search_req(JObj, _Props) ->
    ConferenceId = wh_json:get_value(<<"Conference-ID">>, JObj),
    lager:debug("received search request for conference id ~s", [ConferenceId]),
    case fetch_full(ConferenceId) of
        {'error', 'not_found'} ->
            lager:debug("sending error search response, conference not found"),
            Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                     ,{<<"Error-Message">>, <<"Conference ", ConferenceId/binary, " not found">>}
                     ,{<<"Request">>, JObj}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            wapi_conference:publish_error(wh_json:get_value(<<"Server-ID">>, JObj), Error);
        {'ok', Conference} ->
            lager:debug("sending affirmative search response for conference ~s", [ConferenceId]),
            Resp = wh_json:set_values([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                                       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                      ], Conference),
            wapi_conference:publish_search_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp)
    end.

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
    put('callid', ?LOG_SYSTEM_ID),
    process_flag('trap_exit', 'true'),
    lager:info("starting FreeSWITCH conferences tracker"),
    TID = ets:new(?CONFERENCES_TBL, ['set', 'protected', 'named_table', {'keypos', #conference.name}]),
    {'ok', TID}.

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
handle_call({'new_conference', #conference{node=Node, name=Name}=C}, _, TID) ->
    case ets:lookup(TID, Name) of
        [] ->
            lager:info("creating new conference ~s on node ~s", [Name, Node]),
            ets:insert(TID, C);
        [#conference{node=Node}] -> lager:debug("conference ~s already on node ~s", [Name, Node]);
        [#conference{node=_Other}] -> lager:debug("conference ~s already on ~s, not ~s", [Name, _Other, Node])
    end,
    {'reply', 'ok', TID};
handle_call({'conference_update', Node, Name, Update}, _, TID) ->
    case ets:lookup(TID, Name) of
        [#conference{node=Node}] ->
            ets:update_element(TID, Name, Update),
            lager:debug("conference ~s already on node ~s", [Name, Node]);
        [#conference{node=_Other}] -> lager:debug("conference ~s already on ~s, not ~s, ignoring update", [Name, _Other, Node]);
        [] -> lager:debug("no conference ~s on ~s, ignoring update", [Name, Node])
    end,
    {'reply', 'ok', TID};
handle_call({'participant_update', Node, UUID, Update}, _, TID) ->
    case ets:lookup(TID, UUID) of
        [] ->
            lager:info("creating participant ~s on node ~s", [UUID, Node]),
            'true' = ets:insert_new(TID, #participant{uuid=UUID}),
            'true' = ets:update_element(TID, UUID, Update);
        [#participant{node=Node}] ->
            lager:debug("participant ~s on ~s, applying update", [UUID, Node]),
            ets:update_element(TID, UUID, Update);
        [#participant{node=_OtherNode}] ->
            lager:debug("participant ~s is on ~s, not ~s, ignoring update", [UUID, _OtherNode, Node])
    end,
    {'reply', 'ok', TID};
handle_call({'conference_destroy', Node, Name}, _, TID) ->
    MatchSpecC = [{#conference{name='$1', node='$2', _ = '_'}
                   ,[{'andalso', {'=:=', '$2', {'const', Node}}
                      ,{'=:=', '$1', Name}
                     }
                    ],
                   ['true']
                  }],
    N = ets:select_delete(TID, MatchSpecC),
    lager:info("removed ~p conference(s) with name ~s on ~s", [N, Name, Node]),

    MatchSpecP = [{#participant{conference_name='$1', node='$2', _ = '_'}
                   ,[{'andalso', {'=:=', '$2', {'const', Node}}
                      ,{'=:=', '$1', Name}
                     }
                    ],
                   ['true']
                  }],
    N1 = ets:select_delete(TID, MatchSpecP),
    lager:info("removed ~p participant(s) in conference ~s on ~s", [N1, Name, Node]),
    {'reply', 'ok', TID};
handle_call({'participant_destroy', Node, UUID}, _, TID) ->
    MatchSpec = [{#participant{uuid='$1', node='$2', _ = '_'}
                  ,[{'andalso'
                     ,{'=:=', '$2', {'const', Node}}
                     ,{'=:=', '$1', UUID}
                    }
                   ],
                  ['true']
                 }],
    N = ets:select_delete(TID, MatchSpec),
    lager:info("removed ~p participants(s) with uuid ~s on ~s", [N, UUID, Node]),
    {'reply', 'ok', TID};
handle_call(_Req, _From, TID) ->
    lager:debug("unhandled call from ~p: ~p", [_From, _Req]),
    {'reply', {'error', 'unimplemented'}, TID}.

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
handle_cast({'sync_conferences', Node}, TID) ->
    Conferences = show_conferences(Node),
    lager:debug("ensuring conferences cache is in sync with ~s", [Node]),
    CachedConferences = ets:match_object(TID, #conference{node = Node, _ = '_'}) ++
        ets:match_object(TID, #participant{node = Node, _ = '_'}),
    Remove = subtract_from(CachedConferences, Conferences),
    Add = subtract_from(Conferences, CachedConferences),
    _ = [ets:delete_object(TID, R) || R <- Remove],
    _ = [ets:insert(TID, C) || C <- Add],
    {'noreply', TID};
handle_cast({'flush_node_conferences', Node}, TID) ->
    lager:debug("flushing all conferences in cache associated to node ~s", [Node]),
    MatchSpecC = [{#conference{node = '$1', _ = '_'}
                   ,[{'=:=', '$1', {'const', Node}}]
                   ,['true']}
                 ],
    _ = ets:select_delete(TID, MatchSpecC),
    MatchSpecP = [{#participant{node = '$1', _ = '_'}
                   ,[{'=:=', '$1', {'const', Node}}]
                   ,['true']}
                 ],
    _ = ets:select_delete(TID, MatchSpecP),
    {'noreply', TID};
handle_cast(_Req, TID) ->
    lager:debug("unhandled cast: ~p", [_Req]),
    {'noreply', TID}.

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
handle_info(_Msg, TID) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
    {'noreply', TID}.

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
terminate(_Reason, TID) ->
    ets:delete(TID),
    lager:debug("FreeSWITCH conference tracker going down: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, TID, _Extra) ->
    {'ok', TID}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec xml_list_to_records(xml_els(), atom()) -> conferences() | participants().
xml_list_to_records(Xml, Node) -> xml_list_to_records(Xml, Node, []).

xml_list_to_records(#xmlElement{name='conferences'
                                ,content=Cs
                               }, Node, Recs) ->
    xml_list_to_records(Cs, Node, Recs);
xml_list_to_records([], _, Recs) -> Recs;
xml_list_to_records([#xmlElement{name='conference'
                                 ,content=Participants
                                }=ConfXml
                     |Confs], Node, Recs) ->
    xml_list_to_records(Confs, Node, [xml_to_conference(ConfXml, Node)
                                      | xml_members_to_records(Participants, Node) ++ Recs
                                     ]);
xml_list_to_records([_El|Els], Node, Recs) ->
    xml_list_to_records(Els, Node, Recs);
xml_list_to_records(#xmlElement{name='conference'
                                ,content=Participants
                               }=ConfXml, Node, Recs) ->
    [xml_to_conference(ConfXml, Node)
     | xml_members_to_records(Participants, Node) ++ Recs
    ];
xml_list_to_records(_, _, Recs) -> Recs.

xml_members_to_records([], _Node) -> [];
xml_members_to_records([#xmlElement{name='members'
                                    ,content=Participants
                                   }
                        |_], Node) ->
    [xml_to_participant(P, Node)
     || #xmlElement{name='member'}=P <- Participants
    ];
xml_members_to_records([_El|Els], Node) -> xml_members_to_records(Els, Node).

record_to_json(#conference{uuid=UUID
                           ,name=Name
                           ,participants=Participants
                           ,profile_name=Profile
                           ,with_floor=WithFloor
                           ,lost_floor=LostFloor
                           ,running=Running
                           ,answered=Answered
                           ,dynamic=Dynamic
                           ,run_time=RunTime
                           ,switch_hostname=SwitchHostname
                           ,switch_url=SwitchUrl
                           ,switch_external_ip=SwitchExtIp
                          }) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"UUID">>, UUID}
         ,{<<"Conference-ID">>, Name}
         ,{<<"Participant-Count">>, Participants}
         ,{<<"Profile">>, Profile}
         ,{<<"Participant-With-Floor">>, WithFloor}
         ,{<<"Particiapnt-Lost-Floor">>, LostFloor}
         ,{<<"Running">>, Running}
         ,{<<"Answered">>, Answered}
         ,{<<"Dynamic">>, Dynamic}
         ,{<<"Run-Time">>, RunTime}
         ,{<<"Switch-Hostname">>, SwitchHostname}
         ,{<<"Switch-URL">>, SwitchUrl}
         ,{<<"Switch-External-IP">>, SwitchExtIp}
        ])).

add_participants_to_conference_json(ConfId, ConfJObj) ->
    {'ok', wh_json:set_value(<<"Participants">>, participants_list(ConfId), ConfJObj)}.

props_to_record(Props, Node) ->
    #conference{node=Node
                ,uuid=props:get_value(<<"Conference-Unique-ID">>, Props)
                ,name=props:get_value(<<"Conference-Name">>, Props)
                ,participants=props:get_integer_value(<<"Conference-Size">>, Props, 0)
                ,profile_name=props:get_value(<<"Conference-Profile-Name">>, Props)
                ,switch_hostname=props:get_value(<<"FreeSWITCH-Hostname">>, Props)
                ,switch_url=props:get_value(<<"URL">>, Props)
                ,switch_external_ip=props:get_value(<<"Ext-SIP-IP">>, Props)
               }.

event(Node, 'undefined', Props) ->
    case props:get_value(<<"Action">>, Props) of
        <<"conference-create">> -> new(Node, Props);
        <<"play-file">> = A -> relay_event(fix_props(Props, A));
        <<"play-file-done">> = A -> relay_event(fix_props(Props, A));
        <<"start-recording">> = A -> relay_event(fix_props(Props, A));
        <<"stop-recording">> = A -> relay_event(fix_props(Props, A));
        <<"floor-change">> -> update_conference(Node, Props);
        <<"conference-destroy">> -> destroy(Node, Props);

        _Action -> lager:debug("unknown action with no uuid: ~s", [_Action])
    end;
event(Node, [UUID], Props) -> event(Node, UUID, Props);
event(Node, UUID, Props) ->
    case props:get_value(<<"Action">>, Props) of
        <<"add-member">> ->
            %% Apparently not all FS servers issue the "conference-create"
            %% event so lets have a plan-B in place....
            new(Node, Props),
            update_all(Node, UUID, Props);
        <<"floor-change">> -> update_all(Node, UUID, Props);
        <<"start-talking">> -> update_all(Node, UUID, Props);
        <<"stop-talking">> -> update_all(Node, UUID, Props);
        <<"mute-member">> -> update_all(Node, UUID, Props);
        <<"unmute-member">> -> update_all(Node, UUID, Props);
        <<"deaf-member">> -> update_all(Node, UUID, Props);
        <<"undeaf-member">> -> update_all(Node, UUID, Props);
        <<"del-member">> -> participant_destroy(Node, UUID, Props);
        <<"play-file">> = A -> relay_event(UUID, Node, fix_props(Props, A));
        <<"play-file-done">> = A -> relay_event(UUID, Node, fix_props(Props, A));
        <<"play-file-member">> = A -> relay_event(UUID, Node, fix_props(Props, A));
        <<"play-file-member-done">> = A -> relay_event(UUID, Node, fix_props(Props, A));
        <<"start-recording">> = A -> relay_event(UUID, Node, fix_props(Props, A));
        <<"stop-recording">> = A -> relay_event(UUID, Node, fix_props(Props, A));
        _Action -> lager:debug("unhandled conference action for ~s: ~s", [UUID, _Action])
    end.

fix_props(Props, A) when A =:= <<"play-file">>; A =:= <<"play-file-member">> ->
    [{<<"Event-Name">>, <<"CHANNEL_EXECUTE">>}
     ,{<<"whistle_event_name">>, <<"CHANNEL_EXECUTE">>}
     ,{<<"Application">>, A}
     ,{<<"whistle_application_name">>, A}
     ,{<<"Application-Data">>, props:get_value(<<"File">>, Props)}
     | props:delete_keys([<<"Event-Name">>, <<"Event-Subclass">>], Props)
    ];
fix_props(Props, A) when A =:= <<"play-file-done">>; A =:= <<"play-file-member-done">> ->
    [{<<"Event-Name">>, <<"CHANNEL_EXECUTE_COMPLETE">>}
     ,{<<"whistle_event_name">>, <<"CHANNEL_EXECUTE_COMPLETE">>}
     ,{<<"Application">>, A}
     ,{<<"whistle_application_name">>, A}
     ,{<<"Application-Data">>, props:get_value(<<"File">>, Props)}
     | props:delete_keys([<<"Event-Name">>, <<"Event-Subclass">>], Props)
    ];
fix_props(Props, A) when A =:= <<"start-recording">> ->
    [{<<"Event-Name">>, <<"CHANNEL_EXECUTE">>}
     ,{<<"whistle_event_name">>, <<"CHANNEL_EXECUTE">>}
     ,{<<"Application">>, A}
     ,{<<"whistle_application_name">>, A}
     ,{<<"Application-Data">>, props:get_value(<<"Path">>, Props)}
     | props:delete_keys([<<"Event-Name">>, <<"Event-Subclass">>], Props)
    ];
fix_props(Props, A) when A =:= <<"stop-recording">> ->
    [{<<"Event-Name">>, <<"CHANNEL_EXECUTE_COMPLETE">>}
     ,{<<"whistle_event_name">>, <<"CHANNEL_EXECUTE_COMPLETE">>}
     ,{<<"Application">>, A}
     ,{<<"whistle_application_name">>, A}
     ,{<<"Application-Data">>, props:get_value(<<"Path">>, Props)}
     | props:delete_keys([<<"Event-Name">>, <<"Event-Subclass">>], Props)
    ];
fix_props(Props, _A) ->
    lager:debug("not fixing ~s", [_A]),
    Props.

update_all(Node, UUID, Props) ->
    update_conference(Node, Props),
    update_participant(Node, UUID, Props).

update_participant(Node, UUID, Props) ->
    gen_server:call(?MODULE, {'participant_update'
                                 ,Node
                                 ,UUID
                                 ,[{#participant.node, Node} | participant_fields(Props)]
                                }),
    maybe_publish_participants_event(Node, UUID, Props).

update_conference(Node, Props) ->
    ProfileProps = ecallmgr_util:get_interface_properties(Node),
    gen_server:call(?MODULE, {'conference_update'
                              ,Node
                              ,props:get_value(<<"Conference-Name">>, Props)
                              ,[{#conference.node, Node}
                                ,{#conference.switch_url, props:get_value(<<"URL">>, ProfileProps)}
                                ,{#conference.switch_external_ip, props:get_value(<<"Ext-SIP-IP">>, ProfileProps)}
                                | conference_fields(Props)
                               ]
                             }).

relay_event(Props) ->
    [relay_event(UUID, Node, Props) || [UUID, Node] <- participants_uuids(props:get_value(<<"Conference-Name">>, Props))].
relay_event(UUID, Node, Props) ->
    EventName = props:get_value(<<"Event-Name">>, Props),
    Payload = {'event', [UUID, {<<"Caller-Unique-ID">>, UUID} | Props]},
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, EventName)}, Payload),
    gproc:send({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, UUID)}, Payload).

-define(FS_CONF_FIELDS, [{<<"Conference-Unique-ID">>, #conference.uuid}
                         ,{<<"Conference-Size">>, #conference.participants, fun props:get_integer_value/2}
                         ,{<<"Conference-Profile-Name">>, #conference.profile_name}
                         ,{<<"New-ID">>, #conference.with_floor, fun safe_integer_get/3, 0}
                         ,{<<"Old-ID">>, #conference.lost_floor, fun safe_integer_get/3, 0}
                         ,{<<"Switch-Hostname">>, #conference.switch_hostname, fun props:get_value/2}
                        ]).
conference_fields(Props) -> fields(Props, ?FS_CONF_FIELDS).

-define(FS_PARTICIPANT_FIELDS, [{<<"Floor">>, #participant.floor, fun props:get_is_true/2}
                                %%,{<<"Unique-ID">>, #participant.uuid}
                                ,{<<"Conference-Name">>, #participant.conference_name}
                                ,{<<"Hear">>, #participant.hear, fun props:get_is_true/2}
                                ,{<<"Speak">>, #participant.speak, fun props:get_is_true/2}
                                ,{<<"Talking">>, #participant.talking, fun props:get_is_true/2}
                                ,{<<"Mute-Detect">>,#participant.mute_detect, fun props:get_is_true/2}
                                ,{<<"Member-ID">>, #participant.member_id, fun props:get_integer_value/2}
                                ,{<<"Member-Type">>, #participant.member_type}
                                ,{<<"Participant-ID">>, #participant.member_id, fun props:get_integer_value/2}
                                ,{<<"Participant-Type">>, #participant.member_type}
                                ,{<<"Energy-Level">>, #participant.energy_level, fun props:get_integer_value/2}
                                ,{<<"Current-Energy">>, #participant.current_energy, fun props:get_integer_value/2}
                                ,{<<"Video">>, #participant.video, fun props:get_is_true/2}
                               ]).
participant_fields(Props) -> fields(Props, ?FS_PARTICIPANT_FIELDS).

fields(Props, Fields) ->
    lists:foldl(fun(K, Acc) -> maybe_include_key(K, Acc, Props) end, [], Fields).

maybe_include_key({K, Pos}, Acc, Props) ->
    maybe_include_key(K, Pos, Acc, Props, fun props:get_value/2);
maybe_include_key({K, Pos, Getter}, Acc, Props) ->
    maybe_include_key(K, Pos, Acc, Props, Getter);
maybe_include_key({K, Pos, Getter, Default}, Acc, Props) ->
    maybe_include_key(K, Pos, Acc, Props, Getter, Default).

maybe_include_key(K, Pos, Acc, Props, Getter) when is_function(Getter, 2) ->
    case Getter(K, Props) of
        'undefined' -> Acc;
        V -> [{Pos, V} | Acc]
    end.
maybe_include_key(K, Pos, Acc, Props, Getter, Default) when is_function(Getter, 3) ->
    [{Pos, Getter(K, Props, Default)} | Acc].

safe_integer_get(K, Props, D) ->
    case props:get_value(K, Props) of
        'undefined' -> D;
        V ->
            try wh_util:to_integer(V) of
                I -> I
            catch
                'error':'badarg' -> D
            end
    end.

subtract_from([], _) -> [];
subtract_from(Set1, []) -> Set1;
subtract_from(Set1, [S2|Set2]) ->
    subtract_from([S1 || S1 <- Set1, should_remove(S1, S2)], Set2).

should_remove(#participant{uuid=UUID1}, #participant{uuid=UUID2}) -> UUID1 =/= UUID2;
should_remove(#conference{name=N1}, #conference{name=N2}) -> N1 =/= N2;
should_remove(_, _) -> 'true'.

-spec show_conferences(atom()) ->
                              conferences() |
                              participants().
show_conferences(Node) ->
    case freeswitch:api(Node, 'conference', "xml_list") of
        {'ok', XmlStr} ->
            {Xml, _} = xmerl_scan:string(wh_util:to_list(XmlStr)),
            case catch xml_list_to_records(Xml, Node) of
                {'EXIT', _R} -> [];
                Rs -> Rs
            end;
        {'error', _} -> [];
        'timeout' -> []
    end.

-spec xml_to_conference(xml_el() | xml_attribs(), atom()) -> ecallmgr_fs_conferences:conference().
xml_to_conference(#xmlElement{name='conference'
                              ,attributes=Attrs
                             }, Node) ->
    xml_attrs_to_record(Attrs, Node);
xml_to_conference([#xmlAttribute{}|_]=Attrs, Node) -> xml_attrs_to_record(Attrs, Node).

-spec xml_attrs_to_record(xml_attribs(), atom()) -> conference().
xml_attrs_to_record(Attrs, Node) -> update_conference_with_xml_attrs(Attrs, #conference{node=Node}).

-spec update_conference_with_xml_attrs(xml_attribs(), conference()) -> conference().
update_conference_with_xml_attrs([], Conf) -> Conf;
update_conference_with_xml_attrs([#xmlAttribute{name=N, value=V}|Attrs], Conf) ->
    update_conference_with_xml_attrs(Attrs, update_conference_record(Conf, N, V)).

-spec update_conference_record(conference(), atom(), any()) -> conference().
update_conference_record(Conf, 'name', V) ->
    Conf#conference{name=wh_util:to_binary(V)};
update_conference_record(Conf, 'member-count', V) ->
    Conf#conference{participants=wh_util:to_integer(V)};
update_conference_record(Conf, 'uuid', V) ->
    Conf#conference{uuid=wh_util:to_binary(V)};
update_conference_record(Conf, 'running', V) ->
    Conf#conference{running=wh_util:is_true(V)};
update_conference_record(Conf, 'answered', V) ->
    Conf#conference{answered=wh_util:is_true(V)};
update_conference_record(Conf, 'enforce_min', V) ->
    Conf#conference{enforce_min=wh_util:is_true(V)};
update_conference_record(Conf, 'dynamic', V) ->
    Conf#conference{dynamic=wh_util:is_true(V)};
update_conference_record(Conf, 'exit_sound', V) ->
    Conf#conference{exit_sound=wh_util:is_true(V)};
update_conference_record(Conf, 'enter_sound', V) ->
    Conf#conference{enter_sound=wh_util:is_true(V)};
update_conference_record(Conf, 'run_time', V) ->
    Conf#conference{run_time=wh_util:to_integer(V)};
update_conference_record(Conf, _K, _V) ->
    lager:debug("unhandled conference k/v ~s: ~p", [_K, _V]),
    Conf.

xml_to_participant(#xmlElement{name='member'
                               ,attributes=Attrs
                               ,content=MemberTags
                              }, Node) ->
    update_from_children(from_attrs(Attrs, Node), MemberTags).

from_attrs(Attrs, Node) -> from_attrs(Attrs, Node, #participant{}).

from_attrs([], Node, P) -> P#participant{node=Node};
from_attrs([#xmlAttribute{name='type'
                          ,value="caller"
                         }
            |Attrs], Node, P) ->
    lager:debug("member is a caller"),
    from_attrs(Attrs, Node, P);
from_attrs([_Attr|Attrs], Node, P) ->
    lager:debug("unhandled attr: ~p", [_Attr]),
    from_attrs(Attrs, Node, P).

update_from_children(P, []) -> P;
update_from_children(P, [#xmlElement{name='id'
                                     ,content=Id
                                    }
                         |Els]) ->
    update_from_children(P#participant{member_id=wh_util:to_integer(xml_text_to_binary(Id))}, Els);
update_from_children(P, [#xmlElement{name='flags'
                                     ,content=Flags
                                    }
                         |Els]) ->
    update_from_children(update_from_flags(P, Flags), Els);
update_from_children(P, [#xmlElement{name='uuid'
                                     ,content=UUID
                                    }
                         |Els]) ->
    CallId = wh_util:uri_decode(xml_text_to_binary(UUID)),
    lager:debug("uuid ~s callid ~s", [xml_text_to_binary(UUID), CallId]),
    update_from_children(P#participant{uuid=wh_util:to_binary(CallId)}, Els);
update_from_children(P, [#xmlElement{name='energy'
                                    ,content=Energy
                                    }
                        |Els]) ->
    update_from_children(P#participant{energy_level=wh_util:to_integer(xml_text_to_binary(Energy))}, Els);
update_from_children(P, [#xmlElement{name=_N}|Els]) ->
    update_from_children(P, Els);
update_from_children(P, [_El|Els]) ->
    update_from_children(P, Els).

-spec update_from_flags(participant(), xml_els()) -> participant().
update_from_flags(P, []) -> P;
update_from_flags(P, #xmlElement{name='flags'
                                 ,content=Flags
                                }) ->
    update_from_flags(P, Flags);
update_from_flags(P, [#xmlElement{name='talking'
                                  ,content=Speak
                                 }
                      | Els]) ->
    update_from_flags(P#participant{speak=wh_util:is_true(xml_text_to_binary(Speak))}, Els);
update_from_flags(P, [#xmlElement{name='has_floor'
                                  ,content=HasFloor
                                 }
                      | Els]) ->
    update_from_flags(P#participant{floor=wh_util:is_true(xml_text_to_binary(HasFloor))}, Els);
update_from_flags(P, [#xmlElement{name='is_moderator'
                                  ,content=IsMod
                                 }
                      | Els]) ->
    update_from_flags(P#participant{is_moderator=wh_util:is_true(xml_text_to_binary(IsMod))}, Els);
update_from_flags(P, [#xmlElement{name='can_hear'
                                  ,content=Hear
                                 }
                      | Els]) ->
    update_from_flags(P#participant{hear=wh_util:is_true(xml_text_to_binary(Hear))}, Els);
update_from_flags(P, [#xmlElement{name='can_speak'
                                  ,content=Speak
                                 }
                      | Els]) ->
    update_from_flags(P#participant{speak=wh_util:is_true(xml_text_to_binary(Speak))}, Els);
update_from_flags(P, [_|Els]) -> update_from_flags(P, Els).

xml_text_to_binary(Els) -> iolist_to_binary([V || #xmlText{value=V} <- Els]).

maybe_publish_participants_event(Node, _, Props) ->
    ConferenceName = props:get_value(<<"Conference-Name">>, Props),
    case participants_list(ConferenceName) of
        [] ->
            %% On some systems the conference-destroy is not reliable
            %% so once the last participant leaves ensure its destroyed
            destroy(Node, Props);
        Participants ->
            Event = [{<<"Participants">>, Participants}
                     ,{<<"Focus">>, wh_util:to_binary(Node)}
                     ,{<<"Conference-ID">>, ConferenceName}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            Publisher = fun(P) -> wapi_conference:publish_participants_event(ConferenceName, P) end,
            wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                                ,Event
                                ,Publisher
                               )
    end.
