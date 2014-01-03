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
-export([summary/0
         ,summary/1
        ]).
-export([details/0
         ,details/1
        ]).
-export([create/2]).
-export([update/2]).
-export([destroy/1]).
-export([node/1]).
-export([participants/1]).
-export([participants_to_json/1]).
-export([participant_create/3]).
-export([participant_update/2]).
-export([participant_destroy/1]).
-export([participant_callid/2]).
-export([sync_node/1]).
-export([flush_node/1]).
-export([handle_search_req/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).
-export([list_conferences/1
         ,get_conference_dictionary/1
         ,get_participant_dictionary/1
         ,sync_conferences/2
         ,sync_participants/2
        ]).

-include("ecallmgr.hrl").

-record(state, {}).

-define(CONFERENCES_TBL, 'ecallmgr_conferences').
-define(PARTICIPANTS_TBL, 'ecallmgr_participants').

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


-spec summary() -> 'ok'.
summary() ->
    MatchSpec = [{#conference{_ = '_'}
                  ,[]
                  ,['$_']
                 }],
    print_summary(ets:select(?CONFERENCES_TBL, MatchSpec, 1)).

-spec summary(text()) -> 'ok'.
summary(Node) when not is_atom(Node) ->
    summary(wh_util:to_atom(Node, 'true'));
summary(Node) ->
    MatchSpec = [{#conference{node='$1', _ = '_'}
                  ,[{'=:=', '$1', {'const', Node}}]
                  ,['$_']
                 }],
    print_summary(ets:select(?CONFERENCES_TBL, MatchSpec, 1)).

-spec details() -> 'ok'.
details() ->
    MatchSpec = [{#conference{_ = '_'}
                  ,[]
                  ,['$_']
                 }],
    print_details(ets:select(?CONFERENCES_TBL, MatchSpec, 1)).

-spec details(text()) -> 'ok'.
details(UUID) when not is_binary(UUID) ->
    details(wh_util:to_binary(UUID));
details(UUID) ->
    MatchSpec = [{#conference{uuid=UUID, _ = '_'}
                  ,[{'=:=', '$1', {'const', UUID}}]
                  ,['$_']
                 }],
    print_details(ets:select(?CONFERENCES_TBL, MatchSpec, 1)).

-spec create(wh_proplist(), atom()) -> conference().
create(Props, Node) ->
    gen_server:call(?MODULE, {'conference_create', Props, Node}).

-spec update(ne_binary(), wh_proplist()) -> 'ok'.
update(UUID, Update) ->
    gen_server:call(?MODULE, {'conference_update', UUID, Update}).

-spec destroy(ne_binary()) -> 'ok'.
destroy(UUID) ->
    gen_server:call(?MODULE, {'conference_destroy', UUID}).

-spec node(ne_binary()) ->
                  {'ok', atom()} |
                  {'error', 'not_found'}.
node(Name) ->
    case ets:match_object(?CONFERENCES_TBL, #conference{name=Name, _ = '_'}) of
        %% TODO: this ignores conferences on multiple nodes until big-conferences
        [#conference{node=Node}|_] -> {'ok', Node};
        %% [#conference{}|_]=Conferences ->
        %%     {'error', 'multiple_conferences'
        %%      ,[Node || #conference{node=Node} <- Conferences]
        %%     };
        _ -> {'error', 'not_found'}
    end.

-spec participants(conference() | ne_binary()) -> participants().
participants(#conference{name=Name}) -> participants(Name);
participants(Name) ->
    %% Note: used the conference name supports participants on multiple nodes
    ets:match_object(?PARTICIPANTS_TBL, #participant{conference_name=Name, _ = '_'}).

-spec participants_to_json(participants()) -> wh_json:objects().
participants_to_json(Participants) ->
    participants_to_json(Participants, []).

-spec participant_create(wh_proplist(), atom(), ne_binary()) -> participant().
participant_create(Props, Node, CallId) ->
    gen_server:call(?MODULE, {'participant_create', Props, Node, CallId}).

-spec participant_update(ne_binary(), wh_proplist()) -> 'ok'.
participant_update(CallId, Update) ->
    gen_server:call(?MODULE, {'participant_update', CallId, Update}).

-spec participant_destroy(ne_binary()) -> 'ok'.
participant_destroy(CallId) ->
    gen_server:call(?MODULE, {'participant_destroy', CallId}).

-spec participant_callid(ne_binary(), non_neg_integer()) -> api_binary().
participant_callid(UUID, MemberId) ->
    case ets:match(?PARTICIPANTS_TBL, #participant{conference_uuid=UUID
                                                   ,member_id=MemberId
                                                   ,uuid='$1'
                                                   ,_ = '_'})
    of
        [[CallId]] -> CallId;
        _Else -> 'undefined'
    end.

-spec sync_node(atom()) -> 'ok'.
sync_node(Node) -> gen_server:cast(?MODULE, {'sync_node', Node}).

-spec flush_node(atom()) -> 'ok'.
flush_node(Node) -> gen_server:cast(?MODULE, {'flush_node', Node}).

-spec handle_search_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_search_req(JObj, _Props) ->
    Name = wh_json:get_value(<<"Conference-ID">>, JObj),
    lager:info("received search request for conference name ~s", [Name]),
    case ets:match_object(?CONFERENCES_TBL, #conference{name=Name, _ = '_'}) of
        %% TODO: this ignores conferences on multiple nodes until big-conferences
        [#conference{uuid=UUID
                     ,start_time=StartTime
                     ,switch_hostname=Hostname
                     ,switch_url=SwitchURL
                     ,switch_external_ip=ExternalIP
                    } | _Conferences
        ] ->
            lager:debug("sending affirmative search response for conference ~s", [Name]),
            Participants = participants(Name),
            Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                    ,{<<"Conference-ID">>, Name}
                    ,{<<"UUID">>, UUID}
                    ,{<<"Run-Time">>, wh_util:current_tstamp() - StartTime}
                    ,{<<"Switch-Hostname">>, Hostname}
                    ,{<<"Switch-URL">>, SwitchURL}
                    ,{<<"Switch-External-IP">>, ExternalIP}
                    ,{<<"Participant-Count">>, length(Participants)}
                    ,{<<"Participants">>, participants_to_json(Participants)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_conference:publish_search_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
        [] ->
            lager:debug("sending error search response, conference not found"),
            Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                     ,{<<"Error-Message">>, <<"Conference ", Name/binary, " not found">>}
                     ,{<<"Request">>, JObj}
                     ,{<<"Conference-ID">>, Name}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            wapi_conference:publish_error(wh_json:get_value(<<"Server-ID">>, JObj), Error)
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
    _ = ets:new(?CONFERENCES_TBL, ['set', 'protected', 'named_table', {'keypos', #conference.uuid}]),
    _ = ets:new(?PARTICIPANTS_TBL, ['set', 'protected', 'named_table', {'keypos', #participant.uuid}]),
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
handle_call({'conference_create', Props, Node}, _, State) ->
    lager:debug("created conference ~p", [Props]),
    Conference = conference_from_props(Props, Node),
    _ = ets:insert_new(?CONFERENCES_TBL, Conference),
    {'reply', Conference, State};
handle_call({'conference_update', UUID, Update}, _, State) ->
    _ = ets:update_element(?CONFERENCES_TBL, UUID, Update),
    {'reply', 'ok', State};
handle_call({'conference_destroy', UUID}, _, State) ->
    MatchSpecC = [{#conference{uuid='$1', _ = '_'}
                   ,[{'=:=', '$1', {'const', UUID}}]
                   ,['true']
                  }],
    _ = ets:select_delete(?CONFERENCES_TBL, MatchSpecC),
    MatchSpecP = [{#participant{conference_uuid='$1', _ = '_'}
                   ,[{'=:=', '$1', {'const', UUID}}]
                   ,['true']
                  }],
    _ = ets:select_delete(?PARTICIPANTS_TBL, MatchSpecP),
    {'reply', 'ok', State};
handle_call({'participant_create', Props, Node, CallId}, _, State) ->
    Participant = participant_from_props(Props, Node, CallId),
    _ = ets:insert_new(?PARTICIPANTS_TBL, Participant),
    UUID = props:get_value(<<"Conference-Unique-ID">>, Props),
    _ = case ets:lookup(?CONFERENCES_TBL, UUID) of
            [#conference{}] -> 'ok';
            _Else ->
                lager:info("failed to find participants conference ~s, adding", [UUID]),
                Conference = conference_from_props(Props, Node),
                _ = ets:insert_new(?CONFERENCES_TBL, Conference)
        end,
    {'reply', Participant, State};
handle_call({'participant_update', CallId, Update}, _, State) ->
    _ = ets:update_element(?PARTICIPANTS_TBL, CallId, Update),
    {'reply', 'ok', State};
handle_call({'participant_destroy', CallId}, _, State) ->
    _ = case ets:lookup(?PARTICIPANTS_TBL, CallId) of
            [#participant{conference_uuid=UUID}] ->
                _ = ets:delete(?PARTICIPANTS_TBL, CallId),
                maybe_destroy_conference(UUID);
            _Else -> 'ok'
        end,
    {'reply', 'ok', State};
handle_call(_Req, _From, State) ->
    lager:debug("unhandled call from ~p: ~p", [_From, _Req]),
    {'reply', {'error', 'unimplemented'}, State}.

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
handle_cast({'sync_node', Node}, State) ->
    Sync = list_conferences(Node),
    Conferences = get_conference_dictionary(Sync),
    Participants = get_participant_dictionary(Sync),
    _ = sync_conferences(Conferences, Node),
    _ = sync_participants(Participants, Node),
    %% Make sure we didn't cache a conference with no participants
    _ = [maybe_destroy_conference(UUID)
         || [UUID] <- ets:match(?CONFERENCES_TBL, #conference{node=Node
                                                              ,uuid='$1'
                                                              ,_='_'
                                                             })
        ],
    {'noreply', State};
handle_cast({'flush_node', Node}, State) ->
    lager:debug("flushing all conferences in cache associated to node ~s", [Node]),
    MatchSpecC = [{#conference{node = '$1', _ = '_'}
                   ,[{'=:=', '$1', {'const', Node}}]
                   ,['true']}
                 ],
    _ = ets:select_delete(?CONFERENCES_TBL, MatchSpecC),
    MatchSpecP = [{#participant{node = '$1', _ = '_'}
                   ,[{'=:=', '$1', {'const', Node}}]
                   ,['true']}
                 ],
    _ = ets:select_delete(?PARTICIPANTS_TBL, MatchSpecP),
    {'noreply', State};
handle_cast({'wh_amqp_channel',{'new_channel',_IsNew}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue',_QueueName}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Req, State) ->
    lager:debug("unhandled cast: ~p", [_Req]),
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
handle_info(_Msg, State) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
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
    _ = ets:delete(?CONFERENCES_TBL),
    _ = ets:delete(?PARTICIPANTS_TBL),
    lager:debug("FreeSWITCH conference tracker going down: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec conference_from_props(wh_proplist(), atom()) -> conference().
conference_from_props(Props, Node) ->
    conference_from_props(Props, Node, #conference{}).

-spec conference_from_props(wh_proplist(), atom(), conference()) -> conference().
conference_from_props(Props, Node, Conference) ->
    Conference#conference{node=Node
                          ,uuid=props:get_value(<<"Conference-Unique-ID">>, Props)
                          ,name=props:get_value(<<"Conference-Name">>, Props)
                          ,profile_name=props:get_value(<<"Conference-Profile-Name">>, Props)
                          ,start_time = wh_util:current_tstamp()
                          ,switch_hostname=props:get_value(<<"FreeSWITCH-Hostname">>, Props, wh_util:to_binary(Node))
                          ,switch_url=ecallmgr_fs_nodes:sip_url(Node)
                          ,switch_external_ip=ecallmgr_fs_nodes:sip_external_ip(Node)
                         }.

-spec participant_from_props(wh_proplist(), atom(), ne_binary()) -> participant().
participant_from_props(Props, Node, CallId) ->
    participant_from_props(Props, Node, CallId, #participant{}).

-spec participant_from_props(wh_proplist(), atom(), ne_binary(), participant()) -> participant().
participant_from_props(Props, Node, CallId, Participant) ->
    Participant#participant{node=Node
                            ,uuid=CallId
                            ,conference_uuid=props:get_value(<<"Conference-Unique-ID">>, Props)
                            ,conference_name=props:get_value(<<"Conference-Name">>, Props)
                            ,floor=props:get_is_true(<<"Floor">>, Props, 'false')
                            ,hear=props:get_is_true(<<"Hear">>, Props, 'true')
                            ,speak=props:get_is_true(<<"Speak">>, Props, 'true')
                            ,talking=props:get_is_true(<<"Talking">>, Props, 'false')
                            ,mute_detect=props:get_is_true(<<"Mute-Detect">>, Props, 'false')
                            ,member_id=props:get_integer_value(<<"Member-ID">>, Props, 0)
                            ,member_type=props:get_value(<<"Member-Type">>, Props)
                            ,energy_level=props:get_integer_value(<<"Energy-Level">>, Props, 0)
                            ,current_energy=props:get_integer_value(<<"Current-Energy">>, Props, 0)
                            ,video=props:get_is_true(<<"Video">>, Props, 'false')
                           }.

-spec participants_to_json(participants(), wh_json:objects()) -> wh_json:objects().
participants_to_json([], JObjs) -> JObjs;
participants_to_json([Participant|Participants], JObjs) ->
    JObj = participant_to_json(Participant),
    participants_to_json(Participants, [JObj|JObjs]).

-spec participant_to_json(participant()) -> wh_json:object().
participant_to_json(#participant{}=Participant) ->
    wh_json:from_list(participant_to_props(Participant)).

-spec participant_to_props(participant()) -> wh_proplist().
participant_to_props(#participant{uuid=UUID
                                  ,conference_name=ConfName
                                  ,conference_uuid=ConfUUID
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
                                  ,node=Node
                                 }) ->
      props:filter_undefined(
        [{<<"Call-ID">>, UUID}
         ,{<<"Conference-Name">>, ConfName}
         ,{<<"Conference-UUID">>, ConfUUID}
         ,{<<"Switch-Hostname">>, Node}
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
        ]).

-spec conference_to_props(conference()) -> wh_proplist().
conference_to_props(#conference{name=Name
                                ,uuid=UUID
                                ,node=Node
                                ,profile_name=Profile
                                ,with_floor=WithFloor
                                ,lost_floor=LostFloor
                                ,running=Running
                                ,answered=Answered
                                ,enforce_min=EnforceMin
                                ,dynamic=Dynamic
                                ,exit_sound=ExitSound
                                ,enter_sound=EnterSound
                                ,start_time=StartTime
                                ,switch_hostname=Hostname
                                ,switch_external_ip=ExternalIP
                                ,switch_url=SwitchURL
                                ,account_id=AccountId
                               }) ->
    props:filter_undefined(
      [{<<"Name">>, Name}
       ,{<<"Instance-ID">>, UUID}
       ,{<<"Node">>, Node}
       ,{<<"Profile-Name">>, Profile}
       ,{<<"With-Floor">>, WithFloor}
       ,{<<"Lost-Floor">>, LostFloor}
       ,{<<"Running">>, Running}
       ,{<<"Answered">>, Answered}
       ,{<<"Enforce-Minium">>, EnforceMin}
       ,{<<"Dynamic">>, Dynamic}
       ,{<<"Exit-Sound">>, ExitSound}
       ,{<<"Enter-Sound">>, EnterSound}
       ,{<<"Start-Time">>, StartTime}
       ,{<<"Switch-Hostname">>, Hostname}
       ,{<<"Switch-URL">>, SwitchURL}
       ,{<<"Switch-External-IP">>, ExternalIP}
       ,{<<"Account-ID">>, AccountId}
      ]).

-spec list_conferences(atom()) -> conferences() | participants().
list_conferences(Node) ->
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

-spec xml_list_to_records(xml_els(), atom()) -> conferences() | participants().
xml_list_to_records(Xml, Node) -> xml_list_to_records(Xml, Node, []).

xml_list_to_records(#xmlElement{name='conferences'
                                ,content=XmlElements
                               }, Node, Recs) ->
    xml_list_to_records(XmlElements, Node, Recs);
xml_list_to_records([], _, Recs) -> Recs;
xml_list_to_records([#xmlElement{name='conference'
                                 ,content=XmlMembers
                                }=Xml
                     | XmlElements
                    ], Node, Recs) ->
    Conference = xml_to_conference(Xml, Node),
    Participants = xml_members_to_participants(XmlMembers, Conference),
    xml_list_to_records(XmlElements, Node, [Conference | Participants ++ Recs]);
xml_list_to_records([_|XmlElements], Node, Recs) ->
    xml_list_to_records(XmlElements, Node, Recs);
xml_list_to_records(#xmlElement{name='conference'
                                ,content=XmlMembers
                               }=Xml, Node, Recs) ->
    Conference = xml_to_conference(Xml, Node),
    Participants = xml_members_to_participants(XmlMembers, Conference),
    [Conference | Participants ++ Recs];
xml_list_to_records(_, _, Recs) -> Recs.

-spec xml_to_conference(xml_el(), atom()) -> conference().
xml_to_conference(#xmlElement{name='conference'
                              ,attributes=Attrs
                             }, Node) ->
    [_, Hostname] = binary:split(wh_util:to_binary(Node), <<"@">>),
    xml_attrs_to_conference(Attrs, #conference{node=Node
                                               ,switch_hostname=Hostname
                                               ,switch_url=ecallmgr_fs_nodes:sip_url(Node)
                                               ,switch_external_ip=ecallmgr_fs_nodes:sip_external_ip(Node)
                                              }).

-spec xml_attrs_to_conference(xml_attribs(), conference()) ->
                                     conference().
xml_attrs_to_conference([], Conference) -> Conference;
xml_attrs_to_conference([#xmlAttribute{name=Name, value=Value}
                         |Attrs
                        ], Conference) ->
    C = xml_attr_to_conference(Conference, Name, Value),
    xml_attrs_to_conference(Attrs, C).

-spec xml_attr_to_conference(conference(), xml_attrib_name(), xml_attrib_value()) ->
                                    conference().
xml_attr_to_conference(Conference, 'name', Value) ->
    Conference#conference{name=wh_util:to_binary(Value)};
xml_attr_to_conference(Conference, 'member-count', Value) ->
    Conference#conference{participants=wh_util:to_integer(Value)};
xml_attr_to_conference(Conference, 'uuid', Value) ->
    Conference#conference{uuid=wh_util:to_binary(Value)};
xml_attr_to_conference(Conference, 'running', Value) ->
    Conference#conference{running=wh_util:is_true(Value)};
xml_attr_to_conference(Conference, 'answered', Value) ->
    Conference#conference{answered=wh_util:is_true(Value)};
xml_attr_to_conference(Conference, 'enforce_min', Value) ->
    Conference#conference{enforce_min=wh_util:is_true(Value)};
xml_attr_to_conference(Conference, 'dynamic', Value) ->
    Conference#conference{dynamic=wh_util:is_true(Value)};
xml_attr_to_conference(Conference, 'exit_sound', Value) ->
    Conference#conference{exit_sound=wh_util:is_true(Value)};
xml_attr_to_conference(Conference, 'enter_sound', Value) ->
    Conference#conference{enter_sound=wh_util:is_true(Value)};
xml_attr_to_conference(Conference, 'run_time', Value) ->
    Conference#conference{start_time=wh_util:decr_timeout(wh_util:current_tstamp()
                                                          ,wh_util:to_integer(Value)
                                                         )};
xml_attr_to_conference(Conference, _Name, _Value) ->
    lager:debug("unhandled conference k/v ~s: ~p", [_Name, _Value]),
    Conference.

-spec xml_members_to_participants(xml_els(), conference()) -> participants().
xml_members_to_participants([], _) -> [];
xml_members_to_participants([#xmlElement{name='members'
                                         ,content=XmlElements
                                        }
                             |_], #conference{name=Name, uuid=UUID, node=Node}) ->
    [xml_member_to_participant(Xml, #participant{node=Node
                                                 ,conference_name=Name
                                                 ,conference_uuid=UUID
                                                })
     || #xmlElement{content=Xml, name=XmlName} <- XmlElements,
        XmlName =:= 'member'
    ];
xml_members_to_participants([_|XmlElements], Conference) ->
    xml_members_to_participants(XmlElements, Conference).

-spec xml_member_to_participant(xml_els(), participant()) -> participant().
xml_member_to_participant([], Participant) -> Participant;
xml_member_to_participant([#xmlElement{name='id'
                                       ,content=Id
                                      }
                           |XmlElements
                          ], Participant) ->
    Value = wh_util:to_integer(xml_text_to_binary(Id)),
    xml_member_to_participant(XmlElements
                              ,Participant#participant{member_id=Value});
xml_member_to_participant([#xmlElement{name='flags'
                                       ,content=Xml
                                      }
                           | XmlElements
                          ], Participant) ->
    xml_member_to_participant(XmlElements
                              ,xml_member_flags_to_participant(Xml, Participant));
xml_member_to_participant([#xmlElement{name='uuid'
                                       ,content=UUID
                                      }
                           |XmlElements
                          ], Participant) ->
    CallId = wh_util:uri_decode(xml_text_to_binary(UUID)),
    lager:debug("uuid ~s callid ~s", [xml_text_to_binary(UUID), CallId]),
    xml_member_to_participant(XmlElements
                              ,Participant#participant{uuid=wh_util:to_binary(CallId)}
                             );
xml_member_to_participant([#xmlElement{name='energy'
                                       ,content=Energy
                                      }
                           |XmlElements
                          ], Participant) ->
    Value = wh_util:to_integer(xml_text_to_binary(Energy)),
    xml_member_to_participant(XmlElements
                              ,Participant#participant{energy_level=Value});
xml_member_to_participant([_|XmlElements], Participant) ->
    xml_member_to_participant(XmlElements, Participant).

-spec xml_member_flags_to_participant(xml_els(), participant()) -> participant().
xml_member_flags_to_participant([], Participant) -> Participant;
xml_member_flags_to_participant([#xmlElement{name='talking'
                                             ,content=Speak
                                            }
                                 | XmlElements
                                ], Participant) ->
    Value = wh_util:is_true(xml_text_to_binary(Speak)),
    xml_member_flags_to_participant(XmlElements
                                    ,Participant#participant{speak=Value});
xml_member_flags_to_participant([#xmlElement{name='has_floor'
                                             ,content=HasFloor
                                            }
                                 | XmlElements
                                ], Participant) ->
    Value = wh_util:is_true(xml_text_to_binary(HasFloor)),
    xml_member_flags_to_participant(XmlElements
                                    ,Participant#participant{floor=Value});
xml_member_flags_to_participant([#xmlElement{name='is_moderator'
                                             ,content=IsMod
                                            }
                                 | XmlElements
                                ], Participant) ->
    Value = wh_util:is_true(xml_text_to_binary(IsMod)),
    xml_member_flags_to_participant(XmlElements
                                    ,Participant#participant{is_moderator=Value});
xml_member_flags_to_participant([#xmlElement{name='can_hear'
                                             ,content=Hear
                                            }
                                 | XmlElements
                                ], Participant) ->
    Value = wh_util:is_true(xml_text_to_binary(Hear)),
    xml_member_flags_to_participant(XmlElements
                                    ,Participant#participant{hear=Value});
xml_member_flags_to_participant([#xmlElement{name='can_speak'
                                             ,content=Speak
                                            }
                                 | XmlElements
                                ], Participant) ->
    Value = wh_util:is_true(xml_text_to_binary(Speak)),
    xml_member_flags_to_participant(XmlElements
                                    ,Participant#participant{speak=Value});
xml_member_flags_to_participant([_|XmlElements], Participant) ->
    xml_member_flags_to_participant(XmlElements, Participant).

-spec xml_text_to_binary(xml_els()) -> ne_binary().
xml_text_to_binary(XmlElements) ->
    iolist_to_binary([V || #xmlText{value=V} <- XmlElements]).

-spec maybe_destroy_conference(ne_binary()) -> boolean().
maybe_destroy_conference(UUID) ->
    case ets:match(?PARTICIPANTS_TBL, #participant{conference_uuid=UUID, _ = '_'}) of
        [] ->
            lager:info("conference ~s was orphaned, removing", [UUID]),
            ets:delete(?CONFERENCES_TBL, UUID);
        _ -> 'false'
    end.

-spec get_conference_dictionary(conferences()) -> dict().
get_conference_dictionary(Conferences) ->
    get_conference_dictionary(Conferences, dict:new()).

-spec get_conference_dictionary(conferences(), dict()) -> dict().
get_conference_dictionary([], Dictionary) -> Dictionary;
get_conference_dictionary([#conference{uuid=UUID}=Conference
                           | Conferences
                          ], Dictionary) ->
    get_conference_dictionary(Conferences, dict:store(UUID, Conference, Dictionary));
get_conference_dictionary([_|Conferences], Dictionary) ->
    get_conference_dictionary(Conferences, Dictionary).

-spec get_participant_dictionary(participants()) -> dict().
get_participant_dictionary(Participants) ->
    get_participant_dictionary(Participants, dict:new()).

-spec get_participant_dictionary(participants(), dict()) -> dict().
get_participant_dictionary([], Dictionary) -> Dictionary;
get_participant_dictionary([#participant{uuid=UUID}=Participant
                            | Participants
                           ], Dictionary) ->
    get_participant_dictionary(Participants, dict:store(UUID, Participant, Dictionary));
get_participant_dictionary([_|Participants], Dictionary) ->
    get_participant_dictionary(Participants, Dictionary).

-spec sync_conferences(dict(), atom()) -> 'ok'.
sync_conferences(Conferences, Node) ->
    MatchSpec = [{#conference{uuid = '$1', node = '$2', _ = '_'}
                  ,[{'=:=', '$2', {'const', Node}}]
                  ,['$1']}
                ],
    CachedUUIDs = sets:from_list(ets:select(?CONFERENCES_TBL, MatchSpec)),
    SyncUUIDs = sets:from_list(dict:fetch_keys(Conferences)),
    _ = [begin
             lager:debug("removed conference ~s from cache during sync with ~s", [UUID, Node]),
             ets:delete(?CONFERENCES_TBL, UUID),
             ets:match_delete(?PARTICIPANTS_TBL, #participant{conference_uuid=UUID, _='_'})
         end
         || UUID <- sets:to_list(sets:subtract(CachedUUIDs, SyncUUIDs))
        ],
    _ = [begin
             lager:debug("added conference ~s to cache during sync with ~s", [UUID, Node]),
             Conference = dict:fetch(UUID, Conferences),
             ets:insert_new(?CONFERENCES_TBL, Conference)
         end
         || UUID <- sets:to_list(sets:subtract(SyncUUIDs, CachedUUIDs))
        ],
    'ok'.

-spec sync_participants(dict(), atom()) -> 'ok'.
sync_participants(Participants, Node) ->
    MatchSpec = [{#participant{uuid = '$1', node = '$2', _ = '_'}
                  ,[{'=:=', '$2', {'const', Node}}]
                  ,['$1']}
                ],
    CachedUUIDs = sets:from_list(ets:select(?PARTICIPANTS_TBL, MatchSpec)),
    SyncUUIDs = sets:from_list(dict:fetch_keys(Participants)),
    _ = [begin
             lager:debug("removed participant ~s from cache during sync with ~s", [UUID, Node]),
             ets:delete(?PARTICIPANTS_TBL, UUID)
         end
         || UUID <- sets:to_list(sets:subtract(CachedUUIDs, SyncUUIDs))
        ],
    _ = [begin
             lager:debug("added participant ~s to cache during sync with ~s", [UUID, Node]),
             Participant = dict:fetch(UUID, Participants),
             ets:insert_new(?PARTICIPANTS_TBL, Participant)
         end
         || UUID <- sets:to_list(sets:subtract(SyncUUIDs, CachedUUIDs))
        ],
    'ok'.

print_summary('$end_of_table') ->
    io:format("No conferences found!~n", []);
print_summary(Match) ->
    io:format("+----------------------------------+----------------------------------------------------+--------------+-------------+----------------------------------+~n"),
    io:format("| Name                             | Node                                               | Participants | Uptime(sec) | Account-ID                       |~n"),
    io:format("+==================================+====================================================+==============+=============+==================================+~n"),
    print_summary(Match, 0).

print_summary('$end_of_table', Count) ->
    io:format("+----------------------------------+----------------------------------------------------+--------------+-------------+----------------------------------+~n"),
    io:format("Found ~p conferences~n", [Count]);
print_summary({[#conference{name=Name
                            ,node=Node
                            ,start_time=StartTime
                            ,account_id=AccountId
                           }]
               ,Continuation}
              ,Count) ->
    Participants = participants(Name),
    io:format("| ~-32s | ~-50s | ~-12B | ~-11B | ~-32s |~n"
              ,[Name, Node, length(Participants), wh_util:current_tstamp() - StartTime, AccountId]
             ),
    print_summary(ets:select(Continuation), Count + 1).

print_details('$end_of_table') ->
    io:format("No conferences found!~n", []);
print_details(Match) ->
    print_details(Match, 0).

print_details('$end_of_table', Count) ->
    io:format("~nFound ~p conferences~n", [Count]);
print_details({[#conference{name=Name}=Conference]
               ,Continuation}
              ,Count) ->
    io:format("~n"),
    _ = [io:format("~-19s: ~s~n", [K, wh_util:to_binary(V)])
         || {K, V} <- conference_to_props(Conference)
        ],
    _ = case participants(Name) of
            [] -> io:format("Participants       : 0~n");
            Participants ->
                io:format("Participants       : ~B", [length(Participants)]),
                print_participant_details(Participants)
        end,
    print_details(ets:select(Continuation), Count + 1).

print_participant_details([]) -> io:format("~n");
print_participant_details([#participant{uuid=UUID}=Participant
                           | Participants
                          ]) ->
    Props = participant_to_props(Participant),
    io:format("~n    ~-52s:", [UUID]),
    print_participant_flags(Props),
    print_participant_details(Participants).

print_participant_flags([]) -> 'ok';
print_participant_flags([{Flag, 'true'}|Props]) ->
    %% Cheating, all this typing today...
    io:format(" ~s", [Flag]),
    print_participant_flags(Props);
print_participant_flags([_|Props]) ->
    print_participant_flags(Props).
