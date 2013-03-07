%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Track FreeSWITCH conference information and provide accessors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_conference).

-export([show_all/0
         ,new/2
         ,node/1
         ,destroy/2
         ,all/0, all/1, all/2
         ,event/3
         ,size/1, set_size/2
         ,xml_list_to_records/2
         ,fetch/1, fetch_full/1

         %% Participant-specific
         ,participant_destroy/2
         ,participants_list/1
         ,participants_uuids/1
         ,participant_record_to_json/1
         ,props_to_participant_record/2
        ]).

-compile([{'no_auto_import', [node/1]}]).

-include("ecallmgr.hrl").

-define(NODES_SRV, 'ecallmgr_fs_nodes').

-spec show_all() -> wh_json:objects().
show_all() ->
    ets:foldl(fun(#conference{}=Conf, Acc) -> [record_to_json(Conf) | Acc];
                 (#participant{}=P, Acc) ->   [participant_record_to_json(P) | Acc]
              end, [], ?CONFERENCES_TBL).

-spec new(atom(), wh_proplist()) -> 'ok'.
new(Node, Props) ->
    gen_server:call(?NODES_SRV, {'new_conference', props_to_record(Props, Node)}),
    ConferenceName = props:get_value(<<"Conference-Name">>, Props),
    case participants_list(ConferenceName) of
        [] -> 'ok';
        Ps ->
            Event = [{<<"Participants">>, Ps}
                     ,{<<"Conference-ID">>, ConferenceName}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            wapi_conference:publish_participants_event(ConferenceName, Event)
    end.

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
    gen_server:call(?NODES_SRV, {'conference_destroy', Node, props:get_value(<<"Conference-Name">>, Props)}).

-spec participant_destroy(atom(), wh_proplist() | ne_binary()) -> 'ok'.
participant_destroy(Node, Props) when is_list(Props) ->
    participant_destroy(Node, props:get_value(<<"Call-ID">>, Props));
participant_destroy(Node, UUID) ->
    gen_server:call(?NODES_SRV, {'participant_destroy', Node, UUID}).

size(ConfId) ->
    case ets:lookup(?CONFERENCES_TBL, ConfId) of
        [] -> {'error', 'invalid_conference'};
        [#conference{participants=P}] -> P
    end.
set_size(ConfId, Size) when is_integer(Size) ->
    gen_server:call(?NODES_SRV, {'conference_update', ConfId, {#conference.participants, Size}}).

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

add_participants_to_conference_json(ConfId, ConfJObj) ->
    {'ok', wh_json:set_value(<<"Participants">>, participants_list(ConfId), ConfJObj)}.

-spec participants_list(ne_binary()) -> wh_json:objects().
participants_list(ConfId) ->
    case ets:match_object(?CONFERENCES_TBL, #participant{conference_name=ConfId, _='_'}) of
        [] -> [];
        Ps -> [participant_record_to_json(P) || P <- Ps]
    end.

-spec participants_uuids(ne_binary()) -> wh_json:objects().
participants_uuids(ConfId) ->
    ets:match(?CONFERENCES_TBL, #participant{conference_name=ConfId, uuid='$1', _='_'}).

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

-spec xml_list_to_records(xml_els(), atom()) -> conferences() | participants().
xml_list_to_records(Xml, Node) ->
    xml_list_to_records(Xml, Node, []).

xml_list_to_records(#xmlElement{name='conferences'
                                ,content=Cs
                               }, Node, Recs) ->
    xml_list_to_records(Cs, Node, Recs);
xml_list_to_records([], _, Recs) -> Recs;
xml_list_to_records([#xmlElement{name='conference'
                                 ,content=Participants
                                }=ConfXml
                     |Confs], Node, Recs) ->
    xml_list_to_records(Confs, Node, [fs_xml_conference:xml_to_conference(ConfXml, Node)
                                      | xml_members_to_records(Participants, Node) ++ Recs
                                     ]);
xml_list_to_records([_El|Els], Node, Recs) ->
    xml_list_to_records(Els, Node, Recs);
xml_list_to_records(#xmlElement{name='conference'
                                ,content=Participants
                               }=ConfXml, Node, Recs) ->
    [fs_xml_conference:xml_to_conference(ConfXml, Node)
     | xml_members_to_records(Participants, Node) ++ Recs
    ];
xml_list_to_records(_, _, Recs) -> Recs.

xml_members_to_records([], _Node) -> [];
xml_members_to_records([#xmlElement{name='members'
                                    ,content=Participants
                                   }
                        |_], Node) ->
    [fs_xml_member:xml_to_participant(P, Node) || #xmlElement{name='member'}=P <- Participants];
xml_members_to_records([_El|Els], Node) ->
    xml_members_to_records(Els, Node).

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

event(Node, 'undefined', Props) ->
    lager:debug("conf event ~s", [props:get_value(<<"Action">>, Props)]),
    case props:get_value(<<"Action">>, Props) of
        <<"conference-create">> -> new(Node, Props);
        <<"play-file">> -> relay_event(Props);
        <<"play-file-done">> -> relay_event(Props);
        <<"floor-change">> -> update_conference(Node, Props);
        <<"conference-destroy">> -> destroy(Node, Props);
        <<"start-recording">> -> relay_event(Props);
        <<"stop-recording">> -> relay_event(Props);
        _Action -> lager:debug("unknown action with no uuid: ~s", [_Action])
    end;
event(Node, UUID, Props) ->
    lager:debug("conf ~s: event ~s", [props:get_value(<<"Conference-Name">>, Props)
                                      ,props:get_value(<<"Action">>, Props)
                                     ]),
    case props:get_value(<<"Action">>, Props) of
        <<"add-member">> -> update_all(Node, UUID, Props);
        <<"floor-change">> -> update_all(Node, UUID, Props);
        <<"start-talking">> -> update_all(Node, UUID, Props);
        <<"stop-talking">> -> update_all(Node, UUID, Props);
        <<"del-member">> -> participant_destroy(Node, UUID);
        _Action -> lager:debug("unhandled conference action ~s", [_Action])
    end.

update_all(Node, UUID, Props) ->
    update_conference(Node, Props),
    update_participant(Node, UUID, Props).

update_participant(Node, UUID, Props) ->
    gen_server:call(?NODES_SRV, {'participant_update'
                                 ,Node
                                 ,UUID
                                 ,[{#participant.node, Node} | participant_fields(Props)]
                                }),
    ConferenceName = props:get_value(<<"Conference-Name">>, Props),
    case participants_list(ConferenceName) of
        [] -> 'ok';
        Ps ->
            Event = [{<<"Participants">>, Ps}
                     ,{<<"Conference-ID">>, ConferenceName}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            wapi_conference:publish_participants_event(ConferenceName, Event)
    end.

update_conference(Node, Props) ->
    ProfileProps = ecallmgr_util:get_interface_properties(Node),
    gen_server:call(?NODES_SRV, {'conference_update'
                                 ,Node
                                 ,props:get_value(<<"Conference-Name">>, Props)
                                 ,[{#conference.node, Node}
                                   ,{#conference.switch_url, props:get_value(<<"URL">>, ProfileProps)}
                                   ,{#conference.switch_external_ip, props:get_value(<<"Ext-SIP-IP">>, ProfileProps)}
                                   | conference_fields(Props)
                                  ]
                                }).

relay_event(Props) ->
    [?NODES_SRV ! {'event', [UUID | Props]} ||
        UUID <- participants_uuids(props:get_value(<<"Conference-Name">>, Props))
    ].

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
