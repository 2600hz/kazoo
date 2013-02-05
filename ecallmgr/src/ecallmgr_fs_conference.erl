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
         ,destroy/2, participant_destroy/2
         ,event/3
         ,size/1, set_size/2
         ,xml_list_to_records/2
        ]).

-compile([{no_auto_import, [node/1]}]).

-include("ecallmgr.hrl").

-define(NODES_SRV, ecallmgr_fs_nodes).

-spec show_all() -> wh_json:objects().
show_all() ->
    ets:foldl(fun(Channel, Acc) ->
                      [record_to_json(Channel) | Acc]
              end, [], ?CONFERENCES_TBL).

-spec new(atom(), wh_proplist()) -> 'ok'.
new(Node, Props) ->
    gen_server:cast(?NODES_SRV, {new_conference, props_to_record(Props, Node)}).

-spec node(ne_binary()) ->
                  {'ok', atom()} |
                  {'error', 'not_found'}.
node(ConfName) ->
    case ets:lookup(?CONFERENCES_TBL, ConfName) of
        [#conference{node=Node}] -> {'ok', Node};
        _ -> {'error', 'not_found'}
    end.

-spec destroy(atom(), wh_proplist()) -> 'ok'.
destroy(Node, Props) ->
    gen_server:cast(?NODES_SRV, {conference_destroy, Node, props:get_value(<<"Conference-Name">>, Props)}).

-spec participant_destroy(atom(), wh_proplist() | ne_binary()) -> 'ok'.
participant_destroy(Node, Props) when is_list(Props) ->
    participant_destroy(Node, props:get_value(<<"Channel-ID">>, Props));
participant_destroy(Node, UUID) ->
    gen_server:cast(?NODES_SRV, {participant_destroy, Node, UUID}).

size(ConfId) ->
    case ets:lookup(?CONFERENCES_TBL, ConfId) of
        [] -> {error, invalid_conference};
        [#conference{participants=P}] -> P
    end.
set_size(ConfId, Size) when is_integer(Size) ->
    gen_server:cast(?NODES_SRV, {conference_update, ConfId, {#conference.participants, Size}}).

props_to_record(Props, Node) ->
    #conference{node=Node
                ,uuid=props:get_value(<<"Conference-Unique-ID">>, Props)
                ,name=props:get_value(<<"Conference-Name">>, Props)
                ,participants=props:get_integer_value(<<"Conference-Size">>, Props, 0)
                ,profile_name=props:get_value(<<"Conference-Profile-Name">>, Props)
               }.

props_to_participant_record(Node, Props) ->
    #participant{node=Node
                 ,uuid=props:get_value(<<"Channel-ID">>, Props, props:get_value(<<"Unique-ID">>, Props))
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

record_to_json(#conference{node=Node
                           ,uuid=UUID
                           ,name=Name
                           ,participants=Participants
                           ,profile_name=Profile
                          }) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"Node">>, Node}
         ,{<<"UUID">>, UUID}
         ,{<<"Name">>, Name}
         ,{<<"Participants">>, Participants}
         ,{<<"Profile">>, Profile}
        ])).

event(Node, 'undefined', Props) ->
    lager:debug("conf event ~s", [props:get_value(<<"Action">>, Props)]),
    case props:get_value(<<"Action">>, Props) of
        <<"conference-create">> -> new(Node, Props);
        <<"play-file">> -> ok;
        <<"play-file-done">> -> ok;
        <<"floor-change">> -> update_conference(Node, Props);
        <<"conference-destroy">> -> destroy(Node, Props);
        _Action -> lager:debug("unknown action with no uuid: ~s", [_Action])
    end;
event(Node, UUID, Props) ->
    lager:debug("conf event ~s", [props:get_value(<<"Action">>, Props)]),
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
    gen_server:cast(?NODES_SRV, {participant_update
                                 ,UUID
                                 ,[{#participant.node, Node} | participant_fields(Props)]
                                }).

update_conference(Node, Props) ->
    gen_server:cast(?NODES_SRV, {conference_update
                                 ,props:get_value(<<"Conference-Name">>, Props)
                                 ,[{#conference.node, Node} | conference_fields(Props)]
                                }).

-define(CONF_FIELDS, [{<<"Conference-Unique-ID">>, #conference.uuid}
                      ,{<<"Conference-Size">>, #conference.participants, fun props:get_integer_value/2}
                      ,{<<"Conference-Profile-Name">>, #conference.profile_name}
                      ,{<<"New-ID">>, #conference.with_floor, fun safe_integer_get/3, 0}
                      ,{<<"Old-ID">>, #conference.lost_floor, fun safe_integer_get/3, 0}
                     ]).
conference_fields(Props) -> fields(Props, ?CONF_FIELDS).

-define(PARTICIPANT_FIELDS, [{<<"Floor">>, #participant.floor, fun props:get_is_true/2}
                             %,{<<"Unique-ID">>, #participant.uuid}
                             ,{<<"Conference-Name">>, #participant.conference_name}
                             ,{<<"Hear">>, #participant.hear, fun props:get_is_true/2}
                             ,{<<"Speak">>, #participant.speak, fun props:get_is_true/2}
                             ,{<<"Talking">>, #participant.talking, fun props:get_is_true/2}
                             ,{<<"Mute-Detect">>,#participant.mute_detect, fun props:get_is_true/2}
                             ,{<<"Member-ID">>, #participant.member_id, fun props:get_integer_value/2}
                             ,{<<"Member-Type">>, #participant.member_type}
                             ,{<<"Energy-Level">>, #participant.energy_level, fun props:get_integer_value/2}
                             ,{<<"Current-Energy">>, #participant.current_energy, fun props:get_integer_value/2}
                             ,{<<"Video">>, #participant.video, fun props:get_is_true/2}
                            ]).
participant_fields(Props) -> fields(Props, ?PARTICIPANT_FIELDS).

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
        undefined -> Acc;
        V -> [{Pos, V} | Acc]
    end.
maybe_include_key(K, Pos, Acc, Props, Getter, Default) when is_function(Getter, 3) ->
    [{Pos, Getter(K, Props, Default)} | Acc].

safe_integer_get(K, Props, D) ->
    case props:get_value(K, Props) of
        undefined -> D;
        V ->
            try wh_util:to_integer(V) of
                I -> I
            catch
                error:badarg -> D
            end
    end.
