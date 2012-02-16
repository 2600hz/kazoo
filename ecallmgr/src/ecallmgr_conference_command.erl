%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Execute conference commands
%%% @end
%%% Created : 16 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------

-module(ecallmgr_conference_command).

-export([exec_cmd/3]).
-export([run/0]).

-include("ecallmgr.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-spec exec_cmd/3 :: (atom(), ne_binary(), wh_json:json_object()) -> 'ok'.
exec_cmd(Node, ConferenceId, JObj) ->
    App = wh_json:get_value(<<"Application-Name">>, JObj),
    case get_conf_command(Node, ConferenceId, JObj, App) of
        {'error', Msg}=E ->
            _ = ecallmgr_util:fs_log(Node, "whistle error while building command ~s: ~s", [App, Msg]),
            send_response(App, E, wh_json:get_value(<<"Server-ID">>, JObj), JObj);
        {AppName, AppData} ->
            Command = wh_util:to_list(list_to_binary([ConferenceId, AppName, " ", AppData])),
            Result = freeswitch:api(Node, 'conference', Command),
            send_response(App, Result, wh_json:get_value(<<"Server-ID">>, JObj), JObj)
    end.

%% return the app name and data (as a binary string) to send to the FS ESL via mod_erlang_event
-spec get_conf_command/4 :: (atom(), ne_binary(), wh_json:json_object(), ne_binary()) -> ne_binary() | {'error', string()}.
get_conf_command(_Node, _ConferenceId, JObj, <<"deaf_participant">>) ->
    case wapi_conference:deaf_participant_v(JObj) of
        false -> 
            {'error', <<"conference deaf_participant failed to execute as JObj did not validate.">>};
        true ->
            {<<"deaf">>, wh_json:get_binary_value(<<"Participant">>, JObj)}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"participant_energy">>) ->
    case wapi_conference:participant_energy_v(JObj) of
        false ->
            {'error', <<"conference participant_energy failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Energy-Level">>, JObj, <<"20">>)
                                  ]),
            {<<"energy">>, Args}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"kick">>) ->
    case wapi_conference:kick_v(JObj) of
        false ->
            {'error', <<"conference kick failed to execute as JObj did not validate.">>};
        true ->
            {<<"hup">>, wh_json:get_binary_value(<<"Participant">>, JObj, <<"last">>)}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"participants">>) ->
    case wapi_conference:participants_v(JObj) of
        false ->
            {'error', <<"conference participants failed to execute as JObj did not validate.">>};
        true ->
            {<<"list">>, <<>>} %% !!!!!!!!!!!!!!!!!!!
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"lock">>) ->
    case wapi_conference:lock_v(JObj) of
        false ->
            {'error', <<"conference lock failed to execute as JObj did not validate.">>};
        true ->
            {<<"lock">>, <<>>}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"mute_participant">>) ->
    case wapi_conference:mute_participant_v(JObj) of
        false ->
            {'error', <<"conference mute_participant failed to execute as JObj did not validate.">>};
        true ->
            {<<"lock">>, <<>>}
    end;
get_conf_command(_Node, ConferenceId, JObj, <<"play">>) ->
    case wapi_conference:play_v(JObj) of
        false ->
            {'error', <<"conference play failed to execute as JObj did not validate.">>};
        true ->
            UUID = wh_json:get_ne_value(<<"Call-ID">>, JObj, ConferenceId),
            Media = list_to_binary(["'", ecallmgr_util:media_path(wh_json:get_value(<<"Media-Name">>, JObj), UUID), "'"]),
            Args = case wh_json:get_value(<<"Participant">>, JObj) of
                       undefined -> Media;                       
                       Participant -> list_to_binary([Media, " ", Participant])
                   end,
            {<<"play">>, Args}
    end;
get_conf_command(_Node, ConferenceId, JObj, <<"record">>) ->
    case wapi_conference:record_v(JObj) of
        false ->
            {'error', <<"conference record failed to execute as JObj did not validate.">>};
        true ->
            UUID = wh_json:get_binary_value(<<"Call-ID">>, JObj, ConferenceId),
            MediaName = wh_json:get_binary_value(<<"Media-Name">>, JObj),
            Media = ecallmgr_media_registry:register_local_media(MediaName, UUID),
            {<<"record">>, Media}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"relate_participants">>) ->
    case wapi_conference:relate_participants_v(JObj) of
        false ->
            {'error', <<"conference relate_participants failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Other-Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Relationship">>, JObj, <<"clear">>)
                                  ]),
            {<<"relate">>, Args}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"set">>) ->
    case wapi_conference:set_v(JObj) of
        false ->
            {'error', <<"conference set failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Parameter">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Value">>, JObj)
                                  ]),
            {<<"set">>, Args}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"stop_play">>) ->
    case wapi_conference:stop_play_v(JObj) of
        false ->
            {'error', <<"conference stop_play failed to execute as JObj did not validate.">>};
        true ->
            Affects = wh_json:get_binary_value(<<"Affects">>, JObj, <<>>),
            Args = case wh_json:get_binary_value(<<"Participant">>, JObj) of
                       undefined -> Affects;
                       Participant -> list_to_binary([Affects, " ", Participant])
                   end,
            {<<"stop">>, Args}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"undeaf_participant">>) ->
    case wapi_conference:undeaf_participant_v(JObj) of
        false ->
            {'error', <<"conference undeaf_participant failed to execute as JObj did not validate.">>};
        true ->
            {<<"undeaf">>, wh_json:get_binary_value(<<"Participant">>, JObj)}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"unlock">>) ->
    case wapi_conference:unlock_v(JObj) of
        false ->
            {'error', <<"conference unlock failed to execute as JObj did not validate.">>};
        true ->
            {<<"unlock">>, <<>>}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"unmute">>) ->
    case wapi_conference:unmute_v(JObj) of
        false ->
            {'error', <<"conference unmute failed to execute as JObj did not validate.">>};
        true ->
            {<<"unmute">>, wh_json:get_binary_value(<<"Participant">>, JObj)}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"participant_volume_in">>) ->
    case wapi_conference:participant_volume_in_v(JObj) of
        false ->
            {'error', <<"conference participant_volume_in failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Volume-In-Level">>, JObj, <<"0">>)
                                  ]),
            {<<"volume_in">>, Args}
    end;
get_conf_command(_Node, _ConferenceId, JObj, <<"participant_volume_out">>) ->
    case wapi_conference:participant_volume_out_v(JObj) of
        false ->
            {'error', <<"conference participant_volume_out failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Volume-Out-Level">>, JObj, <<"0">>)
                                  ]),
            {<<"volume_out">>, Args}
    end.

send_response(_, _, undefined, _) ->
    ok;
send_response(_, {ok, <<"Non-Existant ID", _/binary>> = Msg}, RespQ, Command) ->
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
             ,{<<"Error-Message">>, binary:replace(Msg, <<"\n">>, <<>>)}
             ,{<<"Request">>, Command}
             | wh_api:default_headers(<<>>, <<"conference">>, <<"error">>, ?APP_NAME, ?APP_VERSION)
            ],
    wapi_conference:publish_error(RespQ, Error);
send_response(<<"find">>, {ok, Node}, RespQ, Command) ->
    Props = ecallmgr_util:get_interface_properties(Node),
    [_, Hostname] = binary:split(wh_util:to_binary(Node), <<"@">>),
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
            ,{<<"Switch-Hostname">>, Hostname}
            ,{<<"External-SIP-IP">>, props:get_value(<<"Ext-SIP-IP">>, Props)}
            | wh_api:default_headers(<<>>, <<"conference">>, <<"find">>, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_conference:publish_participants_resp(RespQ, Resp);
send_response(<<"list">>, {ok, Participants}, RespQ, Command) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
            ,{<<"Participants">>, parse_participants(Participants)}
            | wh_api:default_headers(<<>>, <<"conference">>, <<"participants_resp">>, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_conference:publish_participants_resp(RespQ, Resp);
send_response(_, {ok, Response}, RespQ, Command) ->
    case binary:match(Response, <<"not found">>) of
        nomatch -> ok;
        _Else ->
            Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
                     ,{<<"Error-Message">>, binary:replace(Response, <<"\n">>, <<>>)}
                     ,{<<"Request">>, Command}
                     | wh_api:default_headers(<<>>, <<"conference">>, <<"error">>, ?APP_NAME, ?APP_VERSION)
                    ],
            wapi_conference:publish_error(RespQ, Error)
    end;
send_response(_, {error, Msg}, RespQ, Command) ->
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
             ,{<<"Error-Message">>, binary:replace(wh_util:to_binary(Msg), <<"\n">>, <<>>)}
             ,{<<"Request">>, Command}
             | wh_api:default_headers(<<>>, <<"conference">>, <<"error">>, ?APP_NAME, ?APP_VERSION)
            ],
    wapi_conference:publish_error(RespQ, Error);
send_response(_, timeout, RespQ, Command) ->
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Command, <<>>)}
             ,{<<"Error-Message">>, <<"Node Timeout">>}
             ,{<<"Request">>, Command}
             | wh_api:default_headers(<<>>, <<"conference">>, <<"error">>, ?APP_NAME, ?APP_VERSION)
            ],
    wapi_conference:publish_error(RespQ, Error).

-spec parse_participants/1 :: (nonempty_string() | ne_binary()) -> wh_json:json_objects().
parse_participants(Participants) ->
    CSV = wh_util:to_list(Participants),
        lists:foldr(fun(Line, Acc) ->                          
                        [wh_json:from_list(parse_participant(Line))|Acc]
                end, [], string:tokens(CSV, "\n")).

-spec parse_participant/1 :: (nonempty_string()) -> proplist().
parse_participant(Line) ->    
    [Id, _, CallId, CidName, CidNum, Status, VolIn, _, VolOut, Energy] = string:tokens(Line, ";"),
    [{<<"Participant-ID">>, wh_util:to_binary(Id)}
     ,{<<"Call-ID">>, wh_util:to_binary(CallId)}
     ,{<<"CID-Name">>, wh_util:to_binary(CidName)}
     ,{<<"CID-Number">>, wh_util:to_binary(CidNum)}
     ,{<<"Status">>, wh_util:to_binary(Status)}
     ,{<<"Volume-In">>, wh_util:to_binary(VolIn)}
     ,{<<"Volume-Out">>, wh_util:to_binary(VolOut)}
     ,{<<"Energy-Threshold">>, wh_util:to_binary(Energy)}
    ].

run() ->
    {ok, Response} = freeswitch:api('freeswitch@fs001-dev-vb.2600hz.com', 'conference', "xml_list"),
    {Xml, _} = xmerl_scan:string(binary_to_list(binary:replace(Response, <<"\n">>, <<>>, [global]))),
    conferences_xml_to_json(Xml, wh_json:new()).

conferences_xml_to_json([], JObj) ->
    JObj;
conferences_xml_to_json(#xmlElement{name='conferences', content=Content}, JObj) ->
    conferences_xml_to_json(Content, JObj);
conferences_xml_to_json([#xmlElement{name='conferences', content=Content}|_], JObj) ->
    conferences_xml_to_json(Content, JObj);
conferences_xml_to_json([#xmlElement{attributes=Attributes, name='conference', content=Content}|ConferencesXml], JObj) ->
    Conference = wh_json:from_list([{<<"Participants">>, members_xml_to_json(Content, wh_json:new())}
                                    |[{K, wh_util:to_binary(V)} 
                                      || #xmlAttribute{name=Name, value=V} <- Attributes
                                             ,(K = props:get_value(Name, ?FS_CONFERNCE_ATTRS)) =/= undefined
                                     ]]),
    case wh_json:get_value(<<"Conference-ID">>, Conference) of
        undefined -> conferences_xml_to_json(ConferencesXml, JObj);
        ConferenceId ->
            conferences_xml_to_json(ConferencesXml, wh_json:set_value(ConferenceId, Conference, JObj))
    end;
conferences_xml_to_json([_|ConferencesXml], JObj) ->
    conferences_xml_to_json(ConferencesXml, JObj).

members_xml_to_json([], JObj) ->
    JObj;
members_xml_to_json(#xmlElement{content=Content, name='members'}, JObj) ->
    members_xml_to_json(Content, JObj);
members_xml_to_json([#xmlElement{content=Content, name='members'}|_], JObj) ->
    members_xml_to_json(Content, JObj);
members_xml_to_json([#xmlElement{content=Content, name='member'}|MembersXml], JObj) ->
    Member = member_xml_to_json(Content, wh_json:new()),
    case wh_json:get_value(<<"Participant-ID">>, Member) of
        undefined -> members_xml_to_json(MembersXml, JObj);
        ParticipantId ->
            members_xml_to_json(MembersXml, wh_json:set_value(ParticipantId, Member, JObj))
    end;
members_xml_to_json([_|MembersXml], JObj) ->    
    members_xml_to_json(MembersXml, JObj).

member_xml_to_json([], JObj) ->
    JObj;
member_xml_to_json([#xmlElement{name='flags', content=Content}|Xml], JObj) ->
    Flags = member_flags_xml_to_json(Content, wh_json:new()),
    member_xml_to_json(Xml, wh_json:set_value(<<"Flags">>, Flags, JObj));
member_xml_to_json([#xmlElement{name=Name, content=Content}|Xml], JObj) ->
    case {props:get_value(Name, ?FS_CONFERENCE_PARTICIPANT), Content} of
        {Key, [#xmlText{value=Value}]} when is_binary(Key) ->
            member_xml_to_json(Xml, wh_json:set_value(Key, wh_util:to_binary(Value), JObj));
        _Else ->
            member_xml_to_json(Xml, JObj)
    end;
member_xml_to_json([_Else|Xml], JObj) ->
    member_xml_to_json(Xml, JObj).

member_flags_xml_to_json([], JObj) ->
    JObj;
member_flags_xml_to_json([#xmlElement{name=Name, content=Content}|Xml], JObj) ->
    case {props:get_value(Name, ?FS_CONFERENCE_FLAGS), Content} of
        {Key, [#xmlText{value=Value}]} when is_binary(Key) ->
            member_flags_xml_to_json(Xml, wh_json:set_value(Key, wh_util:to_binary(Value), JObj));
        _Else ->
            member_flags_xml_to_json(Xml, JObj)
    end;
member_flags_xml_to_json([_|Xml], JObj) ->
    member_flags_xml_to_json(Xml, JObj).
