%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Track FreeSWITCH conference information and provide accessors
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_conferences).
-behaviour(gen_listener).

-export([start_link/0]).
-export([summary/0
        ,summary/1
        ,count/0
        ]).
-export([details/0
        ,details/1
        ]).
-export([create/2]).
-export([update/2]).
-export([destroy/1]).
-export([conference/1]).
-export([node/1]).
-export([participants/1]).
-export([participants_to_json/1]).
-export([participant_create/2]).
-export([participant_update/2]).
-export([participant_destroy/1]).
-export([participant_get/1]).
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

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

-define(CONFERENCES_TBL, 'ecallmgr_conferences').
-define(PARTICIPANTS_TBL, 'ecallmgr_participants').

-compile([{'no_auto_import', [node/1]}]).

-define(RESPONDERS, [{{?MODULE, 'handle_search_req'}
                     ,[{<<"conference">>, <<"search_req">>}]
                     }
                    ]).
-define(BINDINGS, [{'conference', [{'restrict_to', ['discovery']}
                                  ,'federate'
                                  ]}
                  ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}, ?MODULE,
                            [{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ], []).

-spec summary() -> 'ok'.
summary() ->
    MatchSpec = [{#conference{_ = '_'}, [], ['$_']}],
    print_summary(ets:select(?CONFERENCES_TBL, MatchSpec, 1)).

-spec count() -> non_neg_integer().
count() -> ets:info(?CONFERENCES_TBL, 'size').

-spec summary(kz_term:text()) -> 'ok'.
summary(Node) when not is_atom(Node) ->
    summary(kz_term:to_atom(Node, 'true'));
summary(Node) ->
    MatchSpec = [{#conference{node=Node, _ = '_'}, [], ['$_']}],
    print_summary(ets:select(?CONFERENCES_TBL, MatchSpec, 1)).

-spec details() -> 'ok'.
details() ->
    MatchSpec = [{#conference{_ = '_'}, [], ['$_']}],
    print_details(ets:select(?CONFERENCES_TBL, MatchSpec, 1)).

-spec details(kz_term:text()) -> 'ok'.
details(UUID) when not is_binary(UUID) ->
    details(kz_term:to_binary(UUID));
details(UUID) ->
    MatchSpec = [{#conference{uuid=UUID, _ = '_'}, [], ['$_']}],
    print_details(ets:select(?CONFERENCES_TBL, MatchSpec, 1)).

-spec create(kz_json:object(), atom()) -> conference().
create(JObj, Node) ->
    gen_server:call(?SERVER, {'conference_create', JObj, Node}).

-spec update(kz_term:ne_binary(), kz_term:proplist() | {integer(), any()}) -> 'ok'.
update(UUID, Update) ->
    gen_server:call(?SERVER, {'conference_update', UUID, Update}).

-spec destroy(kz_term:ne_binary()) -> 'ok'.
destroy(UUID) ->
    gen_server:call(?SERVER, {'conference_destroy', UUID}).

-spec node(kz_term:ne_binary()) ->
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

-spec conference(kz_term:ne_binary()) ->  {'ok', conference()} | {'error', 'not_found'}.
conference(UUID) ->
    case ets:match_object(?CONFERENCES_TBL, #conference{uuid=UUID, _ = '_'}) of
        %% TODO: this ignores conferences on multiple nodes until big-conferences
        [#conference{}=Conference|_] -> {'ok', Conference};
        %% [#conference{}|_]=Conferences ->
        %%     {'error', 'multiple_conferences'
        %%      ,[Node || #conference{node=Node} <- Conferences]
        %%     };
        _ -> {'error', 'not_found'}
    end.

-spec participants(conference() | kz_term:ne_binary()) -> participants().
participants(#conference{name=Name}) -> participants(Name);
participants(Name) ->
    %% Note: used the conference name supports participants on multiple nodes
    ets:match_object(?PARTICIPANTS_TBL, #participant{conference_name=Name, _ = '_'}).

-spec participants_to_json(participants()) -> kz_json:objects().
participants_to_json(Participants) ->
    participants_to_json(Participants, []).

-spec participant_create(kz_evt_freeswitch:data(), atom()) -> participant().
participant_create(Props, Node) ->
    gen_server:call(?SERVER, {'participant_create', Props, Node}).

-spec participant_update(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
participant_update(CallId, Update) ->
    gen_server:call(?SERVER, {'participant_update', CallId, Update}).

-spec participant_get(kz_term:ne_binary()) -> participant().
participant_get(CallId) ->
    case ets:lookup(?PARTICIPANTS_TBL, CallId) of
        [Participant=#participant{}] ->
            Participant;
        _ ->
            lager:error("no participant data by call-id ~p", [CallId])
    end.

-spec participant_destroy(kz_term:ne_binary()) -> 'ok'.
participant_destroy(CallId) ->
    gen_server:call(?SERVER, {'participant_destroy', CallId}).

-spec sync_node(atom()) -> 'ok'.
sync_node(Node) -> gen_server:cast(?SERVER, {'sync_node', Node}).

-spec flush_node(atom()) -> 'ok'.
flush_node(Node) -> gen_server:cast(?SERVER, {'flush_node', Node}).

-spec handle_search_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_search_req(JObj, Props) ->
    case kz_json:get_ne_binary_value(<<"Conference-ID">>, JObj) of
        'undefined' -> handle_search_account(JObj, Props);
        ConferenceId -> handle_search_conference(JObj, Props, ConferenceId)
    end.

-spec handle_search_conference(kz_json:object(), kz_term:proplist(), kz_term:ne_binary()) -> 'ok'.
handle_search_conference(JObj, _Props, Name) ->
    lager:info("received search request for conference name ~s", [Name]),
    case ets:match_object(?CONFERENCES_TBL, #conference{name=Name, _ = '_'}) of
        %% TODO: this ignores conferences on multiple nodes until big-conferences
        [#conference{uuid=UUID
                    ,start_time=StartTime
                    ,locked=Locked
                    ,switch_hostname=Hostname
                    ,switch_url=SwitchURL
                    ,switch_external_ip=ExternalIP
                    }
         | _Conferences
        ] ->
            lager:debug("sending affirmative search response for conference ~s", [Name]),
            Participants = participants(Name),
            Resp = [{<<"Msg-ID">>, kz_api:msg_id(JObj)}
                   ,{<<"Conference-ID">>, Name}
                   ,{<<"UUID">>, UUID}
                   ,{<<"Run-Time">>, kz_time:now_s() - StartTime}
                   ,{<<"Start-Time">>, StartTime}
                   ,{<<"Locked">>, Locked}
                   ,{<<"Switch-Hostname">>, Hostname}
                   ,{<<"Switch-URL">>, SwitchURL}
                   ,{<<"Switch-External-IP">>, ExternalIP}
                   ,{<<"Participant-Count">>, length(Participants)}
                   ,{<<"Participants">>, participants_to_json(Participants)}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            kapi_conference:publish_search_resp(kz_api:server_id(JObj), Resp);
        [] ->
            lager:debug("sending error search response, conference not found"),
            Error = [{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                    ,{<<"Error-Message">>, <<"Conference ", Name/binary, " not found">>}
                    ,{<<"Request">>, JObj}
                    ,{<<"Conference-ID">>, Name}
                     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            kapi_conference:publish_error(kz_api:server_id(JObj), Error)
    end.

-spec handle_search_account(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_search_account(JObj, _Props) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    lager:info("received search request for account ~s", [AccountId]),
    case ets:match_object(?CONFERENCES_TBL, #conference{account_id=AccountId, _ = '_'}) of
        [] ->
            lager:debug("sending error search response, conference not found"),
            Resp = [{<<"Msg-ID">>, kz_api:msg_id(JObj)}
                   ,{<<"Account-ID">>, AccountId}
                   ,{<<"Conferences">>, kz_json:new()}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            kapi_conference:publish_search_resp(kz_api:server_id(JObj), Resp);
        Conferences ->
            Payload = [conference_resp(Conference) || Conference <- Conferences],
            lager:debug("sending affirmative search response for account ~s", [AccountId]),
            Resp = [{<<"Msg-ID">>, kz_api:msg_id(JObj)}
                   ,{<<"Account-ID">>, AccountId}
                   ,{<<"Conferences">>, kz_json:from_list(Payload)}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            kapi_conference:publish_search_resp(kz_api:server_id(JObj), Resp)
    end.

-spec conference_resp(conference()) -> {kz_term:ne_binary(), kz_json:object()}.
conference_resp(#conference{uuid=UUID
                           ,name=Name
                           ,start_time=StartTime
                           ,locked=Locked
                           ,switch_hostname=Hostname
                           ,switch_url=SwitchURL
                           ,switch_external_ip=ExternalIP
                           }) ->
    Participants = participants(Name),
    {Moderators, Members} = lists:partition(fun is_moderator/1, Participants),
    Resp = [{<<"UUID">>, UUID}
           ,{<<"Run-Time">>, kz_time:now_s() - StartTime}
           ,{<<"Start-Time">>, StartTime}
           ,{<<"Is-Locked">>, Locked}
           ,{<<"Switch-Hostname">>, Hostname}
           ,{<<"Switch-URL">>, SwitchURL}
           ,{<<"Switch-External-IP">>, ExternalIP}
           ,{<<"Participant-Count">>, length(Participants)}
           ,{<<"Moderators">>, length(Moderators)}
           ,{<<"Members">>, length(Members)}
           ],
    {Name, kz_json:from_list(Resp)}.

-spec is_moderator(participant()) -> boolean().
is_moderator(#participant{conference_channel_vars=Vars}) ->
    kz_json:is_true(<<"Is-Moderator">>, Vars, 'false').

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    process_flag('trap_exit', 'true'),
    lager:info("starting FreeSWITCH conferences tracker"),
    _ = ets:new(?CONFERENCES_TBL, ['set', 'protected', 'named_table', {'keypos', #conference.uuid}]),
    _ = ets:new(?PARTICIPANTS_TBL, ['set', 'protected', 'named_table', {'keypos', #participant.uuid}]),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'conference_create', JObj, Node}, _, State) ->
    Conference = conference_from_jobj(JObj, Node),
    _ = ets:insert_new(?CONFERENCES_TBL, Conference),
    {'reply', Conference, State};
handle_call({'conference_update', UUID, Update}, _, State) ->
    _ = ets:update_element(?CONFERENCES_TBL, UUID, Update),
    {'reply', 'ok', State};
handle_call({'conference_destroy', UUID}, _, State) ->
    MatchSpecC = [{#conference{uuid=UUID, _ = '_'}, [], ['true']}],
    _ = ets:select_delete(?CONFERENCES_TBL, MatchSpecC),
    MatchSpecP = [{#participant{conference_uuid=UUID, _ = '_'}, [], ['true']}],
    _ = ets:select_delete(?PARTICIPANTS_TBL, MatchSpecP),
    {'reply', 'ok', State};
handle_call({'participant_create', JObj, Node}, _, State) ->
    Participant = participant_from_jobj(JObj, Node),
    _ = ets:insert_new(?PARTICIPANTS_TBL, Participant),
    {'reply', Participant, State};
handle_call({'participant_update', CallId, Update}, _, State) ->
    _ = ets:update_element(?PARTICIPANTS_TBL, CallId, Update),
    {'reply', 'ok', State};
handle_call({'participant_destroy', CallId}, _, State) ->
    _ = case ets:lookup(?PARTICIPANTS_TBL, CallId) of
            [#participant{}] ->
                _ = ets:delete(?PARTICIPANTS_TBL, CallId);
            _Else -> 'ok'
        end,
    {'reply', 'ok', State};
handle_call(_Req, _From, State) ->
    lager:debug("unhandled call from ~p: ~p", [_From, _Req]),
    {'reply', {'error', 'unimplemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
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
    MatchSpecC = [{#conference{node = Node, _ = '_'}, [], ['true']}],
    _ = ets:select_delete(?CONFERENCES_TBL, MatchSpecC),
    MatchSpecP = [{#participant{node = Node, _ = '_'}, [], ['true']}],
    _ = ets:select_delete(?PARTICIPANTS_TBL, MatchSpecP),
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue',_QueueName}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Req, State) ->
    lager:debug("unhandled cast: ~p", [_Req]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Msg, State) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    _ = ets:delete(?CONFERENCES_TBL),
    _ = ets:delete(?PARTICIPANTS_TBL),
    lager:debug("conference tracker going down: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) -> {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec participants_to_json(participants(), kz_json:objects()) -> kz_json:objects().
participants_to_json([], JObjs) -> JObjs;
participants_to_json([Participant|Participants], JObjs) ->
    JObj = participant_to_json(Participant),
    participants_to_json(Participants, [JObj|JObjs]).

-spec participant_to_json(participant()) -> kz_json:object().
participant_to_json(#participant{}=Participant) ->
    kz_json:from_list(participant_to_props(Participant)).

-spec participant_to_props(participant()) -> kz_term:proplist().
participant_to_props(#participant{uuid=UUID
                                 ,conference_name=ConfName
                                 ,conference_uuid=ConfUUID
                                 ,node=Node
                                 ,join_time=JoinTime
                                 ,caller_id_name=CallerIDName
                                 ,caller_id_number=CallerIDNumber
                                 ,custom_channel_vars=CCVs
                                 ,conference_channel_vars=ConfVars
                                 ,custom_application_vars=CAVs
                                 }) ->
    [_Name, Hostname] = binary:split(kz_term:to_binary(Node), <<"@">>),
    props:filter_undefined(
      [{<<"Call-ID">>, UUID}
      ,{<<"Conference-Name">>, ConfName}
      ,{<<"Conference-UUID">>, ConfUUID}
      ,{<<"Switch-Hostname">>, Hostname}
      ,{<<"Participant-ID">>, kz_json:get_integer_value(<<"Member-ID">>, ConfVars)}
      ,{<<"Join-Time">>, JoinTime}
      ,{<<"Caller-ID-Name">>, CallerIDName}
      ,{<<"Caller-ID-Number">>, CallerIDNumber}
      ,{<<"Custom-Channel-Vars">>, CCVs}
      ,{<<"Conference-Channel-Vars">>, ConfVars}
      ,{<<"Custom-Application-Vars">>, CAVs}
      ]).

-spec conference_to_props(conference()) -> kz_term:proplist().
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
                               ,locked=Locked
                               ,handling_locally=IsLocal
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
      ,{<<"Enforce-Minimum">>, EnforceMin}
      ,{<<"Dynamic">>, Dynamic}
      ,{<<"Exit-Sound">>, ExitSound}
      ,{<<"Enter-Sound">>, EnterSound}
      ,{<<"Start-Time">>, StartTime}
      ,{<<"Switch-Hostname">>, Hostname}
      ,{<<"Switch-URL">>, SwitchURL}
      ,{<<"Switch-External-IP">>, ExternalIP}
      ,{<<"Account-ID">>, AccountId}
      ,{<<"Locked">>, Locked}
      ,{<<"Is-Local">>, IsLocal}
      ]).

-spec list_conferences(atom()) -> conferences() | participants().
list_conferences(Node) ->
    case freeswitch:api(Node, 'conference', "xml_list") of
        {'ok', XmlStr} ->
            {Xml, _} = xmerl_scan:string(kz_term:to_list(XmlStr)),
            case catch xml_list_to_records(Xml, Node) of
                {'EXIT', _R} -> [];
                Rs -> Rs
            end;
        {'error', _} -> []
    end.

-spec xml_list_to_records(kz_types:xml_els(), atom()) -> conferences() | participants().
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

-spec xml_to_conference(kz_types:xml_el(), atom()) -> conference().
xml_to_conference(#xmlElement{name='conference'
                             ,attributes=Attrs
                             }, Node) ->
    [_, Hostname] = binary:split(kz_term:to_binary(Node), <<"@">>),
    xml_attrs_to_conference(Attrs, #conference{node=Node
                                              ,switch_hostname=Hostname
                                              ,switch_url=ecallmgr_fs_nodes:sip_url(Node)
                                              ,switch_external_ip=ecallmgr_fs_nodes:sip_external_ip(Node)
                                              }).

-spec xml_attrs_to_conference(kz_types:xml_attribs(), conference()) ->
                                     conference().
xml_attrs_to_conference([], Conference) -> Conference;
xml_attrs_to_conference([#xmlAttribute{name=Name, value=Value}
                         |Attrs
                        ], Conference) ->
    C = xml_attr_to_conference(Conference, Name, Value),
    xml_attrs_to_conference(Attrs, C).

-spec xml_attr_to_conference(conference(), kz_types:xml_attrib_name(), kz_types:xml_attrib_value()) ->
                                    conference().
xml_attr_to_conference(Conference, 'name', Value) ->
    Conference#conference{name=kz_term:to_binary(Value)};
xml_attr_to_conference(Conference, 'member-count', Value) ->
    Conference#conference{participants=kz_term:to_integer(Value)};
xml_attr_to_conference(Conference, 'uuid', Value) ->
    Conference#conference{uuid=kz_term:to_binary(Value)};
xml_attr_to_conference(Conference, 'running', Value) ->
    Conference#conference{running=kz_term:is_true(Value)};
xml_attr_to_conference(Conference, 'locked', Value) ->
    Conference#conference{locked=kz_term:is_true(Value)};
xml_attr_to_conference(Conference, 'answered', Value) ->
    Conference#conference{answered=kz_term:is_true(Value)};
xml_attr_to_conference(Conference, 'enforce_min', Value) ->
    Conference#conference{enforce_min=kz_term:is_true(Value)};
xml_attr_to_conference(Conference, 'dynamic', Value) ->
    Conference#conference{dynamic=kz_term:is_true(Value)};
xml_attr_to_conference(Conference, 'exit_sound', Value) ->
    Conference#conference{exit_sound=kz_term:is_true(Value)};
xml_attr_to_conference(Conference, 'enter_sound', Value) ->
    Conference#conference{enter_sound=kz_term:is_true(Value)};
xml_attr_to_conference(Conference, 'run_time', Value) ->
    Conference#conference{start_time=kz_time:now_s() - kz_term:to_integer(Value)};
xml_attr_to_conference(Conference, _Name, _Value) ->
    lager:debug("unhandled conference k/v ~s: ~p", [_Name, _Value]),
    Conference.

-spec xml_members_to_participants(kz_types:xml_els(), conference()) -> participants().
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

-spec xml_member_to_participant(kz_types:xml_els(), participant()) -> participant().
xml_member_to_participant([], Participant) -> Participant;
xml_member_to_participant([#xmlElement{name='uuid'
                                      ,content=UUID
                                      }
                           |XmlElements
                          ], Participant) ->
    CallId = kz_http_util:urldecode(xml_text_to_binary(UUID)),
    lager:debug("uuid ~s callid ~s", [xml_text_to_binary(UUID), CallId]),
    xml_member_to_participant(XmlElements
                             ,Participant#participant{uuid=kz_term:to_binary(CallId)}
                             );

xml_member_to_participant([_|XmlElements], Participant) ->
    xml_member_to_participant(XmlElements, Participant).

-spec xml_text_to_binary(kz_types:xml_els()) -> kz_term:ne_binary().
xml_text_to_binary(XmlElements) ->
    iolist_to_binary([V || #xmlText{value=V} <- XmlElements]).

-spec maybe_destroy_conference(kz_term:ne_binary()) -> boolean().
maybe_destroy_conference(UUID) ->
    case ets:match(?PARTICIPANTS_TBL, #participant{conference_uuid=UUID, _ = '_'}) of
        [] ->
            lager:info("conference ~s was orphaned, removing", [UUID]),
            ets:delete(?CONFERENCES_TBL, UUID);
        _ -> 'false'
    end.

-spec get_conference_dictionary(conferences()) -> dict:dict().
get_conference_dictionary(Conferences) ->
    get_conference_dictionary(Conferences, dict:new()).

-spec get_conference_dictionary(conferences(), dict:dict()) -> dict:dict().
get_conference_dictionary([], Dictionary) -> Dictionary;
get_conference_dictionary([#conference{uuid=UUID}=Conference
                           | Conferences
                          ], Dictionary) ->
    get_conference_dictionary(Conferences, dict:store(UUID, Conference, Dictionary));
get_conference_dictionary([_|Conferences], Dictionary) ->
    get_conference_dictionary(Conferences, Dictionary).

-spec get_participant_dictionary(participants()) -> dict:dict().
get_participant_dictionary(Participants) ->
    get_participant_dictionary(Participants, dict:new()).

-spec get_participant_dictionary(participants(), dict:dict()) -> dict:dict().
get_participant_dictionary([], Dictionary) -> Dictionary;
get_participant_dictionary([#participant{uuid=UUID}=Participant
                            | Participants
                           ], Dictionary) ->
    get_participant_dictionary(Participants, dict:store(UUID, Participant, Dictionary));
get_participant_dictionary([_|Participants], Dictionary) ->
    get_participant_dictionary(Participants, Dictionary).

-spec sync_conferences(dict:dict(), atom()) -> 'ok'.
sync_conferences(Conferences, Node) ->
    MatchSpec = [{#conference{uuid = '$1', node = Node, _ = '_'}, [], ['$1']}],
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

-spec sync_participants(dict:dict(), atom()) -> 'ok'.
sync_participants(Participants, Node) ->
    MatchSpec = [{#participant{uuid = '$1', node = Node, _ = '_'}, [], ['$1']}],
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
    io:format("No conferences found!~n");
print_summary(Match) ->
    io:format("+----------------------------------+----------------------------------------------------+--------------+-------------+----------------------------------+-----+~n"),
    io:format("| Name                             | Node                                               | Participants | Uptime(sec) | Account-ID                       |Local|~n"),
    io:format("+==================================+====================================================+==============+=============+==================================+=====+~n"),
    print_summary(Match, 0).

print_summary('$end_of_table', Count) ->
    io:format("+----------------------------------+----------------------------------------------------+--------------+-------------+----------------------------------+-----+~n"),
    io:format("Found ~p conferences~n", [Count]);
print_summary({[#conference{name=Name
                           ,node=Node
                           ,start_time=StartTime
                           ,account_id=AccountId
                           ,handling_locally=IsLocal
                           }
               ]
              ,Continuation
              }
             ,Count) ->
    Participants = participants(Name),
    io:format("| ~-32s | ~-50s | ~-12B | ~-11B | ~-32s |~-5s|~n"
             ,[Name, Node, length(Participants), kz_time:now_s() - StartTime, AccountId, IsLocal]
             ),
    print_summary(ets:select(Continuation), Count + 1).

print_details('$end_of_table') ->
    io:format("No conferences found!~n");
print_details(Match) ->
    print_details(Match, 0).

print_details('$end_of_table', Count) ->
    io:format("~nFound ~p conferences~n", [Count]);
print_details({[#conference{name=Name}=Conference]
              ,Continuation}
             ,Count) ->
    io:format("~n"),
    _ = [io:format("~-19s: ~s~n", [K, kz_term:to_binary(V)])
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
print_participant_details([#participant{uuid=UUID
                                       ,conference_channel_vars=CCVs
                                       }
                           | Participants
                          ]) ->
    io:format("~n    [~b] ~-52s:", [kz_json:get_integer_value(<<"Member-ID">>, CCVs, 0), UUID]),
    print_participant_flags(kz_json:to_proplist(CCVs)),
    print_participant_details(Participants).

-spec print_participant_flags(kz_term:proplist()) -> 'ok'.
print_participant_flags([]) -> 'ok';
print_participant_flags([{Flag, 'true'}|Props]) ->
    %% Cheating, all this typing today...
    io:format(" ~s", [Flag]),
    print_participant_flags(Props);
print_participant_flags([_|Props]) ->
    print_participant_flags(Props).

-spec conference_from_jobj(kz_json:object(), atom()) -> conference().
conference_from_jobj(JObj, Node) ->
    conference_from_jobj(JObj, Node, #conference{}).

-spec conference_from_jobj(kz_json:object(), atom(), conference()) -> conference().
conference_from_jobj(JObj, Node, Conference) ->
    CtrlNode = kz_conference_event:conference_node(JObj),
    Conference#conference{node=Node
                         ,uuid=kz_conference_event:instance_id(JObj)
                         ,name=kz_conference_event:conference_id(JObj)
                         ,profile_name=kz_conference_event:profile(JObj)
                         ,start_time = kz_time:current_tstamp()
                         ,switch_hostname=kz_conference_event:switch_hostname(JObj)
                         ,switch_url=kz_conference_event:switch_url(JObj)
                         ,account_id = kz_conference_event:account_id(JObj)
                         ,handling_locally = CtrlNode =:= kz_term:to_binary(node())
                         ,origin_node = CtrlNode
                         ,control_node = CtrlNode
                         }.

-spec participant_from_jobj(kz_json:object(), atom()) -> participant().
participant_from_jobj(JObj, Node) ->
    participant_from_jobj(JObj, Node, #participant{}).

-spec participant_from_jobj(kz_json:object(), atom(), participant()) -> participant().
participant_from_jobj(JObj, Node, Participant) ->
    Participant#participant{node=Node
                           ,uuid=kz_conference_event:call_id(JObj)
                           ,conference_uuid=kz_conference_event:instance_id(JObj)
                           ,conference_name=kz_conference_event:conference_id(JObj)
                           ,join_time=kz_conference_event:join_time(JObj, kz_time:current_tstamp())
                           ,caller_id_number=kz_conference_event:caller_id_number(JObj)
                           ,caller_id_name=kz_conference_event:caller_id_name(JObj)
                           ,custom_channel_vars=kz_conference_event:custom_channel_vars(JObj)
                           ,conference_channel_vars=kz_conference_event:conference_channel_vars(JObj)
                           ,custom_application_vars=kz_conference_event:custom_application_vars(JObj)
                           }.
