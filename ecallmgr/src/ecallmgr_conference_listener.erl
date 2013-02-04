%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_conference_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0
         ,conferences_on_node/1
         ,handle_command/2
         ,handle_search_req/2
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-define(RESPONDERS, [{{?MODULE, handle_command}
                      ,[{<<"conference">>, <<"command">>}]
                     }
                     ,{{?MODULE, handle_search_req}
                       ,[{<<"conference">>, <<"search_req">>}]
                      }
                    ]).
-define(BINDINGS, [{conference, [{restrict_to, [command, discovery]}]}]).
-define(QUEUE_NAME, <<"ecallmgr_conference_listener">>).
-define(QUEUE_OPTIONS, [{exclusive, false}]).
-define(CONSUME_OPTIONS, [{exclusive, false}]).

-include("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE,
                            [{responders, ?RESPONDERS}
                             ,{bindings, ?BINDINGS}
                             ,{queue_name, ?QUEUE_NAME}
                             ,{queue_options, ?QUEUE_OPTIONS}
                             ,{consume_options, ?CONSUME_OPTIONS}
                            ], []).

-spec conferences_on_node(atom()) -> wh_json:object().
conferences_on_node(Node) ->
    lager:debug("looking for conferences on node ~s", [Node]),
    [_, Hostname] = binary:split(wh_util:to_binary(Node), <<"@">>),
    lager:debug("requesting xml list of conferences on ~s", [Hostname]),
    {ok, Response} = freeswitch:api(Node, 'conference', "xml_list"),

    %% send search error back

    {Xml, _} = xmerl_scan:string(binary_to_list(binary:replace(Response, <<"\n">>, <<>>, [global]))),
    lager:debug("got xml list of conferences: ~s", [Response]),
    Props = ecallmgr_util:get_interface_properties(Node),
    conferences_xml_to_json(Xml, [{<<"Hostname">>, Hostname}|Props], wh_json:new()).

-spec handle_command(wh_json:object(), wh_proplist()) -> 'ok'.
handle_command(JObj, _Props) ->
    ConferenceId = wh_json:get_value(<<"Conference-ID">>, JObj),
    Focus = get_conference_focus(ConferenceId),
    ecallmgr_conference_command:exec(Focus, ConferenceId, JObj),
    ok.

-spec handle_search_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_search_req(JObj, _Props) ->
    ConferenceId = wh_json:get_value(<<"Conference-ID">>, JObj),
    lager:debug("received search request for conference id ~s", [ConferenceId]),
    case search_nodes_for_conference(ConferenceId) of
        'undefined' ->
            lager:debug("sending error search response, conference not found"),
            Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                     ,{<<"Error-Message">>, <<"Conference ", ConferenceId/binary, " not found">>}
                     ,{<<"Request">>, JObj}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            wapi_conference:publish_error(wh_json:get_value(<<"Server-ID">>, JObj), Error);
        Conference ->
            lager:debug("sending affirmative search response, conference found"),
            Resp = wh_json:set_values([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                                       |wh_api:default_headers(?APP_NAME, ?APP_VERSION)
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
%% @end
%%--------------------------------------------------------------------
init([]) ->
    put('callid', ?LOG_SYSTEM_ID),
    lager:debug("starting new ecallmgr conference listner process"),
    {'ok', 'ok'}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.

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
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec search_nodes_for_conference(ne_binary()) ->
                                         api_object().
-spec search_nodes_for_conference([atom(),...], ne_binary()) ->
                                         api_object().
search_nodes_for_conference(ConferenceId) ->
    search_nodes_for_conference(ecallmgr_fs_nodes:connected(), ConferenceId).

search_nodes_for_conference([], _) ->
    lager:debug("failed to find conference on any of the freeswitch nodes"),
    'undefined';
search_nodes_for_conference([Node|Nodes], ConferenceId) ->
    try conferences_on_node(Node) of
        JObj ->
            case wh_json:get_value(ConferenceId, JObj) of
                'undefined' ->
                    lager:debug("conference ~s not running on node ~s", [ConferenceId, Node]),
                    search_nodes_for_conference(Nodes, ConferenceId);
                Conference ->
                    lager:debug("found conference ~s on node ~s", [ConferenceId, Node]),
                    Conference
            end
    catch
        _E:_R ->
            lager:debug("unable to get conferences on ~p: ~p", [Node, _R]),
            search_nodes_for_conference(Nodes, ConferenceId)
    end.

%% TODO: Flush cache on conference end
-spec get_conference_focus(ne_binary()) -> atom().
get_conference_focus(ConferenceId) ->
    case search_for_conference_focus(ecallmgr_fs_nodes:connected(), ConferenceId) of
        'undefined' ->
            lager:debug("failed to find conference focus for conference ~s", [ConferenceId]),
            'undefined';
        Focus ->
            lager:debug("found conference focus ~s for conference ~s, storing in cache", [Focus, ConferenceId]),
            Focus
    end.

-spec search_for_conference_focus([atom(),...], ne_binary()) -> atom().
search_for_conference_focus([], _) -> 'undefined';
search_for_conference_focus([Node|Nodes], ConferenceId) ->
    try conferences_on_node(Node) of
        JObj ->
            case wh_json:get_value(ConferenceId, JObj) of
                'undefined' -> search_for_conference_focus(Nodes, ConferenceId);
                _Else -> Node
            end
    catch
        _E:_R ->
            lager:debug("unable to get conferences on ~p: ~p", [Node, _R]),
            search_for_conference_focus(Nodes, ConferenceId)
    end.

-spec conferences_xml_to_json(list(), wh_proplist(), wh_json:object()) -> wh_json:object().
conferences_xml_to_json([], _, JObj) -> JObj;
conferences_xml_to_json(#xmlElement{name='conferences'
                                    ,content=Content
                                   }
                        ,Props, JObj) ->
    conferences_xml_to_json(Content, Props, JObj);
conferences_xml_to_json([#xmlElement{name='conferences'
                                     ,content=Content
                                    }|_]
                        ,Props, JObj) ->
    conferences_xml_to_json(Content, Props, JObj);
conferences_xml_to_json([#xmlElement{attributes=Attributes
                                     ,name='conference'
                                     ,content=Content
                                    } | ConferencesXml
                        ]
                        ,Props, JObj) ->
    Conference = wh_json:from_list(
                   [{<<"Participants">>, members_xml_to_json(Content, wh_json:new())}
                    ,{<<"Switch-Hostname">>, props:get_value(<<"Hostname">>, Props)}
                    ,{<<"Switch-URL">>, props:get_value(<<"URL">>, Props)}
                    ,{<<"Switch-External-IP">>, props:get_value(<<"Ext-SIP-IP">>, Props)}
                    |[{K, wh_util:to_binary(V)}
                      || #xmlAttribute{name=Name, value=V} <- Attributes,
                         (K = props:get_value(Name, ?FS_CONFERNCE_ATTRS)) =/= 'undefined'
                     ]]),
    case wh_json:get_value(<<"Conference-ID">>, Conference) of
        'undefined' -> conferences_xml_to_json(ConferencesXml, Props, JObj);
        ConferenceId ->
            lager:debug("populating conference ~s in conference props", [ConferenceId]),
            conferences_xml_to_json(ConferencesXml, Props, wh_json:set_value(ConferenceId, Conference, JObj))
    end;
conferences_xml_to_json([_|ConferencesXml], Props, JObj) ->
    conferences_xml_to_json(ConferencesXml, Props, JObj).

-spec members_xml_to_json(list(), wh_json:object()) -> wh_json:object().
members_xml_to_json([], JObj) -> JObj;
members_xml_to_json(#xmlElement{content=Content, name='members'}, JObj) ->
    members_xml_to_json(Content, JObj);
members_xml_to_json([#xmlElement{content=Content, name='members'}|_], JObj) ->
    members_xml_to_json(Content, JObj);
members_xml_to_json([#xmlElement{content=Content, name='member'}|MembersXml], JObj) ->
    Member = member_xml_to_json(Content, wh_json:new()),
    case wh_json:get_value(<<"Participant-ID">>, Member) of
        'undefined' -> members_xml_to_json(MembersXml, JObj);
        ParticipantId ->
            lager:debug("populating conference participant ~s in conference props", [ParticipantId]),
            members_xml_to_json(MembersXml, wh_json:set_value(ParticipantId, Member, JObj))
    end;
members_xml_to_json([_|MembersXml], JObj) ->
    members_xml_to_json(MembersXml, JObj).

-spec member_xml_to_json(list(), wh_json:object()) -> wh_json:object().
member_xml_to_json([], JObj) -> JObj;
member_xml_to_json([#xmlElement{name='flags', content=Content}|Xml], JObj) ->
    Flags = member_flags_xml_to_json(Content, wh_json:new()),
    member_xml_to_json(Xml, wh_json:set_value(<<"Flags">>, Flags, JObj));
member_xml_to_json([#xmlElement{name=Name, content=Content}|Xml], JObj) ->
    case {props:get_value(Name, ?FS_CONFERENCE_PARTICIPANT), Content} of
        {_, [#xmlText{value=Value}]} when Name =:= 'uuid' ->
            V = wh_util:to_binary(http_uri:decode(Value)),
            member_xml_to_json(Xml, wh_json:set_value(<<"Call-ID">>, V, JObj));
        {Key, [#xmlText{value=Value}]} when is_binary(Key) ->
            member_xml_to_json(Xml, wh_json:set_value(Key, wh_util:to_binary(Value), JObj));
        _Else ->
            member_xml_to_json(Xml, JObj)
    end;
member_xml_to_json([_Else|Xml], JObj) ->
    member_xml_to_json(Xml, JObj).

-spec member_flags_xml_to_json(list(), wh_json:object()) -> wh_json:object().
member_flags_xml_to_json([], JObj) -> JObj;
member_flags_xml_to_json([#xmlElement{name=Name, content=Content}|Xml], JObj) ->
    case {props:get_value(Name, ?FS_CONFERENCE_FLAGS), Content} of
        {Key, [#xmlText{value=Value}]} when is_binary(Key) ->
            member_flags_xml_to_json(Xml, wh_json:set_value(Key, wh_util:to_binary(Value), JObj));
        _Else ->
            member_flags_xml_to_json(Xml, JObj)
    end;
member_flags_xml_to_json([_|Xml], JObj) ->
    member_flags_xml_to_json(Xml, JObj).
