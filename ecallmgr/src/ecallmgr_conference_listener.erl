%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% Created :  15 Feb 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_conference_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0]).
-export([conferences_on_node/1]).
-export([debug/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2,
         terminate/2, code_change/3]).

-define(RESPONDERS, [{{?MODULE, mwi_update}, [{<<"notification">>, <<"mwi">>}]}
                     ,{{?MODULE, presence_update}, [{<<"notification">>, <<"presence_update">>}]}
                    ]).
-define(BINDINGS, [{notifications, [{restrict_to, [mwi_update, presence_update]}]}]).

-define(QUEUE_NAME, <<"ecallmgr_notify">>).
-define(QUEUE_OPTIONS, [{exclusive, false}]).
-define(CONSUME_OPTIONS, [{exclusive, false}]).

-include("ecallmgr.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE,
                            [{responders, ?RESPONDERS}
                             ,{bindings, ?BINDINGS}
                             ,{queue_name, ?QUEUE_NAME}
                             ,{queue_options, ?QUEUE_OPTIONS}
                             ,{consume_options, ?CONSUME_OPTIONS}
                             ,{basic_qos, 1}
                            ], []).

-spec conferences_on_node/1 :: (atom()) -> wh_json:json_object().
conferences_on_node(Node) ->
    Props = ecallmgr_util:get_interface_properties(Node),
    [_, Hostname] = binary:split(wh_util:to_binary(Node), <<"@">>),
    {ok, Response} = freeswitch:api(Node, 'conference', "xml_list"),
    {Xml, _} = xmerl_scan:string(binary_to_list(binary:replace(Response, <<"\n">>, <<>>, [global]))),
    conferences_xml_to_json(Xml, [{<<"Hostname">>, Hostname}|Props], wh_json:new()).

debug() ->
    conferences_on_node('freeswitch@fs001-dev-vb.2600hz.com').

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
    ?LOG_SYS("starting new ecallmgr conference listner process"),
    {ok, ok}.

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
    ?LOG_SYS("ecallmgr conference listener ~p termination", [_Reason]),
    ok.

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
-spec conferences_xml_to_json/3 :: (list(), proplist(), wh_json:json_object()) -> wh_json:json_object().
conferences_xml_to_json([], _, JObj) ->
    JObj;
conferences_xml_to_json(#xmlElement{name='conferences', content=Content}, Props, JObj) ->
    conferences_xml_to_json(Content, Props, JObj);
conferences_xml_to_json([#xmlElement{name='conferences', content=Content}|_], Props, JObj) ->
    conferences_xml_to_json(Content, Props, JObj);
conferences_xml_to_json([#xmlElement{attributes=Attributes, name='conference', content=Content} | ConferencesXml], Props, JObj) ->
    Conference = wh_json:from_list([{<<"Participants">>, members_xml_to_json(Content, wh_json:new())}
                                    ,{<<"Switch-Hostname">>, props:get_value(<<"Hostname">>, Props)}
                                    ,{<<"Switch-URL">>, props:get_value(<<"URL">>, Props)}
                                    ,{<<"Switch-External-IP">>, props:get_value(<<"Ext-SIP-IP">>, Props)}
                                    |[{K, wh_util:to_binary(V)} 
                                      || #xmlAttribute{name=Name, value=V} <- Attributes
                                             ,(K = props:get_value(Name, ?FS_CONFERNCE_ATTRS)) =/= undefined
                                     ]]),
    case wh_json:get_value(<<"Conference-ID">>, Conference) of
        undefined -> conferences_xml_to_json(ConferencesXml, Props, JObj);
        ConferenceId ->
            conferences_xml_to_json(ConferencesXml, Props, wh_json:set_value(ConferenceId, Conference, JObj))
    end;
conferences_xml_to_json([_|ConferencesXml], Props, JObj) ->
    conferences_xml_to_json(ConferencesXml, Props, JObj).

-spec members_xml_to_json/2 :: (list(), wh_json:json_object()) -> wh_json:json_object().
members_xml_to_json([], JObj) ->
    JObj;
members_xml_to_json(#xmlElement{content=Content, name='members'}, JObj) ->
    members_xml_to_json(Content, JObj);
members_xml_to_json([#xmlElement{content=Content, name='members'}|_], JObj) ->
    members_xml_to_json(Content, JObj);
members_xml_to_json([#xmlElement{content=Content, name='member'}|MembersXml], JObj) ->
    Member = member_xml_to_json(Content, JObj),
    case wh_json:get_value(<<"Participant-ID">>, Member) of
        undefined -> members_xml_to_json(MembersXml, JObj);
        ParticipantId ->
            members_xml_to_json(MembersXml, wh_json:set_value(ParticipantId, Member, JObj))
    end;
members_xml_to_json([_|MembersXml], JObj) ->    
    members_xml_to_json(MembersXml, JObj).

-spec member_xml_to_json/2 :: (list(), wh_json:json_object()) -> wh_json:json_object().
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

-spec member_flags_xml_to_json/2 :: (list(), wh_json:json_object()) -> wh_json:json_object().
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






