%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017 2600Hz INC
%%% @doc
%%% Execute conference commands
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_conference).
-behaviour(gen_server).

%% API
-export([start_link/1
        ,start_link/2
        ]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(MEMBER_UPDATE_EVENTS, [<<"stop-talking">>
                              ,<<"start-talking">>
                              ,<<"mute-member">>
                              ,<<"unmute-member">>
                              ,<<"deaf-member">>
                              ,<<"undeaf-member">>
                              ]).

-define(CONFERENCE_EVENTS, [<<"conference-create">>
                           ,<<"conference-destroy">>
                           ,<<"lock">>
                           ,<<"unlock">>
                           ,<<"add-member">>
                           ,<<"del-member">>
                                | ?MEMBER_UPDATE_EVENTS
                           ]).

-record(state, {node = 'undefined' :: atom()
               ,options = [] :: kz_proplist()
               ,events = [] :: ne_binaries()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(atom()) -> startlink_ret().
-spec start_link(atom(), kz_proplist()) -> startlink_ret().
start_link(Node) -> start_link(Node, []).
start_link(Node, Options) ->
    gen_server:start_link(?SERVER, [Node, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init([node() | kz_proplist()]) -> {'ok', state()}.
init([Node, Options]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(?LOG_SYSTEM_ID),
    lager:info("starting new fs conference event listener for ~s", [Node]),
    gen_server:cast(self(), 'bind_to_events'),
    ecallmgr_fs_conferences:sync_node(Node),
    Events = ecallmgr_config:get_ne_binaries(<<"publish_conference_event">>, ?CONFERENCE_EVENTS),
    {'ok', #state{node=Node
                 ,options=Options
                 ,events=Events
                 }
    }.

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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast('bind_to_events', #state{node=Node}=State) ->
    case gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"conference::maintenance">>)}) =:= 'true'
        andalso gproc:reg({'p', 'l', ?FS_OPTION_MSG(Node)}) =:= 'true'
    of
        'true' -> {'noreply', State};
        'false' -> {'stop', 'gproc_badarg', State}
    end;
handle_cast(_Msg, State) ->
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'event', Props}, #state{node=Node
                                    ,events=Events
                                    ,options=Options
                                    }=State) ->
    kz_util:spawn(fun handle_conference_event/4, [Node, Events, Props, Options]),
    {'noreply', State};
handle_info({'option', K, V}, #state{options=Options}=State) ->
    {'noreply', State#state{options=props:set_value(K, V, Options)}};
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {stop, {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("ecallmgr conference node terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec init_props(kzd_freeswitch:data(), kz_proplist()) -> kzd_freeswitch:data().
init_props(Props, Options) ->
    case props:get_is_true(<<"Publish-Channel-State">>, Props) of
        'undefined' ->
            case props:is_false(<<"Publish-Channel-State">>, Options, 'false') of
                'true' -> props:set_value(<<"Publish-Channel-State">>, 'false', Props);
                _ -> Props
            end;
        _Value -> Props
    end.

-spec handle_conference_event(atom(), ne_binaries(), kzd_freeswitch:data(), kz_proplist()) -> 'ok'.
handle_conference_event(Node, Events, [_UUID | FSProps], Options) ->
    Props = init_props(FSProps, Options),
    Action = props:get_value(<<"Action">>, Props),
    process_event(Action, Props, Node),
    maybe_publish_event(Action, Props, Node, Events).

-spec process_event(ne_binary(), kzd_freeswitch:data(), atom()) -> any().
process_event(<<"conference-create">>, Props, Node) ->
    ecallmgr_fs_conferences:create(Props, Node),
    ConferenceId = props:get_value(<<"Conference-Name">>, Props),
    UUID = props:get_value(<<"Conference-Unique-ID">>, Props),
    ecallmgr_conference_sup:start_conference_control(Node, ConferenceId, UUID);
process_event(<<"conference-destroy">>= Action, Props, Node) ->
    UUID = props:get_value(<<"Conference-Unique-ID">>, Props),
    case ecallmgr_fs_conferences:conference(UUID) of
        {'ok', #conference{name=ConferenceId}=Conference} ->
            publish_event(Action, Conference, Props, Node),
            ecallmgr_fs_conferences:destroy(UUID),
            ecallmgr_conference_sup:stop_conference_control(Node, ConferenceId, UUID);
        {'error', 'not_found'} ->
            lager:debug("received conference destroy for inexistant conference ~s", [UUID])
    end;

process_event(<<"add-member">>, Props, Node) ->
    ecallmgr_fs_conferences:participant_create(Props, Node);
process_event(<<"del-member">>, Props, _Node) ->
    ecallmgr_fs_conferences:participant_destroy(kzd_freeswitch:call_id(Props));
process_event(<<"lock">>, Props, _) ->
    UUID = props:get_value(<<"Conference-Unique-ID">>, Props),
    ecallmgr_fs_conferences:update(UUID, {#conference.locked, 'true'});
process_event(<<"unlock">>, Props, _) ->
    UUID = props:get_value(<<"Conference-Unique-ID">>, Props),
    ecallmgr_fs_conferences:update(UUID, {#conference.locked, 'false'});
process_event(Action, Props, _Node) ->
    case lists:member(Action, ?MEMBER_UPDATE_EVENTS) of
        'true' -> update_participant(Props);
        'false' -> 'ok'
    end.

update_participant(Props) ->
    ConferenceVars = ecallmgr_util:conference_channel_vars(Props),
    CustomVars = ecallmgr_util:custom_channel_vars(Props),
    UUID = kzd_freeswitch:call_id(Props),
    Update = [{#participant.conference_channel_vars, ConferenceVars}
             ,{#participant.custom_channel_vars, CustomVars}
             ],
    ecallmgr_fs_conferences:participant_update(UUID, Update).

maybe_publish_event(<<"conference-destroy">>, _, _, _) -> 'ok';
maybe_publish_event(Action, Props, Node, EventsToPublish) ->
    case lists:member(Action, EventsToPublish) of
        'true' -> publish_event(Action, Props, Node);
        'false' -> lager:debug("not publishing conference event : ~s", [Action])
    end.

publish_event(Action, Props, Node) ->
    UUID = props:get_value(<<"Conference-Unique-ID">>, Props),
    case ecallmgr_fs_conferences:conference(UUID) of
        {'ok', #conference{}=Conference} -> publish_event(Action, Conference, Props, Node);
        {'error', 'not_found'} -> lager:debug("not publishing conference event ~s for not existant ~s ", [Action, UUID])
    end.

publish_event(Action, #conference{handling_locally=IsLocal} = Conference, Props, _Node) ->
    case props:is_true(<<"Force-Publish-Event-State">>, Props, 'false')
        orelse (props:is_true(<<"Publish-Channel-State">>, Props, 'true')
                andalso IsLocal)
    of
        'true' -> publish_event(conference_event(Action, Conference, Props));
        'false' -> lager:debug("conference control on another node, not publishing event ~s", [Action])
    end.

-spec publish_event(kz_proplist()) -> 'ok'.
publish_event(Event) ->
    kz_amqp_worker:cast(Event, fun kapi_conference:publish_event/1).

conference_event(Action, Conference, Props) ->
    CCVs = ecallmgr_util:custom_channel_vars(Props),
    ConfVars = ecallmgr_util:conference_channel_vars(Props),
    props:filter_undefined(
      [{<<"Event">>, Action}
      ,{<<"Account-ID">>, Conference#conference.account_id}
      ,{<<"Conference-ID">>, props:get_value(<<"Conference-Name">>, Props)}
      ,{<<"Instance-ID">>, props:get_value(<<"Conference-Unique-ID">>, Props)}
      ,{<<"Call-ID">>, kzd_freeswitch:call_id(Props)}
      ,{<<"Participant-ID">>, props:get_value(<<"Member-ID">>, ConfVars)}
      ,{<<"Caller-ID-Name">>, props:get_value(<<"Caller-Caller-ID-Name">>, Props)}
      ,{<<"Caller-ID-Number">>, props:get_value(<<"Caller-Caller-ID-Number">>, Props)}
      ,{<<"Channel-Presence-ID">>, props:get_value(<<"Channel-Presence-ID">>, Props)}
      ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
      ,{<<"Conference-Channel-Vars">>, kz_json:from_list(ConfVars)}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).
