%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Execute conference commands
%%% @author Karl Anderson <karl@2600hz.org>
%%% @author Roman Galeev
%%% @end
%%%-----------------------------------------------------------------------------
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
               ,options = [] :: kz_term:proplist()
               ,events = [] :: kz_term:ne_binaries()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------

-spec start_link(atom()) -> kz_types:startlink_ret().
start_link(Node) -> start_link(Node, []).

-spec start_link(atom(), kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Node, Options) ->
    gen_server:start_link(?SERVER, [Node, Options], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([node() | kz_term:proplist()]) -> {'ok', state()}.
init([Node, Options]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    lager:info("starting new fs conference event listener for ~s", [Node]),
    gen_server:cast(self(), 'bind_to_events'),
    ecallmgr_fs_conferences:sync_node(Node),
    Events = kapps_config:get_ne_binaries(?APP_NAME, <<"publish_conference_event">>, ?CONFERENCE_EVENTS),
    {'ok', #state{node=Node
                 ,options=Options
                 ,events=Events
                 }
    }.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('bind_to_events', #state{node=Node}=State) ->
    case gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"conference::maintenance">>)}) =:= 'true'
        andalso gproc:reg({'p', 'l', ?FS_OPTION_MSG(Node)}) =:= 'true'
    of
        'true' -> {'noreply', State};
        'false' -> {'stop', 'gproc_badarg', State}
    end;
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
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
    lager:debug("ecallmgr conference node terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init_props(kzd_freeswitch:data(), kz_term:proplist()) -> kzd_freeswitch:data().
init_props(Props, Options) ->
    case props:get_is_true(<<"Publish-Channel-State">>, Props) of
        'undefined' ->
            case props:is_false(<<"Publish-Channel-State">>, Options, 'false') of
                'true' -> props:set_value(<<"Publish-Channel-State">>, 'false', Props);
                _ -> Props
            end;
        _Value -> Props
    end.

-spec handle_conference_event(atom(), kz_term:ne_binaries(), kzd_freeswitch:data(), kz_term:proplist()) -> 'ok'.
handle_conference_event(Node, Events, [_UUID | FSProps], Options) ->
    kz_util:put_callid(_UUID),

    Props = init_props(FSProps, Options),
    Action = props:get_value(<<"Action">>, Props),
    _ = process_event(Action, Props, Node),
    maybe_publish_event(Action, Props, Node, Events).

-spec process_event(kz_term:ne_binary(), kzd_freeswitch:data(), atom()) -> any().
process_event(<<"conference-create">>, Props, Node) ->
    _ = ecallmgr_fs_conferences:create(Props, Node),
    ConferenceId = kzd_freeswitch:conference_name(Props),
    UUID = kzd_freeswitch:conference_uuid(Props),
    ecallmgr_conference_sup:start_conference_control(Node, ConferenceId, UUID);
process_event(<<"conference-destroy">>= Action, Props, Node) ->
    UUID = kzd_freeswitch:conference_uuid(Props),
    case ecallmgr_fs_conferences:conference(UUID) of
        {'ok', #conference{name=ConferenceId}=Conference} ->
            publish_event(Action, Conference, Props, Node),
            ecallmgr_fs_conferences:destroy(UUID),
            ecallmgr_conference_sup:stop_conference_control(Node, ConferenceId, UUID);
        {'error', 'not_found'} ->
            lager:debug("received conference destroy for nonexistent conference ~s", [UUID])
    end;

process_event(<<"add-member">>, Props, Node) ->
    ecallmgr_fs_conferences:participant_create(Props, Node);
process_event(<<"del-member">>, Props, _Node) ->
    ecallmgr_fs_conferences:participant_destroy(kzd_freeswitch:call_id(Props));
process_event(<<"lock">>, Props, _) ->
    UUID = kzd_freeswitch:conference_uuid(Props),
    ecallmgr_fs_conferences:update(UUID, {#conference.locked, 'true'});
process_event(<<"unlock">>, Props, _) ->
    UUID = kzd_freeswitch:conference_uuid(Props),
    ecallmgr_fs_conferences:update(UUID, {#conference.locked, 'false'});
process_event(Action, Props, _Node) ->
    case lists:member(Action, ?MEMBER_UPDATE_EVENTS) of
        'true' -> update_participant(Props);
        'false' -> 'ok'
    end.

update_participant(Props) ->
    ConferenceVars = ecallmgr_util:conference_channel_vars(Props),
    ChanVars = ecallmgr_util:custom_channel_vars(Props),
    AppVars = ecallmgr_util:custom_application_vars(Props),

    UUID = kzd_freeswitch:call_id(Props),
    Update = [{#participant.conference_channel_vars, ConferenceVars}
             ,{#participant.custom_channel_vars, ChanVars}
             ,{#participant.custom_application_vars, AppVars}
             ],
    ecallmgr_fs_conferences:participant_update(UUID, Update).

maybe_publish_event(<<"conference-destroy">>, _, _, _) -> 'ok';
maybe_publish_event(Action, Props, Node, EventsToPublish) ->
    case lists:member(Action, EventsToPublish) of
        'true' -> publish_event(Action, Props, Node);
        'false' -> lager:debug("not publishing conference event : ~s", [Action])
    end.

publish_event(Action, Props, Node) ->
    UUID = kzd_freeswitch:conference_uuid(Props),
    case ecallmgr_fs_conferences:conference(UUID) of
        {'ok', #conference{}=Conference} -> publish_event(Action, Conference, Props, Node);
        {'error', 'not_found'} -> lager:debug("not publishing conference event ~s for nonexistent ~s ", [Action, UUID])
    end.

publish_event(Action, #conference{handling_locally=IsLocal} = Conference, Props, _Node) ->
    case props:is_true(<<"Force-Publish-Event-State">>, Props, 'false')
        orelse (props:is_true(<<"Publish-Channel-State">>, Props, 'true')
                andalso IsLocal
               )
    of
        'true' -> publish_event(conference_event(Action, Conference, Props));
        'false' -> lager:debug("conference control on another node, not publishing event ~s", [Action])
    end.

-spec publish_event(kz_term:proplist()) -> 'ok'.
publish_event(Event) ->
    kz_amqp_worker:cast(Event, fun kapi_conference:publish_event/1).

conference_event(Action, Conference, Props) ->
    CCVs = ecallmgr_util:custom_channel_vars(Props),
    CAVs = ecallmgr_util:custom_application_vars(Props),
    ConfVars = ecallmgr_util:conference_channel_vars(Props),

    lager:debug("publishing conference event action ~s", [Action]),

    props:filter_undefined(
      [{<<"Account-ID">>, Conference#conference.account_id}
      ,{<<"Call-ID">>, kzd_freeswitch:call_id(Props)}
      ,{<<"Caller-ID-Name">>, kzd_freeswitch:caller_id_name(Props)}
      ,{<<"Caller-ID-Number">>, kzd_freeswitch:caller_id_number(Props)}
      ,{<<"Channel-Presence-ID">>, kzd_freeswitch:presence_id(Props)}
      ,{<<"Conference-Channel-Vars">>, kz_json:from_list(ConfVars)}
      ,{<<"Conference-ID">>, kzd_freeswitch:conference_name(Props)}
      ,{<<"Custom-Application-Vars">>, kz_json:from_list(CAVs)}
      ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
      ,{<<"Event">>, Action}
      ,{<<"Instance-ID">>, kzd_freeswitch:conference_uuid(Props)}
      ,{<<"Participant-ID">>, props:get_value(<<"Member-ID">>, ConfVars)}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).
