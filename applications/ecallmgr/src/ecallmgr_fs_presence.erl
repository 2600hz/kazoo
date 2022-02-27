%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc Receives PRESENCE_IN event
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_presence).
-behaviour(gen_server).

-export([start_link/1, start_link/2]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3

        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-record(state, {node = 'undefined' :: atom()
               ,event :: kz_term:ne_binary()
               ,options = [] :: kz_term:proplist()
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
-spec init([atom() | kz_term:proplist()]) -> {'ok', state()}.
init([Node, Options]) ->
    kz_util:put_callid(Node),
    Event = application:get_env(?APP, 'presence_event', <<"PRESENCE_IN">>),
    gen_server:cast(self(), 'bind_to_record'),
    {'ok', #state{node=Node, event = Event, options=Options}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('bind_to_record', #state{node=Node, event=Event}=State) ->
    case gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, Event)}) =:= 'true'
        andalso gproc:reg({'p', 'l', ?FS_OPTION_MSG(Node)}) =:= 'true'
    of
        'true' -> {'noreply', State};
        'false' -> {'stop', 'gproc_badarg', State}
    end;
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'event', [UUID | Props]}, #state{node=Node
                                             ,options=Options
                                             ,event=Event
                                             }=State) ->
    _ = kz_util:spawn(fun handle_presence_event/5, [Event, UUID, Props, Node, Options]),
    {'noreply', State};
handle_info({'option', K, V}, #state{options=Options}=State) ->
    {'noreply', State#state{options=props:set_value(K, V, Options)}};
handle_info(_Other, State) ->
    lager:debug("unhandled msg: ~p", [_Other]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{node=Node}) ->
    lager:info("presence listener for ~s terminating: ~p", [Node, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init_props(kzd_freeswitch:data(), kz_term:proplist()) -> kz_term:proplist().
init_props(FSProps, Options) ->
    case props:get_is_true(<<"Publish-Channel-State">>, FSProps) of
        'undefined' ->
            case props:is_false(<<"Publish-Channel-State">>, Options, 'false') of
                'true' -> props:set_value(<<"Publish-Channel-State">>, 'false', FSProps);
                _ -> FSProps
            end;
        _Value -> FSProps
    end.

-spec handle_presence_event(kz_term:ne_binary(), kz_term:api_binary(), kzd_freeswitch:data(), atom(), kz_term:proplist()) -> any().
handle_presence_event(BindingEvent, UUID, FSProps, Node, Options) ->
    kz_util:put_callid(UUID),
    InitProps = init_props(FSProps, Options),
    EventName = props:get_value(<<"Event-Subclass">>, InitProps, kzd_freeswitch:event_name(InitProps)),
    process_specific_event(BindingEvent, EventName, UUID, InitProps, Node).

-spec process_specific_event(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary(), kzd_freeswitch:data(), atom()) -> any().
process_specific_event(Event, Event, UUID, FSProps, Node) ->
    maybe_build_presence_event(Node, UUID, FSProps).

-spec maybe_build_presence_event(atom(), kz_term:api_binary(), kzd_freeswitch:data()) -> any().
maybe_build_presence_event(Node, UUID, FSProps) ->
    Routines = [fun check_proto/3
               ,fun check_publish_state/3
               ],
    case lists:all(fun(F) -> F(Node, UUID, FSProps) end, Routines) of
        'false' -> 'ok';
        'true' ->
            case kapps_config:get_boolean(?APP_NAME, <<"restrict_presence_event_publisher">>, 'false') of
                'false' -> build_presence_event(Node, UUID, FSProps);
                'true' -> maybe_build_restricted(Node, UUID, FSProps)
            end
    end.

-spec maybe_build_restricted(atom(), kz_term:api_binary(), kzd_freeswitch:data()) -> 'ok'.
maybe_build_restricted(Node, UUID, FSProps) ->
    EcallmgrNode = kz_term:to_binary(node()),
    State = presence_status(FSProps),
    {FromUser, Realm} = get_user_realm(FSProps),
    ToUser =  to_user(FSProps),

    case props:get_value(?GET_CCV(<<"Ecallmgr-Node">>), FSProps) of
        'undefined' ->
            lager:debug("presence ~s to ~s/~s in realm ~s not assigned an ecallmgr, not sending", [State, FromUser, ToUser, Realm]);
        EcallmgrNode -> build_presence_event(Node, UUID, FSProps);
        _EventEcallmgr ->
            lager:debug("presence ~s to ~s/~s in realm ~s handled by another ecallmgr(~s), not sending", [State, FromUser, ToUser, Realm, _EventEcallmgr])
    end.

-spec check_proto(atom(), kz_term:api_binary(), kzd_freeswitch:data()) -> boolean().
check_proto(_Node, _UUID, FSProps) ->
    Proto = props:get_value(<<"proto">>, FSProps),
    check_proto(Proto).

-spec check_proto(kz_term:api_binary()) -> boolean().
check_proto(<<"any">>) -> 'true';
check_proto(_Proto) ->
    lager:debug("presence proto ~p not handled", [_Proto]),
    'false'.

-spec check_publish_state(atom(), kz_term:api_binary(), kzd_freeswitch:data()) -> boolean().
check_publish_state(_Node, _UUID, FSProps) ->
    props:is_true(<<"Force-Publish-Event-State">>, FSProps, 'false')
        orelse props:is_true(<<"Publish-Channel-State">>, FSProps, 'true').

-spec realm(kzd_freeswitch:data()) -> kz_term:ne_binary().
realm(FSProps) ->
    props:get_first_defined([?GET_CCV(<<"Realm">>)
                            ,<<"variable_sip_invite_domain">>
                            ,<<"variable_sip_auth_realm">>
                            ,<<"variable_sip_to_host">>
                            ,<<"variable_domain_name">>
                            ]
                           ,FSProps
                           ,?DEFAULT_REALM
                           ).

-spec get_user_realm(kzd_freeswitch:data()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
get_user_realm(FSProps) ->
    case binary:split(from(FSProps), <<"@">>, ['global']) of
        [Username, Realm | _] -> {Username, Realm};
        [From] -> {From, realm(FSProps)}
    end.

-spec from(kzd_freeswitch:data()) -> kz_term:ne_binary().
from(FSProps) ->
    props:get_first_defined([<<"from">>
                            ,<<"variable_presence_id">>
                            ,<<"Channel-Presence-ID">>
                            ]
                           ,FSProps
                           ).

-spec presence_id(kzd_freeswitch:data(), kz_term:ne_binary()) -> kz_term:ne_binary().
presence_id(FSProps, Realm) ->
    [ID | _] = binary:split(kzd_freeswitch:presence_id(FSProps), <<"@">>),
    <<ID/binary, "@", Realm/binary>>.

-spec to_user(kzd_freeswitch:data()) -> kz_term:ne_binary().
to_user(FSProps) ->
    to_user(direction(FSProps), FSProps).

to_user(<<"initiator">>, FSProps) ->
    props:get_first_defined([<<"Caller-Destination-Number">>
                            ,<<"variable_sip_to_user">>
                            ]
                           ,FSProps
                           ,<<"unknown">>
                           );
to_user(<<"recipient">>, FSProps) ->
    props:get_first_defined([<<"Caller-Caller-ID-Number">>
                            ,<<"variable_sip_from_user">>
                            ]
                           ,FSProps
                           ,<<"unknown">>
                           ).

-spec expires(kz_term:ne_binary()) -> integer().
expires(<<"early">>) -> 0;
expires(<<"confirmed">>) -> 0;
expires(<<"terminated">>) -> 20.

-spec build_presence_event(atom(), kz_term:api_binary(), kzd_freeswitch:data()) -> any().
build_presence_event(_Node, UUID, FSProps) ->
    ToTag = kzd_freeswitch:to_tag(FSProps),
    FromTag = kzd_freeswitch:from_tag(FSProps),

    {FromUser, Realm} = get_user_realm(FSProps),
    PresenceId = presence_id(FSProps, Realm),

    PresenceURI =  <<"sip:", PresenceId/binary>>,

    ToUser =  to_user(FSProps),
    To =  <<ToUser/binary, "@", Realm/binary>>,
    ToURI =  <<"sip:", To/binary>>,

    State = presence_status(FSProps),
    Expires = expires(State),
    SwitchURI = kzd_freeswitch:switch_uri(FSProps),

    Payload = props:filter_undefined(
                [{<<"Presence-ID">>, PresenceId}

                ,{<<"From">>, PresenceURI}
                ,{<<"From-User">>, FromUser}
                ,{<<"From-Realm">>, Realm}
                ,{<<"From-Tag">>, FromTag}

                ,{<<"To">>, ToURI}
                ,{<<"To-User">>, ToUser}
                ,{<<"To-Realm">>, Realm}
                ,{<<"To-Tag">>, ToTag}
                ,{<<"To-URI">>, PresenceURI}

                ,{<<"Direction">>, direction(FSProps)}
                ,{<<"State">>, State}
                ,{<<"Call-ID">>, UUID}
                ,{<<"Switch-URI">>, SwitchURI}
                ,{<<"Expires">>, Expires}
                ,{<<"Event-Package">>, <<"dialog">>}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    lager:debug("sending presence ~s to ~s/~s in realm ~s", [State, FromUser, ToUser, Realm]),
    _ = maybe_delay(State),
    kz_amqp_worker:cast(Payload, fun kapi_presence:publish_dialog/1).

maybe_delay(<<"terminated">>) ->
    timer:sleep(?MILLISECONDS_IN_SECOND);
maybe_delay(_) -> 'ok'.

-spec direction(kzd_freeswitch:data()) -> kz_term:ne_binary().
direction(FSProps) ->
    case kzd_freeswitch:presence_direction(FSProps) of
        <<"inbound">> -> <<"initiator">>;
        <<"outbound">> -> <<"recipient">>
    end.

-spec status(kzd_freeswitch:data()) -> kz_term:ne_binary().
status(FSProps) ->
    kz_term:to_lower_binary(props:get_binary_value(<<"status">>, FSProps)).

-spec answer_state(kzd_freeswitch:data()) -> kz_term:ne_binary().
answer_state(FSProps) ->
    kz_term:to_lower_binary(props:get_value(<<"Answer-State">>, FSProps)).

-spec presence_status(kzd_freeswitch:data()) -> kz_term:ne_binary().
presence_status(FSProps) ->
    Status = status(FSProps),
    AnswerState = answer_state(FSProps),
    Direction = direction(FSProps),
    presence_status(Direction, Status, AnswerState).

-spec presence_status(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
presence_status(_, _, <<"answered">>) -> <<"confirmed">>;
presence_status(_, _, <<"hangup">>) -> <<"terminated">>;
presence_status(_, <<"hangup">>, _) -> <<"terminated">>;
presence_status(Direction, <<"cs_", Status/binary>>, AnswerState) -> presence_status(Direction, Status, AnswerState);
presence_status(<<"initiator">>, _, <<"ringing">>) -> <<"confirmed">>;
presence_status(<<"recipient">>, _, <<"ringing">>) -> <<"early">>;
presence_status(_ , _, AnswerState) -> AnswerState.
