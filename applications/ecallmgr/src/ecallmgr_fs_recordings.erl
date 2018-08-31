%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Receives START/STOP RECORD event
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_recordings).
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
-spec init(list()) -> {'ok', state()}.
init([Node, Options]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(Node),
    gen_server:cast(self(), 'bind_to_record'),
    {'ok', #state{node=Node, options=Options}}.

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
handle_cast('bind_to_record', #state{node=Node}=State) ->
    case gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"RECORD_START">>)}) =:= 'true'
        andalso gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"RECORD_STOP">>)}) =:= 'true'
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
handle_info({'event', Props}, #state{node=Node
                                    ,options=Options
                                    }=State) ->
    _ = kz_util:spawn(fun handle_record_event/3, [Node, Props, Options]),
    {'noreply', State};
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {stop, {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node=Node}) ->
    lager:info("recording handler for ~s terminating: ~p", [Node, _Reason]).

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
-spec init_props(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
init_props(Props, Options) ->
    case props:get_is_true(<<"Publish-Channel-State">>, Props) of
        'undefined' ->
            case props:is_false(<<"Publish-Channel-State">>, Options, 'false') of
                'true' -> props:set_value(<<"Publish-Channel-State">>, 'false', Props);
                _ -> Props
            end;
        _Value -> Props
    end.

-spec handle_record_event(atom(), kz_term:proplist(), kz_term:proplist()) -> 'ok'.
handle_record_event(Node, [UUID | FSProps], Options) ->
    kz_util:put_callid(UUID),
    Props = init_props(FSProps, Options),
    _ = process_event(UUID, Props, Node),
    maybe_publish_record_event(Props).

-spec process_event(kz_term:api_binary(), kz_term:proplist(), atom()) -> any().
process_event(UUID, Props, Node) ->
    kz_util:put_callid(UUID),
    EventName = kzd_freeswitch:event_name(Props),
    process_specific_event(EventName, UUID, Props, Node).

-spec process_specific_event(kz_term:ne_binary(), kz_term:api_binary(), kz_term:proplist(), atom()) -> any().
process_specific_event(<<"RECORD_STOP">>, UUID, Props, Node) ->
    IsLocal = props:is_true(<<"Force-Publish-Event-State">>, Props, 'true')
        orelse (props:is_true(<<"Publish-Channel-State">>, Props, 'true')
                andalso handling_locally(Props)
               ),
    Args = [IsLocal, kzd_freeswitch:media_recorder(Props), Props, UUID, Node],
    kz_util:spawn(fun maybe_store_recording/5, Args);
process_specific_event(<<"RECORD_START">>, _UUID, _Props, _Node) -> 'ok';
process_specific_event(_Event, _UUID, _Props, _Node) ->
    lager:debug("event ~s for callid ~s not handled in recordings (~s)", [_Event, _UUID, _Node]).

-spec maybe_publish_record_event(kz_term:proplist()) -> 'ok'.
maybe_publish_record_event(Props) ->
    case props:is_true(<<"Force-Publish-Event-State">>, Props, 'false')
        orelse (props:is_true(<<"Publish-Channel-State">>, Props, 'true')
                andalso handling_locally(Props)
               )
    of
        'true' -> ecallmgr_call_events:process_channel_event(Props);
        'false' -> lager:debug("not publishing record event ~s", [kzd_freeswitch:event_name(Props)])
    end.

-spec maybe_store_recording(boolean(), kz_term:api_binary(), kz_term:proplist(), kz_term:ne_binary(), atom()) ->
                                   'ok' |
                                   'error' |
                                   ecallmgr_util:send_cmd_ret() |
                                   [ecallmgr_util:send_cmd_ret(),...].
maybe_store_recording('false', _, _Props, _CallId, _Node) -> 'ok';
maybe_store_recording('true', <<"kz_media_recording">>, _Props, _CallId, _Node) -> 'ok';
maybe_store_recording('true', _, Props, CallId, Node) ->
    case kzd_freeswitch:ccv(Props, <<"Media-Transfer-Destination">>) of
        'undefined' -> 'ok';
        <<>> -> 'ok';
        <<_/binary>> = Destination ->
            kz_util:put_callid(CallId),
            lager:debug("no one is handling call recording, storing recording to ~s", [Destination]),

            MediaName = kzd_freeswitch:ccv(Props, <<"Media-Name">>),
            %% TODO: if you change this logic be sure it matches kz_media_util as well!
            Url = kz_binary:join([kz_binary:strip_right(Destination, $/)
                                 ,MediaName
                                 ]
                                ,<<"/">>
                                ),

            JObj = kz_json:from_list(
                     [{<<"Call-ID">>, CallId}
                     ,{<<"Msg-ID">>, CallId}
                     ,{<<"Media-Name">>, MediaName}
                     ,{<<"Media-Transfer-Destination">>, Url}
                     ,{<<"Insert-At">>, <<"now">>}
                     ,{<<"Media-Transfer-Method">>, media_transfer_method(Props)}
                     ,{<<"Application-Name">>, <<"store">>}
                     ,{<<"Event-Category">>, <<"call">>}
                     ,{<<"Event-Name">>, <<"command">>}
                      | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                     ]),
            ecallmgr_call_command:exec_cmd(Node, CallId, JObj, 'undefined')
    end.

-spec media_transfer_method(kz_term:proplist()) -> kz_term:ne_binary().
media_transfer_method(Props) ->
    kzd_freeswitch:ccv(Props, <<"Media-Transfer-Method">>, <<"put">>).

-spec handling_locally(kz_term:proplist()) -> boolean().
handling_locally(Props) ->
    handling_locally(Props, kzd_freeswitch:other_leg_call_id(Props)).

-spec handling_locally(kz_term:proplist(), kz_term:api_binary()) -> boolean().
handling_locally(Props, 'undefined') ->
    props:get_value(?GET_CCV(<<"Ecallmgr-Node">>), Props)
        =:= kz_term:to_binary(node());
handling_locally(Props, OtherLeg) ->
    Node = kz_term:to_binary(node()),
    case props:get_value(?GET_CCV(<<"Ecallmgr-Node">>), Props) of
        Node -> 'true';
        _ -> other_leg_handling_locally(OtherLeg)
    end.

-spec other_leg_handling_locally(kz_term:ne_binary()) -> boolean().
other_leg_handling_locally(OtherLeg) ->
    case ecallmgr_fs_channel:fetch(OtherLeg, 'record') of
        {'ok', #channel{handling_locally=HandleLocally}} -> HandleLocally;
        _ -> 'false'
    end.
