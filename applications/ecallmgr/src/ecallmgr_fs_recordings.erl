%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%% Receives START/STOP RECORD event
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
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
               ,options = [] :: kz_proplist()
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
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(list()) -> {'ok', state()}.
init([Node, Options]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(Node),
    gen_server:cast(self(), 'bind_to_record'),
    {'ok', #state{node=Node, options=Options}}.

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
    {'reply', {'error', 'not_implemented'}, State}.

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
terminate(_Reason, #state{node=Node}) ->
    lager:info("recording handler for ~s terminating: ~p", [Node, _Reason]).

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
-spec init_props(kz_proplist(), kz_proplist()) -> kz_proplist().
init_props(Props, Options) ->
    case props:get_is_true(<<"Publish-Channel-State">>, Props) of
        'undefined' ->
            case props:is_false(<<"Publish-Channel-State">>, Options, 'false') of
                'true' -> props:set_value(<<"Publish-Channel-State">>, 'false', Props);
                _ -> Props
            end;
        _Value -> Props
    end.

-spec handle_record_event(atom(), kz_proplist(), kz_proplist()) -> 'ok'.
handle_record_event(Node, [UUID | FSProps], Options) ->
    kz_util:put_callid(UUID),
    Props = init_props(FSProps, Options),
    process_event(UUID, Props, Node),
    maybe_publish_record_event(Props).

-spec process_event(api_binary(), kz_proplist(), atom()) -> any().
process_event(UUID, Props, Node) ->
    kz_util:put_callid(UUID),
    EventName = kzd_freeswitch:event_name(Props),
    process_specific_event(EventName, UUID, Props, Node).

-spec process_specific_event(ne_binary(), api_binary(), kz_proplist(), atom()) -> any().
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

-spec maybe_publish_record_event(kz_proplist()) -> 'ok'.
maybe_publish_record_event(Props) ->
    case props:is_true(<<"Force-Publish-Event-State">>, Props, 'true')
        orelse (props:is_true(<<"Publish-Channel-State">>, Props, 'true')
                andalso handling_locally(Props)
               )
    of
        'true' -> ecallmgr_call_events:process_channel_event(Props);
        'false' -> lager:debug("not publishing record event ~s", [kzd_freeswitch:event_name(Props)])
    end.

-spec maybe_store_recording(boolean(), api_binary(), kz_proplist(), ne_binary(), atom()) ->
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

-spec media_transfer_method(kz_proplist()) -> ne_binary().
media_transfer_method(Props) ->
    kzd_freeswitch:ccv(Props, <<"Media-Transfer-Method">>, <<"put">>).

-spec handling_locally(kz_proplist()) -> boolean().
handling_locally(Props) ->
    handling_locally(Props, kzd_freeswitch:other_leg_call_id(Props)).

-spec handling_locally(kz_proplist(), api_binary()) -> boolean().
handling_locally(Props, 'undefined') ->
    props:get_value(?GET_CCV(<<"Ecallmgr-Node">>), Props)
        =:= kz_term:to_binary(node());
handling_locally(Props, OtherLeg) ->
    Node = kz_term:to_binary(node()),
    case props:get_value(?GET_CCV(<<"Ecallmgr-Node">>), Props) of
        Node -> 'true';
        _ -> other_leg_handling_locally(OtherLeg)
    end.

-spec other_leg_handling_locally(ne_binary()) -> boolean().
other_leg_handling_locally(OtherLeg) ->
    case ecallmgr_fs_channel:fetch(OtherLeg, 'record') of
        {'ok', #channel{handling_locally=HandleLocally}} -> HandleLocally;
        _ -> 'false'
    end.
