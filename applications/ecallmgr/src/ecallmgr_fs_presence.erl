%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%% Receives PRESENCE_IN event
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
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
-spec init([atom() | kz_proplist()]) -> {'ok', state()}.
init([Node, Options]) ->
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
    gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"PRESENCE_IN">>)}),
    {'noreply', State};
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
handle_info({'event', [UUID | Props]}, #state{node=Node}=State) ->
    _ = kz_util:spawn(fun process_event/3, [UUID, Props, Node]),
    {'noreply', State};
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
-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{node=Node}) ->
    lager:info("presence listener for ~s terminating: ~p", [Node, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec process_event(api_binary(), kz_proplist(), atom()) -> any().
process_event(UUID, Props, Node) ->
    kz_util:put_callid(UUID),
    EventName = props:get_value(<<"Event-Subclass">>, Props, props:get_value(<<"Event-Name">>, Props)),
    process_specific_event(EventName, UUID, Props, Node).

-spec process_specific_event(ne_binary(), api_binary(), kz_proplist(), atom()) -> any().
process_specific_event(<<"PRESENCE_IN">>, UUID, Props, Node) ->
    AuthType = props:get_value(?GET_CCV(<<"Authorizing-Type">>), Props),
    maybe_build_presence_event(Node, UUID, Props, AuthType);
process_specific_event(_Event, _UUID, _Props, _Node) ->
    lager:debug("event ~s for callid ~s not handled in presence (~s)", [_Event, _UUID, _Node]).


maybe_build_presence_event(Node, UUID, Props, <<"device">>) ->
    build_presence_event(Node, UUID, Props);
maybe_build_presence_event(_Node, _UUID, _Props, _AuthType) ->
    lager:debug("Authorizing-Type ~p not handled", [_AuthType]).

from(Props) ->
    props:get_first_defined([<<"from">>
                            ,<<"variable_presence_id">>
                            ,<<"Channel-Presence-ID">>
                            ], Props).

to_user(Props) ->
    to_user(direction(Props), Props).

to_user(<<"initiator">>, Props) ->
    props:get_first_defined([<<"Caller-Destination-Number">>
                            ,<<"variable_sip_to_user">>
                            ], Props);
to_user(<<"recipient">>, Props) ->
    props:get_first_defined([<<"Caller-Caller-ID-Number">>
                            ,<<"variable_sip_from_user">>
                            ], Props).

expires(<<"early">>) -> 0;
expires(<<"confirmed">>) -> 0;
expires(<<"terminated">>) -> 20.

build_presence_event(_Node, UUID, Props) ->
    ToTag = kzd_freeswitch:to_tag(Props),
    FromTag = kzd_freeswitch:from_tag(Props),

    From = from(Props),
    [FromUser, Realm | _] = binary:split(From, <<"@">>, ['global']),

    ToUser =  to_user(Props),

    To =  <<ToUser/binary, "@", Realm/binary>>,
    ToURI =  <<"sip:", To/binary>>,

    PresenceURI =  <<"sip:", From/binary>>,
    State = presence_status(Props),
    Expires = expires(State),
    SwitchURI = props:get_value(<<"Switch-URI">>, Props),

    Payload = props:filter_undefined(
                [{<<"Presence-ID">>, From}

                ,{<<"From">>, PresenceURI}
                ,{<<"From-User">>, FromUser}
                ,{<<"From-Realm">>, Realm}
                ,{<<"From-Tag">>, FromTag}

                ,{<<"To">>, ToURI}
                ,{<<"To-User">>, ToUser}
                ,{<<"To-Realm">>, Realm}
                ,{<<"To-Tag">>, ToTag}
                ,{<<"To-URI">>, PresenceURI}

                ,{<<"Direction">>, direction(Props)}
                ,{<<"State">>, State}
                ,{<<"Call-ID">>, UUID}
                ,{<<"Switch-URI">>, SwitchURI}
                ,{<<"Expires">>, Expires}
                ,{<<"Event-Package">>, <<"dialog">>}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    lager:debug("sending presence ~s to ~s/~s in realm ~s", [State, FromUser, ToUser, Realm]),
    kz_amqp_worker:cast(Payload, fun kapi_presence:publish_dialog/1).

direction(Props) ->
    case props:get_value(<<"Presence-Call-Direction">>, Props) of
        <<"inbound">> -> <<"initiator">>;
        <<"outbound">> -> <<"recipient">>
    end.

status(Props) ->
    kz_term:to_lower_binary(props:get_binary_value(<<"status">>, Props)).

answer_state(Props) ->
    kz_term:to_lower_binary(props:get_value(<<"Answer-State">>, Props)).

presence_status(Props) ->
    Status = status(Props),
    AnswerState = answer_state(Props),
    Direction = direction(Props),
    presence_status(Direction, Status, AnswerState).

presence_status(_, _, <<"answered">>) -> <<"confirmed">>;
presence_status(_, <<"hangup">>, _) -> <<"terminated">>;
presence_status(Direction, <<"cs_", Status/binary>>, AnswerState) -> presence_status(Direction, Status, AnswerState);
presence_status(<<"initiator">>, _, <<"ringing">>) -> <<"confirmed">>;
presence_status(<<"recipient">>, _, <<"ringing">>) -> <<"early">>;
presence_status(_ , _, AnswerState) -> AnswerState.
