%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%% Notify-type requests, like MWI updates, received and processed here
%%% @end
%%% @contributors
%%%    Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_msg).

-behaviour(gen_listener).

-export([start_link/1, start_link/2]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-export([handle_message_route/2]).
-export([process_fs_event/2]).

-record(state, {
          node :: atom()
               ,options :: kz_proplist()
         }).

-define(BINDINGS(Node), [{'sms', [{'route_id', Node}
                                 ,{'restrict_to', ['route']}
                                 ]
                         }
                        ,{'self', []}
                        ]).
-define(RESPONDERS, [{{?MODULE, 'handle_message_route'}
                     ,[{<<"message">>, <<"route">>}]
                     }
                    ]).

-define(QUEUE_NAME(N), <<"ecallmgr_fs_msg_", N/binary>>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-include_lib("kazoo_sip/include/kzsip_uri.hrl").
-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

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
    NodeBin = kz_util:to_binary(Node),
    gen_listener:start_link(?SERVER
                           ,[{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS(NodeBin)}
                            ,{'queue_name', ?QUEUE_NAME(NodeBin)}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[Node, Options]).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([Node, Options]) ->
    put(callid, Node),
    lager:debug("starting new ecallmgr msg process"),
    gproc:reg({'p', 'l', 'fs_msg'}),
    gen_server:cast(self(), 'bind_to_msg_events'),
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
handle_cast('bind_to_msg_events', #state{node=Node}=State) ->
    gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"KZ::DELIVERY_REPORT">>)}),
    gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"SMS::DELIVERY_REPORT">>)}),
    lager:debug("bound to recv_message events on node ~s", [Node]),
    {'noreply', State};
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
handle_info({'event', Props}, #state{node=Node}=State) ->
    _ = kz_util:spawn(fun process_fs_event/2, [Node, Props]),
    {'noreply', State, 'hibernate'};
handle_info({'EXIT', _, _}, State) ->
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("MSG UN", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{node=Node}) ->
    {'reply', [{'node', Node}]}.

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
terminate(_Reason, #state{node=Node}) ->
    lager:info("notify listener for ~s terminating: ~p", [Node, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_message_route(kz_json:object(), kz_proplist()) -> no_return().
handle_message_route(JObj, Props) ->
    _ = kz_util:put_callid(JObj),
    Node = props:get_value('node', Props),
    Endpoints = kz_json:get_ne_value(<<"Endpoints">>, JObj, []),
    case kapi_sms:message_v(JObj) of
        'false' -> send_error(Node, JObj, <<"sms failed to execute as JObj did not validate">>);
        'true' when Endpoints =:= [] -> send_error(Node, JObj, <<"sms request had no endpoints">>);
        'true' -> send_message(JObj, Props, Endpoints)
    end.

-spec send_message(kz_json:object(), kz_proplist(), kz_json:objects()) -> no_return().
send_message(JObj, Props, [Endpoint]) ->
    Node = props:get_value('node', Props),
    case format_endpoint(Endpoint, Props, JObj) of
        {'error', E} ->
            send_error(Node, JObj, E);
        {'ok', EndpointProps} ->
            lager:debug("sending sms message ~s to freeswitch", [kz_json:get_value(<<"Call-ID">>, JObj)]),
            EvtProps = build_message_headers(JObj, Endpoint) ++ EndpointProps,
            freeswitch:sendevent_custom(Node, 'SMS::SEND_MESSAGE', EvtProps)
    end.

-spec build_message_headers(kz_json:object(), kz_json:object()) -> kz_proplist().
build_message_headers(JObj, Endpoint) ->
    Body = kz_json:get_value(<<"Body">>, JObj),
    MessageId = kz_json:get_value(<<"Message-ID">>, JObj),
    MsgId = kz_json:get_value(<<"Msg-ID">>, JObj),
    ServerId = kz_json:get_value(<<"Server-ID">>, JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    CIDNumber = kz_json:get_value(<<"Caller-ID-Number">>, JObj),
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    ToRealm = kz_json:get_value(<<"To-Realm">>, Endpoint),

    FromURI = get_uri(kz_json:get_value(<<"From-URI">>, CCVs)),
    Realm = kz_json:get_value(<<"Account-Realm">>, CCVs),

    FromFull = case {ToRealm, FromURI} of
                   {'undefined', 'undefined'} ->
                       <<"sip:", CIDNumber/binary, "@", Realm/binary>>;
                   {'undefined', FromURI} ->
                       FromURI;
                   {ToRealm, _} ->
                       <<"sip:", CIDNumber/binary, "@", ToRealm/binary>>
               end,
    Header = props:filter_undefined(
               [{"sip_profile", ?DEFAULT_FS_PROFILE}
               ,{"proto", "sip"}
               ,{"blocking", "true"}
               ,{"dest_proto", "sip"}
               ,{"from", kz_util:to_list(CIDNumber)}
               ,{"from_full", kz_util:to_list(FromFull)}
               ,{"content-length", kz_util:to_list(size(Body))}
               ,{"type", "text/plain"}
               ,{"body", Body}
               ,{"Call-ID", kz_util:to_list(CallId)}
               ,{"Unique-ID", kz_util:to_list(CallId)}
               ,{"Server-ID", kz_util:to_list(ServerId)}
               ,{"Message-ID", kz_util:to_list(MessageId)}
               ,{"Msg-ID", kz_util:to_list(MsgId)}
               ,{"sip_h_X-Kazoo-Bounce", kz_util:to_list(kz_util:rand_hex_binary(12))}
               ]),
    kz_json:foldl(fun headers_foldl/3, Header, CCVs).

-spec headers_foldl(kz_json:key(), kz_json:json_term(), kz_proplist()) -> kz_proplist().
headers_foldl(K, V, Acc) ->
    [{kz_util:to_list(?GET_CCV(K)), kz_util:to_list(V)} | Acc].

-spec get_uri(api_binary()) -> api_binary().
get_uri('undefined') -> 'undefined';
get_uri(<<"<sip", _/binary>>=Uri) -> Uri;
get_uri(<<"sip", _/binary>>=Uri) -> Uri;
get_uri(Uri) -> <<"<sip:", Uri/binary, ">">>.

-spec send_error(atom(), kz_json:object(), any()) -> any().
send_error(Node, JObj, Err) ->
    ServerId =  kz_json:get_value(<<"Server-ID">>, JObj),
    Payload =
        [{<<"Error-Description">>, kz_util:to_binary(Err)}
        ,{<<"Status">>, <<"Error">>}
        ,{<<"Delivery-Result-Code">>, <<"503">>}
        ,{<<"Delivery-Result-Text">>, kz_util:to_binary(Err)}
        ,{<<"Delivery-Failure">>, 'true'}
        ,{<<"Call-ID">>, kz_json:get_value(<<"Call-ID">>, JObj)}
        ,{<<"Message-ID">>, kz_json:get_value(<<"Message-ID">>, JObj)}
        ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
        ,{<<"Switch-Nodename">>, kz_util:to_binary(Node)}
        ,{<<"Custom-Channel-Vars">>, kz_json:get_value(<<"Custom-Channel-Vars">>, JObj)}
         | kz_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
        ],
    kz_amqp_worker:cast(Payload, fun(A) -> kapi_sms:publish_targeted_delivery(ServerId, A) end).

-spec format_endpoint(kz_json:object(), kz_proplist(), kz_json:object()) ->
                             {'ok', kz_proplist()} |
                             {'error', ne_binary()}.
-spec format_endpoint(kz_json:object(), kz_proplist(), kz_json:object(), ne_binary()) ->
                             {'ok', kz_proplist()} |
                             {'error', ne_binary()}.
format_endpoint(Endpoint, Props, JObj) ->
    format_endpoint(
      Endpoint
                   ,Props
                   ,JObj
                   ,kz_json:get_value(<<"Invite-Format">>, Endpoint)
     ).

format_endpoint(Endpoint, Props, JObj, <<"route">>) ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    case kz_json:is_true(<<"Bounce-Back">>, CCVs, 'false') of
        'true' -> format_bounce_endpoint(Endpoint, Props, JObj);
        'false' -> format_route_endpoint(Endpoint, Props, JObj)
    end;
format_endpoint(Endpoint, _Props, _JObj, <<"username">>) ->
    Realm = kz_json:get_value(<<"To-Realm">>, Endpoint),
    Username = kz_json:get_value(<<"To-Username">>, Endpoint),
    ToURI = <<Username/binary, "@", Realm/binary>>,
    case ecallmgr_registrar:lookup_original_contact(Realm, Username) of
        {'ok', Contact} ->
            [#uri{user=_ToUser
                 ,domain=ToIP
                 ,port=ToPort
                 }=_ToContact] = kzsip_uri:uris(Contact),
            {'ok', props:filter_empty(
                     [{"to", kz_util:to_list(ToURI)}
                     ,{"to_sip_ip", kz_util:to_list(ToIP)}
                     ,{"to_sip_port", kz_util:to_list(ToPort)}
                     ])};
        {'error', _Err}=E ->
            lager:debug("failed to find original contact for ~s@~s: ~p", [Username, Realm, _Err]),
            E
    end.

-spec format_route_endpoint(kz_json:object(), kz_proplist(), kz_json:object()) ->
                                   {'ok', kz_proplist()} |
                                   {'error', ne_binary()}.
format_route_endpoint(Endpoint, _Props, _JObj) ->
    ToURI = kz_json:get_value(<<"Route">>, Endpoint),
    [#uri{user=_ToUser
         ,domain=ToIP
         ,port=ToPort
         }=_ToContact] = kzsip_uri:uris(ToURI),
    {'ok', props:filter_empty(
             [{"to", kz_util:to_list(ToURI)}
             ,{"to_sip_ip", kz_util:to_list(ToIP)}
             ,{"to_sip_port", kz_util:to_list(ToPort)}
             ])}.

-spec format_bounce_endpoint(kz_json:object(), kz_proplist(), kz_json:object()) -> {'ok', kz_proplist()} | {'error', ne_binary()}.
format_bounce_endpoint(Endpoint, Props, JObj) ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    ToDID = kz_json:get_value(<<"To-DID">>, Endpoint),
    ToRealm = kz_json:get_value(<<"Bounce-Realm">>, CCVs),
    To = <<ToDID/binary, "@", ToRealm/binary>>,
    Node = props:get_value('node', Props),
    ToURI = ecallmgr_fs_node:sip_url(Node),
    [#uri{user=_ToUser
         ,domain=ToIP
         ,port=ToPort
         }=_ToContact] = kzsip_uri:uris(ToURI),
    {'ok', props:filter_empty(
             [{"to", kz_util:to_list(To)}
             ,{"to_sip_ip", kz_util:to_list(ToIP)}
             ,{"to_sip_port", kz_util:to_list(ToPort)}
             ])}.

-spec process_fs_event(atom(), kz_proplist()) -> any().
process_fs_event(Node, Props) ->
    process_fs_event(
      props:get_value(<<"Event-Name">>, Props),
      props:get_value(<<"Event-Subclass">>, Props),
      Node,
      lists:usort(Props)).

-spec process_fs_event(ne_binary(), ne_binary(), atom(), kz_proplist()) -> any().
process_fs_event(<<"CUSTOM">>, <<"KZ::DELIVERY_REPORT">>, Node, Props) ->
    process_fs_event(<<"CUSTOM">>, <<"SMS::DELIVERY_REPORT">>, Node, Props);
process_fs_event(<<"CUSTOM">>, <<"SMS::DELIVERY_REPORT">>, Node, Props) ->
    ServerId =  props:get_value(<<"Server-ID">>, Props),
    CallId = props:get_value(<<"Call-ID">>, Props),
    BaseProps = props:filter_empty(props:filter_undefined(
                                     [{<<"Call-ID">>, CallId}
                                     ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, Props)}
                                     ,{<<"Switch-Nodename">>, kz_util:to_binary(Node)}
                                     ,{<<"Switch-Hostname">>, props:get_value(<<"FreeSWITCH-Hostname">>, Props)}
                                     ,{<<"Delivery-Result-Code">>, props:get_value(<<"Delivery-Result-Code">>, Props)}
                                     ,{<<"Delivery-Failure">>, props:get_value(<<"Delivery-Failure">>, Props)}
                                     ,{<<"Custom-Channel-Vars">>, kz_json:from_list( get_ccvs(Props)) }
                                     ,{<<"Msg-ID">>, props:get_value(<<"Msg-ID">>, Props)}
                                     ,{<<"Status">>, props:get_value(<<"Status">>, Props)}
                                      | kz_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
                                     ])),
    lager:debug("received delivery event for message ~s",[CallId]),
    EventProps = get_event_uris(Props, BaseProps),
    kz_amqp_worker:cast(EventProps, fun(A) -> kapi_sms:publish_targeted_delivery(ServerId, A) end);

process_fs_event(_EventName, _SubClass, _Node, _Props) ->
    lager:debug("Event ~s/~s not processed on node ~s",[_EventName, _SubClass, _Node]).

get_event_uris(Props, EventProps) ->
    Uris = [{<<"From">>, <<"from_full">>}
           ,{<<"To">>, <<"to">>}],
    lists:foldl(fun(T, Acc) ->
                        Acc ++ get_event_uris_props(T, Props)
                end, EventProps, Uris).

-spec get_event_uris_props(tuple() | ne_binary(), kz_proplist() | ne_binary()) -> kz_proplist().
get_event_uris_props({K, F}, Props) ->
    get_event_uris_props( get_uri( props:get_value(F, Props) ), K);
get_event_uris_props('undefined', _) -> [];
get_event_uris_props(Uri, Base) ->
    [#uri{user=User, domain=Realm}=_URI] = kzsip_uri:uris(Uri),
    [{Base, <<User/binary, "@", Realm/binary>>}
    ,{<<Base/binary, "-User">>, User }
    ,{<<Base/binary, "-Realm">>, Realm }].

-spec is_ccv(tuple()) -> boolean().
is_ccv({?GET_CCV(_K), _V}) -> 'true';
is_ccv(_) -> 'false'.

-spec get_ccvs(kz_proplist()) -> kz_proplist().
get_ccvs(Props) ->
    [{K, V} || {?GET_CCV(K), V} <- [P || P <- Props, is_ccv(P)]].
