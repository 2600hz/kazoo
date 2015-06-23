%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
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
          ,options :: wh_proplist()
         }).

-define(SERVER, ?MODULE).

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

-include_lib("nksip/include/nksip.hrl").
-include("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> startlink_ret().
-spec start_link(atom(), wh_proplist()) -> startlink_ret().

start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    NodeBin = wh_util:to_binary(Node),
    gen_listener:start_link(?MODULE
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
handle_info({'event', Props}, #state{node=Node}=State) ->
    _ = wh_util:spawn(?MODULE, 'process_fs_event', [Node, Props]),
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

-spec handle_message_route(wh_json:object(), wh_proplist()) -> no_return().
handle_message_route(JObj, Props) ->
    _ = wh_util:put_callid(JObj),
    Node = props:get_value('node', Props),
    Endpoints = wh_json:get_ne_value(<<"Endpoints">>, JObj, []),
    case wapi_sms:message_v(JObj) of
        'false' -> send_error(Node, JObj, <<"sms failed to execute as JObj did not validate">>);
        'true' when Endpoints =:= [] -> send_error(Node, JObj, <<"sms request had no endpoints">>);
        'true' -> send_message(JObj, Props, Endpoints)
    end.

-spec send_message(wh_json:object(), wh_proplist(), wh_json:objects()) -> no_return().
send_message(JObj, Props, [Endpoint]) ->
    Node = props:get_value('node', Props),
    case format_endpoint(Endpoint, Props, JObj) of
        {'error', E} ->
            send_error(Node, JObj, E);
        {'ok', EndpointProps} ->
            lager:debug("sending sms message ~s to freeswitch", [wh_json:get_value(<<"Call-ID">>, JObj)]),
            EvtProps = lists:append(build_message_headers(JObj, Endpoint), EndpointProps),
            freeswitch:sendevent_custom(Node, 'SMS::SEND_MESSAGE', EvtProps)
    end.

-spec build_message_headers(wh_json:object(), wh_json:object()) -> wh_proplist().
build_message_headers(JObj, Endpoint) ->
    Body = wh_json:get_value(<<"Body">>, JObj),
    MessageId = wh_json:get_value(<<"Message-ID">>, JObj),
    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    CIDNumber = wh_json:get_value(<<"Caller-ID-Number">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    ToRealm = wh_json:get_value(<<"To-Realm">>, Endpoint),

    FromURI = get_uri(wh_json:get_value(<<"From-URI">>, CCVs)),
    Realm = wh_json:get_value(<<"Account-Realm">>, CCVs),

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
                ,{"from", wh_util:to_list(CIDNumber)}
                ,{"from_full", wh_util:to_list(FromFull)}
                ,{"content-length", wh_util:to_list(size(Body))}
                ,{"type", "text/plain"}
                ,{"body", Body}
                ,{"Call-ID", wh_util:to_list(CallId)}
                ,{"Unique-ID", wh_util:to_list(CallId)}
                ,{"Server-ID", wh_util:to_list(ServerId)}
                ,{"Message-ID", wh_util:to_list(MessageId)}
                ,{"Msg-ID", wh_util:to_list(MsgId)}
                ,{"sip_h_X-Kazoo-Bounce", wh_util:to_list(wh_util:rand_hex_binary(12))}
               ]),
    wh_json:foldl(fun headers_foldl/3, Header, CCVs).

-spec headers_foldl(wh_json:key(), wh_json:json_term(), wh_proplist()) -> wh_proplist().
headers_foldl(K, V, Acc) ->
    [{wh_util:to_list(?GET_CCV(K)), wh_util:to_list(V)} | Acc].

-spec get_uri(api_binary()) -> api_binary().
get_uri('undefined') -> 'undefined';
get_uri(<<"<sip", _/binary>>=Uri) -> Uri;
get_uri(<<"sip", _/binary>>=Uri) -> Uri;
get_uri(Uri) -> <<"<sip:", Uri/binary, ">">>.

-spec send_error(atom(), wh_json:object(), any()) -> any().
send_error(Node, JObj, Err) ->
    ServerId =  wh_json:get_value(<<"Server-ID">>, JObj),
    Payload =
        [{<<"Error-Description">>, wh_util:to_binary(Err)}
         ,{<<"Status">>, <<"Error">>}
         ,{<<"Delivery-Result-Code">>, <<"503">>}
         ,{<<"Delivery-Result-Text">>, wh_util:to_binary(Err)}
         ,{<<"Delivery-Failure">>, 'true'}
         ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
         ,{<<"Message-ID">>, wh_json:get_value(<<"Message-ID">>, JObj)}
         ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
         ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
         ,{<<"Custom-Channel-Vars">>, wh_json:get_value(<<"Custom-Channel-Vars">>, JObj)}
             | wh_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
        ],
    wh_amqp_worker:cast(Payload, fun(A) -> wapi_sms:publish_targeted_delivery(ServerId, A) end).

-spec format_endpoint(wh_json:object(), wh_proplist(), wh_json:object()) ->
                             {'ok', wh_proplist()} |
                             {'error', ne_binary()}.
-spec format_endpoint(wh_json:object(), wh_proplist(), wh_json:object(), ne_binary()) ->
                             {'ok', wh_proplist()} |
                             {'error', ne_binary()}.
format_endpoint(Endpoint, Props, JObj) ->
    format_endpoint(
      Endpoint
      ,Props
      ,JObj
      ,wh_json:get_value(<<"Invite-Format">>, Endpoint)
     ).

format_endpoint(Endpoint, Props, JObj, <<"route">>) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    case wh_json:is_true(<<"Bounce-Back">>, CCVs, 'false') of
        'true' -> format_bounce_endpoint(Endpoint, Props, JObj);
        'false' -> format_route_endpoint(Endpoint, Props, JObj)
    end;
format_endpoint(Endpoint, _Props, _JObj, <<"username">>) ->
    Realm = wh_json:get_value(<<"To-Realm">>, Endpoint),
    Username = wh_json:get_value(<<"To-Username">>, Endpoint),
    ToURI = <<Username/binary, "@", Realm/binary>>,
    case ecallmgr_registrar:lookup_original_contact(Realm, Username) of
        {'ok', Contact} ->
            [#uri{user=_ToUser
                  ,domain=ToIP
                  ,port=ToPort
                 }=_ToContact] = nksip_parse:uris(Contact),
            {'ok', props:filter_empty(
                     [{"to", wh_util:to_list(ToURI)}
                      ,{"to_sip_ip", wh_util:to_list(ToIP)}
                      ,{"to_sip_port", wh_util:to_list(ToPort)}
                     ])};
        {'error', _Err}=E ->
            lager:debug("failed to find original contact for ~s@~s: ~p", [Username, Realm, _Err]),
            E
    end.

-spec format_route_endpoint(wh_json:object(), wh_proplist(), wh_json:object()) ->
                                   {'ok', wh_proplist()} |
                                   {'error', ne_binary()}.
format_route_endpoint(Endpoint, _Props, _JObj) ->
    ToURI = wh_json:get_value(<<"Route">>, Endpoint),
    [#uri{user=_ToUser
          ,domain=ToIP
          ,port=ToPort
         }=_ToContact] = nksip_parse:uris(ToURI),
    {'ok', props:filter_empty(
             [{"to", wh_util:to_list(ToURI)}
              ,{"to_sip_ip", wh_util:to_list(ToIP)}
              ,{"to_sip_port", wh_util:to_list(ToPort)}
             ])}.

-spec format_bounce_endpoint(wh_json:object(), wh_proplist(), wh_json:object()) -> {'ok', wh_proplist()} | {'error', ne_binary()}.
format_bounce_endpoint(Endpoint, Props, JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    ToDID = wh_json:get_value(<<"To-DID">>, Endpoint),
    ToRealm = wh_json:get_value(<<"Bounce-Realm">>, CCVs),
    To = <<ToDID/binary, "@", ToRealm/binary>>,
    Node = props:get_value('node', Props),
    ToURI = ecallmgr_fs_node:sip_url(Node),
    [#uri{user=_ToUser
          ,domain=ToIP
          ,port=ToPort
         }=_ToContact] = nksip_parse:uris(ToURI),
    {'ok', props:filter_empty(
      [{"to", wh_util:to_list(To)}
       ,{"to_sip_ip", wh_util:to_list(ToIP)}
       ,{"to_sip_port", wh_util:to_list(ToPort)}
      ])}.

-spec process_fs_event(atom(), wh_proplist()) -> any().
process_fs_event(Node, Props) ->
    process_fs_event(
      props:get_value(<<"Event-Name">>, Props),
      props:get_value(<<"Event-Subclass">>, Props),
      Node,
      lists:usort(Props)).

-spec process_fs_event(ne_binary(), ne_binary(), atom(), wh_proplist()) -> any().
process_fs_event(<<"CUSTOM">>, <<"KZ::DELIVERY_REPORT">>, Node, Props) ->
    process_fs_event(<<"CUSTOM">>, <<"SMS::DELIVERY_REPORT">>, Node, Props);
process_fs_event(<<"CUSTOM">>, <<"SMS::DELIVERY_REPORT">>, Node, Props) ->
    ServerId =  props:get_value(<<"Server-ID">>, Props),
    CallId = props:get_value(<<"Call-ID">>, Props),
    BaseProps = props:filter_empty(props:filter_undefined(
        [{<<"Call-ID">>, CallId}
         ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, Props)}
         ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
         ,{<<"Switch-Hostname">>, props:get_value(<<"FreeSWITCH-Hostname">>, Props)}
         ,{<<"Delivery-Result-Code">>, props:get_value(<<"Delivery-Result-Code">>, Props)}
         ,{<<"Delivery-Failure">>, props:get_value(<<"Delivery-Failure">>, Props)}
         ,{<<"Custom-Channel-Vars">>, wh_json:from_list( get_ccvs(Props)) }
         ,{<<"Msg-ID">>, props:get_value(<<"Msg-ID">>, Props)}
         ,{<<"Status">>, props:get_value(<<"Status">>, Props)}
             | wh_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
        ])),
    lager:debug("Recieved delivery event for message ~s",[CallId]),
    EventProps = get_event_uris(Props, BaseProps),
    wh_amqp_worker:cast(EventProps, fun(A) -> wapi_sms:publish_targeted_delivery(ServerId, A) end);

process_fs_event(_EventName, _SubClass, _Node, _Props) ->
    lager:debug("Event ~s/~s not processed on node ~s",[_EventName, _SubClass, _Node]).

get_event_uris(Props, EventProps) ->
    Uris = [{<<"From">>, <<"from_full">>}
            ,{<<"To">>, <<"to">>}],
    lists:foldl(fun(T, Acc) ->
                        lists:append(Acc, get_event_uris_props(T, Props))
                end, EventProps, Uris).

-spec get_event_uris_props(tuple() | ne_binary(), wh_proplist() | ne_binary()) -> wh_proplist().
get_event_uris_props({K, F}, Props) ->
    get_event_uris_props( get_uri( props:get_value(F, Props) ), K);
get_event_uris_props('undefined', _) -> [];
get_event_uris_props(Uri, Base) ->
    [#uri{user=User, domain=Realm}=_URI] = nksip_parse:uris(Uri),
    [{Base, <<User/binary, "@", Realm/binary>>}
     ,{<<Base/binary, "-User">>, User }
     ,{<<Base/binary, "-Realm">>, Realm }].

-spec is_ccv(tuple()) -> boolean().
is_ccv({?GET_CCV(_K), _V}) -> 'true';
is_ccv(_) -> 'false'.

-spec get_ccvs(wh_proplist()) -> wh_proplist().
get_ccvs(Props) ->
    [ {K, V} || {?GET_CCV(K), V} <- lists:filter(fun is_ccv/1, Props)].
