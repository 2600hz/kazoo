%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Track the FreeSWITCH channel information, and provide accessors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_channel).

-behaviour(gen_server).

-export([start_link/1
         ,start_link/2
        ]).
-export([node/1, set_node/2
         ,former_node/1
         ,is_bridged/1
         ,exists/1
         ,import_moh/1
         ,set_account_id/2
         ,fetch/1
         ,renew/2
         ,to_json/1
        ]).
-export([handle_channel_status/2]).
-export([process_event/3]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-compile([{'no_auto_import', [node/1]}]).

-include("ecallmgr.hrl").

-define(RESPONDERS, [{{?MODULE, 'handle_channel_status'}
                      ,[{<<"call_event">>, <<"channel_status_req">>}]
                     }
                    ]).
-define(BINDINGS, [{'call', [{'restrict_to', ['status_req']}]}]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {node = 'undefined' :: atom()
                ,options = [] :: wh_proplist()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
                                      ,{bindings, ?BINDINGS}
                                      ,{queue_name, ?QUEUE_NAME}
                                      ,{queue_options, ?QUEUE_OPTIONS}
                                      ,{consume_options, ?CONSUME_OPTIONS}
                                     ], [Node, Options]).

-spec fetch(ne_binary()) ->
                   {'ok', wh_json:object()} |
                   {'error', 'not_found'}.
fetch(UUID) ->
    case ets:lookup(?CHANNELS_TBL, UUID) of
        [Channel] -> {'ok', to_json(Channel)};
        _Else -> {'error', 'not_found'}
    end.

-spec node(ne_binary()) ->
                  {'ok', atom()} |
                  {'error', 'not_found'}.
node(UUID) ->
    MatchSpec = [{#channel{uuid = '$1', node = '$2', _ = '_'}
                  ,[{'=:=', '$1', {'const', UUID}}]
                  ,['$2']}
                ],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        [Node] -> {'ok', Node};
        _ -> {'error', 'not_found'}
    end.

-spec set_node(atom(), ne_binary()) -> 'ok'.
set_node(Node, UUID) ->
    Updates =
        case node(UUID) of
            {'error', 'not_found'} -> [{#channel.node, Node}];
            {'ok', Node} -> [];
            {'ok', OldNode} ->
                [{#channel.node, Node}
                 ,{#channel.former_node, OldNode}
                ]
        end,
    ecallmgr_fs_channels:updates(UUID, Updates).

-spec former_node(ne_binary()) ->
                         {'ok', atom()} |
                         {'error', _}.
former_node(UUID) ->
    MatchSpec = [{#channel{uuid = '$1', former_node = '$2', _ = '_'}
                  ,[{'=:=', '$1', {'const', UUID}}]
                  ,['$2']}
                ],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        ['undefined'] -> {'ok', 'undefined'};
        [Node] -> {'ok', Node};
        _ -> {'error', 'not_found'}
    end.

-spec is_bridged(ne_binary()) -> boolean().
is_bridged(UUID) ->
    MatchSpec = [{#channel{uuid = '$1', other_leg = '$2', _ = '_'}
                  ,[{'=:=', '$1', {'const', UUID}}]
                  ,['$2']}
                ],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        ['undefined'] -> lager:debug("not bridged: undefined"), 'false';
        [Bin] when is_binary(Bin) -> lager:debug("bridged: ~s", [Bin]), 'true';
        _E -> lager:debug("not bridged: ~p", [_E]), 'false'
    end.

-spec exists(ne_binary()) -> boolean().
exists(UUID) -> ets:member(?CHANNELS_TBL, UUID).

-spec import_moh(ne_binary()) -> boolean().
import_moh(UUID) ->
    try ets:lookup_element(?CHANNELS_TBL, UUID, #channel.import_moh) of
        Import -> Import
    catch
        'error':'badarg' -> 'false'
    end.

-spec set_account_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_account_id(UUID, Value) when is_binary(Value) ->
    ecallmgr_fs_channels:update(UUID, #channel.account_id, Value);
set_account_id(UUID, Value) ->
    set_account_id(UUID, wh_util:to_binary(Value)).

-spec renew(atom(), ne_binary()) ->
                   {'ok', channel()} |
                   {'error', 'timeout' | 'badarg'}.
renew(Node, UUID) ->
    case freeswitch:api(Node, 'uuid_dump', wh_util:to_list(UUID)) of
        {'ok', Dump} ->
            Props = ecallmgr_util:eventstr_to_proplist(Dump),
            {'ok', props_to_record(Props, Node)};
        {'error', _}=E -> E;
        'timeout' -> {'error', 'timeout'}
    end.

-spec to_json(channel()) -> wh_json:object().
to_json(Channel) ->
    wh_json:from_list([{<<"uuid">>, Channel#channel.uuid}
                       ,{<<"destination">>, Channel#channel.destination}
                       ,{<<"direction">>, Channel#channel.direction}
                       ,{<<"account_id">>, Channel#channel.account_id}
                       ,{<<"account_billing">>, Channel#channel.account_billing}
                       ,{<<"authorizing_id">>, Channel#channel.authorizing_id}
                       ,{<<"authorizing_type">>, Channel#channel.authorizing_type}
                       ,{<<"owner_id">>, Channel#channel.owner_id}
                       ,{<<"resource_id">>, Channel#channel.resource_id}
                       ,{<<"presence_id">>, Channel#channel.presence_id}
                       ,{<<"fetch_id">>, Channel#channel.fetch_id}
                       ,{<<"bridge_id">>, Channel#channel.bridge_id}
                       ,{<<"precedence">>, Channel#channel.precedence}
                       ,{<<"reseller_id">>, Channel#channel.reseller_id}
                       ,{<<"reseller_billing">>, Channel#channel.reseller_billing}
                       ,{<<"realm">>, Channel#channel.realm}
                       ,{<<"username">>, Channel#channel.username}
                       ,{<<"answered">>, Channel#channel.answered}
                       ,{<<"node">>, Channel#channel.node}
                       ,{<<"timestamp">>, Channel#channel.timestamp}
                       ,{<<"profile">>, Channel#channel.profile}
                       ,{<<"context">>, Channel#channel.context}
                       ,{<<"dialplan">>, Channel#channel.dialplan}
                       ,{<<"other_leg">>, Channel#channel.other_leg}
                      ]).

-spec handle_channel_status(wh_json:json_object(), proplist()) -> 'ok'.
handle_channel_status(JObj, _Props) ->
    'true' = wapi_call:channel_status_req_v(JObj),
    _ = wh_util:put_callid(JObj),

    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("channel status request received"),

    AllNodesConnected = ecallmgr_fs_nodes:all_nodes_connected(),
    case fetch(CallId) of
        {'error', 'not_found'} when AllNodesConnected ->
            lager:debug("no node found with channel ~s", [CallId]),
            Resp = [{<<"Call-ID">>, CallId}
                    ,{<<"Status">>, <<"terminated">>}
                    ,{<<"Error-Msg">>, <<"no node found with channel">>}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_call:publish_channel_status_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
        {'error', 'not_found'} ->
            lager:debug("no node found with channel ~s, but we are not authoritative", [CallId]);
        {'ok', Channel} ->
            channel_status_resp(CallId, Channel, JObj)
    end.

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
init([Node, Options]) ->
    put('callid', Node),
    lager:info("starting new fs channel listener for ~s", [Node]),
    gen_server:cast(self(), 'bind_to_events'),
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
handle_cast('bind_to_events', #state{node=Node}=State) ->
    case gproc:reg({'p', 'l', {'event', Node, <<"CHANNEL_DATA">>}}) =:= 'true' 
        andalso gproc:reg({'p', 'l', {'event', Node, <<"CHANNEL_CREATE">>}}) =:= 'true'
        andalso gproc:reg({'p', 'l', {'event', Node, <<"CHANNEL_DESTROY">>}}) =:= 'true'
        andalso gproc:reg({'p', 'l', {'event', Node, <<"CHANNEL_ANSWER">>}}) =:= 'true'
        andalso gproc:reg({'p', 'l', {'event', Node, <<"CHANNEL_BRIDGE">>}}) =:= 'true'
        andalso gproc:reg({'p', 'l', {'event', Node, <<"CHANNEL_UNBRIDGE">>}}) =:= 'true'
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
handle_info({'event', [UUID | Props]}, #state{node=Node}=State) ->
    _ = spawn(?MODULE, 'process_event', [UUID, Props, Node]),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{}) ->
    {'reply', []}.

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
    lager:info("channel listener for ~s terminating: ~p", [Node, _Reason]).

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
process_event(UUID, Props, Node) ->
    EventName = props:get_value(<<"Event-Subclass">>, Props, props:get_value(<<"Event-Name">>, Props)),
    process_event(EventName, UUID, Props, Node).

-spec process_event(ne_binary(), api_binary(), wh_proplist(), atom()) -> any().
process_event(<<"CHANNEL_CREATE">>, UUID, Props, Node) ->
    _ = ecallmgr_fs_channels:new(props_to_record(Props, Node)),
    case props:get_value(?GET_CCV(<<"Ecallmgr-Node">>), Props) =:= wh_util:to_binary(node()) of
        'true' -> ecallmgr_fs_authz:authorize(Props, UUID, Node);
        'false' -> 'ok'
    end;
process_event(<<"CHANNEL_DESTROY">>, UUID, _, Node) ->
    ecallmgr_fs_channels:destroy(UUID, Node);
process_event(<<"CHANNEL_ANSWER">>, UUID, _, _) ->    
    ecallmgr_fs_channels:update(UUID, #channel.answered, 'true');
process_event(<<"CHANNEL_DATA">>, UUID, Props, _) ->
    ecallmgr_fs_channels:updates(UUID, props_to_update(Props));
process_event(<<"CHANNEL_BRIDGE">>, UUID, Props, _) ->
    OtherLeg = get_other_leg(UUID, Props),
    ecallmgr_fs_channels:update(UUID, #channel.other_leg, OtherLeg),
    ecallmgr_fs_channels:update(OtherLeg, #channel.other_leg, UUID);    
process_event(<<"CHANNEL_UNBRIDGE">>, UUID, Props, _) ->
    OtherLeg = get_other_leg(UUID, Props),
    ecallmgr_fs_channels:update(UUID, #channel.other_leg, 'undefined'),
    ecallmgr_fs_channels:update(OtherLeg, #channel.other_leg, 'undefined');
process_event(_, _, _, _) ->
    'ok'.

-spec props_to_record(wh_proplist(), atom()) -> channel().
props_to_record(Props, Node) ->
    UUID = props:get_value(<<"Unique-ID">>, Props),
    #channel{uuid=UUID
             ,destination=props:get_value(<<"Caller-Destination-Number">>, Props)
             ,direction=props:get_value(<<"Call-Direction">>, Props)
             ,account_id=props:get_value(?GET_CCV(<<"Account-ID">>), Props)
             ,account_billing=props:get_value(?GET_CCV(<<"Account-Billing">>), Props)
             ,authorizing_id=props:get_value(?GET_CCV(<<"Authorizing-ID">>), Props)
             ,authorizing_type=props:get_value(?GET_CCV(<<"Authorizing-Type">>), Props)
             ,owner_id=props:get_value(?GET_CCV(<<"Owner-ID">>), Props)
             ,resource_id=props:get_value(?GET_CCV(<<"Resource-ID">>), Props)
             ,presence_id=props:get_value(?GET_CCV(<<"Channel-Presence-ID">>), Props
                                          ,props:get_value(<<"variable_presence_id">>, Props))
             ,fetch_id=props:get_value(?GET_CCV(<<"Fetch-ID">>), Props)
             ,bridge_id=props:get_value(?GET_CCV(<<"Bridge-ID">>), Props, UUID)
             ,reseller_id=props:get_value(?GET_CCV(<<"Reseller-ID">>), Props)
             ,reseller_billing=props:get_value(?GET_CCV(<<"Reseller-Billing">>), Props)
             ,precedence=wh_util:to_integer(props:get_value(?GET_CCV(<<"Precedence">>), Props, 5))
             ,realm=props:get_value(?GET_CCV(<<"Realm">>), Props
                                    ,props:get_value(<<"variable_domain_name">>, Props))
             ,username=props:get_value(?GET_CCV(<<"Username">>), Props
                                       ,props:get_value(<<"variable_user_name">>, Props))
             ,import_moh=props:get_value(<<"variable_hold_music">>, Props) =:= 'undefined'
             ,answered=props:get_value(<<"Answer-State">>, Props) =:= <<"answered">>
             ,node=Node
             ,timestamp=wh_util:current_tstamp()
             ,profile=props:get_value(<<"variable_sofia_profile_name">>, Props, ?DEFAULT_FS_PROFILE)
             ,context=props:get_value(<<"Caller-Context">>, Props, ?WHISTLE_CONTEXT)
             ,dialplan=props:get_value(<<"Caller-Dialplan">>, Props, ?DEFAULT_FS_DIALPLAN)
             ,other_leg=get_other_leg(props:get_value(<<"Unique-ID">>, Props), Props)
            }.

props_to_update(Props) ->
    UUID = props:get_value(<<"Unique-ID">>, Props),
    props:filter_undefined([{#channel.destination, props:get_value(<<"Caller-Destination-Number">>, Props)}
                            ,{#channel.direction, props:get_value(<<"Call-Direction">>, Props)}
                            ,{#channel.account_id, props:get_value(?GET_CCV(<<"Account-ID">>), Props)}
                            ,{#channel.account_billing, props:get_value(?GET_CCV(<<"Account-Billing">>), Props)}
                            ,{#channel.authorizing_id, props:get_value(?GET_CCV(<<"Authorizing-ID">>), Props)}
                            ,{#channel.authorizing_type, props:get_value(?GET_CCV(<<"Authorizing-Type">>), Props)}
                            ,{#channel.owner_id, props:get_value(?GET_CCV(<<"Owner-ID">>), Props)}
                            ,{#channel.resource_id, props:get_value(?GET_CCV(<<"Resource-ID">>), Props)}
                            ,{#channel.presence_id, props:get_value(?GET_CCV(<<"Channel-Presence-ID">>), Props
                                                                   ,props:get_value(<<"variable_presence_id">>, Props))}
                            ,{#channel.fetch_id, props:get_value(?GET_CCV(<<"Fetch-ID">>), Props)}
                            ,{#channel.bridge_id, props:get_value(?GET_CCV(<<"Bridge-ID">>), Props, UUID)}
                            ,{#channel.reseller_id, props:get_value(?GET_CCV(<<"Reseller-ID">>), Props)}
                            ,{#channel.reseller_billing, props:get_value(?GET_CCV(<<"Reseller-Billing">>), Props)}
                            ,{#channel.precedence, wh_util:to_integer(props:get_value(?GET_CCV(<<"Precedence">>), Props, 5))}
                            ,{#channel.realm, props:get_value(?GET_CCV(<<"Realm">>), Props
                                                             ,props:get_value(<<"variable_domain_name">>, Props))}
                            ,{#channel.username, props:get_value(?GET_CCV(<<"Username">>), Props
                                                                ,props:get_value(<<"variable_user_name">>, Props))}
                            ,{#channel.import_moh, props:get_value(<<"variable_hold_music">>, Props) =:= 'undefined'}
                            ,{#channel.answered, props:get_value(<<"Answer-State">>, Props) =:= <<"answered">>}
                            ,{#channel.profile, props:get_value(<<"variable_sofia_profile_name">>, Props)}
                            ,{#channel.context, props:get_value(<<"Caller-Context">>, Props)}
                            ,{#channel.dialplan, props:get_value(<<"Caller-Dialplan">>, Props)}
                           ]).

-spec channel_status_resp(ne_binary(), wh_json:object(), wh_json:object()) -> 'ok'.
channel_status_resp(CallId, Channel, JObj) ->
    Node = wh_json:get_binary_value(<<"node">>, Channel),
    [_, Hostname] = binary:split(Node, <<"@">>),
    lager:debug("channel is on ~s", [Hostname]),
    Resp = [{<<"Call-ID">>, CallId}
            ,{<<"Status">>, <<"active">>}
            ,{<<"Switch-Hostname">>, Hostname}
            ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_call:publish_channel_status_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

get_other_leg(UUID, Props) ->
    get_other_leg(UUID, Props, props:get_value(<<"Other-Leg-Unique-ID">>, Props)).

get_other_leg(UUID, Props, 'undefined') ->
    maybe_other_bridge_leg(UUID
                           ,props:get_value(<<"Bridge-A-Unique-ID">>, Props)
                           ,props:get_value(<<"Bridge-B-Unique-ID">>, Props)
                          );
get_other_leg(_UUID, _Props, OtherLeg) -> OtherLeg.

maybe_other_bridge_leg(UUID, UUID, OtherLeg) -> OtherLeg;
maybe_other_bridge_leg(UUID, OtherLeg, UUID) -> OtherLeg;
maybe_other_bridge_leg(_, _, _) -> 'undefined'.
