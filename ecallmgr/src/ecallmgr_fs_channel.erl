%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_channel).

-compile([{'no_auto_import', [node/1]}]).

-behaviour(gen_server).

-export([start_link/1
         ,start_link/2
        ]).
-export([exists/1]).
-export([fetch/1]).
-export([node/1
         ,set_node/2
         ,former_node/1
        ]).
-export([is_bridged/1]).
-export([import_moh/1]).
-export([set_account_id/2
         ,set_account_billing/2
         ,set_reseller_id/2
         ,set_reseller_billing/2
         ,set_authorizing_id/2
         ,set_resource_id/2
         ,set_authorizing_type/2
         ,set_owner_id/2
         ,set_presence_id/2
         ,set_answered/2
         ,set_import_moh/2
         ,set_bridge/2
        ]).
-export([renew/2]).
-export([from_props/2]).
-export([to_json/1]).
-export([process_event/3]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-record(state, {node :: atom()
                ,options = [] :: wh_proplist()
               }).
-type state() :: #state{}.

-define(FS_BINDINGS, ['CHANNEL_CREATE', 'CHANNEL_DESTROY'
                      ,'CHANNEL_ANSWER', 'CHANNEL_BRIDGE'
                      ,'CHANNEL_UNBRIDGE', 'CHANNEL_EXECUTE_COMPLETE'
                     ]).

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
-spec start_link(atom()) -> {'ok', pid()} | {'error', term()}.
-spec start_link(atom(), proplist()) -> {'ok', pid()} | {'error', term()}.

start_link(Node) ->
    start_link(Node, []).
start_link(Node, Options) ->
    gen_server:start_link(?MODULE, [Node, Options], []).

-spec exists(ne_binary()) -> boolean().
exists(UUID) -> ets:member(?CHANNELS_TBL, UUID).

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
    lager:debug("updaters: ~p", [Updates]),
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

-spec set_account_billing(ne_binary(), string() | ne_binary()) -> 'ok'.
set_account_billing(UUID, Value) when is_binary(Value) ->
    ecallmgr_fs_channels:update(UUID, #channel.account_billing, Value);
set_account_billing(UUID, Value) ->
    set_account_billing(UUID, wh_util:to_binary(Value)).

-spec set_reseller_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_reseller_id(UUID, Value) when is_binary(Value) ->
    ecallmgr_fs_channels:update(UUID, #channel.reseller_id, Value);
set_reseller_id(UUID, Value) ->
    set_reseller_id(UUID, wh_util:to_binary(Value)).

-spec set_reseller_billing(ne_binary(), string() | ne_binary()) -> 'ok'.
set_reseller_billing(UUID, Value) when is_binary(Value) ->
    ecallmgr_fs_channels:update(UUID, #channel.reseller_billing, Value);
set_reseller_billing(UUID, Value) ->
    set_reseller_billing(UUID, wh_util:to_binary(Value)).

-spec set_resource_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_resource_id(UUID, Value) when is_binary(Value) ->
    ecallmgr_fs_channels:update(UUID, #channel.resource_id, Value);
set_resource_id(UUID, Value) ->
    set_resource_id(UUID, wh_util:to_binary(Value)).

-spec set_authorizing_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_authorizing_id(UUID, Value) when is_binary(Value) ->
    ecallmgr_fs_channels:update(UUID, #channel.authorizing_id, Value);
set_authorizing_id(UUID, Value) ->
    set_authorizing_id(UUID, wh_util:to_binary(Value)).

-spec set_authorizing_type(ne_binary(), string() | ne_binary()) -> 'ok'.
set_authorizing_type(UUID, Value) when is_binary(Value) ->
    ecallmgr_fs_channels:update(UUID, #channel.authorizing_type, Value);
set_authorizing_type(UUID, Value) ->
    set_authorizing_type(UUID, wh_util:to_binary(Value)).

-spec set_owner_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_owner_id(UUID, Value) when is_binary(Value) ->
    ecallmgr_fs_channels:update(UUID, #channel.owner_id, Value);
set_owner_id(UUID, Value) ->
    set_owner_id(UUID, wh_util:to_binary(Value)).

-spec set_presence_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_presence_id(UUID, Value) when is_binary(Value) ->
    ecallmgr_fs_channels:update(UUID, #channel.presence_id, Value);
set_presence_id(UUID, Value) ->
    set_presence_id(UUID, wh_util:to_binary(Value)).

-spec set_answered(ne_binary(), boolean()) -> 'ok'.
set_answered(UUID, Answered) when is_boolean(Answered) ->
    ecallmgr_fs_channels:update(UUID, #channel.answered, Answered);
set_answered(UUID, Answered) ->
    set_answered(UUID, wh_util:is_true(Answered)).

-spec set_bridge(ne_binary(), api_binary()) -> 'ok'.
set_bridge(UUID, OtherUUID) ->
    ecallmgr_fs_channels:update(UUID, #channel.other_leg, OtherUUID).

-spec set_import_moh(ne_binary(), boolean()) -> 'ok'.
set_import_moh(UUID, Import) when is_boolean(Import) ->
    ecallmgr_fs_channels:update(UUID, #channel.import_moh, Import);
set_import_moh(UUID, Import) ->
    set_import_moh(UUID, wh_util:is_true(Import)).

-spec renew(atom(), ne_binary()) ->
                   {'ok', channel()} |
                   {'error', 'timeout' | 'badarg'}.
renew(Node, UUID) ->
    case freeswitch:api(Node, 'uuid_dump', wh_util:to_list(UUID)) of
        {'ok', Dump} ->
            Props = ecallmgr_util:eventstr_to_proplist(Dump),
            {'ok', from_props(Props, Node)};
        {'error', _}=E -> E;
        'timeout' -> {'error', 'timeout'}
    end.

-spec from_props(wh_proplist(), atom()) -> channel().
from_props(Props, Node) ->
    #channel{uuid=props:get_value(<<"Unique-ID">>, Props)
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
             ,bridge_id=props:get_value(?GET_CCV(<<"Bridge-ID">>), Props)
             ,reseller_id=props:get_value(?GET_CCV(<<"Reseller-ID">>), Props)
             ,reseller_billing=props:get_value(?GET_CCV(<<"Reseller-Billing">>), Props)
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
            }.

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
                       ,{<<"bridge_id">>, Channel#channel.bridge_id}
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
                      ]).

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
    lager:info("starting new channel listener for ~s", [Node]),
    case bind_to_events(props:get_value('client_version', Options), Node) of
        {'error', Reason} ->
            lager:critical("unable to establish node bindings: ~p", [Reason]),
            {'stop', Reason};
        ok ->
            lager:debug("bound to switch events on node ~s", [Node]),
            {'ok', #state{node=Node, options=Options}}
    end.

bind_to_events(<<"mod_kazoo", _/binary>>, Node) ->
    case freeswitch:event(Node, ?FS_BINDINGS) of
        'timeout' -> {'error', 'timeout'};
        Else -> Else
    end;
bind_to_events(_, Node) ->
    _ = [gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, Binding)})
         || Binding <- ?FS_BINDINGS
        ],
    'ok'.

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
-spec handle_call('node', _, state()) -> {'reply', atom(), state()}.
handle_call('node', _From, #state{node=Node}=State) ->
    {'reply', Node, State}.

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
-spec handle_cast(term(), state()) -> {'noreply', state()}.
handle_cast(_Req, State) ->
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
handle_info({'event', [UUID | Data]}, #state{node=Node}=State) ->
    _ = spawn(?MODULE, 'process_event', [UUID, Data, Node]),
    {'noreply', State};
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
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
terminate(_Reason, #state{node=Node}) ->
    lager:info("node channel listener for ~s terminating: ~p", [Node, _Reason]).

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
-spec process_event(api_binary(), wh_proplist(), atom()) -> 'ok'.
process_event(UUID, Props, Node) ->
    EventName = props:get_value(<<"Event-Subclass">>, Props, props:get_value(<<"Event-Name">>, Props)),
    process_event(EventName, UUID, Props, Node).

-spec process_event(ne_binary(), api_binary(), wh_proplist(), atom()) -> 'ok'.
process_event(<<"CHANNEL_CREATE">>, UUID, Props, Node) ->
    ecallmgr_fs_channels:new(from_props(Props, Node)),
    ecallmgr_fs_authz:handle_channel_create(UUID, Props, Node);
process_event(<<"CHANNEL_DESTROY">>, _, Props, Node) ->
    ecallmgr_fs_channels:destroy(from_props(Props, Node));
process_event(<<"CHANNEL_ANSWER">>, UUID, _, _) ->
    set_answered(UUID, 'true');
process_event(<<"CHANNEL_BRIDGE">>, UUID, Props, _) ->
    OtherLeg = get_other_leg(UUID, Props),
    _ = set_bridge(UUID, OtherLeg),
    set_bridge(OtherLeg, UUID);
process_event(<<"CHANNEL_UNBRIDGE">>, UUID, Props, _) ->
    OtherLeg = get_other_leg(UUID, Props),
    _ = set_bridge(UUID, 'undefined'),
    set_bridge(OtherLeg, 'undefined');
process_event(<<"CHANNEL_EXECUTE_COMPLETE">>, UUID, Props, _) ->
    Data = props:get_value(<<"Application-Data">>, Props),
    case props:get_value(<<"Application">>, Props) of
        <<"set">> -> process_channel_update(UUID, Data);
        <<"export">> -> process_channel_update(UUID, Data);
        <<"multiset">> -> process_channel_multiset(UUID, Data);
        _Else -> 'ok'
    end.

-spec process_channel_multiset(ne_binary(), ne_binary()) -> any().
process_channel_multiset(UUID, Datas) ->
    [process_channel_update(UUID, Data)
     || Data <- binary:split(Datas, <<"|">>, ['global'])
    ].

-spec process_channel_update(ne_binary(), ne_binary()) -> any().
process_channel_update(UUID, Data) ->
    case binary:split(Data, <<"=">>) of
        [Var, Value] -> process_channel_update(UUID, Var, Value);
        _Else -> 'ok'
    end.

-spec process_channel_update(ne_binary(), ne_binary(), ne_binary()) -> any().
process_channel_update(UUID, <<"ecallmgr_", Var/binary>>, Value) ->
    Normalized = wh_util:to_lower_binary(binary:replace(Var, <<"-">>, <<"_">> , ['global'])),
    process_channel_update(UUID, Normalized, Value);
process_channel_update(UUID, <<"hold_music">>, _) ->
    set_import_moh(UUID, 'false');
process_channel_update(UUID, Var, Value) ->
    try wh_util:to_atom(<<"set_", Var/binary>>) of
        Function ->
            Exports = ?MODULE:module_info('exports'),
            case lists:keysearch(Function, 1, Exports) of
                {'value', {_, 2}} -> ?MODULE:Function(UUID, Value);
                _Else -> 'ok'
            end
    catch
        _:_ -> 'ok'
    end.

-spec get_other_leg(ne_binary(), wh_proplist()) -> ne_binary().
get_other_leg(UUID, Props) ->
    get_other_leg(UUID, Props, props:get_value(<<"Other-Leg-Unique-ID">>, Props)).

get_other_leg(UUID, Props, 'undefined') ->
    maybe_other_bridge_leg(UUID
                           ,props:get_value(<<"Bridge-A-Unique-ID">>, Props)
                           ,props:get_value(<<"Bridge-B-Unique-ID">>, Props)
                          );
get_other_leg(_UUID, _Props, OtherLeg) -> OtherLeg.

-spec maybe_other_bridge_leg(ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
maybe_other_bridge_leg(UUID, UUID, OtherLeg) -> OtherLeg;
maybe_other_bridge_leg(UUID, OtherLeg, UUID) -> OtherLeg.


