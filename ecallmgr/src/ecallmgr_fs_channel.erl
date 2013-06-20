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

-export([show_all/0
         ,new/2
         ,node/1, set_node/2
         ,former_node/1
         ,move/3
         ,is_bridged/1, set_bridge/2
         ,account_summary/1
         ,match_presence/1
         ,exists/1
         ,import_moh/1
         ,set_account_id/2
         ,set_billing_id/2
         ,set_account_billing/2
         ,set_reseller_id/2
         ,set_reseller_billing/2
         ,set_authorizing_id/2
         ,set_resource_id/2
         ,set_authorizing_type/2
         ,set_owner_id/2
         ,set_presence_id/2
         ,set_precedence/2
         ,set_answered/2
         ,set_import_moh/2
         ,get_call_precedence/1
         ,fetch/1
         ,destroy/2
         ,props_to_record/2
         ,record_to_json/1
        ]).

-compile([{'no_auto_import', [node/1]}]).

-include("ecallmgr.hrl").

-define(NODES_SRV, 'ecallmgr_fs_nodes').

-spec show_all() -> wh_json:objects().
show_all() ->
    ets:foldl(fun(Channel, Acc) ->
                      [record_to_json(Channel) | Acc]
              end, [], ?CHANNELS_TBL).

-spec new(wh_proplist(), atom()) -> 'ok'.
new(Props, Node) ->
    CallId = props:get_value(<<"Unique-ID">>, Props),
    put(callid, CallId),
    gen_server:cast(?NODES_SRV, {'new_channel', props_to_record(Props, Node)}).

-spec fetch(ne_binary()) ->
                   {'ok', wh_json:object()} |
                   {'error', 'not_found'}.
fetch(UUID) ->
    case ets:lookup(?CHANNELS_TBL, UUID) of
        [Channel] -> {'ok', record_to_json(Channel)};
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

-spec exists(ne_binary()) -> boolean().
exists(UUID) -> ets:member(?CHANNELS_TBL, UUID).

-spec import_moh(ne_binary()) -> boolean().
import_moh(UUID) ->
    try ets:lookup_element(?CHANNELS_TBL, UUID, #channel.import_moh) of
        Import -> Import
    catch
        'error':'badarg' -> 'false'
    end.

-spec account_summary(ne_binary()) -> channels().
account_summary(AccountId) ->
    MatchSpec = [{#channel{direction = '$1', account_id = '$2', account_billing = '$7'
                           ,authorizing_id = '$3', resource_id = '$4', billing_id = '$5'
                           ,bridge_id = '$6',  _ = '_'
                          }
                  ,[{'=:=', '$2', {'const', AccountId}}]
                  ,['$_']}
                ],
    ets:select(?CHANNELS_TBL, MatchSpec).

-spec match_presence(ne_binary()) -> wh_proplist_kv(ne_binary(), atom()).
match_presence(PresenceId) ->
    MatchSpec = [{#channel{uuid = '$1', presence_id = '$2', node = '$3',  _ = '_'}
                  ,[{'=:=', '$2', {'const', PresenceId}}]
                  ,[{{'$1', '$3'}}]}
                ],
    ets:select(?CHANNELS_TBL, MatchSpec).

move(UUID, ONode, NNode) ->
    OriginalNode = wh_util:to_atom(ONode),
    NewNode = wh_util:to_atom(NNode),

    set_node(NewNode, UUID),
    ecallmgr_call_events:shutdown(OriginalNode, UUID),
    ecallmgr_call_control:update_node(NewNode, UUID),

    lager:debug("updated ~s to point to ~s", [UUID, NewNode]),

    case teardown_sbd(UUID, OriginalNode) of
        'true' ->
            lager:debug("sbd teardown of ~s on ~s", [UUID, OriginalNode]),
            resume(UUID, NewNode);
        'false' ->
            lager:debug("failed to teardown ~s on ~s", [UUID, OriginalNode]),
            'false'
    end.

%% listens for the event from FS with the XML
-spec resume(ne_binary(), atom()) -> boolean().
-spec resume(ne_binary(), atom(), wh_proplist()) -> boolean().
resume(UUID, NewNode) ->
    catch gproc:reg({'p', 'l', ?CHANNEL_MOVE_REG(NewNode, UUID)}),
    lager:debug("waiting for message with metadata for channel ~s so we can move it to ~s", [UUID, NewNode]),
    receive
        ?CHANNEL_MOVE_RELEASED_MSG(_Node, UUID, Evt) ->
            lager:debug("channel has been released from former node: ~s", [_Node]),
            case resume(UUID, NewNode, Evt) of
                'true' -> wait_for_completion(UUID, NewNode);
                'false' -> 'false'
            end
    after 5000 ->
            lager:debug("timed out waiting for channel to be released"),
            'false'
    end.

resume(UUID, NewNode, Evt) ->
    Meta = fix_metadata(props:get_value(<<"metadata">>, Evt)),

    case freeswitch:sendevent_custom(NewNode, ?CHANNEL_MOVE_REQUEST_EVENT
                                     ,[{"profile_name", wh_util:to_list(?DEFAULT_FS_PROFILE)}
                                       ,{"channel_id", wh_util:to_list(UUID)}
                                       ,{"metadata", wh_util:to_list(Meta)}
                                       ,{"technology", wh_util:to_list(props:get_value(<<"technology">>, Evt, <<"sofia">>))}
                                      ]) of
        'ok' ->
            lager:debug("sent channel_move::move_request with metadata to ~s for ~s", [NewNode, UUID]),
            'true';
        {'error', _E} ->
            lager:debug("failed to send custom event channel_move::move_request: ~p", [_E]),
            'false';
        'timeout' ->
            lager:debug("timed out sending custom event channel_move::move_request"),
            'false'
    end.

%% We receive un-escaped < and > in the SIP URIs in this data
%% which causes the XML to not be parsable, either in Erlang or
%% by FreeSWITCH's parser. Things like:
%% <sip_uri><sip:user@realm:port>;tag=abc</sip_uri>
%% So this is an awesome search/replace list to convert the '<sip:'
%% and its corresponding '>' to %3C and %3E as they should be
fix_metadata(Meta) ->
    Replacements = [
                    {<<"\<sip\:">>, <<"%3Csip:">>}
                    ,{<<"\>\<sip">>, <<"%3E<sip">>}
                    ,{<<"\>;">>, <<"%3E;">>} % this is especially nice :)
                    %% until such time as FS sets these properly
                    ,{<<"<dialplan></dialplan>">>, <<"<dialplan>XML</dialplan>">>}
                    ,{<<"<context>default</context>">>, <<"<context>context_2</context>">>}
                   ],
    lists:foldl(fun({S, R}, MetaAcc) ->
                        iolist_to_binary(re:replace(MetaAcc, S, R, ['global']))
                end, Meta, Replacements).

wait_for_completion(UUID, NewNode) ->
    lager:debug("waiting for confirmation from ~s of move", [NewNode]),
    receive
        ?CHANNEL_MOVE_COMPLETE_MSG(_Node, UUID, _Evt) ->
            lager:debug("confirmation of move received for ~s, success!", [_Node]),
            _ = ecallmgr_call_sup:start_event_process(NewNode, UUID),
            'true'
    after 5000 ->
            lager:debug("timed out waiting for move to complete"),
            'false'
    end.

teardown_sbd(UUID, OriginalNode) ->
    catch gproc:reg({'p', 'l', ?CHANNEL_MOVE_REG(OriginalNode, UUID)}),

    case freeswitch:sendevent_custom(OriginalNode, ?CHANNEL_MOVE_REQUEST_EVENT
                                     ,[{"profile_name", wh_util:to_list(?DEFAULT_FS_PROFILE)}
                                       ,{"channel_id", wh_util:to_list(UUID)}
                                       ,{"technology", ?DEFAULT_FS_TECHNOLOGY}
                                      ])
    of
        'ok' ->
            lager:debug("sent channel_move::move_request to ~s for ~s", [OriginalNode, UUID]),
            'true';
        {'error', _E} ->
            lager:debug("failed to send custom event channel_move::move_request: ~p", [_E]),
            'false';
        'timeout' ->
            lager:debug("timed out sending custom event channel_move::move_request"),
            'false'
    end.

-spec set_account_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_account_id(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.account_id, Value}});
set_account_id(UUID, Value) ->
    set_account_id(UUID, wh_util:to_binary(Value)).

-spec set_billing_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_billing_id(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.billing_id, Value}});
set_billing_id(UUID, Value) ->
    set_billing_id(UUID, wh_util:to_binary(Value)).

-spec set_account_billing(ne_binary(), string() | ne_binary()) -> 'ok'.
set_account_billing(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.account_billing, Value}});
set_account_billing(UUID, Value) ->
    set_account_billing(UUID, wh_util:to_binary(Value)).

-spec set_reseller_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_reseller_id(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.reseller_id, Value}});
set_reseller_id(UUID, Value) ->
    set_reseller_id(UUID, wh_util:to_binary(Value)).

-spec set_reseller_billing(ne_binary(), string() | ne_binary()) -> 'ok'.
set_reseller_billing(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.reseller_billing, Value}});
set_reseller_billing(UUID, Value) ->
    set_reseller_billing(UUID, wh_util:to_binary(Value)).

-spec set_resource_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_resource_id(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.resource_id, Value}});
set_resource_id(UUID, Value) ->
    set_resource_id(UUID, wh_util:to_binary(Value)).

-spec set_authorizing_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_authorizing_id(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.authorizing_id, Value}});
set_authorizing_id(UUID, Value) ->
    set_authorizing_id(UUID, wh_util:to_binary(Value)).

-spec set_authorizing_type(ne_binary(), string() | ne_binary()) -> 'ok'.
set_authorizing_type(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.authorizing_type, Value}});
set_authorizing_type(UUID, Value) ->
    set_authorizing_type(UUID, wh_util:to_binary(Value)).

-spec set_owner_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_owner_id(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.owner_id, Value}});
set_owner_id(UUID, Value) ->
    set_owner_id(UUID, wh_util:to_binary(Value)).

-spec set_presence_id(ne_binary(), string() | ne_binary()) -> 'ok'.
set_presence_id(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.presence_id, Value}});
set_presence_id(UUID, Value) ->
    set_presence_id(UUID, wh_util:to_binary(Value)).

-spec set_precedence(ne_binary(), string() | ne_binary() | integer()) -> 'ok'.
set_precedence(UUID, Value) when is_integer(Value) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.precedence, Value}});
set_precedence(UUID, Value) ->
    set_precedence(UUID, wh_util:to_integer(Value)).

-spec set_answered(ne_binary(), boolean()) -> 'ok'.
set_answered(UUID, Answered) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.answered, (not wh_util:is_empty(Answered))}}).

-spec set_bridge(ne_binary(), api_binary()) -> 'ok'.
set_bridge(UUID, OtherUUID) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.other_leg, OtherUUID}}).

-spec set_import_moh(ne_binary(), boolean()) -> 'ok'.
set_import_moh(UUID, Import) ->
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, {#channel.import_moh, Import}}).

-spec get_call_precedence(ne_binary()) -> integer().
get_call_precedence(UUID) ->
    MatchSpec = [{#channel{uuid = '$1', precedence = '$2', _ = '_'}
                  ,[{'=:=', '$1', {const, UUID}}]
                  ,['$2']}
                ],
    case ets:select(?CHANNELS_TBL, MatchSpec) of
        [Presedence] -> Presedence;
        _ -> 5
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
    gen_server:cast(?NODES_SRV, {'channel_update', UUID, Updates}).

-spec destroy(wh_proplist(), atom()) -> 'ok'.
destroy(Props, Node) ->
    UUID = props:get_value(<<"Unique-ID">>, Props),
    gen_server:cast(?NODES_SRV, {'destroy_channel', UUID, Node}).

-spec props_to_record(wh_proplist(), atom()) -> channel().
props_to_record(Props, Node) ->
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
             ,billing_id=props:get_value(?GET_CCV(<<"Billing-ID">>), Props)
             ,bridge_id=props:get_value(?GET_CCV(<<"Bridge-ID">>), Props)
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
            }.

-spec record_to_json(channel()) -> wh_json:object().
record_to_json(Channel) ->
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
                       ,{<<"billing_id">>, Channel#channel.billing_id}
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
                      ]).
