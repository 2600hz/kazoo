%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%% Track the FreeSWITCH channel information, and provide accessors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_channel_stream).

-export([init/0]).

-export([channel_create/1
        ,channel_destroy/1
        ,channel_answer/1
        ,channel_bridge/1
        ,channel_unbridge/1
        ,channel_data/1
        ]).

-include("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_CREATE">>, ?MODULE, 'channel_create'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_DESTROY">>, ?MODULE, 'channel_destroy'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_ANSWER">>, ?MODULE, 'channel_answer'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_BRIDGE">>, ?MODULE, 'channel_bridge'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_UNBRIDGE">>, ?MODULE, 'channel_unbridge'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_DATA">>, ?MODULE, 'channel_data'),
    'ok'.

-spec channel_create(tuple()) -> any().
channel_create({Node, UUID, _Category, _Event, JObj}) ->
    maybe_authorize_channel(Node, UUID, JObj),
    ecallmgr_fs_channels:new(jobj_to_record(Node, UUID, JObj)).

-spec channel_destroy(tuple()) -> any().
channel_destroy({Node, UUID, _Category, _Event, _JObj}) ->
    _ = ecallmgr_fs_channels:destroy(UUID, Node).

-spec channel_answer(tuple()) -> any().
channel_answer({_Node, UUID, _Category, _Event, _JObj}) ->
    _ = ecallmgr_fs_channels:update(UUID, #channel.answered, 'true').

-spec channel_data(tuple()) -> any().
channel_data({Node, UUID, _Category, _Event, JObj}) ->
    ecallmgr_fs_channels:updates(UUID, jobj_to_update(Node, UUID, JObj)).

-spec channel_bridge(tuple()) -> any().
channel_bridge({_Node, UUID, _Category, _Event, JObj}) ->
    OtherLeg = kz_json:get_ne_binary_value(<<"Bridge-B-Unique-ID">>, JObj),
    ecallmgr_fs_channels:updates(UUID, props:filter_undefined(
                                         [{#channel.other_leg, OtherLeg}
%%                                          | update_callee(UUID, Props)
                                         ]
                                        )
                                ),
    ecallmgr_fs_channels:update(OtherLeg, #channel.other_leg, UUID).

-spec channel_unbridge(tuple()) -> any().
channel_unbridge({_Node, UUID, _Category, _Event, JObj}) ->
    OtherLeg = kz_json:get_ne_binary_value(<<"Bridge-B-Unique-ID">>, JObj),
    ecallmgr_fs_channels:update(UUID, #channel.other_leg, 'undefined'),
    ecallmgr_fs_channels:update(OtherLeg, #channel.other_leg, 'undefined').


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec jobj_to_update(atom(), ne_binary(), kz_json:object()) -> channel_updates().
jobj_to_update(_Node, _UUID, _JObj) -> [].

-spec jobj_to_record(atom(), ne_binary(), kz_json:object()) -> channel().
jobj_to_record(Node, UUID, JObj) ->
    CCVs = kz_json:to_proplist(kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new())),
    CAVs = kz_json:to_proplist(kz_json:get_json_value(<<"Custom-Application-Vars">>, JObj, kz_json:new())),
    OtherLeg = kz_json:get_ne_binary_value(<<"Other-Leg-Call-ID">>, JObj),
    #channel{uuid=UUID
            ,destination=kz_json:get_value(<<"Caller-Destination-Number">>, JObj)
            ,direction=kz_json:get_value(<<"Call-Direction">>, JObj)
            
            ,account_id=props:get_value(<<"Account-ID">>, CCVs)
            ,account_billing=props:get_value(<<"Account-Billing">>, CCVs)
            ,authorizing_id=props:get_value(<<"Authorizing-ID">>, CCVs)
            ,authorizing_type=props:get_value(<<"Authorizing-Type">>, CCVs)
            ,is_authorized=props:is_true(<<"Channel-Authorized">>, CCVs)
            ,owner_id=props:get_value(<<"Owner-ID">>, CCVs)
            ,resource_id=props:get_value(<<"Resource-ID">>, CCVs)
            ,fetch_id=props:get_value(<<"Fetch-ID">>, CCVs)
            ,bridge_id=props:get_value(<<"Bridge-ID">>, CCVs, UUID)
            ,reseller_id=props:get_value(<<"Reseller-ID">>, CCVs)
            ,reseller_billing=props:get_value(<<"Reseller-Billing">>, CCVs)
            ,precedence=kz_term:to_integer(props:get_value(<<"Precedence">>, CCVs, 5))
            
            ,presence_id=kz_json:get_value(<<"Presence-ID">>, JObj)
            ,realm=props:get_value(<<"Realm">>, CCVs)
            ,username=props:get_value(<<"Username">>, CCVs)
            
            ,answered=kz_json:get_value(<<"Answer-State">>, JObj) =:= <<"answered">>
            ,node=Node
            ,timestamp=kz_time:current_tstamp()

            ,profile=kz_json:get_value(<<"Caller-Profile">>, JObj, ?DEFAULT_FS_PROFILE)
            ,context=kz_json:get_value(<<"Caller-Context">>, JObj, ?DEFAULT_FREESWITCH_CONTEXT)
            ,dialplan=kz_json:get_value(<<"Caller-Dialplan">>, JObj, ?DEFAULT_FS_DIALPLAN)
            
            ,other_leg=OtherLeg
            ,handling_locally=handling_locally(props:get_value(<<"Ecallmgr-Node">>, CCVs), OtherLeg)
            
            ,to_tag=kz_json:get_value(<<"To-Tag">>, JObj)
            ,from_tag=kz_json:get_value(<<"From-Tag">>, JObj)
            
            ,interaction_id=props:get_value(<<?CALL_INTERACTION_ID>>, CCVs)
            
            ,is_loopback=kz_json:is_true(<<"Channel-Is-Loopback">>, JObj)
            ,loopback_leg_name=kz_json:get_value(<<"Channel-Loopback-Leg">>, JObj)
            ,loopback_other_leg=kz_json:get_value(<<"Channel-Loopback-Other-Leg-ID">>, JObj)
            
            ,callflow_id=props:get_value(<<"CallFlow-ID">>, CCVs)
            ,cavs=CAVs
            }.


-spec other_leg_handling_locally(ne_binary()) -> boolean().
other_leg_handling_locally(OtherLeg) ->
    case ecallmgr_fs_channel:fetch(OtherLeg, 'record') of
        {'ok', #channel{handling_locally=HandleLocally}} -> HandleLocally;
        _ -> 'false'
    end.

-spec handling_locally(api_binary(), api_binary()) -> boolean().
handling_locally('undefined', 'undefined') -> 'false';
handling_locally(Node, 'undefined') ->
    Node =:= kz_term:to_binary(node());
handling_locally(Node, OtherLeg) ->
    case kz_term:to_binary(node()) of
        Node -> 'true';
        _ -> other_leg_handling_locally(OtherLeg)
    end.

maybe_authorize_channel(Node, UUID, JObj) ->
    case kz_json:get_value([<<"Custom-Channel-Vars-X">>, <<"Ecallmgr-Node">>], JObj) =:= kz_term:to_binary(node()) of
        'true' -> authorize_channel(Node, UUID, JObj);
        'false' -> 'ok'
    end.

authorize_channel(Node, UUID, JObj) ->
    case ecallmgr_fs_authz:authorize(JObj, UUID, Node) of
        {'true', CCVs} -> ecallmgr_fs_command:set(Node, UUID, kz_json:to_proplist(CCVs));
        Authz -> Authz
    end.
