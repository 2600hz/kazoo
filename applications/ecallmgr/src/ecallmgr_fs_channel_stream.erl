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
        ,channel_sync/1
        ,channel_hold/1, channel_unhold/1
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
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_HOLD">>, ?MODULE, 'channel_hold'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_UNHOLD">>, ?MODULE, 'channel_unhold'),
    kazoo_bindings:bind(<<"event_stream.process.call_event.CHANNEL_SYNC">>, ?MODULE, 'channel_sync'),
    'ok'.

-spec channel_create(map()) -> any().
channel_create(#{node := Node, call_id := UUID, payload := JObj}) ->
    maybe_authorize_channel(Node, UUID, JObj),
    ecallmgr_fs_channels:new(jobj_to_record(Node, UUID, JObj)).

-spec channel_destroy(map()) -> any().
channel_destroy(#{node := Node, call_id := UUID}) ->
    _ = ecallmgr_fs_channels:destroy(UUID, Node).

-spec channel_answer(map()) -> any().
channel_answer(#{call_id := UUID}) ->
    _ = ecallmgr_fs_channels:update(UUID, #channel.answered, 'true').

-spec channel_data(map()) -> any().
channel_data(#{node := Node, call_id := UUID, payload := JObj}) ->
    ecallmgr_fs_channels:updates(UUID, jobj_to_update(Node, UUID, JObj)).

-spec channel_sync(map()) -> any().
channel_sync(#{node := Node, call_id := UUID, payload := JObj}) ->
    ecallmgr_fs_channels:updates(UUID, jobj_to_update(Node, UUID, JObj)).

-spec channel_bridge(map()) -> any().
channel_bridge(#{call_id := UUID, payload := JObj}) ->
    OtherLeg = kz_json:get_ne_binary_value(<<"Bridge-B-Unique-ID">>, JObj),
    ecallmgr_fs_channels:updates(UUID, props:filter_undefined(
                                         [{#channel.other_leg, OtherLeg}
                                          %%                                          | update_callee(UUID, Props)
                                         ]
                                        )
                                ),
    ecallmgr_fs_channels:update(OtherLeg, #channel.other_leg, UUID).

-spec channel_unbridge(map()) -> any().
channel_unbridge(#{call_id := UUID, payload := JObj}) ->
    OtherLeg = kz_json:get_ne_binary_value(<<"Bridge-B-Unique-ID">>, JObj),
    ecallmgr_fs_channels:update(UUID, #channel.other_leg, 'undefined'),
    ecallmgr_fs_channels:update(OtherLeg, #channel.other_leg, 'undefined').

-spec channel_hold(map()) -> any().
channel_hold(#{call_id := UUID}) ->
    ecallmgr_fs_channels:update(UUID, #channel.is_onhold, 'true').

-spec channel_unhold(map()) -> any().
channel_unhold(#{call_id := UUID}) ->
    ecallmgr_fs_channels:update(UUID, #channel.is_onhold, 'false').

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec jobj_to_update(atom(), ne_binary(), kz_json:object()) -> channel_updates().
jobj_to_update(_Node, _UUID, _JObj) -> [].

-spec jobj_to_record(atom(), ne_binary(), kz_json:object()) -> channel().
jobj_to_record(Node, UUID, JObj) ->
    CCVs = kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
    CAVs = kz_json:get_json_value(<<"Custom-Application-Vars">>, JObj, kz_json:new()),
    OtherLeg = kz_json:get_ne_binary_value(<<"Other-Leg-Call-ID">>, JObj),
    #channel{uuid=UUID
            ,destination=kz_json:get_ne_binary_value(<<"Caller-Destination-Number">>, JObj)
            ,direction=kz_json:get_ne_binary_value(<<"Call-Direction">>, JObj)

            ,account_id=kz_json:get_ne_binary_value(<<"Account-ID">>, CCVs)
            ,account_billing=kz_json:get_ne_binary_value(<<"Account-Billing">>, CCVs)
            ,authorizing_id=kz_json:get_ne_binary_value(<<"Authorizing-ID">>, CCVs)
            ,authorizing_type=kz_json:get_ne_binary_value(<<"Authorizing-Type">>, CCVs)
            ,is_authorized=kz_json:is_true(<<"Channel-Authorized">>, CCVs)
            ,owner_id=kz_json:get_ne_binary_value(<<"Owner-ID">>, CCVs)
            ,resource_id=kz_json:get_ne_binary_value(<<"Resource-ID">>, CCVs)
            ,fetch_id=kz_json:get_ne_binary_value(<<"Fetch-ID">>, CCVs)
            ,bridge_id=kz_json:get_ne_binary_value(<<"Bridge-ID">>, CCVs, UUID)
            ,reseller_id=kz_json:get_ne_binary_value(<<"Reseller-ID">>, CCVs)
            ,reseller_billing=kz_json:get_ne_binary_value(<<"Reseller-Billing">>, CCVs)
            ,precedence=kz_term:to_integer(kz_json:get_integer_value(<<"Precedence">>, CCVs, 5))

            ,presence_id=kz_json:get_ne_binary_value(<<"Presence-ID">>, JObj)
            ,realm=kz_json:get_ne_binary_value(<<"Realm">>, CCVs)
            ,username=kz_json:get_ne_binary_value(<<"Username">>, CCVs)

            ,answered=kz_json:get_ne_binary_value(<<"Answer-State">>, JObj) =:= <<"answered">>
            ,node=Node
            ,timestamp=kz_time:current_tstamp()

            ,profile=kz_json:get_ne_binary_value(<<"Caller-Profile">>, JObj, ?DEFAULT_FS_PROFILE)
            ,context=kz_json:get_ne_binary_value(<<"Caller-Context">>, JObj, ?DEFAULT_FREESWITCH_CONTEXT)
            ,dialplan=kz_json:get_ne_binary_value(<<"Caller-Dialplan">>, JObj, ?DEFAULT_FS_DIALPLAN)

            ,other_leg=OtherLeg
            ,handling_locally=handling_locally(kz_json:get_ne_binary_value(<<"Ecallmgr-Node">>, CCVs), OtherLeg)

            ,to_tag=kz_json:get_value(<<"To-Tag">>, JObj)
            ,from_tag=kz_json:get_value(<<"From-Tag">>, JObj)

            ,interaction_id=kz_json:get_ne_binary_value(<<?CALL_INTERACTION_ID>>, CCVs)

            ,is_loopback=kz_json:is_true(<<"Channel-Is-Loopback">>, JObj)
            ,loopback_leg_name=kz_json:get_value(<<"Channel-Loopback-Leg">>, JObj)
            ,loopback_other_leg=kz_json:get_value(<<"Channel-Loopback-Other-Leg-ID">>, JObj)

            ,callflow_id=kz_json:get_ne_binary_value(<<"CallFlow-ID">>, CCVs)
            ,cavs=CAVs
            ,ccvs=CCVs
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
    case kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Ecallmgr-Node">>], JObj) =:= kz_term:to_binary(node()) of
        'true' -> authorize_channel(Node, UUID, JObj);
        'false' -> 'ok'
    end.

authorize_channel(Node, UUID, JObj) ->
    case ecallmgr_fs_authz:authorize(JObj, UUID, Node) of
        {'true', CCVs} -> ecallmgr_fs_command:set(Node, UUID, kz_json:to_proplist(CCVs));
        Authz -> Authz
    end.
