%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%% Pickup a call in the specified group
%%%
%%% data: {
%%%   "group_id":"_group_id_"
%%%   ,"user_id":"_user_id_"
%%%   ,"device_id":"_device_id_"
%%% }
%%%
%%% One of the three - group_id, user_id, or owner_id - must be defined on
%%% the data payload. Preference is given by most restrictive option set,
%%% so device_id is checked for first, then user_id, and finally group_id.
%%%
%%% device_id will only steal a channel currently ringing that device
%%% user_id will only steal a channel currently ringing any of the user's devices*
%%% group_id will steal a channel from any devices/users in a group*
%%%  * no guarantees on which if multiple inbound calls are ringing
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_group_pickup).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    case maybe_allowed_to_intercept(Data, Call) of
        'true' ->
            case find_sip_endpoints(Data, Call) of
                [] -> no_users_in_group(Call);
                Usernames -> connect_to_ringing_channel(Usernames, Call)
            end;
        'false' -> no_permission_to_intercept(Call)
    end,
    cf_exe:stop(Call).

-spec maybe_allowed_to_intercept(wh_json:object(), whapps_call:call()) -> boolean().
maybe_allowed_to_intercept(Data, Call) ->
    case wh_json:get_value(<<"approved_device_id">>, Data) of
        'undefined' ->
            case wh_json:get_value(<<"approved_user_id">>, Data) of
                'undefined' ->
                    case wh_json:get_value(<<"approved_group_id">>, Data) of
                        'undefined' -> 'true';
                        GroupId -> maybe_belongs_to_group(GroupId, Call)
                    end;
                UserId -> maybe_belongs_to_user(UserId, Call)
            end;
        DeviceId ->
            % Compare approved device_id with calling one
            DeviceId == whapps_call:authorizing_id(Call)
    end.

-spec maybe_belongs_to_user(ne_binary(), whapps_call:call()) -> boolean().
maybe_belongs_to_user(UserId, Call) ->
    is_in_list(whapps_call:authorizing_id(Call), find_user_endpoints([UserId],[],Call)).

-spec maybe_belongs_to_group(ne_binary(), whapps_call:call()) -> boolean().
maybe_belongs_to_group(GroupId, Call) ->
    is_in_list(whapps_call:authorizing_id(Call), find_group_endpoints(GroupId, Call)).

-spec connect_to_ringing_channel(ne_binaries(), whapps_call:call()) -> 'ok'.
connect_to_ringing_channel(Usernames, Call) ->
    case find_channels(Usernames, Call) of
        [] -> no_channels_ringing(Call);
        Channels -> connect_to_a_channel(Channels, Call)
    end,
    'ok'.

-spec connect_to_a_channel(wh_json:objects(), whapps_call:call()) -> 'ok'.
connect_to_a_channel(Channels, Call) ->
    MyUUID = whapps_call:call_id(Call),
    MyMediaServer = whapps_call:switch_nodename(Call),

    lager:debug("looking for channels on my node ~s that aren't me", [MyMediaServer]),

    case sort_channels(Channels, MyUUID, MyMediaServer) of
        {[], []} ->
            lager:debug("no channels available to pickup"),
            no_channels_ringing(Call);
        {[], [RemoteUUID|_Remote]} ->
            lager:debug("no unanswered calls on my media server, trying ~s", [RemoteUUID]),
            intercept_call(RemoteUUID, Call);
        {[LocalUUID|_Cs], _} ->
            lager:debug("found a call (~s) on my media server", [LocalUUID]),
            intercept_call(LocalUUID, Call)
    end.

-spec sort_channels(wh_json:objects(), ne_binary(), ne_binary()) ->
                           {ne_binaries(), ne_binaries()}.
-spec sort_channels(wh_json:objects(), ne_binary(), ne_binary(), {ne_binaries(), ne_binaries()}) ->
                           {ne_binaries(), ne_binaries()}.
sort_channels(Channels, MyUUID, MyMediaServer) ->
    sort_channels(Channels, MyUUID, MyMediaServer, {[], []}).
sort_channels([], _MyUUID, _MyMediaServer, Acc) -> Acc;
sort_channels([Channel|Channels], MyUUID, MyMediaServer, Acc) ->
    lager:debug("channel: c: ~s a: ~s n: ~s oleg: ~s", [wh_json:get_value(<<"uuid">>, Channel)
                                                        ,wh_json:is_true(<<"answered">>, Channel)
                                                        ,wh_json:get_value(<<"node">>, Channel)
                                                        ,wh_json:get_value(<<"other_leg">>, Channel)
                                                       ]),
    case wh_json:is_true(<<"answered">>, Channel) of
        'true' ->
            sort_channels(Channels, MyUUID, MyMediaServer, Acc);
        'false' ->
            maybe_add_unanswered_leg(Channels, MyUUID, MyMediaServer, Acc, Channel)
    end.

-spec maybe_add_unanswered_leg(wh_json:objects(), ne_binary(), ne_binary(), {ne_binaries(), ne_binaries()}, wh_json:object()) ->
                                      {ne_binaries(), ne_binaries()}.
maybe_add_unanswered_leg(Channels, MyUUID, MyMediaServer, {Local, Remote}=Acc, Channel) ->
    case wh_json:get_value(<<"node">>, Channel) of
        MyMediaServer ->
            case wh_json:get_value(<<"uuid">>, Channel) of
                MyUUID ->
                    sort_channels(Channels, MyUUID, MyMediaServer, Acc);
                _UUID ->
                    sort_channels(Channels, MyUUID, MyMediaServer, {maybe_add_other_leg(Channel, Local), Remote})
            end;
        _OtherMediaServer ->
            sort_channels(Channels, MyUUID, MyMediaServer, {Local, maybe_add_other_leg(Channel, Remote)})
    end.

-spec maybe_add_other_leg(wh_json:object(), ne_binaries()) -> ne_binaries().
maybe_add_other_leg(Channel, Legs) ->
    case wh_json:get_value(<<"other_leg">>, Channel) of
        'undefined' -> Legs;
        Leg -> [Leg | Legs]
    end.

-spec intercept_call(ne_binary(), whapps_call:call()) -> 'ok'.
intercept_call(UUID, Call) ->
    _ = whapps_call_command:send_command(pickup_cmd(UUID), Call),
    case wait_for_pickup(Call) of
        {'error', _E} ->
            lager:debug("failed to pickup ~s: ~p", [UUID, _E]);
        'ok' ->
            lager:debug("call picked up"),
            whapps_call_command:wait_for_hangup(),
            lager:debug("hangup recv")
    end.

-spec pickup_cmd(ne_binary()) -> wh_proplist().
pickup_cmd(TargetCallId) ->
    [{<<"Application-Name">>, <<"call_pickup">>}
     ,{<<"Target-Call-ID">>, TargetCallId}
     ,{<<"Unbridged-Only">>, 'true'}
    ].

-spec wait_for_pickup(whapps_call:call()) ->
                             'ok' |
                             {'error', 'failed'} |
                             {'error', 'timeout'}.
wait_for_pickup(Call) ->
    case whapps_call_command:receive_event(10000) of
        {'ok', Evt} ->
            pickup_event(Call, wh_util:get_event_type(Evt), Evt);
        {'error', 'timeout'}=E ->
            lager:debug("timed out"),
            E
    end.

-spec pickup_event(whapps_call:call(), {ne_binary(), ne_binary()}, wh_json:object()) ->
                          {'error', 'failed' | 'timeout'} |
                          'ok'.
pickup_event(_Call, {<<"error">>, <<"dialplan">>}, Evt) ->
    lager:debug("error in dialplan: ~s", [wh_json:get_value(<<"Error-Message">>, Evt)]),
    {'error', 'failed'};
pickup_event(_Call, {<<"call_event">>,<<"CHANNEL_BRIDGE">>}, _Evt) ->
    lager:debug("channel bridged to ~s", [wh_json:get_value(<<"Other-Leg-Unique-ID">>, _Evt)]);
pickup_event(Call, _Type, _Evt) ->
    lager:debug("unhandled evt ~p", [_Type]),
    wait_for_pickup(Call).

-spec find_channels(ne_binaries(), whapps_call:call()) -> wh_json:objects().
find_channels(Usernames, Call) ->
    Realm = wh_util:get_account_realm(whapps_call:account_id(Call)),
    lager:debug("finding channels for realm ~s, usernames ~p", [Realm, Usernames]),
    Req = [{<<"Realm">>, Realm}
           ,{<<"Usernames">>, Usernames}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_call:publish_query_user_channels_req/1
                                       ,fun wapi_call:query_user_channels_resp_v/1
                                      )
    of
        {'ok', Resp} -> wh_json:get_value(<<"Channels">>, Resp, []);
        {'error', _E} ->
            lager:debug("failed to get channels: ~p", [_E]),
            []
    end.

-spec find_sip_endpoints(wh_json:object(), whapps_call:call()) ->
                                ne_binaries().
find_sip_endpoints(Data, Call) ->
    case wh_json:get_value(<<"device_id">>, Data) of
        'undefined' ->
            case wh_json:get_value(<<"user_id">>, Data) of
                'undefined' ->
                    find_sip_users(wh_json:get_value(<<"group_id">>, Data), Call);
                UserId ->
                    sip_users_from_endpoints(
                      find_user_endpoints([UserId], [], Call), Call
                     )
            end;
        DeviceId ->
             sip_users_from_endpoints([DeviceId], Call)
    end.

-spec find_sip_users(ne_binary(), whapps_call:call()) -> ne_binaries().
find_sip_users(GroupId, Call) ->
    sip_users_from_endpoints(find_group_endpoints(GroupId, Call), Call).

-spec find_group_endpoints(ne_binary(), whapps_call:call()) -> ne_binaries().
find_group_endpoints(GroupId, Call) ->
    GroupsJObj = cf_attributes:groups(Call),
    case [wh_json:get_value(<<"value">>, JObj)
          || JObj <- GroupsJObj,
             wh_json:get_value(<<"id">>, JObj) =:= GroupId
         ]
    of
        [] -> [];
        [GroupEndpoints] ->
            Ids = wh_json:get_keys(GroupEndpoints),
            find_endpoints(Ids, GroupEndpoints, Call)
    end.

-spec sip_users_from_endpoints(ne_binaries(), whapps_call:call()) -> ne_binaries().
sip_users_from_endpoints(EndpointIds, Call) ->
    lists:foldl(fun(EndpointId, Acc) ->
                        case sip_user_of_endpoint(EndpointId, Call) of
                            'undefined' -> Acc;
                            Username -> [Username|Acc]
                        end
                end, [], EndpointIds).

-spec sip_user_of_endpoint(ne_binary(), whapps_call:call()) -> api_binary().
sip_user_of_endpoint(EndpointId, Call) ->
    case cf_endpoint:get(EndpointId, Call) of
        {'error', _} -> 'undefined';
        {'ok', Endpoint} ->
            wh_json:get_value([<<"sip">>, <<"username">>], Endpoint)
    end.

-spec find_endpoints(ne_binaries(), wh_json:object(), whapps_call:call()) ->
                            ne_binaries().
find_endpoints(Ids, GroupEndpoints, Call) ->
    {DeviceIds, UserIds} =
        lists:partition(fun(Id) ->
                                wh_json:get_value([Id, <<"type">>], GroupEndpoints) =:= <<"device">>
                        end, Ids),
    find_user_endpoints(UserIds, lists:sort(DeviceIds), Call).

-spec find_user_endpoints(ne_binaries(), ne_binaries(), whapps_call:call()) ->
                                 ne_binaries().
find_user_endpoints([], DeviceIds, _) -> DeviceIds;
find_user_endpoints(UserIds, DeviceIds, Call) ->
    UserDeviceIds = cf_attributes:owned_by(UserIds, <<"device">>, Call),
    lists:merge(lists:sort(UserDeviceIds), DeviceIds).

-spec no_users_in_group(whapps_call:call()) -> 'ok'.
no_users_in_group(Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:b_play(<<"system_media/pickup-no_users">>, Call).

-spec no_channels_ringing(whapps_call:call()) -> 'ok'.
no_channels_ringing(Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:b_play(<<"system_media/pickup-no_channels">>, Call).

-spec no_permission_to_intercept(whapps_call:call()) -> 'ok'.
%% TODO: please convert to system_media file (say is not consistent on deployments)
no_permission_to_intercept(Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:b_say(<<"you have no permission to intercept this call">>, Call).

-spec is_in_list(api_binary(), ne_binaries()) -> boolean().
%% NOTE: this could be replaced by list:member
is_in_list(_, []) -> 'false';
is_in_list(Suspect, [H|T]) ->
    case Suspect == H of
        'true' -> 'true';
        _ -> is_in_list(Suspect, T)
    end.
