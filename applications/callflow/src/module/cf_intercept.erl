%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Intercept a call.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`user_id'</dt>
%%%   <dd>User ID.</dd>
%%%
%%%   <dt>`device_id'</dt>
%%%   <dd>Device ID></dd>
%%% </dl>
%%%
%%% One of the two, `user_id' or `device_id', must be defined on
%%% the data payload. Preference is given by most restrictive option set,
%%% so `device_id' is checked for first, then `user_id'.
%%%
%%% `device_id' will only steal a channel of a specific device,
%%% `user_id' will only steal a channel on any of the user's devices.
%%%
%%%
%%% @author SIPLABS LLC (Mikhail Rodionov)
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_intercept).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

%%------------------------------------------------------------------------------
%% @doc Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    _ = case maybe_allowed_to_intercept(Data, Call) of
            {'ok', 'true'} -> continue(Data, Call);
            {'ok', 'false'} -> no_permission_to_intercept(Call);
            {'error', 'not_found'} ->
                case maybe_same_user(Data, Call)
                    orelse maybe_same_group(Data, Call)
                of
                    'true' -> continue(Data, Call);
                    'false' -> no_permission_to_intercept(Call)
                end
        end,
    cf_exe:stop(Call).

-spec maybe_same_user(kz_json:object(), kapps_call:call()) -> boolean().
maybe_same_user(Data, Call) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), kapps_call:authorizing_id(Call)) of
        {'ok', CallerDeviceJObj} ->
            is_caller_same_user(Data, Call, CallerDeviceJObj);
        {'error', _E} ->
            lager:debug("error while opening caller device ~s: ~p", [kapps_call:authorizing_id(Call), _E]),
            'false'
    end.

-spec is_caller_same_user(kz_json:object(), kapps_call:call(), kz_json:object()) -> boolean().
is_caller_same_user(Data, Call, CallerDeviceJObj) ->
    OwnerId = kz_json:get_ne_binary_value(<<"owner_id">>, CallerDeviceJObj),

    case kz_json:get_ne_binary_value(<<"user_id">>, Data) of
        'undefined' ->
            is_device_same_user(Call, OwnerId, kz_json:get_ne_binary_value(<<"device_id">>, Data));
        OwnerId -> 'true';
        _UserId -> 'false'
    end.

-spec is_device_same_user(kapps_call:call(), kz_term:ne_binary(), kz_term:api_binary()) -> boolean().
is_device_same_user(_Call, _OwnerId, 'undefined') -> 'false';
is_device_same_user(Call, OwnerId, DeviceId) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), DeviceId) of
        {'ok', Target} ->
            OwnerId =:= kz_json:get_ne_binary_value(<<"owner_id">>, Target);
        {'error', _E} ->
            lager:debug("error while opening device ~s: ~p", [DeviceId, _E]),
            'false'
    end.

-spec maybe_same_group(kz_json:object(), kapps_call:call()) -> boolean().
maybe_same_group(Data, Call) ->
    CallerDeviceId = kapps_call:authorizing_id(Call),
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), CallerDeviceId) of
        {'ok', CallerDeviceJObj} ->
            is_caller_same_group(Data, Call, CallerDeviceJObj);
        {'error', _E} ->
            lager:debug("error while opening device ~s: ~p", [CallerDeviceId, _E]),
            'false'
    end.

-spec is_caller_same_group(kz_json:object(), kapps_call:call(), kz_json:object()) -> boolean().
is_caller_same_group(Data, Call, CallerDeviceJObj) ->
    DeviceId = kz_doc:id(CallerDeviceJObj),
    OwnerId = kz_json:get_ne_binary_value(<<"owner_id">>, CallerDeviceJObj),
    GroupId = kz_json:get_ne_binary_value(<<"group_id">>, Data),
    is_owner_same_group(Call, DeviceId, OwnerId, GroupId).

-spec is_owner_same_group(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) -> boolean().
is_owner_same_group(_Call, _DeviceId, _OwnerId, 'undefined') -> 'false';
is_owner_same_group(Call, DeviceId, OwnerId, GroupId) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), GroupId) of
        {'ok', GroupJObj} ->
            kz_json:any(fun({EndpointId, Endpoint}) when EndpointId =:= DeviceId ->
                                kz_json:get_ne_binary_value(<<"type">>, Endpoint) =:= <<"device">>;
                           ({EndpointId, Endpoint}) when EndpointId =:= OwnerId ->
                                kz_json:get_ne_binary_value(<<"type">>, Endpoint) =:= <<"user">>;
                           ({_EndpointId, _Endpoint}) -> 'false'
                        end
                       ,kz_json:get_json_value(<<"endpoints">>, GroupJObj, kz_json:new())
                       );
        {'error', _E} ->
            lager:debug("error while opening group ~s: ~p", [GroupId, _E]),
            'false'
    end.

-spec continue(kz_json:object(), kapps_call:call()) -> any().
continue(Data, Call) ->
    case find_sip_endpoints(Data, Call) of
        [] -> no_users(Call);
        Usernames -> connect_to_channel(Usernames, Call)
    end.

-spec maybe_allowed_to_intercept(kz_json:object(), kapps_call:call()) ->
                                        {'ok', boolean()} |
                                        {'error', 'not_found'}.
maybe_allowed_to_intercept(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"approved_device_id">>, Data) of
        'undefined' ->
            maybe_approved_user(Data, Call);
        DeviceId ->
            %% Compare approved device_id with calling one
            {'ok', DeviceId == kapps_call:authorizing_id(Call)}
    end.

-spec maybe_approved_user(kz_json:object(), kapps_call:call()) ->
                                 {'ok', boolean()} |
                                 {'error', 'not_found'}.
maybe_approved_user(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"approved_user_id">>, Data) of
        'undefined' -> maybe_approved_group(Data, Call);
        UserId -> {'ok', cf_util:caller_belongs_to_user(UserId, Call)}
    end.

-spec maybe_approved_group(kz_json:object(), kapps_call:call()) ->
                                  {'ok', boolean()} |
                                  {'error', 'not_found'}.
maybe_approved_group(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"approved_group_id">>, Data) of
        'undefined' -> {'error', 'not_found'};
        GroupId -> {'ok', cf_util:caller_belongs_to_group(GroupId, Call)}
    end.

-spec find_group_endpoints(kz_term:ne_binary(), kapps_call:call()) -> kz_term:ne_binaries().
find_group_endpoints(GroupId, Call) ->
    GroupsJObj = kz_attributes:groups(Call),
    case [kz_json:get_json_value(<<"value">>, JObj)
          || JObj <- GroupsJObj,
             kz_doc:id(JObj) =:= GroupId
         ]
    of
        [] -> [];
        [GroupEndpoints] ->
            Ids = kz_json:get_keys(GroupEndpoints),
            cf_util:find_endpoints(Ids, GroupEndpoints, Call)
    end.

-spec connect_to_channel(kz_term:ne_binaries(), kapps_call:call()) -> 'ok'.
connect_to_channel(Usernames, Call) ->
    _ = case cf_util:find_channels(Usernames, Call) of
            [] -> no_channels(Call);
            Channels -> connect_to_a_channel(Channels, Call)
        end,
    'ok'.

-spec connect_to_a_channel(kz_json:objects(), kapps_call:call()) -> 'ok'.
connect_to_a_channel(Channels, Call) ->
    MyUUID = kapps_call:call_id(Call),
    MyMediaServer = kapps_call:switch_nodename(Call),

    lager:debug("looking for channels on my node ~s that aren't me", [MyMediaServer]),

    case sort_channels(Channels, MyUUID, MyMediaServer) of
        {[], []} ->
            lager:debug("no channels available to intercept"),
            no_channels(Call);
        {[], [RemoteUUID|_Remote]} ->
            lager:debug("no calls on my media server, trying ~s", [RemoteUUID]),
            intercept_call(RemoteUUID, Call);
        {[LocalUUID|_Cs], _} ->
            lager:debug("found a call (~s) on my media server", [LocalUUID]),
            intercept_call(LocalUUID, Call)
    end.

-spec sort_channels(kz_json:objects(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                           {kz_term:ne_binaries(), kz_term:ne_binaries()}.
sort_channels(Channels, MyUUID, MyMediaServer) ->
    sort_channels(Channels, MyUUID, MyMediaServer, {[], []}).

-spec sort_channels(kz_json:objects(), kz_term:ne_binary(), kz_term:ne_binary(), {kz_term:ne_binaries(), kz_term:ne_binaries()}) ->
                           {kz_term:ne_binaries(), kz_term:ne_binaries()}.
sort_channels([], _MyUUID, _MyMediaServer, Acc) -> Acc;
sort_channels([Channel|Channels], MyUUID, MyMediaServer, Acc) ->
    lager:debug("channel: c: ~s a: ~s n: ~s oleg: ~s", [kz_json:get_ne_binary_value(<<"uuid">>, Channel)
                                                       ,kz_json:is_true(<<"answered">>, Channel)
                                                       ,kz_json:get_ne_binary_value(<<"node">>, Channel)
                                                       ,kz_json:get_ne_binary_value(<<"other_leg">>, Channel)
                                                       ]),
    maybe_add_leg(Channels, MyUUID, MyMediaServer, Acc, Channel).

-spec maybe_add_leg(kz_json:objects(), kz_term:ne_binary(), kz_term:ne_binary(), {kz_term:ne_binaries(), kz_term:ne_binaries()}, kz_json:object()) ->
                           {kz_term:ne_binaries(), kz_term:ne_binaries()}.
maybe_add_leg(Channels, MyUUID, MyMediaServer, {Local, Remote}=Acc, Channel) ->
    case kz_json:get_ne_binary_value(<<"node">>, Channel) of
        MyMediaServer ->
            case kz_json:get_ne_binary_value(<<"uuid">>, Channel) of
                MyUUID ->
                    sort_channels(Channels, MyUUID, MyMediaServer, Acc);
                _UUID ->
                    sort_channels(Channels, MyUUID, MyMediaServer, {maybe_add_other_leg(Channel, Local), Remote})
            end;
        _OtherMediaServer ->
            sort_channels(Channels, MyUUID, MyMediaServer, {Local, maybe_add_other_leg(Channel, Remote)})
    end.

-spec maybe_add_other_leg(kz_json:object(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
maybe_add_other_leg(Channel, Legs) ->
    case kz_json:get_ne_binary_value(<<"other_leg">>, Channel) of
        'undefined' -> Legs;
        Leg -> [Leg | Legs]
    end.

-spec intercept_call(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
intercept_call(UUID, Call) ->
    _ = kapps_call_command:send_command(intercept_cmd(UUID), Call),
    case wait_for_intercept(Call) of
        {'error', _E} ->
            lager:debug("failed to intercept ~s: ~p", [UUID, _E]);
        'ok' ->
            lager:debug("call intercepted"),
            _ = kapps_call_command:wait_for_hangup(),
            lager:debug("hangup recv")
    end.

-spec intercept_cmd(kz_term:ne_binary()) -> kz_term:proplist().
intercept_cmd(TargetCallId) ->
    [{<<"Application-Name">>, <<"call_pickup">>}
    ,{<<"Target-Call-ID">>, TargetCallId}
    ,{<<"Unbridged-Only">>, 'false'}
    ,{<<"Unanswered-Only">>, 'false'}
    ].

-spec wait_for_intercept(kapps_call:call()) ->
                                'ok' |
                                {'error', 'failed'} |
                                {'error', 'timeout'}.
wait_for_intercept(Call) ->
    case kapps_call_command:receive_event(10000) of
        {'ok', Evt} ->
            intercept_event(Call, kz_util:get_event_type(Evt), Evt);
        {'error', 'timeout'}=E ->
            lager:debug("timed out"),
            E
    end.

-spec intercept_event(kapps_call:call(), {kz_term:ne_binary(), kz_term:ne_binary()}, kz_json:object()) ->
                             {'error', 'failed' | 'timeout'} |
                             'ok'.
intercept_event(_Call, {<<"error">>, <<"dialplan">>}, Evt) ->
    lager:debug("error in dialplan: ~s", [kz_json:get_ne_binary_value(<<"Error-Message">>, Evt)]),
    {'error', 'failed'};
intercept_event(_Call, {<<"call_event">>,<<"CHANNEL_BRIDGE">>}, _Evt) ->
    lager:debug("channel bridged to ~s", [kz_json:get_ne_binary_value(<<"Other-Leg-Call-ID">>, _Evt)]);
intercept_event(Call, _Type, _Evt) ->
    lager:debug("unhandled evt ~p", [_Type]),
    wait_for_intercept(Call).

-spec find_sip_endpoints(kz_json:object(), kapps_call:call()) ->
                                kz_term:ne_binaries().
find_sip_endpoints(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"device_id">>, Data) of
        'undefined' -> find_user_endpoints(Data, Call);
        DeviceId ->
            cf_util:sip_users_from_device_ids([DeviceId], Call)
    end.

-spec find_user_endpoints(kz_json:object(), kapps_call:call()) -> kz_term:ne_binaries().
find_user_endpoints(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"user_id">>, Data) of
        'undefined' ->
            find_sip_users(kz_json:get_ne_binary_value(<<"group_id">>, Data), Call);
        UserId ->
            cf_util:sip_users_from_device_ids(
              cf_util:find_user_endpoints([UserId], [], Call), Call
             )
    end.

-spec find_sip_users(kz_term:ne_binary(), kapps_call:call()) -> kz_term:ne_binaries().
find_sip_users(GroupId, Call) ->
    cf_util:sip_users_from_device_ids(find_group_endpoints(GroupId, Call), Call).

-spec no_users(kapps_call:call()) -> any().
no_users(Call) ->
    kapps_call_command:answer(Call),
    kapps_call_command:b_prompt(<<"intercept-no_users">>, Call).

-spec no_channels(kapps_call:call()) -> any().
no_channels(Call) ->
    kapps_call_command:answer(Call),
    kapps_call_command:b_prompt(<<"intercept-no_channels">>, Call).

-spec no_permission_to_intercept(kapps_call:call()) -> any().
no_permission_to_intercept(Call) ->
    kapps_call_command:answer(Call),
    kapps_call_command:b_prompt(<<"intercept-no_channels">>, Call).
