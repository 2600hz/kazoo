%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600hz, INC
%%% @doc
%%% Intercept a call
%%%
%%% data: {
%%%   "user_id":"_user_id_"
%%%   ,"device_id":"_device_id_"
%%% }
%%%
%%% One of the two - user_id, or device_id - must be defined on
%%% the data payload. Preference is given by most restrictive option set,
%%% so device_id is checked for first, then user_id.
%%%
%%% device_id will only steal a channel of a specific device,
%%% user_id will only steal a channel on any of the user's devices*
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Mikhail Rodionov)
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_intercept).

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
    _ = case maybe_allowed_to_intercept(Data, Call) of
            {ok, 'true'} ->
                continue(Data, Call);
            {'ok', 'false'} -> no_permission_to_intercept(Call);
            {'error', 'not_found'} ->
                case maybe_same_user(Data, Call) orelse maybe_same_group(Data, Call) of
                    'true' -> continue(Data, Call);
                    'false' -> no_permission_to_intercept(Call)
                end
        end,
    cf_exe:stop(Call).

-spec maybe_same_user(wh_json:object(), whapps_call:call()) -> boolean().
maybe_same_user(Data, Call) ->
    CallerDevice = whapps_call:authorizing_id(Call),
    case couch_mgr:open_cache_doc(whapps_call:account_db(Call), CallerDevice) of
        {'ok', DevDoc} ->
            Caller = wh_json:get_value(<<"owner_id">>, DevDoc),
            case wh_json:get_value(<<"user_id">>, Data) of
                'undefined' ->
                    case wh_json:get_value(<<"device_id">>, Data) of
                        'undefined' ->
                            'false';
                        DevId ->
                            case couch_mgr:open_cache_doc(whapps_call:account_db(Call), DevId) of
                                {ok, Target} ->
                                    Caller =:= wh_json:get_value(<<"owner_id">>, Target);
                                Err ->
                                    lager:debug("Error while opening couch document: ~p", [Err]),
                                    'false'
                            end
                    end;
                UserId ->
                    UserId =:= Caller
            end;
        Err ->
            lager:debug("Error while opening couch document: ~p", [Err]),
            'false'
    end.

-spec maybe_same_group(wh_json:object(), whapps_call:call()) -> boolean().
maybe_same_group(Data, Call) ->
    CallerDevice = whapps_call:authorizing_id(Call),
    Acc = whapps_call:account_db(Call),
    case couch_mgr:open_cache_doc(Acc, CallerDevice) of
        {'ok', DevDoc} ->
            Caller = wh_json:get_value(<<"owner_id">>, DevDoc),
            case wh_json:get_value(<<"group_id">>, Data) of
                'undefined' ->
                    'false';
                GroupId ->
                    case couch_mgr:open_cache_doc(Acc, GroupId) of
                        {'ok', GroupDoc} ->
                            Endpoints = wh_json:get_ne_value(<<"endpoints">>, GroupDoc, []),
                            lists:any(fun({Key, Descr}) ->
                                case wh_json:get_value(<<"type">>, Descr) of
                                    <<"device">> ->
                                        Key =:= CallerDevice;
                                    <<"user">> ->
                                        Key =:= Caller;
                                    _Type ->
                                        'false'
                                end
                            end, wh_json:to_proplist(Endpoints));
                        Err ->
                            lager:debug("Error while opening couch document: ~p", [Err]),
                            'false'
                    end
            end;
        Err ->
            lager:debug("Error while opening couch document: ~p", [Err]),
            'false'
    end.

-spec continue(wh_json:object(), whapps_call:call()) -> any().
continue(Data, Call) ->
    case find_sip_endpoints(Data, Call) of
        [] -> no_users(Call);
        Usernames -> connect_to_channel(Usernames, Call)
    end.

-spec maybe_allowed_to_intercept(wh_json:object(), whapps_call:call()) ->
                                        {'ok', boolean()} |
                                        {'error', 'not_found'}.
maybe_allowed_to_intercept(Data, Call) ->
    case wh_json:get_value(<<"approved_device_id">>, Data) of
        'undefined' ->
            case wh_json:get_value(<<"approved_user_id">>, Data) of
                'undefined' ->
                    case wh_json:get_value(<<"approved_group_id">>, Data) of
                        'undefined' -> {'error', 'not_found'};
                        GroupId -> {'ok', cf_util:caller_belongs_to_group(GroupId, Call)}
                    end;
                UserId -> {'ok', cf_util:caller_belongs_to_user(UserId, Call)}
            end;
        DeviceId ->
            % Compare approved device_id with calling one
            {'ok', DeviceId == whapps_call:authorizing_id(Call)}
    end.

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
            cf_util:find_endpoints(Ids, GroupEndpoints, Call)
    end.

-spec connect_to_channel(ne_binaries(), whapps_call:call()) -> 'ok'.
connect_to_channel(Usernames, Call) ->
    case cf_util:find_channels(Usernames, Call) of
        [] -> no_channels(Call);
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
            lager:debug("no channels available to intercept"),
            no_channels(Call);
        {[], [RemoteUUID|_Remote]} ->
            lager:debug("no calls on my media server, trying ~s", [RemoteUUID]),
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
            maybe_add_leg(Channels, MyUUID, MyMediaServer, Acc, Channel).

-spec maybe_add_leg(wh_json:objects(), ne_binary(), ne_binary(), {ne_binaries(), ne_binaries()}, wh_json:object()) ->
                                      {ne_binaries(), ne_binaries()}.
maybe_add_leg(Channels, MyUUID, MyMediaServer, {Local, Remote}=Acc, Channel) ->
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
    _ = whapps_call_command:send_command(intercept_cmd(UUID), Call),
    case wait_for_intercept(Call) of
        {'error', _E} ->
            lager:debug("failed to intercept ~s: ~p", [UUID, _E]);
        'ok' ->
            lager:debug("call intercepted"),
            whapps_call_command:wait_for_hangup(),
            lager:debug("hangup recv")
    end.

-spec intercept_cmd(ne_binary()) -> wh_proplist().
intercept_cmd(TargetCallId) ->
    [{<<"Application-Name">>, <<"call_pickup">>}
     ,{<<"Target-Call-ID">>, TargetCallId}
     ,{<<"Unbridged-Only">>, 'false'}
     ,{<<"Unanswered-Only">>, 'false'}
    ].

-spec wait_for_intercept(whapps_call:call()) ->
                             'ok' |
                             {'error', 'failed'} |
                             {'error', 'timeout'}.
wait_for_intercept(Call) ->
    case whapps_call_command:receive_event(10000) of
        {'ok', Evt} ->
            intercept_event(Call, wh_util:get_event_type(Evt), Evt);
        {'error', 'timeout'}=E ->
            lager:debug("timed out"),
            E
    end.

-spec intercept_event(whapps_call:call(), {ne_binary(), ne_binary()}, wh_json:object()) ->
                          {'error', 'failed' | 'timeout'} |
                          'ok'.
intercept_event(_Call, {<<"error">>, <<"dialplan">>}, Evt) ->
    lager:debug("error in dialplan: ~s", [wh_json:get_value(<<"Error-Message">>, Evt)]),
    {'error', 'failed'};
intercept_event(_Call, {<<"call_event">>,<<"CHANNEL_BRIDGE">>}, _Evt) ->
    lager:debug("channel bridged to ~s", [wh_json:get_value(<<"Other-Leg-Call-ID">>, _Evt)]);
intercept_event(Call, _Type, _Evt) ->
    lager:debug("unhandled evt ~p", [_Type]),
    wait_for_intercept(Call).

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
                      cf_util:find_user_endpoints([UserId], [], Call), Call
                     )
            end;
        DeviceId ->
             sip_users_from_endpoints([DeviceId], Call)
    end.

-spec find_sip_users(ne_binary(), whapps_call:call()) -> ne_binaries().
find_sip_users(GroupId, Call) ->
  sip_users_from_endpoints(find_group_endpoints(GroupId, Call), Call).

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

-spec no_users(whapps_call:call()) -> any().
no_users(Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:b_prompt(<<"intercept-no_users">>, Call).

-spec no_channels(whapps_call:call()) -> any().
no_channels(Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:b_prompt(<<"intercept-no_channels">>, Call).

-spec no_permission_to_intercept(whapps_call:call()) -> any().
no_permission_to_intercept(Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:b_prompt(<<"intercept-no_channels">>, Call).
