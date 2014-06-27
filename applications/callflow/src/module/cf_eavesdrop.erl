%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600hz, INC
%%% @doc
%%% Eacesdrop
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
%%% device_id will only connect to a channel of a specific device,
%%% user_id will only connect to channel on any of the user's devices*
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Mikhail Rodionov)
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_eavesdrop).

-include("../callflow.hrl").

-export([handle/2
         ,no_permission_to_eavesdrop/1
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    _ = case maybe_allowed_to_eavesdrop(Data, Call) of
            'true' ->
                case find_sip_endpoints(Data, Call) of
                    [] -> no_users(Call);
                    Usernames -> eavesdrop_channel(Usernames, Call)
                end;
            'false' -> no_permission_to_eavesdrop(Call)
        end,
    cf_exe:stop(Call).

-spec fields_to_check() -> wh_proplist().
fields_to_check() ->
    [{<<"approved_device_id">>, fun(Id, Call) -> Id == whapps_call:authorizing_id(Call) end}
     ,{<<"approved_user_id">>, fun cf_util:caller_belongs_to_user/2}
     ,{<<"approved_group_id">>, fun cf_util:caller_belongs_to_group/2}
    ].

-spec maybe_allowed_to_eavesdrop(wh_json:object(), whapps_call:call()) ->
                                        boolean().
maybe_allowed_to_eavesdrop(Data, Call) ->
    cf_util:check_value_of_fields(fields_to_check(), 'false', Data, Call).

-spec eavesdrop_channel(ne_binaries(), whapps_call:call()) -> 'ok'.
eavesdrop_channel(Usernames, Call) ->
    case cf_util:find_channels(Usernames, Call) of
        [] -> no_channels(Call);
        Channels -> eavesdrop_a_channel(Channels, Call)
    end.

-spec eavesdrop_a_channel(wh_json:objects(), whapps_call:call()) -> 'ok'.
eavesdrop_a_channel(Channels, Call) ->
    MyUUID = whapps_call:call_id(Call),
    MyMediaServer = whapps_call:switch_nodename(Call),

    lager:debug("looking for channels on my node ~s that aren't me", [MyMediaServer]),

    case sort_channels(Channels, MyUUID, MyMediaServer) of
        {[], []} ->
            lager:debug("no channels available to eavesdrop"),
            no_channels(Call);
        {[], [RemoteUUID | _Remote]} ->
            lager:debug("no calls on my media server, trying ~s", [RemoteUUID]),
            eavesdrop_call(RemoteUUID, Call);
        {[LocalUUID | _Cs], _} ->
            lager:debug("found a call (~s) on my media server", [LocalUUID]),
            eavesdrop_call(LocalUUID, Call)
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
    UUID = wh_json:get_value(<<"uuid">>, Channel),
    case wh_json:get_value(<<"node">>, Channel) of
        MyMediaServer ->
            case UUID of
                MyUUID ->
                    sort_channels(Channels, MyUUID, MyMediaServer, Acc);
                _ ->
                    sort_channels(Channels, MyUUID, MyMediaServer, {[UUID | Local], Remote})
            end;
        _OtherMediaServer ->
            sort_channels(Channels, MyUUID, MyMediaServer, {Local, [UUID | Remote]})
    end.

-spec eavesdrop_call(ne_binary(), whapps_call:call()) -> 'ok'.
eavesdrop_call(UUID, Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:send_command(eavesdrop_cmd(UUID), Call),
    lager:info("caller ~s is being eavesdropper", [whapps_call:caller_id_name(Call)]),
    wait_for_eavesdrop_complete(),
    whapps_call_command:hangup(Call).

-spec wait_for_eavesdrop_complete() -> {'ok', wh_json:object()}.
wait_for_eavesdrop_complete() ->
    case whapps_call_command:receive_event('infinity') of
        {'ok', JObj} ->
            case whapps_util:get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} -> {'ok', JObj};
                {<<"error">>, <<"dialplan">>} -> {'ok', JObj};
                _ -> wait_for_eavesdrop_complete()
            end;
        _ -> wait_for_eavesdrop_complete()
    end.

-spec eavesdrop_cmd(ne_binary()) -> wh_proplist().
eavesdrop_cmd(TargetCallId) ->
    [{<<"Application-Name">>, <<"eavesdrop">>}
     ,{<<"Target-Call-ID">>, TargetCallId}
     ,{<<"Enable-DTMF">>, 'true'}
    ].

-spec find_sip_endpoints(wh_json:object(), whapps_call:call()) ->
                                ne_binaries().
find_sip_endpoints(Data, Call) ->
    case wh_json:get_value(<<"device_id">>, Data) of
        'undefined' ->
            case wh_json:get_value(<<"user_id">>, Data) of
                UserId ->
                    sip_users_from_endpoints(
                      cf_util:find_user_endpoints([UserId], [], Call), Call
                     )
            end;
        DeviceId ->
            sip_users_from_endpoints([DeviceId], Call)
    end.

-spec sip_users_from_endpoints(ne_binaries(), whapps_call:call()) ->
                                      ne_binaries().
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
    whapps_call_command:b_prompt(<<"pickup-no_users">>, Call).

-spec no_channels(whapps_call:call()) -> any().
no_channels(Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:b_prompt(<<"pickup-no_channels">>, Call).

-spec no_permission_to_eavesdrop(whapps_call:call()) -> any().
no_permission_to_eavesdrop(Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:b_prompt(<<"eavesdrop-no_channels">>, Call).
