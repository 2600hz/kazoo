%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Eavesdrop feature.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`user_id'</dt>
%%%   <dd>User ID.</dd>
%%%
%%%   <dt>`device_id'</dt>
%%%   <dd>Device ID.</dd>
%%% </dl>
%%%
%%% One of the two, `user_id' or `device_id', must be defined on the data payload.
%%% Preference is given by most restrictive option set, so `device_id' is checked
%%% for first, then `user_id'.
%%%
%%% `device_id' will only connect to a channel of a specific device,
%%% `user_id' will only connect to channel on any of the user's devices.
%%%
%%%
%%% @author SIPLABS LLC (Mikhail Rodionov)
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_eavesdrop).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2
        ,no_permission_to_eavesdrop/1
        ]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> any().
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

-spec fields_to_check() -> kz_term:proplist().
fields_to_check() ->
    [{<<"approved_device_id">>, fun(Id, Call) -> Id == kapps_call:authorizing_id(Call) end}
    ,{<<"approved_user_id">>, fun cf_util:caller_belongs_to_user/2}
    ,{<<"approved_group_id">>, fun cf_util:caller_belongs_to_group/2}
    ].

-spec maybe_allowed_to_eavesdrop(kz_json:object(), kapps_call:call()) ->
                                        boolean().
maybe_allowed_to_eavesdrop(Data, Call) ->
    cf_util:check_value_of_fields(fields_to_check(), 'false', Data, Call).

-spec eavesdrop_channel(kz_term:ne_binaries(), kapps_call:call()) -> 'ok'.
eavesdrop_channel(Usernames, Call) ->
    case cf_util:find_channels(Usernames, Call) of
        [] -> no_channels(Call);
        Channels -> eavesdrop_a_channel(Channels, Call)
    end.

-spec eavesdrop_a_channel(kz_json:objects(), kapps_call:call()) -> 'ok'.
eavesdrop_a_channel(Channels, Call) ->
    MyUUID = kapps_call:call_id(Call),
    MyMediaServer = kapps_call:switch_nodename(Call),

    lager:debug("looking for channels on my node ~s that aren't me", [MyMediaServer]),

    {_, _, SortedChannels} = lists:foldl(fun channels_sort/2, {MyUUID, MyMediaServer, {[], []}}, Channels),

    case SortedChannels of
        {[], []} ->
            lager:debug("no channels available to eavesdrop"),
            no_channels(Call);
        {[], [RemoteChannel | _Remote]} ->
            lager:info("no calls on my media server, trying redirect to ~s", [kz_json:get_ne_binary_value(<<"node">>, RemoteChannel)]),
            Contact = erlang:iolist_to_binary(["sip:", kapps_call:request(Call)]),
            kapps_call_command:redirect_to_node(Contact, kz_json:get_ne_binary_value(<<"node">>, RemoteChannel), Call);
        {[LocalChannel | _Cs], _} ->
            lager:info("found a call (~s) on my media server", [kz_json:get_ne_binary_value(<<"uuid">>, LocalChannel)]),
            eavesdrop_call(LocalChannel, Call)
    end.

-type channels() :: {kz_json:objects(), kz_json:objects()}.
-type channel_sort_acc() :: {kz_term:ne_binary(), kz_term:ne_binary(), channels()}.

-spec channels_sort(kz_json:object(), channel_sort_acc()) -> channel_sort_acc().
channels_sort(Channel, {MyUUID, MyMediaServer, {Local, Remote}} = Acc) ->
    lager:debug("channel: c: ~s a: ~s n: ~s oleg: ~s", [kz_json:get_ne_binary_value(<<"uuid">>, Channel)
                                                       ,kz_json:is_true(<<"answered">>, Channel)
                                                       ,kz_json:get_ne_binary_value(<<"node">>, Channel)
                                                       ,kz_json:get_ne_binary_value(<<"other_leg">>, Channel)
                                                       ]),
    case kz_json:get_ne_binary_value(<<"node">>, Channel) of
        MyMediaServer ->
            case kz_json:get_ne_binary_value(<<"uuid">>, Channel) of
                MyUUID -> Acc;
                _LocalUUID ->
                    {MyUUID, MyMediaServer, {[Channel | Local], Remote}}
            end;
        _OtherMediaServer ->
            {MyUUID, MyMediaServer, {Local, [Channel | Remote]}}
    end.

-spec eavesdrop_call(kz_json:object(), kapps_call:call()) -> 'ok'.
eavesdrop_call(Channel, Call) ->
    UUID = kz_json:get_ne_binary_value(<<"uuid">>, Channel),
    _Answer = kapps_call_command:b_answer(Call),
    kapps_call_command:send_command(eavesdrop_cmd(UUID), Call),
    lager:info("caller ~s is being eavesdropper", [kapps_call:caller_id_name(Call)]),
    _ = wait_for_eavesdrop_complete(Call),
    cf_exe:stop(Call).

-spec wait_for_eavesdrop_complete(kapps_call:call()) -> 'ok'.
wait_for_eavesdrop_complete(Call) ->
    case kapps_call_command:wait_for_hangup(?MILLISECONDS_IN_HOUR) of
        {'ok', 'channel_hungup'} -> 'ok';
        {'error', 'timeout'} ->
            verify_call_is_active(Call)
    end.

-spec verify_call_is_active(kapps_call:call()) -> 'ok'.
verify_call_is_active(Call) ->
    lager:debug("timed out while waiting for hangup, checking call status"),
    case kapps_call_command:b_channel_status(Call) of
        {'ok', ChannelStatus} ->
            case kz_json:get_ne_binary_value(<<"Status">>, ChannelStatus) of
                <<"active">> -> wait_for_eavesdrop_complete(Call);
                _Status ->
                    lager:debug("channel has status ~s", [_Status])
            end;
        {'error', _E} ->
            lager:debug("failed to get status: ~p", [_E])
    end.

-spec eavesdrop_cmd(kz_term:ne_binary()) -> kz_term:proplist().
eavesdrop_cmd(TargetCallId) ->
    [{<<"Application-Name">>, <<"eavesdrop">>}
    ,{<<"Target-Call-ID">>, TargetCallId}
    ,{<<"Enable-DTMF">>, 'true'}
    ].

-spec find_sip_endpoints(kz_json:object(), kapps_call:call()) ->
                                kz_term:ne_binaries().
find_sip_endpoints(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"device_id">>, Data) of
        'undefined' ->
            UserId = kz_json:get_ne_binary_value(<<"user_id">>, Data),
            sip_users_from_endpoints(
              cf_util:find_user_endpoints([UserId], [], Call), Call);
        DeviceId ->
            sip_users_from_endpoints([DeviceId], Call)
    end.

-spec sip_users_from_endpoints(kz_term:ne_binaries(), kapps_call:call()) ->
                                      kz_term:ne_binaries().
sip_users_from_endpoints(EndpointIds, Call) ->
    lists:foldl(fun(EndpointId, Acc) ->
                        case sip_user_of_endpoint(EndpointId, Call) of
                            'undefined' -> Acc;
                            Username -> [Username|Acc]
                        end
                end, [], EndpointIds).

-spec sip_user_of_endpoint(kz_term:ne_binary(), kapps_call:call()) -> kz_term:api_binary().
sip_user_of_endpoint(EndpointId, Call) ->
    case kz_endpoint:get(EndpointId, Call) of
        {'error', _} -> 'undefined';
        {'ok', Endpoint} ->
            kzd_devices:sip_username(Endpoint)
    end.

-spec no_users(kapps_call:call()) -> any().
no_users(Call) ->
    kapps_call_command:answer(Call),
    kapps_call_command:b_prompt(<<"pickup-no_users">>, Call).

-spec no_channels(kapps_call:call()) -> any().
no_channels(Call) ->
    kapps_call_command:answer(Call),
    kapps_call_command:b_prompt(<<"pickup-no_channels">>, Call).

-spec no_permission_to_eavesdrop(kapps_call:call()) -> any().
no_permission_to_eavesdrop(Call) ->
    kapps_call_command:answer(Call),
    kapps_call_command:b_prompt(<<"eavesdrop-no_channels">>, Call).
