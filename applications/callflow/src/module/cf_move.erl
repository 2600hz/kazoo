%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_move).
-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    _ = case kapps_call:owner_id(Call) of
            'undefined' ->
                lager:warning("call has no owner_id"),
                kapps_call_command:b_prompt(<<"cf-move-no_owner">>, Call);
            OwnerId ->
                Channels = get_channels(OwnerId, Call),
                case filter_channels(Channels, Call) of
                    {'error', 'no_channel'} ->
                        lager:warning("cannot move call no channel up"),
                        kapps_call_command:b_prompt(<<"cf-move-no_channel">>, Call);
                    {'error', 'too_many_channels'} ->
                        lager:warning("cannot decide which channel to move to, too many channels"),
                        kapps_call_command:b_prompt(<<"cf-move-too_many_channels">>, Call);
                    {'ok', Channel} ->
                        OtherLegId = kz_json:get_ne_binary_value(<<"other_leg">>, Channel),
                        kapps_call_command:b_pickup(OtherLegId, Call)
                end
        end,
    cf_exe:stop(Call).

-spec get_channels(kz_term:ne_binary(), kapps_call:call()) -> dict:dict().
get_channels(OwnerId, Call) ->
    DeviceIds = kz_attributes:owned_by(OwnerId, <<"device">>, Call),
    lager:debug("devices owned by ~p: ~p", [OwnerId, DeviceIds]),
    Req = [{<<"Authorizing-IDs">>, DeviceIds}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    Pub = fun kapi_call:publish_query_user_channels_req/1,
    case kz_amqp_worker:call_collect(Req, Pub, {'ecallmgr', 'true'}) of
        {'error', _E} ->
            lager:error("could not reach ecallmgr channels: ~p", [_E]),
            dict:new();
        {_, Resp} ->
            clean_channels(Resp)
    end.

-spec clean_channels(kz_json:objects()) -> dict:dict().
clean_channels(JObjs) ->
    lists:foldl(fun clean_channels_fold/2, dict:new(), JObjs).

-spec clean_channels_fold(kz_json:object(), dict:new()) -> dict:new().
clean_channels_fold(JObj, Dict) ->
    lists:foldl(fun(Channel, D) ->
                        UUID = kz_json:get_ne_binary_value(<<"uuid">>, Channel),
                        dict:store(UUID, Channel, D)
                end
               ,Dict
               ,kz_json:get_list_value(<<"Channels">>, JObj, [])
               ).

-spec filter_channels(dict:dict(), kapps_call:call()) ->
                             {'ok', kz_json:object()} |
                             {'error', no_channel | too_many_channels}.
filter_channels(Channels, Call) ->
    CallId = kapps_call:call_id(Call),
    case dict:to_list(dict:erase(CallId, Channels)) of
        [] -> {'error', 'no_channel'};
        [{_UUID, Channel}] ->
            lager:debug("selected channel ~s", [_UUID]),
            {'ok', Channel};
        [_|_]=_Channels ->
            _ = [lager:debug("found channel ~s: ~s", [_UUID, kz_json:encode(_Channel)])
                 || {_UUID, _Channel} <- _Channels
                ],
            {'error', 'too_many_channels'}
    end.
