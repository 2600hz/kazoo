%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cf_move).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    case kapps_call:owner_id(Call) of
        'undefined' ->
            lager:warning('call has no owner_id', []),
            kapps_call_command:b_prompt(<<"cf-move-no_owner">>, Call),
            cf_exe:stop(Call);
        OwnerId ->
            Channels = get_channels(OwnerId, Call),
            case filter_channels(Channels, Call) of
                {'error', 'no_channel'} ->
                    lager:warning('cannot move call no channel up', []),
                    kapps_call_command:b_prompt(<<"cf-move-no_channel">>, Call),
                    cf_exe:stop(Call);
                {'error', 'too_many_channels'} ->
                    lager:warning('cannot decide which channel to move to, too many channels', []),
                    kapps_call_command:b_prompt(<<"cf-move-too_many_channels">>, Call),
                    cf_exe:stop(Call);
                {'ok', Channel} ->
                    OtherLegId = kz_json:get_value(<<"other_leg">>, Channel),
                    kapps_call_command:pickup(OtherLegId, Call),
                    cf_exe:stop(Call)
            end
    end.

-spec get_channels(ne_binary(), kapps_call:call()) -> dict:dict().
get_channels(OwnerId, Call) ->
    DeviceIds = kz_attributes:owned_by(OwnerId, <<"device">>, Call),
    Req = [{<<"Authorizing-IDs">>, DeviceIds}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kapps_util:amqp_pool_collect(Req
                                     ,fun kapi_call:publish_query_user_channels_req/1
                                     ,{'ecallmgr', 'true'})
    of
        {'error', _E} ->
            lager:error("could not reach ecallmgr channels: ~p", [_E]),
            [];
        {_, Resp} -> clean_channels(Resp)
    end.

-spec clean_channels(kz_json:objects()) -> dict:dict().
-spec clean_channels(kz_json:objects(), dict:dict()) -> dict:dict().
clean_channels(JObjs) ->
    clean_channels(JObjs, dict:new()).

clean_channels([], Dict) ->
    Dict;
clean_channels([JObj|JObjs], Dict) ->
    clean_channels(JObjs, clean_channel(JObj, Dict)).

clean_channel(JObj, Dict) ->
    lists:foldl(
      fun(Channel, D) ->
              UUID = kz_json:get_value(<<"uuid">>, Channel),
              dict:store(UUID, Channel, D)
      end
               ,Dict
               ,kz_json:get_value(<<"Channels">>, JObj, [])
     ).

-spec filter_channels(dict:dict(), kapps_call:call()) ->
                             {'ok', kz_json:object()} |
                             {'error', atom()}.
filter_channels(Channels, Call) ->
    CallId = kapps_call:call_id(Call),
    NChannels = dict:erase(CallId, Channels),
    case dict:to_list(NChannels) of
        [] -> {'error', 'no_channel'};
        [{_, Channel}] ->
            {'ok', Channel};
        [_|_] -> {'error', 'too_many_channels'}
    end.
