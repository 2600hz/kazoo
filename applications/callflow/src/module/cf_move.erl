%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cf_move).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    case whapps_call:owner_id(Call) of
        'undefined' ->
            lager:warning('call has no owner_id', []),
            whapps_call_command:b_prompt(<<"cf-move-no_owner">>, Call),
            cf_exe:stop(Call);
        OwnerId ->
            Channels = get_channels(OwnerId, Call),
            case filter_channels(Channels, Call) of
                {'error', 'no_channel'} ->
                    lager:warning('cannot move call no channel up', []),
                    whapps_call_command:b_prompt(<<"cf-move-no_channel">>, Call),
                    cf_exe:stop(Call);
                {'error', 'too_many_channels'} ->
                    lager:warning('cannot decide which channel to move to, too many channels', []),
                    whapps_call_command:b_prompt(<<"cf-move-too_many_channels">>, Call),
                    cf_exe:stop(Call);
                {'error', _E} ->
                    lager:error('error while filtering channels ~p', [_E]),
                    cf_exe:stop(Call);
                {'ok', Channel} ->
                    OtherLegId = wh_json:get_value(<<"other_leg">>, Channel),
                    whapps_call_command:pickup(OtherLegId, Call),
                    cf_exe:stop(Call)
            end
    end.
-spec get_channels(ne_binary(), whapps_call:call()) -> wh_json:objects().
get_channels(OwnerId, Call) ->
    DeviceIds = cf_attributes:owned_by(OwnerId, <<"device">>, Call),
    Realm = whapps_call:custom_channel_var(<<"Realm">>, 'undefined', Call),
    Usernames = get_sip_usernames(Call, DeviceIds),
    Req = [{<<"Realm">>, Realm}
           ,{<<"Usernames">>, Usernames}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_collect(Req
                                       ,fun wapi_call:publish_query_user_channels_req/1
                                       ,{'ecallmgr', 'true'})
    of
        {'error', _E} ->
            lager:error("could not reach ecallmgr channels: ~p", [_E]),
            [];
        {_, Resp} -> clean_channels(Resp)
    end.

-spec get_sip_usernames(whapps_call:call(), ne_binaries()) -> ne_binaries().
-spec get_sip_usernames(whapps_call:call(), ne_binaries(), ne_binaries()) -> ne_binaries().
get_sip_usernames(Call, DeviceIds) ->
    get_sip_usernames(Call, DeviceIds, []).

get_sip_usernames(_, [], Acc) -> Acc;
get_sip_usernames(Call, [DeviceId|DeviceIds], Acc) ->
    AccountId = whapps_call:account_id(Call),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, DeviceId) of
        {'error', _E} ->
            lager:error("could not load ~p in ~p : ~p", [AccountId, AccountDb, _E]),
            get_sip_usernames(Call, DeviceIds, Acc);
        {'ok', JObj} ->
            case wh_json:get_value([<<"sip">>, <<"username">>], JObj) of
                'undefined' -> get_sip_usernames(Call, DeviceIds, Acc);
                Username -> get_sip_usernames(Call, DeviceIds, [Username|Acc])
            end
    end.

-spec clean_channels(wh_json:objects()) -> dict().
-spec clean_channels(wh_json:objects(), dict()) -> dict().
clean_channels(JObjs) ->
    clean_channels(JObjs, dict:new()).

clean_channels([], Dict) ->
    Dict;
clean_channels([JObj|JObjs], Dict) ->
    clean_channels(JObjs, clean_channel(JObj, Dict)).

clean_channel(JObj, Dict) ->
    lists:foldl(
        fun(Channel, D) ->
            UUID = wh_json:get_value(<<"uuid">>, Channel),
            dict:store(UUID, Channel, D)
        end
        ,Dict
        ,wh_json:get_value(<<"Channels">>, JObj, [])
    ).

-spec filter_channels(wh_json:objects(), whapps_call:call()) -> {'ok', wh_json:object()} | {'error', atom()}.
filter_channels(Channels, Call) ->
    CallId = whapps_call:call_id(Call),
    NChannels = dict:erase(CallId, Channels),
    case dict:size(NChannels) of
        0 -> {'error', 'no_channel'};
        1 ->
            [{_, Channel}|_] = dict:to_list(NChannels),
            {'ok', Channel};
        _ -> {'error', 'too_many_channels'}
    end.
