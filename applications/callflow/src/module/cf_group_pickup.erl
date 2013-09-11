%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%% Pickup a call in the specified group
%%%
%%% data: {
%%%   "group_id":"_id_"
%%% }
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
    GroupId = wh_json:get_ne_value(<<"group_id">>, Data),

    case find_sip_users(GroupId, Call) of
        [] -> no_users_in_group(Call);
        Usernames -> connect_to_ringing_channel(Usernames, Call)
    end,
    cf_exe:stop(Call).

connect_to_ringing_channel(Usernames, Call) ->
    case find_channels(Usernames, Call) of
        [] -> no_channels_ringing(Call);
        Channels -> connect_to_a_channel(Channels, Call)
    end.

connect_to_a_channel(Channels, Call) ->
    MyUUID = whapps_call:call_id(Call),
    MyMediaServer = whapps_call:switch_nodename(Call),

    lager:debug("looking for channels on my node ~s that aren't me", [MyMediaServer]),

    case sort_channels(Channels, MyUUID, MyMediaServer) of
        {[], []} ->
            lager:debug("no channels available to pickup"),
            no_channels_ringing(Call);
        {[], Remote} ->
            lager:debug("no unanswered calls on my media server"),
            maybe_connect_to_a_remote_channel(Remote, Call);
        {[RingingCallUUID|_Cs], _} ->
            lager:debug("found a call (~s) on my media server", [RingingCallUUID]),
            intercept_call(RingingCallUUID, Call)
    end.

maybe_connect_to_a_remote_channel([{_, MediaServer}|_], Call) ->
    'ok'.

sort_channels(Channels, MyUUID, MyMediaServer) ->
    sort_channels(Channels, MyUUID, MyMediaServer, {[], []}).
sort_channels([Channel|Channels], MyUUID, MyMediaServer, {Local, Remote}=Acc) ->
    case wh_json:is_false(<<"answered">>, Channel) of
        'true' ->
            sort_channels(Channels, MyUUID, MyMediaServer, Acc);
        'false' ->
            UUID = wh_json:get_value(<<"uuid">>, Channel),

            case wh_json:get_value(<<"node">>, Channel) of
                MyMediaServer ->
                    case UUID of
                        MyUUID ->
                            sort_channels(Channels, MyUUID, MyMediaServer, Acc);
                        UUID ->
                            sort_channels(Channels, MyUUID, MyMediaServer, {[{UUID, MyMediaServer} | Local], Remote})
                    end;
                MediaServer ->
                    sort_channels(Channels, MyUUID, MyMediaServer, {Local, [{UUID, MediaServer} | Remote]})
            end
    end.

intercept_call(UUID, Call) ->
    Res = whapps_call_command:b_call_pickup(UUID, Call),
    lager:debug("result of intercept: ~p", [Res]).

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

-spec find_sip_users(ne_binary(), whapps_call:call()) -> ne_binaries().
find_sip_users(GroupId, Call) ->
    GroupsJObj = cf_attributes:groups(Call),
    case [wh_json:get_value(<<"value">>, JObj)
          || JObj <- GroupsJObj,
             wh_json:get_value(<<"id">>, JObj) =:= GroupId
         ]
    of
        [] -> [];
        [GroupEndpoints] ->
            Ids = wh_json:get_keys(GroupEndpoints),
            sip_users_from_endpoints(find_endpoints(Ids, GroupEndpoints, Call), Call)
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

-spec find_endpoints(ne_binaries(), wh_json:object(), whapps_call:call()) -> ne_binaries().
find_endpoints(Ids, GroupEndpoints, Call) ->
    {DeviceIds, UserIds} =
        lists:partition(fun(Id) ->
                                wh_json:get_value([Id, <<"type">>], GroupEndpoints) =:= <<"device">>
                        end, Ids),
    find_user_endpoints(UserIds, lists:sort(DeviceIds), Call).

-spec find_user_endpoints(ne_binaries(), ne_binaries(), whapps_call:call()) -> ne_binaries().
find_user_endpoints([], DeviceIds, _) -> DeviceIds;
find_user_endpoints([UserId|UserIds], DeviceIds, Call) ->
    UserDeviceIds = cf_attributes:owned_by(UserId, <<"device">>, Call),
    find_user_endpoints(UserIds, lists:merge(lists:sort(UserDeviceIds), DeviceIds), Call).

no_users_in_group(Call) ->
    whapps_call_command:b_say(<<"no users found in group">>, Call).
no_channels_ringing(Call) ->
    whapps_call_command:b_say(<<"no channels ringing">>, Call).
