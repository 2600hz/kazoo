%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%% Sends request to start the call to recepient when he's available
%%%
%%% data: {
%%% }
%%%
%%% uses cf_capture_group to extract extension number
%%%
%%% usage example
%%%
%%% 1) create a "pattern callflow" with "patterns": ["^\\*7([0-9]*)$"]
%%% 2) create simple callflow with number = 401
%%% 3) dial 401 to start ringing the phones in group, in another phone dial *7401 to make call camping
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_camping_feature).

-include("../callflow.hrl").

-export([handle/2]).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, creates the parameters and branches
%% to cf_group_pickup.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    lager:info("Camping feature started"),
    Number = whapps_call:kvs_fetch('cf_capture_group', Call),
    {'ok', Callflow, _} = cf_util:lookup_callflow(Number, whapps_call:account_id(Call)),
    TargetId = wh_json:get_ne_value([<<"flow">>, <<"data">>, <<"id">>], Callflow),
    TargetType = wh_json:get_ne_value([<<"flow">>, <<"module">>], Callflow),
    Usernames = case TargetType of
                   <<"device">> -> cf_util:sip_users_from_device_ids([TargetId], Call);
                   <<"user">> ->
                       EPs = cf_util:find_user_endpoints([TargetId], [], Call),
                       cf_util:sip_users_from_device_ids(EPs, Call)
               end,
    Channels = cf_util:find_channels(Usernames, Call),
    case Channels of
        [] -> no_channels(TargetId, TargetType, Call);
        _ -> has_channels(TargetId, TargetType, Number, Call)
    end.

-spec get_sip_usernames_for_target(ne_binary(), ne_binary(), whapps_call:call()) -> wh_json:object().
get_sip_usernames_for_target(TargetId, TargetType, Call) ->
    Targets = case TargetType of
                  <<"user">> -> cf_attributes:owned_by(TargetId, <<"device">>, Call);
                  <<"device">> -> [TargetId];
                  _Else ->
                      lager:debug("Can't found camping target's type. May be wrong extension number?"),
                      []
              end,
    lists:map(
        fun(DevId) ->
            {'ok', JObj} = couch_mgr:open_cache_doc(whapps_call:account_db(Call), DevId),
            wh_json:get_value([<<"sip">>, <<"username">>], JObj)
        end
        ,Targets
    ).

-spec no_channels(ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
no_channels(TargetId, <<"device">>, Call) ->
    Data = wh_json:from_list([{<<"id">>, TargetId}]),
    cf_device:handle(Data, Call);
no_channels(TargetId, <<"user">>, Call) ->
    Data = wh_json:from_list([{<<"id">>, TargetId}]),
    cf_user:handle(Data, Call);
no_channels(_, Type, Call) ->
    lager:info("Unknown target's type: ~s", [Type]),
    cf_exe:stop(Call).

-spec has_channels(ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
has_channels(TargetId, TargetType, Number, Call) ->
    Targets = get_sip_usernames_for_target(TargetId, TargetType, Call),
    camper_channel_tracker:add_request(whapps_call:account_db(Call)
        ,{whapps_call:authorizing_id(Call)
            ,whapps_call:authorizing_type(Call)
        }
        ,Number
        ,Targets
    ),
    whapps_call_command:answer(Call),
    whapps_call_command:b_say(<<"request accepted">>, Call),
    cf_exe:stop(Call).
