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

-record(call_target, {id :: ne_binary()
                      ,type :: ne_binary()
                      ,no_match_flag :: boolean()
                      ,number :: ne_binary()
                     }).
-type call_target() :: #call_target{}.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, creates the parameters and branches
%% to cf_group_pickup.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    whapps_call_command:answer(Call),
    lager:info("Camping feature started"),
    Number = whapps_call:kvs_fetch('cf_capture_group', Call),
    {'ok', Callflow, IsNoMatch} = cf_util:lookup_callflow(Number, whapps_call:account_id(Call)),
    TargetId = wh_json:get_ne_value([<<"flow">>, <<"data">>, <<"id">>], Callflow),
    TargetType = wh_json:get_ne_value([<<"flow">>, <<"module">>], Callflow),
    Usernames = case TargetType of
                   <<"device">> -> cf_util:sip_users_from_device_ids([TargetId], Call);
                   <<"user">> ->
                       EPs = cf_util:find_user_endpoints([TargetId], [], Call),
                       cf_util:sip_users_from_device_ids(EPs, Call);
                   <<"offnet">> ->
                       []
               end,
    Channels = cf_util:find_channels(Usernames, Call),
    Target = #call_target{id = TargetId
                          ,type = TargetType
                          ,no_match_flag = IsNoMatch
                          ,number = Number
                         },
    case Channels of
        [] -> no_channels(Target, Call);
        _ -> has_channels(Target, Call)
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

-spec no_channels(call_target(), whapps_call:call()) -> 'ok'.
no_channels(#call_target{id = TargetId
                         ,type = <<"device">>
                         ,no_match_flag = 'false'
                        }
            ,Call) ->
    Data = wh_json:from_list([{<<"id">>, TargetId}]),
    cf_device:handle(Data, Call);
no_channels(#call_target{id = TargetId
                        ,type = <<"user">>
                        ,no_match_flag = 'false'
                        }
            ,Call) ->
    Data = wh_json:from_list([{<<"id">>, TargetId}]),
    cf_user:handle(Data, Call);
no_channels(#call_target{type = <<"offnet">>
                        ,no_match_flag = 'true'
                        ,number = Number
                        }
            ,Call) ->
    Msg = wh_json:from_list([{<<"Number">>, Number}
                             ,{<<"Call">>, whapps_call:to_json(Call)}
                            ]),
    JObj = wh_json:from_list([{<<"Delegate-Message">>, Msg}
                              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ]),
    wapi_delegate:publish_delegate(<<"camper">>, JObj, <<"offnet">>),
    cf_exe:stop(Call);
no_channels(_Target, Call) ->
    lager:info("Unknown target: ~s", [_Target]),
    cf_exe:stop(Call).

-spec has_channels(call_target(), whapps_call:call()) -> 'ok'.
has_channels(#call_target{id = TargetId
                          ,type = TargetType
                          ,number = Number
                         }, Call) ->
    Targets = get_sip_usernames_for_target(TargetId, TargetType, Call),
    Msg = wh_json:from_list([{<<"Account-DB">>, whapps_call:account_db(Call)}
                             ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}
                             ,{<<"Authorizing-Type">>, whapps_call:authorizing_type(Call)}
                             ,{<<"Number">>, Number}
                             ,{<<"Targets">>, Targets}
                            ]),
    JObj = wh_json:from_list([{<<"Delegate-Message">>, Msg}
                              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ]),
    wapi_delegate:publish_delegate(<<"camper">>, JObj, <<"onnet">>),
    cf_exe:stop(Call).
