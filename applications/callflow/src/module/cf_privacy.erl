%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cf_privacy).

-include("callflow.hrl").

-export([handle/2]).

-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CaptureGroup = whapps_call:kvs_fetch('cf_capture_group', Call),
    AccountId = whapps_call:account_id(Call),
    case cf_util:lookup_callflow(CaptureGroup, AccountId) of
        {'ok', CallFlow, _} ->
            Mode = wh_json:get_value(<<"mode">>, Data),
            whapps_call_command:privacy(Mode, Call),
            update_call(CaptureGroup
                        ,cf_exe:get_call(Call)
                       ),
            cf_exe:branch(wh_json:get_value(<<"flow">>, CallFlow), Call);
        {'error', _} ->
            cf_exe:stop(Call)
    end.

-spec update_call(ne_binary(), {'ok', whapps_call:call()}) -> 'ok'.
update_call(CaptureGroup, {'ok', Call}) ->
    Routines = [{fun whapps_call:set_request/2
                 ,<<CaptureGroup/binary, "@"
                    ,(whapps_call:request_realm(Call))/binary
                  >>
                }
                ,{fun whapps_call:kvs_store/3, 'cf_privacy', 'true'}
                ,{fun whapps_call:set_caller_id_name/2
                  ,whapps_config:get_non_empty(
                     ?CF_CONFIG_CAT
                     ,<<"privacy_name">>
                     ,wh_util:anonymous_caller_id_name()
                    )
                 }
                ,{fun whapps_call:set_caller_id_number/2
                  ,whapps_config:get_non_empty(
                     ?CF_CONFIG_CAT
                     ,<<"privacy_number">>
                     ,wh_util:anonymous_caller_id_number()
                    )
                 }
               ],
    cf_exe:set_call(whapps_call:exec(Routines, Call)).
