%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cf_privacy).

-include("../callflow.hrl").

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
                           ,cf_exe:get_call(Call)),
            cf_exe:branch(wh_json:get_value(<<"flow">>, CallFlow), Call);
        {'error', _} ->
            cf_exe:stop(Call)
    end.

-spec update_call(ne_binary(), {'ok', whapps_call:call()}) -> 'ok'.
update_call(CaptureGroup, {'ok', Call}) ->
    Routines = [fun(C) ->
                        Request = <<CaptureGroup/binary, "@"
                                    ,(whapps_call:request_realm(C))/binary>>,
                        whapps_call:set_request(Request, C)
                end
                ,fun(C) ->
                         whapps_call:kvs_store('cf_privacy', 'true', C)
                 end
                ,fun(C) ->
                         Name = whapps_config:get_non_empty(<<"callflow">>, <<"privacy_name">>, <<"anonymous">>),
                         whapps_call:set_caller_id_name(Name, C)
                 end
                ,fun(C) ->
                         Number = whapps_config:get_non_empty(<<"callflow">>, <<"privacy_number">>, <<"0000000000">>),
                         whapps_call:set_caller_id_number(Number, C)
                 end
               ],
    cf_exe:set_call(lists:foldl(fun(F, C) -> F(C) end, Call, Routines)).

