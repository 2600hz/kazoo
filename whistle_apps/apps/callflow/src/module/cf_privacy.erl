%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cf_privacy).

-include("../callflow.hrl").

-export([handle/2]).

-spec handle/2 :: (wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CaptureGroup = whapps_call:kvs_fetch(cf_capture_group, Call),
    AccountId = whapps_call:account_id(Call),
    io:format("~p~n", [CaptureGroup]),
    case cf_util:lookup_callflow(CaptureGroup, AccountId) of
        {ok, CallFlow, _} ->
            Mode = wh_json:get_value(<<"mode">>, Data),
            whapps_call_command:privacy(Mode, Call),
             update_request(CaptureGroup
                           ,cf_exe:get_call(Call)),
            cf_exe:branch(wh_json:get_value(<<"flow">>, CallFlow), Call);
        {error, _} ->
            cf_exe:stop(Call)
    end.

-spec update_request/2 :: (ne_binary(), {'ok', whapps_call:call()}) -> 'ok'.
update_request(CaptureGroup, {ok, Call}) ->
    Request = <<CaptureGroup/binary, "@"
                ,(whapps_call:request_realm(Call))/binary>>,
    cf_exe:set_call(whapps_call:set_request(Request, Call)).
    
