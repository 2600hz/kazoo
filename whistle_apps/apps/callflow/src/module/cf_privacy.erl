%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cf_privacy).

-include("../callflow.hrl").

-export([handle/2]).

handle(Data, Call) ->
    % Get the number in the 'calling string'
    CaptureGroup = whapps_call:kvs_fetch(cf_capture_group, Call),
    io:format("CG: ~p ~p~n", [CaptureGroup, Data]),

    % Get the account Id in the call
    AccountId = whapps_call:account_id(Call),
    %Look what to do with the call, get the callflow
    case cf_util:lookup_callflow(CaptureGroup, AccountId) of
        {ok, CallFlow, _} ->
            io:format("~p~n", [CallFlow]),
            % give away the call to the next step of the callflow
            cf_exe:branch(wh_json:get_value(<<"flow">>, CallFlow), Call);
        {error, _} ->
            % If error 'drop the call' nothing to do with it bro
            cf_exe:stop(Call)
    end.
