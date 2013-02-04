%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 24 Jan 2012 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(webhooks_util).

-export([api_call/2]).

-include("webhooks.hrl").

-spec api_call(hook_types(), fun((atom()) -> Resp)) -> Resp | {'error', 'non_existing' | 'undefined' | atom()}.
api_call(BindEvent, ApiFun) when is_function(ApiFun, 1) ->
    Wapi = list_to_binary([<<"wapi_">>, wh_util:to_list(BindEvent)]),
    try
        ApiMod = wh_util:to_atom(Wapi),
        ApiFun(ApiMod)
    catch
        error:badarg ->
            lager:debug("api module ~s not found", [Wapi]),
            case code:where_is_file(wh_util:to_list(<<Wapi/binary, ".beam">>)) of
                non_existing ->
                    lager:debug("beam file not found for ~s, fail", [Wapi]),
                    {error, non_existing};
                _Path ->
                    lager:debug("beam file found: ~s", [_Path]),
                    wh_util:to_atom(Wapi, true), %% put atom into atom table
                    api_call(BindEvent, ApiFun)
            end;
        error:undef ->
            lager:debug("module ~s doesn't exist or fun isn't exported", [Wapi]),
            {error, undefined};
        _E:R ->
            ST = erlang:get_stacktrace(),
            lager:debug("exception in executing ApiFun: ~p:~p", [_E,R]),
            [lager:debug("st: ~p", [T]) || T <- ST],
            {error, R}
    end.
