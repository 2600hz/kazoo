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

-spec api_call/2 :: (hook_types(), fun((atom()) -> Resp)) -> {'ok', Resp} | {'error', api_call_errors()}.
api_call(BindEvent, ApiFun) when is_function(ApiFun, 1) ->
    Wapi = list_to_binary([<<"wapi_">>, wh_util:to_list(BindEvent)]),
    try
        ApiMod = wh_util:to_atom(Wapi),
        ApiFun(ApiMod)
    catch
        error:badarg ->
            ?LOG_SYS("api module ~s not found", [Wapi]),
            case code:where_is_file(wh_util:to_list(<<Wapi/binary, ".beam">>)) of
                non_existing ->
                    ?LOG_SYS("beam file not found for ~s, fail", [Wapi]),
                    {error, non_existing};
                _Path ->
                    ?LOG_SYS("beam file found: ~s", [_Path]),
                    wh_util:to_atom(Wapi, true), %% put atom into atom table
                    api_call(BindEvent, ApiFun)
            end;
        error:undef ->
            ?LOG_SYS("module ~s doesn't exist or fun isn't exported", [Wapi]),
            {error, undefined};
        E:R ->
            ST = erlang:get_stacktrace(),
            ?LOG("exception in executing ApiFun: ~p:~p", [E,R]),
            ?LOG_STACKTRACE(ST),
            {error, R}
    end.
