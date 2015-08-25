%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_hangup).

%% API
-export([handle/2]).

-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    whapps_call_command:queued_hangup(Call),
    cf_exe:hard_stop(Call).
