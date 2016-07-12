%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_hangup).

-behaviour(gen_cf_action).

%% API
-export([handle/2]).

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    kapps_call_command:queued_hangup(Call),
    cf_exe:hard_stop(Call).
