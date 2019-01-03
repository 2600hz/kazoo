%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_hangup).

-behaviour(gen_cf_action).

%% API
-export([handle/2]).

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    kapps_call_command:queued_hangup(Call),
    cf_exe:hard_stop(Call).
