%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_branch_bnumber).

-include("../callflow.hrl").

%% API
-export([handle/2]).

-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    Number = whapps_call:kvs_fetch('cf_capture_group', Call),
    NumberToBranch = case wh_util:is_empty(Number) of
                         'true' -> whapps_call:request_user(Call);
                         'false' -> Number
                     end,
    lager:debug("Trying to branch to ~p", [NumberToBranch]),
    cf_exe:continue(NumberToBranch, Call).
