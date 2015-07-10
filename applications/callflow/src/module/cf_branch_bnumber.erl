%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_branch_bnumber).

%% API
-export([handle/2]).

-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    Number = whapps_call:kvs_fetch('cf_capture_group', Call),
    cf_exe:continue(Number, Call).
