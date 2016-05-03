%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_branch_bnumber).

-include("callflow.hrl").

%% API
-export([handle/2]).

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    Number = kapps_call:kvs_fetch('cf_capture_group', Call),
    NumberToBranch = case kz_util:is_empty(Number) of
                         'true' -> kapps_call:request_user(Call);
                         'false' -> Number
                     end,
    lager:debug("trying to branch to ~p", [NumberToBranch]),
    cf_exe:continue(NumberToBranch, Call).
