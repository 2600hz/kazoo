%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc Branch the call to the capture group if found, otherwise
%%% continue with original callee number.
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_branch_bnumber).

-behaviour(gen_cf_action).

-include("callflow.hrl").

%% API
-export([handle/2]).

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    Number = kapps_call:kvs_fetch('cf_capture_group', Call),
    NumberToBranch = case kz_term:is_empty(Number) of
                         'true' -> kapps_call:request_user(Call);
                         'false' -> Number
                     end,
    lager:debug("trying to branch to ~p", [NumberToBranch]),
    cf_exe:continue(NumberToBranch, Call).
