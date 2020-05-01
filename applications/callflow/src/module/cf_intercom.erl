%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_intercom).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Number = cf_util:normalize_capture_group(kapps_call:kvs_fetch('cf_capture_group', Call)),
    AccountId = kapps_call:account_id(Call),
    case Number =/= 'undefined'
        andalso cf_flow:lookup(Number, AccountId)
    of
        'false' ->
            lager:debug("capture group is empty and can not be set as destination."),
            cf_exe:stop_bad_destination(Call);
        {'ok', Flow, 'false'} ->
            JObj = suppress_ccv(Data),
            kapps_call_command:set('undefined', JObj, Call),
            cf_exe:branch(kzd_callflows:flow(Flow, kz_json:new()), Call);
        _ ->
            cf_exe:continue(Call)
    end.

-spec suppress_ccv(kz_json:object()) -> kz_json:object().
suppress_ccv(Data) ->
    case kz_json:is_true(<<"barge_calls">>, Data) of
        'false' -> kz_json:from_list([{<<"Auto-Answer-Suppress-Notify">>, <<"true">>}]);
        'true' -> kz_json:from_list([{<<"Auto-Answer-Suppress-Notify">>, <<"false">>}])
    end.
