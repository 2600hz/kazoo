%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Transfers caller to the extension extracted in the regex.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`takeback_dtmf'</dt>
%%%   <dd>Transferor can cancel the transfer request: `2'.</dd>
%%%
%%%   <dt>`moh'</dt>
%%%   <dd>Media ID for custom music on hold.</dd>
%%%
%%%   <dt>`target'</dt>
%%%   <dd>Extension number or DID to transfer to.</dd>
%%%
%%%   <dt>`ringback'</dt>
%%%   <dd>Ringback to play to transferor: `"%(2000,4000,440,480)"'.</dd>
%%% </dl>
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_transfer).
-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

-spec handle(kz_json:object(), kapps_call:call()) -> no_return().
handle(Data, Call) ->
    kapps_call:put_callid(Call),
    [Capture|_] = kz_json:get_list_value(<<"captures">>, Data, [<<"no_match">>]),
    TransferTo = kz_json:get_ne_binary_value(<<"target">>, Data, Capture),
    TransferLeg = transfer_leg(kz_json:get_ne_binary_value(<<"leg">>, Data, default_transfer_leg(Capture))),

    case kz_json:get_ne_binary_value(<<"transfer_type">>, Data, <<"blind">>) of
        <<"attended">> -> kapps_call_command:transfer(<<"attended">>, TransferTo, Call);
        <<"blind">> -> kapps_call_command:transfer(<<"blind">>, TransferTo, TransferLeg, Call)
    end,
    {'stop', Call}.

-spec default_transfer_leg(kz_term:ne_binary()) -> kz_term:ne_binary().
default_transfer_leg(<<"no_match">>) -> <<"self">>;
default_transfer_leg(_) -> <<"bleg">>.

-spec transfer_leg(kz_term:ne_binary()) -> kz_term:api_ne_binary().
transfer_leg(<<"self">>) -> 'undefined';
transfer_leg(Leg) -> Leg.
