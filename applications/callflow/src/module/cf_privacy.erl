%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_privacy).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    AccountId = kapps_call:account_id(Call),
    Number = cf_util:normalize_capture_group(kapps_call:kvs_fetch('cf_capture_group', Call), AccountId),
    handle(Data, Call, AccountId, Number).

-spec handle(kz_json:object(), kapps_call:call(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> 'ok'.
handle(Data, Call, _AccountId, 'undefined') ->
    UpdatedCall = update_call('undefined', Data, Call),
    cf_exe:continue(UpdatedCall);
handle(Data, Call, AccountId, Number) ->
    case cf_flow:lookup(Number, AccountId) of
        {'ok', CallFlow, _NoMatch} ->
            UpdatedCall = update_call(Number, Data, Call),
            cf_exe:branch(kzd_callflows:flow(CallFlow), UpdatedCall);
        {'error', _} ->
            cf_exe:stop(Call)
    end.

-spec update_call(kz_term:api_ne_binary(), kz_json:object(), kapps_call:call()) -> kapps_call:call().
update_call(Number, Data, Call) ->
    Mode = kz_json:get_ne_binary_value(<<"mode">>, Data, <<"full">>),
    Strategy = kz_json:get_ne_binary_value(<<"endpoint_strategy">>, Data, <<"overwrite">>),

    Routines = [{fun kapps_call:set_custom_channel_vars/2
                ,kz_privacy:flags_by_mode(Mode)
                }
               ,{fun kapps_call:kvs_store/3
                ,<<"use_endpoint_privacy">>
                ,should_use_endpoint_privacy(Strategy)
                }
                | update_number_func(Number, Call, Mode, Strategy)
               ],
    cf_exe:update_call(kapps_call:exec(Routines, Call)).

-spec update_number_func(kz_term:api_ne_binary(), kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary()) -> [any()].
update_number_func('undefined', _Call, Mode, Strategy) ->
    lager:debug("setting privacy mode to ~s. use endpoint privacy: ~s"
               ,[Mode, Strategy]
               ),
    [];
update_number_func(Number, Call, Mode, Strategy) ->
    lager:debug("setting privacy mode for number ~s to ~s. use endpoint privacy: ~s"
               ,[Number, Mode, Strategy]
               ),
    [{fun kapps_call:set_request/2
     ,list_to_binary([Number, "@", kapps_call:request_realm(Call)])
     }].

-spec should_use_endpoint_privacy(kz_term:ne_binary()) -> boolean().
should_use_endpoint_privacy(<<"overwrite">>) -> 'false';
should_use_endpoint_privacy(<<"merge">>) -> 'true'.
