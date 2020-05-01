%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Set Caller ID.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`caller_id_name'</dt>
%%%   <dd>Caller ID Name.</dd>
%%%
%%%   <dt>`caller_id_number'</dt>
%%%   <dd>Caller ID Number.</dd>
%%% </dl>
%%%
%%% If one of them is empty then module will restore original values.
%%%
%%%
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_set_cid).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    NewCIDName = kz_json:get_ne_binary_value(<<"caller_id_name">>, Data, <<>>),
    NewCIDNumber = kz_json:get_ne_binary_value(<<"caller_id_number">>, Data, <<>>),

    lager:info("update with name: ~s num: ~s", [NewCIDName, NewCIDNumber]),

    Updates = [{fun set_cid_name/2, NewCIDName}
              ,{fun set_cid_number/2, NewCIDNumber}
              ],
    {'ok', Call1} = cf_exe:get_call(Call),
    cf_exe:set_call(kapps_call:exec(Updates, Call1)),
    cf_exe:continue(Call1).


-spec set_cid_name(kz_term:api_binary(), kapps_call:call()) -> kapps_call:call().
set_cid_name(<<>>, Call) ->
    kapps_call:kvs_erase('rewrite_cid_name', Call);
set_cid_name(Name, Call) ->
    kapps_call:kvs_store('rewrite_cid_name', Name, Call).

-spec set_cid_number(kz_term:api_binary(), kapps_call:call()) -> kapps_call:call().
set_cid_number(<<>>, Call) ->
    kapps_call:kvs_erase('rewrite_cid_number', Call);
set_cid_number(Number, Call) ->
    kapps_call:kvs_store('rewrite_cid_number', Number, Call).
