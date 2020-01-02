%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc This callflow module can be used to prepend a value (or values) to the
%%% caller ID (Name and Number) of a call.
%%% @author Jon Blanton
%%% @author Mark Magnusson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_prepend_cid).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    handle(Data, Call, kz_json:get_ne_binary_value(<<"action">>, Data, <<"prepend">>)).

-spec handle(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
handle(_Data, Call, <<"reset">>) ->
    lager:info("reset prepend cid"),
    Updates = [fun(C) -> kapps_call:kvs_erase('prepend_cid_number', C) end
              ,fun(C) -> kapps_call:kvs_erase('prepend_cid_name', C) end
              ],
    Call1 = kapps_call:exec(Updates, Call),
    cf_exe:set_call(Call1),
    cf_exe:continue(Call1);

handle(Data, Call, <<"prepend">>) ->
    NamePre = kz_json:get_ne_binary_value(<<"caller_id_name_prefix">>, Data, <<>>),
    NumberPre = kz_json:get_ne_binary_value(<<"caller_id_number_prefix">>, Data, <<>>),

    OrigPreName = kapps_call:kvs_fetch(<<"prepend_cid_name">>, <<>>, Call),
    OrigPreNum  = kapps_call:kvs_fetch(<<"prepend_cid_number">>, <<>>, Call),

    {Name, Number}
        = case kz_json:get_ne_binary_value(<<"apply_to">>, Data, <<"current">>) of
              <<"original">> ->
                  {NamePre, NumberPre};
              <<"current">> ->
                  {<<NamePre/binary, OrigPreName/binary>>
                  ,<<NumberPre/binary, OrigPreNum/binary>>
                  }
          end,
    lager:info("update prepend cid to <~s> ~s", [Name, Number]),
    Updates = [fun(C) -> kapps_call:kvs_store('prepend_cid_number', Number, C) end
              ,fun(C) -> kapps_call:kvs_store('prepend_cid_name', Name, C) end
              ],
    Call1 = kapps_call:exec(Updates, Call),
    cf_exe:set_call(Call1),
    cf_exe:continue(Call1).
