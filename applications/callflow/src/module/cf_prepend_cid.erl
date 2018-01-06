%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz INC
%%% @doc
%%% This callflow module can be used to prepend a value (or values) to the caller-id-name and caller-id-number of a call.
%%% @end
%%%
%%% @contributors
%%%   Jon Blanton
%%%   Mark Magnusson
%%%-------------------------------------------------------------------
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
    set_values('undefined', 'undefined', Call);

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
    set_values(Name, Number, Call).

-spec set_values(kz_term:api_ne_binary(), kz_term:api_ne_binary(), kapps_call:call()) -> 'ok'.
set_values(Name, Number, Call) ->
    Updates = [fun(C) -> kapps_call:kvs_store('prepend_cid_number', Number, C) end
              ,fun(C) -> kapps_call:kvs_store('prepend_cid_name', Name, C) end
              ],
    Call1 = kapps_call:exec(Updates, Call),
    cf_exe:set_call(Call1),
    cf_exe:continue(Call1).
