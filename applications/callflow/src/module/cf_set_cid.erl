%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% params:
%%% caller_id_name & caller_id_number
%%%
%%% if one of them is empty then module will restore original values
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_set_cid).

-include("callflow.hrl").

-export([handle/2]).

-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    NewCIDName = kz_json:get_ne_value(<<"caller_id_name">>, Data, <<>>),
    NewCIDNumber = kz_json:get_ne_value(<<"caller_id_number">>, Data, <<>>),

    lager:info("update with name: ~s num: ~s", [NewCIDName, NewCIDNumber]),

    Updates = [fun(C) -> set_cid_name(C, NewCIDName) end
               ,fun(C) -> set_cid_number(C, NewCIDNumber) end
              ],
    {'ok', Call1} = cf_exe:get_call(Call),
    cf_exe:set_call(kapps_call:exec(Updates, Call1)),
    cf_exe:continue(Call1).


-spec set_cid_name(kapps_call:call(), api(binary())) -> kapps_call:call().
set_cid_name(Call, <<>>) ->
    kapps_call:kvs_erase('rewrite_cid_name', Call);
set_cid_name(Call, Name) ->
    kapps_call:kvs_store('rewrite_cid_name', Name, Call).

-spec set_cid_number(kapps_call:call(), api(binary())) -> kapps_call:call().
set_cid_number(Call, <<>>) ->
    kapps_call:kvs_erase('rewrite_cid_number', Call);
set_cid_number(Call, Number) ->
    kapps_call:kvs_store('rewrite_cid_number', Number, Call).
