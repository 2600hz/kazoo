%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Jon Blanton
%%%-------------------------------------------------------------------
-module(cf_prepend_cid).

-include("../callflow.hrl").

-export([handle/2]).

-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    {CIDNamePrefix, CIDNumberPrefix} =
        case wh_json:get_ne_value(<<"action">>, Data) of
            <<"reset">> -> {'undefined', 'undefined'};
            _ ->
                {wh_json:get_ne_value(<<"caller_id_name_prefix">>, Data)
                 ,wh_json:get_ne_value(<<"caller_id_number_prefix">>, Data)
                }
        end,

    lager:info("update prefix with name: ~s num: ~s", [CIDNamePrefix, CIDNumberPrefix]),

    Updates = [fun(C) -> set_cid_name_prefix(C, CIDNamePrefix) end
               ,fun(C) -> set_cid_number_prefix(C, CIDNumberPrefix) end
              ],
    {'ok', Call1} = cf_exe:get_call(Call),
    cf_exe:set_call(whapps_call:exec(Updates, Call1)),
    cf_exe:continue(Call1).

-spec set_cid_name_prefix(whapps_call:call(), api_binary()) -> whapps_call:call().
set_cid_name_prefix(Call, 'undefined') ->
    whapps_call:kvs_store('prepend_cid_name', 'undefined', Call);
set_cid_name_prefix(Call, Prefix) ->
    Prefix1 = case whapps_call:kvs_fetch('prepend_cid_name', Call) of
                  'undefined' -> Prefix;
                  Prepend -> <<Prefix/binary, Prepend/binary>>
              end,
    whapps_call:kvs_store('prepend_cid_name', Prefix1, Call).

-spec set_cid_number_prefix(whapps_call:call(), api_binary()) -> whapps_call:call().
set_cid_number_prefix(Call, 'undefined') ->
    whapps_call:kvs_store('prepend_cid_number', 'undefined', Call);
set_cid_number_prefix(Call, Prefix) ->
    Prefix1 = case whapps_call:kvs_fetch('prepend_cid_number', Call) of
                  'undefined' -> Prefix;
                  Prepend -> <<Prefix/binary, Prepend/binary>>
              end,
    whapps_call:kvs_store('prepend_cid_number', Prefix1, Call).
