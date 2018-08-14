%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_ledger).

-include("kzl.hrl").

-export([new/0
        ,save/1, save/2
        ]).

-export([type/1, set_type/2
        ,amount/1, set_amount/2
        ,description/1, set_description/2
        ,source/1
        ,source_service/1, set_source_service/2
        ,source_id/1, set_source_id/2
        ,usage/1
        ,usage_type/1, set_usage_type/2
        ,usage_quantity/1, set_usage_quantity/2
        ,usage_unit/1, set_usage_unit/2
        ,period/1
        ,period_start/1, set_period_start/2
        ,period_end/1, set_period_end/2
        ,account/1
        ,account_id/1, set_account_id/2
        ,account_name/1, set_account_name/2
        ,metadata/1, metadata/2
        ,set_metadata/2, set_metadata/3
        ]).


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new() -> kz_json:object().
new() ->
    kz_json:new().

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save(ledger()) -> {'ok', ledger()} | {'error', any()}.
save(Ledger) ->
    LedgerId = account_id(Ledger),
    save(Ledger, LedgerId).

-spec save(ledger(), kz_term:ne_binary()) -> {'ok', ledger()} | {'error', any()}.
save(Ledger, LedgerId) ->
    Props = [{<<"pvt_type">>, ?PVT_TYPE}
            ,{<<"pvt_modified">>, kz_time:now_s()}
            ,{<<"pvt_account_id">>, LedgerId}
             | maybe_add_id(Ledger)
            ],
    kazoo_modb:save_doc(LedgerId, kz_json:set_values(Props, Ledger)).

-spec maybe_add_id(ledger()) -> kz_term:proplist().
maybe_add_id(Ledger) ->
    case kz_doc:id(Ledger) of
        'undefined' ->
            {Year, Month, _} = erlang:date(),
            [{<<"_id">>, list_to_binary([kz_term:to_binary(Year)
                                        ,kz_date:pad_month(Month)
                                        ,"-"
                                        ,create_hash(Ledger)
                                        ])
             }
            ,{<<"pvt_created">>, kz_time:now_s()}
            ];
        _ -> []
    end.

-spec create_hash(ledger()) -> kz_term:ne_binary().
create_hash(Ledger) ->
    Props = [{<<"source">>, source(Ledger)}
            ,{<<"account">>, account(Ledger)}
            ,{<<"usage">>, usage(Ledger)}
            ,{<<"period">>, period(Ledger)}
            ,{<<"type">>, type(Ledger)}
            ],
    kz_binary:md5(kz_json:encode(kz_json:from_list(Props))).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec type(ledger()) -> kz_term:ne_binary().
type(Ledger) ->
    kz_json:get_ne_binary_value(?PVT_LEDGER_TYPE, Ledger).

-spec set_type(ledger(), kz_term:ne_binary()) -> ledger().
set_type(L, Type) ->
    kz_json:set_value(?PVT_LEDGER_TYPE, Type, L).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec amount(ledger()) -> number().
amount(Ledger) ->
    kz_json:get_number_value(?AMOUNT, Ledger, 0).

-spec set_amount(ledger(), number()) -> ledger().
set_amount(L, Amount) ->
    kz_json:set_value(?AMOUNT, Amount, L).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec description(ledger()) -> kz_term:ne_binary().
description(Ledger) ->
    kz_json:get_ne_binary_value(?DESC, Ledger).

-spec set_description(ledger(), kz_term:ne_binary()) -> ledger().
set_description(L, Desc) ->
    kz_json:set_value(?DESC, Desc, L).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec source(ledger()) -> kz_json:object().
source(Ledger) ->
    kz_json:get_json_value(?SRC, Ledger).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec source_service(ledger()) -> kz_term:ne_binary().
source_service(Ledger) ->
    kz_json:get_ne_binary_value(?SRC_SERVICE, Ledger).

-spec set_source_service(ledger(), kz_term:ne_binary()) -> ledger().
set_source_service(L, Service) ->
    kz_json:set_value(?SRC_SERVICE, Service, L).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec source_id(ledger()) -> kz_term:ne_binary().
source_id(Ledger) ->
    kz_json:get_ne_binary_value(?SRC_ID, Ledger).

-spec set_source_id(ledger(), kz_term:ne_binary()) -> ledger().
set_source_id(L, Id) ->
    kz_json:set_value(?SRC_ID, Id, L).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage(ledger()) -> kz_json:object().
usage(Ledger) ->
    kz_json:get_json_value(?USAGE, Ledger).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage_type(ledger()) -> kz_term:ne_binary().
usage_type(Ledger) ->
    kz_json:get_ne_binary_value(?USAGE_TYPE, Ledger).

-spec set_usage_type(ledger(), kz_term:ne_binary()) -> ledger().
set_usage_type(L, Type) ->
    kz_json:set_value(?USAGE_TYPE, Type, L).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage_quantity(ledger()) -> number().
usage_quantity(Ledger) ->
    kz_json:get_number_value(?USAGE_QUANTITY, Ledger).

-spec set_usage_quantity(ledger(), number()) -> ledger().
set_usage_quantity(L, Quantity) ->
    kz_json:set_value(?USAGE_QUANTITY, Quantity, L).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec usage_unit(ledger()) -> kz_term:ne_binary().
usage_unit(Ledger) ->
    kz_json:get_ne_binary_value(?USAGE_UNIT, Ledger).

-spec set_usage_unit(ledger(), kz_term:ne_binary()) -> ledger().
set_usage_unit(L, Unit) ->
    kz_json:set_value(?USAGE_UNIT, Unit, L).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec period(ledger()) -> kz_json:object().
period(Ledger) ->
    kz_json:get_json_value(?PERIOD, Ledger).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec period_start(ledger()) -> integer().
period_start(Ledger) ->
    kz_json:get_integer_value(?PERIOD_START, Ledger).

-spec set_period_start(ledger(), integer()) -> ledger().
set_period_start(L, Start) ->
    kz_json:set_value(?PERIOD_START, Start, L).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec period_end(ledger()) -> integer().
period_end(Ledger) ->
    kz_json:get_integer_value(?PERIOD_END, Ledger).

-spec set_period_end(ledger(), integer()) -> ledger().
set_period_end(L, End) ->
    kz_json:set_value(?PERIOD_END, End, L).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account(ledger()) -> kz_json:object().
account(Ledger) ->
    kz_json:get_json_value(?ACCOUNT, Ledger).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_id(ledger()) -> kz_term:ne_binary().
account_id(Ledger) ->
    kz_json:get_ne_binary_value(?ACCOUNT_ID, Ledger).

-spec set_account_id(ledger(), kz_term:ne_binary()) -> ledger().
set_account_id(L, Start) ->
    kz_json:set_value(?ACCOUNT_ID, Start, L).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_name(ledger()) -> kz_term:ne_binary().
account_name(Ledger) ->
    kz_json:get_ne_binary_value(?ACCOUNT_NAME, Ledger).

-spec set_account_name(ledger(), kz_term:ne_binary()) -> ledger().
set_account_name(L, End) ->
    kz_json:set_value(?ACCOUNT_NAME, End, L).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec metadata(ledger()) -> ledger().
metadata(Ledger) ->
    kz_json:get_value(?METADATA, Ledger).

-spec metadata(ledger(), kz_term:ne_binary() | kz_term:ne_binaries()) -> ledger().
metadata(Ledger, Key)
  when is_binary(Key)->
    kz_json:get_value(?METADATA_KEY(Key), Ledger);
metadata(Ledger, Keys)
  when is_list(Keys)->
    kz_json:get_value(?METADATA_KEYS(Keys), Ledger).

-spec set_metadata(ledger(), kz_json:object() | kz_term:proplist()) -> ledger().
set_metadata(Ledger, List)
  when is_list(List) ->
    kz_json:set_values([{?METADATA_KEY(K), V} || {K,V} <- List], Ledger);
set_metadata(Ledger, JObj) ->
    kz_json:set_value(?METADATA, JObj, Ledger).

-spec set_metadata(ledger(), kz_term:ne_binary(), kz_json:json_term()) -> ledger().
set_metadata(Ledger, Key, Value) ->
    kz_json:set_value(?METADATA_KEY(Key), Value, Ledger).


%%------------------------------------------------------------------------------
%% Internal Function Definitions
%%------------------------------------------------------------------------------
