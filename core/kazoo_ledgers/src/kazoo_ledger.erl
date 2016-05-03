%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600HZ, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(kazoo_ledger).

-include("kzl.hrl").

-export([
    new/0
    ,save/1
]).

-export([
    type/1, set_type/2
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
]).


%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec new() -> kz_json:object().
new() ->
    kz_json:new().

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec save(kz_json:object()) -> {'ok', kz_json:object()} | {'error', any()}.
-spec save(kz_json:object(), ne_binary(), boolean()) -> {'ok', kz_json:object()} | {'error', any()}.
save(Ledger) ->
    AccountId = account_id(Ledger),
    IsReseller = kz_services:is_reseller(AccountId),
    JObj =
        kz_json:set_values([
            {<<"pvt_type">>, ?PVT_TYPE}
            ,{<<"pvt_modified">>, kz_util:current_tstamp()}
            ,{<<"pvt_created">>, kz_util:current_tstamp()}
        ], Ledger),
    save(JObj, AccountId, IsReseller).

save(JObj, AccountId, 'true') ->
    kazoo_modb:save_doc(AccountId, JObj);
save(JObj, AccountId, 'false') ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    case kazoo_modb:save_doc(ResellerId, JObj) of
        {'error', _}=Err -> Err;
        {'ok', _} ->
            kazoo_modb:save_doc(AccountId, JObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec type(ledger()) -> ne_binary().
type(Ledger) ->
    kz_json:get_ne_binary_value(?PVT_LEDGER_TYPE, Ledger).

-spec set_type(ledger(), ne_binary()) -> ledger().
set_type(L, Type) ->
    kz_json:set_value(?PVT_LEDGER_TYPE, Type, L).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec amount(ledger()) -> number().
amount(Ledger) ->
    kz_json:get_number_value(?AMOUNT, Ledger).

-spec set_amount(ledger(), number()) -> ledger().
set_amount(L, Amount) ->
    kz_json:set_value(?AMOUNT, Amount, L).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec description(ledger()) -> ne_binary().
description(Ledger) ->
    kz_json:get_ne_binary_value(?DESC, Ledger).

-spec set_description(ledger(), ne_binary()) -> ledger().
set_description(L, Desc) ->
    kz_json:set_value(?DESC, Desc, L).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec source(ledger()) -> kz_json:object().
source(Ledger) ->
    kz_json:get_json_value(?SRC, Ledger).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec source_service(ledger()) -> ne_binary().
source_service(Ledger) ->
    kz_json:get_ne_binary_value(?SRC_SERVICE, Ledger).

-spec set_source_service(ledger(), ne_binary()) -> ledger().
set_source_service(L, Service) ->
    kz_json:set_value(?SRC_SERVICE, Service, L).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec source_id(ledger()) -> ne_binary().
source_id(Ledger) ->
    kz_json:get_ne_binary_value(?SRC_ID, Ledger).

-spec set_source_id(ledger(), ne_binary()) -> ledger().
set_source_id(L, Id) ->
    kz_json:set_value(?SRC_ID, Id, L).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec usage(ledger()) -> kz_json:object().
usage(Ledger) ->
    kz_json:get_json_value(?USAGE, Ledger).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec usage_type(ledger()) -> ne_binary().
usage_type(Ledger) ->
    kz_json:get_ne_binary_value(?USAGE_TYPE, Ledger).

-spec set_usage_type(ledger(), ne_binary()) -> ledger().
set_usage_type(L, Type) ->
    kz_json:set_value(?USAGE_TYPE, Type, L).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec usage_quantity(ledger()) -> number().
usage_quantity(Ledger) ->
    kz_json:get_number_value(?USAGE_QUANTITY, Ledger).

-spec set_usage_quantity(ledger(), number()) -> ledger().
set_usage_quantity(L, Quantity) ->
    kz_json:set_value(?USAGE_QUANTITY, Quantity, L).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec usage_unit(ledger()) -> ne_binary().
usage_unit(Ledger) ->
    kz_json:get_ne_binary_value(?USAGE_UNIT, Ledger).

-spec set_usage_unit(ledger(), ne_binary()) -> ledger().
set_usage_unit(L, Unit) ->
    kz_json:set_value(?USAGE_UNIT, Unit, L).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec period(ledger()) -> kz_json:object().
period(Ledger) ->
    kz_json:get_json_value(?PERIOD, Ledger).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec period_start(ledger()) -> integer().
period_start(Ledger) ->
    kz_json:get_integer_value(?PERIOD_START, Ledger).

-spec set_period_start(ledger(), integer()) -> ledger().
set_period_start(L, Start) ->
    kz_json:set_value(?PERIOD_START, Start, L).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec period_end(ledger()) -> integer().
period_end(Ledger) ->
    kz_json:get_integer_value(?PERIOD_END, Ledger).

-spec set_period_end(ledger(), integer()) -> ledger().
set_period_end(L, End) ->
    kz_json:set_value(?PERIOD_END, End, L).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec account(ledger()) -> kz_json:object().
account(Ledger) ->
    kz_json:get_json_value(?ACCOUNT, Ledger).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec account_id(ledger()) -> ne_binary().
account_id(Ledger) ->
    kz_json:get_ne_binary_value(?ACCOUNT_ID, Ledger).

-spec set_account_id(ledger(), ne_binary()) -> ledger().
set_account_id(L, Start) ->
    kz_json:set_value(?ACCOUNT_ID, Start, L).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec account_name(ledger()) -> ledger().
account_name(Ledger) ->
    kz_json:get_ne_binary_value(?ACCOUNT_NAME, Ledger).

-spec set_account_name(ledger(), ne_binary()) -> ledger().
set_account_name(L, End) ->
    kz_json:set_value(?ACCOUNT_NAME, End, L).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
