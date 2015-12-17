%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600HZ, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(kazoo_ledger).

-include("kzl.hrl").

-export([
    new/0, save/1
    ,name/1, set_name/2
    ,amount/1, set_amount/2
    ,description/1, set_description/2
    ,account/1, set_account/2
    ,type/1, set_type/2
]).

-export([from_json/1, to_json/1, to_public_json/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec new() -> ledger().
new() -> #kz_ledger{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec save(ledger()) -> {'ok', ledger()} | {'error', any()}.
save(Ledger) ->
    JObj = to_json(Ledger),
    AccountId = account(Ledger),
    case kazoo_modb:save_doc(AccountId, JObj) of
        {'ok', _} -> {'ok', Ledger};
        {'error', _}=Error -> Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec name(ledger()) -> ne_binary().
name(#kz_ledger{name=Name}) -> Name.

-spec set_name(ledger(), ne_binary()) -> ledger().
set_name(L, Name) ->
    L#kz_ledger{name=Name}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec amount(ledger()) -> integer().
amount(#kz_ledger{amount=Amount}) -> Amount.

-spec set_amount(ledger(), integer()) -> ledger().
set_amount(L, Amount) ->
    L#kz_ledger{amount=Amount}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec description(ledger()) -> ne_binary().
description(#kz_ledger{description=Desc}) -> Desc.

-spec set_description(ledger(), ne_binary()) -> ledger().
set_description(L, Desc) ->
    L#kz_ledger{description=Desc}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec account(ledger()) -> ne_binary().
account(#kz_ledger{account=AccountId}) -> AccountId.

-spec set_account(ledger(), ne_binary()) -> ledger().
set_account(L, Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    L#kz_ledger{account=AccountId}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec type(ledger()) -> ne_binary().
type(#kz_ledger{type=Type}) -> Type.

-spec set_type(ledger(), ne_binary()) -> ledger().
set_type(L, Type) ->
    L#kz_ledger{type=Type}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_json(wh_json:object()) -> ledger().
from_json(JObj) ->
    #kz_ledger{
        name=wh_json:get_value(?PVT_NAME, JObj)
        ,amount=wh_json:get_value(?PVT_AMOUNT, JObj)
        ,description=wh_json:get_value(?PVT_DESC, JObj)
        ,account=wh_json:get_value(?PVT_ACCOUNT, JObj)
        ,type=wh_json:get_value(?PVT_LEDGER_TYPE, JObj)
    }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_json(ledger()) -> wh_json:object().
to_json(Ledger) ->
    wh_json:from_list(
        props:filter_undefined([
            {?PVT_NAME, name(Ledger)}
            ,{?PVT_AMOUNT, amount(Ledger)}
            ,{?PVT_DESC, description(Ledger)}
            ,{?PVT_ACCOUNT, account(Ledger)}
            ,{?PVT_LEDGER_TYPE, type(Ledger)}
            ,{?PVT_TYPE, ?LEDGER_TYPE}
            ,{?PVT_CREATED, wh_util:current_tstamp()}
            ,{?PVT_UPDATED, wh_util:current_tstamp()}
        ])
    ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_public_json(ledger()) -> wh_json:object().
to_public_json(Ledger) ->
    wh_json:from_list(
        props:filter_undefined([
            {<<"name">>, name(Ledger)}
            ,{<<"amount">>, amount(Ledger)}
            ,{<<"description">>, description(Ledger)}
            ,{<<"account_id">>, account(Ledger)}
            ,{<<"type">>, type(Ledger)}
        ])
    ).