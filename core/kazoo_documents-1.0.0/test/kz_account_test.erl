%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_account_test).

-include_lib("eunit/include/eunit.hrl").

-define(ID, <<"_id">>).
-define(TREE, <<"pvt_tree">>).

-define(MASTER_ACCOUNT_ID, <<"1">>).
-define(MASTER_ACCOUNT, wh_json:from_list([{?TREE, []}
                                           ,{?ID, ?MASTER_ACCOUNT_ID}
                                          ])).

-define(SUB_ACCOUNT_ID, <<"2">>).
-define(SUB_ACCOUNT, wh_json:from_list([{?TREE, [?MASTER_ACCOUNT_ID]}
                                        ,{?ID, ?SUB_ACCOUNT_ID}
                                       ])).

-define(SUB_SUB_ACCOUNT_ID, <<"2">>).
-define(SUB_SUB_ACCOUNT, wh_json:from_list([{?TREE, [?MASTER_ACCOUNT_ID, ?SUB_ACCOUNT_ID]}
                                            ,{?ID, ?SUB_SUB_ACCOUNT_ID}
                                           ])).

parent_account_id_test() ->
    ?assertEqual('undefined', kz_account:parent_account_id(?MASTER_ACCOUNT)),
    ?assertEqual(?MASTER_ACCOUNT_ID, kz_account:parent_account_id(?SUB_ACCOUNT)),
    ?assertEqual(?SUB_ACCOUNT_ID, kz_account:parent_account_id(?SUB_SUB_ACCOUNT)).

tree_test() ->
    ?assertEqual([], kz_account:tree(?MASTER_ACCOUNT)),
    ?assertEqual([?MASTER_ACCOUNT_ID], kz_account:tree(?SUB_ACCOUNT)),
    ?assertEqual([?MASTER_ACCOUNT_ID, ?SUB_ACCOUNT_ID], kz_account:tree(?SUB_SUB_ACCOUNT)).
