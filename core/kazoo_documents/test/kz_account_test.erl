%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
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
-define(MASTER_ACCOUNT, kz_json:from_list([{?TREE, []}
                                          ,{?ID, ?MASTER_ACCOUNT_ID}
                                          ])).

-define(SUB_ACCOUNT_ID, <<"2">>).
-define(SUB_ACCOUNT, kz_json:from_list([{?TREE, [?MASTER_ACCOUNT_ID]}
                                       ,{?ID, ?SUB_ACCOUNT_ID}
                                       ])).

-define(SUB_SUB_ACCOUNT_ID, <<"2">>).
-define(SUB_SUB_ACCOUNT, kz_json:from_list([{?TREE, [?MASTER_ACCOUNT_ID, ?SUB_ACCOUNT_ID]}
                                           ,{?ID, ?SUB_SUB_ACCOUNT_ID}
                                           ])).

parent_account_id_test_() ->
    [?_assertEqual('undefined', kz_account:parent_account_id(?MASTER_ACCOUNT))
    ,?_assertEqual(?MASTER_ACCOUNT_ID, kz_account:parent_account_id(?SUB_ACCOUNT))
    ,?_assertEqual(?SUB_ACCOUNT_ID, kz_account:parent_account_id(?SUB_SUB_ACCOUNT))
    ].

tree_test_() ->
    [?_assertEqual([], kz_account:tree(?MASTER_ACCOUNT))
    ,?_assertEqual([?MASTER_ACCOUNT_ID], kz_account:tree(?SUB_ACCOUNT))
    ,?_assertEqual([?MASTER_ACCOUNT_ID, ?SUB_ACCOUNT_ID], kz_account:tree(?SUB_SUB_ACCOUNT))
    ].

trial_time_test_() ->
    Now = kz_util:current_tstamp(),
    Passed = kz_account:set_trial_expiration(kz_account:new(), Now - 10000),
    Active = kz_account:set_trial_expiration(kz_account:new(), Now + 10000),

    [{"testing expired trial accounts are computed as such"
     ,?_assertEqual('true', kz_account:trial_has_expired(Passed, Now))
     }
    ,{"testing current trial accounts are computed as such"
     ,?_assertEqual('false', kz_account:trial_has_expired(Active, Now))
     }
    ,{"testing that current trial accounts have proper time left computed"
     ,?_assertEqual(10000, kz_account:trial_time_left(Active, Now))
     }
    ,{"testing that expired trial accounts have proper time since expiration computed"
     ,?_assertEqual(-10000, kz_account:trial_time_left(Passed, Now))
     }
    ].
