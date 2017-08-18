%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
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

-define(MASTER_ACCOUNT_ID, <<"account0000000000000000000000001">>).
-define(SUB_ACCOUNT_ID, <<"account0000000000000000000000002">>).
-define(SUB_SUB_ACCOUNT_ID, <<"account0000000000000000000000003">>).

fetch_test_() ->
    [?_assertEqual({error,invalid_db_name}, kz_account:fetch(undefined))
    ,?_assertEqual(undefined, kz_account:fetch_realm(undefined))
    ,?_assertEqual(undefined, kz_account:fetch_name(undefined))
    ].

tree_test_() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    {'ok', SubAccount} = kz_account:fetch(?SUB_ACCOUNT_ID),
    {'ok', SubSubAccount} = kz_account:fetch(?SUB_SUB_ACCOUNT_ID),
    [?_assertEqual([], kz_account:tree(MasterAccount))
    ,?_assertEqual([?MASTER_ACCOUNT_ID], kz_account:tree(SubAccount))
    ,?_assertEqual([?MASTER_ACCOUNT_ID, ?SUB_ACCOUNT_ID], kz_account:tree(SubSubAccount))
    ].

parent_account_id_test_() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    {'ok', SubAccount} = kz_account:fetch(?SUB_ACCOUNT_ID),
    {'ok', SubSubAccount} = kz_account:fetch(?SUB_SUB_ACCOUNT_ID),
    [{"verify that fetching the parent id of the master account returns 'undefined'"
     ,?_assertEqual('undefined', kz_account:parent_account_id(MasterAccount))
    }
    ,{"verify that fetching the parent id of sub account is the master account"
      ,?_assertEqual(?MASTER_ACCOUNT_ID, kz_account:parent_account_id(SubAccount))
     }
    ,{"verify fetching the parent id of a sub-sub account is the direct ancestor"
     ,?_assertEqual(?SUB_ACCOUNT_ID, kz_account:parent_account_id(SubSubAccount))
    }
    ].

trial_time_test_() ->
    Now = kz_time:current_tstamp(),
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

outbound_flags_test_() ->
    {'ok', OldData} = kz_account:fetch(<<"account0000000000000000000000001">>),
    UpdatedOldData = kz_json:get_value(<<"outbound_flags">>, kz_account:set_outbound_flags(OldData, [<<"updated_flag">>])),
    ExpectedOldUpdate = kz_json:decode("{\"static\": [\"updated_flag\"]}"),

    {'ok', NewData} = kz_account:fetch(<<"account0000000000000000000000002">>),
    UpdatedNewData = kz_json:get_value(<<"outbound_flags">>, kz_account:set_outbound_flags(NewData, [<<"updated_flag">>])),
    ExpectedNewUpdate = kz_json:decode("{\"dynamic\": [\"zone\", \"from_domain\", \"custom_channel_vars.owner_id\"], \"static\": [\"updated_flag\"]}"),

    [{"verify get for deprecated format"
     ,?_assertEqual([<<"old_flag">>], kz_account:outbound_flags(OldData))
     }
    ,{"verify get for new format"
     ,?_assertEqual([<<"new_flag">>], kz_account:outbound_flags(NewData))
     }
    ,{"verify set with old format converts to new"
     ,?_assertEqual(ExpectedOldUpdate, UpdatedOldData)
     }
    ,{"verify set with new format"
     ,?_assertEqual(ExpectedNewUpdate, UpdatedNewData)
     }
    ].

outbound_dynamic_flags_test_() ->
    {'ok', OldData} = kz_account:fetch(<<"account0000000000000000000000001">>),
    UpdatedOldData = kz_json:get_value(<<"outbound_flags">>, kz_account:set_outbound_dynamic_flags(OldData, [<<"updated_flag">>])),
    ExpectedOldUpdate = kz_json:decode("{\"static\": [\"old_flag\"], \"dynamic\": [\"updated_flag\"]}"),

    {'ok', NewData} = kz_account:fetch(<<"account0000000000000000000000002">>),
    UpdatedNewData = kz_json:get_value(<<"outbound_flags">>, kz_account:set_outbound_dynamic_flags(NewData, [<<"updated_flag">>])),
    ExpectedNewUpdate = kz_json:decode("{\"dynamic\": [\"updated_flag\"], \"static\": [\"new_flag\"]}"),
    [{"verify get for deprecated format"
     ,?_assertEqual([], kz_account:outbound_dynamic_flags(OldData))
     }
    ,{"verify get for new format"
     ,?_assertEqual([<<"zone">>, <<"from_domain">>, <<"custom_channel_vars.owner_id">>], kz_account:outbound_dynamic_flags(NewData))
     }
    ,{"verify set with old format converts to new"
     ,?_assertEqual(ExpectedOldUpdate, UpdatedOldData)
     }
    ,{"verify set with new format"
     ,?_assertEqual(ExpectedNewUpdate, UpdatedNewData)
     }
    ].
