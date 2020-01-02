%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzs_plan_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_fixturedb/include/kz_fixturedb.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(ACCOUNT_DB, <<"account%2Fac%2Fco%2Funt0000000000000000000000003">>).
-define(ACCOUNT_MODB, <<"account%2Fac%2Fco%2Funt0000000000000000000000003-201710">>).
-define(ACCOUNT_DB_SPLIT, <<"account%2Fac%2Fco%2Funt0000000000000000000000004">>).
-define(ACCOUNT_MODB_SPLIT, <<"account%2Fac%2Fco%2Funt0000000000000000000000004-201710">>).

%% server tag constant macros
-define(SYS_TAG, <<"local">>).

%% Classifiers
-define(ACCOUNT, <<"account">>).
-define(MODB, <<"modb">>).
-define(SYSTEM, <<"system">>).

%% Data Plan Types
-define(FAX_TYPE, <<"fax">>).
-define(REC_TYPE, <<"call_recordings">>).
-define(VM_TYPE, <<"mailbox_messages">>).

-spec render_test_() -> any().
render_test_() ->
    {'setup'
    ,fun setup/0
    ,fun cleanup/1
    ,fun(_ReturnOfSetup) ->
             [{"Simple system plan test.", ?_test(system_plan())}
             ,{"Simple modb plan test.", ?_test(modb_plan())}
             ,{"Simple account plan test.", ?_test(account_plan())}
             ,{"Simple modb dataplan type match test.", ?_test(match_modb_dataplan_type())}
             ,{"Split account plan test.", ?_test(account_plan_split())}
             ,{"Split modb plan test.", ?_test(modb_plan_split())}
             ,{"Split modb type plan test.", ?_test(match_modb_plan_type_split())}
             ,{"Handle missing connection.", ?_test(handle_missing_connection())}
             ]
     end
    }.

%%%-----------------------------------------------------------------------------
%%% mock utility functions
%%%-----------------------------------------------------------------------------

-spec setup() -> pid().
setup() ->
    kz_fixturedb_util:start_me('false').

cleanup(Pid) ->
    kz_fixturedb_util:stop_me(Pid).

%%%-----------------------------------------------------------------------------
%%% tests
%%%-----------------------------------------------------------------------------

system_plan() ->
    ?assertMatch(#{tag := ?SYS_TAG
                  ,server := {'kazoo_fixturedb', _Conn}
                  ,others := []
                  }
                ,kzs_plan:plan()
                ).

modb_plan() ->
    Plan = kzs_plan:plan(?ACCOUNT_MODB),
    ?assertMatch(#{classification := ?MODB
                  ,tag := ?SYS_TAG
                  ,server := {'kazoo_fixturedb', _Conn}
                  ,others := []
                  }
                ,Plan
                ).

account_plan() ->
    ?assertMatch(#{classification := <<"account">>
                  ,tag := ?SYS_TAG
                  ,server := {'kazoo_fixturedb', _Conn}
                  ,others := []
                  }
                ,kzs_plan:plan(?ACCOUNT_DB)
                ).

match_modb_dataplan_type() ->
    ?assertMatch(#{classification := ?MODB
                  ,tag := ?SYS_TAG
                  ,doc_type := ?VM_TYPE
                  ,server := {'kazoo_fixturedb', _Conn}
                  }
                ,kzs_plan:get_dataplan(?ACCOUNT_MODB, ?VM_TYPE)
                ),
    ?assertMatch(#{classification := ?MODB
                  ,tag := ?SYS_TAG
                  ,doc_type := ?FAX_TYPE
                  ,server := {'kazoo_fixturedb', _Conn}
                  }
                ,kzs_plan:get_dataplan(?ACCOUNT_MODB, ?FAX_TYPE)
                ),
    ?assertMatch(#{classification := ?MODB
                  ,tag := ?SYS_TAG
                  ,doc_type := ?REC_TYPE
                  ,server := {'kazoo_fixturedb', _Conn}
                  }
                ,kzs_plan:get_dataplan(?ACCOUNT_MODB, ?REC_TYPE)
                ).

account_plan_split() ->
    ?assertMatch(#{classification := ?ACCOUNT
                  ,tag := ?SYS_TAG
                  ,others := []
                  }
                ,kzs_plan:plan(?ACCOUNT_DB_SPLIT)
                ).

modb_plan_split() ->
    ?assertMatch(#{classification := ?MODB
                  ,tag := <<"account_4_modb">>
                  ,others := [_Others]
                  }
                ,kzs_plan:plan(?ACCOUNT_MODB_SPLIT)
                ).

match_modb_plan_type_split() ->
    ?assertMatch(#{classification := ?MODB
                  ,tag := <<"account_4_modb">>
                  ,doc_type := ?VM_TYPE
                  ,server := {'kazoo_fixturedb', _Conn}
                  }
                ,kzs_plan:get_dataplan(?ACCOUNT_MODB_SPLIT, ?VM_TYPE)
                ),
    ?assertMatch(#{classification := ?MODB
                  ,tag := <<"account_4_modb">>
                  ,doc_type := ?REC_TYPE
                  ,server := {'kazoo_fixturedb', _Conn}
                  }
                ,kzs_plan:get_dataplan(?ACCOUNT_MODB_SPLIT, ?REC_TYPE)
                ),
    ?assertMatch(#{classification := ?MODB
                  ,tag := <<"account_4_fax">>
                  ,doc_type := ?FAX_TYPE
                  ,server := {'kazoo_fixturedb', _Conn}
                  }
                ,kzs_plan:get_dataplan(?ACCOUNT_MODB_SPLIT, ?FAX_TYPE)
                ).

handle_missing_connection() ->
    AccountId = <<"account0000000000000000000000005">>,
    Plan = kzs_plan:plan(kzs_util:format_account_mod_id(AccountId)),
    ?assertMatch(#{classification := ?MODB
                  ,tag := <<"local">>
                  ,others := []
                  ,server := {'kazoo_fixturedb', _}
                  ,account_id := AccountId
                  }
                ,Plan
                ).
