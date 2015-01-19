%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Device document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_account).

-export([name/1, set_name/2
         ,parent_account_id/1
         ,tree/1
         ,notification_preference/1, set_notification_preference/2
        ]).

-define(ID, <<"_id">>).
-define(NAME, <<"name">>).
-define(TREE, <<"pvt_tree">>).
-define(NOTIFY_PREF, <<"pvt_notification_preference">>).

-include("kz_documents.hrl").

-spec name(wh_json:object()) -> api_binary().
name(JObj) ->
    wh_json:get_value(?NAME, JObj).

-spec set_name(wh_json:object(), ne_binary()) -> wh_json:object().
set_name(JObj, Name) ->
    wh_json:set_value(?NAME, Name, JObj).

-spec parent_account_id(wh_json:object()) -> api_binary().
parent_account_id(JObj) ->
    case wh_json:get_value(?TREE, JObj) of
        [] -> 'undefined';
        Ancestors -> lists:last(Ancestors)
    end.

-spec tree(wh_json:object()) -> ne_binaries().
tree(JObj) ->
    wh_json:get_value(?TREE, JObj, []).

-spec notification_preference(wh_json:object()) -> api_binary().
notification_preference(JObj) ->
    wh_json:get_value(?NOTIFY_PREF, JObj).

-spec set_notification_preference(wh_json:object(), ne_binary()) -> wh_json:object().
set_notification_preference(JObj, Pref) ->
    wh_json:set_value(?NOTIFY_PREF, Pref, JObj).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

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
    ?assertEqual('undefined', parent_account_id(?MASTER_ACCOUNT)),
    ?assertEqual(?MASTER_ACCOUNT_ID, parent_account_id(?SUB_ACCOUNT)),
    ?assertEqual(?SUB_ACCOUNT_ID, parent_account_id(?SUB_SUB_ACCOUNT)).

tree_test() ->
    ?assertEqual([], tree(?MASTER_ACCOUNT)),
    ?assertEqual([?MASTER_ACCOUNT_ID], tree(?SUB_ACCOUNT)),
    ?assertEqual([?MASTER_ACCOUNT_ID, ?SUB_ACCOUNT_ID], tree(?SUB_SUB_ACCOUNT)).

-endif.
