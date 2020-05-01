%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Looks for the account's initial call.
%%% @author Harenson Henao
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_initial_occurrence).

-export([init/0]).

%% Triggerables
-export([maybe_test_for_initial_occurrences/2]).

-include("tasks.hrl").

-define(CATEGORY, "account_crawler").
-define(SHOULD_CRAWL_FOR_FIRST_OCCURRENCE
       ,kapps_config:get_is_true(?CONFIG_CAT
                                ,<<"should_crawl_for_first_occurrence">>
                                ,'true'
                                )
       ).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY>>
                           ,?MODULE
                           ,'maybe_test_for_initial_occurrences'
                           ).

%% Triggerables
-spec maybe_test_for_initial_occurrences(kz_term:ne_binary(), kzd_accounts:doc()) -> 'ok'.
maybe_test_for_initial_occurrences(AccountId, AccountJObj) ->
    AccountDb = kz_doc:account_db(AccountJObj),
    case ?SHOULD_CRAWL_FOR_FIRST_OCCURRENCE of
        'false' -> 'ok';
        'true' ->
            maybe_test_for_registrations(AccountId, AccountJObj),
            maybe_test_for_initial_call(AccountId, AccountDb, AccountJObj)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
%% First Registration
-spec maybe_test_for_registrations(kz_term:ne_binary(), kzd_accounts:doc()) -> 'ok'.
maybe_test_for_registrations(AccountId, AccountJObj) ->
    Realm = kzd_accounts:realm(AccountJObj),
    case Realm =:= 'undefined'
        orelse kzd_accounts:sent_initial_registration(AccountJObj)
    of
        'true' -> 'ok';
        'false' -> test_for_registrations(AccountId, Realm)
    end.

-spec test_for_registrations(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
test_for_registrations(AccountId, Realm) ->
    lager:debug("looking for any registrations in realm ~s", [Realm]),
    Reg = [{<<"Realm">>, Realm}
          ,{<<"Fields">>, [<<"Account-ID">>]}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Reg
                                    ,fun kapi_registration:publish_query_req/1
                                    ,{'ecallmgr', fun kapi_registration:query_resp_v/1}
                                    )
    of
        {'error', _} -> 'ok';
        {_, JObjs} ->
            case lists:any(fun kapi_registration:query_resp_v/1, JObjs) of
                'false' -> 'ok';
                'true' ->
                    lager:debug("found initial registration for account ~s (~s)", [AccountId, Realm]),
                    handle_initial_registration(AccountId)
            end
    end.

-spec handle_initial_registration(kz_term:ne_binary()) -> 'ok'.
handle_initial_registration(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', AccountJObj} -> notify_initial_registration(AccountJObj);
        _E -> 'ok'
    end.

-spec notify_initial_registration(kzd_accounts:doc()) -> 'ok'.
notify_initial_registration(AccountJObj) ->
    Update = [{kzd_accounts:path_initial_registration_sent(), 'true'}],
    {'ok', _} = kzd_accounts:update(kz_doc:id(AccountJObj), Update),

    Req = [{<<"Account-ID">>, kz_doc:id(AccountJObj)}
          ,{<<"Occurrence">>, <<"registration">>}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_first_occurrence/1).

%% First Call
-spec maybe_test_for_initial_call(kz_term:ne_binary(), kz_term:ne_binary(), kzd_accounts:doc()) -> 'ok'.
maybe_test_for_initial_call(AccountId, AccountDb, AccountJObj) ->
    case kzd_accounts:sent_initial_call(AccountJObj) of
        'true' -> 'ok';
        'false' ->
            lager:debug("looking for initial call in account ~s", [AccountId]),
            test_for_initial_call(AccountId, AccountDb)
    end.

-spec test_for_initial_call(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
test_for_initial_call(AccountId, AccountDb) ->
    ViewOptions = [{'key', <<"cdr">>}
                  ,{'limit', 1}
                  ],
    case kz_datamgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', [_|_]} ->
            lager:debug("found initial call in account ~s", [AccountId]),
            handle_initial_call(AccountId);
        _Else -> 'ok'
    end.

-spec handle_initial_call(kz_term:ne_binary()) -> 'ok'.
handle_initial_call(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', AccountJObj} -> notify_initial_call(AccountJObj);
        _ -> 'ok'
    end.

-spec notify_initial_call(kzd_accounts:doc()) -> 'ok'.
notify_initial_call(AccountJObj) ->
    Update = [{kzd_accounts:path_initial_call_sent(), 'true'}],
    _ = kzd_accounts:update(kz_doc:id(AccountJObj), Update),

    Req = [{<<"Account-ID">>, kz_doc:id(AccountJObj)}
          ,{<<"Occurrence">>, <<"call">>}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_first_occurrence/1).
