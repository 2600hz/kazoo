%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% Looks for the account's initial call
%%% @end
%%% @contributors
%%%   Harenson Henao
%%%-------------------------------------------------------------------
-module(kt_initial_occurrence).

-export([init/0]).

%% Triggerables
-export([maybe_test_for_initial_occurrences/2]).

-include("tasks.hrl").

-define(CATEGORY, "account_crawler").
-define(SHOULD_CRAWL_FOR_FIRST_OCCURRENCE,
        kapps_config:get_is_true(?CONFIG_CAT,
                                 <<"should_crawl_for_first_occurrence">>,
                                 'true')).

%%%=============================================================================
%%% API
%%%=============================================================================
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY>>,
                            ?MODULE,
                            'maybe_test_for_initial_occurrences').

%% Triggerables
-spec maybe_test_for_initial_occurrences(ne_binary(), kz_account:doc()) -> 'ok'.
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
-spec maybe_test_for_registrations(ne_binary(), kz_account:doc()) -> 'ok'.
maybe_test_for_registrations(AccountId, AccountJObj) ->
    Realm = kz_account:realm(AccountJObj),
    case Realm =:= 'undefined'
        orelse kz_account:sent_initial_registration(AccountJObj)
    of
        'true' -> 'ok';
        'false' -> test_for_registrations(AccountId, Realm)
    end.

-spec test_for_registrations(ne_binary(), ne_binary()) -> 'ok'.
test_for_registrations(AccountId, Realm) ->
    lager:debug("looking for any registrations in realm ~s", [Realm]),
    Reg = [{<<"Realm">>, Realm}
          ,{<<"Fields">>, [<<"Account-ID">>]}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kapps_util:amqp_pool_collect(Reg
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

-spec handle_initial_registration(ne_binary()) -> 'ok'.
handle_initial_registration(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'ok', AccountJObj} -> notify_initial_registration(AccountJObj);
        _E -> 'ok'
    end.

-spec notify_initial_registration(kz_account:doc()) -> 'ok'.
notify_initial_registration(AccountJObj) ->
    UpdatedAccountJObj = kz_account:set_initial_registration_sent(AccountJObj, 'true'),
    _ = kz_util:account_update(UpdatedAccountJObj),
    Req = [{<<"Account-ID">>, kz_doc:id(AccountJObj)}
          ,{<<"Occurrence">>, <<"registration">>}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_first_occurrence/1).

%% First Call
-spec maybe_test_for_initial_call(ne_binary(), ne_binary(), kz_account:doc()) -> 'ok'.
maybe_test_for_initial_call(AccountId, AccountDb, AccountJObj) ->
    case kz_account:sent_initial_call(AccountJObj) of
        'true' -> 'ok';
        'false' ->
            lager:debug("looking for initial call in account ~s", [AccountId]),
            test_for_initial_call(AccountId, AccountDb)
    end.

-spec test_for_initial_call(ne_binary(), ne_binary()) -> 'ok'.
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

-spec handle_initial_call(ne_binary()) -> 'ok'.
handle_initial_call(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'ok', AccountJObj} -> notify_initial_call(AccountJObj);
        _ -> 'ok'
    end.

-spec notify_initial_call(kz_account:doc()) -> 'ok'.
notify_initial_call(AccountJObj) ->
    UpdatedAccountJObj = kz_account:set_initial_call_sent(AccountJObj, 'true'),
    _ = kz_util:account_update(UpdatedAccountJObj),
    Req = [{<<"Account-ID">>, kz_doc:id(AccountJObj)}
          ,{<<"Occurrence">>, <<"call">>}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_first_occurrence/1).
