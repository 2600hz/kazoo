%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012 VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(crossbar_util).

-export([response/2, response/3, response/4, response/5]).
-export([response_deprecated/1, response_deprecated_redirect/2, response_deprecated_redirect/3
         ,response_redirect/3, response_redirect/4
        ]).
-export([response_202/2]).
-export([response_faulty_request/1]).
-export([response_bad_identifier/2]).
-export([response_conflicting_docs/1]).
-export([response_datastore_timeout/1]).
-export([response_datastore_conn_refused/1]).
-export([response_invalid_data/2]).
-export([response_missing_view/1]).
-export([response_db_missing/1]).
-export([response_db_fatal/1]).
-export([get_account_realm/1, get_account_realm/2]).
-export([disable_account/1, enable_account/1, change_pvt_enabled/2]).
-export([put_reqid/1]).
-export([cache_doc/2, cache_view/3]).
-export([flush_doc_cache/2]).
-export([get_results/3]).
-export([store/3, fetch/2, get_path/2]).
-export([find_account_id/3, find_account_id/4]).
-export([find_account_db/3, find_account_db/4]).

-include_lib("crossbar/include/crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function set the response status to success, and load the provided
%% data.
%% @end
%%--------------------------------------------------------------------
-spec response/2 :: (wh_json:json_term(), #cb_context{}) -> #cb_context{}.
response(JTerm, Context) ->
    create_response(success, undefined, undefined, JTerm, Context).

-spec response_202/2 :: (wh_json:json_string(), #cb_context{}) -> #cb_context{}.
response_202(Msg, Context) ->
    create_response(success, Msg, 202, Msg, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function load the error message into a 500 response, of type
%% fatal or error.
%% @end
%%--------------------------------------------------------------------
-spec response/3 :: ('error'|'fatal', wh_json:json_string(), #cb_context{}) -> #cb_context{}.
response(error, Msg, Context) ->
    create_response(error, Msg, 500, [], Context);
response(fatal, Msg, Context) ->
    create_response(fatal, Msg, 500, [], Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function load the error message into a specifc code response,
%% of type fatal or error.
%% @end
%%--------------------------------------------------------------------
-spec response/4 :: ('error'|'fatal', wh_json:json_string(), integer()|'undefined', #cb_context{}) -> #cb_context{}.
response(error, Msg, Code, Context) ->
    create_response(error, Msg, Code, [], Context);
response(fatal, Msg, Code, Context) ->
    create_response(fatal, Msg, Code, [], Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function load the error message into a specifc code response,
%% of type fatal or error with additional data
%% @end
%%--------------------------------------------------------------------
-spec response/5 :: ('error'|'fatal', wh_json:json_string(), integer()|'undefined', wh_json:json_term(), #cb_context{}) -> #cb_context{}.
response(error, Msg, Code, JTerm, Context) ->
    create_response(error, Msg, Code, JTerm, Context);
response(fatal, Msg, Code, JTerm, Context) ->
    create_response(fatal, Msg, Code, JTerm, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function loads the response vars in Context, soon it will
%% make smarter chooices about formating resp_data and filtering
%% other parameters.
%% @end
%%--------------------------------------------------------------------
-spec create_response/5 :: (crossbar_status(), wh_json:json_string(), integer()|'undefined', wh_json:json_term(), #cb_context{}) -> #cb_context{}.
create_response(Status, Msg, Code, JTerm, Context) ->
    Context#cb_context {
         resp_status = Status
        ,resp_error_msg = Msg
        ,resp_error_code = Code
        ,resp_data = JTerm
    }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the request is faulty (doesnt have a
%% match in validation, or someother issue with it keeps it from being
%% processed, like nonsensical chains)
%% @end
%%--------------------------------------------------------------------
-spec response_faulty_request/1 :: (#cb_context{}) -> #cb_context{}.
response_faulty_request(Context) ->
    response(error, <<"faulty request">>, 404, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% When a module is no longer valid, alert the client of the deprecated status
%% by either sending a 410 Gone or a 301 Redirct (when using the arity
%% 3 version.
%%
%% The RedirectUrl should be relative to the accessed URL. So, if the
%% URL accessed that is deprecated is:
%% /v1/account/{AID}/module/{MID}
%% and that MID moved to module2, the RedirectURL should be:
%% ../../module2/{MID}
%%
%% If redirecting from module1 to module2, RedirectURL should be:
%% ../module2
%% @end
%%--------------------------------------------------------------------
-spec response_deprecated/1 :: (#cb_context{}) -> #cb_context{}.
response_deprecated(Context) ->
    create_response(error, <<"deprecated">>, 410, wh_json:new(), Context).

-spec response_deprecated_redirect/2 :: (#cb_context{}, wh_json:json_string()) -> #cb_context{}.
-spec response_deprecated_redirect/3 :: (#cb_context{}, wh_json:json_string(), wh_json:json_object()) -> #cb_context{}.
response_deprecated_redirect(Context, RedirectUrl) ->
    response_deprecated_redirect(Context, RedirectUrl, wh_json:new()).
response_deprecated_redirect(#cb_context{resp_headers=RespHeaders}=Context, RedirectUrl, JObj) ->
    create_response(error, <<"deprecated">>, 301, JObj
                    ,Context#cb_context{resp_headers=[{"Location", RedirectUrl} | RespHeaders]}).

-spec response_redirect/3 :: (#cb_context{}, wh_json:json_string(), wh_json:json_object()) -> #cb_context{}.
response_redirect(Context, RedirectUrl, JObj) ->
    response_redirect(Context, RedirectUrl, JObj, 301).

-spec response_redirect/4 :: (#cb_context{}, wh_json:json_string(), wh_json:json_object(), integer()) -> #cb_context{}.
response_redirect(#cb_context{resp_headers=RespHeaders}=Context, RedirectUrl, JObj, Redirect) ->
    create_response(error, <<"redirect">>, Redirect, JObj, Context#cb_context{resp_headers=[{"Location", RedirectUrl} | RespHeaders]}).
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the requested ID did not match a
%% data record. Using 404 as 410 is a permanent Gone, while 404 is
%% a softer not found now.
%% @end
%%--------------------------------------------------------------------
-spec response_bad_identifier/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
response_bad_identifier(Id, Context) ->
    response(error, <<"bad identifier">>, 404, [Id], Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the requested resource update fails
%% because of a conflict in the DB
%% @end
%%--------------------------------------------------------------------
-spec response_conflicting_docs/1 :: (#cb_context{}) -> #cb_context{}.
response_conflicting_docs(Context) ->
    response(error, <<"conflicting documents">>, 409, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the requested data query was missing
%% @end
%%--------------------------------------------------------------------
-spec response_missing_view/1 :: (#cb_context{}) -> #cb_context{}.
response_missing_view(Context) ->
    response(fatal, <<"datastore missing view">>, 503, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the datastore time'd out
%% @end
%%--------------------------------------------------------------------
-spec response_datastore_timeout/1 :: (#cb_context{}) -> #cb_context{}.
response_datastore_timeout(Context) ->
    response(error, <<"datastore timeout">>, 503, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the datastore time'd out
%% @end
%%--------------------------------------------------------------------
-spec response_datastore_conn_refused/1 :: (#cb_context{}) -> #cb_context{}.
response_datastore_conn_refused(Context) ->
    response(error, <<"datastore connection refused">>, 503, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the provided data did not validate
%% @end
%%--------------------------------------------------------------------
-spec response_invalid_data/2 :: (wh_json:json_term(), #cb_context{}) -> #cb_context{}.
response_invalid_data(JTerm, Context) ->
    response(error, <<"invalid data">>, 400, JTerm, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the datastore does not have the requested
%% record collection
%% @end
%%--------------------------------------------------------------------
-spec response_db_missing/1 :: (#cb_context{}) -> #cb_context{}.
response_db_missing(Context) ->
    response(fatal, <<"data collection missing: database not found">>, 503, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the datastore does not have the requested
%% record collection
%% @end
%%--------------------------------------------------------------------
-spec response_db_fatal/1 :: (#cb_context{}) -> #cb_context{}.
response_db_fatal(Context) ->
    response(fatal, <<"datastore fatal error">>, 503, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function extracts the request ID and sets it as 'callid' in
%% the process dictionary, where the logger expects it.
%% @end
%%--------------------------------------------------------------------
-spec put_reqid/1 :: (#cb_context{}) -> 'undefined' | ne_binary().
put_reqid(#cb_context{req_id=ReqId}) ->
    put(callid, ReqId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sets a value in the crossbar context for later retrieval during
%% this request.
%% @end
%%--------------------------------------------------------------------
-spec store/3 :: (term(), term(), #cb_context{}) -> #cb_context{}.
store(Key, Data, #cb_context{storage=Storage}=Context) ->
    Context#cb_context{storage=[{Key, Data}|props:delete(Key, Storage)]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a previously stored value from the current request.
%% @end
%%--------------------------------------------------------------------
-spec fetch/2 :: (term(), #cb_context{}) -> term().
fetch(Key, #cb_context{storage=Storage}) ->
    props:get_value(Key, Storage).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a previously stored value from the current request.
%% @end
%%--------------------------------------------------------------------
-spec find_account_id/3 :: ('undefined' | ne_binary(), 'undefined' | ne_binary(), 'undefined' | ne_binary()) 
                           -> {'ok', ne_binary()} | {'multiples', [ne_binary(),...]} | {'error', wh_json:json_object()}.

-spec find_account_id/4 :: ('undefined' | ne_binary(), 'undefined' | ne_binary(), 'undefined' | ne_binary(), 'true') 
                           -> {'ok', ne_binary()} | {'multiples', [ne_binary(),...]} | {'error', wh_json:json_object()};
                           ('undefined' | ne_binary(), 'undefined' | ne_binary(), 'undefined' | ne_binary(), 'false') 
                           -> {'ok', ne_binary()} | {'error', wh_json:json_object()}.

-spec find_account_db/3 :: ('undefined' | ne_binary(), 'undefined' | ne_binary(), 'undefined' | ne_binary()) 
                           -> {'ok', ne_binary()} | {'multiples', [ne_binary(),...]} | {'error', wh_json:json_object()}.

-spec find_account_db/4 :: ('undefined' | ne_binary(), 'undefined' | ne_binary(), 'undefined' | ne_binary(), 'true') 
                           -> {'ok', ne_binary()} | {'multiples', [ne_binary(),...]} | {'error', wh_json:json_object()};
                           ('undefined' | ne_binary(), 'undefined' | ne_binary(), 'undefined' | ne_binary(), 'false') 
                           -> {'ok', ne_binary()} | {'error', wh_json:json_object()}.

find_account_id(PhoneNumber, AccountRealm, AccountName) ->
    find_account_id(PhoneNumber, AccountRealm, AccountName, true).

find_account_id(PhoneNumber, AccountRealm, AccountName, AllowMultiples) ->
    case find_account_db(PhoneNumber, AccountRealm, AccountName, AllowMultiples, wh_json:new()) of
        {ok, AccountDb} -> {ok, wh_util:format_account_id(AccountDb, raw)};
        {multiples, AccountDbs} -> {multiples, [wh_util:format_account_id(AccountDb, raw) || AccountDb <- AccountDbs]};
        Else -> Else
    end.

find_account_db(PhoneNumber, AccountRealm, AccountName) ->
    find_account_db(PhoneNumber, AccountRealm, AccountName, true).

find_account_db(PhoneNumber, AccountRealm, AccountName, AllowMultiples) ->
    find_account_db(PhoneNumber, AccountRealm, AccountName, AllowMultiples, wh_json:new()).

find_account_db(undefined, undefined, undefined, _, Errors) ->
    {error, Errors};
find_account_db(undefined, undefined, AccountName, AllowMultiples, Errors) ->
    case whapps_util:get_accounts_by_name(AccountName) of
        {ok, AccountDb} ->
            lager:debug("found account by name '~s': ~s", [AccountName, AccountDb]),
            {ok, AccountDb};
        {multiples, AccountDbs} when AllowMultiples ->
            lager:debug("the account name returned multiple results, requestor allowed multiple"),
            {multiples, AccountDbs};
        {multiples, _} ->
            lager:debug("the account realm returned multiple results"),
            Error = wh_json:set_value([<<"account_name">>, <<"ambiguous">>]
                                      ,<<"The specific account could not be identified">>
                                     ,Errors),
            find_account_db(undefined, undefined, undefined, AllowMultiples, Error);
        {error, _} ->
            Error = wh_json:set_value([<<"account_name">>, <<"not_found">>]
                                      ,<<"The provided account name could not be found">>
                                      ,Errors),
            find_account_db(undefined, undefined, undefined, AllowMultiples, Error)
    end;
find_account_db(undefined, AccountRealm, AccountName, AllowMultiples, Errors) ->
    case whapps_util:get_account_by_realm(AccountRealm) of
        {ok, AccountDb} ->
            lager:debug("found account by realm '~s': ~s", [AccountRealm, AccountDb]),
            {ok, AccountDb};
        {multiples, AccountDbs} when AllowMultiples ->
            lager:debug("the account realm returned multiple results, requestor allowed multiple"),
            {multiples, AccountDbs};
        {multiples, _} ->
            lager:debug("the account realm realm multiple results"),
            Error = wh_json:set_value([<<"account_realm">>, <<"ambiguous">>]
                                      ,<<"The specific account could not be identified">>
                                     ,Errors),
            find_account_db(undefined, undefined, AccountName, AllowMultiples, Error);
        {error, _} ->
            Error = wh_json:set_value([<<"account_realm">>, <<"not_found">>]
                                      ,<<"The provided account realm could not be found">>
                                      ,Errors),
            find_account_db(undefined, undefined, AccountName, AllowMultiples, Error)
    end;
find_account_db(PhoneNumber, AccountRealm, AccountName, AllowMultiples, Errors) ->
    case wh_number_manager:lookup_account_by_number(PhoneNumber) of
        {ok, AccountId, _, _} -> 
            AccountDb = wh_util:format_account_id(AccountId, encoded),
            lager:debug("found account by phone number '~s': ~s", [PhoneNumber, AccountDb]),
            {ok, AccountDb};
        {error, _} -> 
            Error = wh_json:set_value([<<"phone_number">>, <<"not_found">>]
                                      ,<<"The provided phone number could not be found">>
                                      ,Errors),
            find_account_db(undefined, AccountRealm, AccountName, AllowMultiples, Error)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieves the account realm
%% @end
%%--------------------------------------------------------------------
-spec get_account_realm/1 :: (ne_binary() | #cb_context{}) -> 'undefined' | ne_binary().
-spec get_account_realm/2 :: ('undefined' | ne_binary(), ne_binary()) -> 'undefined' | ne_binary().

get_account_realm(#cb_context{db_name=Db, account_id=AccountId}) ->
    get_account_realm(Db, AccountId);
get_account_realm(AccountId) ->
    get_account_realm(wh_util:format_account_id(AccountId, encoded), AccountId).

get_account_realm(undefined, _) ->
    undefined;
get_account_realm(Db, AccountId) ->
    case couch_mgr:open_cache_doc(Db, AccountId) of
        {ok, JObj} ->
            wh_json:get_ne_value(<<"realm">>, JObj);
        {error, R} ->
            lager:debug("error while looking up account realm: ~p", [R]),
            undefined
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Flag all descendants of the account id as disabled
%% @end
%%--------------------------------------------------------------------
-spec disable_account/1 :: ('undefined' | ne_binary()) -> 'ok' | {'error', _}.
disable_account(undefined) ->
    ok;
disable_account(AccountId) ->
    ViewOptions = [{<<"startkey">>, [AccountId]}, {<<"endkey">>, [AccountId, wh_json:new()]}],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {ok, JObjs} ->
            _ = [change_pvt_enabled(false, wh_json:get_value(<<"id">>, JObj)) || JObj <- JObjs],
            ok;
        {error, R}=E ->
            lager:debug("unable to disable descendants of ~s: ~p", [AccountId, R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Flag all descendants of the account id as enabled
%% @end
%%--------------------------------------------------------------------
-spec enable_account/1 :: ('undefined' | ne_binary()) -> 'ok' | {'error', _}.
enable_account(undefined) ->
    ok;
enable_account(AccountId) ->
    ViewOptions = [{<<"startkey">>, [AccountId]}, {<<"endkey">>, [AccountId, wh_json:new()]}],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {ok, JObjs} ->
            _ = [change_pvt_enabled(true, wh_json:get_value(<<"id">>, JObj)) || JObj <- JObjs],
            ok;
        {error, R}=E ->
            lager:debug("unable to enable descendants of ~s: ~p", [AccountId, R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update all descendants of the account id pvt_enabled flag with State
%% @end
%%--------------------------------------------------------------------
change_pvt_enabled(_, undefined) ->
    ok;
change_pvt_enabled(State, AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    try 
      {ok, JObj1} = couch_mgr:open_doc(AccountDb, AccountId),
      lager:debug("set pvt_enabled to ~s on account ~s", [State, AccountId]),
      {ok, JObj2} = couch_mgr:ensure_saved(AccountDb, wh_json:set_value(<<"pvt_enabled">>, State, JObj1)),
      case couch_mgr:lookup_doc_rev(?WH_ACCOUNTS_DB, AccountId) of
          {ok, Rev} ->
              couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, wh_json:set_value(<<"_rev">>, Rev, JObj2));
          _Else ->
              couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, wh_json:delete_key(<<"_rev">>, JObj2))
      end
    catch
        _:R ->
            lager:debug("unable to set pvt_enabled to ~s on account ~s: ~p", [State, AccountId, R]),
            {error, R}
    end.
    
-spec cache_view/3 :: (ne_binary(), proplist(), wh_json:json_object()) -> false | ok.
cache_view(Db, ViewOptions, JObj) ->
    (?CACHE_TTL =/= 0) andalso
        begin
            lager:debug("caching views results in cache"),
            wh_cache:store_local(?CROSSBAR_CACHE, {crossbar, view, {Db, ?MODULE}}, {ViewOptions, JObj}, ?CACHE_TTL)
        end.

-spec cache_doc/2 :: (ne_binary(), wh_json:json_object()) -> false | ok.
cache_doc(Db, JObj) ->
    Id = wh_json:get_value(<<"_id">>, JObj),
    (?CACHE_TTL =/= 0) andalso
        begin 
            lager:debug("caching document and flushing related views in cache"),
            wh_cache:store_local(?CROSSBAR_CACHE, {crossbar, doc, {Db, Id}}, JObj, ?CACHE_TTL),
            wh_cache:erase_local(?CROSSBAR_CACHE, {crossbar, view, {Db, ?MODULE}})
        end.

-spec flush_doc_cache/2 :: (ne_binary(), ne_binary() | wh_json:json_object()) -> false | ok.
flush_doc_cache(Db, <<Id>>) ->
    (?CACHE_TTL =/= 0) andalso
        begin
            lager:debug("flushing document and related views from cache"),
            wh_cache:erase_local(?CROSSBAR_CACHE, {crossbar, doc, {Db, Id}}),
            wh_cache:erase_local(?CROSSBAR_CACHE, {crossbar, view, {Db, ?MODULE}})
        end;
flush_doc_cache(Db, JObj) ->
    flush_doc_cache(Db, wh_json:get_value(<<"_id">>, JObj)).

-spec get_results/3 :: (ne_binary(), ne_binary(), proplist()) -> {ok, wh_json:json_object()} | {error, term()}.
get_results(Db, View, ViewOptions) ->
    case wh_cache:peek_local(?CROSSBAR_CACHE, {crossbar, view, {Db, ?MODULE}}) of
        {ok, {ViewOptions, ViewResults}} -> 
            lager:debug("found view results in cache"),
            {ok, ViewResults};
        _ ->
            case couch_mgr:get_results(Db, View, ViewOptions) of
                {ok, JObj}=Ok ->
                    cache_view(Db, ViewOptions, JObj),
                    Ok;
                {error, R}=E ->
                    lager:debug("error fetching ~s/~s: ~p", [Db, View, R]),
                    E
            end
    end.    

-spec get_path/2 :: (#http_req{}, ne_binary()) -> ne_binary().
get_path(Req, Relative) ->
    {RawPath, _} = cowboy_http_req:raw_path(Req),

    get_path1(RawPath, Relative).

get_path1(RawPath, Relative) ->
    PathTokensRev = lists:reverse(binary:split(RawPath, <<"/">>, [global])),
    UrlTokens = binary:split(Relative, <<"/">>),

    wh_util:join_binary(
      lists:reverse(
        lists:foldl(fun(<<"..">>, []) -> [];
                       (<<"..">>, [_ | PathTokens]) -> PathTokens;
                       (<<".">>, PathTokens) -> PathTokens;
                       (Segment, PathTokens) -> [Segment | PathTokens]
                    end, PathTokensRev, UrlTokens)
       ), <<"/">>).

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

get_path_test() ->
    RawPath = <<"/v1/accounts/acct_id/module">>,
    Relative = <<"../other_mod">>,
    ?assertEqual(get_path1(RawPath, Relative), <<"/v1/accounts/acct_id/other_mod">>),
    ?assertEqual(get_path1(RawPath, <<Relative/binary, "/mod_id">>), <<"/v1/accounts/acct_id/other_mod/mod_id">>).
-endif.
