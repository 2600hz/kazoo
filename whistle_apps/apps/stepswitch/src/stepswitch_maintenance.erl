%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Preforms maintenance operations against the stepswitch dbs
%%% @end
%%% Created : 14 June 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(stepswitch_maintenance).

-include("stepswitch.hrl").

%% API
-export([reconcile/0, reconcile/1, reconcile/2, lookup_number/1, validate_routes/0]).
-export([reload_resources/0, process_number/1, process_number/2]).

%% These are temporary until the viewing of numbers in an account can
%% be standardized
-define(TS_DB, <<"ts">>).

%% TODO: This makes stepswitch dependent on callflow view... This is safe-ish
%% beacuse if you reconcile without the callflow view then they will never
%% run anyway (no callflow whapp connected to the db to execute). But it is
%% still nasty...
-define(CALLFLOW_VIEW, {<<"callflow">>, <<"listing_by_number">>}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Seach the accounts for number assignements and ensure the routes
%% exist
%% @end
%%--------------------------------------------------------------------
-spec reconcile/0 :: () -> 'done'.
-spec reconcile/1 :: (Account) -> 'done' when
      Account :: string() | binary() | 'all'.
-spec reconcile/2 :: (AccountId, IsTSAccount) -> 'done' when
      AccountId :: binary() | string() | 'all',
      IsTSAccount :: 'undefined' | boolean().

reconcile() ->
    reconcile(all).

reconcile(all) ->
    reconcile(all, undefined);
reconcile(AccountId) when not is_binary(AccountId) ->
    reconcile(wh_util:to_binary(AccountId));
reconcile(AccountId) ->
    case couch_mgr:lookup_doc_rev(?TS_DB, AccountId) of
        {ok, _} ->
            reconcile(AccountId, true);
        {error, _} ->
            reconcile(AccountId, false)
    end.

reconcile(all, undefined) ->
    reconcile_accounts(),
    ok = reconcile_trunkstore(),
    done;
reconcile(AccountId, TSAccount) when not is_binary(AccountId) ->
    reconcile(wh_util:to_binary(AccountId), TSAccount);
reconcile(AccountId, true) ->
    Numbers = get_trunkstore_account_numbers(AccountId),
    _ = reconcile_account_route(whapps_util:get_db_name(AccountId, raw), Numbers),
    done;
reconcile(AccountId, false) ->
    Db = whapps_util:get_db_name(AccountId, encoded),
    Numbers = get_callflow_account_numbers(Db),
    _ = reconcile_account_route(whapps_util:get_db_name(AccountId, raw), Numbers),
    done.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Run validity checks on the routes currently in the db
%% @end
%%--------------------------------------------------------------------
-spec validate_routes/0 :: () -> done.
validate_routes() ->
    ok = find_duplicate_numbers(),
%%    find_missing_accounts(),
    done.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Lookup a number in the route db and return the account ID if known
%% @end
%%--------------------------------------------------------------------
-spec lookup_number/1 :: (Number) -> tuple(ok, binary()) | tuple(error, atom()) when
      Number :: string().
lookup_number(Number) ->
    gen_server:call(stepswitch_inbound, {lookup_number, Number}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Instructs stepswitch_outbound to re-scan the resource db and
%% refresh the cache.
%% @end
%%--------------------------------------------------------------------
-spec reload_resources/0 :: () -> ok.
reload_resources() ->
    gen_server:call(stepswitch_outbound, {reload_resrcs}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns a list of tuples that represent the routing logic for the
%% provided number and flags.  The tuple containts:
%% {Resource ID, Delay (in seconds), SIP URI}
%% @end
%%--------------------------------------------------------------------
-spec process_number/1 :: (Number) -> list()|tuple(error, atom()) when
      Number :: string().
-spec process_number/2 :: (Number, Flags) -> list()|tuple(error, atom()) when
      Number :: string(),
      Flags :: list().

process_number(Number) ->
    gen_server:call(stepswitch_outbound, {process_number, Number}).

process_number(Number, Flags) ->
    gen_server:call(stepswitch_outbound, {process_number, Number, Flags}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop over the accounts and try to reconcile the stepswitch routes
%% with the numbers assigned in the account
%% @end
%%--------------------------------------------------------------------
-spec reconcile_accounts/0 :: () -> ok.
reconcile_accounts() ->
    _ = [begin
	     Numbers = get_callflow_account_numbers(AccountId),
	     reconcile_account_route(
	       whapps_util:get_db_name(AccountId, raw), Numbers)
	 end
	 || AccountId <- whapps_util:get_all_accounts(encoded)],
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given an account create a json object of all numbers that look to
%% external (TODO: currently just uses US rules).
%% @end
%%--------------------------------------------------------------------
-spec get_callflow_account_numbers/1 :: (AccountId) -> json_object() when
      AccountId :: binary().
get_callflow_account_numbers(AccountId) ->
    case couch_mgr:get_all_results(AccountId, ?CALLFLOW_VIEW) of
        {ok, Numbers} ->
            {struct, [{Num, ?EMPTY_JSON_OBJECT}
                      || Number <- Numbers
                             ,begin
                                  Num = wh_util:to_e164(wh_json:get_value(<<"key">>, Number)),
                                  is_binary(Num) andalso re:run(Num, <<"^\\+{0,1}1{0,1}(\\d{10})$">>) =/= nomatch
                              end]};
        {error, _} ->
            ?EMPTY_JSON_OBJECT
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reconciles number assignments in trunkstore with stepswitch
%% route documents (tmp solution until trunkstore follows the
%% account db structure)
%% @end
%%--------------------------------------------------------------------
-spec reconcile_trunkstore/0 :: () -> ok | tuple(error, atom()).
reconcile_trunkstore() ->
    case couch_mgr:all_docs(?TS_DB) of
        {ok, JObj} ->
            _ = [begin
		     AccountId = wh_json:get_value(<<"id">>, Account),
		     Numbers = get_trunkstore_account_numbers(AccountId),
		     reconcile_account_route(AccountId, Numbers)
		 end
		 || Account <- JObj],
            ok;
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a document info json object from trunkstore returns true if
%% it is a 'info_' document (IE: trunkstore account)
%% @end
%%--------------------------------------------------------------------
-spec is_trunkstore_account/1 :: (JObj) -> boolean() when
      JObj :: json_object().
is_trunkstore_account(JObj) ->
    wh_json:get_value(<<"type">>, JObj) =:= <<"sys_info">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a trunkstore account id this function builds a json object
%% containing all numbers assigned to it
%% @end
%%--------------------------------------------------------------------
-spec get_trunkstore_account_numbers/1 :: (Account) -> json_object() when
      Account :: binary().
get_trunkstore_account_numbers(Account) ->
    case couch_mgr:open_doc(?TS_DB, Account) of
        {ok, JObj} ->
	    case is_trunkstore_account(JObj) of
		true ->
                    ?LOG("account ~s is a trunkstore doc...", [Account]),
		    Assigned = [wh_json:get_value(<<"DIDs">>, Server, ?EMPTY_JSON_OBJECT)
				|| Server <- wh_json:get_value(<<"servers">>, JObj, ?EMPTY_JSON_OBJECT)],
		    Unassigned = [wh_json:get_value(<<"DIDs_Unassigned">>, JObj, ?EMPTY_JSON_OBJECT)],
		    {struct, lists:foldr(fun({struct, Numbers}, Acc) ->
						 Numbers ++ Acc;
					    (_, Acc) -> Acc
					 end, [], Assigned ++ Unassigned)};
		false -> ?EMPTY_JSON_OBJECT
	    end;
        {error, _} ->
            ?EMPTY_JSON_OBJECT
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates or creates a route document for the given account with the
%% provided numbers
%% @end
%%--------------------------------------------------------------------
-spec reconcile_account_route/2 :: (AccountId, Numbers) -> tuple(ok, json_object() | json_objects())
                                                               | tuple(error, atom()) when
      AccountId :: binary(),
      Numbers :: json_object().

reconcile_account_route(AccountId, ?EMPTY_JSON_OBJECT) ->
    case couch_mgr:lookup_doc_rev(?ROUTES_DB, AccountId) of
        {ok, Rev} ->
            ?LOG("account ~s no longer has any routes", [AccountId]),
            couch_mgr:del_doc(?ROUTES_DB, {struct, [{<<"_id">>, AccountId}, {<<"_rev">>, Rev}]});
        {error, _} ->
            ?LOG("account ~s has no routes", [AccountId]),
            {error, skipped}
    end;
reconcile_account_route(AccountId, Numbers) ->
    ?LOG_SYS("reconciled route for ~s", [AccountId]),
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    case couch_mgr:open_doc(?ROUTES_DB, AccountId) of
        {ok, J0} ->
            J1 = wh_json:set_value(<<"numbers">>, Numbers, J0),
            J2 = wh_json:set_value(<<"pvt_modified">>, Timestamp, J1),
            couch_mgr:ensure_saved(?ROUTES_DB, J2);
        {error, _} ->
            couch_mgr:ensure_saved(?ROUTES_DB, {struct, [
                                                         {<<"_id">>, AccountId}
                                                         ,{<<"pvt_type">>, <<"route">>}
                                                         ,{<<"pvt_modified">>, Timestamp}
                                                         ,{<<"pvt_created">>, Timestamp}
                                                         ,{<<"numbers">>, Numbers}
                                                        ]})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% prints to the log if any numbers are assigned to more than one
%% account.
%% @end
%%--------------------------------------------------------------------
-spec find_duplicate_numbers/0 :: () -> ok|tuple(error, atom()).
find_duplicate_numbers() ->
    case couch_mgr:get_results(?ROUTES_DB, ?LIST_ROUTE_DUPS, [{<<"group">>, <<"true">>}]) of
        {ok, Routes} ->
            _ = [begin
		     Accounts = [", " ++ Account || Account <- wh_json:get_value([<<"value">>, <<"accounts">>], Route, [])],
		     Number = wh_json:get_value(<<"key">>, Route),
		     ?LOG_SYS("the number ~s routes to multiple accounts~s", [Number, Accounts])
		 end
		 || Route <- Routes,
		    wh_json:get_value([<<"value">>, <<"total">>], Route, 0) > 1],
            ok;
        {error, _}=E ->
            ?LOG_SYS("unable to check for duplicate routes ~p~n", [E]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% prints to the log if any routes have numbers assigned but the
%% account does not exist.  However, until trunkstore is updated
%% this will create false errors on TS accounts.
%% @end
%%--------------------------------------------------------------------
%-spec(find_missing_accounts/0 :: () -> ok|tuple(error, atom())).
%find_missing_accounts() ->
%    case couch_mgr:get_all_results(?ROUTES_DB, ?LIST_ROUTE_ACCOUNTS) of
%        {ok, Routes} ->
%            [begin
%                 AccountId = wh_json:get_value(<<"key">>, Route),
%                 Numbers = wh_json:get_value(<<"value">>, Route, []),
%                 [?LOG_SYS("the number ~s routes to a non-existant account ~s", [Number, AccountId])
%                  || Number <- Numbers]
%             end
%             || Route <- Routes
%                    ,not couch_mgr:db_exists(wh_json:get_value(<<"key">>, Route))
%                    ,length(wh_json:get_value(<<"value">>, Route, [])) > 0],
%            ok;
%        {error, _}=E ->
%            ?LOG_SYS("unable to check for missing accounts ~p~n", [E]),
%            E
%    end.
