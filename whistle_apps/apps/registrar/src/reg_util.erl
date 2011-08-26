%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Shared functions
%%% @end
%%% Created : 19 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(reg_util).

-export([lookup_auth_user/3, send_resp/2, send_resp/3, prime_cache/2]).
-export([cache_reg_key/1, cache_user_to_reg_key/2, cache_user_key/2]).
-export([remove_old_regs/3, store_reg/3, lookup_registration/3, lookup_registrations/2]).
-include("reg.hrl").

cache_reg_key(Id) -> {?MODULE, registration, Id}.
cache_user_to_reg_key(Realm, User) -> {?MODULE, registration, Realm, User}.
cache_user_key(Realm, User) -> {?MODULE, sip_credentials, Realm, User}.

-spec lookup_registration/3 :: (Realm, Username, Cache) -> {'ok', json_object()} | {'error', 'not_found'} when
      Realm :: binary(),
      Username :: binary(),
      Cache :: pid().
lookup_registration(Realm, Username, Cache) ->
    {'ok', RegKey} = wh_cache:fetch_local(Cache, cache_user_to_reg_key(Realm, Username)),
    wh_cache:fetch_local(Cache, RegKey).

-spec lookup_registrations/2 :: (Realm, Cache) -> {'ok', json_objects()} when
      Realm :: binary(),
      Cache :: pid().
lookup_registrations(Realm, Cache) ->
    Users = wh_cache:filter_local(Cache, fun({?MODULE, registration, Realm1, _}, _) when Realm =:= Realm1 -> true;
					    (_K, _V) -> false
					 end),
    {'ok', [ begin {'ok', RegDoc} = wh_cache:fetch_local(Cache, RegKey), RegDoc end
	   || {_RealmKey, RegKey} <- Users]}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% store a sucessful registration in the database
%% @end
%%-----------------------------------------------------------------------------
-spec store_reg/3 :: (JObj, Id, Contact) -> {'ok', json_object()} when
      JObj :: json_object(),
      Id :: binary(),
      Contact :: binary().
store_reg(JObj, Id, Contact) ->
    RegDoc = wh_json:set_value(<<"_id">>, Id, wh_json:set_value(<<"Contact">>, Contact, JObj)),
    {'ok', {struct, _}} = couch_mgr:ensure_saved(?REG_DB, RegDoc).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% periodically remove expired registrations
%% @end
%%-----------------------------------------------------------------------------
-spec remove_old_regs/3 :: (User, Realm, Cache) -> ok when
      User :: binary(),
      Realm :: binary(),
      Cache :: pid().
remove_old_regs(User, Realm, Cache) ->
    case couch_mgr:get_results(<<"registrations">>, <<"registrations/newest">>,
			       [{<<"startkey">>, [Realm, User, 0]}, {<<"endkey">>, [Realm, User, ?EMPTY_JSON_OBJECT]}]) of
	{'ok', [OldDoc]} ->
	    ID = wh_json:get_value(<<"id">>, OldDoc),
	    wh_cache:erase_local(Cache, cache_reg_key(ID)),
	    {'ok', Rev} = couch_mgr:lookup_doc_rev(?REG_DB, ID),
	    couch_mgr:del_doc(?REG_DB, {struct, [{<<"_id">>, ID}, {<<"_rev">>, Rev}]});
	{'ok', OldDocs} ->
	    spawn(fun() ->
			  DelDocs = [ begin
					  ID = wh_json:get_value(<<"id">>, Doc),
					  wh_cache:erase_local(Cache, cache_reg_key(ID)),
					  case couch_mgr:lookup_doc_rev(<<"registrations">>, ID) of
					      {'ok', Rev} -> {struct, [{<<"_id">>, ID}, {<<"_rev">>, Rev}]};
					      _ -> ?EMPTY_JSON_OBJECT
					  end
				      end || Doc <- OldDocs ],
			  couch_mgr:del_docs(<<"registrations">>, DelDocs)
		  end), ok;
	_ -> ok
    end.

-spec prime_cache/2 :: (Pid, ViewResult) -> no_return() when
      Pid :: pid(),
      ViewResult :: json_object().
prime_cache(Pid, ViewResult) ->
    DocId = wh_json:get_value(<<"id">>, ViewResult),
    {'ok', JObj} = couch_mgr:open_doc(?REG_DB, DocId),
    Expires = wh_util:current_tstamp() + wh_json:get_integer_value(<<"Expires">>, JObj, 3600),
    CacheRegKey = cache_reg_key(DocId),
    wh_cache:store_local(Pid, CacheRegKey, JObj, Expires),

    User = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    wh_cache:store_local(Pid, cache_user_to_reg_key(Realm, User), CacheRegKey, Expires).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send a payload to a targeted queue
%% @end
%%-----------------------------------------------------------------------------
send_resp(Payload, RespQ) ->
    send_resp(Payload, RespQ, <<"application/json">>).

send_resp(Payload, RespQ, ContentType) ->
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% look up the user and realm in the database and return the result
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_auth_user/3 :: (Name, Realm, Cache) -> {'ok', json_object()} when
      Name :: binary(),
      Realm :: binary(),
      Cache :: pid().
lookup_auth_user(Name, Realm, Cache) ->
    ?LOG("looking up ~s@~s", [Name, Realm]),
    CacheKey = cache_user_key(Realm, Name),
    case wh_cache:fetch_local(Cache, CacheKey) of
	{'error', not_found} ->
	    {'ok', UserJObj} = lookup_auth_user(Name, Realm),
	    ?LOG("Storing ~s@~s in cache", [Name, Realm]),
	    wh_cache:store_local(Cache, CacheKey, UserJObj),
	    {'ok', UserJObj};
	{'ok', _}=OK ->
	    ?LOG("Pulling auth user from cache"),
	    OK
    end.

-spec lookup_auth_user/2 :: (Name, Realm) -> {'ok', json_object()} | {'error', 'no_user_found'} when
      Name :: binary(),
      Realm :: binary().
lookup_auth_user(Name, Realm) ->
    case whapps_util:get_account_by_realm(Realm) of
	{'error', E} ->
	    ?LOG("Failed to lookup realm ~s in accounts: ~p", [Realm, E]),
	    lookup_auth_user_in_agg(Name, Realm);
	{'ok', []} ->
	    ?LOG("Failed to find realm ~s in accounts", [Realm]),
	    lookup_auth_user_in_agg(Name, Realm);
	{'ok', AccountDB} ->
	    lookup_auth_user_in_account(Name, Realm, AccountDB)
    end.

-spec lookup_auth_user_in_agg/2 :: (Name, Realm) -> {'ok', json_object()} | {'error', 'no_user_found'} when
      Name :: binary(),
      Realm :: binary().
lookup_auth_user_in_agg(Name, Realm) ->
    case couch_mgr:get_results(?AUTH_DB, <<"credentials/lookup">>, [{<<"key">>, [Realm, Name]}, {<<"include_docs">>, true}]) of
	{'error', R} ->
	    ?LOG_END("failed to look up SIP credentials ~p in aggregate", [R]),
	    {'error', 'no_user_found'};
	{'ok', []} ->
	    ?LOG("~s@~s not found in aggregate", [Name, Realm]),
	    {'error', 'no_user_found'};
	{'ok', [User|_]} ->
	    ?LOG("~s@~s found in aggregate", [Name, Realm]),
	    {'ok', User}
    end.

-spec lookup_auth_user_in_account/3 :: (Name, Realm, AccountDB) -> {'ok', json_object()} | {'error', 'no_user_found'} when
      Name :: binary(),
      Realm :: binary(),
      AccountDB :: binary().
lookup_auth_user_in_account(Name, Realm, AccountDB) ->
    case couch_mgr:get_results(AccountDB, <<"devices/sip_credentials">>, [{<<"key">>, [Realm, Name]}, {<<"include_docs">>, true}]) of
	{'error', R} ->
	    ?LOG("failed to look up SIP credentials in ~s: ~p", [AccountDB, R]),
	    lookup_auth_user_in_agg(Name, Realm);
	{'ok', []} ->
	    ?LOG("~s@~s not found in ~s", [Name, Realm, AccountDB]),
	    lookup_auth_user_in_agg(Name, Realm);
	{'ok', [User|_]} ->
	    ?LOG("~s@~s found in account db: ~s", [Name, Realm, AccountDB]),
	    {'ok', User}
    end.
