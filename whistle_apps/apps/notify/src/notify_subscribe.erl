%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 6 Oct 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_subscribe).

-export([init/0, handle_req/2]).

-include("notify.hrl").
-include_lib("whistle/include/wh_types.hrl").

-spec init/0 :: () -> 'ok'.
init() ->
    ok.

-spec handle_req/2 :: (JObj, Props) -> no_return() when
      JObj :: json_object(),
      Props :: proplist().
handle_req(JObj, _Props) ->
    true = wh_api:presence_subscr_v(JObj),
    ?LOG_START("received presence subscription"),

    PUser = wh_json:get_value(<<"To-User">>, JObj),
    PRealm = wh_json:get_value(<<"To-Host">>, JObj),

    WUser = wh_json:get_value(<<"From-User">>, JObj),
    WRealm = wh_json:get_value(<<"From-Host">>, JObj),

    case whapps_util:get_account_by_realm(WRealm) of
        {error, _} ->
            ok;
        {ok, WADb} when WRealm =:= PRealm->
            AccountId = whapps_util:get_db_name(WADb, raw),
            store_and_send(WUser, AccountId, PUser, AccountId, JObj);
        {ok, WADb} ->
            WAId = whapps_util:get_db_name(WADb, raw),
            case whapps_util:get_account_by_realm(PRealm) of
                {error, _} ->
                    ok;
                {ok, PADb} ->
                    PAId = whapps_util:get_db_name(PADb, raw),
                    store_and_send(WUser, WAId, PUser, PAId, JObj)
            end
    end.

store_and_send(WUser, WAccount, PUser, PAccount, JObj) ->
    {ok, Cache} = notify_sup:cache_proc(),

    Expiry = wh_json:get_integer_value(<<"Expires">>, JObj, 3600),
    Expires = round(Expiry * 1.25),

    wh_cache:store_local(Cache, {notify_watcher, WUser, WAccount}, JObj, Expires),

    case wh_cache:fetch_local(Cache, {notify_presentity, PUser, PAccount}) of
	{error, not_found} ->
            wh_cache:store_local(Cache, {notify_presentity, PUser, PAccount}
                                 ,[{WUser, WAccount}]),
	    ?LOG_END("adding presentity ~s@~s to cache", [PUser, PAccount]);
	{ok, Watchers} ->
            NewWatchers = [{WUser, WAccount}|[W || W <- Watchers, W =/= {WUser, WAccount}]],
            wh_cache:store_local(Cache, {notify_presentity, PUser, PAccount}, NewWatchers),
	    ?LOG_END("updated presentity ~s@~s in cache", [PUser, PAccount])
    end.
