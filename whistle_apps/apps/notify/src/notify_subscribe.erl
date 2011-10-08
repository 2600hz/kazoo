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
    {ok, Cache} = notify_sup:cache_proc(),

    ?LOG_START("received presence subscription"),

    Expiry = wh_json:get_integer_value(<<"Expires">>, JObj, 3600),
    Expires = round(Expiry * 1.25),

    PUser = wh_json:get_value(<<"To-User">>, JObj),
    PRealm = wh_json:get_value(<<"To-Host">>, JObj),

    WUser = wh_json:get_value(<<"From-User">>, JObj),
    WRealm = wh_json:get_value(<<"From-Host">>, JObj),

    wh_cache:store_local(Cache, {notify_watcher, WUser, WRealm}, JObj, Expires),

    case wh_cache:fetch_local(Cache, {notify_presentity, PUser, PRealm}) of
	{error, not_found} ->
            wh_cache:store_local(Cache, {notify_presentity, PUser, PRealm}
                                 ,[{WUser, WRealm}]),
	    ?LOG_END("adding presentity ~s@~s to cache", [PUser, PRealm]);
	{ok, Watchers} ->
            NewWatchers = [{WUser, WRealm}|[W || W <- Watchers, W =/= {WUser, WRealm}]],
            wh_cache:store_local(Cache, {notify_presentity, PUser, PRealm}, NewWatchers),
	    ?LOG_END("updated presentity ~s@~s in cache", [PUser, PRealm])
    end.
