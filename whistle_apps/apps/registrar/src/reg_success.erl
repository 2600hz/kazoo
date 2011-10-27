%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle successful registrations
%%% @end
%%% Created : 19 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(reg_success).

-export([init/0, handle_req/2]).

-include("reg.hrl").

init() ->
    ok.

-spec handle_req/2 :: (JObj, Props) -> no_return() when
      JObj :: json_object(),
      Props :: proplist().
handle_req(JObj, Props) ->
    Cache = props:get_value(cache, Props),

    ?LOG_START("received registration success"),
    true = wh_api:reg_success_v(JObj),

    [User, AfterAt] = binary:split(wh_json:get_value(<<"Contact">>, JObj), <<"@">>), % only one @ allowed

    AfterUnquoted = wh_util:to_binary(mochiweb_util:unquote(AfterAt)),
    Contact1 = binary:replace(<<User/binary, "@", AfterUnquoted/binary>>, [<<"<">>, <<">">>], <<>>, [global]),

    JObj1 = wh_json:set_value(<<"Contact">>, Contact1, JObj),

    Id = wh_util:to_binary(wh_util:to_hex(erlang:md5(Contact1))),
    CacheKey = reg_util:cache_reg_key(Id),
    Expiry = wh_json:get_integer_value(<<"Expires">>, JObj, 3600),
    Fudge = round(Expiry * 0.25),
    Expires = Expiry + Fudge + 60, %% increased, yealinks dont even try till ten seconds after the fact...

    Username = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),

    case wh_cache:peek_local(Cache, CacheKey) of
	{error, not_found} ->
	    ?LOG("Cache miss, rm old and save new ~s@~s for ~p seconds", [Username, Realm, Expires]),

            %% do the cache work first so requests dont have to wait till after the db work
            %% for the latest greatest, plus if the db crashes we still want to keep the memory going
	    wh_cache:store_local(Cache, CacheKey, JObj1, Expires),
	    wh_cache:store_local(Cache, reg_util:cache_user_to_reg_key(Realm, Username), CacheKey, Expires),
	    ?LOG("new registration contact ~p hashed as ~s", [Contact1, Id]),

	    reg_util:remove_old_regs(Username, Realm, Cache),
	    {ok, _} = reg_util:store_reg(JObj1, Id, Contact1),

	    ?LOG_END("updated datastore registrations");
	{ok, _} ->
	    ?LOG("contact for ~s@~s found in cache", [Username, Realm]),
	    wh_cache:store_local(Cache, CacheKey, JObj1, Expires),
	    wh_cache:store_local(Cache, reg_util:cache_user_to_reg_key(Realm, Username), CacheKey, Expires),

	    ?LOG_END("update cache expiration timer for ~s", [Id])
    end.
