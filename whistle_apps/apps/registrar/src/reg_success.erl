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
    Expires = Expiry + Fudge,

    Username = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),

    case wh_cache:fetch_local(Cache, CacheKey) of
	{error, not_found} ->
	    ?LOG("contact for ~s@~s not in cache", [Username, Realm]),

	    reg_util:remove_old_regs(Username, Realm, Cache),
	    ?LOG("flushed users registrations"),

	    ?LOG("Cache miss, rm old and save new ~s for ~p seconds", [Id, Expires]),

	    wh_cache:store_local(Cache, CacheKey, JObj1, Expires),
	    wh_cache:store_local(Cache, reg_util:cache_user_to_reg_key(Realm, Username), CacheKey),
	    ?LOG_END("new contact hash ~s cached for ~p seconds", [Id, Expires]),

	    {ok, _} = reg_util:store_reg(JObj1, Id, Contact1),
	    ?LOG_END("new contact hash ~s stored for ~p seconds", [Id, Expires]);
	{ok, _} ->
	    ?LOG("contact for ~s@~s found in cache", [Username, Realm]),
	    wh_cache:store_local(Cache, CacheKey, JObj1, Expires),
	    wh_cache:store_local(Cache, reg_util:cache_user_to_reg_key(Realm, Username), CacheKey),

	    ?LOG_END("not verifying with DB, assuming cached JSON is valid")
    end.
