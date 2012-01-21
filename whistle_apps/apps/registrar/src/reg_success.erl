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

-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    wh_util:put_callid(ApiJObj),

    ?LOG_START("received registration success"),
    true = wapi_registration:success_v(ApiJObj),

    [User, AfterAt] = binary:split(wh_json:get_value(<<"Contact">>, ApiJObj), <<"@">>), % only one @ allowed

    AfterUnquoted = wh_util:to_binary(mochiweb_util:unquote(AfterAt)),
    Contact1 = binary:replace(<<User/binary, "@", AfterUnquoted/binary>>, [<<"<">>, <<">">>], <<>>, [global]),

    ?LOG("registration contact: ~s", [Contact1]),

    JObj1 = wh_json:set_value(<<"Contact">>, Contact1, ApiJObj),

    Expires = reg_util:get_expires(JObj1),

    Realm = wh_json:get_value(<<"Realm">>, JObj1),
    Username = wh_json:get_value(<<"Username">>, JObj1),

    JObj2 = case reg_util:lookup_auth_user(Username, Realm) of
                {ok, AuthJObj} ->
                    wh_json:set_value(<<"Authorizing-ID">>, wh_json:get_value([<<"doc">>, <<"_id">>], AuthJObj), JObj1);
                {error, not_found} ->
                    JObj1
    end,

    {ok, Cache} = registrar_sup:cache_proc(),
    wh_cache:store_local(Cache, reg_util:cache_user_to_reg_key(Realm, Username), JObj2, Expires),

    ?LOG_END("cached registration ~s@~s for ~psec", [Username, Realm, Expires]).
