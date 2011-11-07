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

-spec handle_req/2 :: (json_object(), proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    wh_util:put_callid(ApiJObj),

    ?LOG_START("received registration success"),
    true = wapi_registration:success_v(ApiJObj),

    [User, AfterAt] = binary:split(wh_json:get_value(<<"Contact">>, ApiJObj), <<"@">>), % only one @ allowed

    AfterUnquoted = wh_util:to_binary(mochiweb_util:unquote(AfterAt)),
    Contact1 = binary:replace(<<User/binary, "@", AfterUnquoted/binary>>, [<<"<">>, <<">">>], <<>>, [global]),

    ?LOG("registration contact: ~s", [Contact1]),

    JObj = wh_json:set_value(<<"Contact">>, Contact1, ApiJObj),

    Expires = reg_util:get_expires(JObj),

    Realm = wh_json:get_value(<<"Realm">>, JObj),
    Username = wh_json:get_value(<<"Username">>, JObj),

    {ok, Cache} = registrar_sup:cache_proc(),
    wh_cache:store_local(Cache, reg_util:cache_user_to_reg_key(Realm, Username), JObj, Expires),

    ?LOG_END("cached registration ~s@~s for ~psec", [Username, Realm, Expires]).
