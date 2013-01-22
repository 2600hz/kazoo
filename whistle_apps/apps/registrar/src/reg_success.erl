%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Handle successful registrations
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(reg_success).

-export([init/0, handle_req/2]).

-include_lib("registrar/src/reg.hrl").

init() ->
    ok.

-spec handle_req/2 :: (wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    true = wapi_registration:success_v(ApiJObj),
    _ = wh_util:put_callid(ApiJObj),

    [User, AfterAt] = binary:split(wh_json:get_value(<<"Contact">>, ApiJObj), <<"@">>), % only one @ allowed

    AfterUnquoted = wh_util:to_binary(mochiweb_util:unquote(AfterAt)),
    Contact1 = binary:replace(<<User/binary, "@", AfterUnquoted/binary>>, [<<"<">>, <<">">>], <<>>, [global]),

    lager:debug("new registration contact: ~s", [Contact1]),

    JObj1 = wh_json:set_value(<<"Contact">>, Contact1, ApiJObj),

    Expires = reg_util:get_expires(JObj1),

    Realm = wh_json:get_value(<<"Realm">>, JObj1),
    Username = wh_json:get_value(<<"Username">>, JObj1),

    JObj2 = case reg_util:lookup_auth_user(Username, Realm) of
                {ok, AuthJObj} ->
                    Updaters = [fun(J) ->
                                        AuthId = wh_json:get_value([<<"doc">>, <<"_id">>], AuthJObj), 
                                        wh_json:set_value(<<"Authorizing-ID">>, AuthId, J) 
                                end
                                ,fun(J) ->
                                         AccountId = wh_json:get_value([<<"doc">>, <<"pvt_account_id">>], AuthJObj), 
                                         wh_json:set_value(<<"Account-ID">>, AccountId, J) 
                                 end
                                ,fun(J) ->  
                                         AccountDb = wh_json:get_value([<<"doc">>, <<"pvt_account_db">>], AuthJObj), 
                                         wh_json:set_value(<<"Account-DB">>, AccountDb, J)
                                 end
                                ,fun(J) ->  
                                         SuppressNotify = wh_json:is_true([<<"doc">>, <<"suppress_unregister_notifications">>], AuthJObj), 
                                         wh_json:set_value(<<"Suppress-Unregister-Notify">>, SuppressNotify, J)
                                 end
                               ],
                    lists:foldr(fun(F, J) -> F(J) end, JObj1, Updaters);
                {error, not_found} ->
                    JObj1
    end,

    _ = case wh_cache:peek_local(?REGISTRAR_CACHE, reg_util:cache_user_to_reg_key(Realm, Username)) of
            {ok, _} -> ok;
            {error, not_found} -> catch send_new_register(JObj2)
        end,

    wh_cache:store_local(?REGISTRAR_CACHE
                         ,reg_util:cache_user_to_reg_key(Realm, Username)
                         ,JObj2
                         ,Expires
                         ,fun reg_util:reg_removed_from_cache/3
                        ),

    lager:debug("cached registration ~s@~s for ~p s", [Username, Realm, Expires]).

-spec send_new_register/1 :: (wh_json:object()) -> 'ok'.
send_new_register(JObj) ->
    Updaters = [fun(J) -> wh_json:set_value(<<"Event-Name">>,  <<"register">>, J) end
                ,fun(J) -> wh_json:set_value(<<"Event-Category">>, <<"notification">>, J) end 
                ,fun(J) -> wh_json:delete_key(<<"App-Version">>, J) end
                ,fun(J) -> wh_json:delete_key(<<"App-name">>, J) end 
                ,fun(J) -> wh_json:delete_key(<<"Server-ID">>, J) end
               ],
    lager:debug("sending new registration event for ~s@~s", [wh_json:get_value(<<"Username">>, JObj), wh_json:get_value(<<"Realm">>, JObj)]),
    Event = wh_json:to_proplist(lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters)) 
        ++ wh_api:default_headers(?APP_NAME, ?APP_VERSION),
    wapi_notifications:publish_register(Event).
