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

-include("reg.hrl").

init() -> 'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = wapi_registration:success_v(JObj),
    _ = wh_util:put_callid(JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    Username = wh_json:get_value(<<"Username">>, JObj),
    Routines = [fun fix_contact/3
                ,fun maybe_update_jobj/3
                ,fun store_reg_success/3
                ,fun maybe_send_new_notice/3
               ],
    lists:foldl(fun(F, J) -> F(J, Username, Realm) end, JObj, Routines).

-spec send_new_register(wh_json:object()) -> 'ok'.
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

fix_contact(JObj, _, _) ->
    [User, AfterAt] = binary:split(wh_json:get_value(<<"Contact">>, JObj), <<"@">>), % only one @ allowed
    AfterUnquoted = wh_util:to_binary(mochiweb_util:unquote(AfterAt)),
    Contact = binary:replace(<<User/binary, "@", AfterUnquoted/binary>>, [<<"<">>, <<">">>], <<>>, ['global']),
    lager:debug("new registration contact: ~s", [Contact]),
    wh_json:set_value(<<"Contact">>, Contact, JObj).

maybe_update_jobj(JObj, Username, Realm) ->
    case reg_util:lookup_auth_user(Username, Realm) of
        {'error', _} -> JObj;
        {'ok', #auth_user{}=AuthUser} ->
            update_jobj(JObj, AuthUser)
    end.

update_jobj(JObj, AuthUser) ->
    Props = [{<<"Authorizing-ID">>, AuthUser#auth_user.authorizing_id}
             ,{<<"Account-ID">>, AuthUser#auth_user.account_id}
             ,{<<"Account-DB">>, AuthUser#auth_user.account_db}
             ,{<<"Suppress-Unregister-Notify">>
                  ,AuthUser#auth_user.suppress_unregister_notifications}
            ],
    wh_json:set_values(props:filter_undefined(Props), JObj).

maybe_send_new_notice(JObj, Username, Realm) ->
    case wh_cache:peek_local(?REGISTRAR_CACHE, reg_util:cache_user_to_reg_key(Realm, Username)) of
        {'ok', _} -> JObj;
        {'error', 'not_found'} ->
            catch send_new_register(JObj)
    end.

store_reg_success(JObj, Username, Realm) ->
    CacheProps = [{'expires', reg_util:get_expires(JObj)}
                  ,{'callback', fun reg_util:reg_removed_from_cache/3}
                 ],
    wh_cache:store_local(?REGISTRAR_CACHE
                         ,reg_util:cache_user_to_reg_key(Realm, Username)
                         ,JObj
                         ,CacheProps
                        ),
    Contact = wh_json:get_value(<<"Contact">>, JObj),
    lager:info("successful registration ~s@~s: ~s", [Username, Realm, Contact]).
