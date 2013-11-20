%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Registration viewer / creator
%%%
%%% GET /v1/accounts/{account_id}/registrations :
%%%   Get a list of account registrations
%%% GET /v1/accounts/{account_id}/registrations/count :
%%%   Get a count of account registrations
%%% GET /v1/registrations :
%%%   Get a count of system-wide registrations - for superduper admins only
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_registrations).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,authorize/2
         ,validate/1, validate/2
         ,lookup_regs/1
        ]).

-include("../crossbar.hrl").

-define(MASK_REG_FIELDS, [<<"Account-DB">>
                          ,<<"Account-ID">>
                          ,<<"App-Name">>
                          ,<<"App-Version">>
                          ,<<"Event-Category">>
                          ,<<"Event-Name">>
                          ,<<"Server-ID">>
                         ]).

-define(COUNT_PATH_TOKEN, <<"count">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.registrations">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.registrations">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.registrations">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(?COUNT_PATH_TOKEN) ->
    [?HTTP_GET].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.
resource_exists(?COUNT_PATH_TOKEN) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context(), path_token()) -> boolean().

authorize(#cb_context{req_nouns=[{<<"registrations">>
                                      ,[?COUNT_PATH_TOKEN]}
                                ]}=Context, ?COUNT_PATH_TOKEN) ->
    cb_modules_util:is_superduper_admin(Context);
authorize(_, _) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    crossbar_util:response(lookup_regs(Context), Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?COUNT_PATH_TOKEN) ->
    crossbar_util:response(
      wh_json:from_list([{<<"count">>, count_registrations(Context)}])
      ,Context
     ).

-spec lookup_regs(ne_binary()) -> wh_json:objects().
lookup_regs(Context) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = cb_context:account_db(Context),
    AccountRealm = wh_util:get_account_realm(AccountDb, AccountId),
    Req = [{<<"Realm">>, AccountRealm}
           ,{<<"Fields">>, []}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    ReqResp = whapps_util:amqp_pool_collect(Req
                                            ,fun wapi_registration:publish_query_req/1
                                            ,'ecallmgr'
                                           ),
    case ReqResp of
        {'error', _} -> [];
        {_, JObjs} ->
            merge_responses(JObjs)
    end.

-spec merge_responses(wh_json:objects()) -> wh_json:objects().
merge_responses(JObjs) ->
    [normalize_registration(JObj)
     || {_, JObj} <- dict:to_list(merge_responses(JObjs, dict:new()))
    ].

-spec merge_responses(wh_json:objects(), dict()) -> dict().
merge_responses([], Regs) -> Regs;
merge_responses([JObj|JObjs], Regs) ->
    merge_responses(JObjs, merge_response(JObj, Regs)).

-spec merge_response(wh_json:object(), dict()) -> dict().
merge_response(JObj, Regs) ->
    lists:foldl(fun(J, R) ->
                        case wh_json:get_ne_value(<<"Contact">>, J) of
                            'undefined' -> R;
                            Contact -> dict:store(Contact, J, R)
                        end
                end, Regs, wh_json:get_value(<<"Fields">>, JObj, [])).

-spec normalize_registration(wh_json:object()) -> wh_json:object().
normalize_registration(JObj) ->
    Contact = wh_json:get_binary_value(<<"Contact">>, JObj, <<>>),
    Updaters = [fun(J) -> wh_json:delete_keys(?MASK_REG_FIELDS, J) end
                ,fun(J) ->
                         case re:run(Contact, "sip:[^@]+@(.*?):([0-9]+)", [{'capture', [1, 2], 'binary'}]) of
                             {'match',[Ip, Port]} ->
                                 wh_json:set_value(<<"Contact-IP">>, Ip
                                                   ,wh_json:set_value(<<"Contact-Port">>, Port, J)
                                                  );
                             _Else -> J
                         end
                 end
                ,fun(J) ->
                         case re:run(Contact, "received=sip:([^:;]+):?([0-9]+)?", [{'capture', [1, 2], 'binary'}]) of
                             {'match',[Ip, Port]} ->
                                 wh_json:set_value(<<"Received-IP">>, Ip
                                                   ,wh_json:set_value(<<"Received-Port">>, Port, J)
                                                  );
                             _Else -> J
                         end
                 end
                ,fun(J) ->
                         case re:run(Contact, "fs_path=sip:([^:;]+):?([0-9]+)?", [{'capture', [1, 2], 'binary'}]) of
                             {'match',[Ip, Port]} ->
                                 wh_json:set_value(<<"Proxy-IP">>, Ip
                                                   ,wh_json:set_value(<<"Proxy-Port">>, Port, J)
                                                  );
                             _Else -> J
                         end
                 end
               ],
    wh_json:normalize(lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters)).

-spec count_registrations(cb_context:context()) -> cb_context:context().
count_registrations(Context) ->
    Req = [{<<"Realm">>, get_realm_for_counting(Context)}
           ,{<<"Fields">>, []}
           ,{<<"Count-Only">>, 'true'}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    ReqResp = whapps_util:amqp_pool_request(Req
                                            ,fun wapi_registration:publish_query_req/1
                                            ,fun wapi_registration:query_resp_v/1
                                           ),
    case ReqResp of
        {'error', _E} -> lager:debug("no resps found: ~p", [_E]), 0;
        {'ok', JObj} -> wh_json:get_integer_value(<<"Count">>, JObj);
        {'timeout', _} -> lager:debug("timed out query for counting regs"), 0
    end.

get_realm_for_counting(Context) ->
    AccountId = cb_context:account_id(Context),
    case AccountId =:= 'undefined' of
        'false' ->
            AccountDb = cb_context:account_db(Context),
            wh_util:get_account_realm(AccountDb, AccountId);
        'true' ->
            lager:debug("no account id means all regs are counted"),
            <<"all">>
    end.
