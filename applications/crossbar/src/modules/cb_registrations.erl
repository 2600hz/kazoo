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
         ,delete/1, delete/2
        ]).

-include("crossbar.hrl").

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
    _ = crossbar_bindings:bind(<<"*.validate.registrations">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.registrations">>, ?MODULE, 'delete').

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_DELETE].
allowed_methods(?COUNT_PATH_TOKEN) ->
    [?HTTP_GET];
allowed_methods(_) ->
    [?HTTP_DELETE].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.
resource_exists(?COUNT_PATH_TOKEN) -> 'true';
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context(), path_token()) -> boolean().

authorize(Context, ?COUNT_PATH_TOKEN) ->
    authorize_admin(Context, cb_context:req_nouns(Context));
authorize(_, _) -> 'false'.

-spec authorize_admin(cb_context:context(), req_nouns()) -> boolean().
authorize_admin(Context, [{<<"registrations">>, [?COUNT_PATH_TOKEN]}]) ->
    cb_modules_util:is_superduper_admin(Context).

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
validate(Context) ->
    validate_registrations(Context, cb_context:req_verb(Context)).

-spec validate_registrations(cb_context:context(), http_method()) -> cb_context:context().
validate_registrations(Context, ?HTTP_GET) ->
    crossbar_util:response(lookup_regs(Context), Context);
validate_registrations(Context, ?HTTP_DELETE) ->
    crossbar_util:response(<<"ok">>, Context).

validate(Context, ?COUNT_PATH_TOKEN) ->
    validate_count(Context);
validate(Context, Username) ->
    validate_sip_username(Context, Username).

-spec validate_count(cb_context:context()) -> cb_context:context().
validate_count(Context) ->
    crossbar_util:response(
      kz_json:from_list([{<<"count">>, count_registrations(Context)}])
      ,Context
     ).

-spec validate_sip_username(cb_context:context(), ne_binary()) -> cb_context:context().
validate_sip_username(Context, Username) ->
    case sip_username_exists(Context, Username) of
        'true' ->
            crossbar_util:response(<<"ok">>, Context);
        'false' ->
            crossbar_util:response_bad_identifier(Username, Context)
    end.

-spec sip_username_exists(cb_context:context(), ne_binary()) -> boolean().
sip_username_exists(Context, Username) ->
    ViewOptions = [{'key', kz_term:to_lower_binary(Username)}],
    case kz_datamgr:get_results(cb_context:account_db(Context)
                               ,<<"devices/sip_credentials">>
                               ,ViewOptions
                               )
    of
        {'ok', [_]} -> 'true';
        _ -> 'false'
    end.

-spec delete(cb_context:context()) -> cb_context:context().
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context) ->
    crossbar_util:flush_registrations(Context),
    crossbar_util:response(<<"ok">>, Context).

delete(Context, Username) ->
    crossbar_util:flush_registration(Username, Context),
    crossbar_util:response(<<"ok">>, Context).

-spec lookup_regs(cb_context:context()) -> kz_json:objects().
lookup_regs(Context) ->
    AccountRealm = kz_accounts:get_account_realm(cb_context:account_id(Context)),
    Req = [{<<"Realm">>, AccountRealm}
           ,{<<"Fields">>, []}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    ReqResp = kapps_util:amqp_pool_collect(Req
                                            ,fun kapi_registration:publish_query_req/1
                                            ,{'ecallmgr', 'true'}
                                           ),
    case ReqResp of
        {'error', _} -> [];
        {_, JObjs} -> merge_responses(JObjs)
    end.

-spec merge_responses(kz_json:objects()) -> kz_json:objects().
merge_responses(JObjs) ->
    [normalize_registration(JObj)
     || {_, JObj} <- dict:to_list(merge_responses(JObjs, dict:new()))
    ].

-spec merge_responses(kz_json:objects(), dict:dict()) -> dict:dict().
merge_responses([], Regs) -> Regs;
merge_responses([JObj|JObjs], Regs) ->
    merge_responses(JObjs, merge_response(JObj, Regs)).

-spec merge_response(kz_json:object(), dict:dict()) -> dict:dict().
merge_response(JObj, Regs) ->
    lists:foldl(fun(J, R) ->
                        case kz_json:get_ne_value(<<"Contact">>, J) of
                            'undefined' -> R;
                            Contact -> dict:store(Contact, J, R)
                        end
                end, Regs, kz_json:get_value(<<"Fields">>, JObj, [])).

-spec normalize_registration(kz_json:object()) -> kz_json:object().
normalize_registration(JObj) ->
    Contact = kz_json:get_binary_value(<<"Contact">>, JObj, <<>>),
    Updaters = [fun(J) -> kz_json:delete_keys(?MASK_REG_FIELDS, J) end
                ,fun(J) ->
                         case re:run(Contact, "sip:[^@]+@(.*?):([0-9]+)", [{'capture', [1, 2], 'binary'}]) of
                             {'match',[Ip, Port]} ->
                                 kz_json:set_value(<<"Contact-IP">>, Ip
                                                   ,kz_json:set_value(<<"Contact-Port">>, Port, J)
                                                  );
                             _Else -> J
                         end
                 end
                ,fun(J) ->
                         case re:run(Contact, "received=sip:([^:;]+):?([0-9]+)?", [{'capture', [1, 2], 'binary'}]) of
                             {'match',[Ip, Port]} ->
                                 kz_json:set_value(<<"Received-IP">>, Ip
                                                   ,kz_json:set_value(<<"Received-Port">>, Port, J)
                                                  );
                             _Else -> J
                         end
                 end
                ,fun(J) ->
                         case re:run(Contact, "fs_path=sip:([^:;]+):?([0-9]+)?", [{'capture', [1, 2], 'binary'}]) of
                             {'match',[Ip, Port]} ->
                                 kz_json:set_value(<<"Proxy-IP">>, Ip
                                                   ,kz_json:set_value(<<"Proxy-Port">>, Port, J)
                                                  );
                             _Else -> J
                         end
                 end
               ],
    kz_json:normalize(lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters)).

-spec count_registrations(cb_context:context()) -> integer().
count_registrations(Context) ->
    Req = [{<<"Realm">>, get_realm_for_counting(Context)}
           ,{<<"Fields">>, []}
           ,{<<"Count-Only">>, 'true'}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    ReqResp = kapps_util:amqp_pool_request(Req
                                            ,fun kapi_registration:publish_query_req/1
                                            ,fun kapi_registration:query_resp_v/1
                                           ),
    case ReqResp of
        {'error', _E} -> lager:debug("no resps found: ~p", [_E]), 0;
        {'ok', JObj} -> kz_json:get_integer_value(<<"Count">>, JObj, 0);
        {'timeout', _} -> lager:debug("timed out query for counting regs"), 0
    end.

-spec get_realm_for_counting(cb_context:context()) -> ne_binary().
get_realm_for_counting(Context) ->
    case cb_context:account_id(Context) of
        'undefined' -> <<"all">>;
        _AccountId -> kz_accounts:get_account_realm(cb_context:account_id(Context))
    end.
