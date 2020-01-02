%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Registration viewer / creator
%%% GET /v2/accounts/{account_id}/registrations :
%%%   Get a list of account registrations
%%% GET /v2/accounts/{account_id}/registrations/count :
%%%   Get a count of account registrations
%%% GET /v2/registrations :
%%%   Get a count of system-wide registrations - for superduper admins only
%%%
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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
-include_lib("kazoo_sip/include/kzsip_uri.hrl").

-define(MASK_REG_FIELDS, [<<"Account-DB">>
                         ,<<"Account-ID">>
                         ,<<"App-Name">>
                         ,<<"App-Version">>
                         ,<<"Event-Category">>
                         ,<<"Event-Name">>
                         ,<<"Server-ID">>
                         ]).

-define(COUNT_PATH_TOKEN, <<"count">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.registrations">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.registrations">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authorize.registrations">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.registrations">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.registrations">>, ?MODULE, 'delete').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_DELETE].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?COUNT_PATH_TOKEN) ->
    [?HTTP_GET];
allowed_methods(_Username) ->
    [?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(?COUNT_PATH_TOKEN) -> 'true';
resource_exists(_Username) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context(), path_token()) -> boolean().

authorize(Context, ?COUNT_PATH_TOKEN) ->
    authorize_admin(Context, cb_context:req_nouns(Context));
authorize(_, _) -> 'false'.

-spec authorize_admin(cb_context:context(), req_nouns()) -> boolean().
authorize_admin(Context, [{<<"registrations">>, [?COUNT_PATH_TOKEN]}]) ->
    cb_context:is_superduper_admin(Context).

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_registrations(Context, cb_context:req_verb(Context)).

-spec validate_registrations(cb_context:context(), http_method()) -> cb_context:context().
validate_registrations(Context, ?HTTP_GET) ->
    crossbar_util:response(lookup_regs(Context), Context);
validate_registrations(Context, ?HTTP_DELETE) ->
    crossbar_util:response(<<"ok">>, Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?COUNT_PATH_TOKEN) ->
    validate_count(Context);
validate(Context, Username) ->
    validate_sip_username(Context, Username).

-spec validate_count(cb_context:context()) -> cb_context:context().
validate_count(Context) ->
    crossbar_util:response(kz_json:from_list([{<<"count">>, count_registrations(Context)}])
                          ,Context
                          ).

-spec validate_sip_username(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
validate_sip_username(Context, Username) ->
    case sip_username_exists(Context, Username) of
        'true' ->
            crossbar_util:response(<<"ok">>, Context);
        'false' ->
            crossbar_util:response_bad_identifier(Username, Context)
    end.

-spec sip_username_exists(cb_context:context(), kz_term:ne_binary()) -> boolean().
sip_username_exists(Context, Username) ->
    ViewOptions = [{'key', kz_term:to_lower_binary(Username)}],
    case kz_datamgr:get_results(cb_context:db_name(Context)
                               ,<<"devices/sip_credentials">>
                               ,ViewOptions
                               )
    of
        {'ok', [_]} -> 'true';
        _ -> 'false'
    end.

-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    crossbar_util:flush_registrations(Context),
    crossbar_util:response(<<"ok">>, Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Username) ->
    crossbar_util:flush_registration(Username, Context),
    crossbar_util:response(<<"ok">>, Context).

-spec lookup_regs(cb_context:context()) -> kz_json:objects().
lookup_regs(Context) ->
    Req = [{<<"Realm">>, get_realm(Context)}
          ,{<<"Fields">>, []}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    ReqResp = kz_amqp_worker:call_collect(Req
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
                            Contact ->
                                Username = kz_json:get_binary_value(<<"Username">>, J, <<>>),
                                dict:store(<<Username/binary, Contact/binary>>, J, R)
                        end
                end, Regs, kz_json:get_value(<<"Fields">>, JObj, [])).

-spec maybe_default_port(integer(), nklib:scheme(), kz_term:api_binary()) -> integer().
maybe_default_port(0, 'sips', _) -> 5061;
maybe_default_port(0, 'sip', <<"TLS">>) -> 5061;
maybe_default_port(0, 'sip', <<"tls">>) -> 5061;
maybe_default_port(0, 'sip', _) -> 5060;
maybe_default_port(Port, _, _) -> Port.

-spec normalize_registration(kz_json:object()) -> kz_json:object().
normalize_registration(JObj) ->
    OriginalContact = kz_json:get_binary_value(<<"Original-Contact">>, JObj, <<>>),
    [#uri{scheme=Scheme,domain=Domain, port=Port, ext_opts=Opts}=_Uri] = kzsip_uri:uris(OriginalContact),
    Transport = props:get_binary_value(<<"transport">>, Opts),
    Updaters = [fun(J) -> kz_json:delete_keys(?MASK_REG_FIELDS, J) end
               ,fun(J) ->
                        Values = [{<<"Contact-IP">>, Domain}
                                 ,{<<"Contact-Port">>, maybe_default_port(Port, Scheme, Transport)}
                                 ],
                        kz_json:set_values(Values, J)
                end
               ],
    kz_json:normalize(lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters)).

-spec count_registrations(cb_context:context()) -> integer().
count_registrations(Context) ->
    Req = [{<<"Realm">>, get_realm(Context)}
          ,{<<"Fields">>, []}
          ,{<<"Count-Only">>, 'true'}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    ReqResp = kz_amqp_worker:call(Req
                                 ,fun kapi_registration:publish_query_req/1
                                 ,fun kapi_registration:query_resp_v/1
                                 ),
    case ReqResp of
        {'error', _E} -> lager:debug("no resps found: ~p", [_E]), 0;
        {'ok', JObj} -> kz_json:get_integer_value(<<"Count">>, JObj, 0);
        {'timeout', _} -> lager:debug("timed out query for counting regs"), 0
    end.

-spec get_realm(cb_context:context()) -> kz_term:ne_binary().
get_realm(Context) ->
    case cb_context:account_id(Context) of
        'undefined' -> <<"all">>;
        AccountId -> kzd_accounts:fetch_realm(AccountId)
    end.
