%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% Token auth module
%%%
%%% This is a simple auth mechanism, once the user has aquired an
%%% auth token this module will allow access.  This module should be
%%% updated to be FAR more robust.
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_token_restrictions).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,validate/1
         ,post/1
         ,delete/1
         ,authorize/1

         ,config_cat/0
        ]).

-ifdef(TEST).
-export([maybe_deny_access/1]).

-include_lib("eunit/include/eunit.hrl").
-include("test/cb_token_restrictions_test.hrl").

-define(LOG_DEBUG(F), ?debugFmt(F ++ "\n", [])).
-define(LOG_DEBUG(F, A), ?debugFmt(F ++ "\n", A)).
-else.
-define(LOG_DEBUG(F), lager:debug(F)).
-define(LOG_DEBUG(F, A), lager:debug(F, A)).
-endif.

-include("crossbar.hrl").

-define(PVT_TYPE, <<"token_restrictions">>).
-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".token_restrictions">>).

config_cat() ->
    ?MOD_CONFIG_CAT.

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?KZ_TOKEN_DB),

    _ = couch_mgr:revise_doc_from_file(?KZ_TOKEN_DB, 'crossbar', "views/token_auth.json"),

    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.token_restrictions">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.token_restrictions">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.token_restrictions">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.token_restrictions">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.token_restrictions">>, ?MODULE, 'delete').

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate(Context, cb_context:req_verb(Context)).

validate(Context, ?HTTP_GET) ->
    load_restrictions(Context);
validate(Context, ?HTTP_POST) ->
    validate_request(Context);
validate(Context, ?HTTP_DELETE) ->
    load_restrictions(Context).

-spec load_restrictions(cb_context:context()) -> cb_context:context().
load_restrictions(Context) ->
    crossbar_doc:load(?CB_ACCOUNT_TOKEN_RESTRICTIONS, Context).

validate_request(Context) ->
    OnSuccess = fun(C) -> on_successful_validation(C) end,
    cb_context:validate_request_data(?PVT_TYPE, Context, OnSuccess).

-spec on_successful_validation(cb_context:context()) -> cb_context:context().
on_successful_validation(Context) ->
    Context1 = crossbar_doc:load_merge(?CB_ACCOUNT_TOKEN_RESTRICTIONS, Context),
    case cb_context:resp_status(Context1) of
        'success' -> Context1;
        _Status ->
            Setters = [fun add_doc_id/1
                       ,fun add_pvt_type/1
                      ],
            cb_context:setters(Context, Setters)
    end.

-spec add_pvt_type(cb_context:context()) -> cb_context:context().
add_pvt_type(Context) ->
    cb_context:set_doc(Context, wh_doc:set_type(cb_context:doc(Context), ?PVT_TYPE)).

-spec add_doc_id(cb_context:context()) -> cb_context:context().
add_doc_id(Context) ->
    cb_context:set_doc(Context, wh_doc:set_id(cb_context:doc(Context), ?CB_ACCOUNT_TOKEN_RESTRICTIONS)).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    crossbar_doc:delete(Context, 'permanent').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) ->
                          'false' |
                          {'true' | 'halt', cb_context:context()}.
authorize(Context) ->
    case maybe_deny_access(Context) of
        'false' -> 'false';
        'true' ->
            lager:info("denying access"),
            Cause = wh_json:from_list(
                      [{<<"cause">>, <<"access denied by token restrictions">>}]
                     ),
            {'halt', cb_context:add_system_error('forbidden', Cause, Context)}
    end.

-spec maybe_deny_access(cb_context:context()) -> boolean().
maybe_deny_access(Context) ->
    AuthDoc = cb_context:auth_doc(Context),
    case wh_json:get_json_value(<<"restrictions">>, AuthDoc) of
        'undefined' -> 'false';
        Restrictions ->
            maybe_deny_access(Context, Restrictions)
    end.

-spec maybe_deny_access(cb_context:context(), wh_json:object()) -> boolean().
maybe_deny_access(Context, Restrictions) ->
    MatchFuns = [fun match_endpoint/2
                 ,fun match_account/2
                 ,fun match_arguments/2
                 ,fun match_verb/2
                ],

    not lists:foldl(fun(F, RestrictionContext) ->
                            F(Context, RestrictionContext)
                    end
                    ,Restrictions
                    ,MatchFuns
                   ).

-spec match_endpoint(cb_context:context(), api_object()) ->
                            api_object().
match_endpoint(Context, Restrictions) ->
    [{ReqEndpoint, _}|_] = cb_context:req_nouns(Context),

    match_request_endpoint(Restrictions, ReqEndpoint).

-spec match_request_endpoint(api_object(), ne_binary()) ->
                                    api_object().
match_request_endpoint(Restrictions, ?CATCH_ALL = ReqEndpoint) ->
    wh_json:get_value(ReqEndpoint, Restrictions);
match_request_endpoint(Restrictions, ReqEndpoint) ->
    case wh_json:get_value(ReqEndpoint, Restrictions) of
        'undefined' -> match_request_endpoint(Restrictions, ?CATCH_ALL);
        EndpointRestrictions -> EndpointRestrictions
    end.

-spec match_account(cb_context:context(), api_objects()) -> api_object().
match_account(_Context, 'undefined') -> 'undefined';
match_account(_Context, []) -> 'undefined';
match_account(Context, EndpointRestrictions) ->
    AllowedAccounts = allowed_accounts(Context),
    find_endpoint_restrictions_by_account(AllowedAccounts, EndpointRestrictions).

-spec find_endpoint_restrictions_by_account(ne_binaries(), wh_json:objects()) ->
                                                   api_object().
find_endpoint_restrictions_by_account(_Accounts, []) ->
    'undefined';
find_endpoint_restrictions_by_account(AllowedAccounts
                                      ,[Restriction|Restrictions]
                                     ) ->
    case maybe_match_accounts(AllowedAccounts
                              ,wh_json:get_value(<<"allowed_accounts">>, Restriction)
                             )
    of
        'true' -> wh_json:get_value(<<"rules">>, Restriction);
        'false' ->
            find_endpoint_restrictions_by_account(AllowedAccounts, Restrictions)
    end.

-spec maybe_match_accounts(ne_binaries(), api_binaries()) -> boolean().
maybe_match_accounts(_AllowedAccounts, 'undefined') -> 'true';
maybe_match_accounts(_AllowedAccounts, [?CATCH_ALL]) -> 'true';
maybe_match_accounts(AllowedAccounts, RestrictionAccounts) ->
    SetsAllowedAccounts = sets:from_list(AllowedAccounts),
    SetsRsAccounts = sets:from_list(RestrictionAccounts),
    sets:size(
      sets:intersection(SetsAllowedAccounts, SetsRsAccounts)
     ) > 0.

-spec allowed_accounts(cb_context:context()) -> ne_binaries().
allowed_accounts(Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),

    case cb_context:account_id(Context) of
        AuthAccountId -> [?CATCH_ALL, AuthAccountId, <<"{AUTH_ACCOUNT_ID}">> ];
        AccountId ->
            allowed_accounts(AuthAccountId, AccountId)
    end.

-ifdef(TEST).
allowed_accounts(?AUTH_ACCOUNT_ID, ?ACCOUNT_ID = AccountId) ->
    [?CATCH_ALL, AccountId, <<"{DESCENDANT_ACCOUNT_ID}">>].
-else.
-spec allowed_accounts(api_binary(), api_binary()) -> ne_binaries().
allowed_accounts('undefined', _AccountId) -> [?CATCH_ALL];
allowed_accounts(_AuthAccountId, 'undefined') -> [?CATCH_ALL];
allowed_accounts(AuthAccountId, AccountId) ->
    case wh_util:is_in_account_hierarchy(AuthAccountId, AccountId) of
        'true' -> [?CATCH_ALL, AccountId, <<"{DESCENDANT_ACCOUNT_ID}">>];
        'false' -> [?CATCH_ALL, AccountId]
    end.
-endif.

-spec match_arguments(cb_context:context(), api_object()) ->
                             http_methods().
match_arguments(_Context, 'undefined') -> [];
match_arguments(Context, RulesJObj) ->
    [{_, ReqParams}|_] = cb_context:req_nouns(Context),
    RuleKeys = wh_json:get_keys(RulesJObj),
    match_argument_patterns(ReqParams, RulesJObj, RuleKeys).

-spec match_argument_patterns(req_nouns(), wh_json:object(), ne_binaries()) ->
                                     http_methods().
match_argument_patterns(_ReqParams, _RulesJObj, []) -> [];
match_argument_patterns(ReqParams, RulesJObj, RuleKeys) ->
    case match_rules(ReqParams, RuleKeys) of
        'undefined' -> [];
        MatchedRuleKey ->
            wh_json:get_value(MatchedRuleKey, RulesJObj, [])
    end.

-spec match_rules(ne_binaries(), ne_binaries()) -> api_binary().
match_rules(_ReqParams, []) -> 'undefined';
match_rules(ReqParams, [RuleKey|RuleKeys]) ->
    case does_rule_match(RuleKey, ReqParams) of
        'false' -> match_rules(ReqParams, RuleKeys);
        'true' -> RuleKey
    end.

-spec does_rule_match(ne_binary(), ne_binaries()) -> boolean().
does_rule_match(RuleKey, ReqParams) ->
    kazoo_bindings:matches(binary:split(RuleKey, <<"/">>, ['global', 'trim'])
                           ,ReqParams
                          ).

-spec match_verb(cb_context:context(), http_methods()) -> boolean().
match_verb(_Context, [?CATCH_ALL]) -> 'true';
match_verb(Context, RuleVerbs) ->
    lists:member(cb_context:req_verb(Context), RuleVerbs)
        orelse lists:member(?CATCH_ALL, RuleVerbs).
