
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
         ,authorize/1
        ]).

-include("../crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?KZ_TOKEN_DB),

    _ = couch_mgr:revise_doc_from_file(?KZ_TOKEN_DB, 'crossbar', "views/token_auth.json"),

    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize').

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
        'false' -> 
            %%---------------------------------------------------------
            %% We dont halt this request, let someone else authorize it
            %%---------------------------------------------------------
            'false';
        'true' -> 
            lager:info("deny access"),
            Cause = wh_json:from_list([{<<"cause">>, <<"access denied by token restrictions">>}]),
            {'halt', cb_context:add_system_error('forbidden', Cause, Context)}
    end.

-spec maybe_deny_access(cb_context:context()) -> boolean().
maybe_deny_access(Context) ->
    AuthDoc = cb_context:auth_doc(Context),
    case wh_json:get_ne_value(<<"restrictions">>, AuthDoc) of
        'undefined' ->
            lager:debug("no restrictions"),
            'false';
        Rs ->
            maybe_deny_access(Context, Rs)
    end.

-spec maybe_deny_access(cb_context:context(), wh_json:objects()) -> boolean().
maybe_deny_access(Context, Rs) ->
    MatchFuns = [fun match_endpoint/2
                ,fun match_account/2
                ,fun match_param/2
                ,fun match_verb/2
                ],
    Result = lists:foldl(fun(F, R) -> 
                             case wh_util:is_empty(R) of
                                 'false' -> F(Context, R);
                                 'true' -> 'undefined'
                             end
                     end, 
                     Rs,
                     MatchFuns
                    ),
    wh_util:is_empty(Result).

-spec match_endpoint(cb_context:context(), wh_json:object() | 'undefined') -> wh_json:objects() | 'undefined'.
match_endpoint(Context, Rs) -> 
    [{ReqEndpoint, _}|_] = cb_context:req_nouns(Context),
    case wh_json:get_value(ReqEndpoint, Rs) of
        'undefined' -> 
            case wh_json:get_value(<<"_">>, Rs) of
                'undefined' ->
                    lager:info("no match endpoint for: ~p", [ReqEndpoint]),
                    'undefined';
                R ->
                    lager:debug("match endpoint \"_\""),
                    R
            end;
        R -> 
            lager:debug("match endpoint ~p",[ReqEndpoint]),
            R
    end.

-spec match_account(cb_context:context(), wh_json:objects()) -> wh_json:objects() | 'undefined'.
match_account(Context, Rs) -> 
    AllowedAccounts = allowed_accounts(Context),
    filter_by_account(AllowedAccounts, Rs).

filter_by_account(Accounts, []) -> 
    lager:info("no match account for: ~p",[Accounts]),
    'undefined';
filter_by_account(AllowedAccounts, [R|Rs]) ->
    case wh_json:get_value(<<"allowed_accounts">>, R) of
        'undefined' -> 
            lager:debug("no \"allowed_accounts\" parameter in rule, match any account"),
            wh_json:get_value(<<"rules">>, R);
        RsAccounts -> 
            SetsAllowedAccounts = sets:from_list(AllowedAccounts),
            SetsRsAccounts = sets:from_list(RsAccounts),
            SetsMatch = sets:intersection(SetsAllowedAccounts, SetsRsAccounts),
            case sets:to_list(SetsMatch) of
                [] -> filter_by_account(AllowedAccounts, Rs);
                Match ->
                    lager:debug("match account: ~p",[Match]),
                    wh_json:get_value(<<"rules">>, R)
            end
    end.

-spec allowed_accounts(cb_context:context()) -> ne_binaries().
allowed_accounts(Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    AccountId = cb_context:account_id(Context),
    %% 
    %% #cb_context.account_id not always contain AccountId, try found it from req_nouns
    %%
%%    AccountId = case cb_context:account_id(Context) of
%%        'undefined' -> 
%%            Nouns = cb_context:req_nouns(Context),
%%            case props:get_value(?WH_ACCOUNTS_DB, Nouns) of
%%                'undefined' -> 'undefined';
%%                [] -> 'undefined';
%%                [A|_] -> A
%%            end;
%%        A -> A
%%    end,
    case AccountId =:= AuthAccountId of
        'true' -> [ <<"_">>, AuthAccountId, <<"{AUTH_ACCOUNT_ID}">> ];
        'false' ->
            case wh_util:is_in_account_hierarchy(AuthAccountId, AccountId) of
                'true' -> [ <<"_">>, AccountId, <<"{DESCENDANT_ACCOUNT_ID}">> ];
                'false' when AccountId =:= 'undefined' -> [ <<"_">> ];
                'false' when AuthAccountId =:= 'undefined' -> [ <<"_">> ];
                'false' -> [ <<"_">>, AccountId ]
            end
    end.

-spec match_param(cb_context:context(), wh_json:objects()) -> wh_json:object() | 'undefined'.
match_param(Context, R) -> 
    [{_, ReqParam}|_] = cb_context:req_nouns(Context),
    Rules = wh_json:get_keys(R),
    case match_rules(ReqParam, Rules) of
        'undefined' -> 
            lager:info("no match rule for: ~p",[ReqParam]),
                'undefined';
        MatchRule -> 
            lager:debug("match rule: ~p",[MatchRule]),
            wh_json:get_value(MatchRule, R)
    end.

match_rules(_ReqParam, []) -> 'undefined';
match_rules(ReqParam, [R|Rules]) ->
    Rule = binary:split(R, <<"/">>, [global, trim]),
    case kazoo_bindings:matches(Rule, ReqParam) of
        'true' -> R;
        'false' -> match_rules(ReqParam, Rules)
    end.

-spec match_verb(cb_context:context(), list()) -> boolean().
match_verb(Context, Verbs) ->
    Verb = cb_context:req_verb(Context),
    case lists:member(Verb, Verbs) orelse lists:member(<<"_">>, Verbs) of
        'true' ->
            lager:debug("match verb: ~p", [Verb]),
            [Verb];
        'false' ->
            lager:info("no match verb for: ~p", [Verb]),
            'undefined'
    end.
