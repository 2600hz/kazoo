%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
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
-module(cb_token_auth).

-export([init/0
         ,authenticate/1
         ,finish_request/1
         ,clean_expired/0
        ]).

-include("../crossbar.hrl").

-define(LOOP_TIMEOUT, whapps_config:get_integer(?APP_NAME, <<"token_auth_expiry">>, ?SECONDS_IN_HOUR)).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?TOKEN_DB),

    _ = couch_mgr:revise_doc_from_file(?TOKEN_DB, 'crossbar', "views/token_auth.json"),

    crossbar_bindings:bind(crossbar_cleanup:binding_hour(), ?MODULE, 'clean_expired'),

    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    crossbar_bindings:bind(<<"*.finish_request.*.*">>, ?MODULE, 'finish_request').

-spec finish_request(cb_context:context()) -> 'ok'.
-spec finish_request(cb_context:context(), api_object()) -> 'ok'.
finish_request(Context) ->
    finish_request(Context, cb_context:auth_doc(Context)).

finish_request(_Context, 'undefined') -> 'ok';
finish_request(Context, AuthDoc) ->
    cb_context:put_reqid(Context),
    couch_mgr:suppress_change_notice(),
    lager:debug("maybe updating auth doc: ~s:~s", [wh_json:get_value(<<"_id">>, AuthDoc)
                                                   ,wh_json:get_value(<<"_rev">>, AuthDoc)
                                                  ]),

    case couch_mgr:open_cache_doc(?TOKEN_DB, cb_context:auth_token(Context)) of
        {'ok', OldAuthDoc} ->
            lager:debug("found old doc, checking to see if we should save"),
            maybe_save_auth_doc(AuthDoc, OldAuthDoc);
        {'error', _E} ->
            lager:debug("failed to open auth doc, trying to save our version: ~p", [_E]),
            couch_mgr:ensure_saved(?TOKEN_DB, AuthDoc)
    end,
    couch_mgr:enable_change_notice(),
    'ok'.

-spec maybe_save_auth_doc(wh_json:object(), wh_json:object()) -> any().
maybe_save_auth_doc(AuthDoc, OldAuthDoc) ->
    AuthModified = wh_json:get_integer_value(<<"pvt_modified">>, AuthDoc, 0),
    OldAuthModified = wh_json:get_integer_value(<<"pvt_modified">>, OldAuthDoc, 0),

    Timeout = ?LOOP_TIMEOUT div 2,

    case AuthModified - (OldAuthModified + Timeout) of
        N when N >= 0 ->
            lager:debug("auth doc is past time (~p after) to be saved, saving", [N]),
            couch_mgr:ensure_saved(?TOKEN_DB, AuthDoc);
        _N ->
            lager:debug("auth doc is too new (~p to go), not saving", [(_N*-1)])
    end.

-spec clean_expired() -> 'ok'.
clean_expired() ->
    CreatedBefore = wh_util:current_tstamp() - ?LOOP_TIMEOUT, % gregorian seconds - Expiry time
    ViewOpts = [{'startkey', 0}
                ,{'endkey', CreatedBefore}
                ,{'limit', 5000}
               ],

    case couch_mgr:get_results(?TOKEN_DB, <<"token_auth/listing_by_mtime">>, ViewOpts) of
        {'ok', []} -> lager:debug("no expired tokens found");
        {'ok', L} ->
            lager:debug("removing ~b expired tokens", [length(L)]),
            _ = couch_mgr:del_docs(?TOKEN_DB, prepare_tokens_for_deletion(L)),
            couch_compactor_fsm:compact_db(?TOKEN_DB),
            'ok';
        {'error', _E} ->
            lager:debug("failed to lookup expired tokens: ~p", [_E])
    end.

-spec prepare_token_for_deletion(wh_json:objects() | wh_json:object()) ->
                                        wh_json:objects() | wh_json:object().
prepare_tokens_for_deletion(L) ->
    [prepare_token_for_deletion(T) || T <- L].
prepare_token_for_deletion(Token) ->
    wh_json:from_list([{<<"_id">>, wh_json:get_value(<<"id">>, Token)}
                       ,{<<"_rev">>, wh_json:get_value(<<"value">>, Token)}
                      ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) ->
                          'false' |
                          {'true' | 'halt', cb_context:context()}.
authenticate(Context) ->
    _ = cb_context:put_reqid(Context),
    case kz_buckets:consume_token(cb_modules_util:bucket_name(Context)) of
        'true' -> check_auth_token(Context, cb_context:auth_token(Context), cb_context:magic_pathed(Context));
        'false' ->
            lager:warning("rate limiting threshold hit for ~s!", [cb_context:client_ip(Context)]),
            {'halt', cb_context:add_system_error('too_many_requests', Context)}
    end.

-spec check_auth_token(cb_context:context(), api_binary(), boolean()) ->
                              boolean() |
                              {'true', cb_context:context()}.
check_auth_token(_Context, <<>>, MagicPathed) -> MagicPathed;
check_auth_token(_Context, 'undefined', MagicPathed) -> MagicPathed;
check_auth_token(Context, AuthToken, _MagicPathed) ->
    lager:debug("checking auth token: ~s", [AuthToken]),
    case couch_mgr:open_cache_doc(?TOKEN_DB, AuthToken) of
        {'ok', JObj} -> check_restrictions(Context, JObj);
        {'error', R} ->
            lager:debug("failed to authenticate token auth, ~p", [R]),
            'false'
    end.

-spec check_restrictions(cb_context:context(), json:object()) -> boolean() |
                                                                 {'true', cb_context:context()}.
check_restrictions(Context, JObj) ->
    case wh_json:get_value(<<"restrictions">>, JObj, []) of
        [] ->
            lager:debug("no restrictions, check as object", []),
            check_as(Context, JObj);
        Rs ->
            check_restrictions(Context, JObj, Rs)
    end.

check_restrictions(Context, JObj, Rs) ->
    [_, _|PathTokens] = cb_context:path_tokens(Context),
    Restrictions = get_restrictions(Context, Rs),
    case crossbar_bindings:matches(Restrictions, PathTokens) of
        'false' ->
            lager:debug("failed to find any matches in restrictions"),
            'false';
        'true' ->
            lager:debug("found matche in restrictions, check as object now"),
            check_as(Context, JObj)
    end.

-spec get_restrictions(cb_context:context(), wh_json:object()) -> ne_binaries().
get_restrictions(Context, Restrictions) ->
    Verb = wh_util:to_lower_binary(cb_context:req_verb(Context)),
    DefaultRestrictions = wh_json:get_value(<<"*">>, Restrictions, []),
    VerbRestrictions = wh_json:get_value(Verb, Restrictions, []),
    lists:foldl(
        fun(R, Acc) ->
            case lists:member(R, Acc) of
                'true' -> Acc;
                'false' -> [R|Acc]
            end
        end
        ,[]
        ,VerbRestrictions ++ DefaultRestrictions
    ).

-spec check_as(cb_context:context(), wh_json:object()) -> boolean() |
                                                          {'true', cb_context:context()}.
check_as(Context, JObj) ->
    case wh_json:get_value(<<"account_id">>, JObj, 'undefined') of
        'undefined' -> {'true', set_auth_doc(Context, JObj)};
        AccountId -> check_as_payload(Context, JObj, AccountId)
    end.

-spec check_as_payload(cb_context:context(), wh_json:object(), ne_binary()) -> boolean() |
                                                                               {'true', cb_context:context()}.
check_as_payload(Context, JObj, AccountId) ->
    case {wh_json:get_value([<<"as">>, <<"account_id">>], JObj, 'undefined')
          ,wh_json:get_value([<<"as">>, <<"owner_id">>], JObj, 'undefined')}
    of
        {'undefined', _} -> {'true', set_auth_doc(Context, JObj)};
        {_, 'undefined'} -> {'true', set_auth_doc(Context, JObj)};
        {AsAccountId, AsOwnerId} -> check_descendants(Context, JObj, AccountId, AsAccountId, AsOwnerId)
    end.

-spec check_descendants(cb_context:context(), wh_json:object()
                        ,ne_binary() ,ne_binary() ,ne_binary()) -> boolean() |
                                                                   {'true', cb_context:context()}.
check_descendants(Context, JObj, AccountId, AsAccountId, AsOwnerId) ->
    case get_descendants(AccountId) of
        {'error', _} -> 'false';
        {'ok', Descendants} ->
            case lists:member(AsAccountId, Descendants) of
                'false' -> 'false';
                'true' ->
                    JObj1 = wh_json:set_values([{<<"account_id">>, AsAccountId}
                                                 ,{<<"owner_id">>, AsOwnerId}]
                                               ,JObj),
                    {'true', set_auth_doc(Context, JObj1)}
            end
    end.

-spec get_descendants(ne_binary()) -> {'error', any()} | {'ok', wh_json:objects()}.
get_descendants(AccountId) ->
    case couch_mgr:get_results(<<"accounts">>
                               ,<<"accounts/listing_by_descendants">>
                              ,[{'startkey', [AccountId]}
                                ,{'endkey', [AccountId, wh_json:new()]}])
    of
        {'error', _}=Error -> Error;
        {'ok', JObjs} ->
            Descendants = [wh_json:get_value(<<"id">>, JObj) || JObj <- JObjs],
            {'ok', Descendants}
    end.

-spec set_auth_doc(cb_context:context(), wh_json:object()) -> cb_context:context().
set_auth_doc(Context, JObj) ->
    cb_context:set_auth_doc(
        cb_context:set_auth_account_id(
            Context
            ,wh_json:get_ne_value(<<"account_id">>, JObj)
        )
        ,wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), JObj)
    ).
