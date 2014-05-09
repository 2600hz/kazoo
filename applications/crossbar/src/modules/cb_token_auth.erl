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

maybe_save_auth_doc(AuthDoc, OldAuthDoc) ->
    AuthModified = wh_json:get_integer_value(<<"pvt_modified">>, AuthDoc, 0),
    OldAuthModified = wh_json:get_integer_value(<<"pvt_modified">>, OldAuthDoc, 0),

    Timeout = ?LOOP_TIMEOUT div 2,

    try AuthModified > (OldAuthModified + Timeout) of
        'true' ->
            lager:debug("auth doc is past time to be saved, saving"),
            couch_mgr:ensure_saved(?TOKEN_DB, AuthDoc);
        'false' ->
            lager:debug("auth doc is too new, not saving")
    catch
        _E:_R ->
            lager:debug("comparison failed, saving: ~s: ~p", [_E, _R]),
            couch_mgr:ensure_saved(?TOKEN_DB, AuthDoc)
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
        {'ok', JObj} ->
            lager:debug("token auth is valid, authenticating"),
            {'true', cb_context:set_auth_doc(
                       cb_context:set_auth_account_id(Context
                                                      ,wh_json:get_ne_value(<<"account_id">>, JObj)
                                                     )
                       ,wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), JObj)
                      )
            };
        {'error', R} ->
            lager:debug("failed to authenticate token auth, ~p", [R]),
            'false'
    end.
