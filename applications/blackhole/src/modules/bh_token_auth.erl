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
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(bh_token_auth).

-export([init/0
         ,authenticate/1
         ,finish_request/1
         ,clean_expired/0
        ]).

-include("../blackhole.hrl").

-define(LOOP_TIMEOUT, whapps_config:get_integer(?APP_NAME, <<"token_auth_expiry">>, ?SECONDS_IN_HOUR)).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?TOKEN_DB),

    _ = couch_mgr:revise_doc_from_file(?TOKEN_DB, 'crossbar', "views/token_auth.json"),

    blackhole_bindings:bind(crossbar_cleanup:binding_hour(), ?MODULE, 'clean_expired'),

    _ = blackhole_bindings:bind(<<"blackhole.authenticate">>, ?MODULE, 'authenticate'),
    blackhole_bindings:bind(<<"*.finish_request.*.*">>, ?MODULE, 'finish_request').

-spec finish_request(bh_context:context()) -> 'ok'.
-spec finish_request(bh_context:context(), api_object()) -> 'ok'.
finish_request(Context) ->
    finish_request(Context, bh_context:auth_doc(Context)).

finish_request(_Context, 'undefined') -> 'ok';
finish_request(Context, AuthDoc) ->
    bh_context:put_reqid(Context),
    couch_mgr:suppress_change_notice(),
    lager:debug("updating auth doc: ~s:~s", [wh_json:get_value(<<"_id">>, AuthDoc)
                                             ,wh_json:get_value(<<"_rev">>, AuthDoc)
                                            ]),
    couch_mgr:save_doc(?TOKEN_DB, AuthDoc),
    couch_mgr:enable_change_notice(),
    'ok'.

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
-spec authenticate(bh_context:context()) ->
                          'false' |
                          {'true' | 'halt', bh_context:context()}.
authenticate(Context) ->
    _ = bh_context:put_reqid(Context),
    case kz_buckets:consume_token(bucket_name(Context)) of
        'true' -> check_auth_token(Context, bh_context:auth_token(Context));
        'false' ->
            lager:warning("rate limiting threshold hit for ~s!", [bh_context:session_id(Context)]),
            {'halt', 'badness'}
    end.

-spec check_auth_token(bh_context:context(), api_binary()) ->
                              boolean() |
                              {'true', cb_context:context()}.
check_auth_token(Context, AuthToken) ->
    lager:debug("checking auth token: ~s", [AuthToken]),
    case couch_mgr:open_doc(?TOKEN_DB, AuthToken) of
        {'ok', JObj} ->
            lager:debug("token auth is valid, authenticating"),
            {'true', bh_context:set_auth_doc(
                       bh_context:set_auth_account_id(Context
                                                      ,wh_json:get_ne_value(<<"account_id">>, JObj))
                       ,wh_json:set_value(<<"modified">>, wh_util:current_tstamp(), JObj)
                      )
            };
        {'error', R} ->
            lager:debug("failed to authenticate token auth, ~p", [R]),
            'false'
    end.

-spec bucket_name(bh_context:context()) -> ne_binary().
bucket_name(Context) ->
    bh_context:session_id(Context).
