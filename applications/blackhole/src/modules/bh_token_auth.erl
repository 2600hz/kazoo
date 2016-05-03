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
        ,authenticate/1,authenticate/2
        ]).

-include("blackhole.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    kz_datamgr:db_create(?KZ_TOKEN_DB),
    _ = kz_datamgr:revise_doc_from_file(?KZ_TOKEN_DB, 'crossbar', "views/token_auth.json"),
    _ = blackhole_bindings:bind(<<"blackhole.authenticate">>, ?MODULE, 'authenticate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(bh_context:context()) -> {'true', bh_context:context()} |
                                            'false' |
                                            {'halt', any()}.
authenticate(Context) ->
    lager:debug("trying to authenticate with token: ~s", [bh_context:auth_token(Context)]),
    case kz_buckets:consume_token(?APP_NAME, bucket_name(Context)) of
        'true' -> check_auth_token(Context, bh_context:auth_token(Context));
        'false' ->
            lager:warning("rate limiting threshold hit for ~s!", [bh_context:websocket_session_id(Context)]),
            {'halt', 'badness'}
    end.

authenticate(Context, Foo) ->
    lager:debug("wha wha? ~p (~p)", [Context, Foo]).

-spec check_auth_token(bh_context:context(), api(binary())) -> {'true', bh_context:context()} |
                                                              'false'.
check_auth_token(Context, AuthToken) ->
    lager:debug("checking auth token: ~s", [AuthToken]),
    case kz_datamgr:open_doc(?KZ_TOKEN_DB, AuthToken) of
        {'ok', JObj} ->
            lager:debug("token auth is valid, authenticating"),
            AccountId = kz_json:get_ne_value(<<"account_id">>, JObj),
            Context1 = bh_context:set_auth_account_id(Context, AccountId),
            {'true', Context1};
        {'error', R} ->
            lager:debug("failed to authenticate token auth, ~p", [R]),
            'false'
    end.

-spec bucket_name(bh_context:context()) -> ne_binary().
bucket_name(Context) ->
    bh_context:websocket_session_id(Context).
