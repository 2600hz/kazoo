%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
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
         ,stop/0
         ,authenticate/1
         ,finish_request/1
         ,clean_expired/1
        ]).

%% cleanup proc
-export([start_link/0
         ,init/1
         ,cleanup_loop/1
        ]).

-include("../crossbar.hrl").

-define(CHILDSPEC, {?MODULE, {?MODULE, 'start_link', []}
                    ,'permanent', 5000, 'worker', [?MODULE]
                   }).

-define(LOOP_TIMEOUT, whapps_config:get_integer(?APP_NAME, <<"token_auth_expiry">>, ?SECONDS_IN_HOUR)).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    proc_lib:start_link(?MODULE, 'init', [self()]).

init() ->
    couch_mgr:db_create(?TOKEN_DB),
    crossbar_module_sup:start_child('cb_buckets_sup', 'supervisor'),

    _ = couch_mgr:revise_doc_from_file(?TOKEN_DB, 'crossbar', "views/token_auth.json"),

    _ = supervisor:start_child('crossbar_sup', ?CHILDSPEC),

    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    crossbar_bindings:bind(<<"*.finish_request.*.*">>, ?MODULE, 'finish_request').

stop() ->
    'ok' = supervisor:terminate_child('crossbar_sup', ?MODULE),
    'ok' = supervisor:terminate_child('crossbar_module_sup', 'cb_buckets_sup'),
    'ok' = supervisor:delete_child('crossbar_sup', ?MODULE).

init(Parent) ->
    proc_lib:init_ack(Parent, {'ok', self()}),
    put('callid', ?LOG_SYSTEM_ID),

    ?MODULE:cleanup_loop(?LOOP_TIMEOUT).

-spec finish_request(cb_context:context()) -> any().
finish_request(#cb_context{auth_doc = <<>>}) -> 'ok';
finish_request(#cb_context{auth_doc='undefined'}) -> 'ok';
finish_request(#cb_context{auth_doc=AuthDoc}=Context) ->
    cb_context:put_reqid(Context),
    couch_mgr:suppress_change_notice(),
    lager:debug("updating auth doc: ~s:~s", [wh_json:get_value(<<"_id">>, AuthDoc)
                                             ,wh_json:get_value(<<"_rev">>, AuthDoc)
                                            ]),
    couch_mgr:save_doc(?TOKEN_DB, AuthDoc),
    couch_mgr:enable_change_notice().

cleanup_loop(Expiry) ->
    Timeout = Expiry * 1000,
    lager:debug("waiting ~b s before cleaning", [Expiry]),
    receive
    after
        Timeout ->
            ?MODULE:clean_expired(Expiry),
            ?MODULE:cleanup_loop(?LOOP_TIMEOUT)
    end.

clean_expired(Expiry) ->
    CreatedBefore = wh_util:current_tstamp() - Expiry, % gregorian seconds - Expiry time
    ViewOpts = [{'startkey', 0}
                ,{'endkey', CreatedBefore}
                ,{'limit', 5000}
               ],

    case couch_mgr:get_results(?TOKEN_DB, <<"token_auth/listing_by_mtime">>, ViewOpts) of
        {'ok', []} -> lager:debug("no expired tokens found"), 'ok';
        {'ok', L} ->
            lager:debug("removing ~b expired tokens", [length(L)]),
            _ = couch_mgr:del_docs(?TOKEN_DB, prepare_tokens_for_deletion(L)),
            'ok';
        {'error', _E} ->
            lager:debug("failed to lookup expired tokens: ~p", [_E])
    end.

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
    case cb_buckets_ets:has_tokens(Context) of
        'true' -> check_auth_token(Context);
        'false' ->
            lager:warning("rate limiting threshold hit for ~s!", [cb_context:client_ip(Context)]),
            {'halt', cb_context:add_system_error('too_many_requests', Context)}
    end.

check_auth_token(#cb_context{auth_token = <<>>}) -> 'false';
check_auth_token(#cb_context{auth_token='undefined'}) -> 'false';
check_auth_token(#cb_context{auth_token=AuthToken}=Context) ->
    lager:debug("checking auth token: ~s", [AuthToken]),
    case couch_mgr:open_cache_doc(?TOKEN_DB, AuthToken) of
        {'ok', JObj} ->
            lager:debug("token auth is valid, authenticating"),
            {'true', cb_context:set_auth_doc(
                       cb_context:set_auth_account_id(Context
                                                      ,wh_json:get_ne_value(<<"account_id">>, JObj))
                       ,wh_json:set_value(<<"modified">>, wh_util:current_tstamp(), JObj)
                      )
            };
        {'error', R} ->
            lager:debug("failed to authenticate token auth, ~p", [R]),
            'false'
    end.
