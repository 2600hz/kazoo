%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
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

-export([init/0, stop/0
         ,authenticate/1
        ]).

%% cleanup proc
-export([start_link/0, init/1]).

-include_lib("crossbar/include/crossbar.hrl").

-define(CHILDSPEC, {?MODULE, {?MODULE, start_link, []}, permanent, 5000, worker, [?MODULE]}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

init() ->
    couch_mgr:db_create(?TOKEN_DB),
    couch_mgr:revise_doc_from_file(?TOKEN_DB, crossbar, "views/token_auth.json"),

    _ = supervisor:start_child(crossbar_sup, ?CHILDSPEC),

    crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, authenticate).

stop() ->
    ok = supervisor:terminate_child(crossbar_sup, ?MODULE),
    ok = supervisor:delete_child(crossbar_sup, ?MODULE).

init(Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    put(callid, ?LOG_SYSTEM_ID),
    Expiry = whapps_config:get_integer(?APP_NAME, <<"token_auth_expiry">>, ?SECONDS_IN_DAY),

    cleanup_loop(Expiry).

cleanup_loop(Expiry) ->
    Timeout = Expiry * 1000,
    lager:debug("waiting ~b s before cleaning", [Expiry]),
    receive
    after
        Timeout ->
            clean_expired(Expiry),
            clean_soft_deleted(),
            cleanup_loop(whapps_config:get_integer(?APP_NAME, <<"token_auth_expiry">>, ?SECONDS_IN_DAY))
    end.

clean_expired(Expiry) ->
    CreatedBefore = wh_util:current_tstamp() - Expiry, % gregorian seconds - Expiry time

    case couch_mgr:get_results(?TOKEN_DB, <<"token_auth/listing_by_ctime">>, [{startkey, 0}
                                                                              ,{endkey, CreatedBefore}
                                                                             ]) of
        {ok, []} -> lager:debug("no expired tokens found"), ok;
        {ok, L} ->
            lager:debug("removing ~b expired tokens", [length(L)]),
            _ = couch_mgr:del_docs(?TOKEN_DB, prepare_tokens_for_deletion(L)),
            ok;
        {error, _E} ->
            lager:debug("failed to lookup expired tokens: ~p", [_E])
    end.

prepare_tokens_for_deletion(L) ->
    [prepare_token_for_deletion(T) || T <- L].
prepare_token_for_deletion(Token) ->
    wh_json:from_list([{<<"_id">>, wh_json:get_value(<<"id">>, Token)}
                       ,{<<"_rev">>, wh_json:get_value(<<"value">>, Token)}
                      ]).

clean_soft_deleted() ->
    case couch_mgr:get_results(?TOKEN_DB, <<"token_auth/soft_deleted">>, []) of
        {ok, []} -> lager:debug("no soft-deleted tokens found"), ok;
        {ok, L} ->
            lager:debug("removing ~b soft-deleted tokens", [length(L)]),
            couch_mgr:del_docs(?TOKEN_DB, prepare_tokens_for_deletion(L)),
            ok;
        {error, _E} ->
            lager:debug("failed to lookup soft-deleted tokens: ~p", [_E])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate/1 :: (#cb_context{}) -> 'false' | {'true', #cb_context{}}.
authenticate(#cb_context{auth_token = <<>>}) ->
    false;
authenticate(#cb_context{auth_token=AuthToken}=Context) ->
    _ = crossbar_util:put_reqid(Context),

    lager:debug("checking auth token: ~s", [AuthToken]),
    case couch_mgr:open_cache_doc(?TOKEN_DB, AuthToken) of
        {ok, JObj} ->
            lager:debug("token auth is valid, authenticating"),
            {true, Context#cb_context{auth_account_id=wh_json:get_ne_value(<<"account_id">>, JObj)
                                      ,auth_doc=JObj
                                     }};
        {error, R} ->
            lager:debug("failed to authenticate token auth, ~p", [R]),
            false
    end.
