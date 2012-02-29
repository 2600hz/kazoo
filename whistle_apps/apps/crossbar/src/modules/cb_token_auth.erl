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

-export([init/0
         ,authenticate/1
        ]).

-include_lib("crossbar/include/crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?TOKEN_DB),
    crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, authenticate).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate/1 :: (#cb_context{}) -> boolean() | {boolean(), #cb_context{}}.
authenticate(#cb_context{auth_token = <<>>}) ->
    false;
authenticate(#cb_context{auth_token=AuthToken}=Context) ->
    _ = crossbar_util:put_reqid(Context),

    lager:debug("checking auth token: ~s", [AuthToken]),
    case crossbar_util:open_doc(?TOKEN_DB, AuthToken) of
        {ok, JObj} ->
            lager:debug("token auth is valid, authenticating"),
            {true, Context#cb_context{auth_account_id=wh_json:get_ne_value(<<"account_id">>, JObj)
                                      ,auth_doc=JObj
                                     }};
        {error, R} ->
            lager:debug("failed to authenticate token auth, ~p", [R]),
            false
    end.
