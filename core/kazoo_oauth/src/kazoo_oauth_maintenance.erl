%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_oauth_maintenance).

-behaviour(gen_server).

%% API
-export([start_link/0
        ,stop/1
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-export([register_oauth_app/5]).
-export([register_common_providers/0]).

-export([refresh/0
        ,refresh/1
        ]).

-include("kazoo_oauth.hrl").

-define(SERVER, ?MODULE).
-record(state, {}).
-type state() :: #state{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec refresh() -> 'ok'.
refresh() ->
    refresh(?KZ_OAUTH_DB).

-spec refresh(kz_term:ne_binary()) -> 'ok'.
refresh(?KZ_OAUTH_DB) ->
    _ = kz_datamgr:db_create(?KZ_OAUTH_DB),
    register_common_providers(),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec register_oauth_app(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
register_oauth_app(AccountId, OAuthId, EMail, Secret, Provider) ->
    Doc = kz_json:from_list([{<<"_id">>, OAuthId}
                            ,{<<"pvt_account_id">>, AccountId}
                            ,{<<"pvt_secret">>,Secret}
                            ,{<<"pvt_email">>, EMail}
                            ,{<<"pvt_user_prefix">>, kz_binary:rand_hex(16)}
                            ,{<<"pvt_oauth_provider">>, Provider}
                            ,{<<"pvt_type">>, <<"app">>}
                            ]),
    case kz_datamgr:open_doc(?KZ_OAUTH_DB, OAuthId) of
        {'ok', _JObj} -> {'error', <<"already registered">>};
        {'error', _} -> kz_datamgr:save_doc(?KZ_OAUTH_DB, Doc)
    end.

-spec register_common_providers() -> 'ok'.
register_common_providers() ->
    {'ok', _} = kz_datamgr:revise_doc_from_file(?KZ_OAUTH_DB, 'kazoo_oauth', <<"google.json">>),
    'ok'.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link(?SERVER, [], []).

-spec stop(pid()) -> 'ok'.
stop(Srv) ->
    gen_server:cast(Srv, 'stop').

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', state()}.
init([]) ->
    kapps_maintenance:bind({'refresh', ?KZ_OAUTH_DB}, ?MODULE, 'refresh'),
    refresh(),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Any, _From, #state{}=State) ->
    {'reply', 'not_implemented', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('stop', State) ->
    lager:debug("asked to stop, going down"),
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret().
handle_info(_Info, _State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', 'not_implemented'}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{}) ->
    kapps_maintenance:unbind({'refresh', ?KZ_OAUTH_DB}, ?MODULE, 'refresh'),
    lager:debug("~s going down: ~p", [?MODULE, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
