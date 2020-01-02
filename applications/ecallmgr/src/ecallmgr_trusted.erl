%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc monitors usurp_control
%%%
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_trusted).
-behaviour(gen_listener).

%% API
-export([start_link/0]).

%% gen_listener callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-export([handle_query/2]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-type state() :: map().

-define(BINDINGS, [{'trusted', [{'restrict_to', ['query']}
                               ,'federate'
                               ]}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_query'}
                     ,[{<<"trusted">>, <<"query">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<"ecallmgr_trusted">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}, ?MODULE,
                            [{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ], []).

%%%=============================================================================
%%% gen_listener callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?SERVER),
    {'ok', #{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed
%%
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.


-spec handle_query(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_query(JObj, _Props) ->
    ServerId = kz_api:server_id(JObj),
    Reply = [{<<"Trusted">>, ecallmgr_fs_acls:edge()}
            ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    lager:debug("sending trust reply to ~s", [ServerId]),
    kapi_trusted:publish_reply(ServerId, Reply).
