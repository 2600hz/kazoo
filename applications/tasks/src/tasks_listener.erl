%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tasks_listener).
-behaviour(gen_listener).

-export([start_link/0]).

-export([handle_start_req/2
        ,handle_stop_req/2
        ,handle_remove_req/2
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("tasks.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

-define(BINDINGS, [{'self', []}
                  ,{'tasks', []}
                  ]).
-define(RESPONDERS, [{{'kz_tasks_help', 'handle_lookup_req'}
                     ,[{<<"tasks">>, <<"lookup_req">>}]
                     }
                    ,{{?MODULE, 'handle_start_req'}
                     ,[{<<"tasks">>, <<"start_req">>}]
                     }
                    ,{{?MODULE, 'handle_stop_req'}
                     ,[{<<"tasks">>, <<"stop_req">>}]
                     }
                    ,{{?MODULE, 'handle_remove_req'}
                     ,[{<<"tasks">>, <<"remove_req">>}]
                     }
                    ]).


%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[]
                           ).


%%%=============================================================================
%%% AMQP API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_start_req(kz_json:object(), kz_term:proplist()) -> ok.
handle_start_req(JObj, _Props) ->
    true = kapi_tasks:start_req_v(JObj),
    Help =
        case kz_tasks_scheduler:start(kapi_tasks:task_id(JObj)) of
            {ok, TaskJObj} -> TaskJObj;
            {error, already_started} -> <<"already_started">>;
            {error, not_found} -> <<"not_found">>
        end,
    Resp = kz_json:from_list(
             [{<<"Reply">>, Help}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    kapi_tasks:publish_start_resp(kz_api:server_id(JObj), Resp).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_stop_req(kz_json:object(), kz_term:proplist()) -> ok.
handle_stop_req(JObj, _Props) ->
    true = kapi_tasks:stop_req_v(JObj),
    Help =
        case kz_tasks_scheduler:stop(kapi_tasks:task_id(JObj)) of
            {ok, TaskJObj} -> TaskJObj;
            {error, not_running} -> <<"not_running">>;
            {error, not_found} -> <<"not_found">>
        end,
    Resp = kz_json:from_list(
             [{<<"Reply">>, Help}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapi_tasks:publish_stop_resp(kz_api:server_id(JObj), Resp).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_remove_req(kz_json:object(), kz_term:proplist()) -> ok.
handle_remove_req(JObj, _Props) ->
    true = kapi_tasks:remove_req_v(JObj),
    Help =
        case kz_tasks_scheduler:remove(kapi_tasks:task_id(JObj)) of
            {ok, TaskJObj} -> TaskJObj;
            {error, task_running} -> <<"task_running">>;
            {error, not_found} -> <<"not_found">>
        end,
    Resp = kz_json:from_list(
             [{<<"Reply">>, Help}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    kapi_tasks:publish_remove_resp(kz_api:server_id(JObj), Resp).


%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

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

%% End of Module.
