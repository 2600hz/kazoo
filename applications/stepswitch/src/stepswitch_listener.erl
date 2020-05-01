%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_listener).
-behaviour(gen_listener).

-include("stepswitch.hrl").
-include_lib("kazoo_amqp/include/kapi_offnet_resource.hrl").

%% API
-export([start_link/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

-define(ROUTE_RESOURCE_TYPES_HANDLED
       ,[?RESOURCE_TYPE_AUDIO, ?RESOURCE_TYPE_SMS, ?RESOURCE_TYPE_VIDEO]
       ).
-define(OFFNET_RESOURCE_TYPES_HANDLED
       ,[?RESOURCE_TYPE_AUDIO, ?RESOURCE_TYPE_ORIGINATE, ?RESOURCE_TYPE_SMS, ?RESOURCE_TYPE_VIDEO]
       ).

-define(BINDINGS, [{'offnet_resource', [{'types', ?OFFNET_RESOURCE_TYPES_HANDLED}]}
                  ,{'authn', []}
                  ]).
-define(RESPONDERS, [{'stepswitch_outbound'
                     ,[{<<"resource">>, <<"offnet_req">>}]}
                    ,{'stepswitch_authn_req'
                     ,[{<<"directory">>, <<"authn_req">>}]}
                    ]).
-define(QUEUE_NAME, <<"stepswitch_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}, ?MODULE, [{'bindings', ?BINDINGS}
                                                         ,{'responders', ?RESPONDERS}
                                                         ,{'queue_name', ?QUEUE_NAME}
                                                         ,{'queue_options', ?QUEUE_OPTIONS}
                                                         ,{'consume_options', ?CONSUME_OPTIONS}
                                                         ], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    lager:debug("starting new stepswitch outbound responder"),
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
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{}) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("stepswitch listener terminating: ~p", [_Reason]).

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
