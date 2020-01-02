%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Listener for route requests that can be fulfilled by Callflows.
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_listener).
-behaviour(gen_listener).

-export([start_link/1]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/3
        ,terminate/2
        ,code_change/3
        ]).

-include("callflow.hrl").

-type state() :: map().

-define(SERVER, ?MODULE).

-define(RESPONDERS, []).
-define(BINDINGS, [{'route', [{'types', ?RESOURCE_TYPES_HANDLED}
                             ,{'restrict_to', ['account']}
                             ]
                   }
                  ,{'self', []}
                  ,{'dialplan', []}
                  ]).
-define(QUEUE_NAME(I), <<"callflow_route_", I/binary>>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_types:ne_binary()) -> kz_types:startlink_ret().
start_link(Instance) ->
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', ?BINDINGS}
                                     ,{'queue_name', ?QUEUE_NAME(Instance)}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

%%%=============================================================================
%%% gen_listener callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(map()) -> {'ok', state()}.
init(State) ->
    {'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Msg, _From, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'return', JObj, Returned}}, State) ->
    ServerId = kz_api:server_id(JObj),
    lager:debug("returned: ~p", [ServerId]),
    Pid = kapi:decode_pid(ServerId),
    lager:debug("returned: ~p", [Pid]),
    Pid ! {'amqp_return', JObj, Returned},
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'forward', Msg}, #{queue := Queue}=State) ->
    handle_msg(Msg, [{'queue', Queue}], State),
    {'noreply', State};

handle_info(_Info, State) ->
    lager:info("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling AMQP event objects
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, Props, State) ->
    Msg = kapi:delivery_message(JObj, Props),
    handle_msg(Msg, Props, State).

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_listener' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_listener' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), any()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:info("callflow listener ~p termination", [_Reason]).

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_msg(term(), kz_term:proplist(), state()) -> gen_listener:handle_event_return().
handle_msg({_, {'dialplan', 'route_req'}, JObj}, Props, _State) ->
    lager:debug("new route request"),
    %% TODO
    %% if the cf_exe started, monitor and add it to the state (maybe a map)
    %% that will get us the count
    %% that can be used to decide whch instance will provide a pid
    %% instead of the current randomness in cf_listener_sup
    _ = cf_exe_sup:new(JObj, [{'channel', kz_amqp_channel:consumer_channel()} | Props], fun cf_route_req:handle_req/2),
    'ignore';

handle_msg({_, {'callflow', 'resume'}, JObj}, Props, _State) ->
    %% TODO
    %% look above
    _ = cf_exe_sup:new(JObj, [{'channel', kz_amqp_channel:consumer_channel()} | Props], fun cf_route_resume:handle_req/2),
    'ignore';

handle_msg(_Msg, _Props, _State) ->
    lager:debug("unhandled message => ~p", [_Msg]),
    'ignore'.
