%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author OnNet (Kirill Sysoev github.com/onnet)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cccp_shared_listener).
-behaviour(gen_listener).

-export([start_link/0
        ,handle_camping_req/2
        ]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("cccp.hrl").

-define(SERVER, ?MODULE).

-define(BINDINGS, [{'self', []}
                  ,{'camping', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_camping_req'}
                     ,[{<<"camping">>, <<"request">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<?MODULE_STRING>>).
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
    gen_listener:start_link(?SERVER, [{'bindings', ?BINDINGS}
                                     ,{'responders', ?RESPONDERS}
                                     ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                     ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                     ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], []).

-spec handle_camping_req(kapi_camping:req(), kz_term:proplist()) -> 'ok'.
handle_camping_req(JObj, _Props) ->
    'true' = kapi_camping:req_v(JObj),
    CampingData = kz_json:get_json_value(<<"Camping-Request">>, JObj),
    _Started = cccp_callback_sup:new(CampingData),
    lager:info("started camping on ~p: ~p", [_Started, CampingData]).

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
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
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
%%
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
