%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_resource).
-behaviour(gen_listener).

-export([start_link/1, start_link/2]).
-export([handle_originate_req/2]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-record(state, {node :: atom()
               ,options :: kz_term:proplist()
               }).
-type state() :: #state{}.

-define(BINDINGS, [{'resource', [{'restrict_to', ['originate']}]}
                  ,{'self', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_originate_req'}
                     ,[{<<"resource">>, <<"originate_req">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<"ecallmgr_fs_resource">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------

-spec start_link(atom()) -> kz_types:startlink_ret().
start_link(Node) -> start_link(Node, []).

-spec start_link(atom(), kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Node, Options) ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ],
                            [Node, Options]
                           ).

-spec handle_originate_req(kz_json:object(), kz_term:proplist()) -> kz_types:sup_startchild_ret().
handle_originate_req(JObj, Props) ->
    _ = kz_util:put_callid(JObj),

    Node = props:get_value('node', Props),
    case kz_json:is_true(<<"Start-Control-Process">>, JObj, 'true') of
        'true' ->
            handle_originate_req_if_possible(JObj
                                            ,Node
                                            ,ecallmgr_call_sup:control_context()
                                            );
        'false' ->
            lager:info("originate request does not require an call control process"),
            ecallmgr_originate_sup:start_originate_proc(Node, JObj, #{})
    end.

handle_originate_req_if_possible(JObj, _Node, {'error', _E}) ->
    lager:warning("unable to handle originate request due to AMQP worker error ~p", [_E]),
    lager:info("republishing request for another ecallmgr"),
    kz_amqp_worker:cast(JObj, fun kapi_resource:publish_originate_req/1);
handle_originate_req_if_possible(JObj, Node, {'ok', Context}) ->
    lager:debug("received originate request for node ~s, starting originate process", [Node]),
    ecallmgr_call_sup:wait_for_exit(ecallmgr_originate_sup:start_originate_proc(Node, JObj, Context), Context).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | kz_term:proplist()]) -> {'ok', state()}.
init([Node, Options]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(Node),
    lager:info("starting new fs resource listener for ~s", [Node]),
    {'ok', #state{node=Node, options=Options}}.

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
handle_info({'update_options', NewOptions}, State) ->
    {'noreply', State#state{options=NewOptions}, 'hibernate'};
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {'stop', {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {'stop', Reason, State};
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{node=Node}) ->
    {'reply', [{'node', Node}]}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node=Node}) ->
    lager:info("resource listener for ~s terminating: ~p", [Node, _Reason]).

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
