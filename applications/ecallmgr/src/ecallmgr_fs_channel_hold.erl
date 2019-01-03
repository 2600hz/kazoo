%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Receives HOLD/UNHOLD CHANNEL event
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_channel_hold).
-behaviour(gen_server).

-export([start_link/1, start_link/2]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3

        ,process_event/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-record(state, {node = 'undefined' :: atom()
               ,options = [] :: kz_term:proplist()
               }).
-type state() :: #state{}.

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
    gen_server:start_link(?SERVER, [Node, Options], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | kz_term:proplist()]) -> {'ok', state()}.
init([Node, Options]) ->
    kz_util:put_callid(Node),
    gen_server:cast(self(), 'bind_to_record'),
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
handle_cast('bind_to_record', #state{node=Node}=State) ->
    gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"CHANNEL_HOLD">>)}),
    gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"CHANNEL_UNHOLD">>)}),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'event', [UUID | Props]}, #state{node=Node}=State) ->
    _ = kz_util:spawn(fun process_event/3, [UUID, Props, Node]),
    {'noreply', State};
handle_info(_Other, State) ->
    lager:debug("unhandled msg: ~p", [_Other]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{node=Node}) ->
    lager:info("channel hold listener for ~s terminating: ~p", [Node, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_event(kz_term:api_binary(), kz_term:proplist(), atom()) -> any().
process_event(UUID, Props, Node) ->
    kz_util:put_callid(UUID),
    EventName = props:get_value(<<"Event-Subclass">>, Props, props:get_value(<<"Event-Name">>, Props)),
    process_specific_event(EventName, UUID, Props, Node).

-spec process_specific_event(kz_term:ne_binary(), kz_term:api_binary(), kz_term:proplist(), atom()) -> any().
process_specific_event(<<"CHANNEL_HOLD">>, UUID, Props, _Node) ->
    ecallmgr_fs_channels:update(UUID, #channel.is_onhold, 'true'),
    ecallmgr_call_events:process_channel_event([UUID | Props]);
process_specific_event(<<"CHANNEL_UNHOLD">>, UUID, Props, _Node) ->
    ecallmgr_fs_channels:update(UUID, #channel.is_onhold, 'false'),
    ecallmgr_call_events:process_channel_event([UUID | Props]);
process_specific_event(_Event, _UUID, _Props, _Node) ->
    lager:debug("event ~s for callid ~s not handled in channel hold (~s)", [_Event, _UUID, _Node]).
