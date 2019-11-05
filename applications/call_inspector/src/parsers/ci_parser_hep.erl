%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ci_parser_hep).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("call_inspector.hrl").

-record(state, {parser_id :: atom()
               ,socket :: gen_udp:socket()
               ,listen_ip :: kz_term:ne_binary()
               ,listen_port :: pos_integer()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link([ci_parsers_util:parser_args()]) -> kz_types:startlink_ret().
start_link([Arg]=Args) ->
    ServerName = ci_parsers_util:make_name(Arg),
    gen_server:start_link({'local', ServerName}, ?MODULE, Args, []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init({'parser_args', kz_term:ne_binary(), pos_integer()}) -> {'ok', state()}.
init({'parser_args', IP, Port} = Args) ->
    ParserId = ci_parsers_util:make_name(Args),
    _ = kz_log:put_callid(ParserId),
    {'ok', Socket} = gen_udp:open(Port, ['binary'
                                        ,{'active', 'true'}
                                        ]),
    State = #state{parser_id = ParserId
                  ,socket = Socket
                  ,listen_ip = IP
                  ,listen_port = Port
                  },
    {'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(atom(), any(), state()) -> kz_types:handle_call_ret().
handle_call(_Request, _From, State) ->
    lager:debug("unhandled handle_call executed ~p~p", [_Request, _From]),
    Reply = 'ok',
    {'reply', Reply, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    lager:debug("unhandled handle_cast ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'udp', _Socket, _IPTuple, _InPortNo, Packet}
           ,#state{parser_id=ParserId}=State
           ) ->
    {'ok', Hep} = hep:decode(Packet),
    make_and_store_chunk(ParserId, Hep),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the gen_server terminate
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{socket = Socket}) ->
    'ok' = gen_udp:close(Socket),
    lager:debug("call inspector hep parser terminated: ~p", [_Reason]).

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
-spec make_and_store_chunk(atom(), hep:t()) -> 'ok'.
make_and_store_chunk(ParserId, Hep) ->
    Data = binary:split(hep:payload(Hep), <<"\r\n">>, ['global', 'trim']),
    Chunk =
        ci_chunk:setters(ci_chunk:new()
                        ,[{fun ci_chunk:data/2, Data}
                         ,{fun ci_chunk:call_id/2, ci_parsers_util:call_id(Data)}
                         ,{fun ci_chunk:timestamp/2, ci_parsers_util:timestamp(hep:timestamp(Hep))}
                         ,{fun ci_chunk:parser/2, ParserId}
                         ,{fun ci_chunk:label/2, hd(Data)}
                         ,{fun ci_chunk:src_ip/2, ip(hep:src_ip(Hep))}
                         ,{fun ci_chunk:dst_ip/2, ip(hep:dst_ip(Hep))}
                         ,{fun ci_chunk:src_port/2, hep:src_port(Hep)}
                         ,{fun ci_chunk:dst_port/2, hep:dst_port(Hep)}
                         ,{fun ci_chunk:c_seq/2, ci_parsers_util:c_seq(Data)}
                         ]
                        ),
    lager:debug("parsed chunk ~s", [ci_chunk:call_id(Chunk)]),
    ci_datastore:store_chunk(Chunk).

-spec ip(inet:ip4_address() | inet:ip6_address()) -> kz_term:ne_binary().
ip({92,_,_,_}=IP) ->
    lager:debug("look we hit this terrible case again!"),
    ip(setelement(1, IP, 192));
ip(IP) ->
    kz_network_utils:iptuple_to_binary(IP).
