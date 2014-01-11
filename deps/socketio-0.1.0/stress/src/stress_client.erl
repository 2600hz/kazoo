%% @author Kirill Trofimov <sinnus@gmail.com>
%% @copyright 2012 Kirill Trofimov
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(stress_client).
-author('Kirill Trofimov <sinnus@gmail.com>').
-behaviour(gen_fsm).

%% API
-export([start_link/1, timestamp_to_number/1]).

%% gen_fsm callbacks
-export([init/1,
         get_sid/2, ready/2, polling_result_ready/2, wait_polling_result/2,
         state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {url,
                transport_url,
                heartbeat_timeout,
                session_timeout,
                body = [],
                send_packets,
                connected = false,
                error,
                start_poll_ts,
                sid}).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Url) ->
    gen_fsm:start_link(?MODULE, [Url], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([Url]) ->
    stress_mgr:inc_init_count(),
    gen_fsm:send_event(self(), go),
    {ok, get_sid, #state{url = Url}}.

%%--------------------------------------------------------------------
get_sid(go, State = #state{url = Url}) ->
    case ibrowse:send_req(Url ++ "/1/", [], get, [], []) of
        {ok, "200", _Headers, Body} ->
            [Sid, H, S, _Transports] = string:tokens(Body, ":"),
            gen_fsm:send_event(self(), poll),
            {next_state, ready, State#state{transport_url = Url ++ "/1/xhr-polling/" ++ Sid,
                                            sid = Sid,
                                            heartbeat_timeout = H,
                                            session_timeout = S}};
        {error, Error} ->
            {stop, normal, State#state{error = {create_session_send_req, Error}}}
    end.

ready(poll, State = #state{transport_url = TransportUrl}) ->
    %% TODO Run request timeout == session timeout because we cannot detect async request termination
    StartPollTS = erlang:now(),
    case ibrowse:send_req(TransportUrl, [], get, [], [{stream_to, self()}]) of
        {ibrowse_req_id, _ReqId} ->
            {next_state, wait_polling_result, State#state{start_poll_ts = StartPollTS}};
        {error, Error} ->
            {stop, normal, State#state{error = {send_req, Error}}}
    end.

wait_polling_result(Event, State) ->
    {next_state, Event, State}.

polling_result_ready(go, State = #state{sid = Sid, body = "1::", connected = false, start_poll_ts = StartPollTS}) ->
    log_polling_req(Sid, StartPollTS, erlang:now()),
    send_test_packets(State#state{connected = true});

polling_result_ready(go, State = #state{sid = Sid, body = Body, send_packets = PrevPackets, connected = true, start_poll_ts = StartPollTS}) ->
    log_polling_req(Sid, StartPollTS, erlang:now()),
    Packets = socketio_data_protocol:decode(list_to_binary(Body)),
    case Packets of
        PrevPackets ->
            send_test_packets(State);
        _ ->
            {stop, normal, State#state{error = {wrong_result_packets, {PrevPackets, Packets}}}}
    end.
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
handle_info({ibrowse_async_headers, _ReqId, _Status, _Headers}, wait_polling_result, State) ->
    %% Check status
    {next_state, wait_polling_result, State};

handle_info({ibrowse_async_response, _ReqId, Body}, wait_polling_result, State) ->
    {next_state, wait_polling_result, State#state{body = Body}};

handle_info({ibrowse_async_response_end, _ReqId}, wait_polling_result, State) ->
    gen_fsm:send_event(self(), go),
    {next_state, polling_result_ready, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State = #state{sid = Sid, error = Error}) ->
    stress_mgr:inc_finish_count(),
    error_logger:info_msg("Terminate session ~p~n", [Sid]),
    case Error of
        undefined ->
            ok;
        _ ->
            stress_mgr:inc_fail_count(),
            error_logger:error_msg("Session ~p failed with error ~p~n", [Sid, Error]),
            ok
    end.

%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_test_packets() ->
    [
     {message, <<>>, <<>>, <<"PING1">>},
     {message, <<>>, <<>>, <<"PING2">>},
     {message, <<>>, <<>>, <<"PING3">>}
    ].

send_test_packets(State = #state{transport_url = TransportUrl}) ->
    SendPackets = get_test_packets(),
    PacketsBin = socketio_data_protocol:encode(SendPackets),

    Headers = [{"Content-Type", "text/plain; charset=utf-8"},
               {"Connection", "keep-alive"},
               {"Content-Length", byte_size(PacketsBin)}],

    case ibrowse:send_req(TransportUrl, Headers, post, PacketsBin, []) of
        {ok, "200", _Headers, _} ->
            gen_fsm:send_event(self(), poll),
            {next_state, ready, State#state{send_packets = SendPackets}};
        {error, Error} ->
            {stop, normal, State#state{error = {send_req, Error}}}
    end.

timestamp_to_number({MegaSecs, Secs, MicroSecs}) ->
	Secs1 = (MegaSecs * 1000000) + Secs,
	Epoch = Secs1 * 1000 + trunc(MicroSecs / 1000),
    Epoch.

log_polling_req(Sid, StartTS, EndTS) ->
    Secs = timestamp_to_number(EndTS) - timestamp_to_number(StartTS),
    error_logger:info_msg("Sid ~p ~p secs", [Sid, Secs / 10000]).
