%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @private SCTP Transport.
-module(nksip_transport_sctp).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([get_listener/3, connect/2]).
-export([start_link/3, init/1, terminate/2, code_change/3, handle_call/3,   
         handle_cast/2, handle_info/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").

-define(IN_STREAMS, 10).
-define(OUT_STREAMS, 10).


%% ===================================================================
%% Private
%% ===================================================================

%% @private Starts a new listening server
-spec get_listener(nksip:app_id(), nksip:transport(), nksip:optslist()) ->
    term().

get_listener(AppId, Transp, Opts) ->
    #transport{listen_ip=Ip, listen_port=Port} = Transp,
    {
        {AppId, sctp, Ip, Port}, 
        {?MODULE, start_link, [AppId, Transp, Opts]},
        permanent, 
        5000, 
        worker, 
        [?MODULE]
    }.
    

%% @private Starts a new connection to a remote server
-spec connect(pid(), nksip:transport()) ->
    {ok, pid(), nksip_transport:transport()} | {error, term()}.

connect(Pid, Transp) ->
    #transport{remote_ip=Ip, remote_port=Port} = Transp,
    case catch gen_server:call(Pid, {connect, Ip, Port}, 30000) of
        {ok, Pid1, Transp1} -> 
            {ok, Pid1, Transp1};
        {error, Error} -> 
            {error, Error};
        {'EXIT', Error} ->
            {error, Error}
    end.



%% ===================================================================
%% gen_server
%% ===================================================================


%% @private
start_link(AppId, Transp, Opts) -> 
    gen_server:start_link(?MODULE, [AppId, Transp, Opts], []).


-record(state, {
    app_id :: nksip:app_id(),
    transport :: nksip_transport:transport(),
    socket :: port(),
    pending :: [{inet:ip_address(), inet:port_number(), from()}],
    timeout :: integer()
}).


%% @private 
-spec init(term()) ->
    gen_server_init(#state{}).

init([AppId, Transp, Opts]) ->
    #transport{listen_ip=Ip, listen_port=Port} = Transp,
    Autoclose = nksip_lib:get_value(sctp_timeout, Opts),
    Opts1 = [
        binary, {reuseaddr, true}, {ip, Ip}, {active, once},
        {sctp_initmsg, 
            #sctp_initmsg{num_ostreams=?OUT_STREAMS, max_instreams=?IN_STREAMS}},
        {sctp_autoclose, Autoclose},    
        {sctp_default_send_param, #sctp_sndrcvinfo{stream=0, flags=[unordered]}}
    ],
    case gen_sctp:open(Port, Opts1) of
        {ok, Socket}  ->
            process_flag(priority, high),
            {ok, Port1} = inet:port(Socket),
            Transp1 = Transp#transport{local_port=Port1, listen_port=Port1},
            ok = gen_sctp:listen(Socket, true),
            nksip_proc:put(nksip_transports, {AppId, Transp1}),
            nksip_proc:put({nksip_listen, AppId}, Transp1),
            State = #state{ 
                app_id = AppId, 
                transport = Transp1, 
                socket = Socket,
                pending = [],
                timeout = 2000*Autoclose
            },
            {ok, State};
        {error, Error} ->
            ?error(AppId, <<>>, "could not start SCTP transport on ~p:~p (~p)", 
                   [Ip, Port, Error]),
            {stop, Error}
    end.


%% @private
-spec handle_call(term(), from(), #state{}) ->
    gen_server_call(#state{}).

handle_call({connect, Ip, Port}, From, State) ->
    #state{socket=Socket, pending=Pending} = State,
    Self = self(),
    Fun = fun() ->
        case gen_sctp:connect_init(Socket, Ip, Port, []) of
            ok -> ok;
            {error, Error} -> gen_server:cast(Self, {connection_error, Error, From})
        end
    end,
    spawn_link(Fun),
    {noreply, State#state{pending=[{{Ip, Port}, From}|Pending]}};

handle_call(Msg, _From, State) ->
    lager:error("Module ~p received unexpected call: ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
    gen_server_cast(#state{}).

handle_cast({connection_error, Error, From}, #state{pending=Pending}=State) ->
    gen_server:reply(From, {error, Error}),
    Pending1 = lists:keydelete(From, 2, Pending),
    {noreply, State#state{pending=Pending1}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Msg, State) ->
    lager:error("Module ~p received unexpected cast: ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    gen_server_info(#state{}).

handle_info({sctp, Socket, Ip, Port, {Anc, SAC}}, State) ->
    #state{app_id=AppId, socket=Socket} = State,
    State1 = case SAC of
        #sctp_assoc_change{state=comm_up, assoc_id=AssocId} ->
            Reply = do_connect(Ip, Port, AssocId, State),
            #state{pending=Pending} = State,
            case lists:keytake({Ip, Port}, 1, Pending) of
                {value, {_, From}, Pending1} -> 
                    gen_server:reply(From, Reply),
                    State#state{pending=Pending1};
                false when element(1, Reply)==error -> 
                    ?notice(AppId, <<>>, "Error ~p on SCTP connection up", 
                            [element(2, Reply)]),
                    State;
                false ->
                    State
            end;
        #sctp_assoc_change{state=shutdown_comp, assoc_id=AssocId} ->
            case nksip_transport:get_connected(AppId, sctp, Ip, Port, <<>>) of
                [{#transport{sctp_id=AssocId}, Pid}|_] ->
                    nksip_connection:stop(Pid, normal);
                _ ->
                    ok
            end,
            State;
        #sctp_paddr_change{} ->
            % We don't support address change yet
            State;
        #sctp_shutdown_event{assoc_id=_AssocId} ->
            % Should be already processed
            State; 
        Data when is_binary(Data) ->
            [#sctp_sndrcvinfo{assoc_id=AssocId}] = Anc,
            case do_connect(Ip, Port, AssocId, State) of
                {ok, Pid, _Transp1} ->
                    nksip_connection:incoming(Pid, Data);
                {error, Error} ->
                    ?notice(AppId, <<>>, "Error ~p on SCTP connection up", [Error])
            end,
            State;
        Other ->
            ?notice(AppId, <<>>, "SCTP unknown data from ~p, ~p: ~p", [Ip, Port, Other]),
            State
    end,
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State1};

handle_info(Info, State) -> 
    lager:warning("Module ~p received unexpected info: ~p (~p)", [?MODULE, Info, State]),
    {noreply, State}.


%% @private
-spec code_change(term(), #state{}, term()) ->
    gen_server_code_change(#state{}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
-spec terminate(term(), #state{}) ->
    gen_server_terminate().

terminate(_Reason, #state{app_id=AppId, socket=Socket}) ->  
    ?debug(AppId, <<>>, "SCTP server process stopped", []),
    gen_sctp:close(Socket).



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_connect(Ip, Port, AssocId, State) ->
    #state{app_id=AppId} = State,
    case nksip_transport:get_connected(AppId, sctp, Ip, Port, <<>>) of
        [{Transp, Pid}|_] -> 
            {ok, Pid, Transp};
        [] -> 
            case nksip_connection:is_max(AppId) of
                false ->
                    #state{socket=Socket, transport=Transp, timeout=Timeout} = State,
                    Transp1 = Transp#transport{remote_ip=Ip, remote_port=Port, sctp_id=AssocId},
                    {ok, Pid} = nksip_connection:start_link(AppId, Transp1, Socket, Timeout),
                    {ok, Pid, Transp1};
                true ->
                    {error, max_connections}
            end
    end.
        

