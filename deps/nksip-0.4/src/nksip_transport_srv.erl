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

%% @private Outbound transport controller
-module(nksip_transport_srv).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-include("nksip.hrl").

-export([connect/6]).
-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2]).


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Starts a new outbound connection.
-spec connect(nksip:app_id(), nksip:protocol(),
                       inet:ip_address(), inet:port_number(), binary(),
                       nksip_lib:optslist()) ->
    {ok, pid(), nksip_transport:transport()} | {error, term()}.

connect(AppId, udp, Ip, Port, Res, Opts) ->
    nksip_connection:connect(AppId, udp, Ip, Port, Res, Opts);

connect(AppId, Proto, Ip, Port, Res, Opts)
                    when Proto==tcp; Proto==tls; Proto==sctp; Proto==ws; Proto==wss ->
    ConnId = {AppId, Proto, Ip, Port, Res},
    gen_server:call(?MODULE, {new, ConnId, Opts}, infinity).


%% ===================================================================
%% gen_server
%% ===================================================================

-record(state, {
    pending :: dict()
}).


%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
        

%% @private 
-spec init(term()) ->
    gen_server_init(#state{}).

init([]) ->
    {ok, #state{pending=dict:new()}}.


%% @private
-spec handle_call(term(), from(), #state{}) ->
    gen_server_call(#state{}).

handle_call({new, ConnId, Opts}, From, #state{pending=Pending}=State) ->
    case dict:is_key(ConnId, Pending) of
        true ->
            Pending1 = dict:append(ConnId, From, Pending),
            {noreply, State#state{pending=Pending1}};
        false ->
            Pending1 = dict:store(ConnId, [From], Pending),
            Self = self(),
            spawn_link(fun() -> connect(ConnId, Opts, Self) end),
            {noreply, State#state{pending=Pending1}}
    end;


handle_call(Msg, _From, State) -> 
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
    gen_server_cast(#state{}).

handle_cast({conn, ConnId, Result}, #state{pending=Pending}=State) ->
    {ok, Values} = dict:find(ConnId, Pending),
    lists:foreach(fun(From) -> gen_server:reply(From, Result) end, Values),
    Pending1 = dict:erase(ConnId, Pending),
    {noreply, State#state{pending=Pending1}};

handle_cast(Msg, State) -> 
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    gen_server_info(#state{}).

handle_info(Info, State) -> 
    lager:warning("Module ~p received unexpected info: ~p", [?MODULE, Info]),
    {noreply, State}.


%% @private
-spec code_change(term(), #state{}, term()) ->
    gen_server_code_change(#state{}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
-spec terminate(term(), #state{}) ->
    gen_server_terminate().

terminate(_Reason, _State) ->  
    ok.



%% ===================================================================
%% Internal
%% ===================================================================


connect({AppId, Proto, Ip, Port, Res}=Key, Opts, Pid) ->
    Result = nksip_connection:connect(AppId, Proto, Ip, Port, Res, Opts),
    gen_server:cast(Pid, {conn, Key, Result}).










