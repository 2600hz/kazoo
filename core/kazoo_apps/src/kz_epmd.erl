%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Monitors EPMD connection and restarts it when necessary
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_epmd).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-record(state, {tref :: kz_types:api_reference()
               ,has_check_failed = 'false' :: boolean()
               ,name :: string() %% node name of the running node, e.g. kazoo_apps
               ,host :: string() %% local hostname of the running node
               ,epmd_mod :: atom() %% in case we're using a custom epmd client module
               ,port :: inet_address:port() %% node's port to talk disterl
               }).
-type state() :: #state{}.

-define(CHECK_EPMD, 'check_epmd').

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec init(any()) -> {'ok', state()}.
init(_) ->
    {'match', [Name, Host]} = re:run(atom_to_list(node()), "([^@]+)@(.+)", [{'capture', 'all_but_first', 'list'}]),

    lager:info("starting EPMD monitor with name ~s on host ~s", [Name, Host]),
    {'ok', check_epmd(#state{name=Name
                            ,host=Host
                            ,epmd_mod=net_kernel:epmd_module()
                            })
    }.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Call, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Cast, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'timeout', TRef, ?CHECK_EPMD}
           ,#state{tref=TRef}=State
           ) ->
    {'noreply', check_epmd(State)};
handle_info(_Msg, State) ->
    lager:debug("unhandled ~p", [_Msg]),
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:info("terminating epmd checks: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

start_check_timer() ->
    CheckS = kapps_config:get_integer(<<"epmd">>, <<"check_every_s">>, 60),
    start_check_timer(CheckS).

start_check_timer(CheckS) ->
    erlang:start_timer(CheckS * ?MILLISECONDS_IN_SECOND, self(), ?CHECK_EPMD).

check_epmd(#state{name=Name
                 ,host=Host
                 ,epmd_mod=Mod
                 }=State
          ) ->
    check_epmd(State, Mod:port_please(Name, Host)).

check_epmd(#state{port=_Port}=State, 'noport') ->
    lager:error("no EPMD response, re-registering on port ~p", [_Port]),
    register_with_epmd(State);
check_epmd(#state{port=Port}=State, {'port', Port, _Vsn}) ->
    State#state{tref=start_check_timer()};
check_epmd(#state{port=_OldPort}=State, {'port', Port, _Vsn}) ->
    lager:info("setting our port to ~p (was ~p)", [Port, _OldPort]),
    register_with_epmd(State#state{port=Port}).

register_with_epmd(#state{name=Name
                         ,epmd_mod=Mod
                         ,port=Port
                         }=State) ->
    check_register_result(State, Mod:register_node(Name, Port)).

check_register_result(State, {'ok', _Creation}) ->
    lager:info("created EPMD registration: ~p", [_Creation]),
    State#state{tref=start_check_timer()};
check_register_result(State, {'error', 'already_registered'}) ->
    lager:info("already registered with EPMD"),
    State#state{tref=start_check_timer()};
check_register_result(State, {'error', 'econnrefused'}) ->
    lager:error("refused connection to EPMD"),
    State#state{tref=start_check_timer(1)};
check_register_result(State, _Term) ->
    lager:info("unexpected return from registering: ~p", [_Term]),
    State#state{tref=start_check_timer(1)}.
