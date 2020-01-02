%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Karls Hackity Hack....
%%% We want to block during startup until we have a AMQP connection
%%% but due to the way `kz_amqp_mgr' is structured we can't block in
%%% init there.  So this module will bootstrap `kz_amqp_mgr'
%%% and block until a connection becomes available, after that it
%%% removes itself.
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_bootstrap).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kz_amqp_util.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state(), timeout()}.
init([]) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    add_zones(get_config()),
    lager:info("waiting for first amqp connection..."),
    kz_amqp_connections:wait_for_available(),
    timer:sleep(2 * ?MILLISECONDS_IN_SECOND),
    kz_amqp_util:targeted_exchange(),
    {'ok', #state{}, 100}.

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
handle_info('timeout', State) ->
    _ = kz_amqp_sup:stop_bootstrap(),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("amqp bootstrap terminating: ~p", [_Reason]).

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
-spec add_zones(kz_term:proplist()) -> 'ok'.
add_zones([]) -> 'ok';
add_zones([{ZoneName, Brokers}|Zones]) ->
    _ = add_brokers(Brokers, ZoneName),
    add_zones(Zones).

-spec add_brokers(kz_term:ne_binaries(), atom()) -> 'ok'.
add_brokers([], _) -> 'ok';
add_brokers([Broker|Brokers], ZoneName) ->
    _ = kz_amqp_connections:add(Broker, ZoneName),
    add_brokers(Brokers, ZoneName).

-spec get_config() -> kz_term:proplist().
get_config() ->
    get_from_zone(kz_config:zone()).

-spec get_zones() -> kz_term:proplist().
get_zones() ->
    case kz_config:get_section(<<"zone">>) of
        [] -> kz_config:get_section(<<"amqp">>);
        Zones -> Zones
    end.

-spec get_from_zone(atom()) -> kz_term:proplist().
get_from_zone(ZoneName) ->
    Zones = get_zones(),
    Props = dict:to_list(get_from_zone(ZoneName, Zones, dict:new())),

    case props:get_value('local', Props, []) of
        [] -> [{'local', kz_config:get(<<"amqp">>, <<"uri">>, [?DEFAULT_AMQP_URI])} | Props];
        _Else -> Props
    end.

-spec get_from_zone(atom(), kz_term:proplist(), dict:dict()) -> dict:dict().
get_from_zone(_, [], Dict) -> Dict;
get_from_zone(ZoneName, [{_, Zone}|Zones], Dict) ->
    case props:get_first_defined([<<"name">>, <<"zone">>], Zone) of
        'undefined' -> get_from_zone(ZoneName, Zones, Dict);
        ZoneName ->
            get_from_zone(ZoneName, Zones, import_zone('local', Zone, Dict));
        RemoteZoneName ->
            get_from_zone(ZoneName, Zones, import_zone(RemoteZoneName, Zone, Dict))
    end.

-spec import_zone(atom(), kz_term:proplist(), dict:dict()) -> dict:dict().
import_zone(_, [], Dict) -> Dict;
import_zone(ZoneName, [{<<"amqp_uri">>, URI} | Props], Dict) ->
    case dict:find(ZoneName, Dict) of
        'error' ->
            import_zone(ZoneName, Props, dict:store(ZoneName, [URI], Dict));
        _Found ->
            import_zone(ZoneName, Props, dict:append(ZoneName, URI, Dict))
    end;
import_zone(ZoneName, [{<<"uri">>, URI} | Props], Dict) ->
    case dict:find(ZoneName, Dict) of
        'error' ->
            import_zone(ZoneName, Props, dict:store(ZoneName, [URI], Dict));
        _ ->
            import_zone(ZoneName, Props, dict:append(ZoneName, URI, Dict))
    end;
import_zone(ZoneName, [_|Props], Dict) -> import_zone(ZoneName, Props, Dict).
