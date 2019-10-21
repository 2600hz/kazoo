%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_dataconnections).
-behaviour(gen_server).

-export([start_link/0]).
-export([add/1]).
-export([update/1]).
-export([wait_for_connection/0, wait_for_connection/1, wait_for_connection/2]).
-export([get_server/0, get_server/1
        ,test_conn/0, test_conn/1
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kz_data.hrl").

-define(SERVER, ?MODULE).

-export_type([data_connection/0, data_connections/0]).

-record(state, {cookie = 'change_me'}).
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

-spec update(data_connection()) -> 'ok'.
update(#data_connection{}=Connection) ->
    gen_server:cast(?SERVER, {'update_connection', Connection}).

-spec add(data_connection()) -> 'ok'.
add(#data_connection{tag='undefined'}=Connection) ->
    add(Connection#data_connection{tag = <<"local">>});
add(#data_connection{}=Connection) ->
    gen_server:cast(?SERVER, {'add_connection', Connection}).


-spec wait_for_connection() -> 'ok' | 'no_connection'.
wait_for_connection() ->
    wait_for_connection(<<"local">>).

-spec wait_for_connection(kz_term:ne_binary()) -> 'ok' | 'no_connection'.
wait_for_connection(Tag) ->
    wait_for_connection(Tag, 'infinity').

-spec wait_for_connection(kz_term:ne_binary(), timeout()) -> 'ok' | 'no_connection'.
wait_for_connection(_Tag, 0) -> 'no_connection';
wait_for_connection(Tag, Timeout) ->
    Start = kz_time:start_time(),
    try test_conn(Tag) of
        {'error', _E} ->
            timer:sleep(rand:uniform(?MILLISECONDS_IN_SECOND) + 100),
            wait_for_connection(Tag, kz_time:decr_timeout(Timeout, Start));
        {'ok', Info} ->
            lager:info("connected to ~s: ~s", [Tag, kz_json:encode(Info)])
    catch
        'error':{'badmatch','$end_of_table'} ->
            timer:sleep(rand:uniform(?MILLISECONDS_IN_SECOND) + 100),
            wait_for_connection(Tag, kz_time:decr_timeout(Timeout, Start))
    end.


-spec get_server() -> server().
get_server() ->
    get_server(<<"local">>).

-spec get_server(kz_term:ne_binary()) -> server().
get_server(Tag) ->
    MatchSpec = [{#data_connection{ready = 'true'
                                  ,app = '$1'
                                  ,server = '$2'
                                  ,tag = Tag
                                  ,_ = '_'
                                  }
                 ,[]
                 ,[{{'$1', '$2'}}]
                 }],
    try ets:select(?MODULE, MatchSpec, 1) of
        {[{App, Server}], _} -> {App, Server};
        _ -> 'undefined'
    catch
        _E:_R ->
            case ets:info(?MODULE) of
                'undefined' -> lager:info("ETS table is gone, probably shutting down");
                _ -> lager:error("error querying data connections ETS: ~s: ~p", [_E, _R])
            end
    end.

-spec test_conn() -> {'ok', kz_json:object()} |
                     {'error', any()}.
test_conn() -> test_conn(<<"local">>).

-spec test_conn(kz_term:ne_binary()) -> {'ok', kz_json:object()} |
                                        {'error', any()}.
test_conn(Tag) ->
    case get_server(Tag) of
        'undefined' -> {'error', 'server_not_available'};
        Server -> kzs_server:server_info(Server)
    end.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    process_flag('trap_exit', 'true'),
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    _ = ets:new(?MODULE, ['ordered_set'
                         ,{'read_concurrency', 'true'}
                         ,{'keypos', #data_connection.id}
                         ,'named_table'
                         ]),
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
handle_cast({'add_connection', #data_connection{}=Connection}, State) ->
    lager:info("adding connection"),
    maybe_start_new_connection(Connection),
    {'noreply', State};
handle_cast({'update_connection', #data_connection{}=Connection}, State) ->
    'true' = ets:insert(?MODULE, Connection),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:info("unhandled connection"),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

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
    lager:debug("couch connections terminating: ~p", [_Reason]).

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
-spec maybe_start_new_connection(data_connection()) -> any().
maybe_start_new_connection(Connection) ->
    _ = kz_dataconnection_sup:add(Connection),
    _ = ets:insert(?MODULE, Connection).
