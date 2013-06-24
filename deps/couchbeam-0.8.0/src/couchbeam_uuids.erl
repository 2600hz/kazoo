%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_uuids).
-behaviour(gen_server).

-export([start_link/0, get_uuids/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("couchbeam/include/couchbeam.hrl").

-record(state, {}).

%% @doc Get a list of uuids from the server
%% @spec get_uuids(server(), integer()) -> lists()
get_uuids(Server, Count) ->
    gen_server:call(?MODULE, {get_uuids, Server, Count}).


%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
%% @doc Starts the couchbeam process linked to the calling process. Usually
%% invoked by the supervisor couchbeam_sup
%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------
%% @private

init(_) ->
    process_flag(trap_exit, true),
    _TID = ets:new(couchbeam_uuids, [named_table, public, {keypos, 2}]),
    {ok, #state{}}.

handle_call({get_uuids, #server{host=Host, port=Port}=Server, Count},
        _From, State) ->
    {ok, Uuids} = do_get_uuids(Server, Count, [],
        ets:lookup(couchbeam_uuids, {Host, Port})),
    {reply, Uuids, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_get_uuids(_Server, Count, Acc, _) when length(Acc) >= Count ->
    {ok, Acc};
do_get_uuids(Server, Count, Acc, []) ->
    {ok, ServerUuids} = get_new_uuids(Server),
    do_get_uuids(Server, Count, Acc, [ServerUuids]);
do_get_uuids(Server, Count, Acc, [#server_uuids{uuids=Uuids}]) ->
    case Uuids of
        [] ->
            {ok, ServerUuids} = get_new_uuids(Server),
            do_get_uuids(Server, Count, Acc, [ServerUuids]);
        _ when length(Uuids) < Count ->
            {ok, ServerUuids} = get_new_uuids(Server, Uuids),
            do_get_uuids(Server, Count, Acc, [ServerUuids]);
        _ ->
            {Acc1, Uuids1} = do_get_uuids1(Acc, Uuids, Count),
            #server{host=Host, port=Port} = Server,
            ServerUuids = #server_uuids{host_port={Host,Port},
                uuids=Uuids1},
            ets:insert(couchbeam_uuids, ServerUuids),
            do_get_uuids(Server, Count, Acc1, [ServerUuids])
    end.



do_get_uuids1(Acc, Uuids, 0) ->
    {Acc, Uuids};
do_get_uuids1(Acc, [Uuid|Rest], Count) ->
    do_get_uuids1([Uuid|Acc], Rest, Count-1).


get_new_uuids(Server) ->
    get_new_uuids(Server, []).

get_new_uuids(Server=#server{host=Host, port=Port,
                             options=IbrowseOptions}, Acc) ->
    Count = integer_to_list(1000 - length(Acc)),
    Url = couchbeam:make_url(Server, "_uuids", [{"count", Count}]),
    case couchbeam_httpc:request(get, Url, ["200"], IbrowseOptions) of
        {ok, _Status, _Headers, Body} ->
            {[{<<"uuids">>, Uuids}]} = ejson:decode(Body),
            ServerUuids = #server_uuids{host_port={Host,
                        Port}, uuids=(Acc ++ Uuids)},
                ets:insert(couchbeam_uuids, ServerUuids),
            {ok, ServerUuids};
        Error ->
            Error
    end.



