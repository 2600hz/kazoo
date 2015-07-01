%% @doc Main module of the eradius application.
-module(eradius).
-export([load_tables/0,
	 modules_ready/1, modules_ready/2,
	 statistics/1]).

-behaviour(application).
-export([start/2, stop/1, config_change/3]).

%% internal use

-include("eradius_lib.hrl").

%% @doc Load RADIUS dictionaries from a certain directory.
-spec load_tables() -> ok.
load_tables() ->
    eradius_dict:load_tables().

%% @equiv modules_ready(self(), Modules)
modules_ready(Modules) ->
    eradius_node_mon:modules_ready(self(), Modules).

%% @doc Announce request handler module availability.
%%    Applications need to call this function (usually from their application master)
%%    in order to make their modules (which should implement the {@link eradius_server} behaviour)
%%    available for processing. The modules will be revoked when the given Pid goes down.
modules_ready(Pid, Modules) ->
    eradius_node_mon:modules_ready(Pid, Modules).

%% @doc manipulate server statistics
%%    * reset: reset all counters to zero
%%    * pull:  read counters and reset to zero
%%    * read:  read counters
statistics(reset) ->
    eradius_counter_aggregator:reset();
statistics(pull) ->
    eradius_counter_aggregator:pull();
statistics(read) ->
    eradius_counter_aggregator:read().

ensure_ip(IP = {_,_,_,_}) -> IP;
ensure_ip(IP = {_,_,_,_,_,_,_,_}) -> IP;
ensure_ip(IPString) when is_list(IPString) ->
    case inet_parse:address(IPString) of
        {ok, Address}   -> Address;
        {error, einval} -> error(badarg, [IPString])
    end;
ensure_ip(IP) ->
    error(badarg, [IP]).


%% ----------------------------------------------------------------------------------------------------
%% -- application callbacks

%% @private
start(_StartType, _StartArgs) ->
    eradius_sup:start_link().

%% @private
stop(_State) ->
    ok.

%% @private
config_change(Added, Changed, _Removed) ->
    lists:foreach(fun do_config_change/1, Added),
    lists:foreach(fun do_config_change/1, Changed),
    eradius_client:reconfigure().

do_config_change({tables, NewTables}) ->
    eradius_dict:load_tables(NewTables);
do_config_change({servers, _}) ->
    eradius_server_mon:reconfigure();
do_config_change({_, _}) ->
    ok.
