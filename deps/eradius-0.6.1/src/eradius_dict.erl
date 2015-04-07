%% @private
%% @doc Dictionary server
-module(eradius_dict).
-export([start_link/0, lookup/1, load_tables/1, load_tables/2]).
-export_type([attribute/0, attr_value/0, table_name/0, attribute_id/0, attribute_type/0,
              attribute_prim_type/0, attribute_encryption/0, vendor_id/0, value_id/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("eradius_dict.hrl").

-define(SERVER, ?MODULE).
-define(TABLENAME, ?MODULE).

-type table_name() :: atom() | string().
-type attribute_id() :: pos_integer() | {vendor_id(), pos_integer()}.
-type attribute_encryption() :: 'no' | 'scramble' | 'salt_crypt'.
-type attribute_type() :: attribute_prim_type() | {tagged, attribute_prim_type()}.
-type attribute_prim_type() :: 'string' | 'integer' | 'integer64' | 'ipaddr' | 'ipv6addr'
                             | 'ipv6prefix' | 'date' | 'abinary' | 'binary' | 'octets'.

-type value_id() :: {attribute_id(), pos_integer()}.
-type vendor_id() :: pos_integer().

-type attribute()  :: #attribute{} | attribute_id().
-type attr_value() :: term().

-record(state, {}).

%% ------------------------------------------------------------------------------------------
%% -- API
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec lookup(attribute_id() | value_id()) -> [#attribute{} | #value{} | #vendor{}].
lookup(Id) ->
    ets:lookup(?TABLENAME, Id).

-spec load_tables(list(table_name())) -> ok | {error, {consult, table_name()}}.
load_tables(Tables) when is_list(Tables) ->
    load_tables(code:priv_dir(eradius), Tables).

-spec load_tables(file:filename(), list(table_name())) -> ok | {error, {consult, table_name()}}.
load_tables(Dir, Tables) when is_list(Tables) ->
    gen_server:call(?SERVER, {load_tables, Dir, Tables}, infinity).

%% ------------------------------------------------------------------------------------------
%% -- gen_server callbacks
init([]) ->
    create_table(),
    {ok, InitialLoadTables} = application:get_env(tables),
    do_load_tables(code:priv_dir(eradius), InitialLoadTables),
    {ok, #state{}}.

create_table() ->
    ets:new(?TABLENAME, [named_table, {keypos, 2}, protected]).

handle_call({load_tables, Dir, Tables}, _From, State) ->
    {reply, do_load_tables(Dir, Tables), State}.

%% unused callbacks
handle_cast(_Msg, State)   -> {noreply, State}.
handle_info(_Info, State)  -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, _NewVsn, _State) -> {ok, state}.

%% ------------------------------------------------------------------------------------------
%% -- gen_server callbacks
mapfile(A) when is_atom(A) -> mapfile(atom_to_list(A));
mapfile(A) when is_list(A) -> A ++ ".map".

-spec do_load_tables(file:filename(), [table_name()]) -> ok | {error, {consult, file:filename()}}.
do_load_tables(Dir, Tables) ->
    try
        Defs = lists:flatmap(fun (Tab) ->
                                     TabFile = filename:join(Dir, mapfile(Tab)),
                                     case file:consult(TabFile) of
                                         {ok, Res}       -> Res;
                                         {error, _Error} -> throw({consult, TabFile})
                                     end
                             end, Tables),
        ets:insert(?TABLENAME, Defs),
        lager:info("Loaded RADIUS tables: ~p", [Tables])
    catch
        throw:{consult, FailedTable} ->
            lager:error("Failed to load RADIUS table: ~s (wanted: ~p)", [FailedTable, Tables]),
            {error, {consult, FailedTable}}
    end.