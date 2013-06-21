%% Copyright (c) 2012, Magnus Klaar <klaar@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


%% @doc Event filter implementation.
%%
%% An event query is constructed using the built in operators exported from
%% this module. The filtering operators are used to specify which events
%% should be included in the output of the query. The default output action
%% is to copy all events matching the input filters associated with a query
%% to the output. This makes it possible to construct and compose multiple
%% queries at runtime.
%%
%% === Examples of built in filters ===
%% ```
%% %% Select all events where 'a' exists and is greater than 0.
%% glc:gt(a, 0).
%% %% Select all events where 'a' exists and is equal to 0.
%% glc:eq(a, 0).
%% %% Select all events where 'a' exists and is less than 0.
%% glc:lt(a, 0).
%%
%% %% Select no input events. Used as black hole query.
%% glc:null(false).
%% %% Select all input events. Used as passthrough query.
%% glc:null(true).
%% '''
%%
%% === Examples of combining filters ===
%% ```
%% %% Select all events where both 'a' and 'b' exists and are greater than 0.
%% glc:all([glc:gt(a, 0), glc:gt(b, 0)]).
%% %% Select all events where 'a' or 'b' exists and are greater than 0.
%% glc:any([glc:get(a, 0), glc:gt(b, 0)]).
%% '''
%%
%% === Handling output events ===
%%
%% Once a query has been composed it is possible to override the output action
%% with an erlang function. The function will be applied to each output event
%% from the query. The return value from the function will be ignored.
%%
%% ```
%% %% Write all input events as info reports to the error logger.
%% glc:with(glc:null(true), fun(E) ->
%%     error_logger:info_report(gre:pairs(E)) end).
%% '''
%%
-module(glc).

-export([
    compile/2,
    handle/2,
    delete/1
]).

-export([
    lt/2,
    eq/2,
    gt/2
]).

-export([
    all/1,
    any/1,
    null/1,
    with/2
]).

-export([
    union/1
]).

-record(module, {
    'query' :: term(),
    tables :: [{atom(), ets:tid()}],
    qtree :: term()
}).

-spec lt(atom(), term()) -> glc_ops:op().
lt(Key, Term) ->
    glc_ops:lt(Key, Term).

-spec eq(atom(), term()) -> glc_ops:op().
eq(Key, Term) ->
    glc_ops:eq(Key, Term).

-spec gt(atom(), term()) -> glc_ops:op().
gt(Key, Term) ->
    glc_ops:gt(Key, Term).


%% @doc Filter the input using multiple filters.
%%
%% For an input to be considered valid output the all filters specified
%% in the list must hold for the input event. The list is expected to
%% be a non-empty list. If the list of filters is an empty list a `badarg'
%% error will be thrown.
-spec all([glc_ops:op()]) -> glc_ops:op().
all(Filters) ->
    glc_ops:all(Filters).


%% @doc Filter the input using one of multiple filters.
%%
%% For an input to be considered valid output on of the filters specified
%% in the list must hold for the input event. The list is expected to be
%% a non-empty list. If the list of filters is an empty list a `badarg'
%% error will be thrown.
-spec any([glc_ops:op()]) -> glc_ops:op().
any(Filters) ->
    glc_ops:any(Filters).


%% @doc Always return `true' or `false'.
-spec null(boolean()) -> glc_ops:op().
null(Result) ->
    glc_ops:null(Result).


%% @doc Apply a function to each output of a query.
%%
%% Updating the output action of a query finalizes it. Attempting
%% to use a finalized query to construct a new query will result
%% in a `badarg' error.
-spec with(glc_ops:op(), fun((gre:event()) -> term())) -> glc_ops:op().
with(Query, Action) ->
    glc_ops:with(Query, Action).


%% @doc Return a union of multiple queries.
%%
%% The union of multiple queries is the equivalent of executing multiple
%% queries separately on the same input event. The advantage is that filter
%% conditions that are common to all or some of the queries only need to
%% be tested once.
%%
%% All queries are expected to be valid and have an output action other
%% than the default which is `output'. If these expectations don't hold
%% a `badarg' error will be thrown.
-spec union([glc_ops:op()]) -> glc_ops:op().
union(Queries) ->
    glc_ops:union(Queries).


%% @doc Compile a query to a module.
%%
%% On success the module representing the query is returned. The module and
%% data associated with the query must be released using the {@link delete/1}
%% function. The name of the query module is expected to be unique.
-spec compile(atom(), list()) -> {ok, atom()}.
compile(Module, Query) ->
    {ok, ModuleData} = module_data(Query),
    case glc_code:compile(Module, ModuleData) of
        {ok, Module} ->
            {ok, Module}
    end.

%% @doc Handle an event using a compiled query.
%%
%% The input event is expected to have been returned from {@link gre:make/2}.
-spec handle(atom(), gre:event()) -> ok.
handle(Module, Event) ->
    Module:handle(Event).

%% @doc Release a compiled query.
%%
%% This releases all resources allocated by a compiled query. The query name
%% is expected to be associated with an existing query module. Calling this
%% function will result in a runtime error.
-spec delete(atom()) -> ok.
delete(_Module) ->
    ok.


%% @private Map a query to a module data term.
-spec module_data(term()) -> {ok, #module{}}.
module_data(Query) ->
    %% terms in the query which are not valid arguments to the
    %% erl_syntax:abstract/1 functions are stored in ETS.
    %% the terms are only looked up once they are necessary to
    %% continue evaluation of the query.
    Params = ets:new(params, [set,protected]),
    %% query counters are stored in a shared ETS table. this should
    %% be an optional feature. enable by defaults to simplify tests.
    Counters = ets:new(counters, [set,public]),
    ets:insert(Counters, [{input,0}, {filter,0}, {output,0}]),
    %% the abstract_tables/1 function expects a list of name-tid pairs.
    %% tables are referred to by name in the generated code. the table/1
    %% function maps names to tids.
    Tables = [{params,Params}, {counters,Counters}],
    Query2 = glc_lib:reduce(Query),
    {ok, #module{'query'=Query, tables=Tables, qtree=Query2}}.


%% @todo Move comment.
%% @private Map a query to a simplified query tree term.
%%
%% The simplified query tree is used to combine multiple queries into one
%% query module. The goal of this is to reduce the filtering and dispatch
%% overhead when multiple concurrent queries are executed.
%%
%% A fixed selection condition may be used to specify a property that an event
%% must have in order to be considered part of the input stream for a query.
%%
%% For the sake of simplicity it is only possible to define selection
%% conditions using the fields present in the context and identifiers
%% of an event. The fields in the context are bound to the reserved
%% names:
%%
%% - '$n': node name
%% - '$a': application name
%% - '$p': process identifier
%% - '$t': timestamp
%% 
%%
%% If an event must be selected based on the runtime state of an event handler
%% this must be done in the body of the handler.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup_query(Module, Query) ->
    ?assertNot(erlang:module_loaded(Module)),
    ?assertEqual({ok, Module}, case (catch compile(Module, Query)) of
        {'EXIT',_}=Error -> ?debugFmt("~p", [Error]), Error; Else -> Else end),
    ?assert(erlang:function_exported(Module, table, 1)),
    ?assert(erlang:function_exported(Module, handle, 1)),
    {compiled, Module}.

nullquery_compiles_test() ->
    {compiled, Mod} = setup_query(testmod1, glc:null(false)),
    ?assertError(badarg, Mod:table(noexists)).

params_table_exists_test() ->
    {compiled, Mod} = setup_query(testmod2, glc:null(false)),
    ?assert(is_integer(Mod:table(params))),
    ?assertMatch([_|_], ets:info(Mod:table(params))).

nullquery_exists_test() ->
    {compiled, Mod} = setup_query(testmod3, glc:null(false)),
    ?assert(erlang:function_exported(Mod, info, 1)),
    ?assertError(badarg, Mod:info(invalid)),
    ?assertEqual({null, false}, Mod:info('query')).

init_counters_test() ->
    {compiled, Mod} = setup_query(testmod4, glc:null(false)),
    ?assertEqual(0, Mod:info(input)),
    ?assertEqual(0, Mod:info(filter)),
    ?assertEqual(0, Mod:info(output)).

filtered_event_test() ->
    %% If no selection condition is specified no inputs can match.
    {compiled, Mod} = setup_query(testmod5, glc:null(false)),
    glc:handle(Mod, gre:make([], [list])),
    ?assertEqual(1, Mod:info(input)),
    ?assertEqual(1, Mod:info(filter)),
    ?assertEqual(0, Mod:info(output)).

nomatch_event_test() ->
    %% If a selection condition but no body is specified the event
    %% is expected to count as filtered out if the condition does
    %% not hold.
    {compiled, Mod} = setup_query(testmod6, glc:eq('$n', 'noexists@nohost')),
    glc:handle(Mod, gre:make([{'$n', 'noexists2@nohost'}], [list])),
    ?assertEqual(1, Mod:info(input)),
    ?assertEqual(1, Mod:info(filter)),
    ?assertEqual(0, Mod:info(output)).

opfilter_eq_test() ->
    %% If a selection condition but no body is specified the event
    %% counts as input to the query, but not as filtered out.
    {compiled, Mod} = setup_query(testmod7, glc:eq('$n', 'noexists@nohost')),
    glc:handle(Mod, gre:make([{'$n', 'noexists@nohost'}], [list])),
    ?assertEqual(1, Mod:info(input)),
    ?assertEqual(0, Mod:info(filter)),
    ?assertEqual(1, Mod:info(output)),
    done.


opfilter_gt_test() ->
    {compiled, Mod} = setup_query(testmod8, glc:gt(a, 1)),
    glc:handle(Mod, gre:make([{'a', 2}], [list])),
    ?assertEqual(1, Mod:info(input)),
    ?assertEqual(0, Mod:info(filter)),
    glc:handle(Mod, gre:make([{'a', 0}], [list])),
    ?assertEqual(2, Mod:info(input)),
    ?assertEqual(1, Mod:info(filter)),
    ?assertEqual(1, Mod:info(output)),
    done.

opfilter_lt_test() ->
    {compiled, Mod} = setup_query(testmod9, glc:lt(a, 1)),
    glc:handle(Mod, gre:make([{'a', 0}], [list])),
    ?assertEqual(1, Mod:info(input)),
    ?assertEqual(0, Mod:info(filter)),
    ?assertEqual(1, Mod:info(output)),
    glc:handle(Mod, gre:make([{'a', 2}], [list])),
    ?assertEqual(2, Mod:info(input)),
    ?assertEqual(1, Mod:info(filter)),
    ?assertEqual(1, Mod:info(output)),
    done.

allholds_op_test() ->
    {compiled, Mod} = setup_query(testmod10,
        glc:all([glc:eq(a, 1), glc:eq(b, 2)])),
    glc:handle(Mod, gre:make([{'a', 1}], [list])),
    glc:handle(Mod, gre:make([{'a', 2}], [list])),
    ?assertEqual(2, Mod:info(input)),
    ?assertEqual(2, Mod:info(filter)),
    glc:handle(Mod, gre:make([{'b', 1}], [list])),
    glc:handle(Mod, gre:make([{'b', 2}], [list])),
    ?assertEqual(4, Mod:info(input)),
    ?assertEqual(4, Mod:info(filter)),
    glc:handle(Mod, gre:make([{'a', 1},{'b', 2}], [list])),
    ?assertEqual(5, Mod:info(input)),
    ?assertEqual(4, Mod:info(filter)),
    ?assertEqual(1, Mod:info(output)),
    done.

anyholds_op_test() ->
    {compiled, Mod} = setup_query(testmod11,
        glc:any([glc:eq(a, 1), glc:eq(b, 2)])),
    glc:handle(Mod, gre:make([{'a', 2}], [list])),
    glc:handle(Mod, gre:make([{'b', 1}], [list])),
    ?assertEqual(2, Mod:info(input)),
    ?assertEqual(2, Mod:info(filter)),
    glc:handle(Mod, gre:make([{'a', 1}], [list])),
    glc:handle(Mod, gre:make([{'b', 2}], [list])),
    ?assertEqual(4, Mod:info(input)),
    ?assertEqual(2, Mod:info(filter)),
    done.

with_function_test() ->
    Self = self(),
    {compiled, Mod} = setup_query(testmod12,
        glc:with(glc:eq(a, 1), fun(Event) -> Self ! gre:fetch(a, Event) end)),
    glc:handle(Mod, gre:make([{a,1}], [list])),
    ?assertEqual(1, Mod:info(output)),
    ?assertEqual(1, receive Msg -> Msg after 0 -> notcalled end),
    done.

union_error_test() ->
    ?assertError(badarg, glc:union([glc:eq(a, 1)])),
    done.

-endif.
