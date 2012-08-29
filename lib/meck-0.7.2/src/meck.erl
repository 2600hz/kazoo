%%==============================================================================
%% Copyright 2011 Adam Lindberg & Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%% @author Adam Lindberg <eproxus@gmail.com>
%% @copyright 2011, Adam Lindberg & Erlang Solutions Ltd
%% @doc Module mocking library for Erlang.

-module(meck).
-behaviour(gen_server).

%% Interface exports
-export([new/1]).
-export([new/2]).
-export([expect/3]).
-export([expect/4]).
-export([sequence/4]).
-export([loop/4]).
-export([delete/3]).
-export([exception/2]).
-export([passthrough/1]).
-export([history/1]).
-export([history/2]).
-export([validate/1]).
-export([unload/0]).
-export([unload/1]).
-export([called/3]).
-export([called/4]).
-export([num_calls/3]).
-export([num_calls/4]).
-export([reset/1]).

%% Callback exports
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([exec/5]).

%% Types
%% @type meck_mfa() = {Mod::atom(), Func::atom(), Args::list(term())}.
%% Module, function and arguments that the mock module got called with.
-type meck_mfa() :: {Mod::atom(), Func::atom(), Args::[term()]}.

%% @type history() = [{pid(), meck_mfa(), Result::term()}
%%                     | {pid(), meck_mfa(), Class:: exit | error | throw,
%%                        Reason::term(), Stacktrace::list(mfa())}].
%% History is a list of either successful function calls with a returned
%% result or function calls that resulted in an exception with a type,
%% reason and a stack trace. Each tuple begins with the pid of the process
%% that made the call to the function.
-type history() :: [{pid(), meck_mfa(), Result::term()}
                    | {pid(), meck_mfa(), Class:: exit | error | throw,
                       Reason::term(), Stacktrace::[mfa()]}].

%% Records
-record(state, {mod :: atom(),
                expects :: dict(),
                valid = true :: boolean(),
                history = [] :: history(),
                original :: term(),
                was_sticky :: boolean()}).

%% Includes
-include("meck_abstract.hrl").

%%==============================================================================
%% Interface exports
%%==============================================================================

%% @spec new(Mod:: atom() | list(atom())) -> ok
%% @equiv new(Mod, [])
-spec new(Mod:: atom() | [atom()]) -> ok.
new(Mod) when is_atom(Mod) -> new(Mod, []);
new(Mod) when is_list(Mod) -> lists:foreach(fun new/1, Mod), ok.

%% @spec new(Mod:: atom() | list(atom()), Options::list(term())) -> ok
%% @doc Creates new mocked module(s).
%%
%% This replaces the current version (if any) of the modules in `Mod'
%% with an empty module.
%%
%% Since this library is intended to use from test code, this
%% function links a process for each mock to the calling process.
%%
%% The valid options are:
%% <dl>
%%   <dt>`passthrough'</dt><dd>Retains the original functions, if not
%%                             mocked by meck.</dd>
%%   <dt>`no_link'</dt>    <dd>Does not link the meck process to the caller
%%                             process (needed for using meck in rpc calls).
%%                         </dd>
%%   <dt>`unstick'</dt>    <dd>Unstick the module to be mocked (e.g. needed
%%                             for using meck with kernel and stdlib modules).
%%                         </dd>
%%   <dt>`no_passthrough_cover'</dt><dd>If cover is enabled on the module to be
%%                                      mocked then meck will continue to
%%                                      capture coverage on passthrough calls.
%%                                      This option allows you to disable that
%%                                      feature if it causes problems.
%%                                  </dd>
%% </dl>
-spec new(Mod:: atom() | [atom()], Options::[term()]) -> ok.
new(Mod, Options) when is_atom(Mod), is_list(Options) ->
    case start(Mod, Options) of
        {ok, _Pid} -> ok;
        {error, Reason} -> erlang:error(Reason, [Mod, Options])
    end;
new(Mod, Options) when is_list(Mod) ->
    lists:foreach(fun(M) -> new(M, Options) end, Mod),
    ok.

%% @spec expect(Mod:: atom() | list(atom()), Func::atom(), Expect::fun()) -> ok
%% @doc Add expectation for a function `Func' to the mocked modules `Mod'.
%%
%% An expectation is a fun that is executed whenever the function
%% `Func' is called.
%%
%% It affects the validation status of the mocked module(s). If an
%% expectation is called with the wrong number of arguments or invalid
%% arguments the mock module(s) is invalidated. It is also invalidated if
%% an unexpected exception occurs.
-spec expect(Mod:: atom() | [atom()], Func::atom(), Expect::fun()) -> ok.
expect(Mod, Func, Expect)
  when is_atom(Mod), is_atom(Func), is_function(Expect) ->
    call(Mod, {expect, Func, Expect});
expect(Mod, Func, Expect) when is_list(Mod) ->
    lists:foreach(fun(M) -> expect(M, Func, Expect) end, Mod),
    ok.

%% @spec expect(Mod:: atom() | list(atom()), Func::atom(),
%%              Arity::pos_integer(), Result::term()) -> ok
%% @doc Adds an expectation with the supplied arity and return value.
%%
%% This creates an expectation which takes `Arity' number of functions
%% and always returns `Result'.
%%
%% @see expect/3.
-spec expect(Mod:: atom() | [atom()], Func::atom(),
             Arity::pos_integer(), Result::term()) -> ok.
expect(Mod, Func, Arity, Result)
  when is_atom(Mod), is_atom(Func), is_integer(Arity), Arity >= 0 ->
    valid_expect(Mod, Func, Arity),
    call(Mod, {expect, Func, Arity, Result});
expect(Mod, Func, Arity, Result) when is_list(Mod) ->
    lists:foreach(fun(M) -> expect(M, Func, Arity, Result) end, Mod),
    ok.

%% @spec sequence(Mod:: atom() | list(atom()), Func::atom(),
%%                Arity::pos_integer(), Sequence::[term()]) -> ok
%% @doc Adds an expectation which returns a value from `Sequence'
%% until exhausted.
%%
%% This creates an expectation which takes `Arity' number of arguments
%% and returns one element from `Sequence' at a time. Thus, calls to
%% this expect will exhaust the list of return values in order until
%% the last value is reached. That value is then returned for all
%% subsequent calls.
-spec sequence(Mod:: atom() | [atom()], Func::atom(),
               Arity::pos_integer(), Sequence::[term()]) -> ok.
sequence(Mod, Func, Arity, Sequence)
  when is_atom(Mod), is_atom(Func), is_integer(Arity), Arity >= 0 ->
    call(Mod, {sequence, Func, Arity, Sequence});
sequence(Mod, Func, Arity, Sequence) when is_list(Mod) ->
    lists:foreach(fun(M) -> sequence(M, Func, Arity, Sequence) end, Mod),
    ok.

%% @spec loop(Mod:: atom() | list(atom()), Func::atom(),
%%            Arity::pos_integer(), Loop::[term()]) -> ok
%% @doc Adds an expectation which returns a value from `Loop'
%% infinitely.
%%
%% This creates an expectation which takes `Arity' number of arguments
%% and returns one element from `Loop' at a time. Thus, calls to this
%% expect will return one element at a time from the list and will
%% restart at the first element when the end is reached.
-spec loop(Mod:: atom() | [atom()], Func::atom(),
           Arity::pos_integer(), Loop::[term()]) -> ok.
loop(Mod, Func, Arity, Loop)
  when is_atom(Mod), is_atom(Func), is_integer(Arity), Arity >= 0 ->
    call(Mod, {loop, Func, Arity, Loop});
loop(Mod, Func, Arity, Loop) when is_list(Mod) ->
    lists:foreach(fun(M) -> loop(M, Func, Arity, Loop) end, Mod),
    ok.

%% @spec delete(Mod:: atom() | list(atom()), Func::atom(),
%%              Arity::pos_integer()) -> ok
%% @doc Deletes an expectation.
%%
%% Deletes the expectation for the function `Func' with the matching
%% arity `Arity'.
-spec delete(Mod:: atom() | [atom()], Func::atom(), Arity::pos_integer()) ->
    ok.
delete(Mod, Func, Arity)
  when is_atom(Mod), is_atom(Func), Arity >= 0 ->
    call(Mod, {delete, Func, Arity});
delete(Mod, Func, Arity) when is_list(Mod) ->
    lists:foreach(fun(M) -> delete(M, Func, Arity) end, Mod),
    ok.

%% @spec exception(Class:: throw | error | exit, Reason::term()) -> no_return()
%% @doc Throws an expected exception inside an expect fun.
%%
%% This exception will get thrown without invalidating the mocked
%% module. That is, the code using the mocked module is expected to
%% handle this exception.
%%
%% <em>Note: this code should only be used inside an expect fun.</em>
-spec exception(Class:: throw | error | exit, Reason::term()) -> no_return().
exception(Class, Reason) when Class == throw; Class == error; Class == exit ->
    throw(mock_exception_fun(Class, Reason)).

%% @spec passthrough(Args::list(term())) -> term()
%% @doc Calls the original function (if existing) inside an expectation fun.
%%
%% <em>Note: this code should only be used inside an expect fun.</em>
-spec passthrough(Args::[term()]) -> term().
passthrough(Args) when is_list(Args) ->
    {Mod, Func} = get('$meck_call'),
    erlang:apply(original_name(Mod), Func, Args).

%% @spec validate(Mod:: atom() | list(atom())) -> boolean()
%% @doc Validate the state of the mock module(s).
%%
%% The function returns `true' if the mocked module(s) has been used
%% according to its expectations. It returns `false' if a call has
%% failed in some way. Reasons for failure are wrong number of
%% arguments or non-existing function (undef), wrong arguments
%% (function clause) or unexpected exceptions.
%%
%% Use the {@link history/1} or {@link history/2} function to analyze errors.
-spec validate(Mod:: atom() | [atom()]) -> boolean().
validate(Mod) when is_atom(Mod) ->
    call(Mod, validate);
validate(Mod) when is_list(Mod) ->
    not lists:member(false, [validate(M) || M <- Mod]).

%% @spec history(Mod::atom()) -> history()
%% @doc Return the call history of the mocked module for all processes.
%%
%% @equiv history(Mod, '_')
-spec history(Mod::atom()) -> history().
history(Mod) when is_atom(Mod) -> call(Mod, history).

%% @spec history(Mod::atom(), Pid::pid()) -> history()
%% @doc Return the call history of the mocked module for the specified process.
%%
%% Returns a list of calls to the mocked module and their results for
%% the specified `Pid'.  Results can be either normal Erlang terms or
%% exceptions that occurred.
%%
%% @see history/1
%% @see called/3
%% @see called/4
%% @see num_calls/3
%% @see num_calls/4
-spec history(Mod::atom(), Pid:: pid() | '_') -> history().
history(Mod, Pid) when is_atom(Mod), is_pid(Pid) orelse Pid == '_' -> 
    match_history(match_mfa('_', Pid), call(Mod, history)).

%% @spec unload() -> list(atom())
%% @doc Unloads all mocked modules from memory.
%%
%% The function returns the list of mocked modules that were unloaded
%% in the process.
-spec unload() -> [atom()].
unload() -> lists:foldl(fun unload_if_mocked/2, [], registered()).

%% @spec unload(Mod:: atom() | list(atom())) -> ok
%% @doc Unload a mocked module or a list of mocked modules.
%%
%% This will purge and delete the module(s) from the Erlang virtual
%% machine. If the mocked module(s) replaced an existing module, this
%% module will still be in the Erlang load path and can be loaded
%% manually or when called.
-spec unload(Mods:: atom() | [atom()]) -> ok.
unload(Mod) when is_atom(Mod) -> call(Mod, stop), wait_for_exit(Mod);
unload(Mods) when is_list(Mods) -> lists:foreach(fun unload/1, Mods), ok.

%% @spec called(Mod:: atom(), Fun:: atom(), Args:: list(term())) -> boolean()
%% @doc Returns whether `Mod:Func' has been called with `Args'.
%%
%% @equiv called(Mod, Fun, Args, '_')
called(Mod, Fun, Args) ->
    has_call({Mod, Fun, Args}, meck:history(Mod)).

%% @spec called(Mod:: atom(), Fun:: atom(), Args:: list(term()),
%%              Pid::pid()) -> boolean()
%% @doc Returns whether `Pid' has called `Mod:Func' with `Args'.
%%
%% This will check the history for the module, `Mod', to determine
%% whether process `Pid' call the function, `Fun', with arguments, `Args'. If
%% so, this function returns true, otherwise false.
%%
%% Wildcards can be used, at any level in any term, by using the underscore
%% atom: ``'_' ''
%%
%% @see called/3
-spec called(Mod::atom(), Fun::atom(), Args::list(), Pid::pid()) -> boolean().
called(Mod, Fun, Args, Pid) ->
    has_call({Mod, Fun, Args}, meck:history(Mod, Pid)).

%% @spec num_calls(Mod:: atom(), Fun:: atom(), Args:: list(term()))
%% -> non_neg_integer()
%% @doc Returns the number of times `Mod:Func' has been called with `Args'.
%%
%% @equiv num_calls(Mod, Fun, Args, '_')
num_calls(Mod, Fun, Args) ->
    num_calls({Mod, Fun, Args}, meck:history(Mod)).

%% @spec num_calls(Mod:: atom(), Fun:: atom(), Args:: list(term()),
%%                 Pid::pid()) -> non_neg_integer()
%% @doc Returns the number of times process `Pid' has called `Mod:Func'
%%      with `Args'.
%%
%% This will check the history for the module, `Mod', to determine how
%% many times process `Pid' has called the function, `Fun', with
%% arguments, `Args' and returns the result.
%%
%% @see num_calls/3
-spec num_calls(Mod::atom(), Fun::atom(), Args::list(), Pid::pid()) ->
    non_neg_integer().
num_calls(Mod, Fun, Args, Pid) ->
    num_calls({Mod, Fun, Args}, meck:history(Mod, Pid)).


%% @doc Erases the call history for a mocked module or a list of mocked modules.
%%
%% This function will erase all calls made heretofore from the history of the
%% specified modules. It is intended to prevent cluttering of test results with
%% calls to mocked modules made during the test setup phase.
-spec reset(Mod::atom() | [atom()]) -> ok.
reset(Mod) when is_atom(Mod) ->
    call(Mod, reset);
reset(Mods) when is_list(Mods) ->
    lists:foreach(fun(Mod) -> reset(Mod) end, Mods).



%%==============================================================================
%% Callback functions
%%==============================================================================

%% @hidden
init([Mod, Options]) ->
    WasSticky = case proplists:is_defined(unstick, Options) of
                    true -> {module, Mod} = code:ensure_loaded(Mod),
                            unstick_original(Mod);
                    _    -> false
                end,
    NoPassCover = proplists:get_bool(no_passthrough_cover, Options),
    Original = backup_original(Mod, NoPassCover),
    process_flag(trap_exit, true),
    Expects = init_expects(Mod, Options),
    try
        _Bin = meck_mod:compile_and_load_forms(to_forms(Mod, Expects)),
        {ok, #state{mod = Mod, expects = Expects, original = Original,
                    was_sticky = WasSticky}}
    catch
        exit:{error_loading_module, Mod, sticky_directory} ->
            {stop, module_is_sticky}
    end.

%% @hidden
handle_call({get_expect, Func, Arity}, _From, S) ->
    {Expect, NewExpects} = get_expect(S#state.expects, Func, Arity),
    {reply, Expect, S#state{expects = NewExpects}};
handle_call({expect, Func, Expect}, _From, S) ->
    NewExpects = store_expect(S#state.mod, Func, Expect, S#state.expects),
    {reply, ok, S#state{expects = NewExpects}};
handle_call({expect, Func, Arity, Result}, _From, S) ->
    NewExpects = store_expect(S#state.mod, Func, {anon, Arity, Result},
                              S#state.expects),
    {reply, ok, S#state{expects = NewExpects}};
handle_call({sequence, Func, Arity, Sequence}, _From, S) ->
    NewExpects = store_expect(S#state.mod, Func, {sequence, Arity, Sequence},
                              S#state.expects),
    {reply, ok, S#state{expects = NewExpects}};
handle_call({loop, Func, Arity, Loop}, _From, S) ->
    NewExpects = store_expect(S#state.mod, Func, {loop, Arity, Loop, Loop},
                              S#state.expects),
    {reply, ok, S#state{expects = NewExpects}};
handle_call({delete, Func, Arity}, _From, S) ->
    NewExpects = delete_expect(S#state.mod, Func, Arity, S#state.expects),
    {reply, ok, S#state{expects = NewExpects}};
handle_call(history, _From, S) ->
    {reply, lists:reverse(S#state.history), S};
handle_call(reset, _From, S) ->
    {reply, ok, S#state{history = []}};
handle_call(invalidate, _From, S) ->
    {reply, ok, S#state{valid = false}};
handle_call(validate, _From, S) ->
    {reply, S#state.valid, S};
handle_call(stop, _From, S) ->
    {stop, normal, ok, S}.

%% @hidden
handle_cast({add_history, Item}, S) ->
    {noreply, S#state{history = [Item| S#state.history]}};
handle_cast(_Msg, S)  ->
    {noreply, S}.

%% @hidden
handle_info(_Info, S) -> {noreply, S}.

%% @hidden
terminate(_Reason, #state{mod = Mod, original = OriginalState,
                          was_sticky = WasSticky}) ->
    export_original_cover(Mod, OriginalState),
    cleanup(Mod),
    restore_original(Mod, OriginalState, WasSticky),
    ok.

%% @hidden
code_change(_OldVsn, S, _Extra) -> {ok, S}.

%% @hidden
exec(Pid, Mod, Func, Arity, Args) ->
    Expect = call(Mod, {get_expect, Func, Arity}),
    try
        put('$meck_call', {Mod, Func}),
        Result = call_expect(Mod, Func, Expect, Args),
        add_history(Pid, Mod, Func, Args, Result),
        Result
    catch
        throw:Fun when is_function(Fun) ->
            case is_mock_exception(Fun) of
                true  -> handle_mock_exception(Pid, Mod, Func, Fun, Args);
                false -> invalidate_and_raise(Pid, Mod, Func, Args, throw, Fun)
            end;
        Class:Reason ->
            invalidate_and_raise(Pid, Mod, Func, Args, Class, Reason)
    after
        erase('$meck_call')
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

%% --- Process functions -------------------------------------------------------

start(Mod, Options) ->
    case proplists:is_defined(no_link, Options) of
        true  -> start(start, Mod, Options);
        false -> start(start_link, Mod, Options)
    end.

start(Func, Mod, Options) ->
    gen_server:Func({local, proc_name(Mod)}, ?MODULE, [Mod, Options], []).

cast(Mod, Msg) -> gen_server(cast, Mod, Msg).
call(Mod, Msg) -> gen_server(call, Mod, Msg).

gen_server(Func, Mod, Msg) ->
    Name = proc_name(Mod),
    try gen_server:Func(Name, Msg)
    catch exit:_Reason -> erlang:error({not_mocked, Mod}) end.

proc_name(Name) -> list_to_atom(atom_to_list(Name) ++ "_meck").

original_name(Name) -> list_to_atom(atom_to_list(Name) ++ "_meck_original").

wait_for_exit(Mod) ->
    MonitorRef = erlang:monitor(process, proc_name(Mod)),
    receive {'DOWN', MonitorRef, _Type, _Object, _Info} -> ok end.

unload_if_mocked(P, L) when is_atom(P) ->
    unload_if_mocked(atom_to_list(P), L);
unload_if_mocked(P, L) when length(P) > 5 ->
    case lists:split(length(P) - 5, P) of
        {Name, "_meck"} ->
            Mocked = list_to_existing_atom(Name),
            try
                unload(Mocked)
            catch error:{not_mocked, Mocked} ->
                    ok
            end,
            [Mocked|L];
        _Else ->
            L
    end;
unload_if_mocked(_P, L) ->
    L.

%% --- Mock handling -----------------------------------------------------------

valid_expect(M, F, A) ->
    case expect_type(M, F, A) of
        autogenerated -> erlang:error({cannot_mock_autogenerated, {M, F, A}});
        builtin -> erlang:error({cannot_mock_builtin, {M, F, A}});
        normal -> ok
    end.

init_expects(Mod, Options) ->
    case proplists:get_value(passthrough, Options, false) andalso exists(Mod) of
        true -> dict:from_list([{FA, passthrough} || FA <- exports(Mod)]);
        _    -> dict:new()
    end.


get_expect(Expects, Func, Arity) ->
    case e_fetch(Expects, Func, Arity) of
        {sequence, Arity, [Result]} ->
            {{sequence, Arity, Result}, Expects};
        {sequence, Arity, [Result|Rest]} ->
            {{sequence, Arity, Result},
             e_store(Expects, Func, {sequence, Arity, Rest})};
        {loop, Arity, [Result], Loop} ->
            {{loop, Arity, Result},
             e_store(Expects, Func, {loop, Arity, Loop, Loop})};
        {loop, Arity, [Result|Rest], Loop} ->
            {{loop, Arity, Result},
             e_store(Expects, Func, {loop, Arity, Rest, Loop})};
        Other ->
            {Other, Expects}
    end.

store_expect(Mod, Func, Expect, Expects) ->
    change_expects(fun e_store/3, Mod, Func, Expect, Expects).

delete_expect(Mod, Func, Arity, Expects) ->
    change_expects(fun e_delete/3, Mod, Func, Arity, Expects).

change_expects(Op, Mod, Func, Value, Expects) ->
    NewExpects = Op(Expects, Func, Value),
    _Bin = meck_mod:compile_and_load_forms(to_forms(Mod, NewExpects)),
    NewExpects.

e_store(Expects, Func, Expect) ->
    dict:store({Func, arity(Expect)}, Expect, Expects).

e_fetch(Expects, Func, Arity) ->
    dict:fetch({Func, Arity}, Expects).

e_delete(Expects, Func, Arity) ->
    dict:erase({Func, Arity}, Expects).

%% --- Code generation ---------------------------------------------------------

func(Mod, {Func, Arity}, {anon, Arity, Result}) ->
   case contains_opaque(Result) of
       true ->
            func_exec(Mod, Func, Arity);
       false ->
           func_native(Mod, Func, Arity, Result)
   end;
func(Mod, {Func, Arity}, _Expect) ->
    func_exec(Mod, Func, Arity).

func_exec(Mod, Func, Arity) ->
    Args = args(Arity),
    ?function(Func, Arity,
              [?clause(Args,
                       [?call(?MODULE, exec,
                              [?call(erlang, self, []),
                               ?atom(Mod),
                               ?atom(Func),
                               ?integer(Arity),
                               list(Args)])])]).

func_native(Mod, Func, Arity, Result) ->
    Args = args(Arity),
    AbsResult = erl_parse:abstract(Result),
    ?function(
       Func, Arity,
       [?clause(
           Args,
           [?call(gen_server, cast,
                  [?atom(proc_name(Mod)),
                   ?tuple([?atom(add_history),
                           ?tuple([?call(erlang, self, []),
                                   ?tuple([?atom(Mod), ?atom(Func),
                                           list(Args)]),
                                   AbsResult])])]),
            AbsResult])]).

contains_opaque(Term) when is_pid(Term); is_port(Term); is_function(Term);
                           is_reference(Term) ->
    true;
contains_opaque(Term) when is_list(Term) ->
    lists:any(fun contains_opaque/1, Term);
contains_opaque(Term) when is_tuple(Term) ->
    lists:any(fun contains_opaque/1, tuple_to_list(Term));
contains_opaque(_Term) ->
    false.


to_forms(Mod, Expects) ->
    {Exports, Functions} = functions(Mod, Expects),
    [?attribute(module, Mod)] ++ attributes(Mod) ++ Exports ++ Functions.

attributes(Mod) ->
    try
        [?attribute(Key, Val) || {Key, Val} <-
            proplists:get_value(attributes, Mod:module_info(), []),
            Key =/= vsn]
    catch
        error:undef -> []
    end.

functions(Mod, Expects) ->
    dict:fold(fun(Export, Expect, {Exports, Functions}) ->
                      {[?attribute(export, [Export])|Exports],
                       [func(Mod, Export, Expect)|Functions]}
              end, {[], []}, Expects).

args(0)     -> [];
args(Arity) -> [?var(var_name(N)) || N <- lists:seq(1, Arity)].

list([])    -> {nil, ?LINE};
list([H|T]) -> {cons, ?LINE, H, list(T)}.

var_name(A) -> list_to_atom("A"++integer_to_list(A)).

arity({anon, Arity, _Result}) ->
    Arity;
arity({sequence, Arity, _Sequence}) ->
    Arity;
arity({loop, Arity, _Current, _Loop}) ->
    Arity;
arity(Fun) when is_function(Fun) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    Arity.

%% --- Execution utilities -----------------------------------------------------

is_local_function(Fun) ->
    {module, Module} = erlang:fun_info(Fun, module),
    ?MODULE == Module.

handle_mock_exception(Pid, Mod, Func, Fun, Args) ->
    case Fun() of
        {exception, Class, Reason} ->
            % exception created with the mock:exception function,
            % do not invalidate Mod
            raise(Pid, Mod, Func, Args, Class, Reason)
    end.

-spec invalidate_and_raise(_, _, _, _, _, _) -> no_return().
invalidate_and_raise(Pid, Mod, Func, Args, Class, Reason) ->
    call(Mod, invalidate),
    raise(Pid, Mod, Func, Args, Class, Reason).

raise(Pid, Mod, Func, Args, Class, Reason) ->
    Stacktrace = inject(Mod, Func, Args, erlang:get_stacktrace()),
    add_history(Pid, Mod, Func, Args, Class, Reason, Stacktrace),
    erlang:raise(Class, Reason, Stacktrace).

mock_exception_fun(Class, Reason) -> fun() -> {exception, Class, Reason} end.

call_expect(_Mod, _Func, {_Type, Arity, Return}, VarList)
  when Arity == length(VarList) ->
    Return;
call_expect(Mod, Func, passthrough, VarList) ->
    apply(original_name(Mod), Func, VarList);
call_expect(_Mod, _Func, Fun, VarList) when is_function(Fun) ->
    apply(Fun, VarList).

inject(_Mod, _Func, _Args, []) ->
    [];
inject(Mod, Func, Args, [{meck, exec, _Arity} = Meck|Stack]) ->
    [Meck, {Mod, Func, Args}|Stack];
inject(Mod, Func, Args, [{meck, exec, _Arity, _Location} = Meck|Stack]) ->
    [Meck, {Mod, Func, Args}|Stack];
inject(Mod, Func, Args, [H|Stack]) ->
    [H|inject(Mod, Func, Args, Stack)].

is_mock_exception(Fun) -> is_local_function(Fun).

%% --- Original module handling ------------------------------------------------

backup_original(Module, NoPassCover) ->
    Cover = get_cover_state(Module),
    try
        Forms = meck_mod:abstract_code(meck_mod:beam_file(Module)),
        NewName = original_name(Module),
        CompileOpts = meck_mod:compile_options(meck_mod:beam_file(Module)),
        Renamed = meck_mod:rename_module(Forms, NewName),
        Binary = meck_mod:compile_and_load_forms(Renamed, CompileOpts),

        %% At this point we care about `Binary' if and only if we want
        %% to recompile it to enable cover on the original module code
        %% so that we can still collect cover stats on functions that
        %% have not been mocked.  Below are the different values
        %% passed back along with `Cover'.
        %%
        %% `no_passthrough_cover' - there is no coverage on the
        %% original module OR passthrough coverage has been disabled
        %% via the `no_passthrough_cover' option
        %%
        %% `no_binary' - something went wrong while trying to compile
        %% the original module in `backup_original'
        %%
        %% Binary - a `binary()' of the compiled code for the original
        %% module that is being mocked, this needs to be passed around
        %% so that it can be passed to Cover later.  There is no way
        %% to use the code server to access this binary without first
        %% saving it to disk.  Instead, it's passed around as state.
        if (Cover == false) orelse NoPassCover ->
                Binary2 = no_passthrough_cover;
           true ->
                Binary2 = Binary,
                meck_cover:compile_beam(NewName, Binary2)
        end,
        {Cover, Binary2}
    catch
        throw:{object_code_not_found, _Module} ->
            {Cover, no_binary}; % TODO: What to do here?
        throw:no_abstract_code                 ->
            {Cover, no_binary} % TODO: What to do here?
    end.

restore_original(Mod, {false, _}, WasSticky) ->
    restick_original(Mod, WasSticky),
    ok;
restore_original(Mod, OriginalState={{File, Data, Options},_}, WasSticky) ->
    case filename:extension(File) of
        ".erl" ->
            {ok, Mod} = cover:compile_module(File, Options);
        ".beam" ->
            cover:compile_beam(File)
    end,
    restick_original(Mod, WasSticky),
    import_original_cover(Mod, OriginalState),
    ok = cover:import(Data),
    ok = file:delete(Data),
    ok.

%% @doc Import the cover data for `<name>_meck_original' but since it
%% was modified by `export_original_cover' it will count towards
%% `<name>'.
import_original_cover(Mod, {_,Bin}) when is_binary(Bin) ->
    OriginalData = atom_to_list(original_name(Mod)) ++ ".coverdata",
    ok = cover:import(OriginalData),
    ok = file:delete(OriginalData);
import_original_cover(_, _) ->
    ok.

%% @doc Export the cover data for `<name>_meck_original' and modify
%% the data so it can be imported under `<name>'.
export_original_cover(Mod, {_, Bin}) when is_binary(Bin) ->
    OriginalMod = original_name(Mod),
    File = atom_to_list(OriginalMod) ++ ".coverdata",
    ok = cover:export(File, OriginalMod),
    ok = meck_cover:rename_module(File, Mod);
export_original_cover(_, _) ->
    ok.


unstick_original(Module) -> unstick_original(Module, code:is_sticky(Module)).

unstick_original(Module, true) -> code:unstick_mod(Module);
unstick_original(_,_) -> false.

restick_original(Module, true) ->
    code:stick_mod(Module),
    {module, Module} = code:ensure_loaded(Module),
    ok;
restick_original(_,_) -> ok.

get_cover_state(Module) -> get_cover_state(Module, cover:is_compiled(Module)).

get_cover_state(Module, {file, File}) ->
    Data = atom_to_list(Module) ++ ".coverdata",
    ok = cover:export(Data, Module),
    CompileOptions =
        try
            meck_mod:compile_options(meck_mod:beam_file(Module))
        catch
            throw:{object_code_not_found, _Module} -> []
        end,
    {File, Data, CompileOptions};
get_cover_state(_Module, _IsCompiled) ->
    false.

exists(Module) ->
    code:which(Module) /= non_existing.

exports(M) ->
    [ FA ||  FA = {F, A}  <- M:module_info(exports),
             normal == expect_type(M, F, A)].

%% Functions we should not create expects for (auto-generated and BIFs)
expect_type(_, module_info, 0) -> autogenerated;
expect_type(_, module_info, 1) -> autogenerated;
expect_type(M, F, A) -> expect_type(erlang:is_builtin(M, F, A)).

expect_type(true)  -> builtin;
expect_type(false) -> normal.

cleanup(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(original_name(Mod)),
    code:delete(original_name(Mod)).

%% --- History utilities -------------------------------------------------------

add_history(Pid, Mod, Func, Args, Result) ->
    add_history(Mod, {Pid, {Mod, Func, Args}, Result}).
add_history(Pid, Mod, Func, Args, Class, Reason, Stacktrace) ->
    add_history(Mod, {Pid, {Mod, Func, Args}, Class, Reason, Stacktrace}).

add_history(Mod, Item) ->
    cast(Mod, {add_history, Item}).

has_call(MFA, History) ->
    [] =/= match_history(match_mfa(MFA), History).

num_calls(MFA, History) ->
    length(match_history(match_mfa(MFA), History)).

match_history(MatchSpec, History) ->
    MS = ets:match_spec_compile(MatchSpec),
    ets:match_spec_run(History, MS).

match_mfa(MFA) -> match_mfa(MFA, '_').

match_mfa(MFA, Pid) ->
    [{{Pid, MFA, '_'}, [], ['$_']},
     {{Pid, MFA, '_', '_', '_'}, [], ['$_']}].
