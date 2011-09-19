%%% Copyright 2010-2011 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2010-2011 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Manolis Papadakis

%%% @doc This is the main PropEr module.
%%%
%%% == How to write properties ==
%%% The simplest properties that PropEr can test consist of a single boolean
%%% expression (or a statement block that returns a boolean), which is expected
%%% to evaluate to `true'. Thus, the test `true' always succeeds, while the test
%%% `false' always fails (the failure of a property may also be signified by
%%% throwing an exception, error or exit. More complex (and useful) properties
%%% can be written by wrapping such a boolean expression with one or more of the
%%% following wrappers:
%%%
%%% <dl>
%%% <dt>`?FORALL(<Xs>, <Xs_type>, <Prop>)'</dt>
%%% <dd>The `<Xs>' field can either be a single variable, a tuple of variables
%%%   or a list of variables. The `<Xs_type>' field must then be a single type,
%%%   a tuple of types of the same length as the tuple of variables or a list
%%%   of types of the same length as the list of variables, respectively.
%%%   Tuples and lists can be combined in any way, as long as `<Xs>' and
%%%   `<Xs_type>' are compatible. Both PropEr-provided types, as listed in the
%%%   {@link proper_types} module, and types declared in Erlang's built-in
%%%   typesystem (we will refer to such types in as <em>native types</em>) may
%%%   be used in the `<Xs_type>' field. The use of native types in `?FORALL's is
%%%   subject to some limitations, as described in the documentation for the
%%%   {@link proper_typeserver} module. All the variables inside `<Xs>' can
%%%   (and should) be present as free variables inside the wrapped property
%%%   `<Prop>'. When a `?FORALL' wrapper is encountered, a random instance of
%%%   `<Xs_type>' is produced and each variable in `<Xs>' is replaced inside
%%%   `<Prop>' by its corresponding instance.</dd>
%%% <dt>`?IMPLIES(<Precondition>, <Prop>)'</dt>
%%% <dd>This wrapper only makes sense when in the scope of at least one
%%%   `?FORALL'. The `<Precondition>' field must be a boolean expression or a
%%%   statement block that returns a boolean. If the precondition evaluates to
%%%   `false' for the variable instances produced in the enclosing `?FORALL'
%%%   wrappers, the test case is rejected (it doesn't count as a failing test
%%%   case), and PropEr starts over with a new random test case. Also, in
%%%   verbose mode, an `x' is printed on screen.</dd>
%%% <dt>`?WHENFAIL(<Action>, <Prop>)'</dt>
%%% <dd>The `<Action>' field should contain an expression or statement block
%%%   that produces some side-effect (e.g. prints something to the screen).
%%%   In case this test fails, `<Action>' will be executed. Note that the output
%%%   of such actions is not affected by the verbosity setting of the main
%%%   application.</dd>
%%% <dt>`?TRAPEXIT(<Prop>)'</dt>
%%% <dd>If the code inside `<Prop>' spawns and links to a process that dies
%%%   abnormally, PropEr will catch the exit signal and treat it as a test
%%%   failure, instead of crashing. `?TRAPEXIT' cannot contain any more
%%%   wrappers.</dd>
%%% <dt>`?TIMEOUT(<Time_limit>, <Prop>)'</dt>
%%% <dd>Signifies that `<Prop>' should be considered failing if it takes more
%%%   than `<Time_limit>' milliseconds to return. The purpose of this wrapper is
%%%   to test code that may hang if something goes wrong. `?TIMEOUT' cannot
%%%   contain any more wrappers.</dd>
%%% <dt>`conjunction(<SubProps>)'</dt>
%%% <dd>See the documentation for {@link conjunction/1}.</dd>
%%% <dt>`equals(<A>, <B>)'</dt>
%%% <dd>See the documentation for {@link equals/2}.</dd>
%%% </dl>
%%%
%%% There are also multiple wrappers that can be used to collect statistics on
%%% the distribution of test data:
%%%
%%% <ul>
%%% <li>{@link collect/2}</li>
%%% <li>{@link collect/3}</li>
%%% <li>{@link aggregate/2}</li>
%%% <li>{@link aggregate/3}</li>
%%% <li>{@link classify/3}</li>
%%% <li>{@link measure/3}</li>
%%% </ul>
%%%
%%% <span id="external-wrappers"></span>
%%% A property may also be wrapped with one or more of the following outer-level
%%% wrappers, which control the behaviour of the testing subsystem. If an
%%% outer-level wrapper appears more than once in a property, the innermost
%%% instance takes precedence.
%%%
%%% <ul>
%%% <li>{@link numtests/2}</li>
%%% <li>{@link fails/2}</li>
%%% <li>{@link on_output/2}</li>
%%% </ul>
%%%
%%% For some actual usage examples, see the code in the examples directory, or
%%% check out PropEr's site. The testing modules in the tests directory may also
%%% be of interest.
%%%
%%% == Program behaviour ==
%%% When running in verbose mode (this is the default), each sucessful test
%%% prints a '.' on screen. If a test fails, a '!' is printed, along with the
%%% failing test case (the instances of the types in every `?FORALL') and the
%%% cause of the failure, if it was not simply the falsification of the
%%% property.
%%% Then, unless the test was expected to fail, PropEr attempts to produce a
%%% minimal test case that fails the property in the same way. This process is
%%% called <em>shrinking</em>. During shrinking, a '.' is printed for each
%%% successful simplification of the failing test case. When PropEr reaches its
%%% shrinking limit or realizes that the instance cannot be shrunk further while
%%% still failing the test, it prints the minimal failing test case and failure
%%% reason and exits.
%%%
%%% The return value of PropEr can be one of the following:
%%%
%%% <ul>
%%% <li>`true': The property held for all valid produced inputs.</li>
%%% <li>`false': The property failed for some input.</li>
%%% <li>`{error, <Type_of_error>}': An error occured; see the {@section Errors}
%%%   section for more information.</li>
%%% </ul>
%%%
%%% To test all properties exported from a module (a property is a 0-arity
%%% function whose name begins with `prop_'), you can use {@link module/1} or
%%% {@link module/2}. This returns a list of all failing properties, represented
%%% by MFAs. Testing progress is also printed on screen (unless quiet mode is
%%% active). The provided options are passed on to each property, except for
%%% `long_result', which controls the return value format of the `module'
%%% function itself.
%%%
%%% == Counterexamples ==
%%% A counterexample for a property is represented as a list of terms; each such
%%% term corresponds to the type in a `?FORALL'. The instances are provided in
%%% the same order as the `?FORALL' wrappers in the property, i.e. the instance
%%% at the head of the list corresponds to the outermost `?FORALL' etc.
%%% Instances generated inside a failing sub-property of a conjunction are
%%% marked with the sub-property's tag.
%%%
%%% The last (simplest) counterexample produced by PropEr during a (failing) run
%%% can be retrieved after testing has finished, by running
%%% {@link counterexample/0}. When testing a whole module, run
%%% {@link counterexamples/0} to get a counterexample for each failing property,
%%% as a list of `{mfa(), '{@type counterexample()}`}' tuples. To enable this
%%% functionality, some information has to remain in the process dictionary
%%% even after PropEr has returned. If, for some reason, you want to completely
%%% clean up the process dictionary of PropEr-produced entries, run
%%% {@link clean_garbage/0}.
%%%
%%% Counterexamples can also be retrieved by running PropEr in long-result mode,
%%% where counterexamples are returned as part of the return value.
%%% Specifically, when testing a single property under long-result mode
%%% (activated by supplying the option `long_result', or by calling
%%% {@link counterexample/1} or {@link counterexample/2} instead of
%%% {@link quickcheck/1} and {@link quickcheck/2} respectively), PropEr will
%%% return a counterexample in case of failure (instead of simply returning
%%% `false'). When testing a whole module under long-result mode (activated by
%%% supplying the option `long_result' to {@link module/2}), PropEr will return
%%% a list of `{mfa(), '{@type counterexample()}`}' tuples, one for each failing
%%% property.
%%%
%%% You can re-check a specific counterexample against the property that it
%%% previously falsified by running {@link check/2} or {@link check/3}. This
%%% will return one of the following (both in short- and long-result mode):
%%%
%%% <ul>
%%% <li>`true': The property now holds for this test case.</li>
%%% <li>`false': The test case still fails (although not necessarily for the
%%%   same reason as before).</li>
%%% <li>`{error, <Type_of_error>}': An error occured - see the {@section Errors}
%%%   section for more information.</li>
%%% </ul>
%%%
%%% Proper will not attempt to shrink the input in case it still fails the
%%% property. Unless silent mode is active, PropEr will also print a message on
%%% screen, describing the result of the re-checking. Note that PropEr can do
%%% very little to verify that the counterexample actually corresponds to the
%%% property that it is tested against.
%%%
%%% == Options ==
%%% Options can be provided as an extra argument to most testing functions (such
%%% as {@link quickcheck/1}). A single option can be written stand-alone, or
%%% multiple options can be provided in a list. When two settings conflict, the
%%% one that comes first in the list takes precedence. Settings given inside
%%% external wrappers to a property (see the {@section How to write properties}
%%% section) override any conflicting settings provided as options.
%%%
%%% The available options are:
%%%
%%% <dl>
%%% <dt>`quiet'</dt>
%%% <dd>Enables quiet mode - no output is printed on screen while PropEr is
%%%   running.</dd>
%%% <dt>`verbose'</dt>
%%% <dd>Enables verbose mode - this is the default mode of operation.</dd>
%%% <dt>`{to_file, <IO_device>}'</dt>
%%% <dd>Redirects all of PropEr's output to `<IO_device>', which should be an
%%%   IO device associated with a file opened for writing.</dd>
%%% <dt>`{on_output, <Output_function>}'</dt>
%%% <dd>PropEr will use the supplied function for all output printing. This
%%%   function should accept two arguments in the style of `io:format/2'.<br/>
%%%   CAUTION: The above output control options are incompatible with each
%%%   other.</dd>
%%% <dt>`long_result'</dt>
%%% <dd>Enables long-result mode (see the {@section Counterexamples} section
%%%   for details).</dd>
%%% <dt>`{numtests, <Positive_number>}' or simply `<Positive_number>'</dt>
%%% <dd>This is equivalent to the {@link numtests/1} property wrapper. Any
%%%   {@link numtests/1} wrappers in the actual property will overwrite this
%%%   setting.</dd>
%%% <dt>`{start_size, <Size>}'</dt>
%%% <dd>Specifies the initial value of the `size' parameter (default is 1), see
%%%   the documentation of the {@link proper_types} module for details.</dd>
%%% <dt>`{max_size, <Size>}'</dt>
%%% <dd>Specifies the maximum value of the `size' parameter (default is 42), see
%%%   the documentation of the {@link proper_types} module for details.</dd>
%%% <dt>`{max_shrinks, <Non_negative_number>}'</dt>
%%% <dd>Specifies the maximum number of times a failing test case should be
%%%   shrunk before returning. Note that the shrinking may stop before so many
%%%   shrinks are achieved if the shrinking subsystem deduces that it cannot
%%%   shrink the failing test case further. Default is 500.</dd>
%%% <dt>`noshrink'</dt>
%%% <dd>Instructs PropEr to not attempt to shrink any failing test cases.</dd>
%%% <dt>`{constraint_tries, <Positive_number>}'</dt>
%%% <dd>Specifies the maximum number of tries before the generator subsystem
%%%   gives up on producing an instance that satisfies a `?SUCHTHAT'
%%%   constraint. Default is 50.</dd>
%%% <dt>`fails'</dt>
%%% <dd>This is equivalent to the {@link fails/1} property wrapper.</dd>
%%% <dt>`{spec_timeout, infinity | <Non_negative_number>}'</dt>
%%% <dd>When testing a spec, PropEr will consider an input to be failing if the
%%%   function under test takes more than the specified amount of milliseconds
%%%   to return for that input.</dd>
%%% <dt>`any_to_integer'</dt>
%%% <dd>All generated instances of the type {@link proper_types:any/0} will be
%%%   integers. This is provided as a means to speed up the testing of specs,
%%%   where `any()' is a commonly used type (see the {@section Spec testing}
%%%   section for details).</dd>
%%% </dl>
%%%
%%% == Spec testing ==
%%% You can test the accuracy of an exported function's spec by running
%%% {@link check_spec/1} or {@link check_spec/2}.
%%% Under this mode of operation, PropEr will call the provided function with
%%% increasingly complex valid inputs (according to its spec) and test that no
%%% unexpected value is returned. If an input is found that violates the spec,
%%% it will be saved as a counterexample and PropEr will attempt to shrink it.
%%%
%%% You can test all exported functions of a module against their spec by
%%% running {@link check_specs/1} or {@link check_specs/2}.
%%%
%%% The use of `check_spec' is subject to the following usage rules:
%%%
%%% <ul>
%%% <li>Currently, PropEr can't test functions whose range contains a type
%%%   that exhibits a certain kind of self-reference: it is (directly or
%%%   indirectly) self-recursive and at least one recursion path contains only
%%%   unions and type references. E.g. these types are acceptable:
%%%       ``` -type a(T) :: T | {'bar',a(T)}.
%%%           -type b() :: 42 | [c()].
%%%           -type c() :: {'baz',b()}.'''
%%%   while these are not:
%%%       ``` -type a() :: 'foo' | b().
%%%           -type b() :: c() | [integer()].
%%%           -type c() :: 'bar' | a().
%%%           -type d(T) :: T | d({'baz',T}).''' </li>
%%% <li>Throwing any exception or raising an `error:badarg' is considered
%%%   normal behaviour. Currently, users cannot fine-tune this setting.</li>
%%% <li>Only the first clause of the function's spec is considered.</li>
%%% <li>The only spec constraints we accept are is_subtype' constraints whose
%%%   first argument is a simple, non-'_' variable. It is not checked whether or
%%%   not these variables actually appear in the spec. The second argument of an
%%%   `is_subtype' constraint cannot contain any non-'_' variables. Multiple
%%%   constraints for the same variable are not supported.</li>
%%% </ul>
%%%
%%% == Errors ==
%%% The following errors may be encountered during testing. The term provided
%%% for each error is the error type returned by proper:quickcheck in case such
%%% an error occurs. Normaly, a message is also printed on screen describing
%%% the error.
%%%
%%% <dl>
%%% <dt>`arity_limit'</dt>
%%% <dd>The random instance generation subsystem has failed to produce
%%%   a function of the desired arity. Please recompile PropEr with a suitable
%%%   value for `?MAX_ARITY' (defined in `proper_internal.hrl'). This error
%%%   should only be encountered during normal operation.</dd>
%%% <dt>`cant_generate'</dt>
%%% <dd>The random instance generation subsystem has failed to
%%%   produce an instance that satisfies some `?SUCHTHAT' constraint. You
%%%   should either increase the `constraint_tries' limit, loosen the failing
%%%   constraint, or make it non-strict. This error should only be encountered
%%%   during normal operation.</dd>
%%% <dt>`cant_satisfy'</dt>
%%% <dd>All the tests were rejected because no produced test case
%%%   would pass all `?IMPLIES' checks. You should loosen the failing `?IMPLIES'
%%%   constraint(s). This error should only be encountered during normal
%%%   operation.</dd>
%%% <dt>`non_boolean_result'</dt>
%%% <dd>The property code returned a non-boolean result. Please
%%%   fix your property.</dd>
%%% <dt>`rejected'</dt>
%%% <dd>Only encountered during re-checking, the counterexample does not
%%%   match the property, since the counterexample doesn't pass an `?IMPLIES'
%%%   check.</dd>
%%% <dt>`too_many_instances'</dt>
%%% <dd>Only encountered during re-checking, the counterexample
%%%   does not match the property, since the counterexample contains more
%%%   instances than there are `?FORALL's in the property.</dd>
%%% <dt>`type_mismatch'</dt>
%%% <dd>The variables' and types' structures inside a `?FORALL' don't
%%%   match. Please check your properties.</dd>
%%% <dt>`{typeserver, <SubError>}'</dt>
%%% <dd>The typeserver encountered an error. The `<SubError>' field contains
%%%   specific information regarding the error.</dd>
%%% <dt>`{unexpected, <Result>}'</dt>
%%% <dd>A test returned an unexpected result during normal operation. If you
%%%   ever get this error, it means that you have found a bug in PropEr
%%%   - please send an error report to the maintainers and remember to include
%%%   both the failing test case and the output of the program, if possible.
%%%   </dd>
%%% <dt>`{unrecognized_option, <Option>}'</dt>
%%% <dd>`<Option>' is not an option that PropEr understands.</dd>
%%% </dl>

-module(proper).
-export([quickcheck/1, quickcheck/2, counterexample/1, counterexample/2,
	 check/2, check/3, module/1, module/2, check_spec/1, check_spec/2,
	 check_specs/1, check_specs/2]).
-export([numtests/2, fails/1, on_output/2, conjunction/1]).
-export([collect/2, collect/3, aggregate/2, aggregate/3, classify/3, measure/3,
	 with_title/1, equals/2]).
-export([counterexample/0, counterexamples/0]).
-export([clean_garbage/0, global_state_erase/0]).

-export([get_size/1, global_state_init_size/1, report_error/2]).
-export([pure_check/1, pure_check/2]).
-export([forall/2, implies/2, whenfail/2, trapexit/1, timeout/2]).

-export_type([test/0, outer_test/0, counterexample/0, exception/0]).

-include("proper_internal.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------

-define(MISMATCH_MSG, "Error: The input doesn't correspond to this property: ").


%%-----------------------------------------------------------------------------
%% Test types
%%-----------------------------------------------------------------------------

-type imm_testcase() :: [imm_input()].
-type imm_input() :: proper_gen:imm_instance()
		   | {'$conjunction',sub_imm_testcases()}.
-type sub_imm_testcases() :: [{tag(),imm_testcase()}].
-type imm_counterexample() :: [imm_clean_input()].
-type imm_clean_input() :: proper_gen:instance()
			 | {'$conjunction',sub_imm_counterexamples()}.
-type sub_imm_counterexamples() :: [{tag(),imm_counterexample()}].
-type counterexample() :: [clean_input()].
%% @alias
-type clean_input() :: proper_gen:instance() | sub_counterexamples().
%% @alias
-type sub_counterexamples() :: [{tag(),counterexample()}].

-type sample() :: [term()].
-type freq_sample() :: [{term(),frequency()}].
-type side_effects_fun() :: fun(() -> 'ok').
-type fail_actions() :: [side_effects_fun()].
-type output_fun() :: fun((string(),[term()]) -> 'ok').
%% A fun to be used by PropEr for output printing. Such a fun should follow the
%% conventions of `io:format/2'.
-type tag() :: atom().
-type title() :: atom() | string().
-type stats_printer() :: fun((sample()) -> 'ok')
		       | fun((sample(),output_fun()) -> 'ok').
%% A stats-printing function that can be passed to some of the statistics
%% collection functions, to be used instead of the predefined stats-printer.
%% Such a function will be called at the end of testing (in case no test fails)
%% with a sorted list of collected terms. A commonly used stats-printer is
%% `with_title/1'.
-type numeric_stat() :: number() | 'undefined'.
-type numeric_stats() :: {numeric_stat(),numeric_stat(),numeric_stat()}.
-type time_period() :: non_neg_integer().

%% TODO: This should be opaque.
%% @type outer_test(). A testable property that has optionally been wrapped with
%% one or more <a href="#external-wrappers">external wrappers</a>.
-type outer_test() :: test()
		    | numtests_clause()
		    | fails_clause()
		    | on_output_clause().
%% TODO: This should be opaque.
%% TODO: Should the tags be of the form '$...'?
%% @type test(). A testable property that has not been wrapped with an
%% <a href="#external-wrappers">external wrapper</a>.
-type test() :: boolean()
	      | forall_clause()
	      | conjunction_clause()
	      | implies_clause()
	      | sample_clause()
	      | whenfail_clause()
	      | trapexit_clause()
	      | timeout_clause().
	      %%| always_clause()
	      %%| sometimes_clause()
-type delayed_test() :: fun(() -> test()).
-type dependent_test() :: fun((proper_gen:instance()) -> test()).
-type lazy_test() :: delayed_test() | dependent_test().
-type raw_test_kind() :: 'test' | 'spec'.
-type raw_test() :: {'test',test()} | {'spec',mfa()}.
-type stripped_test() :: boolean()
		       | {proper_types:type(), dependent_test()}
		       | [{tag(),test()}].

-type numtests_clause() :: {'numtests', pos_integer(), outer_test()}.
-type fails_clause() :: {'fails', outer_test()}.
-type on_output_clause() :: {'on_output', output_fun(), outer_test()}.

-type forall_clause() :: {'forall', proper_types:raw_type(), dependent_test()}.
-type conjunction_clause() :: {'conjunction', [{tag(),test()}]}.
-type implies_clause() :: {'implies', boolean(), delayed_test()}.
-type sample_clause() :: {'sample', sample(), stats_printer(), test()}.
-type whenfail_clause() :: {'whenfail', side_effects_fun(), delayed_test()}.
-type trapexit_clause() :: {'trapexit', fun(() -> boolean())}.
-type timeout_clause() :: {'timeout', time_period(), fun(() -> boolean())}.
%%-type always_clause() :: {'always', pos_integer(), delayed_test()}.
%%-type sometimes_clause() :: {'sometimes', pos_integer(), delayed_test()}.


%%-----------------------------------------------------------------------------
%% Options and Context types
%%-----------------------------------------------------------------------------

%% TODO: Rename this to 'options()'?
-type user_opt() :: 'quiet'
		  | 'verbose'
		  | {'to_file',file:io_device()}
		  | {'on_output',output_fun()}
		  | 'long_result'
		  | {'numtests', pos_integer()}
		  | pos_integer()
		  | {'start_size',size()}
		  | {'max_size', size()}
		  | {'max_shrinks',non_neg_integer()}
		  | 'noshrink'
		  | {'constraint_tries',pos_integer()}
		  | 'fails'
		  | 'any_to_integer'
		  | {'spec_timeout',timeout()}.
-type user_opts() :: [user_opt()] | user_opt().
-record(opts, {output_fun       = fun io:format/2 :: output_fun(),
	       long_result      = false           :: boolean(),
	       numtests         = 100             :: pos_integer(),
	       start_size       = 1               :: size(),
	       max_size         = 42              :: size(),
	       max_shrinks      = 500             :: non_neg_integer(),
	       noshrink         = false           :: boolean(),
	       constraint_tries = 50              :: pos_integer(),
	       expect_fail      = false           :: boolean(),
	       any_type	                          :: {'type',
						      proper_types:type()},
	       spec_timeout     = infinity        :: timeout()}).
-type opts() :: #opts{}.
-record(ctx, {mode     = new :: 'new' | 'try_shrunk' | 'try_cexm',
	      bound    = []  :: imm_testcase() | counterexample(),
	      actions  = []  :: fail_actions(),
	      samples  = []  :: [sample()],
	      printers = []  :: [stats_printer()]}).
-type ctx() :: #ctx{}.


%%-----------------------------------------------------------------------------
%% Result types
%%-----------------------------------------------------------------------------

-record(pass, {reason    :: pass_reason(),
	       samples   :: [sample()],
	       printers  :: [stats_printer()],
	       performed :: pos_integer()}).
-record(fail, {reason    :: fail_reason(),
	       bound     :: imm_testcase() | counterexample(),
	       actions   :: fail_actions(),
	       performed :: pos_integer()}).
%% @alias
-type error() :: {'error', error_reason()}.

-type pass_reason() :: 'true_prop' | 'didnt_crash'.
-type fail_reason() :: 'false_prop' | 'time_out' | {'trapped',exc_reason()}
		     | exception() | {'sub_props',[{tag(),fail_reason()},...]}.
%% @private_type
-type exception() :: {'exception',exc_kind(),exc_reason(),stacktrace()}.
-type exc_kind() :: 'throw' | 'error' | 'exit'.
-type exc_reason() :: term().
-type stacktrace() :: [{atom(),atom(),arity() | [term()]}].
-type error_reason() :: 'arity_limit' | 'cant_generate' | 'cant_satisfy'
		      | 'non_boolean_result' | 'rejected' | 'too_many_instances'
		      | 'type_mismatch' | 'wrong_type' | {'typeserver',term()}
		      | {'unexpected',any()} | {'unrecognized_option',term()}.

-type run_result() :: #pass{performed :: 'undefined'}
		    | #fail{performed :: 'undefined'}
		    | error().
-type imm_result() :: #pass{reason :: 'undefined'} | #fail{} | error().
-type long_result() :: 'true' | counterexample() | error().
-type short_result() :: boolean() | error().
-type result() :: long_result() | short_result().
-type long_module_result() :: [{mfa(),counterexample()}] | error().
-type short_module_result() :: [mfa()] | error().
-type module_result() :: long_module_result() | short_module_result().
-type shrinking_result() :: {non_neg_integer(),imm_testcase()}.


%%-----------------------------------------------------------------------------
%% State handling functions
%%-----------------------------------------------------------------------------

-spec grow_size(opts()) -> 'ok'.
grow_size(#opts{max_size = MaxSize} = Opts) ->
    Size = get('$size'),
    case Size < MaxSize of
	true ->
	    case get('$left') of
		0 ->
		    put('$size', Size + 1),
		    case tests_at_size(Size + 1, Opts) of
			0 -> grow_size(Opts);
			N -> put('$left', N - 1), ok
		    end;
		Left ->
		    put('$left', Left - 1),
		    ok
	    end;
	false ->
	    ok
    end.

-spec tests_at_size(size(), opts()) -> non_neg_integer().
tests_at_size(Size, #opts{numtests = NumTests, start_size = StartSize,
			  max_size = MaxSize}) ->
    SizesToTest = MaxSize - StartSize + 1,
    case NumTests >= SizesToTest of
	true ->
	    TotalOverflow = NumTests rem SizesToTest,
	    Overflow = case MaxSize - Size < TotalOverflow of
			   true  -> 1;
			   false -> 0
		       end,
	    NumTests div SizesToTest + Overflow;
	false ->
	    EverySoManySizes = SizesToTest div NumTests,
	    case (Size - StartSize) rem EverySoManySizes of
		0 -> 1;
		_ -> 0
	    end
    end.

%% @private
-spec get_size(proper_types:type()) -> size() | 'undefined'.
get_size(Type) ->
    case get('$size') of
	undefined ->
	    undefined;
	Size ->
	    case proper_types:find_prop(size_transform, Type) of
		{ok,Transform} -> Transform(Size);
		error          -> Size
	    end
    end.

%% @private
-spec global_state_init_size(size()) -> 'ok'.
global_state_init_size(Size) ->
    global_state_init(#opts{start_size = Size}).

-spec global_state_init(opts()) -> 'ok'.
global_state_init(#opts{start_size = StartSize, constraint_tries = CTries,
			any_type = AnyType} = Opts) ->
    clean_garbage(),
    put('$size', StartSize - 1),
    put('$left', 0),
    grow_size(Opts),
    put('$constraint_tries', CTries),
    put('$any_type',AnyType),
    proper_arith:rand_start(),
    proper_typeserver:start(),
    ok.

-spec global_state_reset(opts()) -> 'ok'.
global_state_reset(#opts{start_size = StartSize} = Opts) ->
    clean_garbage(),
    put('$size', StartSize - 1),
    put('$left', 0),
    grow_size(Opts).

%% @private
-spec global_state_erase() -> 'ok'.
global_state_erase() ->
    proper_typeserver:stop(),
    proper_arith:rand_stop(),
    erase('$any_type'),
    erase('$constraint_tries'),
    erase('$left'),
    erase('$size'),
    erase('$parameters'),
    ok.

%% @private
-spec spawn_link_migrate(fun(() -> _)) -> pid().
spawn_link_migrate(ActualFun) ->
    PDictStuff = get(),
    Fun = fun() ->
	      lists:foreach(fun({K,V}) -> put(K,V) end, PDictStuff),
	      proper_arith:rand_reseed(),
	      ActualFun()
	  end,
    spawn_link(Fun).

-spec save_counterexample(counterexample()) -> 'ok'.
save_counterexample(CExm) ->
    put('$counterexample', CExm),
    ok.

%% @doc Retrieves the last (simplest) counterexample produced by PropEr during
%% the most recent testing run.
-spec counterexample() -> counterexample() | 'undefined'.
counterexample() ->
    get('$counterexample').

-spec save_counterexamples([{mfa(),counterexample()}]) -> 'ok'.
save_counterexamples(CExms) ->
    put('$counterexamples', CExms),
    ok.

%% @doc Returns a counterexample for each failing property of the most recent
%% module testing run.
-spec counterexamples() -> [{mfa(),counterexample()}] | 'undefined'.
counterexamples() ->
    get('$counterexamples').

%% @doc Cleans up the process dictionary of all PropEr-produced entries.
-spec clean_garbage() -> 'ok'.
clean_garbage() ->
    erase('$counterexample'),
    erase('$counterexamples'),
    ok.


%%-----------------------------------------------------------------------------
%% Public interface functions
%%-----------------------------------------------------------------------------

%% @doc Runs PropEr on the property `OuterTest'.
-spec quickcheck(outer_test()) -> result().
quickcheck(OuterTest) ->
    quickcheck(OuterTest, []).

%% @doc Same as {@link quickcheck/1}, but also accepts a list of options.
-spec quickcheck(outer_test(), user_opts()) -> result().
quickcheck(OuterTest, UserOpts) ->
    try parse_opts(UserOpts) of
	ImmOpts ->
	    {Test,Opts} = peel_test(OuterTest, ImmOpts),
	    test({test,Test}, Opts)
    catch
	throw:{unrecognized_option,_UserOpt} = Reason ->
	    report_error(Reason, fun io:format/2),
	    {error, Reason}
    end.

%% @equiv quickcheck(OuterTest, [long_result])
-spec counterexample(outer_test()) -> long_result().
counterexample(OuterTest) ->
    counterexample(OuterTest, []).

%% @doc Same as {@link counterexample/1}, but also accepts a list of options.
-spec counterexample(outer_test(), user_opts()) -> long_result().
counterexample(OuterTest, UserOpts) ->
    quickcheck(OuterTest, add_user_opt(long_result,UserOpts)).

%% @private
%% @doc Runs PropEr in pure mode. Under this mode, PropEr will perform no I/O
%% and will not access the caller's process dictionary in any way. Please note
%% that PropEr will not actually run as a pure function under this mode.
-spec pure_check(outer_test()) -> result().
pure_check(OuterTest) ->
    pure_check(OuterTest, []).

%% @private
%% @doc Same as {@link pure_check/2}, but also accepts a list of options.
-spec pure_check(outer_test(), user_opts()) -> result().
pure_check(OuterTest, ImmUserOpts) ->
    Parent = self(),
    UserOpts = add_user_opt(quiet, ImmUserOpts),
    spawn_link(fun() -> Parent ! {result,quickcheck(OuterTest,UserOpts)} end),
    receive
	{result,Result} -> Result
    end.

%% @doc Tests the accuracy of an exported function's spec.
-spec check_spec(mfa()) -> result().
check_spec(MFA) ->
    check_spec(MFA, []).

%% @doc Same as {@link check_spec/1}, but also accepts a list of options.
-spec check_spec(mfa(), user_opts()) -> result().
check_spec(MFA, UserOpts) ->
    try parse_opts(UserOpts) of
	Opts ->
	    test({spec,MFA}, Opts)
    catch
	throw:{unrecognized_option,_UserOpt} = Reason ->
	    report_error(Reason, fun io:format/2),
	    {error, Reason}
    end.

%% @doc Re-checks a specific counterexample `CExm' against the property
%% `OuterTest' that it previously falsified.
-spec check(outer_test(), counterexample()) -> short_result().
check(OuterTest, CExm) ->
    check(OuterTest, CExm, []).

%% @doc Same as {@link check/2}, but also accepts a list of options.
-spec check(outer_test(), counterexample(), user_opts()) -> short_result().
check(OuterTest, CExm, UserOpts) ->
    try parse_opts(UserOpts) of
	ImmOpts ->
	    {Test,Opts} = peel_test(OuterTest, ImmOpts),
	    retry(Test, CExm, Opts)
    catch
	throw:{unrecognized_option,_UserOpt} = Reason ->
	    report_error(Reason, fun io:format/2),
	    {error, Reason}
    end.

%% @doc Tests all properties (i.e., all 0-arity functions whose name begins with
%% `prop_')exported from module `Mod'.
-spec module(mod_name()) -> module_result().
module(Mod) ->
    module(Mod, []).

%% @doc Same as {@link module/1}, but also accepts a list of options.
-spec module(mod_name(), user_opts()) -> module_result().
module(Mod, UserOpts) ->
    multi_test_prep(Mod, test, UserOpts).

%% @doc Tests all exported, `-spec'ed functions of a module `Mod' against their
%% spec.
-spec check_specs(mod_name()) -> module_result().
check_specs(Mod) ->
    check_specs(Mod, []).

%% @doc Same as {@link check_specs/1}, but also accepts a list of options.
-spec check_specs(mod_name(), user_opts()) -> module_result().
check_specs(Mod, UserOpts) ->
    multi_test_prep(Mod, spec, UserOpts).

-spec multi_test_prep(mod_name(), raw_test_kind(), user_opts()) ->
	  module_result().
multi_test_prep(Mod, Kind, UserOpts) ->
    try parse_opts(UserOpts) of
	Opts ->
	    multi_test(Mod, Kind, Opts)
    catch
	throw:{unrecognized_option,_UserOpt} = Reason ->
	    report_error(Reason, fun io:format/2),
	    {error, Reason}
    end.


%%-----------------------------------------------------------------------------
%% Options parsing functions
%%-----------------------------------------------------------------------------

-spec add_user_opt(user_opt(), user_opts()) -> [user_opt(),...].
add_user_opt(NewUserOpt, UserOptsList) when is_list(UserOptsList) ->
    [NewUserOpt | UserOptsList];
add_user_opt(NewUserOpt, SingleUserOpt) ->
    add_user_opt(NewUserOpt, [SingleUserOpt]).

-spec parse_opts(user_opts()) -> opts().
parse_opts(UserOptsList) when is_list(UserOptsList) ->
    parse_opts(lists:reverse(UserOptsList), #opts{});
parse_opts(SingleUserOpt) ->
    parse_opts([SingleUserOpt]).

-spec parse_opts([user_opt()], opts()) -> opts().
parse_opts([], Opts) ->
    Opts;
parse_opts([UserOpt | Rest], Opts) ->
    parse_opts(Rest, parse_opt(UserOpt,Opts)).

-spec parse_opt(user_opt(), opts()) -> opts().
parse_opt(UserOpt, Opts) ->
    case UserOpt of
	quiet                -> Opts#opts{output_fun = fun(_,_) -> ok end};
	verbose              -> Opts#opts{output_fun = fun io:format/2};
	{to_file,IoDev}      -> Opts#opts{output_fun =
				    fun(S,F) -> io:format(IoDev, S, F) end
				};
	{on_output,Print}    -> Opts#opts{output_fun = Print};
	long_result          -> Opts#opts{long_result = true};
	{numtests,N}         -> Opts#opts{numtests = N};
	N when is_integer(N) -> Opts#opts{numtests = N};
	{start_size,Size}    -> Opts#opts{start_size = Size};
	{max_size,Size}      -> Opts#opts{max_size = Size};
	{max_shrinks,N}      -> Opts#opts{max_shrinks = N};
	noshrink             -> Opts#opts{noshrink = true};
	{constraint_tries,N} -> Opts#opts{constraint_tries = N};
	fails                -> Opts#opts{expect_fail = true};
	any_to_integer       -> Opts#opts{any_type =
				    {type,proper_types:integer()}
				};
	{spec_timeout,N}     -> Opts#opts{spec_timeout = N};
	_                    -> throw({unrecognized_option,UserOpt})
    end.

-spec peel_test(outer_test(), opts()) -> {test(),opts()}.
peel_test({numtests,N,OuterTest}, Opts) ->
    peel_test(OuterTest, Opts#opts{numtests = N});
peel_test({fails,OuterTest}, Opts) ->
    peel_test(OuterTest, Opts#opts{expect_fail = true});
peel_test({on_output,Print,OuterTest}, Opts) ->
    peel_test(OuterTest, Opts#opts{output_fun = Print});
peel_test(Test, Opts) ->
    {Test, Opts}.


%%-----------------------------------------------------------------------------
%% Test declaration functions
%%-----------------------------------------------------------------------------

%% TODO: All of these should have a test() or outer_test() return type.

%% @doc Specifies the number `N' of tests to run when testing the property
%% `Test'. Default is 100.
%% @spec numtests(pos_integer(), outer_test()) -> outer_test()
-spec numtests(pos_integer(), outer_test()) -> numtests_clause().
numtests(N, Test) ->
    {numtests, N, Test}.

%% @doc Specifies that we expect the property `Test' to fail for some input. The
%% property will be considered failing if it passes all the tests.
%% @spec fails(outer_test()) -> outer_test()
-spec fails(outer_test()) -> fails_clause().
fails(Test) ->
    {fails, Test}.

%% @doc Specifies an output function `Print' to be used by PropEr for all output
%% printing during the testing of property `Test'. This wrapper is equivalent to
%% the `on_output' option.
%% @spec on_output(output_fun(), outer_test()) -> outer_test()
-spec on_output(output_fun(), outer_test()) -> on_output_clause().
on_output(Print, Test) ->
    {on_output, Print, Test}.

%% @private
-spec forall(proper_types:raw_type(), dependent_test()) -> forall_clause().
forall(RawType, DTest) ->
    {forall, RawType, DTest}.

%% @doc Returns a property that is true only if all of the sub-properties
%% `SubProps' are true. Each sub-property should be tagged with a distinct atom.
%% If this property fails, each failing sub-property will be reported and saved
%% inside the counterexample along with its tag.
%% @spec conjunction([{tag(),test()}]) -> test()
-spec conjunction([{tag(),test()}]) -> conjunction_clause().
conjunction(SubProps) ->
    {conjunction, SubProps}.

%% @private
-spec implies(boolean(), delayed_test()) -> implies_clause().
implies(Pre, DTest) ->
    {implies, Pre, DTest}.

%% @doc Specifies that test cases produced by this property should be
%% categorized under the term `Category'. This field can be an expression or
%% statement block that evaluates to any term. All produced categories are
%% printed at the end of testing (in case no test fails) along with the
%% percentage of test cases belonging to each category. Multiple `collect'
%% wrappers are allowed in a single property, in which case the percentages for
%% each `collect' wrapper are printed separately.
%% @spec collect(term(), test()) -> test()
-spec collect(term(), test()) -> sample_clause().
collect(Category, Test) ->
    collect(with_title(""), Category, Test).

%% @doc Same as {@link collect/2}, but also accepts a fun `Printer' to be used
%% as the stats printer.
%% @spec collect(stats_printer(), term(), test()) -> test()
-spec collect(stats_printer(), term(), test()) -> sample_clause().
collect(Printer, Category, Test) ->
    aggregate(Printer, [Category], Test).

%% @doc Same as {@link collect/2}, but accepts a list of categories under which
%% to classify the produced test case.
%% @spec aggregate(sample(), test()) -> test()
-spec aggregate(sample(), test()) -> sample_clause().
aggregate(Sample, Test) ->
    aggregate(with_title(""), Sample, Test).

%% @doc Same as {@link collect/3}, but accepts a list of categories under which
%% to classify the produced test case.
%% @spec aggregate(stats_printer(), sample(), test()) -> test()
-spec aggregate(stats_printer(), sample(), test()) -> sample_clause().
aggregate(Printer, Sample, Test) ->
    {sample, Sample, Printer, Test}.

%% @doc Same as {@link collect/2}, but can accept both a single category and a
%% list of categories. `Count' is a boolean flag: when `false', the particular
%% test case will not be counted.
%% @spec classify(Count::boolean(), term() | sample(), test()) -> test()
-spec classify(boolean(), term() | sample(), test()) -> sample_clause().
classify(false, _TermOrSample, Test) ->
    aggregate([], Test);
classify(true, Sample, Test) when is_list(Sample) ->
    aggregate(Sample, Test);
classify(true, Term, Test) ->
    collect(Term, Test).

%% @doc A function that collects numeric statistics on the produced instances.
%% The number (or numbers) provided are collected and some statistics over the
%% collected sample are printed at the end of testing (in case no test fails),
%% prepended with `Title', which should be an atom or string.
%% @spec measure(title(), number() | [number()], test()) -> test()
-spec measure(title(), number() | [number()], test()) -> sample_clause().
measure(Title, Sample, Test) when is_number(Sample) ->
    measure(Title, [Sample], Test);
measure(Title, Sample, Test) when is_list(Sample) ->
    aggregate(numeric_with_title(Title), Sample, Test).

%% @private
-spec whenfail(side_effects_fun(), delayed_test()) -> whenfail_clause().
whenfail(Action, DTest) ->
    {whenfail, Action, DTest}.

%% @private
-spec trapexit(fun(() -> boolean())) -> trapexit_clause().
trapexit(DTest) ->
    {trapexit, DTest}.

%% @private
-spec timeout(time_period(), fun(() -> boolean())) -> timeout_clause().
timeout(Limit, DTest) ->
    {timeout, Limit, DTest}.

%% @doc A custom property that evaluates to `true' only if `A =:= B', else
%% evaluates to `false' and prints "`A =/= B'" on the screen.
%% @spec equals(term(), term()) -> test()
-spec equals(term(), term()) -> whenfail_clause().
equals(A, B) ->
    ?WHENFAIL(io:format("~w =/= ~w~n",[A,B]), A =:= B).


%%-----------------------------------------------------------------------------
%% Bulk testing functions
%%-----------------------------------------------------------------------------

-spec test(raw_test(), opts()) -> result().
test(RawTest, Opts) ->
    global_state_init(Opts),
    Result = inner_test(RawTest, Opts),
    global_state_erase(),
    Result.

-spec inner_test(raw_test(), opts()) -> result().
inner_test(RawTest, #opts{numtests = NumTests, long_result = ReturnLong,
			  output_fun = Print} = Opts) ->
    Test = cook_test(RawTest, Opts),
    ImmResult = perform(NumTests, Test, Opts),
    Print("~n", []),
    report_imm_result(ImmResult, Opts),
    {ShortResult,LongResult} = get_result(ImmResult, Test, Opts),
    case ReturnLong of
	true  -> LongResult;
	false -> ShortResult
    end.

-spec retry(test(), counterexample(), opts()) -> short_result().
retry(Test, CExm, Opts) ->
    global_state_init(Opts),
    RunResult = rerun(Test, false, CExm),
    report_rerun_result(RunResult, Opts),
    ShortResult = get_rerun_result(RunResult),
    global_state_erase(),
    ShortResult.

-spec multi_test(mod_name(), raw_test_kind(), opts()) -> module_result().
multi_test(Mod, RawTestKind,
	   #opts{long_result = ReturnLong, output_fun = Print} = Opts) ->
    global_state_init(Opts),
    MaybeMFAs =
	case RawTestKind of
	    test -> {ok, [{Mod,Name,0} || {Name,0} <- Mod:module_info(exports),
					  lists:prefix(?PROPERTY_PREFIX,
						       atom_to_list(Name))]};
	    spec -> proper_typeserver:get_exp_specced(Mod)
	end,
    {ShortResult, LongResult} =
	case MaybeMFAs of
	    {ok,MFAs} ->
		RawLRes = [{MFA,mfa_test(MFA,RawTestKind,Opts)} || MFA <- MFAs],
		LRes = [T || {_MFA,Res} = T <- RawLRes, is_list(Res)],
		SRes = [MFA || {MFA,_Res} <- LRes],
		save_counterexamples(LRes),
		{SRes, LRes};
	    {error,SubReason} ->
		Reason = {typeserver,SubReason},
		report_error(Reason, Print),
		Error = {error,Reason},
		{Error, Error}
	end,
    global_state_erase(),
    case ReturnLong of
	true  -> LongResult;
	false -> ShortResult
    end.

-spec mfa_test(mfa(), raw_test_kind(), opts()) -> long_result().
mfa_test({Mod,Fun,Arity} = MFA, RawTestKind, ImmOpts) ->
    {RawTest,#opts{output_fun = Print} = Opts} =
	case RawTestKind of
	    test ->
		OuterTest = Mod:Fun(),
		{Test,FinalOpts} = peel_test(OuterTest, ImmOpts),
		{{test,Test}, FinalOpts};
	    spec ->
		{{spec,MFA}, ImmOpts}
	end,
    global_state_reset(Opts),
    Print("Testing ~w:~w/~b~n", [Mod,Fun,Arity]),
    LongResult = inner_test(RawTest, Opts#opts{long_result = true}),
    Print("~n", []),
    LongResult.

-spec cook_test(raw_test(), opts()) -> test().
cook_test({test,Test}, _Opts) ->
    Test;
cook_test({spec,MFA}, #opts{spec_timeout = SpecTimeout}) ->
    case proper_typeserver:create_spec_test(MFA, SpecTimeout) of
	{ok,Test} ->
	    Test;
	{error,Reason}  ->
	    ?FORALL(_, dummy, throw({'$typeserver',Reason}))
    end.

-spec get_result(imm_result(),test(),opts()) -> {short_result(),long_result()}.
get_result(#pass{}, _Test, _Opts) ->
    {true, true};
get_result(#fail{reason = Reason, bound = Bound}, Test, Opts) ->
    case shrink(Bound, Test, Reason, Opts) of
	{ok,MinImmTestCase} ->
	    MinTestCase = clean_testcase(MinImmTestCase),
	    save_counterexample(MinTestCase),
	    {false, MinTestCase};
	{error,ErrorReason} = Error ->
	    report_error(ErrorReason, Opts#opts.output_fun),
	    {Error, Error}
    end;
get_result({error,_Reason} = ErrorResult, _Test, _Opts) ->
    {ErrorResult, ErrorResult}.

-spec get_rerun_result(run_result()) -> short_result().
get_rerun_result(#pass{}) ->
    true;
get_rerun_result(#fail{}) ->
    false;
get_rerun_result({error,_Reason} = ErrorResult) ->
    ErrorResult.

-spec perform(non_neg_integer(), test(), opts()) -> imm_result().
perform(NumTests, Test, Opts) ->
    perform(0, NumTests, ?MAX_TRIES_FACTOR * NumTests, Test, none, none, Opts).

-spec perform(non_neg_integer(), non_neg_integer(), non_neg_integer(), test(),
	      [sample()] | 'none', [stats_printer()] | 'none', opts()) ->
	  imm_result().
perform(Passed, _ToPass, 0, _Test, Samples, Printers, _Opts) ->
    case Passed of
	0 -> {error, cant_satisfy};
	_ -> #pass{samples = Samples, printers = Printers, performed = Passed}
    end;
perform(ToPass, ToPass, _TriesLeft, _Test, Samples, Printers, _Opts) ->
    #pass{samples = Samples, printers = Printers, performed = ToPass};
perform(Passed, ToPass, TriesLeft, Test, Samples, Printers,
	#opts{output_fun = Print} = Opts) ->
    case run(Test) of
	#pass{reason = true_prop, samples = MoreSamples,
	      printers = MorePrinters} ->
	    Print(".", []),
	    NewSamples = add_samples(MoreSamples, Samples),
	    NewPrinters = case Printers of
			      none -> MorePrinters;
			      _    -> Printers
			  end,
	    grow_size(Opts),
	    perform(Passed + 1, ToPass, TriesLeft - 1, Test,
		    NewSamples, NewPrinters, Opts);
	#fail{} = FailResult ->
	    Print("!", []),
	    FailResult#fail{performed = Passed + 1};
	{error, rejected} ->
	    Print("x", []),
	    grow_size(Opts),
	    perform(Passed, ToPass, TriesLeft - 1, Test,
		    Samples, Printers, Opts);
	{error, Reason} = Error when Reason =:= arity_limit
			      orelse Reason =:= cant_generate
			      orelse Reason =:= non_boolean_result
			      orelse Reason =:= type_mismatch ->
	    Error;
	{error, {typeserver,_SubReason}} = Error ->
	    Error;
	Other ->
	    {error, {unexpected,Other}}
    end.

-spec add_samples([sample()], [sample()] | 'none') -> [sample()].
add_samples(MoreSamples, none) ->
    MoreSamples;
add_samples(MoreSamples, Samples) ->
    [M ++ S || {M,S} <- proper_arith:safe_zip(MoreSamples,Samples)].


%%-----------------------------------------------------------------------------
%% Single test runner functions
%%-----------------------------------------------------------------------------

-spec run(test()) -> run_result().
run(Test) ->
    run(Test, #ctx{}).

-spec rerun(test(),boolean(),imm_testcase() | counterexample()) -> run_result().
rerun(Test, IsImm, ToTry) ->
    Mode = case IsImm of true -> try_shrunk; false -> try_cexm end,
    Ctx = #ctx{mode = Mode, bound = ToTry},
    run(Test, Ctx).

-spec run(test(), ctx()) -> run_result().
run(Result, #ctx{mode = Mode, bound = Bound} = Ctx) when is_boolean(Result) ->
    case Mode =:= new orelse Bound =:= [] of
	true ->
	    case Result of
		true  -> create_pass_result(Ctx, true_prop);
		false -> create_fail_result(Ctx, false_prop)
	    end;
	false ->
	    {error, too_many_instances}
    end;
run({forall,RawType,Prop}, #ctx{mode = new, bound = Bound} = Ctx) ->
    case proper_gen:safe_generate(RawType) of
	{ok,ImmInstance} ->
	    Instance = proper_gen:clean_instance(ImmInstance),
	    NewCtx = Ctx#ctx{bound = [ImmInstance | Bound]},
	    force(Instance, Prop, NewCtx);
	{error,_Reason} = Error ->
	    Error
    end;
run({forall,_RawType,_Prop}, #ctx{bound = []} = Ctx) ->
    create_pass_result(Ctx, didnt_crash);
run({forall,RawType,Prop}, #ctx{mode = try_shrunk,
				bound = [ImmInstance | Rest]} = Ctx) ->
    case proper_types:safe_is_instance(ImmInstance, RawType) of
	true ->
	    Instance = proper_gen:clean_instance(ImmInstance),
	    force(Instance, Prop, Ctx#ctx{bound = Rest});
	false ->
	    %% TODO: could try to fix the instances here
	    {error, wrong_type};
	{error,_Reason} = Error ->
	    Error
    end;
run({forall,_RawType,Prop}, #ctx{mode = try_cexm,
				 bound = [Instance | Rest]} = Ctx) ->
    force(Instance, Prop, Ctx#ctx{bound = Rest});
run({conjunction,SubProps}, #ctx{mode = new} = Ctx) ->
    run_all(SubProps, [], Ctx);
run({conjunction,SubProps}, #ctx{mode = try_shrunk, bound = Bound} = Ctx) ->
    case Bound of
	[] ->
	    create_pass_result(Ctx, didnt_crash);
	[{'$conjunction',SubImmTCs}] ->
	    run_all(SubProps, SubImmTCs, Ctx#ctx{bound = []});
	_ ->
	    {error, too_many_instances}
    end;
run({conjunction,SubProps}, #ctx{mode = try_cexm, bound = Bound} = Ctx) ->
    RealBound = case Bound of [] -> [[]]; _ -> Bound end,
    case RealBound of
	[SubTCs] -> run_all(SubProps, SubTCs, Ctx#ctx{bound = []});
	_        -> {error, too_many_instances}
    end;
run({implies,true,Prop}, Ctx) ->
    force(Prop, Ctx);
run({implies,false,_Prop}, _Ctx) ->
    {error, rejected};
run({sample,NewSample,NewPrinter,Prop}, #ctx{samples = Samples,
					     printers = Printers} = Ctx) ->
    NewCtx = Ctx#ctx{samples = [NewSample | Samples],
		     printers = [NewPrinter | Printers]},
    run(Prop, NewCtx);
run({whenfail,NewAction,Prop}, #ctx{actions = Actions} = Ctx)->
    NewCtx = Ctx#ctx{actions = [NewAction | Actions]},
    force(Prop, NewCtx);
run({trapexit,Prop}, Ctx) ->
    OldFlag = process_flag(trap_exit, true),
    Self = self(),
    Child = spawn_link_migrate(fun() -> child(Self,Prop,Ctx) end),
    Result =
	receive
	    {result, RecvResult} ->
		RecvResult;
	    {'EXIT', Child, ExcReason} ->
		create_fail_result(Ctx, {trapped,ExcReason})
	end,
    true = process_flag(trap_exit, OldFlag),
    Result;
run({timeout,Limit,Prop}, Ctx) ->
    Self = self(),
    Child = spawn_link_migrate(fun() -> child(Self,Prop,Ctx) end),
    receive
	{result, RecvResult} -> RecvResult
    after Limit ->
	unlink(Child),
	exit(Child, kill),
	clear_mailbox(),
	create_fail_result(Ctx, time_out)
    end;
run(_Other, _Ctx) ->
    {error, non_boolean_result}.

-spec run_all([{tag(),test()}], sub_imm_testcases() | sub_counterexamples(),
	      ctx()) -> run_result().
run_all(SubProps, Bound, Ctx) ->
    run_all(SubProps, Bound, [], Ctx).

-spec run_all([{tag(),test()}], sub_imm_testcases() | sub_counterexamples(),
	      [{tag(),fail_reason()}], ctx()) -> run_result().
run_all([], SubBound, SubReasons, #ctx{mode = new, bound = OldBound} = Ctx) ->
    NewBound = [{'$conjunction',lists:reverse(SubBound)} | OldBound],
    NewCtx = Ctx#ctx{bound = NewBound},
    case SubReasons of
	[] -> create_pass_result(NewCtx, true_prop);
	_  -> create_fail_result(NewCtx, {sub_props,lists:reverse(SubReasons)})
    end;
run_all([], SubBound, SubReasons, Ctx) ->
    case {SubBound,SubReasons} of
	{[],[]} ->
	    create_pass_result(Ctx, true_prop);
	{[],_ } ->
	    create_fail_result(Ctx, {sub_props,lists:reverse(SubReasons)});
	{_ ,_ } ->
	    {error, too_many_instances}
    end;
run_all([{Tag,Prop}|Rest], OldSubBound, SubReasons,
	#ctx{mode = Mode, actions = Actions, samples = Samples,
	     printers = Printers} = Ctx) ->
    {SubCtxBound,SubBound} =
	case Mode of
	    new -> {[], OldSubBound};
	    _   -> {proplists:get_value(Tag, OldSubBound, []),
		    proplists:delete(Tag, OldSubBound)}
	end,
    case run(Prop, #ctx{mode = Mode, bound = SubCtxBound}) of
	#pass{samples = MoreSamples, printers = MorePrinters} ->
	    NewSamples = lists:reverse(MoreSamples) ++ Samples,
	    NewPrinters = lists:reverse(MorePrinters) ++ Printers,
	    NewCtx = Ctx#ctx{samples = NewSamples, printers = NewPrinters},
	    run_all(Rest, SubBound, SubReasons, NewCtx);
	#fail{reason = Reason, bound = SubImmTC, actions = MoreActions} ->
	    NewActions = lists:reverse(MoreActions) ++ Actions,
	    NewCtx = Ctx#ctx{actions = NewActions},
	    NewSubBound =
		case Mode of
		    new -> [{Tag,SubImmTC}|SubBound];
		    _   -> SubBound
		end,
	    NewSubReasons = [{Tag,Reason}|SubReasons],
	    run_all(Rest, NewSubBound, NewSubReasons, NewCtx);
	{error,_Reason} = Error ->
	    Error
    end.

-spec force(delayed_test(), ctx()) -> run_result().
force(Prop, Ctx) ->
    apply_args([], Prop, Ctx).

-spec force(proper_gen:instance(), dependent_test(), ctx()) -> run_result().
force(Arg, Prop, Ctx) ->
    apply_args([proper_symb:internal_eval(Arg)], Prop, Ctx).

-spec apply_args([proper_gen:instance()], lazy_test(), ctx()) -> run_result().
apply_args(Args, Prop, Ctx) ->
    try apply(Prop, Args) of
	InnerProp ->
	    run(InnerProp, Ctx)
    catch
	error:ErrReason ->
	    RawTrace = erlang:get_stacktrace(),
	    case ErrReason =:= function_clause
		 andalso threw_exception(Prop, RawTrace) of
		true ->
		    {error, type_mismatch};
		false ->
		    Trace = clean_stacktrace(RawTrace),
		    create_fail_result(Ctx, {exception,error,ErrReason,Trace})
	    end;
	throw:'$arity_limit' ->
	    {error, arity_limit};
	throw:'$cant_generate' ->
	    {error, cant_generate};
	throw:{'$typeserver',SubReason} ->
	    {error, {typeserver,SubReason}};
	ExcKind:ExcReason ->
	    Trace = erlang:get_stacktrace(),
	    create_fail_result(Ctx, {exception,ExcKind,ExcReason,Trace})
    end.

-spec create_pass_result(ctx(), pass_reason()) ->
	  #pass{performed :: 'undefined'}.
create_pass_result(#ctx{samples = Samples, printers = Printers}, Reason) ->
    #pass{reason = Reason, samples = lists:reverse(Samples),
	  printers = lists:reverse(Printers)}.

-spec create_fail_result(ctx(), fail_reason()) ->
	  #fail{performed :: 'undefined'}.
create_fail_result(#ctx{bound = Bound, actions = Actions}, Reason) ->
    #fail{reason = Reason, bound = lists:reverse(Bound),
	  actions = lists:reverse(Actions)}.

-spec child(pid(), delayed_test(), ctx()) -> 'ok'.
child(Father, Prop, Ctx) ->
    Result = force(Prop, Ctx),
    Father ! {result,Result},
    ok.

-spec clear_mailbox() -> 'ok'.
clear_mailbox() ->
    receive
	_ -> clear_mailbox()
    after 0 ->
	ok
    end.

-spec threw_exception(function(), stacktrace()) -> boolean().
threw_exception(Fun, [{TopMod,TopName,TopArgs} | _Rest]) ->
    {module,FunMod} = erlang:fun_info(Fun, module),
    {name,FunName} = erlang:fun_info(Fun, name),
    {arity,FunArity} = erlang:fun_info(Fun, arity),
    TopArity = if
		   is_integer(TopArgs) -> TopArgs;
		   is_list(TopArgs)    -> length(TopArgs)
	       end,
    FunMod =:= TopMod andalso FunName =:= TopName andalso FunArity =:= TopArity.

-spec clean_stacktrace(stacktrace()) -> stacktrace().
clean_stacktrace(RawTrace) ->
    IsNotPropErCall =
	fun({Mod,_Fun,_Args}) ->
	    not lists:prefix("proper", atom_to_list(Mod))
	end,
    {Trace,_Rest} = lists:splitwith(IsNotPropErCall, RawTrace),
    Trace.

-spec clean_testcase(imm_testcase()) -> counterexample().
clean_testcase(ImmTestCase) ->
    finalize_counterexample(preclean_testcase(ImmTestCase, [])).

-spec preclean_testcase(imm_testcase(), imm_counterexample()) ->
	  imm_counterexample().
preclean_testcase([], Acc) ->
    lists:reverse(Acc);
preclean_testcase([{'$conjunction',SubImmTCs} | Rest], Acc) ->
    Rest = [],
    case preclean_sub_imm_testcases(SubImmTCs, []) of
	[]          -> preclean_testcase([], Acc);
	SubImmCExms -> preclean_testcase([], [{'$conjunction',SubImmCExms}|Acc])
    end;
preclean_testcase([ImmInstance | Rest], Acc) ->
    preclean_testcase(Rest, [proper_gen:clean_instance(ImmInstance) | Acc]).

-spec preclean_sub_imm_testcases(sub_imm_testcases(),
				 sub_imm_counterexamples()) ->
	  sub_imm_counterexamples().
preclean_sub_imm_testcases([], Acc) ->
    lists:reverse(Acc);
preclean_sub_imm_testcases([{Tag,ImmTC} | Rest], Acc) ->
    case preclean_testcase(ImmTC, []) of
	[]      -> preclean_sub_imm_testcases(Rest, Acc);
	ImmCExm -> preclean_sub_imm_testcases(Rest, [{Tag,ImmCExm} | Acc])
    end.

-spec finalize_counterexample(imm_counterexample()) -> counterexample().
finalize_counterexample(ImmCExm) ->
    [finalize_input(ImmCleanInput) || ImmCleanInput <- ImmCExm].

-spec finalize_input(imm_clean_input()) -> clean_input().
finalize_input({'$conjunction',SubImmCExms}) ->
    [{Tag,finalize_counterexample(SubImmCExm)}
     || {Tag,SubImmCExm} <- SubImmCExms];
finalize_input(Instance) ->
    Instance.


%%-----------------------------------------------------------------------------
%% Shrinking functions
%%-----------------------------------------------------------------------------

-spec shrink(imm_testcase(), test(), fail_reason(), opts()) ->
	  {'ok',imm_testcase()} | error().
shrink(ImmTestCase, Test, Reason,
       #opts{expect_fail = false, noshrink = false, max_shrinks = MaxShrinks,
	     output_fun = Print} = Opts) ->
    Print("~nShrinking ", []),
    try
	StrTest = skip_to_next(Test),
	fix_shrink(ImmTestCase, StrTest, Reason, 0, MaxShrinks, Opts)
    of
	{Shrinks,MinImmTestCase} ->
	    case rerun(Test, true, MinImmTestCase) of
		#pass{} ->
		    %% TODO: The fail actions are silently skipped.
		    report_shrinking(Shrinks, MinImmTestCase, [], Print),
		    {ok, MinImmTestCase};
		#fail{actions = MinActions} ->
		    report_shrinking(Shrinks, MinImmTestCase, MinActions,
				     Print),
		    {ok, MinImmTestCase};
		{error,_Reason} = Error ->
		    Print("~n", []),
		    Error
	    end
    catch
	throw:non_boolean_result ->
	    Print("~n", []),
	    {error, non_boolean_result}
    end;
shrink(ImmTestCase, _Test, _Reason, _Opts) ->
    {ok, ImmTestCase}.

-spec fix_shrink(imm_testcase(), stripped_test(), fail_reason(),
		 non_neg_integer(), non_neg_integer(), opts()) ->
	  shrinking_result().
fix_shrink(ImmTestCase, _StrTest, _Reason, Shrinks, 0, _Opts) ->
    {Shrinks, ImmTestCase};
fix_shrink(ImmTestCase, StrTest, Reason, Shrinks, ShrinksLeft, Opts) ->
    case shrink([], ImmTestCase, StrTest, Reason, 0, ShrinksLeft, init, Opts) of
	{0,_MinImmTestCase} ->
	    {Shrinks, ImmTestCase};
	{MoreShrinks,MinImmTestCase} ->
	    fix_shrink(MinImmTestCase, StrTest, Reason, Shrinks + MoreShrinks,
		       ShrinksLeft - MoreShrinks, Opts)
    end.

-spec shrink(imm_testcase(), imm_testcase(), stripped_test(), fail_reason(),
	     non_neg_integer(), non_neg_integer(), proper_shrink:state(),
	     opts()) -> shrinking_result().
%% TODO: 'tries_left' instead of 'shrinks_left'? shrinking timeout?
%% TODO: Can we do anything better for non-deterministic tests?
shrink(Shrunk, TestTail, StrTest, _Reason,
       Shrinks, ShrinksLeft, _State, _Opts) when is_boolean(StrTest)
					  orelse ShrinksLeft =:= 0 ->
    {Shrinks, lists:reverse(Shrunk) ++ TestTail};
shrink(Shrunk, [ImmInstance | Rest], {_Type,Prop}, Reason,
       Shrinks, ShrinksLeft, done, Opts) ->
    Instance = proper_gen:clean_instance(ImmInstance),
    NewStrTest = force_skip(Instance, Prop),
    shrink([ImmInstance | Shrunk], Rest, NewStrTest, Reason,
	   Shrinks, ShrinksLeft, init, Opts);
shrink(Shrunk, [ImmInstance | Rest] = TestTail, {Type,Prop} = StrTest, Reason,
       Shrinks, ShrinksLeft, State, Opts) ->
    {NewImmInstances,NewState} = proper_shrink:shrink(ImmInstance, Type, State),
    %% TODO: Should we try fixing the nested ?FORALLs while shrinking? We could
    %%       also just produce new test tails.
    IsValid = fun(I) ->
		  I =/= ImmInstance andalso
		  still_fails(I, Rest, Prop, Reason)
	      end,
    case proper_arith:find_first(IsValid, NewImmInstances) of
	none ->
	    shrink(Shrunk, TestTail, StrTest, Reason,
		   Shrinks, ShrinksLeft, NewState, Opts);
	{Pos, ShrunkImmInstance} ->
	    (Opts#opts.output_fun)(".", []),
	    shrink(Shrunk, [ShrunkImmInstance | Rest], StrTest, Reason,
		   Shrinks+1, ShrinksLeft-1, {shrunk,Pos,NewState}, Opts)
    end;
shrink(Shrunk, [{'$conjunction',SubImmTCs}], SubProps, {sub_props,SubReasons},
       Shrinks, ShrinksLeft, init, Opts) when is_list(SubProps) ->
    shrink_all(Shrunk, [], SubImmTCs, SubProps, SubReasons,
	       Shrinks, ShrinksLeft, Opts).

-spec shrink_all(imm_testcase(), sub_imm_testcases(), sub_imm_testcases(),
		 [{tag(),test()}], [{tag(),fail_reason()}],
		 non_neg_integer(), non_neg_integer(), opts()) ->
	  shrinking_result().
shrink_all(ShrunkHead, Shrunk, SubImmTCs, _SubProps, _SubReasons,
	   Shrinks, 0, _Opts) ->
    ShrunkSubImmTCs = lists:reverse(Shrunk) ++ SubImmTCs,
    ImmTC = lists:reverse([{'$conjunction',ShrunkSubImmTCs} | ShrunkHead]),
    {Shrinks, ImmTC};
shrink_all(ShrunkHead, Shrunk, [], [], [],
	   Shrinks, _ShrinksLeft, Opts) ->
    shrink_all(ShrunkHead, Shrunk, [], [], [], Shrinks, 0, Opts);
shrink_all(ShrunkHead, Shrunk, SubImmTCs, [{Tag,Prop}|Rest], SubReasons,
	   Shrinks, ShrinksLeft, Opts) ->
    case lists:keytake(Tag, 1, SubReasons) of
	{value,{Tag,Reason},NewSubReasons} ->
	    {value,{Tag,SubImmTC},NewSubImmTCs} =
		lists:keytake(Tag, 1, SubImmTCs),
	    {MoreShrinks,MinSubImmTC} =
		shrink([], SubImmTC, skip_to_next(Prop), Reason,
		       0, ShrinksLeft, init, Opts),
	    shrink_all(ShrunkHead, [{Tag,MinSubImmTC}|Shrunk], NewSubImmTCs,
		       Rest, NewSubReasons, Shrinks+MoreShrinks,
		       ShrinksLeft-MoreShrinks, Opts);
	false ->
	    shrink_all(ShrunkHead, Shrunk, SubImmTCs, Rest, SubReasons,
		       Shrinks, ShrinksLeft, Opts)
    end.

-spec still_fails(proper_gen:imm_instance(), imm_testcase(), dependent_test(),
		  fail_reason()) -> boolean().
still_fails(ImmInstance, TestTail, Prop, OldReason) ->
    Instance = proper_gen:clean_instance(ImmInstance),
    Ctx = #ctx{mode = try_shrunk, bound = TestTail},
    case force(Instance, Prop, Ctx) of
	#fail{reason = NewReason} ->
	    same_fail_reason(OldReason, NewReason);
	_ ->
	    false
    end.

-spec same_fail_reason(fail_reason(), fail_reason()) -> boolean().
same_fail_reason({trapped,{SameExcReason,_StackTrace1}},
		 {trapped,{SameExcReason,_StackTrace2}}) ->
    true;
same_fail_reason({exception,SameExcKind,SameExcReason,_StackTrace1},
		 {exception,SameExcKind,SameExcReason,_StackTrace2}) ->
    true; %% We don't mind if the stacktraces are different.
same_fail_reason({sub_props,SubReasons1}, {sub_props,SubReasons2}) ->
    length(SubReasons1) =:= length(SubReasons2) andalso
    lists:all(fun({A,B}) -> same_sub_reason(A,B) end,
	      lists:zip(lists:sort(SubReasons1),lists:sort(SubReasons2)));
same_fail_reason(SameReason, SameReason) ->
    true;
same_fail_reason(_, _) ->
    false.

-spec same_sub_reason({tag(),fail_reason()},{tag(),fail_reason()}) -> boolean().
same_sub_reason({SameTag,Reason1}, {SameTag,Reason2}) ->
    same_fail_reason(Reason1, Reason2);
same_sub_reason(_, _) ->
    false.

-spec skip_to_next(test()) -> stripped_test().
skip_to_next(Result) when is_boolean(Result) ->
    Result;
skip_to_next({forall,RawType,Prop}) ->
    Type = proper_types:cook_outer(RawType),
    {Type, Prop};
skip_to_next({conjunction,SubProps}) ->
    SubProps;
skip_to_next({implies,Pre,Prop}) ->
    case Pre of
	true  -> force_skip(Prop);
	false -> true
    end;
skip_to_next({sample,_Sample,_Printer,Prop}) ->
    skip_to_next(Prop);
skip_to_next({whenfail,_Action,Prop}) ->
    force_skip(Prop);
%% The following 2 clauses assume that _Prop cannot contain any other wrappers.
skip_to_next({trapexit,_Prop}) ->
    false;
skip_to_next({timeout,_Limit,_Prop}) ->
    false;
skip_to_next(_Other) ->
    throw(non_boolean_result).

-spec force_skip(delayed_test()) -> stripped_test().
force_skip(Prop) ->
    apply_skip([], Prop).

-spec force_skip(proper_gen:instance(), dependent_test()) -> stripped_test().
force_skip(Arg, Prop) ->
    apply_skip([proper_symb:internal_eval(Arg)], Prop).

-spec apply_skip([proper_gen:instance()], lazy_test()) -> stripped_test().
apply_skip(Args, Prop) ->
    try
	apply(Prop, Args)
    of
	InnerTest -> skip_to_next(InnerTest)
    catch
	%% Should be OK to catch everything here, since we have already tested
	%% at this point that the test still fails.
	_ExcKind:_ExcReason -> false
    end.


%%-----------------------------------------------------------------------------
%% Output functions
%%-----------------------------------------------------------------------------

-spec report_imm_result(imm_result(), opts()) -> 'ok'.
report_imm_result(#pass{samples = Samples, printers = Printers,
			performed = Performed},
		  #opts{expect_fail = ExpectF, output_fun = Print}) ->
    case ExpectF of
	true  -> Print("Failed: All tests passed when a failure was expected."
		       "~n", []);
	false -> Print("OK: Passed ~b test(s).~n", [Performed])
    end,
    SortedSamples = [lists:sort(Sample) || Sample <- Samples],
    lists:foreach(fun({P,S}) -> apply_stats_printer(P, S, Print) end,
		  proper_arith:safe_zip(Printers, SortedSamples)),
    ok;
report_imm_result(#fail{reason = Reason, bound = Bound, actions = Actions,
			performed = Performed},
		  #opts{expect_fail = ExpectF, output_fun = Print}) ->
    case ExpectF of
	true ->
	    Print("OK: Failed as expected, after ~b test(s).~n", [Performed]);
	false ->
	    Print("Failed: After ~b test(s).~n", [Performed])
    end,
    report_fail_reason(Reason, "", Print),
    print_imm_testcase(Bound, "", Print),
    execute_actions(Actions);
report_imm_result({error,Reason}, #opts{output_fun = Print}) ->
    report_error(Reason, Print).

-spec report_rerun_result(run_result(), opts()) -> 'ok'.
report_rerun_result(#pass{reason = Reason},
		    #opts{expect_fail = ExpectF, output_fun = Print}) ->
    case ExpectF of
	true  -> Print("Failed: ", []);
	false -> Print("OK: ", [])
    end,
    case Reason of
	true_prop   -> Print("The input passed the test.~n", []);
	didnt_crash -> Print("The input didn't raise an early exception.~n", [])
    end;
report_rerun_result(#fail{reason = Reason, actions = Actions},
		    #opts{expect_fail = ExpectF, output_fun = Print}) ->
    case ExpectF of
	true  -> Print("OK: ", []);
	false -> Print("Failed: ", [])
    end,
    Print("The input fails the test.~n", []),
    report_fail_reason(Reason, "", Print),
    execute_actions(Actions);
report_rerun_result({error,Reason}, #opts{output_fun = Print}) ->
    report_error(Reason, Print).

%% @private
-spec report_error(error_reason(), output_fun()) -> 'ok'.
report_error(arity_limit, Print) ->
    Print("Error: Couldn't produce a function of the desired arity, please "
	  "recompile PropEr with an increased value for ?MAX_ARITY.~n", []);
report_error(cant_generate, Print) ->
    Print("Error: Couldn't produce an instance that satisfies all strict "
	  "constraints after ~b tries.~n", [get('$constraint_tries')]);
report_error(cant_satisfy, Print) ->
    Print("Error: No valid test could be generated.~n", []);
report_error(non_boolean_result, Print) ->
    Print("Error: The property code returned a non-boolean result.~n", []);
report_error(rejected, Print) ->
    Print(?MISMATCH_MSG ++ "It failed an ?IMPLIES check.~n", []);
report_error(too_many_instances, Print) ->
    Print(?MISMATCH_MSG ++ "It's too long.~n", []); %% that's what she said
report_error(type_mismatch, Print) ->
    Print("Error: The variables' and types' structures inside a ?FORALL don't "
	  "match.~n", []);
report_error(wrong_type, Print) ->
    Print("Internal error: 'wrong_type' error reached toplevel.~n"
	  "Please notify the maintainers about this error.~n", []);
report_error({typeserver,SubReason}, Print) ->
    Print("Error: The typeserver encountered an error: ~w.~n", [SubReason]);
report_error({unexpected,Unexpected}, Print) ->
    Print("Internal error: The last run returned an unexpected result:~n~w~n"
	  "Please notify the maintainers about this error.~n", [Unexpected]);
report_error({unrecognized_option,UserOpt}, Print) ->
    Print("Error: Unrecognized option: ~w.~n", [UserOpt]).

-spec report_fail_reason(fail_reason(), string(), output_fun()) -> 'ok'.
report_fail_reason(false_prop, _Prefix, _Print) ->
    ok;
report_fail_reason(time_out, Prefix, Print) ->
    Print(Prefix ++ "Test execution timed out.~n", []);
report_fail_reason({trapped,ExcReason}, Prefix, Print) ->
    Print(Prefix ++ "A linked process died with reason ~w.~n", [ExcReason]);
report_fail_reason({exception,ExcKind,ExcReason,StackTrace}, Prefix, Print) ->
    Print(Prefix ++ "An exception was raised: ~w:~w.~n", [ExcKind,ExcReason]),
    Print(Prefix ++ "Stacktrace: ~p.~n", [StackTrace]);
report_fail_reason({sub_props,SubReasons}, Prefix, Print) ->
    Report =
	fun({Tag,Reason}) ->
	    Print(Prefix ++ "Sub-property ~w failed.~n", [Tag]),
	    report_fail_reason(Reason, ">> " ++ Prefix, Print)
	end,
    lists:foreach(Report, SubReasons),
    ok.

-spec print_imm_testcase(imm_testcase(), string(), output_fun()) -> 'ok'.
print_imm_testcase(ImmTestCase, Prefix, Print) ->
    ImmCExm = preclean_testcase(ImmTestCase, []),
    print_imm_counterexample(ImmCExm, Prefix, Print).

-spec print_imm_counterexample(imm_counterexample(), string(), output_fun()) ->
	  'ok'.
print_imm_counterexample(ImmCExm, Prefix, Print) ->
    PrintImmCleanInput = fun(I) -> print_imm_clean_input(I, Prefix, Print) end,
    lists:foreach(PrintImmCleanInput, ImmCExm),
    ok.

-spec print_imm_clean_input(imm_clean_input(), string(), output_fun()) -> 'ok'.
print_imm_clean_input({'$conjunction',SubImmCExms}, Prefix, Print) ->
    PrintSubImmCExm =
	fun({Tag,ImmCExm}) ->
	    Print(Prefix ++ "~w:~n", [Tag]),
	    print_imm_counterexample(ImmCExm, ">> " ++ Prefix, Print)
	end,
    lists:foreach(PrintSubImmCExm, SubImmCExms),
    ok;
print_imm_clean_input(Instance, Prefix, Print) ->
    Print(Prefix ++ "~w~n", [Instance]).

-spec execute_actions(fail_actions()) -> 'ok'.
execute_actions(Actions) ->
    lists:foreach(fun(A) -> ?FORCE(A) end, Actions),
    ok.

-spec report_shrinking(non_neg_integer(), imm_testcase(), fail_actions(),
		       output_fun()) -> 'ok'.
report_shrinking(Shrinks, MinImmTestCase, MinActions, Print) ->
    Print("(~b time(s))~n", [Shrinks]),
    print_imm_testcase(MinImmTestCase, "", Print),
    execute_actions(MinActions).


%%-----------------------------------------------------------------------------
%% Stats printing functions
%%-----------------------------------------------------------------------------

-spec apply_stats_printer(stats_printer(), sample(), output_fun()) -> 'ok'.
apply_stats_printer(Printer, SortedSample, Print) ->
    {arity,Arity} = erlang:fun_info(Printer, arity),
    case Arity of
	1 -> Printer(SortedSample);
	2 -> Printer(SortedSample, Print)
    end.

%% @doc A predefined function that accepts an atom or string and returns a
%% stats printing function which is equivalent to the default one, but prints
%% the given title `Title' above the statistics.
-spec with_title(title()) -> stats_printer().
with_title(Title) ->
    fun(S,O) -> plain_stats_printer(S, O, Title) end.

-spec plain_stats_printer(sample(), output_fun(), title()) -> 'ok'.
plain_stats_printer(SortedSample, Print, Title) ->
    print_title(Title, Print),
    Total = length(SortedSample),
    FreqSample = process_sorted_sample(SortedSample),
    lists:foreach(fun({X,F}) -> Print("~b\% ~w~n", [100 * F div Total,X]) end,
		  FreqSample).

-spec print_title(title(), output_fun()) -> 'ok'.
print_title(RawTitle, Print) ->
    Print("~n", []),
    Title = if
                is_atom(RawTitle) -> atom_to_list(RawTitle);
                is_list(RawTitle) -> RawTitle
	    end,
    case Title of
	"" -> ok;
	_  -> Print(Title ++ "~n", [])
    end.

-spec process_sorted_sample(sample()) -> freq_sample().
process_sorted_sample(SortedSample) ->
    Freqs = get_freqs(SortedSample, []),
    lists:reverse(lists:keysort(2, Freqs)).

-spec get_freqs(sample(), freq_sample()) -> freq_sample().
get_freqs([], Freqs) ->
    Freqs;
get_freqs([Term | Rest], Freqs) ->
    {Freq,Others} = remove_all(Term, 1, Rest),
    get_freqs(Others, [{Term,Freq} | Freqs]).

-spec remove_all(term(), frequency(), sample()) -> {frequency(), sample()}.
remove_all(X, Freq, [X | Rest]) ->
    remove_all(X, Freq + 1, Rest);
remove_all(_X, Freq, Sample) ->
    {Freq, Sample}.

-spec numeric_with_title(title()) -> stats_printer().
numeric_with_title(Title) ->
    fun(S,O) -> num_stats_printer(S, O, Title) end.

-spec num_stats_printer([number()], output_fun(), title()) -> 'ok'.
num_stats_printer(SortedSample, Print, Title) ->
    print_title(Title, Print),
    {Min,Avg,Max} = get_numeric_stats(SortedSample),
    Print("minimum: ~w~naverage: ~w~nmaximum: ~w~n", [Min,Avg,Max]).

-spec get_numeric_stats([number()]) -> numeric_stats().
get_numeric_stats([]) ->
    {undefined, undefined, undefined};
get_numeric_stats([Min | _Rest] = SortedSample) ->
    {Avg,Max} = avg_and_last(SortedSample, 0, 0),
    {Min, Avg, Max}.

-spec avg_and_last([number(),...], number(), non_neg_integer()) ->
	  {number(),number()}.
avg_and_last([Last], Sum, Len) ->
    {(Sum + Last) / (Len + 1), Last};
avg_and_last([X | Rest], Sum, Len) ->
    avg_and_last(Rest, Sum + X, Len + 1).
