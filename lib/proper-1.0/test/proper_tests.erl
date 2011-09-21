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
%%% @doc This modules contains PropEr's Unit tests. You need the EUnit
%%%      application to compile it.

-module(proper_tests).

-include("proper.hrl").

-include_lib("eunit/include/eunit.hrl").


%%------------------------------------------------------------------------------
%% Helper macros
%%------------------------------------------------------------------------------

%% NOTE: Never add long_result to Opts for these macros.

state_is_clean() ->
    get() =:= [].

assertEqualsOneOf(_X, none) ->
    ok;
assertEqualsOneOf(X, List) ->
    ?assert(lists:any(fun(Y) -> Y =:= X end, List)).

-define(_passes(Test),
	?_passes(Test, [])).

-define(_passes(Test, Opts),
	?_assertRun(true, Test, Opts, true)).

-define(_errorsOut(ExpReason, Test),
	?_errorsOut(ExpReason, Test, [])).

-define(_errorsOut(ExpReason, Test, Opts),
	?_assertRun({error,ExpReason}, Test, Opts, true)).

-define(_assertRun(ExpResult, Test, Opts, AlsoLongResult),
	?_test(begin
	    ?assertMatch(ExpResult, proper:quickcheck(Test,Opts)),
	    proper:clean_garbage(),
	    ?assert(state_is_clean()),
	    case AlsoLongResult of
		true ->
		    ?assertMatch(ExpResult,
				 proper:quickcheck(Test,[long_result|Opts])),
		    proper:clean_garbage(),
		    ?assert(state_is_clean());
		false ->
		    ok
	    end
	end)).

-define(_assertCheck(ExpShortResult, CExm, Test),
	?_assertCheck(ExpShortResult, CExm, Test, [])).

-define(_assertCheck(ExpShortResult, CExm, Test, Opts),
	?_test(?assertCheck(ExpShortResult, CExm, Test, Opts))).

-define(assertCheck(ExpShortResult, CExm, Test, Opts),
	begin
	    ?assertMatch(ExpShortResult, proper:check(Test,CExm,Opts)),
	    ?assert(state_is_clean())
	end).

-define(_fails(Test),
	?_fails(Test, [])).

-define(_fails(Test, Opts),
	?_failsWith(_, Test, Opts)).

-define(_failsWith(ExpCExm, Test),
	?_failsWith(ExpCExm, Test, [])).

-define(_failsWith(ExpCExm, Test, Opts),
	?_assertFailRun(ExpCExm, none, Test, Opts)).

-define(_failsWithOneOf(AllCExms, Test),
	?_failsWithOneOf(AllCExms, Test, [])).

-define(_failsWithOneOf(AllCExms, Test, Opts),
	?_assertFailRun(_, AllCExms, Test, Opts)).

-define(SHRINK_TEST_OPTS, [{start_size,10},{max_shrinks,10000}]).

-define(_shrinksTo(ExpShrunk, Type),
	?_assertFailRun([ExpShrunk], none, ?FORALL(_X,Type,false),
			?SHRINK_TEST_OPTS)).

-define(_shrinksToOneOf(AllShrunk, Type),
	?_assertFailRun(_, [[X] || X <- AllShrunk], ?FORALL(_X,Type,false),
			?SHRINK_TEST_OPTS)).

-define(_nativeShrinksTo(ExpShrunk, TypeStr),
	?_assertFailRun([ExpShrunk], none,
			?FORALL(_X,assert_can_translate(?MODULE,TypeStr),false),
			?SHRINK_TEST_OPTS)).

-define(_nativeShrinksToOneOf(AllShrunk, TypeStr),
	?_assertFailRun(_, [[X] || X <- AllShrunk],
			?FORALL(_X,assert_can_translate(?MODULE,TypeStr),false),
			?SHRINK_TEST_OPTS)).

-define(_assertFailRun(ExpCExm, AllCExms, Test, Opts),
	?_test(begin
	    ShortResult = proper:quickcheck(Test, Opts),
	    CExm1 = get_cexm(),
	    ?checkCExm(CExm1, ExpCExm, AllCExms, Test, Opts),
	    ?assertEqual(false, ShortResult),
	    LongResult = proper:quickcheck(Test, [long_result|Opts]),
	    CExm2 = get_cexm(),
	    ?checkCExm(CExm2, ExpCExm, AllCExms, Test, Opts),
	    ?checkCExm(LongResult, ExpCExm, AllCExms, Test, Opts)
	end)).

get_cexm() ->
    CExm = proper:counterexample(),
    proper:clean_garbage(),
    ?assert(state_is_clean()),
    CExm.

-define(checkCExm(CExm, ExpCExm, AllCExms, Test, Opts),
	begin
	    ?assertCheck(false, CExm, Test, Opts),
	    ?assertMatch(ExpCExm, CExm),
	    assertEqualsOneOf(CExm, AllCExms)
	end).

-define(_assertTempBecomesN(N, ExpShortResult, Prop),
	?_assertTempBecomesN(N, ExpShortResult, Prop, [])).

-define(_assertTempBecomesN(N, ExpShortResult, Prop, Opts),
	?_test(begin
	    ?assertMatch(ExpShortResult, proper:quickcheck(Prop,Opts)),
	    ?assertEqual(N, get_temp()),
	    erase_temp(),
	    proper:clean_garbage(),
	    ?assert(state_is_clean())
	end)).

inc_temp() ->
    inc_temp(1).

inc_temp(Inc) ->
    case get(temp) of
	undefined -> put(temp, Inc);
	X         -> put(temp, X + Inc)
    end,
    ok.

get_temp() ->
    get(temp).

erase_temp() ->
    erase(temp),
    ok.

non_deterministic(Behaviour) ->
    inc_temp(),
    N = get_temp(),
    {MustReset,Result} = get_result(N, 0, Behaviour),
    case MustReset of
	true  -> erase_temp();
	false -> ok
    end,
    Result.

get_result(N, Sum, [{M,Result}]) ->
    {N >= Sum + M, Result};
get_result(N, Sum, [{M,Result} | Rest]) ->
    NewSum = Sum + M,
    case N =< NewSum of
	true  -> {false, Result};
	false -> get_result(N, NewSum, Rest)
    end.

setup_run_commands(Module, Cmds, Env) ->
    Module:set_up(),
    Res = proper_statem:run_commands(Module, Cmds, Env),
    Module:clean_up(),
    Res.


%%------------------------------------------------------------------------------
%% Helper Functions
%%------------------------------------------------------------------------------

assert_type_works({Type,Are,_Target,Arent,TypeStr}, IsSimple) ->
    case Type of
	none ->
	    ok;
	_ ->
	    lists:foreach(fun(X) -> assert_is_instance(X,Type) end, Are),
	    assert_can_generate(Type, IsSimple),
	    lists:foreach(fun(X) -> assert_not_is_instance(X,Type) end, Arent)
    end,
    case TypeStr of
	none ->
	    ok;
	_ ->
	    TransType = assert_can_translate(?MODULE, TypeStr),
	    lists:foreach(fun(X) -> assert_is_instance(X,TransType) end, Are),
	    assert_can_generate(TransType, IsSimple),
	    lists:foreach(fun(X) -> assert_not_is_instance(X,TransType) end,
			  Arent)
    end.

assert_can_translate(Mod, TypeStr) ->
    proper_typeserver:start(),
    Type = {Mod,TypeStr},
    Result1 = proper_typeserver:translate_type(Type),
    Result2 = proper_typeserver:translate_type(Type),
    proper_typeserver:stop(),
    ?assert(state_is_clean()),
    {ok,Type1} = Result1,
    {ok,Type2} = Result2,
    ?assert(proper_types:equal_types(Type1,Type2)),
    Type1.

assert_cant_translate(Mod, TypeStr) ->
    proper_typeserver:start(),
    Result = proper_typeserver:translate_type({Mod,TypeStr}),
    proper_typeserver:stop(),
    ?assert(state_is_clean()),
    ?assertMatch({error,_}, Result).

%% TODO: after fixing the typesystem, use generic reverse function.
assert_is_instance(X, Type) ->
    ?assert(proper_types:is_inst(X, Type) andalso state_is_clean()).

assert_can_generate(Type, CheckIsInstance) ->
    lists:foreach(fun(Size) -> try_generate(Type,Size,CheckIsInstance) end,
		  [1,2,5,10,20,40,50]).

try_generate(Type, Size, CheckIsInstance) ->
    {ok,Instance} = proper_gen:pick(Type, Size),
    ?assert(state_is_clean()),
    case CheckIsInstance of
	true  -> assert_is_instance(Instance, Type);
	false -> ok
    end.

assert_native_can_generate(Mod, TypeStr, CheckIsInstance) ->
    assert_can_generate(assert_can_translate(Mod,TypeStr), CheckIsInstance).

assert_cant_generate(Type) ->
    ?assertEqual(error, proper_gen:pick(Type)),
    ?assert(state_is_clean()).

assert_cant_generate_cmds(Type, N) ->
    ?assertEqual(error, proper_gen:pick(?SUCHTHAT(T, Type, length(T) > N))),
    ?assert(state_is_clean()).

assert_not_is_instance(X, Type) ->
    ?assert(not proper_types:is_inst(X, Type) andalso state_is_clean()).

assert_function_type_works(FunType) ->
    {ok,F} = proper_gen:pick(FunType),
    %% TODO: this isn't exception-safe
    ?assert(proper_types:is_instance(F, FunType)),
    assert_is_pure_function(F),
    proper:global_state_erase(),
    ?assert(state_is_clean()).

assert_is_pure_function(F) ->
    {arity,Arity} = erlang:fun_info(F, arity),
    ArgsList = [lists:duplicate(Arity,0), lists:duplicate(Arity,1),
		lists:seq(1,Arity), lists:seq(0,Arity-1)],
    lists:foreach(fun(Args) -> ?assertEqual(apply(F,Args),apply(F,Args)) end,
		  ArgsList).


%%------------------------------------------------------------------------------
%% Unit test arguments
%%------------------------------------------------------------------------------

simple_types_with_data() ->
    [{integer(), [-1,0,1,42,-200], 0, [0.3,someatom,<<1>>], "integer()"},
     {integer(7,88), [7,8,87,88,23], 7, [1,90,a], "7..88"},
     {integer(0,42), [0,11,42], 0, [-1,43], "0..42"},
     {integer(-99,0), [-88,-99,0], 0, [1,-1112], "-99..0"},
     {integer(-999,-12), [-34,-999,-12], -12, [0,5], "-999..-12"},
     {integer(-99,21), [-98,0,21], 0, [-100], "-99..21"},
     {integer(0,0), [0], 0, [1,-1,100,-100], "0..0"},
     {pos_integer(), [12,1,444], 1, [-12,0], "pos_integer()"},
     {non_neg_integer(), [42,0], 0, [-9,rr], "non_neg_integer()"},
     {neg_integer(), [-222,-1], -1, [0,1111], "neg_integer()"},
     {float(), [17.65,-1.12], 0.0, [11,atomm,<<>>], "float()"},
     {float(7.4,88.0), [7.4,88.0], 7.4, [-1.0,3.2], none},
     {float(0.0,42.1), [0.1,42.1], 0.0, [-0.1], none},
     {float(-99.9,0.0), [-0.01,-90.0], 0.0, [someatom,-12,-100.0,0.1], none},
     {float(-999.08,-12.12), [-12.12,-12.2], -12.12, [-1111.0,1000.0], none},
     {float(-71.8,99.0), [-71.8,99.0,0.0,11.1], 0.0, [100.0,-71.9], none},
     {float(0.0,0.0), [0.0], 0.0, [0.1,-0.1], none},
     {non_neg_float(), [88.8,98.9,0.0], 0.0, [-12,1,-0.01], none},
     {atom(), [elvis,'Another Atom',''], '', ["not_an_atom",12,12.2], "atom()"},
     {binary(), [<<>>,<<12,21>>], <<>>, [<<1,2:3>>,binary_atom,42], "binary()"},
     {binary(3), [<<41,42,43>>], <<0,0,0>>, [<<1,2,3,4>>], "<<_:3>>"},
     {binary(0), [<<>>], <<>>, [<<1>>], none},
     {bitstring(), [<<>>,<<87,76,65,5:4>>], <<>>, [{12,3},11], "bitstring()"},
     {bitstring(18), [<<0,1,2:2>>,<<1,32,123:2>>], <<0,0,0:2>>, [<<12,1,1:3>>],
      "<<_:18, _:_*1>>"},
     {bitstring(32), [<<120,120,120,120>>], <<0,0,0,0>>, [7,8],
      "<<_:32, _:_*1>>"},
     {bitstring(0), [<<>>], <<>>, [<<1>>], none},
     {list(integer()), [[],[2,42],[0,1,1,2,3,5,8,13,21,34,55,89,144]], [],
      [[4,4.2],{12,1},<<12,113>>], "[integer()]"},
     {list(atom()), [[on,the,third,day,'of',christmas,my,true,love,sent,to,me]],
      [], [['not',1,list,'of',atoms],not_a_list], "[atom()]"},
     {list(union([integer(),atom()])), [[3,french,hens,2],[turtle,doves]], [],
      [{'and',1}], "[integer() | atom()]"},
     {vector(5,atom()), [[partridge,in,a,pear,tree],[a,b,c,d,e]],
      ['','','','',''], [[a,b,c,d],[a,b,c,d,e,f]], none},
     {vector(2,float()), [[0.0,1.1],[4.4,-5.5]], [0.0,0.0], [[1,1]], none},
     {vector(0,integer()), [[]], [], [[1],[2]], none},
     {union([good,bad,ugly]), [good,bad,ugly], good, [clint,"eastwood"],
      "good | bad | ugly"},
     {union([integer(),atom()]), [twenty_one,21], 0, ["21",<<21>>],
      "integer() | atom()"},
     {weighted_union([{10,luck},{20,skill},{15,concentrated_power_of_will},
		      {5,pleasure},{50,pain},{100,remember_the_name}]),
      [skill,pain,pleasure], luck, [clear,20,50], none},
     {{integer(0,42),list(atom())}, [{42,[a,b]},{21,[c,de,f]},{0,[]}], {0,[]},
      [{-1,[a]},{12},{21,[b,c],12}], "{0..42,[atom()]}"},
     {tuple([atom(),integer()]), [{the,1}], {'',0}, [{"a",0.0}],
      "{atom(),integer()}"},
     {{}, [{}], {}, [[],{1,2}], "{}"},
     {loose_tuple(integer()), [{1,44,-1},{},{99,-99}], {}, [4,{hello,2},[1,2]],
      none},
     {loose_tuple(union([atom(),float()])), [{a,4.4,b},{},{'',c},{1.2,-3.4}],
      {}, [an_atom,0.4,{hello,2},[aa,bb,3.1]], none},
     {loose_tuple(list(integer())), [{[1,-1],[],[2,3,-12]},{}], {},
      [[[1,2],[3,4]],{1,12},{[1,99,0.0],[]}], none},
     {loose_tuple(loose_tuple(integer())), [{},{{}},{{1,2},{-1,11},{}}], {},
      [{123},[{12},{24}]], none},
     {exactly({[writing],unit,[tests,is],{2},boring}),
      [{[writing],unit,[tests,is],{2},boring}],
      {[writing],unit,[tests,is],{2},boring}, [no,its,'not','!'], none},
     {[], [[]], [], [[a],[1,2,3]], "[]"},
     {fixed_list([neg_integer(),pos_integer()]), [[-12,32],[-1,1]], [-1,1],
      [[0,0]], none},
     {[atom(),integer(),atom(),float()], [[forty_two,42,forty_two,42.0]],
      ['',0,'',0.0], [[proper,is,licensed],[under,the,gpl]], none},
     {[42 | list(integer())], [[42],[42,44,22]], [42], [[],[11,12]], none},
     {number(), [12,32.3,-9,-77.7], 0, [manolis,papadakis], "number()"},
     {boolean(), [true,false], false, [unknown], "boolean()"},
     {string(), ["hello","","world"], "", ['hello'], "string()"},
     {?LAZY(integer()), [0,2,99], 0, [1.1], "integer()"},
     {?LAZY(list(float())), [[0.0,1.2,1.99],[]], [], [1.1,[1,2]], "[float()]"},
     {zerostream(10), [[0,0,0],[],[0,0,0,0,0,0,0]], [], [[1,0,0],[0.1]], none},
     {?SHRINK(pos_integer(),[0]), [1,12,0], 0, [-1,-9,6.0], none},
     {?SHRINK(float(),[integer(),atom()]), [1.0,0.0,someatom,'',42,0], 0,
      [<<>>,"hello"], none},
     {noshrink(?SHRINK(42,[0,1])), [42,0,1], 42, [-1], "42 | 0 | 1"},
     {non_empty(list(integer())), [[1,2,3],[3,42],[11]], [0], [[],[0.1]],
      "[integer(),...]"},
     {default(42,float()), [4.1,-99.0,0.0,42], 42, [43,44], "42 | float()"},
     {?SUCHTHAT(X,non_neg_integer(),X rem 4 =:= 1), [1,5,37,89], 1, [4,-12,11],
      none},
     {?SUCHTHATMAYBE(X,non_neg_integer(),X rem 4 =:= 1), [1,2,3,4,5,37,89], 0,
      [1.1,2.2,-12], "non_neg_integer()"},
     {any(), [1,-12,0,99.9,-42.2,0.0,an_atom,'',<<>>,<<1,2>>,<<1,2,3:5>>,[],
	      [42,<<>>],{},{tag,12},{tag,[vals,12,12.2],[],<<>>}],
	     0, [], "any()"},
     {list(any()), [[<<>>,a,1,-42.0,{11.8,[]}]], [], [{1,aa},<<>>], "[any()]"},
     {deeplist(), [[[],[]], [[[]],[]]], [], [[a]], "deeplist()"},
     {none, [[234,<<1>>,[<<78>>,[]],0],[]], [], [21,3.1,[7.1],<<22>>],
      "iolist()"},
     {none, [[234,<<1>>,[<<78>>,[]],0],[],<<21,15>>], <<>>, [21,3.1,[7.1]],
      "iodata()"}].

%% TODO: These rely on the intermediate form of the instances.
constructed_types_with_data() ->
    [{?LET(X,range(1,5),X*X), [{'$used',1,1},{'$used',5,25}], 1,
      [4,{'$used',3,8},{'$used',0,0}], none},
     {?LET(L,non_empty(list(atom())),oneof(L)),
      [{'$used',[aa],aa},{'$used',[aa,bb],aa},{'$used',[aa,bb],bb}], '',
      [{'$used',[],''},{'$used',[aa,bb],cc}], none},
     {?LET(X,pos_integer(),?LET(Y,range(0,X),X-Y)),
      [{'$used',3,{'$used',2,1}},{'$used',9,{'$used',9,0}},
       {'$used',5,{'$used',0,5}}], 1,
      [{'$used',0,{'$used',0,0}},{'$used',3,{'$used',4,-1}},
       {'$used',7,{'$used',6,2}}], none},
     {?LET(Y,?LET(X,integer(),X*X),-Y),
      [{'$used',{'$used',-9,81},-81},{'$used',{'$used',2,4},-4}], 0,
      [{'$used',{'$used',1,2},-2},{'$used',{'$used',3,9},9}], none},
     {?SUCHTHAT(Y,?LET(X,oneof([1,2,3]),X+X),Y>3),
      [{'$used',2,4},{'$used',3,6}], 4, [{'$used',1,2}], none},
     {?LET(X,?SUCHTHAT(Y,pos_integer(),Y=/=0),X*X),
      [{'$used',3,9},{'$used',1,1},{'$used',11,121}], 1,
      [{'$used',-1,1},{'$used',0,0}], none},
     {tree(integer()), [{'$used',[null,null],{node,42,null,null}},
			{'$used',[{'$used',[null,null],{node,2,null,null}},
				  {'$used',[null,null],{node,3,null,null}}],
			 {node,-1,{node,2,null,null},{node,3,null,null}}},
			 {'$to_part',null},
			 {'$to_part',{'$used',[null,null],{node,7,null,null}}}],
      null, [{'$used',[null,null],{node,1.1,null,null}}], "tree(integer())"},
     {?LETSHRINK(L,[],{tag,L}), [{'$used',[],{tag,[]}}], {tag,[]}, [], none},
     {?LETSHRINK(L,non_empty(list(atom())),{tag,L}),
      [{'$used',[aa],{tag,[aa]}},{'$to_part',aa}], '', [], none},
     {a(), [aleaf, {'$used',[aleaf],{anode,aleaf,bleaf}},
	    {'$used',[aleaf],{anode,aleaf,{'$to_part',bleaf}}}],
      aleaf, [], "a()"},
     {b(), [bleaf, {'$used',[bleaf],{bnode,aleaf,bleaf}},
	    {'$used',[bleaf],{bnode,{'$to_part',aleaf},bleaf}}],
      bleaf, [], "b()"},
     {gen_tree(integer()),
      [{'$used',[null,null],{12,[null,null]}},{'$to_part',null}],
      null, [{'$used',[],{42,[]}}], "gen_tree(integer())"},
     {none, [{'$used',[],{tag,[]}}, {'$used',[null,null],{tag,[null,null]}},
	     {'$used',[{'$used',[],{tag,[]}},{'$to_part',null}],
	      {tag,[{tag,[]},null]}}, {'$to_part',{'$used',[],{tag,[]}}}],
      null, [], "g()"},
     {none, [{'$used',[null],{tag,[{ok,null}]}}, {'$to_part',null},
	     {'$used',[null,null],{tag,[{ok,null},{ok,null}]}}],
      null, [], "h()"},
     {none, [{'$used',[null,null,{'$used',[null],{tag,null,[]}}],
	      {tag,null,[null,{tag,null,[]}]}}, {'$to_part',null}],
      null, [], "i()"},
     {none, [{'$used',[{'$to_part',null},{'$used',[null],{one,null}},null,null],
	      {tag,null,{one,null},[null,null],[null]}}], null, [], "j()"},
     {none, [{tag,[]}, {tag,[{null,null}]},
	     {tag,[{{tag,[]},null},{null,{tag,[]}}]}],
      null, [{'$to_part',null}], "k()"},
     {none, [{'$used',[null,null,{'$used',[null,null],{tag,null,[null]}}],
	      {tag,null,[null,{tag,null,[null]}]}}, {'$to_part',null}],
      null, [{'$used',[null],{tag,null,[]}}], "l()"}].

function_types() ->
    [{function([],atom()), "fun(() -> atom())"},
     {function([integer(),integer()],atom()),
      "fun((integer(),integer()) -> atom())"},
     {function(5,union([a,b])), "fun((_,_,_,_,_) -> a | b)"},
     {function(0,function(1,integer())),
      "fun(() -> fun((_) -> integer()))"}].

remote_native_types() ->
    [{types_test1,["#rec1{}","rec1()","exp1()","type1()","type2(atom())",
		   "rem1()","rem2()","types_test1:exp1()",
		   "types_test2:exp1(float())","types_test2:exp2()"]},
     {types_test2,["exp1(#rec1{})","exp2()","#rec1{}","types_test1:exp1()",
		   "types_test2:exp1(binary())","types_test2:exp2()"]}].

impossible_types() ->
    [?SUCHTHAT(X, pos_integer(), X =< 0),
     ?SUCHTHAT(X, non_neg_integer(), X < 0),
     ?SUCHTHAT(X, neg_integer(), X >= 0),
     ?SUCHTHAT(X, integer(1,10), X > 20),
     ?SUCHTHAT(X, float(0.0,10.0), X < 0.0),
     ?SUCHTHAT(L, vector(12,integer()), length(L) =/= 12),
     ?SUCHTHAT(B, binary(), lists:member(256,binary_to_list(B))),
     ?SUCHTHAT(X, exactly('Lelouch'), X =:= 'vi Brittania')].

impossible_native_types() ->
    [{types_test1, ["1.1","no_such_module:type1()","no_such_type()"]},
     {types_test2, ["types_test1:type1()","function()","fun((...) -> atom())",
		    "pid()","port()","ref()"]}].

recursive_native_types() ->
    [{rec_test1, ["a()","b()","a()|b()","d()","f()","deeplist()",
		  "mylist(float())","aa()","bb()","expc()"]},
     {rec_test2, ["a()","expa()","rec()"]}].

impossible_recursive_native_types() ->
    [{rec_test1, ["c()","e()","cc()","#rec{}","expb()"]},
     {rec_test2, ["b()","#rec{}","aa()"]}].

symb_calls() ->
    [{[3,2,1], "lists:reverse([1,2,3])", [], {call,lists,reverse,[[1,2,3]]}},
     {[a,b,c,d], "erlang:'++'([a,b],[c,d])",
      [{a,some_value}], {call,erlang,'++',[[a,b],[c,d]]}},
     {42, "erlang:'*'(erlang:'+'(3,3),erlang:'-'(8,1))",
      [{b,dummy_value},{e,another_dummy}],
      {call,erlang,'*',[{call,erlang,'+',[3,3]},{call,erlang,'-',[8,1]}]}},
     {something, "something",
      [{a,somebody},{b,put},{c,something},{d,in_my_drink}], {var,c}},
     {{var,b}, "{var,b}", [{a,not_this},{c,neither_this}], {var,b}},
     {42, "erlang:'+'(40,2)", [{m,40},{n,2}],
      {call,erlang,'+',[{var,m},{var,n}]}},
     {[i,am,{var,iron},man],
      "erlang:'++'(lists:reverse([am,i]),erlang:'++'([{var,iron}],[man]))",
      [{a,man},{b,woman}],
      {call,erlang,'++',[{call,lists,reverse,[[am,i]]},
			 {call,erlang,'++',[[{var,iron}],[{var,a}]]}]}}].

undefined_symb_calls() ->
    [{call,erlang,error,[an_error]},
     {call,erlang,throw,[a_throw]},
     {call,erlang,exit,[an_exit]},
     {call,lists,reverse,[<<12,13>>]},
     {call,erlang,'+',[1,2,3]}].

combinations() ->
    [{[{1,[1,3,5,7,9,10]}, {2,[2,4,6,8,11]}], 5, 11, [1,2,3,4,5,6,7,8,9,10,11], 2, 2,
      [{1,[1,3,5,7,8,11]}, {2,[2,4,6,9,10]}]},
     {[{1,[1,3,5]}, {2,[7,8,9]}, {3,[2,4,6]}], 3, 9, [1,3,5,7,8,9], 3, 2,
      [{1,[6,8,9]}, {2,[1,3,5]}, {3,[2,4,7]}]}].

first_comb() -> [{10,3,3,[{1,[7,8,9,10]}, {2,[4,5,6]}, {3,[1,2,3]}]},
		 {11,5,2,[{1,[6,7,8,9,10,11]}, {2,[1,2,3,4,5]}]},
		 {12,3,4,[{1,[10,11,12]}, {2,[7,8,9]}, {3,[4,5,6]}, {4,[1,2,3]}]}].

lists_to_zip() ->
    [{[],[],[]},
     {[], [dummy, atom], []},
     {[1, 42, 1, 42, 1, 2 ,3], [], []},
     {[a, b, c], lists:seq(1,6), [{a,1}, {b,2}, {c,3}]},
     {[a, b, c], lists:seq(1,3), [{a,1}, {b,2}, {c,3}]},
     {[a, d, d, d, d], lists:seq(1,3), [{a,1}, {d,2}, {d,3}]}].

command_names() ->
    [{[{set,{var,1},{call,erlang,put,[a,0]}},
       {set,{var,3},{call,erlang,erase,[a]}},
       {set,{var,4},{call,erlang,get,[b]}}],
      [{erlang,put,2},
       {erlang,erase,1},
       {erlang,get,1}]},
     {[{set,{var,1},{call,foo,bar,[]}},
       {set,{var,2},{call,bar,foo,[a,{var,1}]}},
       {set,{var,3},{call,bar,foo,[a,[[3,4]]]}}],
      [{foo,bar,0},
       {bar,foo,2},
       {bar,foo,2}]},
     {[],[]}].

valid_command_sequences() ->
    %% {module, initial_state, command_sequence, symbolic_state_after,
    %%  dynamic_state_after,initial_environment}
    [{pdict_statem, [], [{init,[]},
			 {set,{var,1},{call,erlang,put,[a,0]}},
			 {set,{var,2},{call,erlang,put,[b,1]}},
			 {set,{var,3},{call,erlang,erase,[a]}},
			 {set,{var,4},{call,erlang,get,[b]}},
			 {set,{var,5},{call,erlang,erase,[b]}},
			 {set,{var,6},{call,erlang,put,[a,4]}},
			 {set,{var,7},{call,erlang,put,[a,42]}}],
      [{a,42}], [{a,42}], []},
     {pdict_statem, [], [{init,[]},
			 {set,{var,1},{call,erlang,put,[b,5]}},
			 {set,{var,2},{call,erlang,erase,[b]}},
			 {set,{var,3},{call,erlang,put,[a,5]}}],
      [{a,5}], [{a,5}], []},
     {pdict_statem, [], [{init,[]},
			 {set,{var,1},{call,erlang,put,[a,{var,start_value}]}},
			 {set,{var,2},{call,erlang,put,[b,{var,another_start_value}]}},
			 {set,{var,3},{call,erlang,get,[b]}},
			 {set,{var,4},{call,erlang,get,[b]}}],
      [{b,{var,another_start_value}}, {a,{var,start_value}}], [{b,-1}, {a, 0}],
      [{start_value, 0}, {another_start_value, -1}]}].

symbolic_init_invalid_sequences() ->
    %% {module, command_sequence, environment, shrunk}
    [{pdict_statem, [{init,[{a,{call,foo,bar,[some_arg]}}]},
		     {set,{var,1},{call,erlang,put,[b,42]}},
		     {set,{var,2},{call,erlang,get,[b]}}],
      [{some_arg, 0}],
      [{init,[{a,{call,foo,bar,[some_arg]}}]}]}].

invalid_precondition() ->
    %% {module, command_sequence, environment, shrunk}
    [{pdict_statem, [{init,[]},
		      {set,{var,1},{call,erlang,put,[a,0]}},
		      {set,{var,2},{call,erlang,put,[b,1]}},
		      {set,{var,3},{call,erlang,erase,[a]}},
		      {set,{var,4},{call,erlang,get,[a]}}],
       [], [{set,{var,4},{call,erlang,get,[a]}}]}].

invalid_var() ->
    [{pdict_statem, [{init,[]},
		     {set,{var,2},{call,erlang,put,[b,{var,1}]}}]},
     {pdict_statem, [{init,[]},
		     {set,{var,1},{call,erlang,put,[b,9]}},
		     {set,{var,5},{call,erlang,put,[a,3]}},
		     {set,{var,6},{call,erlang,get,[{var,2}]}}]}].

arguments_not_defined() ->
    [{[simple,atoms,are,valid,{var,42}], []},
     {[{var,1}], [{var,2},{var,3},{var,4}]},
     {[hello,world,[hello,world,{var,6}]], []},
     {[{1,2,3,{var,1},{var,2}},not_really], []},
     {[[[[42,{var,42}]]]], []},
     {[{43,41,{1,{var,42}}},why_not], []}].

all_data() ->
    [1, 42.0, "$hello", "world\n", [smelly, cat, {smells,bad}],
     '$this_should_be_copied', '$this_one_too', 'but$ this$ not',
     or_this].

dollar_data() ->
    ['$this_should_be_copied', '$this_one_too'].

%%------------------------------------------------------------------------------
%% Unit tests
%%------------------------------------------------------------------------------

%% TODO: write tests for old datatypes, use old tests
%% TODO: check output redirection, quiet, verbose, to_file, on_output/2 (maybe
%%       by writing to a string in the process dictionary), statistics printing,
%%	 standard verbose behaviour
%% TODO: fix compiler warnings
%% TODO: LET and LETSHRINK testing (these need their intermediate form for
%%	 standalone instance testing and shrinking) - update needed after
%%	 fixing the internal shrinking in LETs, use recursive datatypes, like
%%	 trees, for testing, also test with noshrink and LAZY
%% TODO: use size=100 for is_instance testing?
%% TODO: typeserver: check that the same type is returned for consecutive calls,
%%	 even with no caching (no_caching option?)
%% TODO: typeserver: recursive types containing functions
%% TODO: ?LET, ?LETSHRINK: only the top-level base type can be a native type
%% TODO: Test with native types: ?SUCHTHATMAYBE, noshrink, ?LAZY, ?SHRINK,
%%	 resize, ?SIZED
%% TODO: no debug_info at compile time => call, not type
%%	 no debug_info at runtime => won't find type
%%	 no module in code path at runtime => won't find type
%% TODO: try some more expressions with a ?FORALL underneath
%% TODO: various constructors like '|' (+ record notation) are parser-rejected
%% TODO: test nonempty recursive lists
%% TODO: test list-recursive with instances
%% TODO: more ADT tests: check bad declarations, bad variable use, multi-clause,
%%	 is_subtype, unacceptable range, unexported opaque, no-specs opaque,
%%	 unexported/unspecced functions, unbound variables, check as constructed
%% TODO: module, check_spec, check_module_specs, retest_spec (long result mode
%%	 too, other options pass)
%% TODO: proper_typeserver:is_instance (with existing types too, plus types we
%%	 can't produce, such as impropers) (also check that everything we
%%	 produce based on a type is an instance)
%% TODO: check that functions that throw exceptions pass
%% TODO: property inside a ?TIMEOUT returning false
%% TODO: some branch of a ?FORALL has a collect while another doesn't
%% TODO: symbolic functions returning functions are evaluated?
%% TODO: pure_check
%% TODO: spec_timeout option
%% TODO: defined option precedence
%% TODO: conversion of maybe_improper_list
%% TODO: use demo_is_instance and demo_translate_type
%% TODO: debug option to output tests passed, fail reason, etc.


simple_types_test_() ->
    [?_test(assert_type_works(TD, true)) || TD <- simple_types_with_data()].

constructed_types_test_() ->
    [?_test(assert_type_works(TD, false))
     || TD <- constructed_types_with_data()].

%% TODO: specific test-starting instances would be useful here
%%	 (start from valid Xs)
shrinks_to_test_() ->
    [?_shrinksTo(Target, Type)
     || {Type,_Xs,Target,_Ys,_TypeStr} <- simple_types_with_data()
					  ++ constructed_types_with_data(),
	Type =/= none].

native_shrinks_to_test_() ->
    [?_nativeShrinksTo(Target, TypeStr)
     || {_Type,_Xs,Target,_Ys,TypeStr} <- simple_types_with_data()
					  ++ constructed_types_with_data(),
	TypeStr =/= none].

cant_generate_test_() ->
    [?_test(assert_cant_generate(Type)) || Type <- impossible_types()].

native_cant_translate_test_() ->
    [?_test(assert_cant_translate(Mod,TypeStr))
     || {Mod,Strings} <- impossible_native_types(), TypeStr <- Strings].

remote_native_types_test_() ->
    [?_test(assert_can_translate(Mod,TypeStr))
     || {Mod,Strings} <- remote_native_types(), TypeStr <- Strings].

recursive_native_types_test_() ->
    [?_test(assert_native_can_generate(Mod,TypeStr,false))
     || {Mod,Strings} <- recursive_native_types(), TypeStr <- Strings].

recursive_native_cant_translate_test_() ->
    [?_test(assert_cant_translate(Mod,TypeStr))
     || {Mod,Strings} <- impossible_recursive_native_types(),
	TypeStr <- Strings].

random_functions_test_() ->
    [[?_test(assert_function_type_works(FunType)),
      ?_test(assert_function_type_works(assert_can_translate(proper,TypeStr)))]
     || {FunType,TypeStr} <- function_types()].

parse_transform_test_() ->
    [?_passes(auto_export_test1:prop_1()),
     ?_assertError(undef, auto_export_test2:prop_1()),
     ?_assertError(undef, no_native_parse_test:prop_1()),
     ?_assertError(undef, no_out_of_forall_test:prop_1())].

native_type_props_test_() ->
    [?_passes(?FORALL({X,Y}, {my_native_type(),my_proper_type()},
		      is_integer(X) andalso is_atom(Y))),
     ?_passes(?FORALL([X,Y,Z],
		      [my_native_type(),my_proper_type(),my_native_type()],
		      is_integer(X) andalso is_atom(Y) andalso is_integer(Z))),
     ?_passes(?FORALL([Y,X,{Z,W}],
		      [my_proper_type() | [my_native_type()]] ++
		      [{my_native_type(),my_proper_type()}],
		      is_integer(X) andalso is_atom(Y) andalso is_integer(Z)
		      andalso is_atom(W))),
     ?_passes(?FORALL([X|Y], [my_native_type()|my_native_type()],
		      is_integer(X) andalso is_integer(Y))),
     ?_passes(?FORALL(X, type_and_fun(), is_atom(X))),
     ?_passes(?FORALL(X, type_only(), is_integer(X))),
     ?_passes(?FORALL(L, [integer()], length(L) =:= 1)),
     ?_fails(?FORALL(L, id([integer()]), length(L) =:= 1)),
     ?_passes(?FORALL(_, types_test1:exp1(), true)),
     ?_assertError(undef, ?FORALL(_,types_test1:rec1(),true)),
     ?_assertError(undef, ?FORALL(_,no_such_module:some_call(),true)),
     {setup, fun() -> code:purge(to_remove),
		      code:delete(to_remove),
		      code:purge(to_remove),
		      file:rename("tests/to_remove.beam",
				  "tests/to_remove.bak") end,
	     fun(_) -> file:rename("tests/to_remove.bak",
				   "tests/to_remove.beam") end,
	     ?_passes(?FORALL(_, to_remove:exp1(), true))},
     ?_passes(rec_props_test1:prop_1()),
     ?_passes(rec_props_test2:prop_2()),
     ?_passes(?FORALL(L, vector(2,my_native_type()),
		      length(L) =:= 2
		      andalso lists:all(fun erlang:is_integer/1, L))),
     ?_passes(?FORALL(F, function(0,my_native_type()), is_integer(F()))),
     ?_passes(?FORALL(X, union([my_proper_type(),my_native_type()]),
		      is_integer(X) orelse is_atom(X))),
     ?_assertError(undef, begin
			    Vector5 = fun(T) -> vector(5,T) end,
			    ?FORALL(V, Vector5(types_test1:exp1()),
				    length(V) =:= 5)
			end),
     ?_passes(?FORALL(X, ?SUCHTHAT(Y,types_test1:exp1(),is_atom(Y)),
		      is_atom(X))),
     ?_passes(?FORALL(L,non_empty(lof()),length(L) > 0)),
     ?_passes(?FORALL(X, ?LET(L,lof(),lists:min([99999.9|L])),
		      is_float(X))),
     ?_shrinksTo(0, ?LETSHRINK([X],[my_native_type()],{'tag',X})),
     ?_passes(weird_types:prop_export_all_works()),
     ?_passes(weird_types:prop_no_auto_import_works())].

-record(untyped, {a, b = 12}).
-type untyped() :: #untyped{}.

true_props_test_() ->
    [?_passes(?FORALL(X,integer(),X < X + 1)),
     ?_passes(?FORALL(X,atom(),list_to_atom(atom_to_list(X)) =:= X)),
     ?_passes(?FORALL(L,list(integer()),is_sorted(L,quicksort(L)))),
     ?_passes(?FORALL(L,ulist(integer()),is_sorted(L,lists:usort(L)))),
     ?_passes(?FORALL(L,non_empty(list(integer())),L =/= [])),
     ?_passes(?FORALL({I,L}, {integer(),list(integer())},
		      ?IMPLIES(no_duplicates(L),
			       not lists:member(I,lists:delete(I,L))))),
     ?_passes(?FORALL(L, ?SIZED(Size,resize(Size div 5,list(integer()))),
		      length(L) =< 20), [{max_size,100}]),
     %% TODO: check that the samples are collected correctly
     ?_passes(?FORALL(L, list(integer()),
		      collect(length(L), collect(L =:= [],
			      lists:reverse(lists:reverse(L)) =:= L)))),
     ?_passes(?FORALL(L, list(integer()),
		      aggregate(smaller_lengths_than_my_own(L), true))),
     ?_assertTempBecomesN(300, true,
			  numtests(300,?FORALL(_,1,begin inc_temp(),true end))),
     ?_assertTempBecomesN(30, true, ?FORALL(X, ?SIZED(Size,Size),
					    begin inc_temp(X),true end),
			  [{numtests,12},{max_size,4}]),
     ?_assertTempBecomesN(12, true,
			  ?FORALL(X, ?SIZED(Size,Size),
				  begin inc_temp(X),true end),
			  [{numtests,3},{start_size,4},{max_size,4}]),
     ?_passes(?FORALL(X, integer(), ?IMPLIES(abs(X) > 1, X * X > X))),
     ?_passes(?FORALL(X, integer(), ?IMPLIES(X >= 0, true))),
     ?_passes(?FORALL({X,Lim}, {int(),?SIZED(Size,Size)}, abs(X) =< Lim)),
     ?_passes(?FORALL({X,Lim}, {nat(),?SIZED(Size,Size)}, X =< Lim)),
     ?_passes(?FORALL(L, orderedlist(integer()), is_sorted(L))),
     ?_passes(conjunction([
		  {one, ?FORALL(_, integer(), true)},
		  {two, ?FORALL(X, integer(), collect(X > 0, true))},
		  {three, conjunction([{a,true},{b,true}])}
	      ])),
     ?_passes(?FORALL(X, untyped(), is_record(X, untyped))),
     ?_passes(pdict_statem:prop_pdict()),
     ?_passes(symb_statem:prop_simple()),
     {timeout, 20, ?_passes(symb_statem:prop_parallel_simple())},
     {timeout, 10, ?_passes(ets_statem:prop_ets())},
     {timeout, 20, ?_passes(ets_statem:prop_parallel_ets())},
     {timeout, 20, ?_passes(pdict_fsm:prop_pdict())}].

false_props_test_() ->
    [?_failsWith([[_Same,_Same]],
		 ?FORALL(L,list(integer()),is_sorted(L,lists:usort(L)))),
     ?_failsWith([[_Same,_Same],_Same],
		 ?FORALL(L, non_empty(list(union([a,b,c,d]))),
			 ?FORALL(X, elements(L),
				 not lists:member(X,lists:delete(X,L))))),
     ?_failsWith(['\000\000\000\000'],
		 ?FORALL(A, atom(), length(atom_to_list(A)) < 4)),
     %% TODO: check that these only run once
     ?_failsWith([1], ?FORALL(X, non_neg_integer(),
			      case X > 0 of
				  true  -> throw(not_zero);
				  false -> true
			      end)),
     ?_fails(?FORALL(_,1,lists:min([]) > 0)),
     ?_failsWith([[12,42]], ?FORALL(L, [12,42|list(integer())],
				    case lists:member(42, L) of
					true  -> erlang:exit(you_got_it);
					false -> true
				    end)),
     ?_fails(?FORALL(_, integer(), ?TIMEOUT(100,timer:sleep(150) =:= ok))),
     ?_failsWith([20], ?FORALL(X, pos_integer(), ?TRAPEXIT(creator(X) =:= ok))),
     ?_assertTempBecomesN(7, false,
			  ?FORALL(X, ?SIZED(Size,integer(Size,Size)),
				  begin inc_temp(), X < 5 end),
			  [{numtests,5}, {max_size,5}]),
     %% it runs 2 more times: one while shrinking (recursing into the property)
     %% and one when the minimal input is rechecked
     ?_assertTempBecomesN(2, false,
			  ?FORALL(L, list(atom()),
				  ?WHENFAIL(inc_temp(),length(L) < 5))),
     ?_assertTempBecomesN(3, false,
			  ?FORALL(S, ?SIZED(Size,Size),
				  begin inc_temp(), S < 20 end),
			  [{numtests,3},{max_size,40},noshrink]),
     ?_failsWithOneOf([[{true,false}],[{false,true}]],
		      ?FORALL({B1,B2}, {boolean(),boolean()}, equals(B1,B2))),
     ?_failsWith([2,1],
		 ?FORALL(X, integer(1,10), ?FORALL(Y, integer(1,10), X =< Y))),
     ?_failsWith([1,2],
		 ?FORALL(Y, integer(1,10), ?FORALL(X, integer(1,10), X =< Y))),
     ?_failsWithOneOf([[[0,1]],[[0,-1]],[[1,0]],[[-1,0]]],
		      ?FORALL(L, list(integer()), lists:reverse(L) =:= L)),
     ?_failsWith([[1,2,3,4,5,6,7,8,9,10]],
		 ?FORALL(_L, shuffle(lists:seq(1,10)), false)),
     %% TODO: check that these don't shrink
     ?_fails(?FORALL(_, integer(0,0), false)),
     ?_fails(?FORALL(_, float(0.0,0.0), false)),
     ?_fails(fails(?FORALL(_, integer(), false))),
     ?_failsWith([16], ?FORALL(X, ?LET(Y,integer(),Y*Y), X < 15)),
     ?_failsWith([0.0],
		 ?FORALL(_, ?LETSHRINK([A,B], [float(),atom()], {A,B}), false)),
     ?_failsWith([], conjunction([{some,true},{thing,false}])),
     ?_failsWith([{2,1},[{group,[[{sub_group,[1]}]]},{stupid,[1]}]],
	 ?FORALL({X,Y}, {pos_integer(),pos_integer()},
		 conjunction([
		     {add_next, ?IMPLIES(X > Y, X + 1 > Y)},
		     {symmetry,
			 conjunction([
			     {add_sym, collect(X+Y, X+Y =:= Y+X)},
			     {sub_sym,
			      ?WHENFAIL(io:format("'-' isn't symmetric!~n",[]),
					X-Y =:= Y-X)}
			 ])},
		     {group,
		      conjunction([
			  {add_group,
			   ?WHENFAIL(io:format("This shouldn't happen!~n",[]),
				     ?FORALL(Z, pos_integer(),
					     (X+Y)+Z =:= X+(Y+Z)))},
			  {sub_group,
			   ?WHENFAIL(io:format("'-' doesn't group!~n",[]),
				     ?FORALL(W, pos_integer(),
					     (X-Y)-W =:= X-(Y-W)))}
		      ])},
		     {stupid, ?FORALL(_, pos_integer(), throw(woot))}
		 ]))),
     {timeout, 20, ?_fails(ets_counter:prop_ets_counter())},
     ?_fails(post_false:prop_simple()),
     ?_fails(error_statem:prop_simple())].

error_props_test_() ->
    [?_errorsOut(cant_generate,
		 ?FORALL(_, ?SUCHTHAT(X, pos_integer(), X =< 0), true)),
     ?_errorsOut(cant_satisfy,
		 ?FORALL(X, pos_integer(), ?IMPLIES(X =< 0, true))),
     ?_errorsOut(type_mismatch,
		 ?FORALL({X,Y}, [integer(),integer()], X < Y)),
     ?_assertCheck({error,rejected}, [2],
		   ?FORALL(X, integer(), ?IMPLIES(X > 5, X < 6))),
     ?_assertCheck({error,too_many_instances}, [1,ab],
		   ?FORALL(X, pos_integer(), X < 0)),
     ?_errorsOut(cant_generate, prec_false:prop_simple()),
     ?_errorsOut(cant_generate, nogen_statem:prop_simple()),
     ?_errorsOut(non_boolean_result, ?FORALL(_, integer(), not_a_boolean)),
     ?_errorsOut(non_boolean_result,
		 ?FORALL(_, ?SHRINK(42,[0]),
			 non_deterministic([{2,false},{1,not_a_boolean}]))),
     ?_assertRun(false,
		 ?FORALL(_, ?SHRINK(42,[0]),
			 non_deterministic([{4,false},{1,true}])),
		 [], false),
     ?_assertRun(false,
		 ?FORALL(_, ?SHRINK(42,[0]),
			 non_deterministic([{3,false},{1,true},{1,false}])),
		 [], false)].

eval_test_() ->
    [?_assertEqual(Result, eval(Vars,SymbCall))
     || {Result,_Repr,Vars,SymbCall} <- symb_calls()].

pretty_print_test_() ->
    [?_assert(equal_ignoring_ws(Repr, proper_symb:pretty_print(Vars,SymbCall)))
     || {_Result,Repr,Vars,SymbCall} <- symb_calls()].

not_defined_test_() ->
    [?_assertNot(defined(SymbCall))
     || SymbCall <- undefined_symb_calls()].

options_test_() ->
    [?_assertTempBecomesN(300, true,
			  ?FORALL(_, 1, begin inc_temp(), true end),
			  [{numtests,300}]),
     ?_assertTempBecomesN(300, true,
			  ?FORALL(_, 1, begin inc_temp(), true end),
			  [300]),
     ?_failsWith([42], ?FORALL(_,?SHRINK(42,[0,1]),false), [noshrink]),
     ?_failsWith([42], ?FORALL(_,?SHRINK(42,[0,1]),false), [{max_shrinks,0}]),
     ?_fails(?FORALL(_,integer(),false), [fails]),
     ?_assertRun({error,cant_generate},
		 ?FORALL(_,?SUCHTHAT(X,pos_integer(),X > 0),true),
		 [{constraint_tries,0}], true),
     ?_failsWith([12],
		 ?FORALL(_,?SIZED(Size,integer(Size,Size)),false),
		 [{start_size,12}])].

adts_test_() ->
    [{timeout, 20,	% for Kostis' old laptop
      ?_passes(?FORALL({X,S},{integer(),set()},
		       sets:is_element(X,sets:add_element(X,S))), [20])},
     ?_passes(?FORALL({X,Y,D},
		      {integer(),float(),dict(integer(),float())},
		      dict:fetch(X,dict:store(X,Y,eval(D))) =:= Y), [30]),
     ?_fails(?FORALL({X,D},
	     {boolean(),dict(boolean(),integer())},
	     dict:erase(X, dict:store(X,42,D)) =:= D))].

parameter_test_() ->
    ?_passes(?FORALL(List, [zero1(),zero2(),zero3(),zero4()],
		     begin
			 [?assertMatch(undefined, proper_types:parameter(P))
			  || P <- [x1,x2,y2,x3,y3,x4,y4,v,w,z]],
			 lists:all(fun is_zero/1, List)
		     end)).

zip_test_() ->
    [?_assertEqual(proper_statem:zip(X, Y), Expected)
     || {X,Y,Expected} <- lists_to_zip()].

command_names_test_() ->
    [?_assertEqual(proper_statem:command_names(Cmds), Expected)
     || {Cmds,Expected} <- command_names()].

valid_cmds_test_() ->
    [?_assert(proper_statem:is_valid(Mod, State, Cmds, Env))
     || {Mod,State,Cmds,_,_,Env} <- valid_command_sequences()].

invalid_cmds_test_() ->
    [?_assertNot(proper_statem:is_valid(Mod, Mod:initial_state(), Cmds, []))
     || {Mod,Cmds,_,_} <- invalid_precondition()] ++
    [?_assertNot(proper_statem:is_valid(Mod, Mod:initial_state(), Cmds, []))
     || {Mod,Cmds} <- invalid_var()].

state_after_test_() ->
    [?_assertEqual(proper_statem:state_after(Mod, Cmds), StateAfter)
     || {Mod,_,Cmds,StateAfter,_,_} <- valid_command_sequences()].

cannot_generate_commands_test_() ->
    [?_test(assert_cant_generate_cmds(proper_statem:commands(Mod), 6))
     || Mod <- [prec_false]].

can_generate_commands0_test_() ->
    [?_test(assert_can_generate(proper_statem:commands(Mod), false))
     || Mod <- [pdict_statem]].

can_generate_commands1_test_() ->
    [?_test(assert_can_generate(proper_statem:commands(Mod, StartState), false))
     || {Mod,StartState} <- [{pdict_statem,[{a,1},{b,1},{c,100}]}]].

can_generate_parallel_commands0_test_() ->
    {timeout, 20,
     [?_test(assert_can_generate(proper_statem:parallel_commands(Mod), false))
      || Mod <- [ets_counter]]}.

can_generate_parallel_commands1_test_() ->
    {timeout, 20,
     [?_test(assert_can_generate(
	       proper_statem:parallel_commands(Mod, Mod:initial_state()),
	       false))
      || Mod <- [ets_counter]]}.

run_valid_commands_test_() ->
    [?_assertMatch({_H,DynState,ok}, setup_run_commands(Mod, Cmds, Env))
     || {Mod,_,Cmds,_,DynState,Env} <- valid_command_sequences()].

run_invalid_precondition_test_() ->
     [?_assertMatch({_H,_S,{precondition,false}},
		    setup_run_commands(Mod, Cmds, Env))
      || {Mod,Cmds,Env,_Shrunk} <- invalid_precondition()].

run_init_error_test_() ->
    [?_assertMatch({_H,_S,initialization_error},
		   setup_run_commands(Mod, Cmds, Env))
     || {Mod,Cmds,Env,_Shrunk} <- symbolic_init_invalid_sequences()].

run_postcondition_false() ->
    ?_assertMatch({_H,_S,{postcondition,false}},
		  run_commands(post_false, proper_statem:commands(post_false))).

run_exception() ->
    ?_assertMatch(
       {_H,_S,{exception,throw,badarg,_}},
       run_commands(post_false, proper_statem:commands(error_statem))).

get_next_test_() ->
    [?_assertEqual(Expected,
		   proper_statem:get_next(L, Len, MaxIndex, Available, W, N))
     || {L, Len, MaxIndex, Available, W, N, Expected} <- combinations()].

mk_first_comb_test_() ->
     [?_assertEqual(Expected, proper_statem:mk_first_comb(N, Len, W))
      || {N, Len, W, Expected} <- first_comb()].

args_not_defined_test() ->
    [?_assertNot(proper_statem:args_defined(Args, SymbEnv))
     || {Args,SymbEnv} <- arguments_not_defined()].

command_props_test_() ->
    {timeout, 150, [?_assertEqual([], proper:module(command_props, 50))]}.

%% TODO: is_instance check fails because of ?LET in fsm_commands/1?
can_generate_fsm_commands_test_() ->
    [?_test(assert_can_generate(proper_fsm:commands(Mod), false))
     || Mod <- [pdict_fsm, numbers_fsm]].

transition_target_test_() ->
    {timeout, 20, [?_assertEqual([], proper:module(numbers_fsm))]}.

dollar_only_cp_test_() ->
    ?_assertEqual(
       dollar_data(),
       [K || K <- all_data(),
	     is_atom(K),
	     re:run(atom_to_list(K), ["^[$]"], [{capture,none}]) =:= match]).


%%------------------------------------------------------------------------------
%% Helper Predicates
%%------------------------------------------------------------------------------

no_duplicates(L) ->
    length(lists:usort(L)) =:= length(L).

is_sorted([]) -> true;
is_sorted([_]) -> true;
is_sorted([A | [B|_] = T]) when A =< B -> is_sorted(T);
is_sorted(_) -> false.

same_elements(L1, L2) ->
    length(L1) =:= length(L2) andalso same_elems(L1, L2).

same_elems([], []) ->
    true;
same_elems([H|T], L) ->
    lists:member(H, L) andalso same_elems(T, lists:delete(H, L));
same_elems(_, _) ->
    false.

is_sorted(Old, New) ->
    same_elements(Old, New) andalso is_sorted(New).

equal_ignoring_ws(Str1, Str2) ->
    WhiteSpace = [32,9,10],
    equal_ignoring_chars(Str1, Str2, WhiteSpace).

equal_ignoring_chars([], [], _Ignore) ->
    true;
equal_ignoring_chars([_SameChar|Rest1], [_SameChar|Rest2], Ignore) ->
    equal_ignoring_chars(Rest1, Rest2, Ignore);
equal_ignoring_chars([Char1|Rest1] = Str1, [Char2|Rest2] = Str2, Ignore) ->
    case lists:member(Char1, Ignore) of
	true ->
	    equal_ignoring_chars(Rest1, Str2, Ignore);
	false ->
	    case lists:member(Char2, Ignore) of
		true ->
		    equal_ignoring_chars(Str1, Rest2, Ignore);
		false ->
		    false
	    end
    end.

smaller_lengths_than_my_own(L) ->
    lists:seq(0,length(L)).

is_zero(X) -> X =:= 0.


%%------------------------------------------------------------------------------
%% Functions to test
%%------------------------------------------------------------------------------

partition(Pivot, List) ->
    partition_tr(Pivot, List, [], []).

partition_tr(_Pivot, [], Lower, Higher) ->
    {Lower, Higher};
partition_tr(Pivot, [H|T], Lower, Higher) ->
    if
	H =< Pivot -> partition_tr(Pivot, T, [H|Lower], Higher);
	H > Pivot  -> partition_tr(Pivot, T, Lower, [H|Higher])
    end.

quicksort([]) -> [];
quicksort([H|T]) ->
    {Lower, Higher} = partition(H, T),
    quicksort(Lower) ++ [H] ++ quicksort(Higher).

creator(X) ->
    Self = self(),
    spawn_link(fun() -> destroyer(X,Self) end),
    receive
	_ -> ok
    end.

destroyer(X, Father) ->
    if
	X < 20 -> Father ! not_yet;
	true   -> exit(this_is_the_end)
    end.


%%------------------------------------------------------------------------------
%% Datatypes to test
%%------------------------------------------------------------------------------

%% TODO: remove this if you make 'shuffle' a default constructor
shuffle([]) ->
    [];
shuffle(L) ->
    ?LET(X, elements(L), [X | shuffle(lists:delete(X,L))]).

ulist(ElemType) ->
    ?LET(L, list(ElemType), L--(L--lists:usort(L))).

zerostream(ExpectedMeanLen) ->
    ?LAZY(frequency([
	{1, []},
	{ExpectedMeanLen, [0 | zerostream(ExpectedMeanLen)]}
    ])).

-type my_native_type() :: integer().
my_proper_type() -> atom().
-type type_and_fun() :: integer().
type_and_fun() -> atom().
-type type_only() :: integer().
-type id(X) :: X.
-type lof() :: [float()].

-type deeplist() :: [deeplist()].

deeplist() ->
    ?SIZED(Size, deeplist(Size)).

deeplist(0) ->
    [];
deeplist(Size) ->
    ?LAZY(proper_types:distlist(Size, fun deeplist/1, false)).

-type tree(T) :: 'null' | {'node',T,tree(T),tree(T)}.

tree(ElemType) ->
    ?SIZED(Size, tree(ElemType,Size)).

tree(_ElemType, 0) ->
    null;
tree(ElemType, Size) ->
    LeftTree = tree(ElemType, Size div 2),
    RightTree = tree(ElemType, Size div 2),
    frequency([
	{1, tree(ElemType,0)},
	{5, ?LETSHRINK([L,R], [LeftTree,RightTree], {node,ElemType,L,R})}
    ]).

-type a() :: 'aleaf' | {'anode',a(),b()}.
-type b() :: 'bleaf' | {'bnode',a(),b()}.

a() ->
    ?SIZED(Size, a(Size)).

a(0) ->
    aleaf;
a(Size) ->
    union([
	?LAZY(a(0)),
	?LAZY(?LETSHRINK([A], [a(Size div 2)], {anode,A,b(Size)}))
    ]).

b() ->
    ?SIZED(Size, b(Size)).

b(0) ->
    bleaf;
b(Size) ->
    union([
	?LAZY(b(0)),
	?LAZY(?LETSHRINK([B], [b(Size div 2)], {bnode,a(Size),B}))
    ]).

-type gen_tree(T) :: 'null' | {T,[gen_tree(T),...]}.

gen_tree(ElemType) ->
    ?SIZED(Size, gen_tree(ElemType,Size)).

gen_tree(_ElemType, 0) ->
    null;
gen_tree(ElemType, Size) ->
    SubGen = fun(S) -> gen_tree(ElemType,S) end,
    oneof([
	?LAZY(gen_tree(ElemType,0)),
	?LAZY(?LETSHRINK(Children, proper_types:distlist(Size, SubGen, true),
			 {ElemType,Children}))
    ]).

-type g() :: 'null' | {'tag',[g()]}.
-type h() :: 'null' | {'tag',[{'ok',h()}]}.
-type i() :: 'null' | {'tag',i(),[i()]}.
-type j() :: 'null' | {'one',j()} | {'tag',j(),j(),[j()],[j()]}.
-type k() :: 'null' | {'tag',[{k(),k()}]}.
-type l() :: 'null' | {'tag',l(),[l(),...]}.

zero1() ->
    proper_types:with_parameter(
      x1, 0, ?SUCHTHAT(I, range(-1, 1), I =:= proper_types:parameter(x1))).

zero2() ->
    proper_types:with_parameters(
      [{x2,41}],
      ?LET(X,
	   proper_types:with_parameter(
	     y2, 43,
	     ?SUCHTHAT(
		I, range(41, 43),
		I > proper_types:parameter(x2)
		andalso I < proper_types:parameter(y2))),
	   X - 42)).

zero3() ->
    ?SUCHTHAT(I, range(-1, 1),
	      I > proper_types:parameter(x3, -1)
	      andalso I < proper_types:parameter(y3, 1)).

zero4() ->
    proper_types:with_parameters(
      [{x4,-2}, {y4,2}],
      proper_types:with_parameters(
	[{x4,-1}, {y4,1}],
	?SUCHTHAT(I, range(-1, 1),
		  I > proper_types:parameter(x4)
		  andalso I < proper_types:parameter(y4)))).


%%------------------------------------------------------------------------------
%% Old Tests and datatypes
%%------------------------------------------------------------------------------

% nelist(ElemType) ->
%     [ElemType | list(ElemType)].
%
% uvector(0, _ElemType) ->
%    [];
% uvector(N, ElemType) ->
%     ?LET(Rest,
% 	 uvector(N-1, ElemType),
% 	 ?LET(Elem,
% 	      ?SUCHTHAT(E, ElemType, not lists:member(E,Rest)),
% 	      [Elem | Rest])).
%
% subset(Generators) ->
%     ?LET(Keep,
% 	 [{boolean(),G} || G <- Generators],
% 	 [G || {true,G} <- Keep]).
%
% unique(ElemTypes) ->
%     ?LET(Values,
% 	 list(ElemTypes),
% 	 lists:usort(Values)).
%
% ulist2(ElemType) ->
%     ?SUCHTHAT(L, list(ElemType), no_duplicates(L)).
%
% kvlist(KeyType, ValueType) ->
%     ?LET(Keys,
% 	 list(KeyType),
% 	 [{K,ValueType} || K <- Keys]).
%
% tree_member(_X, {node,_X,_L,_R}) -> true;
% tree_member(X, {node,_Y,L,R}) -> tree_member(X, L) orelse tree_member(X, R);
% tree_member(_X, {empty}) -> false.
%
% symbdict(KeyType, ValueType) ->
%     ?SIZED(Size, symbdict(Size, KeyType, ValueType)).
%
% symbdict(0, _KeyType, _ValueType) ->
%     {call,dict,new,[]};
% symbdict(Size, KeyType, ValueType) ->
%     ?LAZY(
% 	frequency([
% 	    {1,symbdict(0, KeyType, ValueType)},
% 	    {4,?LETSHRINK([Smaller], [symbdict(Size - 1, KeyType, ValueType)],
% 			  {call, dict, append,[KeyType,ValueType,Smaller]})}
% 	])
%     ).
%
% test(15) ->
%     ?FORALL(T,
% 	    ?LET(L,
% 		 non_empty(list(integer())),
% 		 ?LET(Y,
% 		      elements(L),
% 		      {Y,L})),
% 	    erlang:element(1,T) =/= 42);
% test(18) ->
%     ?FORALL(L, kvlist(atom(),integer()), not lists:keymember(42,2,L));
% test(19) ->
%     ?FORALL(T, tree(integer()), not tree_member(42, T));
% test(20) ->
%     ?FORALL(X,
% 	    ?LET(L, non_empty(list(integer())), list(oneof(L))),
% 	    length(X) < 10);
% test(27) ->
%     ?FORALL(SD,
% 	    symbdict(integer(),integer()),
% 	    not dict:is_key(42, eval(SD)));
% test(29) ->
%     ?FORALL({F,L},
% 	    {function(1,integer(1,100)), list(integer())},
% 	    lists:all(fun(X) -> F(X) =/= 42 end, L));
% correct_smaller_length_aggregation(Tests, SmallerLens) ->
%     {Zeros,Larger} = lists:partition(fun(X) -> X =:= 0 end, SmallerLens),
%     length(Zeros) =:= Tests
%     andalso correct_smaller_length_aggregation(Tests, Larger, 1).
%
% correct_smaller_length_aggregation(0, SmallerLens, _Len) ->
%     SmallerLens =:= [];
% correct_smaller_length_aggregation(NotMoreThan, SmallerLens, Len) ->
%     {Lens,Larger} = lists:partition(fun(X) -> X =:= Len end, SmallerLens),
%     Num = length(Lens),
%     Num =< NotMoreThan
%     andalso correct_smaller_length_aggregation(Num, Larger, Len+1).
