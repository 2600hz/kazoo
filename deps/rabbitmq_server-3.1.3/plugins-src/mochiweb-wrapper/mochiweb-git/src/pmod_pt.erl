%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(pmod_pt).
-export([parse_transform/2,
	 format_error/1]).

%% Expand function definition forms of parameterized module.
%% The code is based on the code in sys_expand_pmod which used to be
%% included in the compiler, but details are different because
%% sys_pre_expand has not been run. In particular:
%%
%% * Record definitions are still present and must be handled.
%%
%% * (Syntatic) local calls may actually be calls to an imported
%%   funtion or a BIF. It is a local call if and only if there
%%   is a definition for the function in the module.
%%
%% * When we introduce the module parameters and 'THIS' in each
%%   function, we must artificially use it to avoid a warning for
%%   unused variables.
%%
%% * On the other hand, we don't have to worry about module_info/0,1
%%   because they have not been added yet.

-record(pmod, {parameters,
	       defined
	      }).

parse_transform(Forms0, _Options) ->
    put(?MODULE, []),
    Forms = transform(Forms0),
    case erase(?MODULE) of
	[] ->
	    Forms;
	[_|_]=Errors ->
	    File = get_file(Forms),
	    {error,[{File,Errors}],[]}
    end.
  
format_error(extends_self) ->
    "cannot extend from self";
format_error(define_instance) ->
    "defining instance function not allowed in parameterized module".

add_error(Line, Error) ->
    put(?MODULE, get(?MODULE) ++ [{Line,?MODULE,Error}]).

get_file([{attribute,_,file,{File,_}}|_]) -> File;
get_file([_|T]) -> get_file(T).
    
transform(Forms0) ->
    Def = collect_defined(Forms0),
    {Base,ModAs,Forms1} = attribs(Forms0, [], undefined, []),
    {Mod,Ps0} = case ModAs of
		    {M0,P0} -> {M0,P0};
		    M0 -> {M0,undefined}
		end,
    Forms2 = case Ps0 of
		 undefined ->
		     Forms1;
		 _ ->
		     pmod_expand(Forms1, Mod, Base, Ps0, Def)
	     end,

    %% Add new functions.
    NewFs0 = maybe_extend(Base, Mod, Ps0),
    NewExps = collect_defined(NewFs0),
    Forms3 = add_attributes(Forms2, [{attribute,0,export,NewExps}]),
    add_new_funcs(Forms3, NewFs0).

pmod_expand(Forms0, Mod, Base, Ps0, Def) ->
    Ps = if is_atom(Base) ->
		 ['BASE' | Ps0];
	    true ->
		 Ps0
	 end,
    St0 = #pmod{parameters=Ps,defined=gb_sets:from_list(Def)},
    {Forms1,_} = forms(Forms0, St0),
    Forms2 = update_exps(Forms1),
    Forms3 = update_forms(Forms2),
    NewFs0 = add_instance(Mod, Ps, []),
    NewFs = ensure_new(Base, Ps0, NewFs0),
    Forms = add_new_funcs(Forms3, NewFs),
    NewExps = collect_defined(NewFs),
    add_attributes(Forms, [{attribute,0,export,NewExps}]).

add_attributes([{attribute,_,module,_}=F|Fs], Attrs) ->
    [F|Attrs++Fs];
add_attributes([F|Fs], Attrs) ->
    [F|add_attributes(Fs, Attrs)].

add_new_funcs([{eof,_}|_]=Fs, NewFs) ->
    NewFs ++ Fs;
add_new_funcs([F|Fs], Es) ->
    [F|add_new_funcs(Fs, Es)].

maybe_extend([], _, _) ->
    %% No 'extends' attribute.
    [];
maybe_extend(Base, _Mod, undefined) ->
    %% There is a an 'extends' attribute; the module is not parameterized.
    Name = '$handle_undefined_function',
    Args = [{var,0,'Func'},{var,0,'Args'}],
    Body = [make_apply({atom,0,Base}, {var,0,'Func'}, {var,0,'Args'})],
    F = {function,0,Name,2,[{clause,0,Args,[],Body}]},
    [F];
maybe_extend(Base, Mod, Ps) ->
    %% There is a an 'extends' attribute; the module is parameterized.
    Name = '$handle_undefined_function',
    Args = [{var,0,'Func'},{var,0,'Args'}],
    DontCares = [{var,0,'_'} || _ <- Ps],
    TuplePs = {tuple,0,[{atom,0,Mod},{var,0,'BaseVars'}|DontCares]},
    G = [{call,0,{atom,0,is_atom},
	  [{call,0,{atom,0,element},
	    [{integer,0,1},{var,0,'BaseVars'}]}]}],
    FixedArgs = make_lists_rev([{var,0,'Rs'},
				{cons,0,{var,0,'BaseVars'},{nil,0}}]),
    Body = [{'case',0,make_lists_rev([{var,0,'Args'}]),
	     [{clause,0,[{cons,0,TuplePs,{var,0,'Rs'}}],[G],
	       [make_apply({atom,0,Base}, {var,0,'Func'}, FixedArgs)]},
	      {clause,0,[{var,0,'_'}],[],
	       [make_apply({atom,0,Base}, {var,0,'Func'}, {var,0,'Args'})]}
	     ]}],
    F = {function,0,Name,2,[{clause,0,Args,[],Body}]},
    [F].

make_apply(M, F, A) ->
    {call,0,{remote,0,{atom,0,erlang},{atom,0,apply}},[M,F,A]}.

make_lists_rev(As) ->
    {call,0,{remote,0,{atom,0,lists},{atom,0,reverse}},As}.

ensure_new(Base, Ps, Fs) ->
    case has_new(Fs) of
	true ->
	    Fs;
	false ->
	    add_new(Base, Ps, Fs)
    end.

has_new([{function,_L,new,_A,_Cs} | _Fs]) ->
    true;
has_new([_ | Fs]) ->
    has_new(Fs);
has_new([]) ->
    false.

add_new(Base, Ps, Fs) ->
    Vs = [{var,0,V} || V <- Ps],
    As = if is_atom(Base) ->
		 [{call,0,{remote,0,{atom,0,Base},{atom,0,new}},Vs} | Vs];
	    true ->
		 Vs
	 end,
    Body = [{call,0,{atom,0,instance},As}],
    add_func(new, Vs, Body, Fs).

add_instance(Mod, Ps, Fs) ->
    Vs = [{var,0,V} || V <- Ps],
    AbsMod = [{tuple,0,[{atom,0,Mod}|Vs]}],
    add_func(instance, Vs, AbsMod, Fs).

add_func(Name, Args, Body, Fs) ->
    A = length(Args),
    F = {function,0,Name,A,[{clause,0,Args,[],Body}]},
    [F|Fs].

collect_defined(Fs) ->
    [{N,A} || {function,_,N,A,_} <- Fs].

attribs([{attribute,Line,module,{Mod,_}=ModAs}|T], Base, _, Acc) ->
    attribs(T, Base, ModAs, [{attribute,Line,module,Mod}|Acc]);
attribs([{attribute,_,module,Mod}=H|T], Base, _, Acc) ->
    attribs(T, Base, Mod, [H|Acc]);
attribs([{attribute,Line,extends,Base}|T], Base0, Ps, Acc) when is_atom(Base) ->
    Mod = case Ps of
	      {Mod0,_} -> Mod0;
	      Mod0 -> Mod0
	  end,
    case Mod of
	Base ->
	    add_error(Line, extends_self),
	    attribs(T, Base0, Ps, Acc);
	_ ->
	    attribs(T, Base, Ps, Acc)
    end;
attribs([H|T], Base, Ps, Acc) ->
    attribs(T, Base, Ps, [H|Acc]);
attribs([], Base, Ps, Acc) ->
    {Base,Ps,lists:reverse(Acc)}.

%% This is extremely simplistic for now; all functions get an extra
%% parameter, whether they need it or not, except for static functions.

update_function_name({F,A}) when F =/= new ->
    {F,A+1};
update_function_name(E) ->
    E.

update_forms([{function,L,N,A,Cs}|Fs]) when N =/= new ->
    [{function,L,N,A+1,Cs}|update_forms(Fs)];
update_forms([F|Fs]) ->
    [F|update_forms(Fs)];
update_forms([]) ->
    [].

update_exps([{attribute,Line,export,Es0}|T]) ->
    Es = [update_function_name(E) || E <- Es0],
    [{attribute,Line,export,Es}|update_exps(T)];
update_exps([H|T]) ->
    [H|update_exps(T)];
update_exps([]) ->
    [].

%% Process the program forms.

forms([F0|Fs0],St0) ->
    {F1,St1} = form(F0,St0),
    {Fs1,St2} = forms(Fs0,St1),
    {[F1|Fs1],St2};
forms([], St0) ->
    {[], St0}.

%% Only function definitions are of interest here. State is not updated.
form({function,Line,instance,_Arity,_Clauses}=F,St) ->
    add_error(Line, define_instance),
    {F,St};
form({function,Line,Name0,Arity0,Clauses0},St) when Name0 =/= new ->
    {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0, St),
    {{function,Line,Name,Arity,Clauses},St};
%% Pass anything else through
form(F,St) -> {F,St}.

function(Name, Arity, Clauses0, St) ->
    Clauses1 = clauses(Clauses0,St),
    {Name,Arity,Clauses1}.

clauses([C|Cs],#pmod{parameters=Ps}=St) ->
    {clause,L,H,G,B0} = clause(C,St),
    T = {tuple,L,[{var,L,V} || V <- ['_'|Ps]]},
    B = [{match,L,{var,L,'_'},{var,L,V}} || V <- ['THIS'|Ps]] ++ B0,
    [{clause,L,H++[{match,L,T,{var,L,'THIS'}}],G,B}|clauses(Cs,St)];
clauses([],_St) -> [].

clause({clause,Line,H,G,B0},St) ->
    %% We never update H and G, so we will just copy them.
    B1 = exprs(B0,St),
    {clause,Line,H,G,B1}.

pattern_grp([{bin_element,L1,E1,S1,T1} | Fs],St) ->
    S2 = case S1 of
	     default ->
		 default;
	     _ ->
		 expr(S1,St)
	 end,
    T2 = case T1 of
	     default ->
		 default;
	     _ ->
		 bit_types(T1)
	 end,
    [{bin_element,L1,expr(E1,St),S2,T2} | pattern_grp(Fs,St)];
pattern_grp([],_St) ->
    [].

bit_types([]) ->
    [];
bit_types([Atom | Rest]) when is_atom(Atom) ->
    [Atom | bit_types(Rest)];
bit_types([{Atom, Integer} | Rest]) when is_atom(Atom), is_integer(Integer) ->
    [{Atom, Integer} | bit_types(Rest)].

exprs([E0|Es],St) ->
    E1 = expr(E0,St),
    [E1|exprs(Es,St)];
exprs([],_St) -> [].

expr({var,_L,_V}=Var,_St) ->
    Var;
expr({integer,_Line,_I}=Integer,_St) -> Integer;
expr({float,_Line,_F}=Float,_St) -> Float;
expr({atom,_Line,_A}=Atom,_St) -> Atom;
expr({string,_Line,_S}=String,_St) -> String;
expr({char,_Line,_C}=Char,_St) -> Char;
expr({nil,_Line}=Nil,_St) -> Nil;
expr({cons,Line,H0,T0},St) ->
    H1 = expr(H0,St),
    T1 = expr(T0,St),
    {cons,Line,H1,T1};
expr({lc,Line,E0,Qs0},St) ->
    Qs1 = lc_bc_quals(Qs0,St),
    E1 = expr(E0,St),
    {lc,Line,E1,Qs1};
expr({bc,Line,E0,Qs0},St) ->
    Qs1 = lc_bc_quals(Qs0,St),
    E1 = expr(E0,St),
    {bc,Line,E1,Qs1};
expr({tuple,Line,Es0},St) ->
    Es1 = expr_list(Es0,St),
    {tuple,Line,Es1};
expr({record,Line,Name,Is0},St) ->
    Is = record_fields(Is0,St),
    {record,Line,Name,Is};
expr({record,Line,E0,Name,Is0},St) ->
    E = expr(E0,St),
    Is = record_fields(Is0,St),
    {record,Line,E,Name,Is};
expr({record_field,Line,E0,Name,Key},St) ->
    E = expr(E0,St),
    {record_field,Line,E,Name,Key};
expr({block,Line,Es0},St) ->
    Es1 = exprs(Es0,St),
    {block,Line,Es1};
expr({'if',Line,Cs0},St) ->
    Cs1 = icr_clauses(Cs0,St),
    {'if',Line,Cs1};
expr({'case',Line,E0,Cs0},St) ->
    E1 = expr(E0,St),
    Cs1 = icr_clauses(Cs0,St),
    {'case',Line,E1,Cs1};
expr({'receive',Line,Cs0},St) ->
    Cs1 = icr_clauses(Cs0,St),
    {'receive',Line,Cs1};
expr({'receive',Line,Cs0,To0,ToEs0},St) ->
    To1 = expr(To0,St),
    ToEs1 = exprs(ToEs0,St),
    Cs1 = icr_clauses(Cs0,St),
    {'receive',Line,Cs1,To1,ToEs1};
expr({'try',Line,Es0,Scs0,Ccs0,As0},St) ->
    Es1 = exprs(Es0,St),
    Scs1 = icr_clauses(Scs0,St),
    Ccs1 = icr_clauses(Ccs0,St),
    As1 = exprs(As0,St),
    {'try',Line,Es1,Scs1,Ccs1,As1};
expr({'fun',_,{function,_,_,_}}=ExtFun,_St) ->
    ExtFun;
expr({'fun',Line,Body},St) ->
    case Body of
	{clauses,Cs0} ->
	    Cs1 = fun_clauses(Cs0,St),
	    {'fun',Line,{clauses,Cs1}};
	{function,F,A} = Function ->
	    {F1,A1} = update_function_name({F,A}),
	    if A1 =:= A ->
		    {'fun',Line,Function};
	       true ->
		    %% Must rewrite local fun-name to a fun that does a
		    %% call with the extra THIS parameter.
		    As = make_vars(A, Line),
		    As1 = As ++ [{var,Line,'THIS'}],
		    Call = {call,Line,{atom,Line,F1},As1},
		    Cs = [{clause,Line,As,[],[Call]}],
		    {'fun',Line,{clauses,Cs}}
	    end;
	{function,_M,_F,_A} = Fun4 ->		%This is an error in lint!
	    {'fun',Line,Fun4}
    end;
expr({call,Lc,{atom,_,instance}=Name,As0},St) ->
    %% All local functions 'instance(...)' are static by definition,
    %% so they do not take a 'THIS' argument when called
    As1 = expr_list(As0,St),
    {call,Lc,Name,As1};
expr({call,Lc,{atom,_,new}=Name,As0},St) ->
    %% All local functions 'new(...)' are static by definition,
    %% so they do not take a 'THIS' argument when called
    As1 = expr_list(As0,St),
    {call,Lc,Name,As1};
expr({call,Lc,{atom,_Lf,F}=Atom,As0}, #pmod{defined=Def}=St) ->
    As1 = expr_list(As0,St),
    case gb_sets:is_member({F,length(As0)}, Def) of
	false ->
	    %% BIF or imported function.
	    {call,Lc,Atom,As1};
	true ->
	    %% Local function call - needs THIS parameter.
	    {call,Lc,Atom,As1 ++ [{var,0,'THIS'}]}
    end;
expr({call,Line,F0,As0},St) ->
    %% Other function call
    F1 = expr(F0,St),
    As1 = expr_list(As0,St),
    {call,Line,F1,As1};
expr({'catch',Line,E0},St) ->
    E1 = expr(E0,St),
    {'catch',Line,E1};
expr({match,Line,P,E0},St) ->
    E1 = expr(E0,St),
    {match,Line,P,E1};
expr({bin,Line,Fs},St) ->
    Fs2 = pattern_grp(Fs,St),
    {bin,Line,Fs2};
expr({op,Line,Op,A0},St) ->
    A1 = expr(A0,St),
    {op,Line,Op,A1};
expr({op,Line,Op,L0,R0},St) ->
    L1 = expr(L0,St),
    R1 = expr(R0,St),
    {op,Line,Op,L1,R1};
%% The following are not allowed to occur anywhere!
expr({remote,Line,M0,F0},St) ->
    M1 = expr(M0,St),
    F1 = expr(F0,St),
    {remote,Line,M1,F1}.

expr_list([E0|Es],St) ->
    E1 = expr(E0,St),
    [E1|expr_list(Es,St)];
expr_list([],_St) -> [].

record_fields([{record_field,L,K,E0}|T],St) ->
    E = expr(E0,St),
    [{record_field,L,K,E}|record_fields(T,St)];
record_fields([],_) -> [].

icr_clauses([C0|Cs],St) ->
    C1 = clause(C0,St),
    [C1|icr_clauses(Cs,St)];
icr_clauses([],_St) -> [].

lc_bc_quals([{generate,Line,P,E0}|Qs],St) ->
    E1 = expr(E0,St),
    [{generate,Line,P,E1}|lc_bc_quals(Qs,St)];
lc_bc_quals([{b_generate,Line,P,E0}|Qs],St) ->
    E1 = expr(E0,St),
    [{b_generate,Line,P,E1}|lc_bc_quals(Qs,St)];
lc_bc_quals([E0|Qs],St) ->
    E1 = expr(E0,St),
    [E1|lc_bc_quals(Qs,St)];
lc_bc_quals([],_St) -> [].

fun_clauses([C0|Cs],St) ->
    C1 = clause(C0,St),
    [C1|fun_clauses(Cs,St)];
fun_clauses([],_St) -> [].

make_vars(N, L) ->
    make_vars(1, N, L).

make_vars(N, M, L) when N =< M ->
    V = list_to_atom("X"++integer_to_list(N)),
    [{var,L,V} | make_vars(N + 1, M, L)];
make_vars(_, _, _) ->
    [].
