-module(command_props).

-include_lib("proper/include/proper.hrl").
-define(MOD, ets_counter).
-define(MOD1, pdict_statem).

ne_nd_list(ElemType) ->
    ?LET(L,
	 non_empty(list(ElemType)),
	 lists:usort(L)).

short_ne_nd_list(ElemType) ->
    ?LET(L,
	 resize(8, non_empty(list(ElemType))),
	 lists:usort(L)).

no_duplicates(L) -> length(L) =:= length(lists:usort(L)).

prop_index() ->
    ?FORALL(List, ne_nd_list(integer()),
	    ?FORALL(X, union(List), 
		    lists:nth(proper_statem:index(X,List),List) =:= X)).

prop_all_insertions() ->
     ?FORALL(List, list(integer()),
        begin
	    Len = length(List),
	    ?FORALL(Limit, range(1,Len+1),
		    ?FORALL(X, integer(),
		       begin
			   AllIns = proper_statem:all_insertions(X,Limit,List),
			   length(AllIns) =:= Limit
		       end))
	end).

prop_insert_all() ->
    ?FORALL(List, short_ne_nd_list(integer()),
       begin
	   Len = length(List),
	   {L1,L2} = lists:split(Len div 2, List),
	   AllIns = proper_statem:insert_all(L1,L2), 
	   ?WHENFAIL(io:format("~nList: ~w, L1: ~w, L2: ~w~nAllIns: ~w~n",
			       [List,L1,L2,AllIns]), 
		     lists:all( fun(L) -> 
					length(L)=:=Len andalso no_duplicates(L)
			                andalso lists:subtract(L,L2) =:= L1
				end, AllIns))
       end).

prop_zip() ->	       
    ?FORALL({X,Y}, {list(),list()},
	    begin
		LenX = length(X),
		LenY = length(Y),
		Res = if LenX < LenY -> 
			      lists:zip(X, lists:sublist(Y, LenX));
			 LenX =:= LenY ->
			      lists:zip(X, Y);
			 LenX > LenY -> 
			      lists:zip(lists:sublist(X, LenY), Y)
		      end,
		equals(zip(X, Y), Res)
	    end).

prop_state_after() ->
    ?FORALL(Cmds, proper_statem:commands(?MOD1),
	    begin
		SymbState = proper_statem:state_after(?MOD1, Cmds),
		{_,S,ok} = proper_statem:run_commands(?MOD1, Cmds),
		?MOD1:clean_up(),
		equals(proper_symb:eval(SymbState), S)
	    end).

prop_p() ->
    ?FORALL(Workers, range(2, 4),
	    ?FORALL(CmdList,
		    ?SUCHTHAT(X, resize(12, commands(?MOD)), length(X) >= Workers),
		    begin
			N = length(CmdList),
			Len = N div Workers,
			Comb = proper_statem:mk_first_comb(N, Len, Workers),
			LookUp =  orddict:from_list(proper_statem:mk_dict(CmdList,1)),
			State = ?MOD:initial_state(),
			Res = proper_statem:fix_gen(N, Len, Comb, LookUp, ?MOD, State,
						    [], Workers), 
			?WHENFAIL(io:format("CmdList: ~w\nResult: ~w\n", [CmdList, Res]),
				  length(lists:last(Res)) =:= Len) 
		    end)).
	
prop_check_true() ->
    ?FORALL(Cmds, proper_statem:parallel_commands(?MOD),
	    begin
		?MOD:clean_up(),
		?MOD:set_up(),
		{Seq,Parallel} = Cmds,
		InitialState = proper_statem:get_initial_state(Seq),
		{ok,DynState} = proper_statem:safe_eval_init([], InitialState),
		{{_, State, ok}, Env1} =
		    proper_statem:run_sequential(Seq, [], ?MOD, [], DynState),
		Res = lists:map(fun(C) -> proper_statem:execute(C, Env1, ?MOD, []) end,
				Parallel),
		V = proper_statem:check(?MOD, State, Env1, Env1, [], Res, []),
		equals(V, true)
	    end).
