%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
%%% File    : indexer_porter.erl
%%% Author  : Hans Nilsson
%%% Purpose : Porter Stemming 
%%% Created : 16 Nov 2004

-module(indexer_porter).

%% This module is an Erlang implementation of the "Porter Stemming Algorithm".
%% 
%% Stemming is the process of converting a word into its stem. This is needed 
%% in text search algorithms.  For instance all of
%% 
%%         CONNECT
%%         CONNECTED
%%         CONNECTING
%%         CONNECTION
%%         CONNECTIONS
%% 
%% are converted into "connect".   
%% 
%% For the Porter Stemming, see 
%%       http://www.tartarus.org/~martin/PorterStemmer/index.html
%% 
%% This implementation uses the fact that Erlang is best suited for operations
%% on heads of lists, while the algorithm operates only on tails of lists. 
%% Therefore all endings and routines are transformed into there reversed
%% equivalence.  By this, the speed is increased by a factor of at least two.
%% 
%% For example, the rule
%%       (m>0) ATIONAL ->  ATE 
%% is transformed into
%%       (m>0) LANOITA ->  ETA
%% 
%% Otherwise the implementation follows the original specification rather
%% directly.
    
-export([stem/1,  test/0]).

-export([tst/3]).   % "Internal" export for apply/3

%%% API
%%   @spec stem(In::string()) -> Out::string()
%%      In: A word (case insensitive)
%%      Out: The corresponding stem in lower case

stem(Word) when length(Word) > 2 ->
    W = lower_case(Word),
    lists:reverse(
      step_5( step_4( step_3( step_2( step_1(W) ))))
     );

stem(ShortWord) ->
    lists:reverse(lower_case(ShortWord)).

%%%================================================================
%%% Test routines

test() ->
    Ins = f("voc.txt"),
    Outs = f("output.txt"),
    NumWords = length(Ins),
    {Time_us,NumErrors} = timer:tc(?MODULE,tst, [Ins,Outs,0]),
    io:format("NumErrors = ~w\n"
	      "     Time = ~w us\n"
	      " NumWords = ~w\n"
	      "Time/word = ~w us\n",
	      [NumErrors, Time_us, NumWords, Time_us/NumWords]).

tst([In|Ins], [Out|Outs], N) ->
    tst(Ins, Outs,
	case stem(In) of
	    Out -> N;
	    Other -> io:format('**~w: ~s -> ~s. Expecting ~s\n',
			       [N, In,Other,Out]),
		     N+1
	end);
tst([], [], N) ->
    N.

f(F) ->
    {ok,B} = file:read_file(F),
    string:tokens(binary_to_list(B), "\n").

%%%================================================================
%%% Debug macros

%-define(dfmt(Fmt,Args), io:format(Fmt,Args)).
-define(dfmt(Fmt,Args), ok).

-define(D, ?dfmt('<Line ~p>', [?LINE])).
-define(Dstep(StepNum, WordIn, WordOutExpr),
	begin
	    ?dfmt('Step ~w: In=~p (reversed: ~p) ',
		  [StepNum, lists:reverse(WordIn), WordIn]),
	    Out__ = (WordOutExpr),
	    ?dfmt(' Out=~p (reversed: ~p)\n', 
		  [lists:reverse(Out__), Out__]),
	    Out__
	end).

%%%================================================================
%%%
%%% Local routines
%%%

step_1(W) -> step_1c( step_1b( step_1a(W) )).

%%%----------------------------------------------------------------
step_1a(W) -> ?Dstep('1a', W, s_1a(W)).

s_1a("s"++W) -> s_1a1(W);
s_1a(W) -> W.

s_1a1("ess"++S) -> "ss"++S;
s_1a1("ei" ++S) -> "i"++S;
s_1a1("s"  ++S) -> "ss"++S;
s_1a1(S) -> S.

%%%----------------------------------------------------------------
step_1b(W) -> ?Dstep('1b', W, s_1b(W)).

s_1b(W="dee"++S) -> ?D, if_m_gt("ee"++S, W, S, 0);
s_1b(W="de"++S) ->
    case '*v*'(S) of
	true -> step_1b2(S);
	false -> W
    end;
s_1b(W="gni"++S) ->
    case '*v*'(S) of
	true -> step_1b2(S);
	false -> W
    end;
s_1b(W) ->	    
    W.


step_1b2(W) -> ?Dstep('1b2', W, s_1b2(W)).

s_1b2("ta"++S) -> "eta"++S;
s_1b2("lb"++S) -> "elb"++S;
s_1b2("zi"++S) -> "ezi"++S;
s_1b2(W=[C,C|S]) when C=/=$l,
		      C=/=$s,
		      C=/=$z ->
    case '*d'(W) of
	true -> [C|S];
	false -> s_1b2_2(W)
    end;
s_1b2(W) ->
    s_1b2_2(W).


s_1b2_2(W) -> 
    case (m(W)==1) and '*o'(W) of
	true -> "e"++W;
	false -> W
    end.

%%%----------------------------------------------------------------
step_1c(W) -> ?Dstep('1c', W, s_1c(W)).

s_1c("y"++Stem=W) ->
    case '*v*'(Stem) of
	true -> "i"++Stem;
	false -> W
    end;
s_1c(W) ->
    W.
    
%%%----------------------------------------------------------------
step_2(W) -> ?Dstep(2, W, s_2(W)).

%% The Porter paper suggests indexing on the penultimate letter (= our second)
%% but this gave no speedup, rather a bit slowdown.  This is probably because
%% the clever indexing that the Erlang compiler intoduces on this type of
%% arguments

s_2(W="lanoita"++S) -> ?D, if_m_gt("eta"++S, W, S, 0);
s_2(W="lanoit"++S) -> ?D, if_m_gt("noit"++S, W, S, 0);
s_2(W="icne"++S) -> ?D, if_m_gt("ecne"++S, W, S, 0);
s_2(W="icna"++S) -> ?D, if_m_gt("ecna"++S, W, S, 0);
s_2(W="rezi"++S) -> ?D, if_m_gt("ezi"++S, W, S, 0);
s_2(W="igol"++S) -> ?D, if_m_gt("gol"++S, W, S, 0); % New rule in Porter 1
s_2(W="ilb"++S) -> ?D, if_m_gt("elb"++S, W, S, 0); % replacement in Porter 1
s_2(W="illa"++S) -> ?D, if_m_gt("la"++S, W, S, 0);
s_2(W="iltne"++S) -> ?D, if_m_gt("tne"++S, W, S, 0);
s_2(W="ile"++S) -> ?D, if_m_gt("e"++S, W, S, 0);
s_2(W="ilsuo"++S) -> ?D, if_m_gt("suo"++S, W, S, 0);
s_2(W="noitazi"++S) -> ?D, if_m_gt("ezi"++S, W, S, 0);
s_2(W="noita"++S) -> ?D, if_m_gt("eta"++S, W, S, 0);
s_2(W="rota"++S) -> ?D, if_m_gt("eta"++S, W, S, 0);
s_2(W="msila"++S) -> ?D, if_m_gt("la"++S, W, S, 0);
s_2(W="ssenevi"++S) -> ?D, if_m_gt("evi"++S, W, S, 0);
s_2(W="ssenluf"++S) -> ?D, if_m_gt("luf"++S, W, S, 0);
s_2(W="ssensuo"++S) -> ?D, if_m_gt("suo"++S, W, S, 0);
s_2(W="itila"++S) -> ?D, if_m_gt("la"++S, W, S, 0);
s_2(W="itivi"++S) -> ?D, if_m_gt("evi"++S, W, S, 0);
s_2(W="itilib"++S) -> ?D, if_m_gt("elb"++S, W, S, 0);
s_2(W) -> W.
    
%%%----------------------------------------------------------------
step_3(W) -> ?Dstep(3, W, s_3(W)).

s_3(W="etaci"++S) -> ?D, if_m_gt("ci"++S, W, S, 0);
s_3(W="evita"++S) -> ?D, if_m_gt(S, W, S, 0);
s_3(W="ezila"++S) -> ?D, if_m_gt("la"++S, W, S, 0);
s_3(W="itici"++S) -> ?D, if_m_gt("ci"++S, W, S, 0);
s_3(W="laci"++S) -> ?D, if_m_gt("ci"++S, W, S, 0);
s_3(W="luf"++S) -> ?D, if_m_gt(S, W, S, 0);
s_3(W="ssen"++S) -> ?D, if_m_gt(S, W, S, 0);
s_3(W) -> W.
    
%%%----------------------------------------------------------------
step_4(W) -> ?Dstep(4, W, s_4(W)).

s_4(W="la"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="ecna"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="ecne"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="re"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="ci"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="elba"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="elbi"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="tna"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="tneme"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="tnem"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="tne"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="noi"++S) -> case S of
		       [$s|_] -> ?D, if_m_gt(S, W, S, 1);
		       [$t|_] -> ?D, if_m_gt(S, W, S, 1);
		       _ -> W
    end;
s_4(W="uo"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="msi"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="eta"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="iti"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="suo"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="evi"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W="ezi"++S) -> ?D, if_m_gt(S, W, S, 1);
s_4(W) -> W.
    
%%%----------------------------------------------------------------
step_5(W) -> step_5b(step_5a(W)).

step_5a(W) -> ?Dstep('5a', W, s_5a(W)).

s_5a("e"++Stem=W) ->
    ?D, case m(Stem) of
	    1 -> ?D, case not '*o'(Stem) of
			 true -> Stem;
			 false -> W
		     end;
	    M when M>1 -> ?D, Stem;
	    _ -> W
	end;
s_5a(W) -> W.

step_5b(W) -> ?Dstep('5b', W, s_5b(W)).

s_5b(W="ll"++S) -> ?D, if_m_gt("l"++S, W, "l"++S, 1);
s_5b(W) -> W.
	    
%%%================================================================
%%% for rules like:
%%%   (m>1) SUFFIX -> NEW
%%% Repl is NEW, Orig is SUFFIX and Ml is what m should be greater than

if_m_gt(Repl, Orig, Stem, Ml) -> 
    case m_gt(Ml, Stem) of
	true -> Repl;
	false -> Orig
    end.
    
%%%----------------------------------------------------------------
%% Test if m > M for Stem.
%%
%% The sequence (see Porter) [C](VC){m}[V] is transformed (reversed)
%% to [V](CV){m}[C]

m_gt(M, Stem) -> m_gt1(skip(v,Stem), M).
    
m_gt1(S,M) when M>=0 -> case catch take(v, take(c,S)) of
			    {'EXIT',_} -> false;
			    S1 -> m_gt1(S1,M-1)
			end;
m_gt1(_,M) when M<0 -> true.
		  
%%%----------------------------------------------------------------
%%% find the m-value for a Stem

m(Stem) -> m(skip(v,Stem),0).

m(S,M) -> case catch take(v, take(c,S)) of
	      {'EXIT',_} -> M;
	      S1 -> m(S1,M+1)
	  end.
    
%%%----------------------------------------------------------------
%% Remove characters from W as long as they belong to the class Class
%% (vowels or consonants)

take(Class,W) -> Class = vc_class(W), skip(Class,tl(W)).

skip(Class,W) -> case vc_class(W) of
		     Class -> skip(Class,tl(W));
		     _ -> W
		 end.
		      
classtst([C|Cs], W=[_|Ws]) -> 
    case vc_class(W) of
	C when Cs=/=[] -> classtst(Cs,Ws);
	C when Cs==[] -> true;
	_ -> false
    end;
classtst(_, _) -> false.
    
%%%----------------------------------------------------------------
vc_class([$a|_]) -> v;
vc_class([$e|_]) -> v;
vc_class([$i|_]) -> v;
vc_class([$o|_]) -> v;
vc_class([$u|_]) -> v;
vc_class("y") -> c;				% 'y' first in a word
vc_class([$y|T]) -> case vc_class(T) of		% check preceding character
			c -> v;
			_ -> c
		    end;
vc_class(_) -> c.

%%%----------------------------------------------------------------
'*v*'(W=[_|Ws]) -> case vc_class(W) of
		       v -> true;
		       _ -> '*v*'(Ws)
		   end;
'*v*'(_) -> false.

%%%----------------------------------------------------------------
'*o'("w"++_) -> false;
'*o'("x"++_) -> false;
'*o'("y"++_) -> false;
'*o'(W) -> classtst([c,v,c], W).

%%%----------------------------------------------------------------
'*d'(W) -> classtst([c,c], W).

%%%----------------------------------------------------------------
%%% Make the word lower case and return in reversed

lower_case(Word) -> lower_case(Word,[]).

lower_case([C|Cs], Acc) -> 
    if
	$A=<C, C=<$Z -> lower_case(Cs, [(C-$A+$a)|Acc]);
	true -> lower_case(Cs, [C|Acc])
    end;
lower_case([], Acc) ->
    Acc.

    





    
