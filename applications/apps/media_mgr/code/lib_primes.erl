%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(lib_primes).

-export([make_prime/1, is_prime/1, make_random_int/1]).

%% Make a prime with at least K decimal digits. Here
%% we use 'Bertrand's postulate. Bertrand's postulate
%% is that for every N > 3, there is a prime P
%% satisfying N < P < 2N - 2. This was proved by 
%% Tchebychef in 1850. (Erdos improved this proof in 1932)

make_prime(1) ->
    lists:nth(random:uniform(4), [2,3,5,7]);
make_prime(K) when K > 0 ->
    new_seed(),
    N = make_random_int(K),
    if N > 3 ->
	    io:format("Generating a ~w digit prime ",[K]),
	    MaxTries = N - 3,
	    P1 = make_prime(MaxTries, N+1),
	    io:format("~n",[]),
	    P1;
	true ->
	    make_prime(K)
    end.

make_prime(0, _) ->
    exit(impossible);
make_prime(K, P) ->
    io:format(".",[]),
    case is_prime(P) of
	true  -> P;
	false -> make_prime(K-1, P+1)
    end.

%% Fermat's little theorem says that if 
%% N is a prime and if A < N then
%% A^N mod N = A

is_prime(D) when D < 10 ->
    lists:member(D, [2,3,5,7]);
is_prime(D) ->
    new_seed(),
    is_prime(D, 100).

is_prime(D, Ntests) ->
    N = length(integer_to_list(D)) -1,
    is_prime(Ntests, D, N).

is_prime(0, _, _) -> true;
is_prime(Ntest, N, Len) ->
    K = random:uniform(Len),
    %% A is a random number less than N 
    A = make_random_int(K),
    if 
	A < N ->
	    case lib_lin:pow(A,N,N) of
		A -> is_prime(Ntest-1,N,Len);
		_ -> false
	    end;
	true ->
	    is_prime(Ntest, N, Len)
    end.


%% make_random_int(N) -> a random integer with N digits.


make_random_int(N) -> new_seed(), make_random_int(N, 0).

make_random_int(0, D) -> D;
make_random_int(N, D) ->
    make_random_int(N-1, D*10 + (random:uniform(10)-1)).
%END:make_ran_int


new_seed() ->
    {_,_,X} = erlang:now(),
    {H,M,S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1,M1,S1}).






