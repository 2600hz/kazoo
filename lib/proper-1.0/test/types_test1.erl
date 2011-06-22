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
%%% @doc This module contains types for testing the typeserver.

-module(types_test1).
-export_type([exp1/0]).

-record(rec1, {a = 42 :: integer(), b :: float(), c = this_atom}).
-type rec1() :: #rec1{}.
-opaque exp1() :: rec1() | atom().
-type type1() :: {exp1(), [float() | boolean()]}.
-type type2(T) :: {T,T} | [T].
-type rem1() :: types_test2:exp1(integer()) | integer().
-type rem2() :: {bitstring(), types_test2:exp2()}.
