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
%%% @doc Complementary user header file: This file can be included in any
%%%      module, to allow for the use of parametric versions of some common
%%%      opaque datatypes from STDLIB. PropEr will recognize these types as
%%%      opaques and treat them accordingly. This is meant as a temporary
%%%      measure until Dialyzer implements support for parametric opaques.


%%------------------------------------------------------------------------------
%% Type declarations
%%------------------------------------------------------------------------------

-type array(_T) :: array().
-type dict(_K,_V) :: dict().
-type gb_set(_T) :: gb_set().
-type gb_tree(_K,_V) :: gb_tree().
-type orddict(K,V) :: [{K,V}].
-type ordset(T) :: [T].
-type queue(_T) :: queue().
-type set(_T) :: set().
