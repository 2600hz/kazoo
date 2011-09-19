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
%%% @doc User header file: This file should be included in each file containing
%%%      user type declarations and/or properties to be tested.

-include_lib("proper/include/proper_common.hrl").

-ifndef(PROPER_NO_IMPORTS).


%%------------------------------------------------------------------------------
%% Test generation functions
%%------------------------------------------------------------------------------

-import(proper, [numtests/2, fails/1, on_output/2, conjunction/1]).
-import(proper, [collect/2, collect/3, aggregate/2, aggregate/3, classify/3,
		 measure/3, with_title/1, equals/2]).


%%------------------------------------------------------------------------------
%% Basic types
%%------------------------------------------------------------------------------

-import(proper_types, [integer/2, float/2, atom/0, binary/0, binary/1,
		       bitstring/0, bitstring/1, list/1, vector/2, union/1,
		       weighted_union/1, tuple/1, loose_tuple/1, exactly/1,
		       fixed_list/1, function/2, any/0]).


%%------------------------------------------------------------------------------
%% Type aliases
%%------------------------------------------------------------------------------

-import(proper_types, [integer/0, non_neg_integer/0, pos_integer/0,
		       neg_integer/0, range/2, float/0, non_neg_float/0,
		       number/0, boolean/0, byte/0, char/0, list/0, tuple/0,
		       string/0, wunion/1, term/0, timeout/0, arity/0]).
-import(proper_types, [int/0, nat/0, largeint/0, real/0, bool/0, choose/2,
		       elements/1, oneof/1, frequency/1, return/1, default/2,
		       orderedlist/1, function0/1, function1/1, function2/1,
		       function3/1, function4/1, weighted_default/2, parameter/1,
		       parameter/2, with_parameter/3, with_parameters/2]).


%%------------------------------------------------------------------------------
%% Type manipulation functions
%%------------------------------------------------------------------------------

-import(proper_types, [resize/2, non_empty/1, noshrink/1]).


%%------------------------------------------------------------------------------
%% Symbolic generation functions
%%------------------------------------------------------------------------------

-import(proper_symb, [eval/1, eval/2, defined/1, well_defined/1, pretty_print/1,
		      pretty_print/2]).


%%------------------------------------------------------------------------------
%% Statem functions
%%------------------------------------------------------------------------------

-import(proper_statem, [commands/1, commands/2, parallel_commands/1,
			parallel_commands/2, more_commands/2]).
-import(proper_statem, [run_commands/2, run_commands/3,  state_after/2,
			command_names/1, zip/2, run_parallel_commands/2,
			run_parallel_commands/3]).

-endif.


%%------------------------------------------------------------------------------
%% Enable the PropEr parse transformer
%%------------------------------------------------------------------------------

-ifndef(PROPER_NO_TRANS).
-ifdef(PROPER_REMOVE_PROPS).
-compile({parse_transform, proper_prop_remover}).
-else.
-compile({parse_transform, proper_transformer}).
-endif.
-endif.
