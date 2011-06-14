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
%%% @doc This module tests a weird scenario for the parse transform.

-module(weird_types).
-export([]).
-export_type([foo/0]).
-compile(export_all).
-compile([{no_auto_import,[hd/1]}]).

-include_lib("proper/include/proper.hrl").

-type foo() :: atom().
foo() -> integer().
-type hd(T) :: {'head',T}.

prop_export_all_works() ->
    ?FORALL(X, ?MODULE:foo(), is_integer(X)).

prop_no_auto_import_works() ->
    ?FORALL(X, hd([42]), is_tuple(X)).
