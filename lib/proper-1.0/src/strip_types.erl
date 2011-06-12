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

%%% @doc A parse transform that removes all type-related annotations from a
%%% module. Its intended use within PropEr is to allow the main application
%%% modules, which make heavy use of recursive types, to compile on versions of
%%% the Erlang/OTP distribution older than R13B04. To enable this
%%% transformation, add ``{d,'NO_TYPES'}'' to the option `erl_opts' inside
%%% `rebar.config'.

-module(strip_types).
-export([parse_transform/2]).

%% We need to strip the following:
%% -export_type declarations:
%%	{attribute,LINE,export_type,_}
%% -type attributes:
%%	{attribute,LINE,type,_}
%% -opaque attributes:
%%	{attribute,LINE,opaque,_}
%% record field types:
%%	stored separately from the record declaration, in a -type attribute
%% -spec attributes:
%%	{attribute,LINE,spec,_}
%% -callback attributes:
%%	{attribute,LINE,callback,_}
-define(ATTRS_TO_STRIP, [export_type,type,opaque,spec,callback]).

%% @private
parse_transform(Forms, _Options) ->
    strip_types(Forms, []).

strip_types([], Acc) ->
    lists:reverse(Acc);
strip_types([{attribute,_,Kind,_} = Attr | Rest], Acc) ->
    case lists:member(Kind, ?ATTRS_TO_STRIP) of
	true ->
	    strip_types(Rest, Acc);
	false ->
	    strip_types(Rest, [Attr | Acc])
    end;
strip_types([Form | Rest], Acc) ->
    strip_types(Rest, [Form | Acc]).
