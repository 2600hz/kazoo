%%% Copyright (C) 2006 - 2008 Willem de Jong
%%%
%%% This file is part of Erlsom.
%%%
%%% Erlsom is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as 
%%% published by the Free Software Foundation, either version 3 of 
%%% the License, or (at your option) any later version.
%%%
%%% Erlsom is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public 
%%% License along with Erlsom.  If not, see 
%%% <http://www.gnu.org/licenses/>.
%%%
%%% Author contact: w.a.de.jong@gmail.com

%%% ====================================================================
%%% Header file for erlsom
%%% ====================================================================

%% prefix=the prefix that will be used in the result
-record(ns, {uri, prefix}).
-record(qname, {uri, localPart, prefix, mappedPrefix}).
