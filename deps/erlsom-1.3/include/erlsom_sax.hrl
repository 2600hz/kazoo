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
%%% data structures produced by erlsom_sax
%%% ====================================================================

%% data structures produced by sax.erl.

-record(attribute, {localName, prefix = [], uri = [], value}).

-record(erlsom_sax_state, 
  {user_state, 
   callback,
   continuation_state,
   entities = [], 
   par_entities = [], 
   current_entity = '__top',
   namespaces = [], 
   endtags = [],
   output, %% determines the encoding of text and atttribute values
   continuation_fun,
   %% entity_relations is used to check on circular
   %% definitions
   entity_relations = []}).

%% useful macro approach copied from xmerl
-define(space, 32).
-define(cr,    13).
-define(lf,    10).
-define(tab,   9).

%% whitespace consists of 'space', 'carriage return', 'line feed' or 'tab'
-define(is_whitespace(C), 
        C =:= ?space; C =:= ?cr ; C =:= ?lf; C =:= ?tab).

-define(is_namestart_char(C), 
        C > 96, C < 123; C > 64, C < 91; C =:= $_).

-define(is_namestart_char2(C),          %%  also for characters <> 7 bit asscii
        C > 96, C < 123; C > 64, C < 91; C =:= $_;
        C > 191, C =/= 215, C =/= 247). %% this check is far from complete!

-define(is_name_char(C), 
        C > 96, C < 123; 
        C > 64, C < 91; 
        C > 47, C < 58;
        C =:= $_;
        C =:= $-;
        C =:= $.).

-define(is_name_char2(C),             %% also for characters <> 7 bit ascii
        C > 96, C < 123; 
        C > 64, C < 91; 
        C > 47, C < 58;
        C =:= $_;
        C =:= $-;
        C =:= $.;
        C > 191, C /= 215, C /= 247). %% this check is far from complete!
