%%%-------------------------------------------------------------------
%%% File:      slex_parser.yrl
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2013 Andreas Stenius
%%% @doc
%%% slex parser
%%% @end
%%%
%%% Copyright 2013 Andreas Stenius
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @since 2013-11-05 by Andreas Stenius
%%%-------------------------------------------------------------------

Nonterminals
  action actions arg args attr comments guard prefix rule rule_body
  prio scanner scanner_exp state state_in state_new state_next states
  tag tag_body tag_head tag_op.
  
Terminals
  '+' '-' ':' ',' '.'
  any code comment identifier number skip string until.

Rootsymbol
  scanner.

Expect 1.

%% ----------------------------------------
%% ----------------------------------------

scanner -> scanner_exp : ['$1'].
scanner -> scanner_exp scanner : ['$1' | '$2'].

%% A Scanner is made up of `attr' `rule', `tag' and `form' expressions.
scanner_exp -> attr : {attr, '$1'}.
scanner_exp -> rule : {rule, '$1'}.
scanner_exp -> tag  : {tag, '$1'}.
scanner_exp -> comments code '.' : {form, {value_of('$2'), '$1'}}.

comments -> comment comments : [value_of('$1') | '$2'].
comments -> '$empty' : [].

%% ----------------------------------------
%% `attr'
%% ----------------------------------------
attr -> comments '-' identifier args '.' : {value_of('$3'), {'$4', '$1'}}.


%% ----------------------------------------
%% `rule'
%% ----------------------------------------
rule -> comments prio prefix state_in guard ':' rule_body '.'
  : {rule, '$2', '$3', '$4', '$5', '$7', '$1'}.

prio -> number : {prio, value_of('$1')}.

prefix -> string : {prefix, value_of('$1')}.
prefix -> any : any_prefix.
prefix -> ':' : end_of_input.

state_in -> state : '$1'.
state_in -> any : any_state.
state_in -> any '+' : close_any_state.
state_in -> any '-' : any_stateless.

rule_body -> actions : {'$1', keep_state}.
rule_body -> state_new : {[], '$1'}.
rule_body -> actions ',' state_next : {'$1', '$3'}.
rule_body -> code : {code, value_of('$1')}.

actions -> action actions : ['$1' | '$2'].
actions -> skip : [].
actions -> '$empty' : [].

action -> identifier : {add, value_of('$1')}.
action -> identifier '-' string : {add, value_of('$1'), value_of('$3')}.
action -> '+' identifier : {append, value_of('$2')}.
action -> '+' identifier '-' string : {append, value_of('$2'), value_of('$4')}.
action -> string : {add, value_of('$1')}.

state_next -> state : '$1'.
state_next -> state_new : '$1'.

state_new -> identifier until string : {state, {value_of('$1'), value_of('$3')}}.


%% ----------------------------------------
%% `tag'
%% ----------------------------------------
tag -> comments tag_head guard ':' tag_body '.'
  : {tag, '$2', '$3', '$5', '$1'}.

tag_head -> states : lists:reverse('$1').
tag_head -> states ',' state : {lists:reverse('$1'), '$3'}.

tag_body -> tag_op : ['$1'].
tag_body -> tag_op ',' tag_body : ['$1' | '$3'].

tag_op -> args : '$1'.
tag_op -> code : {code, value_of('$1')}.

states -> state : ['$1'].
states -> state states : ['$1' | '$2'].


%% ----------------------------------------
%% Common rules
%% ----------------------------------------

args -> arg : ['$1'].
args -> arg args : ['$1' | '$2'].

arg -> identifier : value_of('$1').
arg -> string : value_of('$1').

guard -> ',' code : {guard, value_of('$2')}.
guard -> '$empty' : {guard, []}.

state -> identifier : {state, value_of('$1')}.
state -> identifier '-' : {stateless, value_of('$1')}.


%% ----------------------------------------
Erlang code.
%% ----------------------------------------

value_of({_, _, Value}) -> Value.
