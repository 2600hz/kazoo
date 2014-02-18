%%% -*- mode: erlang -*- ------------------------------------------------------------------
%%% File:      erlydtl_parser.erl
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2013 Andreas Stenius
%%% @doc Sample extension grammar
%%% @reference  See <a href="http://erlydtl.googlecode.com" target="_top">http://erlydtl.googlecode.com</a> for more information
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2013 Andreas Stenius
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2013-06-20 by Andreas Stenius
%%%-------------------------------------------------------------------

Nonterminals
    Extensions
    Literal

    ValueExpressionBraced

    ValueExpression
    Value
    Variable
.
    
Terminals
    %% "new" terminals that are partially parsed tokens from the erlydtl parser:
    variable

    %% standard scanner tokens:

    %% and_keyword
    %% as_keyword
    %% autoescape_keyword
    %% block_keyword
    %% blocktrans_keyword
    %% by_keyword
    %% call_keyword
    %% close_tag
    close_var
    %% comment_keyword
    %% cycle_keyword
    %% elif_keyword
    %% else_keyword
    %% empty_keyword
    %% endautoescape_keyword
    %% endblock_keyword
    %% endblocktrans_keyword
    %% endcomment_keyword
    %% endfilter_keyword
    %% endfor_keyword
    %% endif_keyword
    %% endifchanged_keyword
    %% endifequal_keyword
    %% endifnotequal_keyword
    %% endregroup_keyword
    %% endspaceless_keyword
    %% endwith_keyword
    %% extends_keyword
    %% filter_keyword
    %% firstof_keyword
    %% for_keyword
    identifier
    %% if_keyword
    %% ifchanged_keyword
    %% ifequal_keyword
    %% ifnotequal_keyword
    %% in_keyword
    %% include_keyword
    %% noop_keyword
    %% not_keyword
    %% now_keyword
    number_literal
    %% only_keyword
    or_keyword
    %% open_tag
    open_var
    %% parsed_keyword
    %% regroup_keyword
    %% reversed_keyword
    %% spaceless_keyword
    %% ssi_keyword
    string_literal
    %% string
    %% templatetag_keyword
    %% openblock_keyword
    %% closeblock_keyword
    %% openvariable_keyword
    %% closevariable_keyword
    %% openbrace_keyword
    %% closebrace_keyword
    %% opencomment_keyword
    %% closecomment_keyword
    %% trans_keyword
    %% widthratio_keyword
    %% with_keyword
    %% ',' '|' '=' ':' 
    '.'
    %% '==' '!='
    %% '>=' '<='
    %% '>' '<'
    %% '(' ')'
    %% '_'
.

Rootsymbol
    Extensions.

%% Operator precedences for the E non terminal
Left 100 or_keyword.
%Left 110 and_keyword.
%Nonassoc 300 '==' '!=' '>=' '<=' '>' '<'.
%Unary 600 Unot.

Extensions -> ValueExpressionBraced : ['$1'].

ValueExpressionBraced -> open_var ValueExpression close_var : '$2'.

ValueExpression -> Value or_keyword Value : {extension, {value_or, {'$1', '$3'}}}.
    
%Value -> Value '|' Filter : {apply_filter, '$1', '$3'}.
%Value -> '_' '(' Value ')' : {trans, '$3'}.
Value -> Variable : '$1'.
Value -> Literal : '$1'.
    
Variable -> identifier : {variable, '$1'}.
Variable -> variable : '$1'.
Variable -> Variable '.' identifier : {attribute, {'$3', '$1'}}.

Literal -> string_literal : '$1'.
Literal -> number_literal : '$1'.


%% vim: syntax=erlang
