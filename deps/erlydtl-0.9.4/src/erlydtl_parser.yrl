%%% -*- mode: erlang -*- ------------------------------------------------------------------
%%% File:      erlydtl_parser.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc Template language grammar
%%% @reference  See <a href="http://erlydtl.googlecode.com" target="_top">http://erlydtl.googlecode.com</a> for more information
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
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
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------

Nonterminals
    Elements
    Literal

    ValueBraced

    Value
    Values
    Variable
    Filter
    FilterArg

    AutoEscapeBlock
    AutoEscapeBraced
    EndAutoEscapeBraced

    BlockBlock
    BlockBraced
    EndBlockBraced

    CommentTag
    CommentBlock
    CommentBraced
    EndCommentBraced

    CycleTag
    CycleNames
    CycleNamesCompat

    ExtendsTag
    IncludeTag
    NowTag

    FirstofTag

    FilterBlock
    FilterBraced
    EndFilterBraced
    Filters

    ForBlock
    ForBraced
    EmptyBraced
    EndForBraced
    ForExpression
    ForGroup

    IfBlock
    IfBraced
    ElifBlock
    ElifBraced
    IfExpression
    ElseBraced
    EndIfBraced
    
    IfChangedBlock
    IfChangedBraced
    EndIfChangedBraced

    IfEqualBlock
    IfEqualBraced
    IfEqualExpression
    EndIfEqualBraced  
    
    IfNotEqualBlock
    IfNotEqualBraced
    IfNotEqualExpression
    EndIfNotEqualBraced      

    CustomTag
    CustomArgs
    Arg
    Args

    RegroupTag

    SpacelessBlock

    SSITag

    BlockTransBlock
    BlockTransBraced
    EndBlockTransBraced
    BlockTransArgs
    BlockTransContents

    PluralTag

    TransTag
    TransArgs
    TransText
    TransValue

    TemplatetagTag
    Templatetag

    WidthRatioTag

    WithBlock
    WithBraced
    EndWithBraced

    CallTag
    CallWithTag

    LoadTag
    LoadArgs

    Unot.

Terminals
    and_keyword
    as_keyword
    autoescape_keyword
    block_keyword
    blocktrans_keyword
    by_keyword
    call_keyword
    close_tag
    close_var
    comment_tag
    comment_keyword
    context_keyword
    count_keyword
    cycle_keyword
    elif_keyword
    else_keyword
    empty_keyword
    endautoescape_keyword
    endblock_keyword
    endblocktrans_keyword
    endcomment_keyword
    endfilter_keyword
    endfor_keyword
    endif_keyword
    endifchanged_keyword
    endifequal_keyword
    endifnotequal_keyword
    endregroup_keyword
    endspaceless_keyword
    endwith_keyword
    extends_keyword
    filter_keyword
    firstof_keyword
    for_keyword
    from_keyword
    identifier
    if_keyword
    ifchanged_keyword
    ifequal_keyword
    ifnotequal_keyword
    in_keyword
    include_keyword
    load_keyword
    noop_keyword
    not_keyword
    now_keyword
    number_literal
    only_keyword
    or_keyword
    open_tag
    open_var
    parsed_keyword
    plural_keyword
    regroup_keyword
    reversed_keyword
    spaceless_keyword
    ssi_keyword
    string_literal
    string
    templatetag_keyword
    openblock_keyword
    closeblock_keyword
    openvariable_keyword
    closevariable_keyword
    openbrace_keyword
    closebrace_keyword
    opencomment_keyword
    closecomment_keyword
    trans_keyword
    widthratio_keyword
    with_keyword
    ',' '|' '=' ':' '.'
    '==' '!='
    '>=' '<='
    '>' '<'
    '(' ')'
    '_'.

Rootsymbol
    Elements.

%% Operator precedences for the E non terminal
Left 100 or_keyword.
Left 110 and_keyword.
Nonassoc 300 '==' '!=' '>=' '<=' '>' '<'.
Unary 600 Unot.

Elements -> '$empty' : [].
Elements -> Elements string : '$1' ++ ['$2'].
Elements -> Elements AutoEscapeBlock : '$1' ++ ['$2'].
Elements -> Elements BlockBlock : '$1' ++ ['$2'].
Elements -> Elements BlockTransBlock : '$1' ++ ['$2'].
Elements -> Elements CallTag : '$1' ++ ['$2'].
Elements -> Elements CallWithTag : '$1' ++ ['$2'].
Elements -> Elements CommentBlock : '$1' ++ ['$2'].
Elements -> Elements CommentTag : '$1' ++ ['$2'].
Elements -> Elements CustomTag : '$1' ++ ['$2'].
Elements -> Elements CycleTag : '$1' ++ ['$2'].
Elements -> Elements ExtendsTag : '$1' ++ ['$2'].
Elements -> Elements FilterBlock : '$1' ++ ['$2'].
Elements -> Elements FirstofTag : '$1' ++ ['$2'].
Elements -> Elements ForBlock : '$1' ++ ['$2'].
Elements -> Elements IfBlock : '$1' ++ ['$2'].
Elements -> Elements IfEqualBlock : '$1' ++ ['$2'].
Elements -> Elements IfNotEqualBlock : '$1' ++ ['$2'].
Elements -> Elements IfChangedBlock : '$1' ++ ['$2'].
Elements -> Elements IncludeTag : '$1' ++ ['$2'].
Elements -> Elements LoadTag : '$1' ++ ['$2'].
Elements -> Elements NowTag : '$1' ++ ['$2'].
Elements -> Elements RegroupTag : '$1' ++ ['$2'].
Elements -> Elements SpacelessBlock : '$1' ++ ['$2'].
Elements -> Elements SSITag : '$1' ++ ['$2'].
Elements -> Elements TemplatetagTag : '$1' ++ ['$2'].
Elements -> Elements TransTag : '$1' ++ ['$2'].
Elements -> Elements ValueBraced : '$1' ++ ['$2'].
Elements -> Elements WidthRatioTag : '$1' ++ ['$2'].
Elements -> Elements WithBlock : '$1' ++ ['$2'].

ValueBraced -> open_var Value close_var : '$2'.

Value -> Value '|' Filter : {apply_filter, '$1', '$3'}.
Value -> '_' '(' Value ')' : {trans, '$3'}.
Value -> Variable : '$1'.
Value -> Literal : '$1'.

Values -> Value : ['$1'].
Values -> Value Values : ['$1'|'$2'].

Filter -> identifier FilterArg : {'$1', '$2'}.

FilterArg -> '$empty' : [].
FilterArg -> ':' Variable : ['$2'].
FilterArg -> ':' Literal : ['$2'].

Variable -> identifier : {variable, '$1'}.
Variable -> Variable '.' identifier : {attribute, {'$3', '$1'}}.
Variable -> Variable '.' Literal : {attribute, {'$3', '$1'}}.

Literal -> string_literal : '$1'.
Literal -> number_literal : '$1'.

AutoEscapeBlock -> AutoEscapeBraced Elements EndAutoEscapeBraced : {autoescape, '$1', '$2'}.
AutoEscapeBraced -> open_tag autoescape_keyword identifier close_tag : '$3'.
EndAutoEscapeBraced -> open_tag endautoescape_keyword close_tag.

BlockBlock -> BlockBraced Elements EndBlockBraced : {block, '$1', '$2'}.
BlockBraced -> open_tag block_keyword identifier close_tag : '$3'.
EndBlockBraced -> open_tag endblock_keyword close_tag.

ExtendsTag -> open_tag extends_keyword string_literal close_tag : {extends, '$3'}.

IncludeTag -> open_tag include_keyword string_literal close_tag : {include, '$3', []}.
IncludeTag -> open_tag include_keyword string_literal with_keyword Args close_tag : {include, '$3', '$5'}.
IncludeTag -> open_tag include_keyword string_literal only_keyword close_tag : {include_only, '$3', []}.
IncludeTag -> open_tag include_keyword string_literal with_keyword Args only_keyword close_tag : {include_only, '$3', '$5'}.

LoadTag -> open_tag load_keyword LoadArgs close_tag : {load_libs, '$3'}.
LoadTag -> open_tag load_keyword LoadArgs from_keyword identifier close_tag : {load_from_lib, '$3', '$5'}.

LoadArgs -> '$empty' : [].
LoadArgs -> identifier LoadArgs : ['$1'|'$2'].

NowTag -> open_tag now_keyword string_literal close_tag : {date, now, '$3'}.

CommentBlock -> CommentBraced Elements EndCommentBraced : {comment, '$2'}.
CommentBraced -> open_tag comment_keyword close_tag.
EndCommentBraced -> open_tag endcomment_keyword close_tag.

CommentTag -> comment_tag : '$1'.

CycleTag -> open_tag cycle_keyword CycleNamesCompat close_tag : {cycle_compat, '$3'}.
CycleTag -> open_tag cycle_keyword CycleNames close_tag : {cycle, '$3'}.

CycleNames -> Value : ['$1'].
CycleNames -> CycleNames Value : '$1' ++ ['$2'].

CycleNamesCompat -> identifier ',' : ['$1'].
CycleNamesCompat -> CycleNamesCompat identifier ',' : '$1' ++ ['$2'].
CycleNamesCompat -> CycleNamesCompat identifier : '$1' ++ ['$2'].

FilterBlock -> FilterBraced Elements EndFilterBraced : {filter, '$1', '$2'}.
FilterBraced -> open_tag filter_keyword Filters close_tag : '$3'.
EndFilterBraced -> open_tag endfilter_keyword close_tag.

Filters -> Filter : ['$1'].
Filters -> Filter '|' Filters : ['$1'|'$3'].

FirstofTag -> open_tag firstof_keyword Values close_tag : {firstof, '$3'}.

ForBlock -> ForBraced Elements EndForBraced : {for, '$1', '$2'}.
ForBlock -> ForBraced Elements EmptyBraced Elements EndForBraced : {for, '$1', '$2', '$4'}.
EmptyBraced -> open_tag empty_keyword close_tag.
ForBraced -> open_tag for_keyword ForExpression close_tag : '$3'.
EndForBraced -> open_tag endfor_keyword close_tag.
ForExpression -> ForGroup in_keyword Value : {'in', '$1', '$3', false}.
ForExpression -> ForGroup in_keyword Value reversed_keyword : {'in', '$1', '$3', true}.
ForGroup -> identifier : ['$1'].
ForGroup -> ForGroup ',' identifier : '$1' ++ ['$3'].

IfBlock -> IfBraced Elements ElseBraced Elements EndIfBraced : {'ifelse', '$1', '$2', '$4'}.
IfBlock -> IfBraced Elements EndIfBraced : {'if', '$1', '$2'}.
IfBlock -> IfBraced Elements ElifBlock : {'if', '$1', '$2', ['$3']}.
ElifBlock -> ElifBraced Elements ElseBraced Elements EndIfBraced : {'ifelse', '$1', '$2', '$4'}.
ElifBlock -> ElifBraced Elements EndIfBraced : {'if', '$1', '$2'}.
ElifBlock -> ElifBraced Elements ElifBlock : {'if', '$1', '$2', ['$3']}.
IfBraced -> open_tag if_keyword IfExpression close_tag : '$3'.
ElifBraced -> open_tag elif_keyword IfExpression close_tag : '$3'.
IfExpression -> Value in_keyword Value : {'expr', "in", '$1', '$3'}.
IfExpression -> Value not_keyword in_keyword Value : {'expr', "not", {'expr', "in", '$1', '$4'}}.
IfExpression -> Value '==' Value : {'expr', "eq", '$1', '$3'}.
IfExpression -> Value '!=' Value : {'expr', "ne", '$1', '$3'}.
IfExpression -> Value '>=' Value : {'expr', "ge", '$1', '$3'}.
IfExpression -> Value '<=' Value : {'expr', "le", '$1', '$3'}.
IfExpression -> Value '>' Value : {'expr', "gt", '$1', '$3'}.
IfExpression -> Value '<' Value : {'expr', "lt", '$1', '$3'}.
IfExpression -> '(' IfExpression ')' : '$2'.
IfExpression -> Unot : '$1'.
IfExpression -> IfExpression or_keyword IfExpression : {'expr', "or", '$1', '$3'}.
IfExpression -> IfExpression and_keyword IfExpression : {'expr', "and", '$1', '$3'}.
IfExpression -> Value : '$1'.

Unot -> not_keyword IfExpression : {expr, "not", '$2'}.

ElseBraced -> open_tag else_keyword close_tag.
EndIfBraced -> open_tag endif_keyword close_tag.

IfChangedBlock -> IfChangedBraced Elements ElseBraced Elements EndIfChangedBraced : {ifchangedelse, '$1', '$2', '$4'}.
IfChangedBlock -> IfChangedBraced Elements EndIfChangedBraced : {ifchanged, '$1', '$2'}.
IfChangedBraced -> open_tag ifchanged_keyword close_tag.
IfChangedBraced -> open_tag ifchanged_keyword Values close_tag : '$3'.
EndIfChangedBraced -> open_tag endifchanged_keyword close_tag.

IfEqualBlock -> IfEqualBraced Elements ElseBraced Elements EndIfEqualBraced : {ifequalelse, '$1', '$2', '$4'}.
IfEqualBlock -> IfEqualBraced Elements EndIfEqualBraced : {ifequal, '$1', '$2'}.
IfEqualBraced -> open_tag ifequal_keyword IfEqualExpression Value close_tag : ['$3', '$4'].
IfEqualExpression -> Value : '$1'.
EndIfEqualBraced -> open_tag endifequal_keyword close_tag.

IfNotEqualBlock -> IfNotEqualBraced Elements ElseBraced Elements EndIfNotEqualBraced : {ifnotequalelse, '$1', '$2', '$4'}.
IfNotEqualBlock -> IfNotEqualBraced Elements EndIfNotEqualBraced : {ifnotequal, '$1', '$2'}.
IfNotEqualBraced -> open_tag ifnotequal_keyword IfNotEqualExpression Value close_tag : ['$3', '$4'].
IfNotEqualExpression -> Value : '$1'.
EndIfNotEqualBraced -> open_tag endifnotequal_keyword close_tag.

RegroupTag -> open_tag regroup_keyword Value by_keyword Value as_keyword identifier close_tag : {regroup, {'$3', '$5', '$7'}}.
RegroupTag -> open_tag endregroup_keyword close_tag : end_regroup.

SpacelessBlock -> open_tag spaceless_keyword close_tag Elements open_tag endspaceless_keyword close_tag : {spaceless, '$4'}.

SSITag -> open_tag ssi_keyword Value close_tag : {ssi, '$3'}.
SSITag -> open_tag ssi_keyword string_literal parsed_keyword close_tag : {ssi_parsed, '$3'}.

BlockTransBlock -> BlockTransBraced BlockTransContents EndBlockTransBraced : {blocktrans, '$1', '$2', undefined}.
BlockTransBlock -> BlockTransBraced BlockTransContents PluralTag BlockTransContents EndBlockTransBraced : {blocktrans, '$1', '$2', '$4'}.
BlockTransBraced -> open_tag blocktrans_keyword BlockTransArgs close_tag : '$3'.
EndBlockTransBraced -> open_tag endblocktrans_keyword close_tag.

BlockTransArgs -> '$empty' : [].
BlockTransArgs -> count_keyword Arg BlockTransArgs : [{count, '$2'}|'$3'].
BlockTransArgs -> with_keyword Args BlockTransArgs : [{args, '$2'}|'$3'].
BlockTransArgs -> context_keyword string_literal BlockTransArgs : [{context, '$2'}|'$3'].

BlockTransContents -> '$empty' : [].
BlockTransContents -> open_var identifier close_var BlockTransContents : [{variable, '$2'}|'$4'].
BlockTransContents -> string BlockTransContents : ['$1'|'$2'].

PluralTag -> open_tag plural_keyword close_tag.

TemplatetagTag -> open_tag templatetag_keyword Templatetag close_tag : {templatetag, '$3'}.

Templatetag -> openblock_keyword : '$1'.
Templatetag -> closeblock_keyword : '$1'.
Templatetag -> openvariable_keyword : '$1'.
Templatetag -> closevariable_keyword : '$1'.
Templatetag -> openbrace_keyword : '$1'.
Templatetag -> closebrace_keyword : '$1'.
Templatetag -> opencomment_keyword : '$1'.
Templatetag -> closecomment_keyword : '$1'.

TransTag -> open_tag trans_keyword TransArgs close_tag : '$3'.
TransTag -> open_tag trans_keyword TransArgs as_keyword identifier close_tag : {scope_as, '$5', ['$3']}.

TransArgs -> TransText : {trans, '$1'}.
TransArgs -> TransText context_keyword string_literal: {trans, '$1', '$3'}.

TransText -> TransValue : '$1'.
TransText -> TransValue noop_keyword : {noop, '$1'}.

TransValue -> string_literal : '$1'.
TransValue -> Variable : '$1'.

WidthRatioTag -> open_tag widthratio_keyword Value Value number_literal close_tag : {widthratio, '$3', '$4', '$5'}.

WithBlock -> WithBraced Elements EndWithBraced : {with, '$1', '$2'}.
WithBraced -> open_tag with_keyword Args close_tag : '$3'.
EndWithBraced -> open_tag endwith_keyword close_tag.

CustomTag -> open_tag identifier CustomArgs close_tag : {tag, '$2', '$3'}.

CustomArgs -> '$empty' : [].
CustomArgs -> identifier '=' Value CustomArgs : [{'$1', '$3'}|'$4'].
CustomArgs -> Value CustomArgs : ['$1'|'$2'].

Args -> '$empty' : [].
Args -> Arg Args : ['$1'|'$2'].

Arg -> identifier '=' Value : {'$1', '$3'}.
%% Arg -> identifier : {'$1', true}.

CallTag -> open_tag call_keyword identifier close_tag : {call, '$3'}.
CallWithTag -> open_tag call_keyword identifier with_keyword Value close_tag : {call, '$3', '$5'}.

Erlang code.

%% vim: syntax=erlang
