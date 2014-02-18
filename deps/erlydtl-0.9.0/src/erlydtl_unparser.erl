-module(erlydtl_unparser).
-export([unparse/1]).

unparse(DjangoParseTree) ->
    unparse(DjangoParseTree, []).

unparse([], Acc) ->
    lists:reverse(Acc);
unparse([{'extends', Value}|Rest], Acc) ->
    unparse(Rest, [["{% extends ", unparse_value(Value), " %}"]|Acc]);
unparse([{'autoescape', OnOrOff, Contents}|Rest], Acc) ->
    unparse(Rest, [["{% autoescape ", unparse_identifier(OnOrOff), " %}", unparse(Contents), "{% endautoescape %}"]|Acc]);
unparse([{'block', Identifier, Contents}|Rest], Acc) ->
    unparse(Rest, [["{% block ", unparse_identifier(Identifier), " %}", unparse(Contents), "{% endblock %}"]|Acc]);
unparse([{'blocktrans', [], Contents}|Rest], Acc) ->
    unparse(Rest, [["{% blocktrans %}", unparse(Contents), "{% endblocktrans %}"]|Acc]);
unparse([{'blocktrans', Args, Contents}|Rest], Acc) ->
    unparse(Rest, [["{% blocktrans ", unparse_args(Args), " %}", unparse(Contents), "{% endblocktrans %}"]|Acc]);
unparse([{'call', Identifier}|Rest], Acc) ->
    unparse(Rest, [["{% call ", unparse_identifier(Identifier), " %}"]|Acc]);
unparse([{'call', Identifier, With}|Rest], Acc) ->
    unparse(Rest, [["{% call ", unparse_identifier(Identifier), " with ", unparse_args(With), " %}"]|Acc]);
unparse([{'comment', Contents}|Rest], Acc) ->
    unparse(Rest, [["{% comment %}", unparse(Contents), "{% endcomment %}"]|Acc]);
unparse([{'cycle', Names}|Rest], Acc) ->
    unparse(Rest, [["{% cycle ", unparse(Names), " %}"]|Acc]);
unparse([{'cycle_compat', Names}|Rest], Acc) ->
    unparse(Rest, [["{% cycle ", unparse_cycle_compat_names(Names), " %}"]|Acc]);
unparse([{'date', 'now', Value}|Rest], Acc) ->
    unparse(Rest, [["{% now ", unparse_value(Value), " %}"]|Acc]);
unparse([{'filter', FilterList, Contents}|Rest], Acc) ->
    unparse(Rest, [["{% filter ", unparse_filters(FilterList), " %}", unparse(Contents), "{% endfilter %}"]|Acc]);
unparse([{'firstof', Vars}|Rest], Acc) ->
    unparse(Rest, [["{% firstof ", unparse(Vars), " %}"]|Acc]);
unparse([{'for', {'in', IteratorList, Identifier}, Contents}|Rest], Acc) ->
    unparse(Rest, [["{% for ", unparse_identifier(Identifier), " in ", unparse(IteratorList), " %}",
                    unparse(Contents),
                    "{% endfor %}"]|Acc]);
unparse([{'for', {'in', IteratorList, Identifier}, Contents, EmptyPartsContents}|Rest], Acc) ->
    unparse(Rest, [["{% for ", unparse_identifier(Identifier), " in ", unparse(IteratorList), " %}",
                    unparse(Contents),
                    "{% empty %}",
                    unparse(EmptyPartsContents),
                    "{% endfor %}"]|Acc]);
unparse([{'if', Expression, Contents}|Rest], Acc) ->
    unparse(Rest, [["{% if ", unparse_expression(Expression), " %}",
                    unparse(Contents),
                    "{% endif %}"]|Acc]);
unparse([{'ifchanged', Expression, IfContents}|Rest], Acc) ->
    unparse(Rest, [["{% ifchanged ", unparse_expression(Expression), " %}",
                    unparse(IfContents),
                    "{% endifchanged %}"]|Acc]);
unparse([{'ifchangedelse', Expression, IfContents, ElseContents}|Rest], Acc) ->
    unparse(Rest, [["{% ifchanged ", unparse_expression(Expression), " %}",
                    unparse(IfContents),
                    "{% else %}",
                    unparse(ElseContents),
                    "{% endifchanged %}"]|Acc]);
unparse([{'ifelse', Expression, IfContents, ElseContents}|Rest], Acc) ->
    unparse(Rest, [["{% if ", unparse_expression(Expression), " %}",
                    unparse(IfContents),
                    "{% else %}",
                    unparse(ElseContents),
                    "{% endif %}"]|Acc]);
unparse([{'ifequal', [Arg1, Arg2], Contents}|Rest], Acc) ->
    unparse(Rest, [["{% ifequal ", unparse_value(Arg1), " ", unparse_value(Arg2), " %}",
                    unparse(Contents),
                    "{% endifequal %}"]|Acc]);
unparse([{'ifequalelse', [Arg1, Arg2], IfContents, ElseContents}|Rest], Acc) ->
    unparse(Rest, [["{% ifequal ", unparse_value(Arg1), " ", unparse_value(Arg2), " %}",
                    unparse(IfContents),
                    "{% else %}",
                    unparse(ElseContents),
                    "{% endifequal %}"]|Acc]);
unparse([{'ifnotequal', [Arg1, Arg2], Contents}|Rest], Acc) ->
    unparse(Rest, [["{% ifnotequal ", unparse_value(Arg1), " ", unparse_value(Arg2), " %}",
                    unparse(Contents),
                    "{% endifnotequal %}"]|Acc]);
unparse([{'ifnotequalelse', [Arg1, Arg2], IfContents, ElseContents}|Rest], Acc) ->
    unparse(Rest, [["{% ifnotequal ", unparse_value(Arg1), " ", unparse_value(Arg2), " %}",
                    unparse(IfContents),
                    "{% else %}",
                    unparse(ElseContents),
                    "{% endifnotequal %}"]|Acc]);
unparse([{'include', Value, []}|Rest], Acc) ->
    unparse(Rest, [["{% include ", unparse_value(Value), " %}"]|Acc]);
unparse([{'include', Value, Args}|Rest], Acc) ->
    unparse(Rest, [["{% include ", unparse_value(Value), " with ", unparse_args(Args)]|Acc]);
unparse([{'include_only', Value, []}|Rest], Acc) ->
    unparse(Rest, [["{% include ", unparse_value(Value), " only %}"]|Acc]);
unparse([{'include_only', Value, Args}|Rest], Acc) ->
    unparse(Rest, [["{% include ", unparse_value(Value), " with ", unparse_args(Args), " only %}"]|Acc]);
unparse([{'regroup', {Variable, Identifier1, Identifier2}, Contents}|Rest], Acc) ->
    unparse(Rest, [["{% regroup ", unparse_value(Variable), " by ", unparse_identifier(Identifier1), " as ", unparse_identifier(Identifier2), " %}",
                    unparse(Contents),
                    "{% endregroup %}"]|Acc]);
unparse([{'spaceless', Contents}|Rest], Acc) ->
    unparse(Rest, [["{% spaceless %}", unparse(Contents), "{% endspaceless %}"]|Acc]);
unparse([{'ssi', Arg}|Rest], Acc) ->
    unparse(Rest, [["{% ssi ", unparse_value(Arg), " %}"]|Acc]);
unparse([{'ssi_parsed', Arg}|Rest], Acc) ->
    unparse(Rest, [["{% ssi ", unparse_value(Arg), " parsed %}"]|Acc]);
unparse([{'string', _, String}|Rest], Acc) ->
    unparse(Rest, [[String]|Acc]);
unparse([{'tag', Identifier, []}|Rest], Acc) ->
    unparse(Rest, [["{% ", unparse_identifier(Identifier), " %}"]|Acc]);
unparse([{'tag', Identifier, Args}|Rest], Acc) ->
    unparse(Rest, [["{% ", unparse_identifier(Identifier), " ", unparse_args(Args), " %}"]|Acc]);
unparse([{'templatetag', Identifier}|Rest], Acc) ->
    unparse(Rest, [["{% templatetag ", unparse_identifier(Identifier), " %}"]|Acc]);
unparse([{'trans', Value}|Rest], Acc) ->
    unparse(Rest, [["{% trans ", unparse_value(Value), " %}"]|Acc]);
unparse([{'widthratio', Numerator, Denominator, Scale}|Rest], Acc) ->
    unparse(Rest, [["{% widthratio ", unparse_value(Numerator), " ", unparse_value(Denominator), " ", unparse_value(Scale), " %}"]|Acc]);
unparse([{'with', Args, Contents}|Rest], Acc) ->
    unparse(Rest, [["{% with ", unparse_args(Args), " %}",
                    unparse(Contents),
                    "{% endwidth %}"]|Acc]);
unparse([ValueToken|Rest], Acc) ->
    unparse(Rest, [["{{ ", unparse_value(ValueToken), " }}"]|Acc]).


unparse_identifier({identifier, _, Name}) ->
    atom_to_list(Name).

unparse_filters(FilterList) ->
    unparse_filters(FilterList, []).

unparse_filters([], Acc) ->
    lists:reverse(Acc);
unparse_filters([Filter], Acc) ->
    unparse_filters([], [unparse_filter(Filter)|Acc]);
unparse_filters([Filter|Rest], Acc) ->
    unparse_filters(Rest, lists:reverse([unparse_filter(Filter), "|"], Acc)).

unparse_filter([Identifier]) ->
    unparse_identifier(Identifier);
unparse_filter([Identifier, Arg]) ->
    [unparse_identifier(Identifier), ":", unparse_value(Arg)].

unparse_expression({'expr', "in", Arg1, Arg2}) ->
    [unparse_value(Arg1), " in ", unparse_value(Arg2)];
unparse_expression({'expr', "not", {'expr', "in", Arg1, Arg2}}) ->
    [unparse_value(Arg1), " not in ", unparse_value(Arg2)];
unparse_expression({'expr', "not", Expr}) ->
    ["not ", unparse_expression(Expr)];
unparse_expression({'expr', "eq", Arg1, Arg2}) ->
    [unparse_value(Arg1), " == ", unparse_value(Arg2)];
unparse_expression({'expr', "ne", Arg1, Arg2}) ->
    [unparse_value(Arg1), " != ", unparse_value(Arg2)];
unparse_expression({'expr', "ge", Arg1, Arg2}) ->
    [unparse_value(Arg1), " >= ", unparse_value(Arg2)];
unparse_expression({'expr', "le", Arg1, Arg2}) ->
    [unparse_value(Arg1), " <= ", unparse_value(Arg2)];
unparse_expression({'expr', "gt", Arg1, Arg2}) ->
    [unparse_value(Arg1), " > ", unparse_value(Arg2)];
unparse_expression({'expr', "lt", Arg1, Arg2}) ->
    [unparse_value(Arg1), " < ", unparse_value(Arg2)];
unparse_expression({'expr', "or", Arg1, Arg2}) ->
    [unparse_expression(Arg1), " or ", unparse_expression(Arg2)];
unparse_expression({'expr', "and", Arg1, Arg2}) ->
    [unparse_expression(Arg1), " and ", unparse_expression(Arg2)];
unparse_expression(Other) ->
    unparse_value(Other).

unparse_value({'string_literal', _, Value}) ->
    Value;
unparse_value({'number_literal', _, Value}) ->
    Value;
unparse_value({'apply_filter', Variable, Filter}) ->
    [unparse_value(Variable), "|", unparse_filter(Filter)];
unparse_value({'attribute', {Variable, Identifier}}) ->
    [unparse_value(Variable), ".", unparse_identifier(Identifier)];
unparse_value({'variable', Identifier}) ->
    unparse_identifier(Identifier).

unparse_args(Args) ->
    unparse_args(Args, []).

unparse_args([], Acc) ->
    lists:reverse(Acc);
unparse_args([{{identifier, _, Name}, Value}], Acc) ->
    unparse_args([], [[atom_to_list(Name), "=", unparse_value(Value)]|Acc]);
unparse_args([{{identifier, _, Name}, Value}|Rest], Acc) ->
    unparse_args(Rest, lists:reverse([[atom_to_list(Name), "=", unparse_value(Value)], " "], Acc)).

unparse_cycle_compat_names(Names) ->
    unparse_cycle_compat_names(Names, []).

unparse_cycle_compat_names([], Acc) ->
    lists:reverse(Acc);
unparse_cycle_compat_names([{identifier, _, Name}], Acc) ->
    unparse_cycle_compat_names([], [atom_to_list(Name)|Acc]);
unparse_cycle_compat_names([{identifier, _, Name}|Rest], Acc) ->
    unparse_cycle_compat_names(Rest, lists:reverse([atom_to_list(Name), ", "], Acc)).
