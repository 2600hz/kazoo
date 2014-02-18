-module(erlydtl_unittests).

-export([run_tests/0]).

-record(testrec, {foo, bar, baz}).

-ifndef(GRP_ERROR_REPORTING_COMPILER_OPTS).
-define(GRP_ERROR_REPORTING_COMPILER_OPTS,[]).
%%-define(GRP_ERROR_REPORTING_COMPILER_OPTS,[report]).
%% define GRP_ERROR_REPORTING_COMPILER_OPTS to [report] to print
%% tested error messages.
-endif.

tests() ->
    [
     %% {"scanner",
     %%  [{"multiline tags", %% weird formatting example from issue #103.
     %%    <<"{% if a \n"
     %%      "  %}{% if a.b \n"
     %%      "    %}{{ a.b \n"
     %%      "    }}{% endif\n"
     %%      "  %}{% endif\n"
     %%      "%}">>,
     %%    [{a, [{b, 123}]}],
     %%    <<"...">>} %% dtl compat: expect the whole input text, tags and all..
     %%  ]},
     {"vars", [
               {"string",
                <<"String value is: {{ var1 }}">>,
                [{var1, "foo"}], <<"String value is: foo">>},
               {"int",
                <<"The magic number is: {{ var1 }}">>,
                [{var1, 42}], <<"The magic number is: 42">>},
               {"float",
                <<"The price of milk is: {{ var1 }}">>,
                [{var1, 0.42}], <<"The price of milk is: 0.42">>},
               {"No spaces",
                <<"{{var1}}">>,
                [{var1, "foo"}], <<"foo">>},
               {"Variable name is a tag name",
                <<"{{ comment }}">>,
                [{comment, "Nice work!"}], <<"Nice work!">>}
              ]},
     {"comment", [
                  {"comment block is excised",
                   <<"bob {% comment %}(moron){% endcomment %} loblaw">>,
                   [], <<"bob  loblaw">>},
                  {"inline comment is excised",
                   <<"you're {# not #} a very nice person">>,
                   [], <<"you're  a very nice person">>}
                 ]},
     {"autoescape", [
                     {"Autoescape works",
                      <<"{% autoescape on %}{{ var1 }}{% endautoescape %}">>,
                      [{var1, "<b>bold</b>"}], <<"&lt;b&gt;bold&lt;/b&gt;">>},
                     {"Nested autoescape",
                      <<"{% autoescape on %}{{ var1 }}{% autoescape off %}{{ var1 }}{% endautoescape %}{% endautoescape %}">>,
                      [{var1, "<b>"}], <<"&lt;b&gt;<b>">>}
                    ]},
     {"string literal", [
                         {"Render literal",
                          <<"{{ \"foo\" }} is my name">>, [], <<"foo is my name">>},
                         {"Newlines are escaped",
                          <<"{{ \"foo\\n\" }}">>, [], <<"foo\n">>}
                        ]},
     {"cycle", [
                {"Cycling through quoted strings",
                 <<"{% for i in test %}{% cycle 'a' 'b' %}{{ i }},{% endfor %}">>,
                 [{test, ["0", "1", "2", "3", "4"]}], <<"a0,b1,a2,b3,a4,">>},
                {"Cycling through normal variables",
                 <<"{% for i in test %}{% cycle aye bee %}{{ i }},{% endfor %}">>,
                 [{test, ["0", "1", "2", "3", "4"]}, {aye, "a"}, {bee, "b"}],
                 <<"a0,b1,a2,b3,a4,">>}
               ]},
     {"number literal", [
                         {"Render integer",
                          <<"{{ 5 }}">>, [], <<"5">>}
                        ]},
     {"variable", [
                   {"Render variable",
                    <<"{{ var1 }} is my game">>, [{var1, "bar"}], <<"bar is my game">>},
                   {"Render variable with attribute",
                    <<"I enjoy {{ var1.game }}">>, [{var1, [{game, "Othello"}]}], <<"I enjoy Othello">>},
                   {"Render variable with string-key attribute",
                    <<"I also enjoy {{ var1.game }}">>, [{var1, [{"game", "Parcheesi"}]}], <<"I also enjoy Parcheesi">>},
                   {"Render variable with binary-key attribute",
                    <<"I also enjoy {{ var1.game }}">>, [{var1, [{<<"game">>, "Parcheesi"}]}], <<"I also enjoy Parcheesi">>},
                   {"Render variable in dict",
                    <<"{{ var1 }}">>, dict:store(var1, "bar", dict:new()), <<"bar">>},
                   {"Render variable with missing attribute in dict",
                    <<"{{ var1.foo }}">>, [{var1, dict:store(bar, "Othello", dict:new())}], <<"">>},
                   {"Render variable in gb_tree",
                    <<"{{ var1 }}">>, gb_trees:insert(var1, "bar", gb_trees:empty()), <<"bar">>},
                   {"Render variable in arity-1 func",
                    <<"I enjoy {{ var1 }}">>, fun (var1) -> "Othello" end, <<"I enjoy Othello">>},
                   {"Render variable with attribute in dict",
                    <<"{{ var1.attr }}">>, [{var1, dict:store(attr, "Othello", dict:new())}], <<"Othello">>},
                   {"Render variable with attribute in gb_tree",
                    <<"{{ var1.attr }}">>, [{var1, gb_trees:insert(attr, "Othello", gb_trees:empty())}], <<"Othello">>},
                   {"Render variable with attribute in arity-1 func",
                    <<"I enjoy {{ var1.game }}">>, [{var1, fun (game) -> "Othello" end}], <<"I enjoy Othello">>},
                   {"Render variable in parameterized module",
                    <<"{{ var1.some_var }}">>, [{var1, erlydtl_example_variable_storage:new("foo")}], <<"foo">>},
                   {"Nested attributes",
                    <<"{{ person.city.state.country }}">>, [{person, [{city, [{state, [{country, "Italy"}]}]}]}],
                    <<"Italy">>},
                   {"Index list variable",
                    <<"{{ var1.2 }}">>, [{var1, [a, b, c]}],
                    <<"b">>},
                   {"Index tuple variable",
                    <<"{{ var1.2 }}">>, [{var1, {a, b, c}}],
                    <<"b">>}
                  ]},
     {"now", [
              {"now functional",
               <<"It is the {% now \"jS \\o\\f F Y\" %}.">>, [{var1, ""}], generate_test_date()}
             ]},
     {"if", [
             {"If/else",
              <<"{% if var1 %}boo{% else %}yay{% endif %}">>, [{var1, ""}], <<"yay">>},
             {"If elif",
              <<"{% if var1 %}boo{% elif var2 %}yay{% endif %}">>, [{var1, ""}, {var2, "happy"}], <<"yay">>},
             {"If elif/else",
              <<"{% if var1 %}boo{% elif var2 %}sad{% else %}yay{% endif %}">>, [{var1, ""}, {var2, ""}], <<"yay">>},
             {"If elif/elif/else",
              <<"{% if var1 %}boo{% elif var2 %}yay{% elif var3 %}sad{% else %}noo{% endif %}">>, [{var1, ""},
                                                                                                   {var2, "happy"}, {var3, "not_taken"}], <<"yay">>},
             {"If",
              <<"{% if var1 %}boo{% endif %}">>, [{var1, ""}], <<>>},
             {"If not",
              <<"{% if not var1 %}yay{% endif %}">>, [{var1, ""}], <<"yay">>},
             {"If \"0\"",
              <<"{% if var1 %}boo{% endif %}">>, [{var1, "0"}], <<>>},
             {"If 0",
              <<"{% if var1 %}boo{% endif %}">>, [{var1, 0}], <<>>},
             {"If false",
              <<"{% if var1 %}boo{% endif %}">>, [{var1, false}], <<>>},
             {"If false string",
              <<"{% if var1 %}boo{% endif %}">>, [{var1, "false"}], <<"boo">>},
             {"If undefined",
              <<"{% if var1 %}boo{% endif %}">>, [{var1, undefined}], <<>>},
             {"If other atom",
              <<"{% if var1 %}yay{% endif %}">>, [{var1, foobar}], <<"yay">>},
             {"If non-empty string",
              <<"{% if var1 %}yay{% endif %}">>, [{var1, "hello"}], <<"yay">>},
             {"If proplist",
              <<"{% if var1 %}yay{% endif %}">>, [{var1, [{foo, "bar"}]}], <<"yay">>},
             {"If complex",
              <<"{% if foo.bar.baz %}omgwtfbbq{% endif %}">>, [], <<"">>}
            ]},
     {"if .. in ..", [
                      {"If substring in string",
                       <<"{% if var1 in var2 %}yay{% endif %}">>, [{var1, "rook"}, {var2, "Crooks"}], <<"yay">>},
                      {"If substring in string (false)",
                       <<"{% if var1 in var2 %}boo{% endif %}">>, [{var1, "Cook"}, {var2, "Crooks"}], <<>>},
                      {"If substring not in string",
                       <<"{% if var1 not in var2 %}yay{% endif %}">>, [{var1, "Cook"}, {var2, "Crooks"}], <<"yay">>},
                      {"If substring not in string (false)",
                       <<"{% if var1 not in var2 %}boo{% endif %}">>, [{var1, "rook"}, {var2, "Crooks"}], <<>>},
                      {"If literal substring in string",
                       <<"{% if \"man\" in \"Ottoman\" %}yay{% endif %}">>, [], <<"yay">>},
                      {"If literal substring in string (false)",
                       <<"{% if \"woman\" in \"Ottoman\" %}boo{% endif %}">>, [], <<>>},
                      {"If element in list",
                       <<"{% if var1 in var2 %}yay{% endif %}">>, [{var1, "foo"}, {var2, ["bar", "foo", "baz"]}], <<"yay">>},
                      {"If element in list (false)",
                       <<"{% if var1 in var2 %}boo{% endif %}">>, [{var1, "FOO"}, {var2, ["bar", "foo", "baz"]}], <<>>}
                     ]},
     {"if .. and ..", [
                       {"If true and true",
                        <<"{% if var1 and var2 %}yay{% endif %}">>, [{var1, true}, {var2, true}], <<"yay">>},
                       {"If true and false",
                        <<"{% if var1 and var2 %}yay{% endif %}">>, [{var1, true}, {var2, false}], <<"">>},
                       {"If false and true",
                        <<"{% if var1 and var2 %}yay{% endif %}">>, [{var1, false}, {var2, true}], <<"">>},
                       {"If false and false ",
                        <<"{% if var1 and var2 %}yay{% endif %}">>, [{var1, false}, {var2, false}], <<"">>}
                      ]},
     {"if .. or ..", [
                      {"If true or true",
                       <<"{% if var1 or var2 %}yay{% endif %}">>, [{var1, true}, {var2, true}], <<"yay">>},
                      {"If true or false",
                       <<"{% if var1 or var2 %}yay{% endif %}">>, [{var1, true}, {var2, false}], <<"yay">>},
                      {"If false or true",
                       <<"{% if var1 or var2 %}yay{% endif %}">>, [{var1, false}, {var2, true}], <<"yay">>},
                      {"If false or false ",
                       <<"{% if var1 or var2 %}yay{% endif %}">>, [{var1, false}, {var2, false}], <<"">>}
                     ]},
     {"if equality", [
                      {"If int equals number literal",
                       <<"{% if var1 == 2 %}yay{% endif %}">>, [{var1, 2}], <<"yay">>},
                      {"If int equals number literal (false)",
                       <<"{% if var1 == 2 %}yay{% endif %}">>, [{var1, 3}], <<"">>},
                      {"If string equals string literal",
                       <<"{% if var1 == \"2\" %}yay{% endif %}">>, [{var1, "2"}], <<"yay">>},
                      {"If string equals string literal (false)",
                       <<"{% if var1 == \"2\" %}yay{% endif %}">>, [{var1, "3"}], <<"">>},
                      {"If int not equals number literal",
                       <<"{% if var1 != 2 %}yay{% endif %}">>, [{var1, 3}], <<"yay">>},
                      {"If string not equals string literal",
                       <<"{% if var1 != \"2\" %}yay{% endif %}">>, [{var1, "3"}], <<"yay">>},
                      {"If filter result equals number literal",
                       <<"{% if var1|length == 2 %}yay{% endif %}">>, [{var1, ["fo", "bo"]}], <<"yay">>},
                      {"If filter result equals string literal",
                       <<"{% if var1|capfirst == \"Foo\" %}yay{% endif %}">>, [{var1, "foo"}], <<"yay">>}
                     ]},
     {"if size comparison", [
                             {"If int greater than number literal",
                              <<"{% if var1 > 2 %}yay{% endif %}">>, [{var1, 3}], <<"yay">>},
                             {"If int greater than negative number literal",
                              <<"{% if var1 > -2 %}yay{% endif %}">>, [{var1, -1}], <<"yay">>},
                             {"If int greater than number literal (false)",
                              <<"{% if var1 > 2 %}yay{% endif %}">>, [{var1, 2}], <<"">>},

                             {"If int greater than or equal to number literal",
                              <<"{% if var1 >= 2 %}yay{% endif %}">>, [{var1, 3}], <<"yay">>},
                             {"If int greater than or equal to number literal (2)",
                              <<"{% if var1 >= 2 %}yay{% endif %}">>, [{var1, 2}], <<"yay">>},
                             {"If int greater than or equal to number literal (false)",
                              <<"{% if var1 >= 2 %}yay{% endif %}">>, [{var1, 1}], <<"">>},

                             {"If int less than number literal",
                              <<"{% if var1 < 2 %}yay{% endif %}">>, [{var1, 1}], <<"yay">>},
                             {"If int less than number literal (false)",
                              <<"{% if var1 < 2 %}yay{% endif %}">>, [{var1, 2}], <<"">>},

                             {"If int less than or equal to number literal",
                              <<"{% if var1 <= 2 %}yay{% endif %}">>, [{var1, 1}], <<"yay">>},
                             {"If int less than or equal to number literal",
                              <<"{% if var1 <= 2 %}yay{% endif %}">>, [{var1, 2}], <<"yay">>},
                             {"If int less than or equal to number literal (false)",
                              <<"{% if var1 <= 2 %}yay{% endif %}">>, [{var1, 3}], <<"">>}
                            ]},
     {"if complex bool", [
                          {"If (true or false) and true",
                           <<"{% if (var1 or var2) and var3 %}yay{% endif %}">>,
                           [{var1, true}, {var2, false}, {var3, true}], <<"yay">>},
                          {"If true or (false and true)",
                           <<"{% if var1 or (var2 and var3) %}yay{% endif %}">>,
                           [{var1, true}, {var2, false}, {var3, true}], <<"yay">>}
                         ]},
     {"for", [
              {"Simple loop",
               <<"{% for x in list %}{{ x }}{% endfor %}">>, [{'list', ["1", "2", "3"]}],
               <<"123">>},
              {"Reversed loop",
               <<"{% for x in list reversed %}{{ x }}{% endfor %}">>, [{'list', ["1", "2", "3"]}],
               <<"321">>},
              {"Expand list",
               <<"{% for x, y in list %}{{ x }},{{ y }}\n{% endfor %}">>, [{'list', [["X", "1"], ["X", "2"]]}],
               <<"X,1\nX,2\n">>},
              {"Expand tuple",
               <<"{% for x, y in list %}{{ x }},{{ y }}\n{% endfor %}">>, [{'list', [{"X", "1"}, {"X", "2"}]}],
               <<"X,1\nX,2\n">>},
              {"Resolve variable attribute",
               <<"{% for number in person.numbers %}{{ number }}\n{% endfor %}">>, [{person, [{numbers, ["411", "911"]}]}],
               <<"411\n911\n">>},
              {"Resolve nested variable attribute",
               <<"{% for number in person.home.numbers %}{{ number }}\n{% endfor %}">>, [{person, [{home, [{numbers, ["411", "911"]}]}]}],
               <<"411\n911\n">>},
              {"Counter0",
               <<"{% for number in numbers %}{{ forloop.counter0 }}. {{ number }}\n{% endfor %}">>,
               [{numbers, ["Zero", "One", "Two"]}], <<"0. Zero\n1. One\n2. Two\n">>},
              {"Counter",
               <<"{% for number in numbers %}{{ forloop.counter }}. {{ number }}\n{% endfor %}">>,
               [{numbers, ["One", "Two", "Three"]}], <<"1. One\n2. Two\n3. Three\n">>},
              {"Reverse Counter0",
               <<"{% for number in numbers %}{{ forloop.revcounter0 }}. {{ number }}\n{% endfor %}">>,
               [{numbers, ["Two", "One", "Zero"]}], <<"2. Two\n1. One\n0. Zero\n">>},
              {"Reverse Counter",
               <<"{% for number in numbers %}{{ forloop.revcounter }}. {{ number }}\n{% endfor %}">>,
               [{numbers, ["Three", "Two", "One"]}], <<"3. Three\n2. Two\n1. One\n">>},
              {"Counter \"first\"",
               <<"{% for number in numbers %}{% if forloop.first %}{{ number }}{% endif %}{% endfor %}">>,
               [{numbers, ["One", "Two", "Three"]}], <<"One">>},
              {"Counter \"last\"",
               <<"{% for number in numbers %}{% if forloop.last %}{{ number }}{% endif %}{% endfor %}">>,
               [{numbers, ["One", "Two", "Three"]}], <<"Three">>},
              {"Nested for loop",
               <<"{% for outer in list %}{% for inner in outer %}{{ inner }}\n{% endfor %}{% endfor %}">>,
               [{'list', [["Al", "Albert"], ["Jo", "Joseph"]]}],
               <<"Al\nAlbert\nJo\nJoseph\n">>},
              {"Access parent loop counters",
               <<"{% for outer in list %}{% for inner in outer %}({{ forloop.parentloop.counter0 }}, {{ forloop.counter0 }})\n{% endfor %}{% endfor %}">>,
               [{'list', [["One", "two"], ["One", "two"]]}], [], [], <<"(0, 0)\n(0, 1)\n(1, 0)\n(1, 1)\n">>,
               %% the warnings we get from the erlang compiler still needs some care..
               [error_info("erlydtl_running_test", [{0, erl_lint, {unused_var, 'Var_inner/1_1:31'}}, no_out_dir])]},
              {"If changed",
               <<"{% for x in list %}{% ifchanged %}{{ x }}\n{% endifchanged %}{% endfor %}">>,
               [{'list', ["one", "two", "two", "three", "three", "three"]}], <<"one\ntwo\nthree\n">>},
              {"If changed/2",
               <<"{% for x, y in list %}{% ifchanged %}{{ x|upper }}{% endifchanged %}{% ifchanged %}{{ y|lower }}{% endifchanged %}\n{% endfor %}">>,
               [{'list', [["one", "a"], ["two", "A"], ["two", "B"], ["three", "b"], ["three", "c"], ["Three", "b"]]}], <<"ONEa\nTWO\nb\nTHREE\nc\nb\n">>},
              {"If changed/else",
               <<"{% for x in list %}{% ifchanged %}{{ x }}\n{% else %}foo\n{% endifchanged %}{% endfor %}">>,
               [{'list', ["one", "two", "two", "three", "three", "three"]}], <<"one\ntwo\nfoo\nthree\nfoo\nfoo\n">>},
              {"If changed/param",
               <<"{% for date in list %}{% ifchanged date.month %} {{ date.month }}:{{ date.day }}{% else %},{{ date.day }}{% endifchanged %}{% endfor %}\n">>,
               [{'list', [[{month,"Jan"},{day,1}],[{month,"Jan"},{day,2}],[{month,"Apr"},{day,10}],
                          [{month,"Apr"},{day,11}],[{month,"May"},{day,4}]]}],
               <<" Jan:1,2 Apr:10,11 May:4\n">>},
              {"If changed/param2",
               <<"{% for x, y in list %}{% ifchanged y|upper %}{{ x|upper }}{% endifchanged %}\n{% endfor %}">>,
               [{'list', [["one", "a"], ["two", "A"], ["two", "B"], ["three", "b"], ["three", "c"], ["Three", "b"]]}], <<"ONE\n\nTWO\n\nTHREE\nTHREE\n">>},
              {"If changed/param2 combined",
               <<"{% for x, y in list %}{% ifchanged x y|upper %}{{ x }}{% endifchanged %}\n{% endfor %}">>,
               [{'list', [["one", "a"], ["two", "A"], ["two", "B"], ["three", "b"], ["three", "B"], ["three", "c"]]}], <<"one\ntwo\ntwo\nthree\n\nthree\n">>},
              {"If changed/resolve",
               <<"{% for x in list %}{% ifchanged x.name|first %}{{ x.value }}{% endifchanged %}\n{% endfor %}">>,
               [{'list', [[{"name", ["nA","nB"]},{"value","1"}],[{"name", ["nA","nC"]},{"value","2"}],
                          [{"name", ["nB","nC"]},{"value","3"}],[{"name", ["nB","nA"]},{"value","4"}]]}],
               <<"1\n\n3\n\n">>},

              {"Loop undefined var",
               <<"{% for i in undef %}i = {{ i }}.\n{% endfor %}">>,
               [],
               <<"">>},
              {"Loop filtered value rather than variable",
               <<"{% for x in 123|make_list %}{% if not forloop.first %}, {% endif %}{{ x }}{% endfor %}">>,
               [],
               <<"1, 2, 3">>}
             ]},
     {"for/empty", [
                    {"Simple loop",
                     <<"{% for x in list %}{{ x }}{% empty %}shucks{% endfor %}">>, [{'list', ["1", "2", "3"]}],
                     <<"123">>},
                    {"Simple loop (empty)",
                     <<"{% for x in list %}{{ x }}{% empty %}shucks{% endfor %}">>, [{'list', []}],
                     <<"shucks">>}
                   ]},
     {"ifequal", [
                  {"Compare variable to literal",
                   <<"{% ifequal var1 \"foo\" %}yay{% endifequal %}">>,
                   [{var1, "foo"}], <<"yay">>},
                  {"Compare variable to unequal literal",
                   <<"{% ifequal var1 \"foo\" %}boo{% endifequal %}">>,
                   [{var1, "bar"}], <<>>},
                  {"Compare literal to variable",
                   <<"{% ifequal \"foo\" var1 %}yay{% endifequal %}">>,
                   [{var1, "foo"}], <<"yay">>},
                  {"Compare literal to unequal variable",
                   <<"{% ifequal \"foo\" var1 %}boo{% endifequal %}">>,
                   [{var1, "bar"}], <<>>},
                  {"Compare variable to literal (int string)",
                   <<"{% ifequal var1 \"2\" %}yay{% else %}boo{% endifequal %}">>,
                   [{var1, "2"}], <<"yay">>},
                  {"Compare variable to literal (int)",
                   <<"{% ifequal var1 2 %}yay{% else %}boo{% endifequal %}">>,
                   [{var1, 2}], <<"yay">>},
                  {"Compare variable to unequal literal (int)",
                   <<"{% ifequal var1 2 %}boo{% else %}yay{% endifequal %}">>,
                   [{var1, 3}], <<"yay">>},
                  {"Compare variable to equal literal (atom)",
                   <<"{% ifequal var1 \"foo\"%}yay{% endifequal %}">>,
                   [{var1, foo}], <<"yay">>},
                  {"Compare variable to unequal literal (atom)",
                   <<"{% ifequal var1 \"foo\"%}yay{% else %}boo{% endifequal %}">>,
                   [{var1, bar}], <<"boo">>}
                 ]},
     {"ifequal/else", [
                       {"Compare variable to literal",
                        <<"{% ifequal var1 \"foo\" %}yay{% else %}boo{% endifequal %}">>,
                        [{var1, "foo"}], <<"yay">>},
                       {"Compare variable to unequal literal",
                        <<"{% ifequal var1 \"foo\" %}boo{% else %}yay{% endifequal %}">>,
                        [{var1, "bar"}], <<"yay">>},
                       {"Compare literal to variable",
                        <<"{% ifequal \"foo\" var1 %}yay{% else %}boo{% endifequal %}">>,
                        [{var1, "foo"}], <<"yay">>},
                       {"Compare literal to unequal variable",
                        <<"{% ifequal \"foo\" var1 %}boo{% else %}yay{% endifequal %}">>,
                        [{var1, "bar"}], <<"yay">>}
                      ]},
     {"ifnotequal", [
                     {"Compare variable to literal",
                      <<"{% ifnotequal var1 \"foo\" %}boo{% endifnotequal %}">>,
                      [{var1, "foo"}], <<>>},
                     {"Compare variable to unequal literal",
                      <<"{% ifnotequal var1 \"foo\" %}yay{% endifnotequal %}">>,
                      [{var1, "bar"}], <<"yay">>},
                     {"Compare literal to variable",
                      <<"{% ifnotequal \"foo\" var1 %}boo{% endifnotequal %}">>,
                      [{var1, "foo"}], <<>>},
                     {"Compare literal to unequal variable",
                      <<"{% ifnotequal \"foo\" var1 %}yay{% endifnotequal %}">>,
                      [{var1, "bar"}], <<"yay">>}
                    ]},
     {"ifnotequal/else", [
                          {"Compare variable to literal",
                           <<"{% ifnotequal var1 \"foo\" %}boo{% else %}yay{% endifnotequal %}">>,
                           [{var1, "foo"}], <<"yay">>},
                          {"Compare variable to unequal literal",
                           <<"{% ifnotequal var1 \"foo\" %}yay{% else %}boo{% endifnotequal %}">>,
                           [{var1, "bar"}], <<"yay">>},
                          {"Compare literal to variable",
                           <<"{% ifnotequal \"foo\" var1 %}boo{% else %}yay{% endifnotequal %}">>,
                           [{var1, "foo"}], <<"yay">>},
                          {"Compare literal to unequal variable",
                           <<"{% ifnotequal \"foo\" var1 %}yay{% else %}boo{% endifnotequal %}">>,
                           [{var1, "bar"}], <<"yay">>}
                         ]},
     {"filter tag", [
                     {"Apply a filter",
                      <<"{% filter escape %}&{% endfilter %}">>, [], <<"&amp;">>},
                     {"Chained filters",
                      <<"{% filter linebreaksbr|escape %}\n{% endfilter %}">>, [], <<"&lt;br /&gt;">>}
                    ]},
     {"filters", [
                  {"Filter a literal",
                   <<"{{ \"pop\"|capfirst }}">>, [],
                   <<"Pop">>},
                  {"Filters applied in order",
                   <<"{{ var1|force_escape|length }}">>, [{var1, <<"&">>}],
                   <<"5">>},
                  {"Escape is applied last",
                   <<"{{ var1|escape|linebreaksbr }}">>, [{var1, <<"\n">>}],
                   <<"&lt;br /&gt;">>},
                  {"add; lhs number, rhs number",
                   <<"{{ one|add:4}}">>, [{one, 1}],
                   <<"5">>},
                  {"add; lhs numeric string, rhs number",
                   <<"{{ one|add:4}}">>, [{one, "1"}],
                   <<"5">>},
                  {"add; lhs number, rhs numeric string",
                   <<"{{ one|add:'4'}}">>, [{one, 1}],
                   <<"5">>},
                  {"add; lhs non-numeric string, rhs number",
                   <<"{{ one|add:4}}">>, [{one, "foo"}],
                   <<"foo4">>},
                  {"add; lhs number, rhs non-numeric string",
                   <<"{{ one|add:'foo'}}">>, [{one, 1}],
                   <<"1foo">>},
                  {"add; lhs non-numeric string, rhs non-numeric string",
                   <<"{{ one|add:'bar'}}">>, [{one, "foo"}],
                   <<"foobar">>},
                  {"add; lhs numeric string, rhs numeric string",
                   <<"{{ one|add:'4'}}">>, [{one, "1"}],
                   <<"5">>},
                  {"|addslashes",
                   <<"{{ var1|addslashes }}">>, [{var1, "Jimmy's \"great\" meats'n'things"}],
                   <<"Jimmy\\'s \\\"great\\\" meats\\'n\\'things">>},
                  {"|capfirst",
                   <<"{{ var1|capfirst }}">>, [{var1, "dana boyd"}],
                   <<"Dana boyd">>},
                  {"|center:10",
                   <<"{{ var1|center:10 }}">>, [{var1, "MB"}],
                   <<"    MB    ">>},
                  {"|center:1",
                   <<"{{ var1|center:1 }}">>, [{var1, "KBR"}],
                   <<"B">>},
                  {"|cut:\" \"",
                   <<"{{ var1|cut:\" \" }}">>, [{var1, "String with spaces"}],
                   <<"Stringwithspaces">>},
                  {"|date 1",
                   <<"{{ var1|date:\"jS F Y H:i\" }}">>,
                   [{var1, {1975,7,24}}],
                   <<"24th July 1975 00:00">>},
                  {"|date 2",
                   <<"{{ var1|date:\"jS F Y H:i\" }}">>,
                   [{var1, {{1975,7,24}, {7,13,1}}}],
                   <<"24th July 1975 07:13">>},
                  {"|date 3",
                   <<"{{ var1|date }}">>,
                   [{var1, {{1975,7,24}, {7,13,1}}}],
                   <<"July 24, 1975">>},
                  {"|default:\"foo\" 1",
                   <<"{{ var1|default:\"foo\" }}">>, [], <<"foo">>},
                  {"|default:\"foo\" 2",
                   <<"{{ var1|default:\"foo\" }}">>, [{var1, "bar"}], <<"bar">>},
                  {"|default:\"foo\" 3",
                   <<"{{ var1|default:\"foo\" }}">>, [{var1, "0"}], <<"foo">>},
                  {"|default_if_none:\"foo\"",
                   <<"{{ var1|default_if_none:\"foo\" }}">>, [], <<"foo">>},
                  {"|default_if_none:\"foo\" 2",
                   <<"{{ var1|default_if_none:\"foo\" }}">>, [{var1, "bar"}], <<"bar">>},
                  {"|dictsort 1",
                   <<"{{ var1|dictsort:\"foo\" }}">>,
                   [{var1,[[{foo,2}],[{foo,1}]]}], <<"{foo,1}{foo,2}">>},
                  {"|dictsort 2",
                   <<"{{ var1|dictsort:\"foo.bar\" }}">>,
                   [{var1,[[{foo,[{bar,2}]}],[{foo,[{bar,1}]}]]}],
                   <<"{foo,[{bar,1}]}{foo,[{bar,2}]}">>},
                  {"|divisibleby:\"3\"",
                   <<"{% if var1|divisibleby:\"3\" %}yay{% endif %}">>, [{var1, 21}], <<"yay">>},
                  {"|divisibleby:\"3\"",
                   <<"{% if var1|divisibleby:\"3\" %}yay{% endif %}">>, [{var1, 22}], <<"">>},
                  {"|escape",
                   <<"{% autoescape on %}{{ var1|escape|escape|escape }}{% endautoescape %}">>, [{var1, ">&1"}], <<"&gt;&amp;1">>},
                  {"|escapejs",
                   <<"{{ var1|escapejs }}">>, [{var1, "testing\r\njavascript 'string\" <b>escaping</b>"}],
                   <<"testing\\u000D\\u000Ajavascript \\u0027string\\u0022 \\u003Cb\\u003Eescaping\\u003C/b\\u003E">>},
                  {"|filesizeformat (bytes)",
                   <<"{{ var1|filesizeformat }}">>, [{var1, 1023}], <<"1023 bytes">>},
                  {"|filesizeformat (KB)",
                   <<"{{ var1|filesizeformat }}">>, [{var1, 3487}], <<"3.4 KB">>},
                  {"|filesizeformat (MB)",
                   <<"{{ var1|filesizeformat }}">>, [{var1, 6277098}], <<"6.0 MB">>},
                  {"|filesizeformat (GB)",
                   <<"{{ var1|filesizeformat }}">>, [{var1, 1024 * 1024 * 1024}], <<"1.0 GB">>},
                  {"|first",
                   <<"{{ var1|first }}">>, [{var1, "James"}],
                   <<"J">>},
                  {"|fix_ampersands",
                   <<"{{ var1|fix_ampersands }}">>, [{var1, "Ben & Jerry's"}],
                   <<"Ben &amp; Jerry's">>},

                  {"|floatformat:\"-1\"",
                   <<"{{ var1|floatformat:\"-1\" }}">>, [{var1, 34.23234}],
                   <<"34.2">>},
                  {"int |floatformat",
                   <<"{{ var1|floatformat:\"-1\" }}">>, [{var1, 123}],
                   <<"123">>},
                  {"string |floatformat",
                   <<"{{ var1|floatformat:\"-1\" }}">>, [{var1, "123.321"}],
                   <<"123.3">>},
                  {"binary |floatformat",
                   <<"{{ var1|floatformat:\"-1\" }}">>, [{var1, <<"123.321">>}],
                   <<"123.3">>},

                  %% from: https://docs.djangoproject.com/en/1.6/ref/templates/builtins/#floatformat
                  {"1.a) |floatformat",
                   <<"{{ var1|floatformat }}">>, [{var1, 34.23234}],
                   <<"34.2">>},
                  {"1.b) |floatformat",
                   <<"{{ var1|floatformat }}">>, [{var1, 34.00000}],
                   <<"34">>},
                  {"1.c) |floatformat",
                   <<"{{ var1|floatformat }}">>, [{var1, 34.26000}],
                   <<"34.3">>},
                  {"2.a) |floatformat:\"3\"",
                   <<"{{ var1|floatformat:\"3\" }}">>, [{var1, 34.23234}],
                   <<"34.232">>},
                  {"2.b) |floatformat:\"3\"",
                   <<"{{ var1|floatformat:\"3\" }}">>, [{var1, 34.00000}],
                   <<"34.000">>},
                  {"2.c) |floatformat:\"3\"",
                   <<"{{ var1|floatformat:\"3\" }}">>, [{var1, 34.26000}],
                   <<"34.260">>},
                  {"3.a) |floatformat:\"0\"",
                   <<"{{ var1|floatformat:\"0\" }}">>, [{var1, 34.23234}],
                   <<"34">>},
                  {"3.b) |floatformat:\"0\"",
                   <<"{{ var1|floatformat:\"0\" }}">>, [{var1, 34.00000}],
                   <<"34">>},
                  {"3.c) |floatformat:\"0\"",
                   <<"{{ var1|floatformat:\"0\" }}">>, [{var1, 39.56000}],
                   <<"40">>},
                  {"4.a) |floatformat:\"-3\"",
                   <<"{{ var1|floatformat:\"-3\" }}">>, [{var1, 34.23234}],
                   <<"34.232">>},
                  {"4.b) |floatformat:\"-3\"",
                   <<"{{ var1|floatformat:\"-3\" }}">>, [{var1, 34.00000}],
                   <<"34">>},
                  {"4.c) |floatformat:\"-3\"",
                   <<"{{ var1|floatformat:\"-3\" }}">>, [{var1, 34.26000}],
                   <<"34.260">>},

                  {"|force_escape",
                   <<"{{ var1|force_escape }}">>, [{var1, "Ben & Jerry's <=> \"The World's Best Ice Cream\""}],
                   <<"Ben &amp; Jerry&#039;s &lt;=&gt; &quot;The World&#039;s Best Ice Cream&quot;">>},
                  {"iolist |force_escape",
                   <<"{{ var1|force_escape }}">>, [{var1, ["'a'"]}],
                   <<"&#039;a&#039;">>},
                  {"nested iolist |force_escape",
                   <<"{{ var1|force_escape }}">>, [{var1, ["a'", <<"b">>, [<<"<c">>, "d", ["e>"]]]}],
                   <<"a&#039;b&lt;cde&gt;">>},
                  {"|format_integer",
                   <<"{{ var1|format_integer }}">>, [{var1, 28}], <<"28">>},
                  {"|format_number 1",
                   <<"{{ var1|format_number }}">>, [{var1, 28}], <<"28">>},
                  {"|format_number 2",
                   <<"{{ var1|format_number }}">>, [{var1, 23.77}], <<"23.77">>},
                  {"|format_number 3",
                   <<"{{ var1|format_number }}">>, [{var1, "28.77"}], <<"28.77">>},
                  {"|format_number 4",
                   <<"{{ var1|format_number }}">>, [{var1, "23.77"}], <<"23.77">>},
                  {"|format_number 5",
                   <<"{{ var1|format_number }}">>, [{var1, fun() -> 29 end}], <<"29">>},
                  {"|format_number 6",
                   <<"{{ var1|format_number }}">>, [{var1, fun() -> fun() -> 31 end end}], <<"31">>},
                  {"|get_digit:\"2\"",
                   <<"{{ var1|get_digit:\"2\" }}">>, [{var1, 42}], <<"4">>},
                  {"|iriencode",
                   <<"{{ url|iriencode }}">>, [{url, "You #$*@!!"}], <<"You+#$*@!!">>},
                  {"|join:\", \" (list)",
                   <<"{{ var1|join:\", \" }}">>, [{var1, ["Liberte", "Egalite", "Fraternite"]}],
                   <<"Liberte, Egalite, Fraternite">>},
                  {"|join:\", \" (binary)",
                   <<"{{ var1|join:\", \" }}">>, [{var1, [<<"Liberte">>, "Egalite", <<"Fraternite">>]}],
                   <<"Liberte, Egalite, Fraternite">>},
                  {"|last",
                   <<"{{ var1|last }}">>, [{var1, "XYZ"}],
                   <<"Z">>},
                  {"|length",
                   <<"{{ var1|length }}">>, [{var1, "antidisestablishmentarianism"}],
                   <<"28">>},
                  {"|linebreaks",
                   <<"{{ var1|linebreaks }}">>, [{var1, "Joel\nis a slug"}],
                   <<"<p>Joel<br />is a slug</p>">>},
                  {"|linebreaks",
                   <<"{{ var1|linebreaks }}">>, [{var1, "Joel\n\n\n\nis a slug"}],
                   <<"<p>Joel</p><p>is a slug</p>">>},
                  {"|linebreaks",
                   <<"{{ var1|linebreaks }}">>, [{var1, "Joel\n\nis a \nslug"}],
                   <<"<p>Joel</p><p>is a <br />slug</p>">>},
                  {"|linebreaksbr",
                   <<"{{ var1|linebreaksbr }}">>, [{var1, "One\nTwo\n\nThree\n\n\n"}],
                   <<"One<br />Two<br /><br />Three<br /><br /><br />">>},
                  {"|linebreaksbr",
                   <<"{{ \"One\\nTwo\\n\\nThree\\n\\n\\n\"|linebreaksbr }}">>, [],
                   <<"One<br />Two<br /><br />Three<br /><br /><br />">>},
                  {"|linenumbers",
                   <<"{{ var1|linenumbers }}">>, [{var1, "a\nb\nc"}],
                   <<"1. a\n2. b\n3. c">>},
                  {"|linenumbers",
                   <<"{{ var1|linenumbers }}">>, [{var1, "a"}],
                   <<"1. a">>},
                  {"|linenumbers",
                   <<"{{ var1|linenumbers }}">>, [{var1, "a\n"}],
                   <<"1. a\n2. ">>},
                  {"|ljust:10",
                   <<"{{ var1|ljust:10 }}">>, [{var1, "Gore"}],
                   <<"Gore      ">>},
                  {"|lower",
                   <<"{{ var1|lower }}">>, [{var1, "E. E. Cummings"}],
                   <<"e. e. cummings">>},
                  {"|makelist",
                   <<"{{ list|make_list }}">>, [{list, "Joel"}],
                   <<"J","o","e","l">>},
                  {"|pluralize",
                   <<"{{ num|pluralize }}">>, [{num, 1}],
                   <<"">>},
                  {"|pluralize",
                   <<"{{ num|pluralize }}">>, [{num, 2}],
                   <<"s">>},
                  {"|pluralize:\"s\"",
                   <<"{{ num|pluralize }}">>, [{num, 1}],
                   <<"">>},
                  {"|pluralize:\"s\"",
                   <<"{{ num|pluralize }}">>, [{num, 2}],
                   <<"s">>},
                  {"|pluralize:\"y,es\" (list)",
                   <<"{{ num|pluralize:\"y,es\" }}">>, [{num, 1}],
                   <<"y">>},
                  {"|pluralize:\"y,es\" (list)",
                   <<"{{ num|pluralize:\"y,es\" }}">>, [{num, 2}],
                   <<"es">>},
                  {"|random",
                   <<"{{ var1|random }}">>, [{var1, ["foo", "foo", "foo"]}],
                   <<"foo">>},
                  {"|removetags:\"b span\"",
                   <<"{{ var1|removetags:\"b span\" }}">>, [{var1, "<B>Joel</B> <button>is</button> a <span>slug</span>"}],
                   <<"<B>Joel</B> <button>is</button> a slug">>},
                  {"|rjust:10",
                   <<"{{ var1|rjust:10 }}">>, [{var1, "Bush"}],
                   <<"      Bush">>},
                  {"|safe",
                   <<"{% autoescape on %}{{ var1|safe|escape }}{% endautoescape %}">>, [{var1, "&"}],
                   <<"&">>},
                  %%python/django slice is zero based, erlang lists are 1 based
                  %%first number included, second number not
                  %%negative numbers are allowed
                  %%regex to convert from erlydtl_filters_tests:
                                                % for slice: \?assert.*\( \[(.*)\], erlydtl_filters:(.*)\((.*),"(.*)"\)\),
                                                % {"|slice:\"$4\"", <<"{{ var|$2:\"$4\" }}">>, [{var, $3}],<<$1>>},
                                                % \t\t{"|slice:\"$4\"",\n\t\t\t\t\t <<"{{ var|$2:\"$4\" }}">>, [{var, $3}],\n\t\t\t\t\t<<$1>>},
                                                %
                                                % for stringformat:
                                                % \?assert.*\( (.*), erlydtl_filters:(.*)\((.*), "(.*)"\) \)
                                                % \t\t{"|stringformat:\"$4\"",\n\t\t\t\t\t <<"{{ var|$2:\"$4\" }}">>, [{var, $3}],\n\t\t\t\t\t<<$1>>}

                  {"|slice:\":\"",
                   <<"{{ var|slice:\":\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<1,2,3,4,5,6,7,8,9>>},
                  {"|slice:\"1\"",
                   <<"{{ var|slice:\"1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<"2">>},
                  {"|slice:\"100\"",
                   <<"{{ var|slice:\"100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<"indexError">>},
                  {"|slice:\"-1\"",
                   <<"{{ var|slice:\"-1\" }}">>, [{var, ["a","b","c","d","e","f","g","h","i"]}],
                   <<"i">>},
                  {"|slice:\"-1\"",
                   <<"{{ var|slice:\"-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<"9">>},
                  {"|slice:\"-100\"",
                   <<"{{ var|slice:\"-100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<"indexError">>},
                  {"|slice:\"1:\"",
                   <<"{{ var|slice:\"1:\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<2,3,4,5,6,7,8,9>>},
                  {"|slice:\"100:\"",
                   <<"{{ var|slice:\"100:\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},
                  {"|slice:\"-1:\"",
                   <<"{{ var|slice:\"-1:\" }}">>, [{var, ["a","b","c","d","e","f","h","i","j"]}],
                   <<"j">>},
                  {"|slice:\"-1:\"",
                   <<"{{ var|slice:\"-1:\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<9>>},
                  {"|slice:\"-100:\"",
                   <<"{{ var|slice:\"-100:\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<1,2,3,4,5,6,7,8,9>>},

                  {"|slice:\":1\"",
                   <<"{{ var|slice:\":1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<1>>},
                  {"|slice:\":100\"",
                   <<"{{ var|slice:\":100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<1,2,3,4,5,6,7,8,9>>},
                  {"|slice:\":-1\"",
                   <<"{{ var|slice:\":-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<1,2,3,4,5,6,7,8>>},
                  {"|slice:\":-100\"",
                   <<"{{ var|slice:\":-100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},

                  {"|slice:\"-1:-1\"",
                   <<"{{ var|slice:\"-1:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},
                  {"|slice:\"1:1\"",
                   <<"{{ var|slice:\"1:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},
                  {"|slice:\"1:-1\"",
                   <<"{{ var|slice:\"1:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<2,3,4,5,6,7,8>>},
                  {"|slice:\"-1:1\"",
                   <<"{{ var|slice:\"-1:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},

                  {"|slice:\"-100:-100\"",
                   <<"{{ var|slice:\"-100:-100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},
                  {"|slice:\"100:100\"",
                   <<"{{ var|slice:\"100:100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},
                  {"|slice:\"100:-100\"",
                   <<"{{ var|slice:\"100:-100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},
                  {"|slice:\"-100:100\"",
                   <<"{{ var|slice:\"-100:100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<1,2,3,4,5,6,7,8,9>>},


                  {"|slice:\"1:3\"",
                   <<"{{ var|slice:\"1:3\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<2,3>>},

                  {"|slice:\"::\"",
                   <<"{{ var|slice:\"::\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<1,2,3,4,5,6,7,8,9>>},
                  {"|slice:\"1:9:1\"",
                   <<"{{ var|slice:\"1:9:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<2,3,4,5,6,7,8,9>>},
                  {"|slice:\"10:1:-1\"",
                   <<"{{ var|slice:\"10:1:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<9,8,7,6,5,4,3>>},
                  {"|slice:\"-111:-1:1\"",
                   <<"{{ var|slice:\"-111:-1:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<1,2,3,4,5,6,7,8>>},

                  {"|slice:\"-111:-111:1\"",
                   <<"{{ var|slice:\"-111:-111:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},
                  {"|slice:\"111:111:1\"",
                   <<"{{ var|slice:\"111:111:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},
                  {"|slice:\"-111:111:1\"",
                   <<"{{ var|slice:\"-111:111:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<1,2,3,4,5,6,7,8,9>>},
                  {"|slice:\"111:-111:1\"",
                   <<"{{ var|slice:\"111:-111:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},

                  {"|slice:\"-111:-111:-1\"",
                   <<"{{ var|slice:\"-111:-111:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},
                  {"|slice:\"111:111:-1\"",
                   <<"{{ var|slice:\"111:111:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},
                  {"|slice:\"-111:111:-1\"",
                   <<"{{ var|slice:\"-111:111:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<>>},
                  {"|slice:\"111:-111:-1\"",
                   <<"{{ var|slice:\"111:-111:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
                   <<9,8,7,6,5,4,3,2,1>>},              {"|phone2numeric",
                                                         <<"{{ var1|phone2numeric }}">>, [{var1, "1-800-COLLECT"}],
                                                         <<"1-800-2655328">>},
                  {"|slugify",
                   <<"{{ var1|slugify }}">>, [{var1, "What The $#_! Was He Thinking?"}],
                   <<"what-the-_-was-he-thinking">>},
                  {"|slice:\"s\"",
                   <<"{{ var|stringformat:\"s\" }}">>, [{var, "test"}],
                   <<"test">>},
                  {"|stringformat:\"s\"",
                   <<"{{ var|stringformat:\"s\" }}">>, [{var, "test"}],
                   <<"test">>},
                  {"|stringformat:\"s\"",
                   <<"{{ var|stringformat:\"s\" }}">>, [{var, "1"}],
                   <<"1">>},
                  {"|stringformat:\"s\"",
                   <<"{{ var|stringformat:\"s\" }}">>, [{var, "test"}],
                   <<"test">>},
                  {"|stringformat:\"10s\"",
                   <<"{{ var|stringformat:\"10s\" }}">>, [{var, "test"}],
                   <<"      test">>},
                  {"|stringformat:\"-10s\"",
                   <<"{{ var|stringformat:\"-10s\" }}">>, [{var, "test"}],
                   <<"test      ">>},

                  {"|stringformat:\"d\"",
                   <<"{{ var|stringformat:\"d\" }}">>, [{var, "90"}],
                   <<"90">>},
                  {"|stringformat:\"10d\"",
                   <<"{{ var|stringformat:\"10d\" }}">>, [{var, "90"}],
                   <<"        90">>},
                  {"|stringformat:\"-10d\"",
                   <<"{{ var|stringformat:\"-10d\" }}">>, [{var, "90"}],
                   <<"90        ">>},
                  {"|stringformat:\"i\"",
                   <<"{{ var|stringformat:\"i\" }}">>, [{var, "90"}],
                   <<"90">>},
                  {"|stringformat:\"10i\"",
                   <<"{{ var|stringformat:\"10i\" }}">>, [{var, "90"}],
                   <<"        90">>},
                  {"|stringformat:\"-10i\"",
                   <<"{{ var|stringformat:\"-10i\" }}">>, [{var, "90"}],
                   <<"90        ">>},
                  {"|stringformat:\"0.2d\"",
                   <<"{{ var|stringformat:\"0.2d\" }}">>, [{var, "9"}],
                   <<"09">>},
                  {"|stringformat:\"10.4d\"",
                   <<"{{ var|stringformat:\"10.4d\" }}">>, [{var, "9"}],
                   <<"      0009">>},
                  {"|stringformat:\"-10.4d\"",
                   <<"{{ var|stringformat:\"-10.4d\" }}">>, [{var, "9"}],
                   <<"0009      ">>},

                  {"|stringformat:\"f\"",
                   <<"{{ var|stringformat:\"f\" }}">>, [{var, "1"}],
                   <<"1.000000">>},
                  {"|stringformat:\".2f\"",
                   <<"{{ var|stringformat:\".2f\" }}">>, [{var, "1"}],
                   <<"1.00">>},
                  {"|stringformat:\"0.2f\"",
                   <<"{{ var|stringformat:\"0.2f\" }}">>, [{var, "1"}],
                   <<"1.00">>},
                  {"|stringformat:\"-0.2f\"",
                   <<"{{ var|stringformat:\"-0.2f\" }}">>, [{var, "1"}],
                   <<"1.00">>},
                  {"|stringformat:\"10.2f\"",
                   <<"{{ var|stringformat:\"10.2f\" }}">>, [{var, "1"}],
                   <<"      1.00">>},
                  {"|stringformat:\"-10.2f\"",
                   <<"{{ var|stringformat:\"-10.2f\" }}">>, [{var, "1"}],
                   <<"1.00      ">>},
                  {"|stringformat:\".2f\"",
                   <<"{{ var|stringformat:\".2f\" }}">>, [{var, "1"}],
                   <<"1.00">>},
                  {"|stringformat:\"x\"",
                   <<"{{ var|stringformat:\"x\" }}">>, [{var, "90"}],
                   <<"5a">>},
                  {"|stringformat:\"X\"",
                   <<"{{ var|stringformat:\"X\" }}">>, [{var, "90"}],
                   <<"5A">>},

                  {"|stringformat:\"o\"",
                   <<"{{ var|stringformat:\"o\" }}">>, [{var, "90"}],
                   <<"132">>},

                  {"|stringformat:\"e\"",
                   <<"{{ var|stringformat:\"e\" }}">>, [{var, "90"}],
                   <<"9.000000e+01">>},
                  {"|stringformat:\"e\"",
                   <<"{{ var|stringformat:\"e\" }}">>, [{var, "90000000000"}],
                   <<"9.000000e+10">>},
                  {"|stringformat:\"E\"",
                   <<"{{ var|stringformat:\"E\" }}">>, [{var, "90"}],
                   <<"9.000000E+01">>},
                  {"|striptags",
                   <<"{{ var|striptags }}">>, [{var, "<b>Joel</b> <button>is</button> a <span>slug</span>"}],
                   <<"Joel is a slug">>},
                  {"|striptags",
                   <<"{{ var|striptags }}">>, [{var, "<B>Joel</B> <button>is</button> a <span>slug</Span>"}],
                   <<"Joel is a slug">>},
                  {"|striptags",
                   <<"{{ var|striptags }}">>, [{var, "Check out <a href=\"http://www.djangoproject.com\" rel=\"nofollow\">http://www.djangoproject.com</a>"}],
                   <<"Check out http://www.djangoproject.com">>},
                  {"|time:\"H:i\"",
                   <<"{{ var|time:\"H:i\" }}">>, [{var, {{2010,12,1}, {10,11,12}} }],
                   <<"10:11">>},
                  {"|time",
                   <<"{{ var|time }}">>, [{var, {{2010,12,1}, {10,11,12}} }],
                   <<"10:11 a.m.">>},
                  {"|timesince:from_date",
                   <<"{{ from_date|timesince:conference_date }}">>, [{conference_date, {{2006,6,1},{8,0,0}} }, {from_date, {{2006,6,1},{0,0,0}} }],
                   <<"8 hours">>},
                  {"|timesince:from_date",
                   <<"{{ from_date|timesince:conference_date }}">>, [{conference_date, {{2010,6,1},{8,0,0}} },{from_date, {{2006,6,1},{0,0,0}} }],
                   <<"4 years, 1 day">>}, % leap year
                  {"|timesince:from_date",
                   <<"{{ from_date|timesince:conference_date }}">>, [{conference_date, {{2006,7,15},{8,0,0}} },{from_date, {{2006,6,1},{0,0,0}} }],
                   <<"1 month, 2 weeks">>},
                  {"|timeuntil:from_date",
                   <<"{{ conference_date|timeuntil:from_date }}">>, [{conference_date, {{2006,6,1},{8,0,0}} }, {from_date, {{2006,6,1},{0,0,0}} }],
                   <<"8 hours">>},
                  {"|timeuntil:from_date",
                   <<"{{ conference_date|timeuntil:from_date }}">>, [{conference_date, {{2010,6,1},{8,0,0}} },{from_date, {{2006,6,1},{0,0,0}} }],
                   <<"4 years, 1 day">>},
                  {"|timeuntil:from_date",
                   <<"{{ conference_date|timeuntil:from_date }}">>, [{conference_date, {{2006,7,15},{8,0,0}} },{from_date, {{2006,6,1},{0,0,0}} }],
                   <<"1 month, 2 weeks">>},
                  {"|title",
                   <<"{{ \"my title case\"|title }}">>, [],
                   <<"My Title Case">>},
                  {"|title (pre-formatted)",
                   <<"{{ \"My Title Case\"|title }}">>, [],
                   <<"My Title Case">>},
                  {"|title (wacky separators)",
                   <<"{{ \"my-title!case\"|title }}">>, [],
                   <<"My-Title!Case">>},
                  {"|title (numbers)",
                   <<"{{ \"my-title123CaSe\"|title }}">>, [],
                   <<"My-Title123case">>},
                  {"|title (Irish names)",
                   <<"{{ \"who's o'malley?\"|title }}">>, [],
                   <<"Who's O'Malley?">>},
                  {"|truncatechars:0",
                   <<"{{ var1|truncatechars:0 }}">>, [{var1, "Empty Me"}],
                   <<"...">>},
                  {"|truncatechars:14",
                   <<"{{ var1|truncatechars:14 }}">>, [{var1, "Truncate Me Please"}],
                   <<"Truncate Me...">>},
                  {"|truncatechars:17",
                   <<"{{ var1|truncatechars:17 }}">>, [{var1, "Don't Truncate Me"}],
                   <<"Don't Truncate Me">>},
                  {"|truncatechars:4 (UTF-8)",
                   <<"{{ var1|truncatechars:4 }}">>, [{var1, "\x{E2}\x{82}\x{AC}1.99"}],
                   <<"\x{E2}\x{82}\x{AC}...">>},
                  {"|truncatechars:5 (UTF-8)",
                   <<"{{ var1|truncatechars:5 }}">>, [{var1, "\x{E2}\x{82}\x{AC} 1.99"}],
                   <<"\x{E2}\x{82}\x{AC} ...">>},
                  {"|truncatewords:0",
                   <<"{{ var1|truncatewords:0 }}">>, [{var1, "Empty Me"}],
                   <<" ...">>},
                  {"|truncatewords:2",
                   <<"{{ var1|truncatewords:2 }}">>, [{var1, "Truncate Me Please"}],
                   <<"Truncate Me ...">>},
                  {"|truncatewords:3",
                   <<"{{ var1|truncatewords:3 }}">>, [{var1, "Don't Truncate Me"}],
                   <<"Don't Truncate Me">>},
                  {"|truncatewords_html:4",
                   <<"{{ var1|truncatewords_html:4 }}">>, [{var1, "<p>The <strong>Long and <em>Winding</em> Road</strong> is too long</p>"}],
                   <<"<p>The <strong>Long and <em>Winding</em>...</strong></p>">>},
                  {"|unordered_list",
                   <<"{{ var1|unordered_list }}">>, [{var1, ["States", ["Kansas", ["Lawrence", "Topeka"], "Illinois"]]}],
                   <<"<li>States<ul><li>Kansas<ul><li>Lawrence</li><li>Topeka</li></ul></li><li>Illinois</li></ul></li>">>},
                  {"|upper",
                   <<"{{ message|upper }}">>, [{message, "That man has a gun."}],
                   <<"THAT MAN HAS A GUN.">>},
                  {"|urlencode",
                   <<"{{ url|urlencode }}">>, [{url, "You #$*@!!"}],
                   <<"You%20%23%24%2A%40%21%21">>},
                  {"|urlencode",
                   <<"{{ url|urlencode }}">>, [{url, "http://www.example.org/foo?a=b&c=d"}],
                   <<"http%3A//www.example.org/foo%3Fa%3Db%26c%3Dd">>},
                  {"|urlencode",
                   <<"{{ url|urlencode:\"\" }}">>, [{url, "http://www.example.org/foo?a=b&c=d"}],
                   <<"http%3A%2F%2Fwww.example.org%2Ffoo%3Fa%3Db%26c%3Dd">>},
                  {"|urlencode",
                   <<"{{ url|urlencode:\":/?=&\" }}">>, [{url, "http://www.example.org/foo?a=b&c=d"}],
                   <<"http://www.example.org/foo?a=b&c=d">>},
                  {"|urlize",
                   <<"{{ var|urlize }}">>, [{var, "Check out www.djangoproject.com"}],
                   <<"Check out <a href=\"http://www.djangoproject.com\" rel=\"nofollow\">www.djangoproject.com</a>">>},
                  {"|urlize",
                   <<"{{ var|urlize }}">>, [{var, "Check out http://www.djangoproject.com"}],
                   <<"Check out <a href=\"http://www.djangoproject.com\" rel=\"nofollow\">http://www.djangoproject.com</a>">>},
                  {"|urlize",
                   <<"{{ var|urlize }}">>, [{var, "Check out \"http://www.djangoproject.com\""}],
                   <<"Check out \"<a href=\"http://www.djangoproject.com\" rel=\"nofollow\">http://www.djangoproject.com</a>\"">>},
                  {"|urlizetrunc:15",
                   <<"{{ var|urlizetrunc:15 }}">>, [{var, "Check out www.djangoproject.com"}],
                   <<"Check out <a href=\"http://www.djangoproject.com\" rel=\"nofollow\">www.djangopr...</a>">>},
                  {"|wordcount",
                   <<"{{ words|wordcount }}">>, [{words, "Why Hello There!"}],
                   <<"3">>},
                  {"|wordwrap:2",
                   <<"{{ words|wordwrap:2 }}">>, [{words, "this is"}],
                   <<"this \nis">>},
                  {"|wordwrap:100",
                   <<"{{ words|wordwrap:100 }}">>, [{words, "testing    testing"}],
                   <<"testing    testing">>},
                  {"|wordwrap:10",
                   <<"{{ words|wordwrap:10 }}">>, [{words, ""}],
                   <<"">>},
                  {"|wordwrap:1",
                   <<"{{ words|wordwrap:1 }}">>, [{words, "two"}],
                   <<"two">>},
                                                % yesno match: \?assert.*\( (.*), erlydtl_filters:(.*)\((.*), "(.*)"\)\)
                                                % yesno replace: \t\t{"|$2:\"$4\"",\n\t\t\t\t\t <<"{{ var|$2:\"$4\" }}">>, [{var, $3}],\n\t\t\t\t\t<<$1>>}
                  {"|yesno:\"yeah,no,maybe\"",
                   <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, true}],
                   <<"yeah">>},
                  {"|yesno:\"yeah,no,maybe\"",
                   <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, false}],
                   <<"no">>},
                  {"|yesno:\"yeah,no\"",
                   <<"{{ var|yesno:\"yeah,no\" }}">>, [{var, undefined}],
                   <<"no">>},
                  {"|yesno:\"yeah,no,maybe\"",
                   <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, undefined}],
                   <<"maybe">>},

                  {"string |yesno:\"yeah,no,maybe\"",
                   <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, "non-empty string"}],
                   <<"yeah">>},
                  {"binary |yesno:\"yeah,no,maybe\"",
                   <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, <<"non-empty binary">>}],
                   <<"yeah">>},
                  {"empty string |yesno:\"yeah,no,maybe\"",
                   <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, ""}],
                   <<"no">>},
                  {"empty binary |yesno:\"yeah,no\"",
                   <<"{{ var|yesno:\",no\" }}">>, [{var, <<"">>}],
                   <<"no">>},
                  {"term |yesno:\"yeah,,maybe\"",
                   <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, {my, [term, "test"]}}],
                   <<"yeah">>},
                  {"|yesno:\"yeah,\"",
                   <<"{{ var|yesno:\"yeah,\" }}">>, [{var, false}],
                   <<"">>},
                  {"|yesno:\"yeah,,maybe\"",
                   <<"{{ var|yesno:\"yeah,,maybe\" }}">>, [{var, false}],
                   <<"">>},
                  {"|yesno:\"missing_false_choice\"",
                   <<"{{ var|yesno:\"missing_false_choice\" }}">>, [{var, true}],
                   {error, {yesno, choices}}}
                 ]},
     {"filters_if", [
                     {"Filter if 1.1",
                      <<"{% if var1|length_is:0 %}Y{% else %}N{% endif %}">>,
                      [{var1, []}],
                      <<"Y">>},
                     {"Filter if 1.2",
                      <<"{% if var1|length_is:1 %}Y{% else %}N{% endif %}">>,
                      [{var1, []}],
                      <<"N">>},
                     {"Filter if 1.3",
                      <<"{% if var1|length_is:7 %}Y{% else %}N{% endif %}">>,
                      [{var1, []}],
                      <<"N">>},
                     {"Filter if 2.1",
                      <<"{% if var1|length_is:0 %}Y{% else %}N{% endif %}">>,
                      [{var1, ["foo"]}],
                      <<"N">>},
                     {"Filter if 2.2",
                      <<"{% if var1|length_is:1 %}Y{% else %}N{% endif %}">>,
                      [{var1, ["foo"]}],
                      <<"Y">>},
                     {"Filter if 2.3",
                      <<"{% if var1|length_is:7 %}Y{% else %}N{% endif %}">>,
                      [{var1, ["foo"]}],
                      <<"N">>},
                     {"Filter if 3.1",
                      <<"{% ifequal var1|length 0 %}Y{% else %}N{% endifequal %}">>,
                      [{var1, []}],
                      <<"Y">>},
                     {"Filter if 3.2",
                      <<"{% ifequal var1|length 1 %}Y{% else %}N{% endifequal %}">>,
                      [{var1, []}],
                      <<"N">>},
                     {"Filter if 4.1",
                      <<"{% ifequal var1|length 3 %}Y{% else %}N{% endifequal %}">>,
                      [{var1, ["foo", "bar", "baz"]}],
                      <<"Y">>},
                     {"Filter if 4.2",
                      <<"{% ifequal var1|length 0 %}Y{% else %}N{% endifequal %}">>,
                      [{var1, ["foo", "bar", "baz"]}],
                      <<"N">>},
                     {"Filter if 4.3",
                      <<"{% ifequal var1|length 1 %}Y{% else %}N{% endifequal %}">>,
                      [{var1, ["foo", "bar", "baz"]}],
                      <<"N">>}
                    ]},
     {"firstof", [
                  {"Firstof first",
                   <<"{% firstof foo bar baz %}">>,
                   [{foo, "1"},{bar, "2"}],
                   <<"1">>},
                  {"Firstof second",
                   <<"{% firstof foo bar baz %}">>,
                   [{bar, "2"}],
                   <<"2">>},
                  {"Firstof none",
                   <<"{% firstof foo bar baz %}">>,
                   [],
                   <<"">>},
                  {"Firstof complex",
                   <<"{% firstof foo.bar.baz bar %}">>,
                   [{foo, [{bar, [{baz, "quux"}]}]}],
                   <<"quux">>},
                  {"Firstof undefined complex",
                   <<"{% firstof foo.bar.baz bar %}">>,
                   [{bar, "bar"}],
                   <<"bar">>},
                  {"Firstof literal",
                   <<"{% firstof foo bar \"baz\" %}">>,
                   [],
                   <<"baz">>}
                 ]},
     {"regroup", [
                  {"Ordered", <<"{% regroup people by gender as gender_list %}{% for gender in gender_list %}{{ gender.grouper }}\n{% for item in gender.list %}{{ item.first_name }}\n{% endfor %}{% endfor %}{% endregroup %}">>,
                   [{people, [[{first_name, "George"}, {gender, "Male"}], [{first_name, "Bill"}, {gender, "Male"}],
                              [{first_name, "Margaret"}, {gender, "Female"}], [{first_name, "Condi"}, {gender, "Female"}]]}],
                   <<"Male\nGeorge\nBill\nFemale\nMargaret\nCondi\n">>},
                  {"Unordered", <<"{% regroup people by gender as gender_list %}{% for gender in gender_list %}{{ gender.grouper }}\n{% for item in gender.list %}{{ item.first_name }}\n{% endfor %}{% endfor %}{% endregroup %}">>,
                   [{people, [[{first_name, "George"}, {gender, "Male"}],
                              [{first_name, "Margaret"}, {gender, "Female"}],
                              [{first_name, "Condi"}, {gender, "Female"}],
                              [{first_name, "Bill"}, {gender, "Male"}]
                             ]}],
                   <<"Male\nGeorge\nFemale\nMargaret\nCondi\nMale\nBill\n">>},
                  {"NestedOrdered", <<"{% regroup people by name.last as lastname_list %}{% for lastname in lastname_list %}{{ lastname.grouper }}\n{% for item in lastname.list %}{{ item.name.first }}\n{% endfor %}{% endfor %}{% endregroup %}">>,
                   [{people, [[{name, [{first,"George"},{last,"Costanza"}]}],
                              [{name, [{first,"Margaret"},{last,"Costanza"}]}],
                              [{name, [{first,"Bill"},{last,"Buffalo"}]}],
                              [{name, [{first,"Condi"},{last,"Buffalo"}]}]]}],
                   <<"Costanza\nGeorge\nMargaret\nBuffalo\nBill\nCondi\n">>},
                  {"NestedUnordered", <<"{% regroup people by name.last as lastname_list %}{% for lastname in lastname_list %}{{ lastname.grouper }}\n{% for item in lastname.list %}{{ item.name.first }}\n{% endfor %}{% endfor %}{% endregroup %}">>,
                   [{people, [[{name, [{first,"George"},{last,"Costanza"}]}],
                              [{name, [{first,"Bill"},{last,"Buffalo"}]}],
                              [{name, [{first,"Margaret"},{last,"Costanza"}]}],
                              [{name, [{first,"Condi"},{last,"Buffalo"}]}]]}],
                   <<"Costanza\nGeorge\nBuffalo\nBill\nCostanza\nMargaret\nBuffalo\nCondi\n">>},
                  {"Filter", <<"{% regroup people|dictsort:\"name.last\" by name.last as lastname_list %}{% for lastname in lastname_list %}{{ lastname.grouper }}\n{% for item in lastname.list %}{{ item.name.first }}\n{% endfor %}{% endfor %}{% endregroup %}">>,
                   [{people, [[{name, [{first,"George"},{last,"Costanza"}]}],
                              [{name, [{first,"Bill"},{last,"Buffalo"}]}],
                              [{name, [{first,"Margaret"},{last,"Costanza"}]}],
                              [{name, [{first,"Condi"},{last,"Buffalo"}]}]]}],
                   <<"Buffalo\nBill\nCondi\nCostanza\nGeorge\nMargaret\n">>}
                 ]},
     {"spaceless", [
                    {"Beginning", <<"{% spaceless %}    <b>foo</b>{% endspaceless %}">>, [], <<"<b>foo</b>">>},
                    {"Middle", <<"{% spaceless %}<b>foo</b>  <b>bar</b>{% endspaceless %}">>, [], <<"<b>foo</b><b>bar</b>">>},
                    {"End", <<"{% spaceless %}<b>foo</b>  {% endspaceless %}">>, [], <<"<b>foo</b>">>},
                    {"NewLine", <<"{% spaceless %}\n<div> \n <b>foo</b> \n </div>\n {% endspaceless %}">>, [], <<"<div><b>foo</b></div>">>}
                   ]},
     {"templatetag", [
                      {"openblock", <<"{% templatetag openblock %}">>, [], <<"{%">>},
                      {"closeblock", <<"{% templatetag closeblock %}">>, [], <<"%}">>},
                      {"openvariable", <<"{% templatetag openvariable %}">>, [], <<"{{">>},
                      {"closevariable", <<"{% templatetag closevariable %}">>, [], <<"}}">>},
                      {"openbrace", <<"{% templatetag openbrace %}">>, [], <<"{">>},
                      {"closebrace", <<"{% templatetag closebrace %}">>, [], <<"}">>},
                      {"opencomment", <<"{% templatetag opencomment %}">>, [], <<"{#">>},
                      {"closecomment", <<"{% templatetag closecomment %}">>, [], <<"#}">>}
                     ]},
     {"trans",
      [
       {"trans functional default locale",
        <<"Hello {% trans \"Hi\" %}">>, [], <<"Hello Hi">>
       },
       {"trans functional reverse locale",
        <<"Hello {% trans \"Hi\" %}">>, [], [{locale, "reverse"}],
        [{blocktrans_locales, ["reverse"]}, {blocktrans_fun, fun("Hi"=Key, "reverse") -> list_to_binary(lists:reverse(Key)) end}],
        <<"Hello iH">>
       },
       {"trans literal at run-time",
        <<"Hello {% trans \"Hi\" %}">>, [], [{translation_fun, fun("Hi") -> "Konichiwa" end}], [],
        <<"Hello Konichiwa">>},
       {"trans variable at run-time",
        <<"Hello {% trans var1 %}">>, [{var1, <<"Hi">>}], [{translation_fun, fun(<<"Hi">>) -> <<"Konichiwa">> end}], [],
        <<"Hello Konichiwa">>},
       {"trans literal at run-time: No-op",
        <<"Hello {% trans \"Hi\" noop %}">>, [], [{translation_fun, fun("Hi") -> <<"Konichiwa">> end}], [],
        <<"Hello Hi">>},
       {"trans variable at run-time: No-op",
        <<"Hello {% trans var1 noop %}">>, [{var1, <<"Hi">>}], [{translation_fun, fun(<<"Hi">>) -> <<"Konichiwa">> end}], [],
        <<"Hello Hi">>}
      ]},
     {"blocktrans",
      [
       {"blocktrans default locale",
        <<"{% blocktrans %}Hello{% endblocktrans %}">>, [], <<"Hello">>},
       {"blocktrans choose locale",
        <<"{% blocktrans %}Hello, {{ name }}{% endblocktrans %}">>, [{name, "Mr. President"}], [{locale, "de"}],
        [{blocktrans_locales, ["de"]}, {blocktrans_fun, fun("Hello, {{ name }}", "de") -> <<"Guten tag, {{ name }}">> end}], <<"Guten tag, Mr. President">>},
       {"blocktrans with args",
        <<"{% blocktrans with var1=foo %}{{ var1 }}{% endblocktrans %}">>, [{foo, "Hello"}], <<"Hello">>},
       {"blocktrans blocks in content not allowed",
        <<"{% blocktrans %}Hello{%if name%}, {{ name }}{%endif%}!{% endblocktrans %}">>, [],
        {error, [error_info([{{1, 24}, erlydtl_parser, ["syntax error before: ",["\"if\""]]}])], []}},
       {"blocktrans nested variables not allowed",
        <<"{% blocktrans %}Hello, {{ user.name }}!{% endblocktrans %}">>, [],
        {error, [error_info([{{1,31}, erlydtl_parser, ["syntax error before: ","'.'"]}])], []}},
       {"blocktrans runtime",
        <<"{% blocktrans with v1=foo%}Hello, {{ name }}! See {{v1}}.{%endblocktrans%}">>,
        [{name, "Mr. President"}, {foo, <<"rubber-duck">>}],
        [{translation_fun, fun("Hello, {{ name }}! See {{ v1 }}.") -> <<"Guten tag, {{name}}! Sehen {{    v1   }}.">> end}],
        [], <<"Guten tag, Mr. President! Sehen rubber-duck.">>}
      ]},
     {"verbatim", [
                   {"Plain verbatim",
                    <<"{% verbatim %}{{ oh no{% foobar %}{% endverbatim %}">>, [],
                    <<"{{ oh no{% foobar %}">>},
                   {"Named verbatim",
                    <<"{% verbatim foobar %}{% verbatim %}{% endverbatim foobar2 %}{% endverbatim foobar %}">>, [],
                    <<"{% verbatim %}{% endverbatim foobar2 %}">>}
                  ]},
     {"widthratio", [
                     {"Literals", <<"{% widthratio 5 10 100 %}">>, [], <<"50">>},
                     {"Rounds up", <<"{% widthratio a b 100 %}">>, [{a, 175}, {b, 200}], <<"88">>}
                    ]},
     {"with", [
               {"Cache literal",
                <<"{% with a=1 %}{{ a }}{% endwith %}">>, [], <<"1">>},
               {"Cache variable",
                <<"{% with a=b %}{{ a }}{% endwith %}">>, [{b, "foo"}], <<"foo">>},
               {"Cache variable with attribute",
                <<"I enjoy {% with a = var1 %}{{ a.game }}{% endwith %}">>, [{var1, [{game, "Othello"}]}], <<"I enjoy Othello">>},
               {"Cache variable attribute",
                <<"I enjoy {% with a = var1.game %}{{ a }}{% endwith %}">>, [{var1, [{game, "Othello"}]}], <<"I enjoy Othello">>},
               {"Cache multiple",
                <<"{% with alpha=1 beta=b %}{{ alpha }}/{{ beta }}{% endwith %}">>, [{b, 2}], <<"1/2">>}
              ]},
     {"unicode", [
                  {"(tm) somewhere",
                   <<"™">>, [], <<"™">>}
                 ]},
     {"contrib_humanize", [
                           {"intcomma",
                            <<"{{ a|intcomma }} {{ b|intcomma }} {{ c|intcomma }} {{ d|intcomma }}">>,
                            [{a, 999}, {b, 123456789}, {c, 12345}, {d, 1234567890}],
                            <<"999 123,456,789 12,345 1,234,567,890">>}
                          ]},
     %% custom syntax stuff
     {"extension_module",
      [ %% the erlydtl_extension_test module replaces a foo identifier with bar when hitting a # following foo.
        {"replace parsed token", <<"{{ foo # }}">>, [{bar, "ok"}], [],
         [{extension_module, erlydtl_extension_test}], <<"ok">>},
        {"proper error message", <<"{{ bar # }}">>, [{bar, "ok"}], [],
         [{extension_module, erlydtl_extension_test}],
         {error, [error_info([{1,erlydtl_extension_test,"Unexpected '#' in code at column 8"}])], []}},
        %% accept identifiers as expressions (this is a dummy functionality to test the parser extensibility)
        {"identifiers as expressions", <<"{{ foo.bar or baz }}">>, [{baz, "ok"}], [],
         [{extension_module, erlydtl_extension_test}], <<"ok">>}
      ]},
     {"records",
      [{"field access",
        <<"{{ r.baz }}">>, [{r, #testrec{ foo="Foo", bar="Bar", baz="Baz" }}], [],
        [{record_info, [{testrec, record_info(fields, testrec)}]}],
        <<"Baz">>}
      ]},
     {"error reporting",
      [{"no out dir warning",
        <<"foo bar">>,
        [], [], %% Vars, RenderOpts
        %%[report], %% CompilerOpts
        ?GRP_ERROR_REPORTING_COMPILER_OPTS,
        <<"foo bar">>, %% Output
        [error_info([no_out_dir])] %% Warnings
       },
       {"warnings as errors",
        <<"foo bar">>,
        [], [],
        %%[report, warnings_as_errors],
        [warnings_as_errors|?GRP_ERROR_REPORTING_COMPILER_OPTS],
        {error, %% Output...
         [error_info([no_out_dir])], %% Errors
         [] %% Warnings
        }
       },
       {"illegal character",
        <<"{{{">>,
        [], [],
        %%[report],
        ?GRP_ERROR_REPORTING_COMPILER_OPTS,
        {error,
         [error_info(
            [{{1,3},erlydtl_scanner,{illegal_char, ${}}] )],
         []
        }
       },
       {"unexpected end of file - in code",
        <<"{{">>,
        [], [],
        ?GRP_ERROR_REPORTING_COMPILER_OPTS,
        {error,
         [error_info(
           [{{1,3},erlydtl_scanner,{eof, in_code}}] )],
         []
        }
       },
       {"unexpected end of file - in comment",
        <<"{#">>,
        [], [],
        ?GRP_ERROR_REPORTING_COMPILER_OPTS,
        {error,
         [error_info(
           [{{1,3},erlydtl_scanner,{eof, in_comment}}] )],
         []
        }
       }
      ]}
    ].

%% {Name, DTL, Vars, Output}
%% {Name, DTL, Vars, RenderOpts, Output}
%% {Name, DTL, Vars, RenderOpts, CompilerOpts, Output}
%% {Name, DTL, Vars, RenderOpts, CompilerOpts, Output, Warnings}

run_tests() ->
    io:format("Running unit tests..."),
    {Times, Failures} =
        lists:foldl(
          fun({Group, Assertions}, {GroupTs, GroupEs}) ->
                  io:format("~n Test group ~p ", [Group]),
                  {Ts, Es} =
                      lists:foldl(
                        fun(Setup, {Ts, Es}) ->
                                try process_unit_test(Setup) of
                                    {ok, T} ->
                                        io:format("."),
                                        {merge_times(T, Ts), Es};
                                    {T, E} ->
                                        io:format("!"),
                                        {merge_times(T, Ts), [E|Es]}
                                catch
                                    Class:Error ->
                                        {Ts, [format_error(
                                                element(1, Setup),
                                                Class, Error)
                                              |Es]}
                                end
                        end, {[], []}, Assertions),
                  Ts1 = merge_times(Ts, GroupTs),
                  if length(Es) =:= 0 ->
                          io:format("~n~s (msec)", [format_times(Ts, length(Assertions))]),
                          {Ts1, GroupEs};
                     true ->
                          {Ts1, [{Group, Es}|GroupEs]}
                  end
          end, {[], []}, tests()),

    case length(Failures) of
        0 ->
            io:format("~nAll unit tests PASS~nTotal~s (msec)~n~n", [format_times(Times)]);
        Length ->
            io:format("~n### FAILED groups: ~b ####~n", [Length]),
            [begin
                 io:format("  Group: ~s (~b failures)~n", [Group, length(Failed)]),
                 [io:format("    Test: ~s~n~s~n", [Name, Error])
                  || {Name, Error} <- lists:reverse(Failed)]
             end || {Group, Failed} <- lists:reverse(Failures)],
            throw(failed)
    end.

merge_times(Ts1, Ts2) ->
    merge_times(Ts1, Ts2, []).

merge_times([{K, V1}|Ts1], [{K, V2}|Ts2], Acc) ->
    merge_times(Ts1, Ts2, [{K, V1 + V2}|Acc]);
merge_times(Ts1, [T|Ts2], Acc) ->
    merge_times(Ts1, Ts2, [T|Acc]);
merge_times([T|Ts1], [], Acc) ->
    merge_times(Ts1, [], [T|Acc]);
merge_times([], [], Acc) ->
    lists:reverse(Acc).

format_times(Ts) ->
    [io_lib:format("  ~p ~.3f", [K, V / 1000]) || {K, V} <- Ts].

format_times(Ts, Count) ->
    [io_lib:format("  ~p ~.3f (~.3f)", [K, V / 1000, V / 1000 / Count])
     || {K, V} <- Ts].

format_error(Name, Class, Error) ->
    io:format("!"),
    {Name, io_lib:format("~s:~p~n  ~p", [Class, Error, erlang:get_stacktrace()])}.

compile_test(DTL, Opts) ->
    Options = [force_recompile,
               {auto_escape, false},
               return_errors, return_warnings,
               {custom_filters_modules, [erlydtl_contrib_humanize]}
               |Opts],
    timer:tc(erlydtl, compile, [DTL, erlydtl_running_test, Options]).

render_test(Vars, RenderOpts) ->
    timer:tc(erlydtl_running_test, render, [Vars, RenderOpts]).

test_pass(T) ->
    {ok, T}.

test_fail(Name, Fmt, Args, T) ->
    {T, {Name, io_lib:format(Fmt, Args)}}.

process_unit_test({Name, DTL, Vars, Output}) ->
    process_unit_test({Name, DTL, Vars, [], [], Output, default_warnings()});
process_unit_test({Name, DTL, Vars, RenderOpts, Output}) ->
    process_unit_test({Name, DTL, Vars, RenderOpts, [], Output, default_warnings()});
process_unit_test({Name, DTL, Vars, RenderOpts, CompilerOpts, Output}) ->
    process_unit_test({Name, DTL, Vars, RenderOpts, CompilerOpts, Output, default_warnings()});
process_unit_test({Name, DTL, Vars, RenderOpts, CompilerOpts, Output, Warnings}) ->
    case compile_test(DTL, CompilerOpts) of
        {Tcompile, {ok, _, Warnings}} ->
            Tc = [{compile, Tcompile}],
            case render_test(Vars, RenderOpts) of
                {TrenderL, {ok, IOList}} ->
                    TrL = [{render_list, TrenderL}|Tc],
                    case render_test(vars_to_binary(Vars), RenderOpts) of
                        {TrenderB, {ok, IOListBin}} ->
                            TrB = [{render_binary, TrenderB}|TrL],
                            case {iolist_to_binary(IOList), iolist_to_binary(IOListBin)} of
                                {Output, Output} ->
                                    test_pass(TrB);
                                {Output, Unexpected} ->
                                    test_fail(
                                      Name,
                                      "Unexpected result with binary variables: ~n"
                                      "Expected: ~p~n"
                                      "Actual: ~p",
                                      [Output, Unexpected], TrB);
                                {Unexpected, Output} ->
                                    test_fail(
                                      Name,
                                      "Unexpected result with list variables: ~n"
                                      "Expected: ~p~n"
                                      "Actual: ~p",
                                      [Output, Unexpected], TrB);
                                {Unexpected1, Unexpected2} ->
                                    test_fail(
                                      Name,
                                      "Unexpected result: ~n"
                                      "Expected: ~p~n"
                                      "Actual (list): ~p~n"
                                      "Actual (binary): ~p",
                                      [Output, Unexpected1, Unexpected2], TrB)
                            end;
                        {TrenderB, Output} ->
                            test_pass([{render_binary, TrenderB}|TrL]);
                        {TrenderB, Err} ->
                            test_fail(Name, "Render error (with binary variables): ~p", [Err],
                                      [{render_binary, TrenderB}|TrL])
                    end;
                {TrenderL, Output} ->
                    test_pass([{render_list, TrenderL}|Tc]);
                {TrenderL, Err} ->
                    test_fail(Name, "Render error (with list variables): ~p", [Err],
                              [{render_list, TrenderL}|Tc])
            end;
        {Tcompile, {ok, _, ActualWarnings}} ->
            test_fail(
              Name,
              "Unexpected warnings: ~p~n"
              "Expected: ~p",
              [ActualWarnings, Warnings], [{compile, Tcompile}]);
        {Tcompile, Output} -> test_pass([{compile, Tcompile}]);
        {Tcompile, Err} ->
            test_fail(Name, "Compile error: ~p~nExpected: ~p",
                      [Err, Output], [{compile, Tcompile}])
    end.


vars_to_binary(Vars) when is_list(Vars) ->
    lists:map(fun
                  ({Key, [H|_] = Value}) when is_tuple(H) ->
                     {Key, vars_to_binary(Value)};
                  ({Key, [H|_] = Value}) when is_integer(H) ->
                     {Key, list_to_binary(Value)};
                  ({Key, Value}) ->
                     {Key, Value}
             end, Vars);
vars_to_binary(Vars) ->
    Vars.

generate_test_date() ->
    {{Y,M,D}, _} = erlang:localtime(),
    MonthName = [
                 "January", "February", "March", "April",
                 "May", "June", "July", "August", "September",
                 "October", "November", "December"
                ],
    OrdinalSuffix = [
                     "st","nd","rd","th","th","th","th","th","th","th", % 1-10
                     "th","th","th","th","th","th","th","th","th","th", % 10-20
                     "st","nd","rd","th","th","th","th","th","th","th", % 20-30
                     "st"
                    ],
    list_to_binary([
                    "It is the ",
                    integer_to_list(D),
                    lists:nth(D, OrdinalSuffix),
                    " of ", lists:nth(M, MonthName),
                    " ", integer_to_list(Y), "."
                   ]).


default_warnings() ->
    [error_info([no_out_dir])].

error_info(File, Ws) ->
    {File, [error_info(W) || W <- Ws]}.

error_info({Line, ErrorDesc})
  when is_integer(Line) ->
  {Line, erlydtl_compiler, ErrorDesc};
error_info({Line, Module, _}=ErrorDesc)
  when is_integer(Line), is_atom(Module) ->
    ErrorDesc;
error_info({{Line, Col}, Module, _}=ErrorDesc)
  when is_integer(Line), is_integer(Col), is_atom(Module) ->
    ErrorDesc;
error_info(Ws) when is_list(Ws) ->
    error_info("erlydtl_running_test", Ws);
error_info(ErrorDesc) ->
    {none, erlydtl_compiler, ErrorDesc}.
