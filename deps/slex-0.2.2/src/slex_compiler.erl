%%%-------------------------------------------------------------------
%%% File:      slex_compiler.erl
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2013 Andreas Stenius
%%% @doc
%%% Stateful Lexical Analyzer Compiler.
%%%
%%% Compiles a slex file mimicking the original erlydtl_scanner
%%% implementation, based on the rules in the slex file.
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
-module(slex_compiler).

-export([compile/1, compile/2, format_error/1]).

-import(proplists, [get_value/3, get_all_values/2]).

-import(erl_syntax, [abstract/1, application/2, application/3,
                     arity_qualifier/2, atom/1, attribute/2,
                     block_expr/1, case_expr/2, char/1, comment/1,
                     clause/2, clause/3, form_list/1, fun_expr/1,
                     function/2, if_expr/1, infix_expr/3, integer/1,
                     list/1, list/2, match_expr/2, operator/1,
                     revert_forms/1, set_precomments/2, string/1,
                     tuple/1, underscore/0, variable/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(rule, {prio, prefix, in_state, guard, body, comments}).
-record(tag, {tag, guard, body, comments}).


%% ===================================================================
%% API
%% ===================================================================

compile(Input) ->
    compile(Input, []).

compile(Data, Options) when is_binary(Data) ->
    case scan_and_parse(binary_to_list(Data), Options) of
        {ok, Scanner} ->
            Forms = compile_module(Scanner, Options),
            case get_value(target, Options, beam) of
                beam ->
                    case compile:forms(revert_forms(Forms), [return]) of
                        {ok, Mod, Bin, _} ->
                            code:purge(Mod),
                            case code:load_binary(Mod, atom_to_list(Mod) ++ ".tsd", Bin) of
                                {module, Mod} -> {ok, Mod, Bin};
                                Err -> Err
                            end;
                        Err -> Err
                    end;
                erl ->
                    {ok, io_lib:format(
                           "~s~n",
                           [erl_prettypr:format(Forms)]
                          )}
            end;
        Err -> Err
    end;
compile(File, Options) ->
    {ok, Data} = file:read_file(File),
    case compile(Data, [{source, File}|Options]) of
        {ok, Mod, Bin}=Res ->
            case get_value(out_dir, Options) of
                undefined -> Res;
                OutDir ->
                    OutFile = filename:join(
                                [OutDir, atom_to_list(Mod) ++ ".beam"]),
                    case file:write_file(OutFile, Bin) of
                        ok -> {ok, Mod, OutFile};
                        Err -> Err
                    end
            end;
        {ok, Src}=Res ->
            case get_value(out_dir, Options) of
                undefined -> Res;
                OutDir ->
                    OutFile = filename:join(
                               [OutDir, filename:basename(File, ".slex") ++ ".erl"]),
                    case file:write_file(OutFile, Src) of
                        ok -> {ok, OutFile};
                        Err -> Err
                    end
            end;
        Err -> Err
    end.

format_error({multiple_append_actions, Prefix}) ->
    io_lib:format(
      "Multiple actions not supported with append, for ~s.",
      [case Prefix of
           {prefix, P} -> io_lib:format("prefix ~p", [P]);
           any_prefix -> "`any' prefix"
       end]).

%% TODO: format errors for attribute_error and bad_state_transition.


%% ===================================================================
%% Internal functions
%% ===================================================================

get_value(Key, Props) ->
    get_value(Key, Props, default(Key)).

get_attr1(Key, Attrs) ->
    case get_value(Key, Attrs) of
        {[Attr], Comments} ->
            {atom(Attr), comment(Comments)};
        Attr -> throw({?MODULE, {attribute_error, {Key, Attr}}})
    end.

default(attr) -> [];
default(rule) -> [];
default(tag) -> [];
default(function) -> {[scan], []};
default(init_state) -> {[root], []};
default(_) -> undefined.

parsed(rule, Rules) ->
    {rule, lists:keysort(#rule.prio, Rules)};
parsed(Key, Values) ->
    {Key, Values}.

include_files(Parsed) ->
    lists:flatten(
      lists:reverse(
        lists:foldl(
          fun ({attr, {include, {[Lib, File], _}}}, Acc) ->
                  Path = filename:join(code:lib_dir(Lib), File),
                  Included =
                      case file:read_file(Path) of
                          {ok, Data} ->
                              case slex_scanner:string(
                                     binary_to_list(Data)) of
                                  {ok, Tokens, _} ->
                                      case slex_parser:parse(Tokens) of
                                          {ok, P} -> [{source, {Lib, File}}|P];
                                          {error, Err, _State} -> Err
                                      end;
                                  {error, Err, _} -> Err
                              end;
                          Err -> Err
                      end,
                  if is_list(Included) -> [Included|Acc];
                     true -> throw({?MODULE, {include_file, Path, Included}})
                  end;
              (Prop, Acc) -> [Prop|Acc]
          end,
          [], Parsed))).

scan_and_parse(String, Options) ->
    String1 = case get_value(extra_data, Options) of
                  undefined -> String;
                  Data when is_list(Data) ->
                      String ++ Data;
                  {file, File} ->
                      {ok, Data} = file:read_file(File),
                      String ++ binary_to_list(Data)
              end,
    case slex_scanner:string(String1) of
        {ok, Tokens, _} ->
            case slex_parser:parse(Tokens) of
                {ok, Parsed} ->
                    Props = include_files(Parsed),
                    Keys = proplists:get_keys(Props),
                    Scanner = [parsed(Key, get_all_values(Key, Props)) || Key <- Keys],
                    {ok, Scanner};
                {error, Err, _State} -> {error, Err}
            end;
        {error, Err, _} -> {error, Err}
    end.

compile_module(Scanner, Options) ->
    form_list(
      lists:foldr(
        fun (F, Acc) ->
                F(Scanner, Options, Acc)
        end,
        [],
        [fun compile_head_forms/3,
         fun compile_funcs/3
        ]
       )).

compile_head_forms(Scanner, Options, Acc) ->
    Attrs = get_value(attr, Scanner),
    {Mod, ModC} = get_attr1(module, Attrs),
    {Fun, FunC} = get_attr1(function, Attrs),

    ModuleAttribute = set_precomments(
                        attribute(atom(module), [Mod]),
                        [comment(["%%%% THIS IS A SLEX GENERATED FILE %%%%%\n"]),
                         ModC]),
    ExportAttribute = set_precomments(
                        attribute(
                          atom(export),
                          [list(
                             [arity_qualifier(Fun, integer(1)),
                              arity_qualifier(Fun, integer(4))
                             ])]),
                        [FunC]),

    SlexVsn =
        case application:get_key(slex, vsn) of
            undefined ->
                ok = application:load(slex),
                {ok, V} = application:get_key(slex, vsn),
                V;
            {ok, V} -> V
        end,
    {{Y,M,D},{H,Mm,S}} = calendar:universal_time(),
    MetaAttribute = set_precomments(
                      attribute(
                        atom(slex_source),
                        [abstract([get_value(source, Options, '<string>')
                                   |get_value(source, Scanner, [])])
                        ]),
                      [comment(
                         [io_lib:format(
                            "% This file was generated "
                            "~4b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b"
                            " UTC by slex ~s.",
                            [Y, M, D, H, Mm, S, SlexVsn]),
                          "% http://github.com/erlydtl/slex"
                         ])
                      ]),
    Forms =
        case get_value(form, Scanner) of
            undefined -> [];
            Fs -> [set_precomments(F, [comment(Fc)]) || {F, Fc} <- Fs]
        end,

    [ModuleAttribute, MetaAttribute, ExportAttribute | Forms] ++ Acc.

compile_funcs(Scanner, _Options, Acc) ->
    Attrs = get_value(attr, Scanner),
    Rules = get_value(rule, Scanner),
    Tags = get_value(tag, Scanner),

    {Name, _} = get_attr1(function, Attrs),
    [compile_scanner_api(Name, Attrs),
     compile_scanner(Name, Rules)
     | compile_post_process(atom(post_process), Tags, Acc)].

compile_scanner_api(Name, Attrs) ->
    {State, _} = get_attr1(init_state, Attrs),
    function(
      Name, 
      [clause(
         [variable('Template')],
         [application(atom(is_list), [variable('Template')])],
         [application(
            Name, [variable('Template'), list([]),
                   tuple([integer(1), integer(1)]),
                   State])
         ]),
       clause(
         [variable('Template')],
         [application(atom(is_binary), [variable('Template')])],
         [application(
            Name, [application(atom(binary_to_list),
                               [variable('Template')])
                  ])
         ])
      ]).

compile_scanner(Name, Rules) ->
    function(Name, [compile_rule(Name, Rule) || Rule <- Rules]).

compile_post_process(Name, Tags, Acc) ->
    TagClauses = [compile_tag(Name, Tag) || Tag <- Tags],
    CatchallClause = clause([underscore(), variable('T'), underscore()],
                            none,
                            [variable('T')]),

    %% pick up Tag trail to go from Name/2 to Name/3..
    ListClause = clause(
                   [list([variable('S')], variable('Ss')), variable('N')],
                   none,
                   [list([application(
                            Name,
                            [variable('Ss'), variable('S'), variable('N')])],
                         variable('Ss'))]),
    TagClause = clause(
                  [variable('T'), variable('N')],
                  none,
                  [application(Name, [atom(undefined), variable('T'), variable('N')])]),

    [function(Name, TagClauses ++ [CatchallClause]), %% Name/3
     function(Name, [ListClause, TagClause]) %% Name/2
     | Acc].

compile_rule(Name, #rule{ comments=Comments }=Rule) ->
    set_precomments(
      compile_rule_clause(Name, Rule),
      [comment(Comments)]).

compile_rule_clause(_Name, #rule{ prefix=Prefix, in_state=InState, body={code, Body} }=Rule) ->
    {Pprefix, _Apos} = compile_prefix(Prefix),
    Pstate = compile_match_state(InState, Prefix, keep_state),
    Ppos = match_expr(
             tuple([variable('R'), variable('C')]),
             variable('P')),

    clause(
      [Pprefix,
       variable('S'),
       Ppos,
       Pstate
      ],
      compile_guard(Rule),
      Body);

compile_rule_clause(Name, #rule{ prefix=Prefix, in_state=InState, body={Actions, OutState} }=Rule) ->
    {Pprefix, Apos} = compile_prefix(Prefix),
    {Ppos, Ascanned} = compile_actions(Actions, Prefix),
    Pstate = compile_match_state(InState, Prefix, OutState),
    Astate = compile_new_state(OutState),

    clause(
      [Pprefix,
       variable('S'),
       Ppos,
       Pstate
      ],
      compile_guard(Rule),
      [application(
         Name,
         [variable('T'),
          Ascanned,
          Apos,
          Astate
         ])
      ]).

compile_prefix({prefix,P}) ->
    Prefix = lists:foldl(fun ($\n, {R,_}) -> {R+1,1};
                             (_  , {R,C}) -> {R,C+1}
                         end,
                         {0, 0},
                         P),
    {infix_expr(string(P), operator('++'), variable('T')),
     tuple(case Prefix of
               {0,C} ->
                   [variable('R'),
                    infix_expr(
                      variable('C'),
                      operator('+'),
                      integer(C)
                     )];
               {R,C} ->
                   [infix_expr(
                      variable('R'),
                      operator('+'),
                      integer(R)),
                    integer(C)]
           end)};
compile_prefix(any_prefix) ->
    {list([variable('H')], variable('T')),
     case_expr(
       variable('H'),
       [clause(
          [char($\n)], none, 
          [tuple([infix_expr(variable('R'), operator('+'), integer(1)),
                  integer(1)])]),
        clause(
          [underscore()], none,
          [tuple([variable('R'),
                  infix_expr(variable('C'), operator('+'), integer(1))])])
       ])};
compile_prefix(end_of_input) ->
    {list([]),
     tuple([variable('R'), variable('C')])}.

compile_actions([], _Prefix) ->
    {tuple([variable('R'), variable('C')]), variable('S')};
compile_actions(Actions, Prefix) ->
    {Tags, N} = lists:foldl(
                  fun (Action, {Ts, N0}) ->
                          {T, N} = compile_action(Action, Prefix, N0),
                          {[T|Ts], N0 + N}
                  end,
                  {[], 0}, Actions),
    {match_expr(
       tuple([variable('R'), variable('C')]),
       variable('P')),
     if N == 0 -> list(Tags, application(
                               atom(post_process),
                               [variable('S'),
                                atom(element(2, hd(Actions)))
                               ]));
        N == 1, length(Tags) == 1 -> hd(Tags);
        true -> throw({?MODULE, {multiple_append_actions, Prefix}})
     end}.

compile_action({Action, Tag, Value}, _Prefix, N) ->
    compile_action({Action, Tag}, {prefix, Value}, N);
compile_action({add, Tag}, Prefix, _N) ->
    {tuple(
       [atom(Tag), variable('P')
        | if is_atom(Tag) ->
                  case Prefix of
                      any_prefix -> [list([variable('H')])];
                      {prefix,P} -> [string(P)]
                  end;
             true -> []
          end
       ]), 0};
compile_action({append, Tag}, Prefix, _N) ->
    P = case Prefix of
            any_prefix -> [variable('H')];
            {prefix, S} -> string(lists:reverse(S))
        end,
    {case_expr(
       variable('S'),
       [clause(
          [list(
             [match_expr(
                tuple([atom(Tag), underscore(), variable('L')]),
                variable('M'))],
             variable('Ss'))
          ],
          none,
          [list([application(
                   atom(setelement),
                   [integer(3),
                    variable('M'),
                    if is_list(P) -> list(P, variable('L'));
                       true -> infix_expr(P, operator('++'),
                                          variable('L'))
                    end])
                ],
                variable('Ss'))
          ]),
        clause(
          [underscore()],
          none,
          [list(
             [tuple(
                [atom(Tag), variable('P'),
                 if is_list(P) -> list(P);
                    true -> P
                 end])
             ],
             application(
               atom(post_process),
               [variable('S'),
                atom(Tag)]))
          ])
       ]), 1}.

compile_match_state(any_stateless, Prefix, {state,S})
  when not is_tuple(S) -> throw({?MODULE, {bad_state_transition, Prefix, S}});
compile_match_state(any_stateless, Prefix, _) -> compile_in_state(any_stateless, Prefix);
compile_match_state(State, Prefix, keep_state) ->
    match_expr(
      compile_in_state(State, Prefix),
      variable('St'));
compile_match_state(State, Prefix, _) ->
    compile_in_state(State, Prefix).

compile_in_state({state, S}, _Prefix) -> tuple([atom(S), variable('E')]);
compile_in_state({stateless, S}, _Prefix) -> atom(S);
compile_in_state(any_state, _Prefix) -> tuple([underscore(), variable('E')]);
compile_in_state(any_stateless, _Prefix) -> variable('St');
compile_in_state(close_any_state, Prefix) ->
    case Prefix of
        any_prefix -> underscore();
        {prefix,P} -> tuple([underscore(), string(P)])
    end.

compile_new_state({state, {S, E}}) -> tuple([atom(S), string(E)]);
compile_new_state({state, S}) -> tuple([atom(S), variable('E')]);
compile_new_state({stateless, S}) -> atom(S);
compile_new_state(keep_state) -> variable('St').

compile_guard(#rule{ in_state=any_stateless, guard={guard, Guard} }) ->
    [application(atom(is_atom), [variable('St')])|Guard];
compile_guard(#rule{ guard={guard, Guard} }) -> Guard.

compile_tag(Name, #tag{ comments=Comments }=Tag) ->
    set_precomments(
      compile_tag_clause(Name, Tag),
      [comment(Comments)]).

compile_tag_clause(_Name, #tag{ tag=Tag, guard={guard, Guard}, body=Body }) ->
    clause(
      compile_tag_pattern(Tag),
      Guard,
      case Body of
          [{code, C}] -> C;
          _ -> [application(
                  atom(setelement),
                  [integer(3), variable('T'),
                   block_expr(
                     [match_expr(
                        tag_var(N),
                        compile_tag_body(B, N - 1))
                      || {B, N} <- lists:zip(
                                     Body,
                                     lists:seq(1, length(Body)))
                     ] ++ [tag_var(length(Body))])
                  ])
               ]
      end).

compile_tag_pattern({Ts, {S, T}})
  when S =:= state; S =:= stateless ->
    compile_tag_pattern(Ts, atom(T));
compile_tag_pattern(Ts) ->
    compile_tag_pattern(Ts, underscore()).

compile_tag_pattern([T|Ts], NextT) ->
    [compile_tag_trail(Ts),
     match_expr(
       case T of
           {state, S} ->
               tuple([atom(S), underscore(), tag_var(0)]);
           {stateless, S} ->
               tuple([atom(S), underscore()])
       end,
       variable('T')),
     NextT
    ].

compile_tag_trail([]) -> underscore();
compile_tag_trail(Ts) ->
    list([compile_tag_trail_pattern(S) || S <- Ts],
         underscore()).

compile_tag_trail_pattern({state, T}) -> tuple([atom(T), underscore(), underscore()]);
compile_tag_trail_pattern({stateless, T}) -> tuple([atom(T), underscore()]).

tag_var(0) -> variable('L');
tag_var(N) -> variable([$L|integer_to_list(N)]).

compile_tag_body([], N) -> tag_var(N);
compile_tag_body([M, F], N) ->
    application(atom(M), atom(F), [tag_var(N)]);
compile_tag_body([F], N) ->
    application(atom(F), [tag_var(N)]);
compile_tag_body({code, C}, _) -> block_expr(C).


-ifdef (TEST).

scan_and_parse(String) ->
    scan_and_parse(String, []).

-define(test_scan_and_parse(String, Prop, Expect),
        {ok, Scanner} = scan_and_parse(String),
        Ps = get_value(Prop, Scanner),
        ?assertMatch(Expect, Ps)).

-define(test_compile(Prop, Values, Pretty),
        Clause = case Prop of
                     rule -> [compile_rule(atom(t), Rule) || Rule <- Values];
                     tag -> [compile_tag(atom(t), Tag) || Tag <- Values]
                 end,
        Tree = function(atom(t), Clause),
        ?assertEqual(
           Pretty,
           erl_prettypr:format(Tree))
       ).

-define(test_scan_parse_compile(String, Prop, Expect, Pretty),
        ?test_scan_and_parse(String, Prop, Expect),
        ?test_compile(Prop, Ps, Pretty)).

-define(test_attr(String, Attr),
       ?test_scan_and_parse(String, attr, [Attr])).
-define(test_attrs(String, Attrs),
       ?test_scan_and_parse(String, attr, Attrs)).

-define(test_rule(String, Rule, Result),
        ?test_scan_parse_compile(String, rule, [Rule], Result)).

-define(test_rules(String, Rules, Result),
        ?test_scan_parse_compile(String, rule, Rules, Result)).

-define(test_tag(String, Tag, Result),
        ?test_scan_parse_compile(String, tag, [Tag], Result)).

-define(test_bad_rule(String, Rule, Error),
        ?test_scan_and_parse(String, rule, [Rule]),
        ?assertThrow(Error, compile_rule(atom(t), hd(Ps)))).

-define(_test_attr(String, Attr),
        {?LINE, fun () -> ?test_attr(String, Attr) end}).

-define(_test_attrs(String, Attrs),
        {?LINE, fun () -> ?test_attrs(String, Attrs) end}).

-define(_test_rule(String, Rule, Result),
        {?LINE, fun () -> ?test_rule(String, Rule, Result) end}).

-define(_test_rules(String, Rules, Result),
        {?LINE, fun () -> ?test_rules(String, Rules, Result) end}).

-define(_test_tag(String, Tag, Result),
        {?LINE, fun () -> ?test_tag(String, Tag, Result) end}).

-define(_test_bad_rule(String, Rule, Error),
        {?LINE, fun () -> ?test_bad_rule(String, Rule, Error) end}).

-define(_test_scan(T, E), ?_assertEqual(E, apply(M, F, [T]))).


attr_test_() ->
    [?_test_attr(
        "-module foo.",
        {module, {[foo], []}}),
     ?_test_attr(
        "%%% a comment.\n"
        "-my_attr test this.",
        {my_attr, {[test, this], ["%% a comment."]}}),

     %% test include, and that order does matter
     ?_test_attrs(
        "-include slex 'priv/test.slex'.",
        [{module, {[test_scanner], _}}|_]),
     ?_test_attrs(
        "-include slex 'priv/test.slex'.\n"
        "-module foo_bar.",
        [{module, {[test_scanner], _}}|_]),
     ?_test_attrs(
        "-module foo_bar.\n"
        "-include slex 'priv/test.slex'.",
        [{module, {[foo_bar], _}}|_])
    ].

compile_rule_test_() ->
    [?_test_rule(
        "0 {{ in_text-:open_var,in_code until}}.",
        {rule,
         {prio, 0},
         {prefix,"{{"},
         {stateless,in_text},
         {guard,[]},
         {[{add,open_var}],
          {state,{in_code,"}}"}}},
         []},
        "t(\"{{\" ++ T, S, {R, C} = P, in_text) ->\n"
        "    t(T, [{open_var, P, \"{{\"} | post_process(S, open_var)],\n"
        "      {R, C + 2}, {in_code, \"}}\"})."),
     ?_test_rule(
        "1 {# in_text-: in_comment until #}.",
        {rule,
         {prio, 1},
         {prefix,"{#"},
         {stateless,in_text},
         {guard,[]},
         {[],{state,{in_comment,"#}"}}},
         []},
        "t(\"{#\" ++ T, S, {R, C}, in_text) ->\n"
        "    t(T, S, {R, C + 2}, {in_comment, \"#}\"})."),
     ?_test_rule(
        "2 any in_text-: +string.",
        {rule,
         {prio, 2},
         any_prefix,
         {stateless,in_text},
         {guard,[]},
         {[{append,string}],
          keep_state},
         []},
        "t([H | T], S, {R, C} = P, in_text = St) ->\n"
        "    t(T,\n"
        "      case S of\n"
        "\t[{string, _, L} = M | Ss] ->\n"
        "\t    [setelement(3, M, [H | L]) | Ss];\n"
        "\t_ -> [{string, P, [H]} | post_process(S, string)]\n"
        "      end,\n"
        "      case H of\n"
        "\t$\\n -> {R + 1, 1};\n"
        "\t_ -> {R, C + 1}\n"
        "      end,\n"
        "      St)."),
     ?_test_rule(
        "3 \\\" in_code: string_literal, in_double_quote.",
        {rule,
         {prio, 3},
         {prefix,"\""},
         {state,in_code},
         {guard,[]},
         {[{add,string_literal}],
          {state,in_double_quote}},
         []},
        "t(\"\\\"\" ++ T, S, {R, C} = P, {in_code, E}) ->\n"
        "    t(T,\n"
        "      [{string_literal, P, \"\\\"\"} | post_process(S,\n"
        "\t\t\t\t\t\tstring_literal)],\n"
        "      {R, C + 1}, {in_double_quote, E})."),
     ?_test_rule(
        "4 }} any+: close_var, in_text-.",
        {rule,
         {prio, 4},
         {prefix,"}}"},
         close_any_state,
         {guard,[]},
         {[{add,close_var}],
          {stateless,in_text}},
         []},
        "t(\"}}\" ++ T, S, {R, C} = P, {_, \"}}\"}) ->\n"
        "    t(T,\n"
        "      [{close_var, P, \"}}\"} | post_process(S, close_var)],\n"
        "      {R, C + 2}, in_text)."),
     ?_test_rule(
        "5 \\\" in_double_quote: +string_literal, in_code.",
        {rule,
         {prio, 5},
         {prefix,"\""},
         {state,in_double_quote},
         {guard,[]},
         {[{append,string_literal}],
          {state,in_code}},
         []},
        "t(\"\\\"\" ++ T, S, {R, C} = P, {in_double_quote, E}) ->\n"
        "    t(T,\n"
        "      case S of\n"
        "\t[{string_literal, _, L} = M | Ss] ->\n"
        "\t    [setelement(3, M, \"\\\"\" ++ L) | Ss];\n"
        "\t_ ->\n"
        "\t    [{string_literal, P, \"\\\"\"} | post_process(S,\n"
        "\t\t\t\t\t\t      string_literal)]\n"
        "      end,\n"
        "      {R, C + 1}, {in_code, E})."),
     ?_test_rule(
        "6 == any: ==, in_code.",
        {rule,
         {prio, 6},
         {prefix,"=="},
         any_state,
         {guard,[]},
         {[{add,"=="}],
          {state,in_code}},
         []},
        "t(\"==\" ++ T, S, {R, C} = P, {_, E}) ->\n"
        "    t(T, [{'==', P} | post_process(S, '==')], {R, C + 2},\n"
        "      {in_code, E})."),
     ?_test_rule(
        "7 \\  in_code: skip.",
        {rule,
         {prio, 7},
         {prefix," "},
         {state,in_code},
         {guard,[]},
         {[], keep_state},
         []},
        "t(\" \" ++ T, S, {R, C}, {in_code, E} = St) ->\n"
        "    t(T, S, {R, C + 1}, St)."),
     ?_test_rule(
        "8 \\s any: skip, in_code.",
        {rule,
         {prio, 8},
         {prefix," "},
         any_state,
         {guard,[]},
         {[], {state,in_code}},
         []},
        "t(\" \" ++ T, S, {R, C}, {_, E}) ->\n"
        "    t(T, S, {R, C + 1}, {in_code, E})."),
     ?_test_rule(
        "9 any in_code, expr H >= $0 andalso H =< $9 orelse H == $- end"
        ": number_literal, in_number.",
        {rule,
         {prio, 9},
         any_prefix,
         {state,in_code},
         {guard,[{op,1,'orelse',_,_}]},
         {[{add,number_literal}],
          {state,in_number}},
         []},
        "t([H | T], S, {R, C} = P, {in_code, E})\n"
        "    when H >= $0 andalso H =< $9 orelse H == $- ->\n"
        "    t(T,\n"
        "      [{number_literal, P, [H]} | post_process(S,\n"
        "\t\t\t\t\t       number_literal)],\n"
        "      case H of\n"
        "\t$\\n -> {R + 1, 1};\n"
        "\t_ -> {R, C + 1}\n"
        "      end,\n"
        "      {in_number, E})."),
     ?_test_rule(
        "10 \! any: expr io:write(\"custom code!\"), w00t end.",
        {rule,
         {prio, 10},
         {prefix,"!"},
         any_state,
         {guard,[]},
         {code,[_,_]},
         []},
        "t(\"!\" ++ T, S, {R, C} = P, {_, E} = St) ->\n"
        "    io:write(\"custom code!\"), w00t."),
     ?_test_rule(
        "11 = any-:skip.",
        {rule,
         {prio, 11},
         {prefix,"="},
         any_stateless,
         {guard,[]},
         {[], keep_state},
         []},
        "t(\"=\" ++ T, S, {R, C}, St) when is_atom(St) ->\n"
        "    t(T, S, {R, C + 1}, St)."),
     ?_test_bad_rule(
        "12 ) any-: skip, in_code.",
        {rule,
         {prio, 12},
         {prefix,")"},
         any_stateless,
         {guard,[]},
         {[],{state,in_code}},
         []},
        {?MODULE, {bad_state_transition,{prefix,")"},in_code}}),
     ?_test_rule(
        "13 ( any-: skip, in_code until ).",
        {rule,
         {prio, 13},
         {prefix,"("},
         any_stateless,
         {guard,[]},
         {[],{state,{in_code,")"}}},
         []},
        "t(\"(\" ++ T, S, {R, C}, St) when is_atom(St) ->\n"
        "    t(T, S, {R, C + 1}, {in_code, \")\"})."),
     ?_test_rule(
        "14 \\n any: +foo.",
        {rule,
         {prio, 14},
         {prefix, "\n"},
         any_state,
         {guard,[]},
         {[{append,foo}],
          keep_state},
         []},
        "t(\"\\n\" ++ T, S, {R, C} = P, {_, E} = St) ->\n"
        "    t(T,\n"
        "      case S of\n"
        "\t[{foo, _, L} = M | Ss] ->\n"
        "\t    [setelement(3, M, \"\\n\" ++ L) | Ss];\n"
        "\t_ -> [{foo, P, \"\\n\"} | post_process(S, foo)]\n"
        "      end,\n"
        "      {R + 1, 1}, St)."),
     ?_test_rule(
        "15 _( any: \\_ \\(, in_code.",
        {rule,
         {prio, 15},
         {prefix, "_("},
         any_state,
         {guard,[]},
         {[{add,"_"},{add, "("}],
          {state,in_code}},
         []},
        "t(\"_(\" ++ T, S, {R, C} = P, {_, E}) ->\n"
        "    t(T, [{'(', P}, {'_', P} | post_process(S, '_')],\n"
        "      {R, C + 2}, {in_code, E})."),
     ?_test_rule(
        "16 \\' foo-: bar-\\\", baz-.",
        {rule,
         {prio, 16},
         {prefix, "'"},
         {stateless,foo},
         {guard,[]},
         {[{add,bar,"\""}],
          {stateless,baz}},
         []},
        "t(\"'\" ++ T, S, {R, C} = P, foo) ->\n"
        "    t(T, [{bar, P, \"\\\"\"} | post_process(S, bar)],\n"
        "      {R, C + 1}, baz)."),
     ?_test_rules(
        "17 'c' any:expr 17 end.\n"
        "5 'a' any:expr 5 end.\n"
        "10 \"b\" any:expr 10 end.",
        [{rule,
          {prio, 5},
          {prefix, "a"},
          any_state,
          {guard,[]},
          {code, _},
          []},
         {rule,
          {prio, 10},
          {prefix, "b"},
          any_state,
          {guard,[]},
          {code, _},
          []},
         {rule,
          {prio, 17},
          {prefix, "c"},
          any_state,
          {guard,[]},
          {code, _},
          []}],
        "t(\"a\" ++ T, S, {R, C} = P, {_, E} = St) -> 5;\n"
        "t(\"b\" ++ T, S, {R, C} = P, {_, E} = St) -> 10;\n"
        "t(\"c\" ++ T, S, {R, C} = P, {_, E} = St) -> 17."),
     ?_test_rule(
        "%%% rule comment.\n"
        "18 \\. any:expr ok end.",
        {rule,
         {prio, 18},
         {prefix, "."},
         any_state,
         {guard, []},
         {code, _},
         ["%% rule comment."]},
        "%%% rule comment.\n"
        "t(\".\" ++ T, S, {R, C} = P, {_, E} = St) -> ok.")
    ].

compile_tag_test_() ->
    [?_test_tag(
        "string: 'lists' 'reverse'.",
        {tag,
         [{state,string}],
         {guard,[]},
         [["lists","reverse"]],
         []},
        "t(_, {string, _, L} = T, _) ->\n"
        "    setelement(3, T, begin L1 = lists:reverse(L), L1 end)."),
     ?_test_tag(
        "identifier: \"lists\" reverse, 'list_to_atom'.",
        {tag,
         [{state,identifier}],
         {guard,[]},
         [["lists",reverse],["list_to_atom"]],
         []},
        "t(_, {identifier, _, L} = T, _) ->\n"
        "    setelement(3, T,\n"
        "\t       begin\n"
        "\t\t L1 = lists:reverse(L), L2 = list_to_atom(L1), L2\n"
        "\t       end)."),
     ?_test_tag(
        "foo: dummy, expr test:dummy(L1) end.",
        {tag,
         [{state,foo}],
         {guard,[]},
         [[dummy], {code, _}],
         []},
        "t(_, {foo, _, L} = T, _) ->\n"
        "    setelement(3, T,\n"
        "\t       begin\n"
        "\t\t L1 = dummy(L), L2 = begin test:dummy(L1) end, L2\n"
        "\t       end)."),
     ?_test_tag(
        "foo:expr setelement(1, T, bar) end.",
        {tag,
         [{state,foo}],
         {guard,[]},
         [{code,_}],
         []},
        "t(_, {foo, _, L} = T, _) -> setelement(1, T, bar)."),
     ?_test_tag(
        "foo, bar:expr setelement(1, T, baz) end.",
        {tag,
         {[{state,foo}],{state,bar}},
         {guard,[]},
         [{code,_}],
         []},
        "t(_, {foo, _, L} = T, bar) -> setelement(1, T, baz)."),
     ?_test_tag(
        "foo bar: to_atom.",
        {tag,
         [{state,bar},{state,foo}],
         {guard,[]},
         [[to_atom]],
         []},
        "t([{foo, _, _} | _], {bar, _, L} = T, _) ->\n"
        "    setelement(3, T, begin L1 = to_atom(L), L1 end)."),
     ?_test_tag(
        "%%% tag comment.\n"
        "foo:expr bar end.",
        {tag,
         [{state,foo}],
         {guard,[]},
         [{code, _}],
         ["%% tag comment."]},
        "%%% tag comment.\n"
        "t(_, {foo, _, L} = T, _) -> bar.")
    ].

scanner_test_() ->
    {setup,
     fun () ->
             compile(filename:join(code:priv_dir(slex), "test.slex"))
     end,
     fun ({ok, M, _}) ->
             F = scan,
             [?_test_scan(
                 "foo bar",
                 {ok, [{string, {1, 1}, "foo bar"}]}),
              ?_test_scan(
                 "foo {{ bar }}",
                 {ok, [{string, {1, 1}, "foo "},
                       {open_var, {1, 5}, '{{'},
                       {identifier, {1, 8}, bar},
                       {close_var, {1, 12}, '}}'}]}),
              ?_test_scan(
                 "{{ \"test\" }}",
                 {ok, [{open_var, {1, 1}, '{{'},
                       {string_literal, {1, 4}, "\"test\""},
                       {close_var, {1, 11}, '}}'}]}),
              ?_test_scan(
                 "{{ 12345 }}",
                 {ok, [{open_var, {1, 1}, '{{'},
                       {number_literal, {1, 4}, "12345"},
                       {close_var, {1, 10}, '}}'}]}),
              ?_test_scan(
                 "{{ foo.bar }}",
                 {ok, [{open_var, {1, 1}, '{{'},
                       {identifier, {1, 4}, foo},
                       {'.', {1, 7}},
                       {identifier, {1, 8}, bar},
                       {close_var, {1, 12}, '}}'}]}),
              ?_test_scan(
                 "{% if 1 %}",
                 {ok, [{open_tag, {1, 1}, '{%'},
                       {if_keyword, {1, 4}, "if"},
                       {number_literal, {1, 7}, "1"},
                       {close_tag, {1, 9}, '%}'}]}),
              ?_test_scan(
                 "{{ ~ }}",
                 {error, {1, M, "Illegal character in column 4"},
                  {scanner_state, "~ }}", [{open_var,{1,1},"{{"}], {1,4}, {in_code, "}}"}}
                 }),
              ?_test_scan(
                 "{{ \"\\\" '\" }}",
                 {ok, [{open_var, {1, 1}, '{{'},
                       {string_literal, {1, 4}, "\"\\\" '\""},
                       {close_var, {1, 11}, '}}'}]}),
              ?_test_scan(
                 "{% cycle 'a' 'b' %}",
                 {ok, [{open_tag, {1, 1}, '{%'},
                       {cycle_keyword, {1, 4}, "cycle"},
                       {string_literal, {1, 10}, "\"a\""},
                       {string_literal, {1, 14}, "\"b\""},
                       {close_tag, {1, 18}, '%}'}]}),
              ?_test_scan(
                 "{%with a=1%}{{a}}{%endwith%}",
                 {ok, [{open_tag, {1, 1}, '{%'},
                       {with_keyword, {1, 3},  "with"},
                       {identifier, {1, 8}, a},
                       {'=', {1, 9}},
                       {number_literal, {1, 10}, "1"},
                       {close_tag, {1, 11}, '%}'},
                       {open_var, {1, 13}, '{{'},
                       {identifier, {1, 15}, a},
                       {close_var, {1, 16}, '}}'},
                       {open_tag, {1, 18}, '{%'},
                       {endwith_keyword, {1, 20}, "endwith"},
                       {close_tag, {1, 27}, '%}'}
                      ]}),
              ?_test_scan(
                 "foo{% verbatim %}bar{% endverbatim %}baz",
                 {ok, [{string, {1, 1}, "foo"},
                       {string, {1, 18}, "barbaz"}]}),
              ?_test_scan(
                 "foo{% verbatim %}{% endverbatim %}bar",
                 {ok, [{string, {1, 1}, "foo"},
                       {string, {1, 18}, "bar"}]}),
              ?_test_scan(
                 "foo{% verbatim bar %}bar{% verbatim %}baz{% endverbatim baz %}{% endverbatim bar %}",
                 {ok, [{string, {1, 1}, "foo"},
                       {string, {1, 22}, "bar{% verbatim %}baz{% endverbatim baz %}"}]}
                )
             ];
         (Err) -> ?_test(throw({setup_error, Err}))
     end}.

extra_data_test() ->
    String =
        "-test orig."
        "10 'foo' bar: skip.",
    Extra =
        "5 'bar' foo: skip, bar until 'rab'."
        "-test extra.",
    Expect =
        [{attr,
          [{test, {[orig], []}},
           {test, {[extra], []}}]
         },
         {rule,
          [{rule,
            {prio, 5},
            {prefix, "bar"},
            {state, foo},
            {guard, []},
            {[], {state, {bar, "rab"}}},
            []},
           {rule,
            {prio, 10},
            {prefix, "foo"},
            {state, bar},
            {guard, []},
            {[], keep_state},
            []}
          ]}
        ],
    {ok, Scanner} = scan_and_parse(String, [{extra_data, Extra}]),
    ?assertMatch(Expect, Scanner).

-endif.
