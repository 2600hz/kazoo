%% The MIT License
%%
%% Copyright (c) 2009 Tom Preston-Werner <tom@mojombo.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% See the README at http://github.com/mojombo/mustache.erl for additional
%% documentation and usage examples.

-module(mustache).  %% v0.1.0
-author("Tom Preston-Werner").
-export([compile/1, compile/2, render/1, render/2, render/3, get/2, get/3, escape/1, start/1]).

-record(mstate, {mod = undefined,
                 section_re = undefined,
                 tag_re = undefined}).

-define(MUSTACHE_CTX, mustache_ctx).
-define(MUSTACHE_CTX_STR, "mustache_ctx").
-define(MUSTACHE_STR, "mustache").

compile(Body) when is_list(Body) ->
  State = #mstate{},
  CompiledTemplate = pre_compile(Body, State),
  % io:format("~p~n~n", [CompiledTemplate]),
  % io:format(CompiledTemplate ++ "~n", []),
  {ok, Tokens, _} = erl_scan:string(CompiledTemplate),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Bindings = erl_eval:new_bindings(),
  {value, Fun, _} = erl_eval:expr(Form, Bindings),
  Fun;
compile(Mod) ->
  TemplatePath = template_path(Mod),
  compile(Mod, TemplatePath).

compile(Mod, File) ->
  code:purge(Mod),
  {module, _} = code:load_file(Mod),
  {ok, TemplateBin} = file:read_file(File),
  Template = re:replace(TemplateBin, "\"", "\\\\\"", [global, {return,list}]),
  State = #mstate{mod = Mod},
  CompiledTemplate = pre_compile(Template, State),
  % io:format("~p~n~n", [CompiledTemplate]),
  % io:format(CompiledTemplate ++ "~n", []),
  {ok, Tokens, _} = erl_scan:string(CompiledTemplate),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Bindings = erl_eval:new_bindings(),
  {value, Fun, _} = erl_eval:expr(Form, Bindings),
  Fun.

render(Mod) ->
  TemplatePath = template_path(Mod),
  render(Mod, TemplatePath).

render(Body, Ctx) when is_list(Body) ->
  TFun = compile(Body),
  render(undefined, TFun, Ctx);
render(Mod, File) when is_list(File) ->
  render(Mod, File, []);
render(Mod, CompiledTemplate) ->
  render(Mod, CompiledTemplate, []).

render(Mod, File, Ctx) when is_list(File) ->
  CompiledTemplate = compile(Mod, File),
  render(Mod, CompiledTemplate, Ctx);
render(Mod, CompiledTemplate, CtxData) ->
  Ctx0 = ?MUSTACHE_CTX:new(CtxData),
  Ctx1 = ?MUSTACHE_CTX:module(Mod, Ctx0),
  lists:flatten(CompiledTemplate(Ctx1)).

pre_compile(T, State) ->
  SectionRE = "{{(#|\\^)([^}]*)}}\\s*(.+?){{/\\2}}\\s*",
  {ok, CompiledSectionRE} = re:compile(SectionRE, [dotall]),
  TagRE = "{{(#|=|!|<|>|{|&)?(.+?)\\1?}}+",
  {ok, CompiledTagRE} = re:compile(TagRE, [dotall]),
  State2 = State#mstate{section_re = CompiledSectionRE, tag_re = CompiledTagRE},
  "fun(Ctx) -> " ++
    compiler(T, State2) ++ " end.".

compiler(T, State) ->
  Res = re:run(T, State#mstate.section_re),
  case Res of
    {match, [{M0, M1}, {K0, K1}, {N0, N1}, {C0, C1}]} ->
      Front = string:substr(T, 1, M0),
      Back = string:substr(T, M0 + M1 + 1),
      Kind = string:substr(T, K0 + 1, K1),
      Name = string:substr(T, N0 + 1, N1),
      Content = string:substr(T, C0 + 1, C1),
      "[" ++ compile_tags(Front, State) ++
        " | [" ++ compile_section(Kind, Name, Content, State) ++
        " | [" ++ compiler(Back, State) ++ "]]]";
    nomatch ->
      compile_tags(T, State)
  end.

compile_section("#", Name, Content, State) ->
  Mod = State#mstate.mod,
  Result = compiler(Content, State),
  "fun() -> " ++
    "case " ++ ?MUSTACHE_STR ++ ":get(" ++ Name ++ ", Ctx, " ++ atom_to_list(Mod) ++ ") of " ++
      "\"true\" -> " ++ Result ++ "; " ++
      "\"false\" -> []; " ++
      "List when is_list(List) -> " ++
        "[fun(Ctx) -> " ++ Result ++ " end(" ++ ?MUSTACHE_CTX_STR ++ ":merge(SubCtx, Ctx)) || SubCtx <- List]; " ++
      "Else -> " ++
        "throw({template, io_lib:format(\"Bad context for ~p: ~p\", [" ++ Name ++ ", Else])}) " ++
    "end " ++
  "end()";
compile_section("^", Name, Content, State) ->
  Mod = State#mstate.mod,
  Result = compiler(Content, State),
  "fun() -> " ++
    "case " ++ ?MUSTACHE_STR ++ ":get(" ++ Name ++ ", Ctx, " ++ atom_to_list(Mod) ++ ") of " ++
      "\"false\" -> " ++ Result ++ "; " ++
      "[] -> " ++ Result ++ "; " ++
      "_ -> [] "
    "end " ++
  "end()".

compile_tags(T, State) ->
  Res = re:run(T, State#mstate.tag_re),
  case Res of
    {match, [{M0, M1}, K, {C0, C1}]} ->
      Front = string:substr(T, 1, M0),
      Back = string:substr(T, M0 + M1 + 1),
      Content = string:substr(T, C0 + 1, C1),
      Kind = tag_kind(T, K),
      Result = compile_tag(Kind, Content, State),
      "[\"" ++ escape_special(Front) ++
        "\" | [" ++ Result ++
        " | " ++ compile_tags(Back, State) ++ "]]";
    nomatch ->
      "[\"" ++ escape_special(T) ++ "\"]"
  end.

tag_kind(_T, {-1, 0}) ->
  none;
tag_kind(T, {K0, K1}) ->
  string:substr(T, K0 + 1, K1).

compile_tag(none, Content, State) ->
  compile_escaped_tag(Content, State);
compile_tag("&", Content, State) ->
  compile_unescaped_tag(Content, State);
compile_tag("{", Content, State) ->
  compile_unescaped_tag(Content, State);
compile_tag("!", _Content, _State) ->
  "[]".

compile_escaped_tag(Content, State) ->
  Mod = State#mstate.mod,
  ?MUSTACHE_STR ++ ":escape(" ++ ?MUSTACHE_STR ++ ":get(" ++ Content ++ ", Ctx, " ++ atom_to_list(Mod) ++ "))".

compile_unescaped_tag(Content, State) ->
  Mod = State#mstate.mod,
  ?MUSTACHE_STR ++ ":get(" ++ Content ++ ", Ctx, " ++ atom_to_list(Mod) ++ ")".

template_dir(Mod) ->
  DefaultDirPath = filename:dirname(code:which(Mod)),
  case application:get_env(mustache, templates_dir) of
    {ok, DirPath} when is_list(DirPath) ->
      case filelib:ensure_dir(DirPath) of
        ok -> DirPath;
        _  -> DefaultDirPath
      end;
    _ ->
      DefaultDirPath
  end.
template_path(Mod) ->
  DirPath = template_dir(Mod),
  Basename = atom_to_list(Mod),
  filename:join(DirPath, Basename ++ ".mustache").

get(Key, Ctx, Mod) ->
  get(Key, ?MUSTACHE_CTX:module(Mod, Ctx)).

get(Key, Ctx) when is_list(Key) ->
  get(list_to_atom(Key), Ctx);
get(Key, Ctx) ->
  case ?MUSTACHE_CTX:get(Key, Ctx) of
    {ok, Value} -> to_s(Value);
    {error, _} -> []
  end.


to_s(Val) when is_integer(Val) ->
  integer_to_list(Val);
to_s(Val) when is_float(Val) ->
  io_lib:format("~.2f", [Val]);
to_s(Val) when is_atom(Val) ->
  atom_to_list(Val);
to_s(Val) ->
  Val.

escape(HTML) ->
  escape(HTML, []).

escape([], Acc) ->
  lists:reverse(Acc);
escape([$< | Rest], Acc) ->
  escape(Rest, lists:reverse("&lt;", Acc));
escape([$> | Rest], Acc) ->
  escape(Rest, lists:reverse("&gt;", Acc));
escape([$& | Rest], Acc) ->
  escape(Rest, lists:reverse("&amp;", Acc));
escape([X | Rest], Acc) ->
  escape(Rest, [X | Acc]).

escape_special(String) ->
    lists:flatten([escape_char(Char) || Char <- String]).

escape_char($\0) -> "\\0";
escape_char($\n) -> "\\n";
escape_char($\t) -> "\\t";
escape_char($\b) -> "\\b";
escape_char($\r) -> "\\r";
escape_char($')  -> "\\'";
escape_char($")  -> "\\\"";
escape_char($\\) -> "\\\\";
escape_char(Char) -> Char.

%%---------------------------------------------------------------------------

start([T]) ->
  Out = render(list_to_atom(T)),
  io:format(Out ++ "~n", []).
