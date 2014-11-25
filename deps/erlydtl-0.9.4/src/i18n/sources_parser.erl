%% Author: dave
%% Author: Sergey Prokhorov <me@seriyps.ru> (new/ext API)
%% Created: Mar 1, 2010
%% @doc:
%% Parses source files and extracts translation directives on templates
%% Examples:
%% <pre>
%% Tpl = <<"111"
%%         "{#Translators: btrans comment #}{%blocktrans%}btrns{%endblocktrans%}"
%%         "{%comment%}  TRANSLATORS: trans comment {%endcomment%}222{%trans 'trns'%}"
%%         "333">>,
%% Phrases = sources_parser:parse_content("filename.dtl", Tpl),
%% Msgids = [sources_parser:phrase_info(msgid, P) || P <- Phrases].
%% %% -> ["btrns", "trns"]
%% InOldFormat = [begin
%%                  [Str, File, Line, Col] = sources_parser:phrase_info([msgid, file, line, col], P),
%%                  {Str, {File, Line, Col}}
%%                end || P <- Phrases].
%% %% -> [{"btrns", {"filename.dtl", 1, 47}}, {"trns", {"filename.dtl", 1, 135}]
%% </pre>
-module(sources_parser).

%%
%% Exported Functions
%%

%% New API
-export([parse_pattern/1, parse_file/1, parse_content/2, phrase_info/2]).
%% Deprecated API
-export([parse/0, parse/1, process_content/2]).

-export_type([phrase/0, compat_phrase/0, field/0]).

%%
%% Include files
%%

-include("include/erlydtl_ext.hrl").

-record(phrase, {msgid :: string(),
                 msgid_plural :: string() | undefined,
                 context :: string() | undefined,
                 comment :: string() | undefined,
                 file :: string(),
                 line :: non_neg_integer(),
                 col :: non_neg_integer()}).
-record(state, {acc=[], translators_comment}).

-opaque phrase() :: #phrase{}.
-type compat_phrase() :: {string(), {string(), non_neg_integer(), non_neg_integer()}}.
-type field() :: msgid | msgid_plural | context | comment | file | line | col.

-define(bail(Fmt, Args),
        throw(lists:flatten(io_lib:format(Fmt, Args)))).

-define(GET_FIELD(Key), phrase_info(Key, #phrase{ Key = Value }) -> Value).

%%
%% API Functions
%%

%% Old API
parse() ->
    Parsed_Files = parse(["./views/*/*.html"]),
    io:format("Parsed files are ~p~n",[Parsed_Files]).

parse(Pattern) ->
    to_compat(parse_pattern(Pattern)).

process_content(Path, Content) ->
    to_compat(parse_content(Path, Content)).

%% @doc convert new API output to old one.
-spec to_compat([phrase()]) -> [compat_phrase()].
to_compat(Phrases) ->
    [{Str, {File, Line, Col}}
     || #phrase{msgid=Str, file=File, line=Line, col=Col}
            <- Phrases].

%% New API

%% @doc extract info about phrase.
%% See `field()' type for list of available info field names.
-spec phrase_info([field()] | field(), phrase()) -> [Info] | Info when
      Info :: non_neg_integer() | string() | undefined.
?GET_FIELD(msgid);
?GET_FIELD(msgid_plural);
?GET_FIELD(context);
?GET_FIELD(comment);
?GET_FIELD(file);
?GET_FIELD(line);
?GET_FIELD(col);
phrase_info(Fields, Phrase) when is_list(Fields) ->
    %% you may pass list of fields
    [phrase_info(Field, Phrase) || Field <- Fields].

%% @doc list files, using wildcard and extract phrases from them
-spec parse_pattern([string()]) -> [phrase()].
parse_pattern(Pattern) ->
    %%We assume a basedir
    GetFiles = fun(Path,Acc) -> Acc ++ [F || F <- filelib:wildcard(Path), filelib:is_regular(F)] end,
    Files = lists:foldl(GetFiles,[],Pattern),
    io:format("Parsing files ~p~n",[Files]),
    ParsedFiles = [parse_file(File) || File <- Files],
    lists:flatten(ParsedFiles).

%% @doc extract phrases from single file
parse_file(Path) ->
    case file:read_file((Path)) of
        {ok, Content} ->
            parse_content(Path, Content);
        Error ->
            ?bail("Cannot read file ~s problem ~p~n", [Path, Error])
    end.

%% @doc extract phrases from string / binary
-spec parse_content(string(), binary()) -> [phrase()].
parse_content(Path,Content)->
    case erlydtl_compiler:do_parse_template(Content, #dtl_context{}) of
        {ok, Data} ->
            try process_ast(Path, Data) of
                {ok, Result} -> Result
            catch
                Error:Reason ->
                    io:format("~s: Template processing failed~nData: ~p~n", [Path, Data]),
                    erlang:raise(Error, Reason, erlang:get_stacktrace())
            end;
        Error ->
            ?bail("Template parsing failed for template ~s, cause ~p~n", [Path, Error])
    end.


%%
%% Local Functions
%%

process_ast(Fname, Tokens) ->
    State = process_ast(Fname, Tokens, #state{}),
    {ok, State#state.acc}.

process_ast(Fname, Tokens, State) when is_list(Tokens) ->
    lists:foldl(
      fun (Token, St) ->
              process_token(Fname, Token, St)
      end, State, Tokens);
process_ast(Fname, Token, State) ->
    process_token(Fname, Token, State).


%%Block are recursivelly processed, trans are accumulated and other tags are ignored
process_token(Fname, {block,{identifier,{_Line,_Col},_Identifier},Children}, St) -> process_ast(Fname, Children, St);
process_token(Fname, {trans,Text}, #state{acc=Acc, translators_comment=Comment}=St) ->
    {{Line, Col}, String} = trans(Text),
    Phrase = #phrase{msgid=unescape(String),
                     comment=Comment,
                     file=Fname,
                     line=Line,
                     col=Col},
    St#state{acc=[Phrase | Acc], translators_comment=undefined};
process_token(Fname,
              {trans,Text,{string_literal, _, Context}},
              #state{acc=Acc, translators_comment=Comment}=St) ->
    {{Line, Col}, String} = trans(Text),
    Phrase = #phrase{msgid=unescape(String),
                     context=unescape(Context),
                     comment=Comment,
                     file=Fname,
                     line=Line,
                     col=Col},
    St#state{acc=[Phrase | Acc], translators_comment=undefined};
process_token(Fname, {blocktrans, Args, Contents, PluralContents}, #state{acc=Acc, translators_comment=Comment}=St) ->
    {Fname, Line, Col} = guess_blocktrans_lc(Fname, Args, Contents),
    Phrase = #phrase{msgid=unparse(Contents),
                     msgid_plural=unparse(PluralContents),
                     context=case proplists:get_value(context, Args) of
                                 {string_literal, _, String} ->
                                     erlydtl_compiler_utils:unescape_string_literal(String);
                                 undefined -> undefined
                             end,
                     comment=Comment,
                     file=Fname,
                     line=Line,
                     col=Col},
    St#state{acc=[Phrase | Acc], translators_comment=undefined};
process_token(_, {comment, Comment}, St) ->
    St#state{translators_comment=maybe_translators_comment(Comment)};
process_token(_Fname, {comment_tag, _Pos, Comment}, St) ->
    St#state{translators_comment=translators_comment_text(Comment)};
process_token(Fname, {_Instr, _Cond, Children}, St) -> process_ast(Fname, Children, St);
process_token(Fname, {_Instr, _Cond, Children, Children2}, St) ->
    StModified = process_ast(Fname, Children, St),
    process_ast(Fname, Children2, StModified);
process_token(_,_AST,St) -> St.

trans({noop, Value}) ->
    trans(Value);
trans({string_literal,Pos,String}) -> {Pos, String}.

unescape(String) -> string:sub_string(String, 2, string:len(String) -1).

unparse(undefined) -> undefined;
unparse(Contents) -> erlydtl_unparser:unparse(Contents).

%% hack to guess ~position of blocktrans
guess_blocktrans_lc(Fname, [{{identifier, {L, C}, _}, _} | _], _) ->
    %% guess by 1'st with
    {Fname, L, C - length("blocktrans with ")};
guess_blocktrans_lc(Fname, _, [{string, {L, C}, _} | _]) ->
    %% guess by 1'st string
    {Fname, L, C - length("blocktrans %}")};
guess_blocktrans_lc(Fname, _, [{variable, {identifier, {L, C}, _}} | _]) ->
    %% guess by 1'st {{...}}
    {Fname, L, C - length("blocktrans %}")};
guess_blocktrans_lc(Fname, _, _) ->
    {Fname, -1, -1}.


maybe_translators_comment([{string, _Pos, S}]) ->
    translators_comment_text(S);
maybe_translators_comment(Other) ->
    %% smth like "{%comment%}Translators: Hey, {{var}} is variable substitution{%endcomment%}"
    Unparsed = erlydtl_unparser:unparse(Other),
    translators_comment_text(Unparsed).

translators_comment_text(S) ->
    Stripped = string:strip(S, left),
    case "translators:" == string:to_lower(string:substr(Stripped, 1, 12)) of
        true -> S;
        false -> undefined
    end.
