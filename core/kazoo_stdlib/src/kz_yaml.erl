%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_yaml).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([encode/1, encode/2
        ,decode/1, decode/2
        ,decode_file/1, decode_file/2


        ,analyze_state/4
        ,analyze_string/3

        ,has_foldable_line/2
        ,block_header/2
        ,is_break_space/1

        ,escape_string/2
        ,encode_hex/1

        ,is_plain_safe_first/1
        ,is_plain_safe/1
        ,is_printable/1
        ,is_whitespace/1
        ]).

-define(JSON_WRAPPER(Proplist), {Proplist}).

-type decode_object() :: yamerl_constr:yamerl_doc() | [yamerl_constr:yamerl_doc()] |
                         yamerl_constr:yamerl_simple_doc() | [yamerl_constr:yamerl_simple_doc()] |
                         term().

-type node_scalar() :: atom() |
                       binary() |
                       boolean() |
                       float() |
                       integer().

-type node_map() :: #{yaml_node() => yaml_node()}.
-type node_seq() :: [yaml_node()].

-type yaml_node() :: node_scalar() |
                     node_map() |
                     node_seq().

-type case_styles() :: 'camelcase' | 'lowercase' | 'uppercase'.
-type string_style() :: 'double_qoute' | 'fold' | 'literal' | 'plain' | 'single_quote'.

-type options() :: #{compact => boolean()
                     %% Whether to compact writing mapping or sequence or not. Default is `true'.
                    ,indent  => 1..9
                     %% Indentation to use (in spaces). Default is 2.
                    ,line_width => integer()
                     %% Maximum line width when writing string scalar not counting the indentation. A long line without suitable
                     %% break point (non-consecutive white spaces) will exceed the width limit. If you don't want to
                     %% break long lines automatically set this to `-1'.
                     %% Default is 80 characters.
                    ,bool_style => case_styles()
                     %% Writing style for boolean scalar type. Default is lowercase.
                    ,null_style => 'canonical' | case_styles()
                     %% Writing style for `null' scalar type. Canonical is `~' character. Default is lowercase.
                    ,string_style => string_style()
                     %% Writing style for string scalar if it is possible to write the string in that style.
                    ,undefined_type => 'null' | 'undefined'
                     %% Write `undefined' as is or as `null'.
                    }.

-type state() :: #{result := iodata()
                  ,tag := kz_term:api_ne_binary()
                  ,explicit := boolean()
                  ,is_key := boolean()
                  ,empty_collection => boolean()

                  ,compact => boolean()
                  ,indent  => 1..9
                  ,line_width => integer()
                  ,bool_style => case_styles()
                  ,null_style => 'canonical' | case_styles()
                  ,string_style => string_style()
                  ,undefined_type => 'null' | 'undefined'
                  }.

%% @equiv encode(Yaml, #{})
-spec encode(yaml_node() | kz_json:object()) -> binary().
encode(Yaml) ->
    encode(Yaml, #{}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec encode(yaml_node() | kz_json:object(), options()) -> binary().
encode(?JSON_WRAPPER(_) = JObj, Options) ->
    encode(kz_json:to_map(JObj), Options);
encode(Yaml, Options) ->
    ret_result(encode_node(start_state(Options), Yaml, 0)).

%% @equiv decode(Yaml, ['str_node_as_binary', {'map_node_format', 'map'}])
-spec decode(kz_term:ne_binary() | string()) -> decode_object().
decode(Yaml) ->
    decode(Yaml, ['str_node_as_binary', {'map_node_format', 'map'}]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec decode(kz_term:ne_binary() | string(), kz_term:proplist()) -> decode_object().
decode(Yaml, Options) ->
    yamerl:decode(Yaml, Options).

%% @equiv decode_file(Yaml, ['str_node_as_binary', {'map_node_format', 'map'}])
-spec decode_file(kz_term:ne_binary() | string()) -> decode_object().
decode_file(Yaml) ->
    decode_file(Yaml, ['str_node_as_binary', {'map_node_format', 'map'}]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec decode_file(kz_term:ne_binary() | string(), kz_term:proplist()) -> decode_object().
decode_file(Yaml, Options) ->
    yamerl:decode_file(Yaml, Options).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec encode_node(state(), yaml_node(), non_neg_integer()) -> state().
encode_node(State, #{}=Map, Level) ->
    encode_block_mapping(State, Map, Level);
encode_node(State, Atom, Level) when is_atom(Atom) ->
    encode_block_scalar(State, Atom, Level);
encode_node(State, String, Level) when is_binary(String) ->
    encode_block_scalar(State, String, Level);
encode_node(State, Float, Level) when is_float(Float) ->
    encode_block_scalar(State, Float, Level);
encode_node(State, Integer, Level) when is_integer(Integer) ->
    encode_block_scalar(State, Integer, Level);
encode_node(State, Sequence, Level) when is_list(Sequence) ->
    encode_block_sequence(State, Sequence, Level);
encode_node(_State, Other, _Level) ->
    throw({'error', {'invalid_type', Other}}).

-spec encode_block_mapping(state(), node_map(), non_neg_integer()) -> state().
encode_block_mapping(State, Map, Level) when map_size(Map) =:= 0 ->
    State#{result => [indent(State, 0, Level), <<"{}">>, <<$\n>>]
          ,tag    => <<"tag:yaml.org,2002:map">>
          ,explicit => 'false'
          ,empty_collection => 'true'
          };
encode_block_mapping(State, Map, Level) ->
    {_, Result} = maps:fold(fun(K, V, Acc) ->
                                    encode_block_mapping_fold(State, K, V, Acc, Level)
                            end, {0, []}, Map),
    State#{result => lists:reverse(Result)
          ,tag    => <<"tag:yaml.org,2002:map">>
          ,explicit => 'false'
          }.

-spec encode_block_mapping_fold(state(), yaml_node(), yaml_node(), {non_neg_integer(), iolist()}, non_neg_integer()) ->
                                       {non_neg_integer(), iolist()}.
encode_block_mapping_fold(State, Key, Value, {Index, Acc}, Level) ->
    NewState = new_state(State),
    #{explicit := Explicit
     } = KeyState = encode_node(NewState#{is_key => 'true'}, Key, Level + 1),

    ValueState = encode_node(NewState#{compact => not Explicit}, Value, Level + 1),

    {Index + 1
    ,[ [mapping_key(KeyState, Index, Level), mapping_value(ValueState, Level)] | Acc]
    }.

-spec mapping_key(state(), non_neg_integer(), non_neg_integer()) -> iolist().
mapping_key(#{explicit := 'true', result := Result}=State, Index, Level) ->
    Indent = indent(State, Index, Level),
    [Indent, <<"? ">>, Result, <<$\n>>];
mapping_key(#{result := Result}=State, Index, Level) ->
    [indent(State, Index, Level), Result].

-spec mapping_value(state(), non_neg_integer()) -> iolist().
mapping_value(#{tag := Tag, empty_collection := 'true'}, _Level) ->
    [<<": ">>, empty_collection(Tag), <<$\n>>];
mapping_value(#{result := Result, tag := Tag, indent := 1}, _Level)
  when Tag =:= <<"tag:yaml.org,2002:map">>
       orelse Tag =:= <<"tag:yaml.org,2002:seq">> ->
    [<<":">>, <<$\n>>, Result];
mapping_value(#{result := Result, tag := Tag}=State, Level)
  when Tag =:= <<"tag:yaml.org,2002:map">>
       orelse Tag =:= <<"tag:yaml.org,2002:seq">> ->
    [<<":">>, <<$\n>>, indent(State, Level + 1), Result];
mapping_value(#{result := Result}, _Level) ->
    [<<": ">>, Result, <<$\n>>].

-spec encode_block_sequence(state(), node_seq(), non_neg_integer()) -> state().
encode_block_sequence(State, [], Level) ->
    State#{result => [indent(State, 0, Level), <<"[]">>, <<$\n>>]
          ,tag    => <<"tag:yaml.org,2002:seq">>
          ,explicit => 'false'
          ,empty_collection => 'true'
          };
encode_block_sequence(State, Seqs, Level) ->
    {_, Result} = lists:foldl(fun(Seq, Acc) ->
                                      encode_block_seq_fold(State, Seq, Acc, Level)
                              end, {0, []}, Seqs),
    State#{result => lists:reverse(Result)
          ,tag    => <<"tag:yaml.org,2002:seq">>
          ,explicit => 'false'
          }.

-spec encode_block_seq_fold(state(), yaml_node(), {non_neg_integer(), iolist()}, non_neg_integer()) ->
                                   {non_neg_integer(), iolist()}.
encode_block_seq_fold(State, Seq, {Index, Acc}, Level) ->
    ValueState = encode_node(new_state(State), Seq, Level + 1),
    {Index + 1, [ seq_value(ValueState, Index, Level) | Acc]}.

-spec seq_value(state(), non_neg_integer(), non_neg_integer()) -> iolist().
seq_value(#{tag := Tag, empty_collection := 'true'}=State, Index, Level) ->
    [indent(State, Index, Level), <<"- ">>, empty_collection(Tag), <<$\n>>];
seq_value(#{result := Result, compact := 'true', tag := Tag}=State, Index, Level)
  when Tag =:= <<"tag:yaml.org,2002:map">>
       orelse Tag =:= <<"tag:yaml.org,2002:seq">> ->
    [indent(State, Index, Level), align_indent(State), Result];
seq_value(#{result := Result, compact := 'false', tag := Tag}=State, Index, Level)
  when Tag =:= <<"tag:yaml.org,2002:map">>
       orelse Tag =:= <<"tag:yaml.org,2002:seq">> ->
    [indent(State, Index, Level), <<"-">>, <<$\n>>, Result];
seq_value(#{result := Result, indent := 1}=State, Index, Level) ->
    [indent(State, Index, Level), <<"- ">>, Result, <<$\n>>];
seq_value(#{result := Result}=State, Index, Level) ->
    [indent(State, Index, Level), Result, <<$\n>>].

-spec empty_collection(kz_term:ne_binary()) -> kz_term:ne_binary().
empty_collection(<<"tag:yaml.org,2002:map">>) -> <<"{}">>;
empty_collection(<<"tag:yaml.org,2002:seq">>) -> <<"[]">>.

-spec align_indent(state()) -> iolist().
align_indent(#{indent := 1}) ->
    [<<"-">>, <<$\n>>];
align_indent(_) ->
    [<<"- ">>].

-spec indent(state(), non_neg_integer(), non_neg_integer()) -> binary().
indent(#{compact := 'true', indent := 1}=State, Index, Level) when Index =:= 0 ->
    indent(State, Level);
indent(#{compact := 'true'}, Index, _Level) when Index =:= 0 ->
    <<>>;
indent(State, _Index, Level) ->
    indent(State, Level).

-spec indent(state(), non_neg_integer()) -> binary().
indent(#{indent := Indent}, Level) ->
    binary:copy(<<" ">>, Indent * Level).

-spec encode_block_scalar(state(), node_scalar(), non_neg_integer()) -> state().
encode_block_scalar(#{bool_style := Style}=State, Bool, _Level) when is_boolean(Bool) ->
    State#{result => [to_case_style(Bool, Style)]
          ,tag    => <<"tag:yaml.org,2002:bool">>
          ,explicit => 'false'
          };
encode_block_scalar(#{null_style := Style}=State, 'null', _Level) ->
    State#{result => [to_case_style('null', Style)]
          ,tag    => <<"tag:yaml.org,2002:null">>
          ,explicit => 'false'
          };
encode_block_scalar(#{undefined_type := 'null'}=State, 'undefined', Level) ->
    encode_block_scalar(State, 'null', Level);
encode_block_scalar(State, Atom, Level) when is_atom(Atom) ->
    State#{result => encode_string(State, kz_term:to_binary(Atom), Level)
          ,tag    => <<"tag:yaml.org,2002:str">>
          ,explicit => 'false'
          };
encode_block_scalar(State, Binary, Level) when is_binary(Binary) ->
    State#{result => encode_string(State, Binary, Level)
          ,tag    => <<"tag:yaml.org,2002:str">>
          ,explicit => 'false'
          };
encode_block_scalar(State, Float, _Level) when is_float(Float) ->
    State#{result => [kz_term:to_binary(Float)]
          ,tag    => <<"tag:yaml.org,2002:float">>
          ,explicit => 'false'
          };
encode_block_scalar(State, Int, _Level) when is_integer(Int) ->
    State#{result => [kz_term:to_binary(Int)]
          ,tag    => <<"tag:yaml.org,2002:int">>
          ,explicit => 'false'
          }.
-spec to_case_style(boolean(), 'canonical' | case_styles()) -> kz_term:ne_binary().
to_case_style('false', 'camelcase') ->
    <<"False">>;
to_case_style('true', 'camelcase') ->
    <<"True">>;
to_case_style('null', 'camelcase') ->
    <<"Null">>;
to_case_style('null', 'canonical') ->
    <<"~">>;
to_case_style(Scalar, 'lowercase') ->
    kz_term:to_binary(Scalar);
to_case_style(Scalar, 'uppercase') ->
    kz_term:to_upper_binary(kz_term:to_binary(Scalar)).

%% Encoding string scalar.
%%
%% The approach lifted from js-yaml (https://github.com/nodeca/js-yaml).
-spec encode_string(state(), binary(), non_neg_integer()) -> iodata().
encode_string(_State, <<>>, _Level) ->
    [<<"''">>];
encode_string(#{indent := UserIndent
               ,line_width := UserLineWidth
               }=State, String, Level) ->
    %% avoid zero indentation string, e.g. indent string in level-0 too.
    Indent = UserIndent * erlang:max(1, Level),

    %% Decrease width as indentation gets deeper and deeper, but maintain a lower bound 40.
    LineWidth = erlang:max(erlang:min(UserLineWidth, 40), UserLineWidth - Indent),

    encode_string(State, String, Indent, LineWidth, choose_string_style(State, String, LineWidth)).

-spec encode_string(state(), binary(), non_neg_integer(), non_neg_integer(), string_style()) -> iodata().
encode_string(_State, String, _Indent, _LineWidth, 'plain') ->
    [String];
encode_string(_State, String, _Indent, _LineWidth, 'single_quote') ->
    [<<$'>>, binary:replace(String, <<"'">>, <<"''">>, [global]), <<$'>>];
encode_string(_State, String, _Indent, _LineWidth, 'double_qoute') ->
    [<<$">>, escape_string(String, <<>>), <<$">>];
encode_string(#{indent := StateIndent}, String, Indent, _LineWidth, 'literal') ->
    [<<$|>>, block_header(String, StateIndent), drop_ending_new_line(indent_string(String, Indent))];
encode_string(#{indent := StateIndent}, String, Indent, LineWidth, 'fold') ->
    [<<$>>>, block_header(String, StateIndent), drop_ending_new_line(indent_string(fold_string(String, LineWidth), Indent))].

%% @doc Escape non-printable characters for double quote stryle.
-spec escape_string(binary(), binary()) -> binary().
escape_string(<<>>, Result) ->
    Result;
%% UTF-16 surrogate
escape_string(<<Char/utf8, Rest/binary>>, Result)
  when Char >= 16#010000, Char =< 16#10FFFF ->
    ?DEV_LOG("we got ourself a surrogate ~p (~p)", [Char, unicode:characters_to_binary(<<Char/utf8>>)]),
    escape_string(Rest, <<Result/binary, (encode_hex(Char))/binary>>);
escape_string(<<Char, Rest/binary>>, Result) ->
    Escaped = escape_sequences(Char),
    case escape_sequences(Char) of
        <<_/binary>> ->
            escape_string(Rest, <<Result/binary, Escaped/binary>>);
        Char ->
            case is_printable(Char) of
                'true' ->
                    escape_string(Rest, <<Result/binary, (unicode:characters_to_binary(<<Char/utf8>>))/binary>>);
                'false' ->
                    escape_string(Rest, <<Result/binary, (encode_hex(Char))/binary>>)
            end
    end.

%% @doc Add block indicator and chomp if necessary.
-spec block_header(binary(), non_neg_integer()) -> iodata().
block_header(String, Indent) ->
    IndentIndicator = case re:run(String, <<"^">>) =:= 'nomatch' of
                          'true' -> kz_term:to_binary(Indent);
                          'false' -> <<>>
                      end,
    {Clip, PreceededLF} =
        case kz_binary:reverse(String) of
            <<$\n, $\n, _/binary>> -> {'true', 'true'};
            <<$\n, _/binary>> -> {'true', 'false'};
            _ -> {'false', 'false'}
        end,
    Keep = Clip
        andalso (PreceededLF
                 orelse String =:= <<$\n>>
                ),
    Chomp = case Keep of
                'true' -> <<"+">>;
                'false' when Clip -> <<>>;
                'false' -> <<"-">>
            end,
    [IndentIndicator, Chomp, <<$\n>>].

%% @doc Indent every lines in the string, except empty lines.
-spec indent_string(binary(), non_neg_integer()) -> iolist().
indent_string(String, Indent) ->
    Indented = indent(#{indent => Indent}, 1),
    [begin
         case Line =/= <<>>
             andalso Line =/= <<$\n>>
         of
             'true' -> [Indented, Line, <<$\n>>];
             'false' -> [Line, <<$\n>>]
         end
     end
     || Line <- binary:split(String, <<$\n>>, [global])
    ].

-spec drop_ending_new_line(iolist()) -> binary().
drop_ending_new_line([]) -> <<>>;
drop_ending_new_line(Lines) ->
    LastLine = lists:last(Lines),
    Rest = lists:droplast(Lines),
    case LastLine =/= <<>>
        andalso lists:last(LastLine)
    of
        'false' -> kz_term:to_binary(Lines);
        <<$\n>> -> kz_term:to_binary(Rest ++ lists:droplast(Lines));
        _ -> kz_term:to_binary(Lines)
    end.

-spec fold_string(binary(), non_neg_integer()) -> binary().
fold_string(String, Width) ->
    [FirstLine | Matches] = re:split(String, <<"(\n+)([^\n]*)">>, []),
    PrevMoreIndented = FirstLine =/= <<>>
        andalso (binary:first(FirstLine) =/= <<$\n>>
                     orelse binary:first(FirstLine) =/= <<" ">>
                ),
    fold_string_fold(Matches, Width, PrevMoreIndented, fold_line(FirstLine, Width)).

fold_string_fold([], _, _, Acc) -> Acc;
fold_string_fold([StringLFs, Line, <<>> | Lines], Width, PrevMoreIndented, Acc) ->
    MoreIndented = Line =/= <<>>
        andalso binary:first(Line) =/= <<" ">>,
    YamlLF = case not PrevMoreIndented
                 andalso not MoreIndented
                 andalso Line =/= <<>>
             of
                 'true' -> <<$\n>>;
                 'false' -> <<>>
             end,
    Fold = fold_line(Line, Width),
    % ?DEV_LOG("~nAcc ~p~n~nFold ~p~n~n", [Acc, Fold]),
    Result = <<Acc/binary, StringLFs/binary, YamlLF/binary, Fold/binary>>,
    fold_string_fold(Lines, Width, PrevMoreIndented, Result).

fold_line(<<>>, _) -> <<>>;
fold_line(<<" ", _/binary>> = Line, _) -> Line;
fold_line(Line, Width) ->
    Matches = re:run(Line, <<" [^ ]">>, [global]),
    {Start, Curr, Acc} = fold_line_fold(Line, Width, Matches, 0, 0, <<>>),
    Result0 = <<Acc/binary, $\n>>,
    case byte_size(Line) - Start > Width
        andalso Curr > Start
    of
        'true' ->
            %% Insert a break if the remainder is too long and there is a break available. Also drop extra \n joiner.
            % ?DEV_LOG("~nR0 ~p~n Start~p Curr ~p LSize ~p", [Result0, Start, Curr, byte_size(Line)]),
            LineA = binary:part(Line, Start, Curr),
            LineB = binary:part(Line, Curr + 1, byte_size(Line) - (Curr + 1)),
            <<_/utf8, Rest/binary>> = <<Result0/binary, LineA/binary, $\n, LineB/binary>>,
            % ?DEV_LOG("~nRest ~p~n", [Rest]),
            Rest;
        'false' ->
            <<_/utf8, _:Start/binary, Result1/binary>> = Result0,
            %% drop extra \n joiner
            ?DEV_LOG("~nResult1 ~p~n", [Result1]),
            Result1
    end.

fold_line_fold(_, _, 'nomatch', _, _, Acc) ->
    Acc;
fold_line_fold(Line, Width, {'match', Matches}, Start, Curr, Acc) ->
    % io:format("~n~p~n~n", [Matches]),
    fold_line_fold(Line, Width, Matches, Start, Curr, Acc);
fold_line_fold(_, _, [], Start, Curr, Acc) ->
    % io:format("sorry~n"),
    {Start, Curr, Acc};
fold_line_fold(Line, Width, [[{Next, _}] | Matches], Start, Curr, Acc)
  when Next - Start > Width ->
    % io:format("yup~n"),
    End = case Curr > Start of
              'true' -> Curr;
              'false' -> Next
          end,
    Result = <<Acc/binary, $\n, (binary:part(Line, Start, End))/binary>>,
    fold_line_fold(Line, Width, Matches, End + 1, Curr, Result);
fold_line_fold(Line, Width, [[{Next, _}] | Matches], Start, _, Acc) ->
    % io:format("dool Next ~p W ~p M ~p~n", [Next, Width, Next - Start]),
    fold_line_fold(Line, Width, Matches, Start, Next, Acc).

%% @doc Encode the characters to its Unicode code point and escape it.
-spec encode_hex(integer()) -> kz_term:ne_binary().
encode_hex(Char) when Char =< 16#FF ->
    <<"\\x", (kz_term:to_upper_binary(kz_term:to_hex_binary(<<Char/utf8>>)))/binary>>;
encode_hex(Char) when Char =< 16#FFFF ->
    <<"\\u", (kz_term:to_upper_binary(kz_term:to_hex_binary(<<Char/utf16>>)))/binary>>;
encode_hex(Char) when Char =< 16#FFFFFFFF ->
    <<"\\U", (kz_term:to_upper_binary(kz_term:to_hex_binary(<<Char/utf32>>)))/binary>>.

%% Escape the escapable characters for double quote style.
-spec escape_sequences(integer()) -> binary() | integer().
escape_sequences(16#00)   -> <<"\\0">>;
escape_sequences(16#07)   -> <<"\\a">>;
escape_sequences(16#08)   -> <<"\\b">>;
escape_sequences(16#09)   -> <<"\\t">>;
escape_sequences(16#0A)   -> <<"\\n">>;
escape_sequences(16#0B)   -> <<"\\v">>;
escape_sequences(16#0C)   -> <<"\\f">>;
escape_sequences(16#0D)   -> <<"\\r">>;
escape_sequences(16#1B)   -> <<"\\e">>;
escape_sequences(16#22)   -> <<"\\\"">>;
escape_sequences(16#5C)   -> <<"\\\\">>;
escape_sequences(16#85)   -> <<"\\N">>;
escape_sequences(16#A0)   -> <<"\\_">>;
escape_sequences(16#2028) -> <<"\\L">>;
escape_sequences(16#2029) -> <<"\\P">>;
escape_sequences(Char) -> Char.

-spec choose_string_style(map(), binary(), non_neg_integer()) -> string_style().
choose_string_style(#{line_width := UserLineWidth
                     ,indent := _UserIndent
                     ,is_key := IsKey
                     ,string_style := StringStyle
                     }, String, LineWidth) ->
    ShouldTrackWidth = UserLineWidth =/= -1,
    Plain = is_plain_safe_first(binary:first(String))
        andalso not is_whitespace(binary:last(String)),
    AnalyzeState = analyze_state(ShouldTrackWidth, Plain, IsKey, LineWidth),
    #{chosen_one := ChosenOne} = analyze_string(String, 0, AnalyzeState),
    choose_string_style(ChosenOne, StringStyle).

-spec analyze_state(boolean(), boolean(), boolean(), non_neg_integer()) -> map().
analyze_state(ShouldTrackWidth, Plain, IsKey, LineWidth) ->
    #{should_track_width => ShouldTrackWidth
     ,plain_safe => Plain
     ,is_key => IsKey
     ,line_width => LineWidth
     ,has_foldable_line => 'false'
     ,has_break => 'false'
     ,previous_break => -1
     ,previous_break_space => -1
     }.

-spec choose_string_style(string_style(), string_style()) -> string_style().
choose_string_style('double_qoute', _) -> 'double_qoute';
choose_string_style('literal', Style) when Style =:= 'literal'
                                           andalso Style =:= 'fold' ->
    Style;
choose_string_style('literal', _) -> 'literal';
choose_string_style('fold', Style) when Style =:= 'literal'
                                        andalso Style =:= 'fold' ->
    Style;
choose_string_style('fold', _) -> 'fold';
choose_string_style('single_quote', Style) when Style =:= 'plain'
                                                andalso Style =:= 'single_quote'
                                                andalso Style =:= 'double_qoute' ->
    Style;
choose_string_style('single_quote', _) -> 'single_quote';
choose_string_style('plain', Style) when Style =:= 'plain'
                                         andalso Style =:= 'single_quote'
                                         andalso Style =:= 'double_qoute' ->
    Style;
choose_string_style('plain', _) -> 'plain'.

-spec analyze_string(binary(), non_neg_integer(), map()) -> map().
analyze_string(<<>>, _, #{is_key := 'true', plain_safe := 'true'}=Analyze) ->
    Analyze#{chosen_one => 'plain'};
analyze_string(<<>>, _, #{is_key := 'true', plain_safe := 'false'}=Analyze) ->
    Analyze#{chosen_one => 'single_quote'};
analyze_string(<<Char, Rest/binary>>, Index, #{is_key := 'true', plain_safe := Plain}=Analyze) ->
    case is_printable(Char) of
        'false' -> Analyze#{chosen_one => 'double_qoute'};
        'true' ->
            analyze_string(Rest, Index + 1, Analyze#{plain_safe => Plain
                                                     andalso is_plain_safe(Char)})
    end;
analyze_string(<<>>, Index, #{has_break := HasBreak, plain_safe := Plain}=Analyze) ->
    HasFoldable = has_foldable_line(Index, Analyze),
    case HasBreak
        andalso HasFoldable
    of
        'false' when Plain      ->  Analyze#{chosen_one => 'plain'};
        'false'                 ->  Analyze#{chosen_one => 'single_quote'};
        'true' when HasFoldable ->  Analyze#{chosen_one => 'fold'};
        'true'                  ->  Analyze#{chosen_one => 'literal'}
    end;
analyze_string(<<$\n, Rest/binary>>, Index, Analyze) ->
    analyze_string(Rest, Index + 1
                  ,Analyze#{has_break => 'true'
                           ,has_foldable_line => has_foldable_line(Index, Analyze)
                           ,previous_break => Index
                           ,previous_break_space => is_break_space(Rest)
                           }
                  );
analyze_string(<<Char, Rest/binary>>, Index, #{plain_safe := Plain}=Analyze) ->
    case is_printable(Char) of
        'false' -> Analyze#{chosen_one => 'double_qoute'};
        'true' ->
            analyze_string(Rest, Index + 1, Analyze#{plain_safe => Plain
                                                     andalso is_plain_safe(Char)})
    end.

-spec has_foldable_line(non_neg_integer(), map()) -> boolean().
has_foldable_line(_, #{should_track_width := 'false'}) ->
    'false';
has_foldable_line(_, #{has_foldable_line := 'true'}) ->
    'true';
has_foldable_line(Index, #{previous_break := Break
                          ,previous_break_space := BreakSpace
                          ,line_width := Linewidth
                          }) ->
    Index - Break - 1 > Linewidth
        andalso BreakSpace =:= -1.

-spec is_break_space(binary()) -> 1 | -1.
is_break_space(<<" ", _/binary>>) -> 1;
is_break_space(_) -> -1.

-define(CHAR_TAB                  , 16#09). %% Tab
-define(CHAR_LINE_FEED            , 16#0A). %% LF
-define(CHAR_SPACE                , 16#20). %% Space
-define(CHAR_EXCLAMATION          , 16#21). %% !
-define(CHAR_DOUBLE_QUOTE         , 16#22). %% "
-define(CHAR_SHARP                , 16#23). %% #
-define(CHAR_PERCENT              , 16#25). %% %
-define(CHAR_AMPERSAND            , 16#26). %% &
-define(CHAR_SINGLE_QUOTE         , 16#27). %% '
-define(CHAR_ASTERISK             , 16#2A). %% *
-define(CHAR_COMMA                , 16#2C). %% ,
-define(CHAR_MINUS                , 16#2D). %% -
-define(CHAR_COLON                , 16#3A). %% :
-define(CHAR_GREATER_THAN         , 16#3E). %% >
-define(CHAR_QUESTION             , 16#3F). %% ?
-define(CHAR_COMMERCIAL_AT        , 16#40). %% @
-define(CHAR_LEFT_SQUARE_BRACKET  , 16#5B). %% [
-define(CHAR_RIGHT_SQUARE_BRACKET , 16#5D). %% ]
-define(CHAR_GRAVE_ACCENT         , 16#60). %% `
-define(CHAR_LEFT_CURLY_BRACKET   , 16#7B). %% {
-define(CHAR_VERTICAL_LINE        , 16#7C). %% |
-define(CHAR_RIGHT_CURLY_BRACKET  , 16#7D). %% }

%% Check if the character is allowed as the first character in plain style.
%% This is a copy pasta from js-yaml :)
-spec is_plain_safe_first(integer()) -> boolean().
is_plain_safe_first(Char) ->
    %% Uses a subset of ns-char - c-indicator
    %% where ns-char = nb-char - s-white.
    is_printable(Char)
        andalso Char =/= 16#FEFF %% BOM
        andalso not is_whitespace(Char)
    %% - (c-indicator ::=
    %% “-” | “?” | “:” | “,” | “[” | “]” | “{” | “}”
        andalso Char =/= ?CHAR_MINUS
        andalso Char =/= ?CHAR_QUESTION
        andalso Char =/= ?CHAR_COLON
        andalso Char =/= ?CHAR_COMMA
        andalso Char =/= ?CHAR_LEFT_SQUARE_BRACKET
        andalso Char =/= ?CHAR_RIGHT_SQUARE_BRACKET
        andalso Char =/= ?CHAR_LEFT_CURLY_BRACKET
        andalso Char =/= ?CHAR_RIGHT_CURLY_BRACKET
    %% | “#” | “&” | “*” | “!” | “|” | “>” | “'” | “"”
        andalso Char =/= ?CHAR_SHARP
        andalso Char =/= ?CHAR_AMPERSAND
        andalso Char =/= ?CHAR_ASTERISK
        andalso Char =/= ?CHAR_EXCLAMATION
        andalso Char =/= ?CHAR_VERTICAL_LINE
        andalso Char =/= ?CHAR_GREATER_THAN
        andalso Char =/= ?CHAR_SINGLE_QUOTE
        andalso Char =/= ?CHAR_DOUBLE_QUOTE
    %% | “%” | “@” | “`”)
        andalso Char =/= ?CHAR_PERCENT
        andalso Char =/= ?CHAR_COMMERCIAL_AT
        andalso Char =/= ?CHAR_GRAVE_ACCENT.

%% Check if character is allowed after the first character in plain style.
%% This is a copy pasta from js-yaml :)
-spec is_plain_safe(integer()) -> boolean().
is_plain_safe(Char) ->
    %% Uses a subset of nb-char - c-flow-indicator - ":" - "#"
    %% where nb-char ::= c-printable - b-char - c-byte-order-mark.
    is_printable(Char)
        andalso Char =/= 16#FEFF %% BOM
    %% - c-flow-indicator
        andalso Char =/= ?CHAR_COMMA
        andalso Char =/= ?CHAR_LEFT_SQUARE_BRACKET
        andalso Char =/= ?CHAR_RIGHT_SQUARE_BRACKET
        andalso Char =/= ?CHAR_LEFT_CURLY_BRACKET
        andalso Char =/= ?CHAR_RIGHT_CURLY_BRACKET
    %% - ":" - "#"
        andalso Char =/= ?CHAR_COLON
        andalso Char =/= ?CHAR_SHARP.

%% Returns true if the character can be printed without escaping.
%% From YAML 1.2: "any allowed characters known to be non-printable
%% should also be escaped. [However,] This isn’t mandatory" <- LOL
%% Derived from nb-char - \t - #x85 - #xA0 - #x2028 - #x2029.
%%
%% This is a copy pasta from js-yaml :)
-spec is_printable(integer()) -> boolean().
is_printable(Char) ->
    (16#00020 =< Char
     andalso Char =< 16#00007E
    )
        orelse ((16#000A1 =< Char
                 andalso Char =< 16#00D7FF
                )
                andalso Char =/= 16#2028
                andalso Char =/= 16#2029
               )
        orelse ((16#0E000 =< Char
                 andalso Char =< 16#00FFFD
                )
                andalso Char =/= 16#FEFF %% BOM
               )
        orelse (16#10000 =< Char
                andalso Char =< 16#10FFFF
               ).

%% [33] s-white ::= s-space | s-tab
%% This a copy pasta from js-yaml :)
-spec is_whitespace(integer()) -> boolean().
is_whitespace(Char) ->
    Char =:= ?CHAR_SPACE
        orelse Char =:= ?CHAR_TAB.

-spec start_state(options()) -> state().
start_state(#{}=Options) ->
    {Indent, Compact} =
        case {erlang:max(1, maps:get('indent', Options, 2))
             ,maps:get('compact', Options, 'true')
             }
        of
            {I, _} when I > 2 ->
                {I, 'false'};
            {I, C} ->
                {I, C}
        end,
    #{compact        => Compact
     ,indent         => Indent
     ,string_style   => maps:get('string_style', Options, literal)
     ,line_width     => maps:get('line_width', Options, 80)
     ,bool_style     => maps:get('bool_style', Options, 'lowercase')
     ,null_style     => maps:get('null_style', Options, 'lowercase')
     ,undefined_type => maps:get('undefined_type', Options, 'undefined')

     ,result  => <<>>
     ,tag     => 'undefined'
     ,explicit => 'false'
     ,is_key => 'false'
     }.

-spec new_state(state()) -> state().
new_state(#{}=Map) ->
    Map#{result  => <<>>
        ,tag     => 'undefined'
        ,explicit => 'false'
        ,is_key => 'false'
        }.

-spec ret_result(state()) -> binary().
ret_result(#{result := Result}) -> erlang:iolist_to_binary(Result).
