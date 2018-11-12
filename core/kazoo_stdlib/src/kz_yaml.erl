%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_yaml).

-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([encode/1, encode/2]).

-define(DEFAULT_OPTS, #{indent => 2
                       ,multiline => false
                       ,null_undefined => false
                       ,quote_char => <<>>

                       %% other options
                       %% float_precision => integer()
                       }
       ).


-define(ESCAPABLE, [<<$:>>, <<${>>, <<$}>>, <<$[>>, <<$]>>, <<$,>>, <<$&>>, <<$*>>
                   ,<<$#>>, <<$?>>, <<$|>>, <<$->>, <<$<>>, <<$>>>, <<$=>>, <<$!>>
                   ,<<$%>>, <<$@>>, <<$`>>, <<$^>>, <<$\0>>, <<$\x01>>, <<$\x02>>
                   ,<<$\x03>>, <<$\x04>>, <<$\x05>>, <<$\x06>>, <<$\a>>, <<$\b>>
                   ,<<$\t>>, <<$\n>>, <<$\v>>, <<$\f>>, <<$\r>>, <<$\x0e>>, <<$\x0f>>
                   ,<<$\x10>>, <<$\x11>>, <<$\x12>>, <<$\x13>>, <<$\x14>>, <<$\x15>>
                   ,<<$\x16>>, <<$\x17>>, <<$\x18>>, <<$\x19>>, <<$\x1a>>, <<$\e>>
                   ,<<$\x1c>>, <<$\x1d>>, <<$\x1e>>, <<$\x1f>>, <<$\N>>
                   ,<<$\_>>, <<$\L>>, <<$\P>>
                   ]).

-spec encode(kz_term:proplist() | kz_term:proplists() | kz_json:object() | kz_json:objects()) -> binary().
encode(PropsObjs) ->
    encode(PropsObjs, #{}).

-spec encode(kz_term:proplist() | kz_term:proplists() | kz_json:object() | kz_json:objects(), map()) -> binary().
encode([], _) -> <<>>;
encode([{_, _}|_]=Props, Options) ->
    iolist_to_binary(encode_document(Props, 0, <<>>, options(Options)));
encode([[{_, _}|_]|_]=PropsLists, Options) ->
    iolist_to_binary([[<<"---\n">>, encode_document(Props, 0, <<>>, options(Options))] || Props <- PropsLists]);
encode(?JSON_WRAPPER(_) = JObj, Options) ->
    encode(kz_json:from_list_recursive(JObj), Options);
encode([?JSON_WRAPPER(_)|_]=JObjs, Options) ->
    encode([kz_json:from_list_recursive(JObj) || JObj <- JObjs], Options).

options(#{}=Options) ->
    maps:merge(?DEFAULT_OPTS, Options).

encode_document(Doc, Indent, BlockEntry, Options) ->
    [encode_scalar(KV, Indent, BlockEntry, Options) || KV <- Doc].

%% undefined and null
encode_scalar({Key, 'undefined'}, Indent, BlockEntry, #{null_undefined := 'true'}=Options) ->
    [indent(Indent, Options), BlockEntry, quote_wrap(Key, Options), <<": null\n">>];
encode_scalar({Key, 'undefined'}, Indent, BlockEntry, #{null_undefined := 'false'}=Options) ->
    [indent(Indent, Options), BlockEntry, quote_wrap(Key, Options), <<":\n">>];
encode_scalar({Key, 'null'}, Indent, BlockEntry, Options) ->
    [indent(Indent, Options), BlockEntry, quote_wrap(Key, Options), <<": null\n">>];

%% scalars in map
encode_scalar({Key, Atom}, Indent, BlockEntry, Options) when is_atom(Atom) ->
    [indent(Indent, Options), BlockEntry, quote_wrap(Key, Options), <<": ">>, kz_term:to_binary(Atom), <<$\n>>];
encode_scalar({Key, String}, Indent, BlockEntry, Options) when is_binary(String) ->
    [indent(Indent, Options), BlockEntry, quote_wrap(Key, Options), <<": ">>, string_wrap(String, Indent, Options), <<$\n>>];
encode_scalar({Key, Integer}, Indent, BlockEntry, Options) when is_integer(Integer) ->
    [indent(Indent, Options), BlockEntry, quote_wrap(Key, Options), <<": ">>, kz_term:to_binary(Integer), <<$\n>>];
encode_scalar({Key, Float}, Indent, BlockEntry, Options) when is_float(Float) ->
    [indent(Indent, Options), BlockEntry, quote_wrap(Key, Options), <<": ">>, to_float(Float, Options), <<$\n>>];

%% collections
encode_scalar({Key, []}, Indent, BlockEntry, Options) ->
    %% handle empty list here to catch empty string ""
    [indent(Indent, Options), BlockEntry, quote_wrap(Key, Options), <<":">>, <<$\n>>];
encode_scalar({Key, [{_,_}|_]=KVs}, Indent, BlockEntry, Options) ->
    [indent(Indent, Options), BlockEntry, quote_wrap(Key, Options), <<":\n">>
    ,[encode_scalar(KV, Indent+1, <<>>, Options) || KV <- KVs]
    ];
encode_scalar({Key, Sequence}, Indent, BlockEntry, Options) when is_list(Sequence) ->
    case io_lib:char_list(Sequence) of
        'true' ->
            throw({'error', 'do.not.use.char.list'});
        'false' ->
            [indent(Indent, Options), BlockEntry, quote_wrap(Key, Options), <<":\n">>, encode_sequence(Sequence, Indent+1, <<"- ">>, Options)]
    end.

encode_sequence(Sequence, Indent, BlockEntry, Options) ->
    [encode_sequence_item(Item, Indent, BlockEntry, Options) || Item <- Sequence].

encode_sequence_item(Scalar, Indent, BlockEntry, Options) when is_atom(Scalar) ->
    [indent(Indent, Options), BlockEntry, kz_term:to_binary(Scalar), <<$\n>>];
encode_sequence_item(Scalar, Indent, BlockEntry, Options) when is_binary(Scalar) ->
    [indent(Indent, Options), BlockEntry, string_wrap(Scalar, Indent, Options), <<$\n>>];
encode_sequence_item(Scalar, Indent, BlockEntry, Options) when is_integer(Scalar) ->
    [indent(Indent, Options), BlockEntry, kz_term:to_binary(Scalar), <<$\n>>];
encode_sequence_item(Scalar, Indent, BlockEntry, Options) when is_float(Scalar) ->
    [indent(Indent, Options), BlockEntry, to_float(Scalar, Options), <<$\n>>];
encode_sequence_item({Key, Value}, Indent, BlockEntry, Options) ->
    encode_scalar({Key, Value}, Indent, BlockEntry, Options);
encode_sequence_item([{_,_}|_]=Sequence, Indent, BlockEntry, Options) ->
    [encode_scalar(KV, Indent, BlockEntry, Options) || KV <- Sequence];
encode_sequence_item([], Indent, BlockEntry, Options) ->
    %% handle empty list here to catch empty string ""
    [indent(Indent, Options), BlockEntry, <<$\n>>].

to_float(Float, #{float_precision := Prescision}) ->
    kz_term:to_binary(io_lib:format("~.*f", [Prescision, Float]));
to_float(Float, _) ->
    kz_term:to_binary(io_lib:format("~f", [Float])).

indent(Indent, #{indent := Multiply}) when Multiply > 0 ->
    binary:copy(<<" ">>, Indent * Multiply).

string_wrap(String, Indent, #{multiline := true}=Options) ->
    [<<$>>>, <<$\n>>,
     [[indent(Indent+1, Options), Text]
      || Text <- binary:split(String, <<$\n>>, [global])
     ]
    ];
string_wrap(String, _, Options) ->
    quote_wrap(String, Options).

-spec quote_wrap(binary(), map()) -> kz_term:ne_binary().
quote_wrap(Text0, Options) ->
    {QuoteChar, Text1} = quote_char(Text0, Options),
    <<QuoteChar/binary, Text1/binary, QuoteChar/binary>>.

quote_char(Text, #{quote_char := <<"'">> = Char}) ->
    {Char, binary:replace(Text, <<"'">>, <<"''">>, [global])};
quote_char(Text, #{quote_char := <<>>}=Options) ->
    case binary:match(Text, ?ESCAPABLE) of
        'nomatch' -> {<<>>, Text};
        _ -> quote_char(Text, Options#{quote_char => <<$">>})
    end;
quote_char(Text, #{quote_char := <<$">>}) ->
    {<<$">>, replace(Text, [<<$">>, <<$\\>> | ?ESCAPABLE])}.

replace(<<A, B/binary>>, Escapable) ->
    case lists:member(A, Escapable) of
        'true' -> <<$\\, A/binary, (replace(B, Escapable))/binary>>;
        'false' ->
            io:format("~n A ~p Re ~p~n", [A, replace(B, Escapable)]),
            <<(kz_term:to_binary(A))/binary, (replace(B, Escapable))/binary>>
    end;
replace(<<>>, _) -> <<>>.
