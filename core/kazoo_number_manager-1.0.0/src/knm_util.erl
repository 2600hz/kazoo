%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(knm_util).

-export([get_all_number_dbs/0]).

-export([pretty_print/1, pretty_print/2
         ,fixture/1
        ]).

-include("knm.hrl").

-spec get_all_number_dbs() -> ne_binaries().
get_all_number_dbs() ->
    {'ok', Dbs} = couch_mgr:admin_all_docs(<<"dbs">>
                                               ,[{'startkey', ?KNM_DB_PREFIX}
                                                 ,{'endkey', <<?KNM_DB_PREFIX_L, "\ufff0">>}
                                                ]),
    [cow_qs:urlencode(wh_doc:id(View))
     || View <- Dbs
    ].

-spec pretty_print(ne_binary()) -> ne_binary().
pretty_print(Number) ->
    case pretty_print_format(Number) of
        'undefined' -> Number;
        Format ->
            pretty_print(Format, Number)
    end.

-spec pretty_print(ne_binary(), ne_binary()) -> ne_binary().
pretty_print(Format, Number) ->
    Num = knm_converters:normalize(Number),
    pretty_print(Format, Num, <<>>).

-spec pretty_print(binary(), binary(), binary()) -> binary().
pretty_print(<<>>, _, Acc) -> Acc;
pretty_print(<<"\\S", Format/binary>>, Number, Acc) ->
    pretty_print(Format, Number, <<Acc/binary, "S">>);
pretty_print(<<"\\#", Format/binary>>, Number, Acc) ->
    pretty_print(Format, Number, <<Acc/binary, "#">>);
pretty_print(<<"\\*", Format/binary>>, Number, Acc) ->
    pretty_print(Format, Number, <<Acc/binary, "*">>);
pretty_print(<<"S", Format/binary>>, <<>>, Acc) ->
    pretty_print(Format, <<>>, Acc);
pretty_print(<<"#", Format/binary>>, <<>>, Acc) ->
    pretty_print(Format, <<>>, Acc);
pretty_print(<<"*", Format/binary>>, <<>>, Acc) ->
    pretty_print(Format, <<>>, Acc);
pretty_print(<<"S", Format/binary>>, Number, Acc) ->
    pretty_print(Format, binary_tail(Number), Acc);
pretty_print(<<"#", Format/binary>>, Number, Acc) ->
    pretty_print(Format
                 ,binary_tail(Number)
                 ,<<Acc/binary, (binary_head(Number))/binary>>
                );
pretty_print(<<"*", Format/binary>>, Number, Acc) ->
    pretty_print(Format, <<>>, <<Acc/binary, Number/binary>>);
pretty_print(<<Char, Format/binary>>, Number, Acc) ->
    pretty_print(Format, Number, <<Acc/binary, Char/integer>>).

-spec binary_tail(ne_binary()) -> ne_binary().
binary_tail(Binary) ->
    binary:part(Binary, 1, byte_size(Binary) - 1).

-spec binary_head(ne_binary()) -> ne_binary().
binary_head(Binary) ->
    binary:part(Binary, 0, 1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec pretty_print_format(ne_binary()) -> api_binary().
pretty_print_format(Number) ->
    Classifiers = knm_converters:available_classifiers(),
    Num = knm_converters:normalize(Number),
    pretty_print_format(Num, wh_json:to_proplist(Classifiers)).

-spec pretty_print_format(ne_binary(), wh_proplist()) -> api_binary().
pretty_print_format(Num, []) ->
    lager:debug("unable to get pretty print format for number ~s", [Num]),
    maybe_use_us_default(Num);
pretty_print_format(Num, [{Classification, Classifier}|Classifiers]) ->
    case re:run(Num, get_classifier_regex(Classifier)) of
        'nomatch' -> pretty_print_format(Num, Classifiers);
        _ when is_binary(Classifier) ->
            lager:debug("number '~s' is classified as ~s but no pretty print format available", [Num, Classification]),
            maybe_use_us_default(Num);
        _ ->
            case wh_json:get_value(<<"pretty_print">>, Classifier) of
                'undefined' -> maybe_use_us_default(Num);
                Format -> Format
            end
    end.

-spec maybe_use_us_default(ne_binary()) -> api_binary().
maybe_use_us_default(<<"+1", _/binary>>) ->
    lager:debug("using US number default pretty print", []),
    <<"SS(###) ### - ####">>;
maybe_use_us_default(_) ->
    'undefined'.

-spec get_classifier_regex(ne_binary() | wh_json:object()) ->
                                  ne_binary().
get_classifier_regex(Classifier) when is_binary(Classifier) ->
    Classifier;
get_classifier_regex(JObj) ->
    wh_json:get_value(<<"regex">>, JObj, <<"^$">>).

-spec fixture(file:filename()) -> binary().
fixture(Filename) ->
    Priv = code:priv_dir('kazoo_number_manager'),
    Fixture = filename:join([Priv, "fixtures", Filename]),
    read_fixture(file:read_file(Fixture), Fixture).

read_fixture({'ok', Contents}, _F) ->
    wh_util:to_list(Contents);
read_fixture({'error', 'enoent'}, F) ->
    throw({'error', 'missing_fixture', F}).
