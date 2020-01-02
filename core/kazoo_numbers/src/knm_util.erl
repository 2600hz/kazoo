%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_util).

-export([get_all_number_dbs/0]).

-export([pretty_print/1, pretty_print/2
        ,fixture/1
        ,prefix_for_country/1
        ]).

-include("knm.hrl").

-type country_iso3166a2() :: <<_:(8*2)>>.
-export_type([country_iso3166a2/0]).

-spec get_all_number_dbs() -> kz_term:ne_binaries().
get_all_number_dbs() ->
    ViewOptions = [{'startkey', <<?KNM_DB_PREFIX>>}
                  ,{'endkey', <<?KNM_DB_PREFIX "\ufff0">>}
                  ],
    {'ok', Dbs} = kz_datamgr:db_list(ViewOptions),
    [kz_http_util:urlencode(Db) || Db <- Dbs].

-spec pretty_print(kz_term:api_binary()) -> kz_term:ne_binary().
pretty_print('undefined') -> <<"unknown">>;
pretty_print(Number) ->
    case pretty_print_format(Number) of
        'undefined' -> Number;
        Format ->
            pretty_print(Format, Number)
    end.

-spec pretty_print(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
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

-spec binary_tail(kz_term:ne_binary()) -> kz_term:ne_binary().
binary_tail(Binary) ->
    binary:part(Binary, 1, byte_size(Binary) - 1).

-spec binary_head(kz_term:ne_binary()) -> kz_term:ne_binary().
binary_head(Binary) ->
    binary:part(Binary, 0, 1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pretty_print_format(kz_term:ne_binary()) -> kz_term:api_binary().
pretty_print_format(Number) ->
    Classifiers = knm_converters:available_classifiers(),
    Num = knm_converters:normalize(Number),
    pretty_print_format(Num, kz_json:to_proplist(Classifiers)).

-spec pretty_print_format(kz_term:ne_binary(), kz_term:proplist()) -> kz_term:api_binary().
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
            case kz_json:get_value(<<"pretty_print">>, Classifier) of
                'undefined' -> maybe_use_us_default(Num);
                Format -> Format
            end
    end.

-spec maybe_use_us_default(kz_term:ne_binary()) -> kz_term:api_binary().
maybe_use_us_default(<<"+1", _/binary>>) ->
    lager:debug("using US number default pretty print", []),
    <<"SS(###) ### - ####">>;
maybe_use_us_default(_) ->
    'undefined'.

-spec get_classifier_regex(kz_term:ne_binary() | kz_json:object()) ->
          kz_term:ne_binary().
get_classifier_regex(Classifier) when is_binary(Classifier) ->
    Classifier;
get_classifier_regex(JObj) ->
    kz_json:get_value(<<"regex">>, JObj, <<"^$">>).

-spec fixture(file:filename()) -> string().
fixture(Filename) ->
    Fixture = filename:join([code:priv_dir(?APP), "fixtures", Filename]),
    lager:debug("loading fixture ~s", [Fixture]),
    read_fixture(file:read_file(Fixture), Fixture).

read_fixture({'ok', Contents}, _F) ->
    kz_term:to_list(Contents);
read_fixture({'error', 'enoent'}, F) ->
    throw({'error', 'missing_fixture', F}).

%% TODO
%% This should be replaced with a call to elibphonenumber
%% when/if we integrate that lib or do it ourselves
-spec prefix_for_country(country_iso3166a2()) -> kz_term:ne_binary().
prefix_for_country(Country) ->
    knm_iso3166a2_itu:to_itu(kz_term:to_upper_binary(Country)).
