%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_converter_regex).

-include("../knm.hrl").

-export([
    normalize/1
    ,to_e164/1
]).

-define(DEFAULT_E164_CONVERTERS, [{<<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$">>
                                   ,wh_json:from_list([{<<"prefix">>, <<"+1">>}])
                                  }
                                  ,{<<"^011(\\d*)$|^00(\\d*)$">>
                                    ,wh_json:from_list([{<<"prefix">>, <<"+">>}])
                                   }
                                  ,{<<"^[2-9]\\d{7,}$">>
                                    ,wh_json:from_list([{<<"prefix">>, <<"+">>}])
                                   }
                                 ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize(ne_binary()) -> ne_binary().
normalize(Num) ->
    to_e164(Num).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_e164(ne_binary()) -> ne_binary().
to_e164(<<$+, _/binary>> = N) -> N;
to_e164(Number) ->
    Converters = get_e164_converters(),
    Regexes = wh_json:get_keys(Converters),
    maybe_convert_to_e164(Regexes, Converters, Number).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_e164_converters() -> wh_json:object().
get_e164_converters() ->
    Default = wh_json:from_list(?DEFAULT_E164_CONVERTERS),
    try whapps_config:get(?KNM_CONFIG_CAT, <<"e164_converters">>, Default) of
        Converters -> Converters
    catch
        _:_ ->
            Default
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_convert_to_e164(ne_binaries(), wh_json:object(), ne_binary()) -> ne_binary().
maybe_convert_to_e164([], _, Number) -> Number;
maybe_convert_to_e164([Regex|Regexs], Converters, Number) ->
    case re:run(Number, Regex, [{'capture', 'all', 'binary'}]) of
        'nomatch' ->
            maybe_convert_to_e164(Regexs, Converters, Number);
        {'match', Captures} ->
            Root = lists:last(Captures),
            Prefix = wh_json:get_binary_value([Regex, <<"prefix">>], Converters, <<>>),
            Suffix = wh_json:get_binary_value([Regex, <<"suffix">>], Converters, <<>>),
            <<Prefix/binary, Root/binary, Suffix/binary>>
    end.

