%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%% This module looks up the Caller ID Name by matching
%%% numbers/patters with the provided lists.
%%%
%%% "data":{
%%%   "lists": ["01fc63f92d9b89a25dd4ff1039e64497"]
%%% },
%%% @end
%%% @contributors
%%%     Sponsored by Conversant Ltd,
%%%         implemented by SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(cf_lookupcidname).

-export([handle/2]).

-include("callflow.hrl").

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CallerNumber = kapps_call:caller_id_number(Call),
    ListIds = kz_json:get_value(<<"lists">>, Data, []),
    AccountDb = kapps_call:account_db(Call),
    lager:debug("matching ~p in ~p", [CallerNumber, AccountDb]),
    CallerName = case match_number_in_lists(AccountDb, CallerNumber, ListIds) of
                     'continue' ->
                         lager:debug("matching regexps"),
                         match_regexp_in_lists(AccountDb, CallerNumber, ListIds);
                     {'stop', CallerName_} -> CallerName_
                 end,
    case CallerName of
        'undefined' -> cf_exe:continue(Call);
        CallerName ->
            lager:info("setting caller name to ~p", [CallerName]),
            cf_exe:continue(kapps_call:set_caller_id_name(CallerName, Call))
    end.

-type match_number_result() :: {'stop', api_binary()} | 'continue'.
-spec match_number_in_lists(ne_binary(), ne_binary(), ne_binaries()) -> match_number_result().
match_number_in_lists(AccountDb, Number, Lists) ->
    Prefixes = build_keys(Number),
    match_prefixes_in_lists(AccountDb, Prefixes, Lists).

-spec match_prefixes_in_lists(ne_binary(), ne_binaries(), ne_binaries()) -> match_number_result().
match_prefixes_in_lists(AccountDb, Prefixes, [ListId | Rest]) ->
    case match_prefixes_in_list(AccountDb, Prefixes, ListId) of
        {'stop', _Name} = Result -> Result;
        'continue' -> match_prefixes_in_lists(AccountDb, Prefixes, Rest)
    end;
match_prefixes_in_lists(_AccountDb, _Number, []) ->
    lager:debug("no matching prefix"),
    'continue'.

-spec match_prefixes_in_list(ne_binary(), ne_binaries(), ne_binary()) -> match_number_result().
match_prefixes_in_list(AccountDb, Prefixes, ListId) ->
    Keys = [[ListId, Prefix] || Prefix <- Prefixes],
    case kz_datamgr:get_results(AccountDb
                               ,<<"lists/match_prefix_in_list">>
                               ,[{'keys', Keys}, {'include_docs', 'true'}])
    of
        {'ok', [_ | _] = Matched} ->
            lager:debug("matched ~p prefixes, getting longest", [length(Matched)]),
            ListEntry = hd(lists:sort(fun compare_prefixes/2, Matched)),
            Name = kz_json:get_value([<<"doc">>, <<"displayname">>], ListEntry),
            lager:debug("matched prefix ~p", [get_prefix(ListEntry)]),
            {'stop', Name};
        {'ok', []} ->
            'continue';
        {'error', Error} ->
            lager:warning("error while matching prefixes in list ~p: ~p", [ListId, Error]),
            'continue'
    end.

-spec compare_prefixes(kz_json:object(), kz_json:object()) -> boolean().
compare_prefixes(JObj1, JObj2) ->
    byte_size(get_prefix(JObj1)) >= byte_size(get_prefix(JObj2)).

-spec get_prefix(kz_json:object()) -> binary().
get_prefix(JObj) ->
    Doc = kz_json:get_value(<<"doc">>, JObj),
    kz_json:get_ne_binary_value(<<"number">>, Doc, kz_json:get_ne_binary_value(<<"prefix">>, Doc)).

%% TODO: this function from hon_util, may be place it somewhere in library?
-spec build_keys(binary()) -> binaries().
build_keys(<<"+", E164/binary>>) ->
    build_keys(E164);
build_keys(<<D:1/binary, Rest/binary>>) ->
    build_keys(Rest, D, [D]).

-spec build_keys(binary(), binary(), binaries()) -> binaries().
build_keys(<<D:1/binary, Rest/binary>>, Prefix, Acc) ->
    build_keys(Rest, <<Prefix/binary, D/binary>>, [<<Prefix/binary, D/binary>> | Acc]);
build_keys(<<>>, _, Acc) -> Acc.

-spec match_regexp_in_list(ne_binary(), ne_binary(), ne_binary()) ->
                                  'continue' | {'stop', api_binary()}.
match_regexp_in_list(AccountDb, Number, ListId) when is_binary(ListId) ->
    case kz_datamgr:get_results(AccountDb
                               ,<<"lists/regexps_in_list">>
                               ,[{'keys', [ListId]} , {'include_docs', 'true'}])
    of
        {'ok', Regexps} ->
            match_regexp(Regexps, Number);
        Error ->
            lager:warning("getting regexps error: ~p", [Error]),
            'continue'
    end.

-spec match_regexp_in_lists(ne_binary(), ne_binary(), ne_binary() | [ne_binary()]) -> api_binary().
match_regexp_in_lists(AccountDb, Number, [ListId | Rest]) ->
    case match_regexp_in_list(AccountDb, Number, ListId) of
        'continue' -> match_regexp_in_lists(AccountDb, Number, Rest);
        {'stop', Name} -> Name
    end;
match_regexp_in_lists(_, _, []) ->
    'undefined'.

-spec match_regexp(kz_json:objects(), ne_binary()) ->
                          'continue' | {'stop', api_binary()}.
match_regexp([Re | Rest], Number) ->
    case re:run(Number, kz_json:get_value(<<"value">>, Re)) of
        'nomatch' -> match_regexp(Rest, Number);
        {'match', _} ->
            lager:debug("matched regexp ~p", [Re]),
            {'stop', kz_json:get_value([<<"doc">>, <<"displayname">>], Re)}
    end;
match_regexp([], _Number) ->
    'continue'.
