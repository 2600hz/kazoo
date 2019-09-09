%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Sergey Safarov sponsored by Audian
%%% @end
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%%-----------------------------------------------------------------------------
-module(cf_blacklist).

-export([lookup/4]).

-include("callflow.hrl").

-record(pattern, {action :: kz_term:ne_binary()
                 ,caller_name :: kz_term:api_ne_binary()
                 ,has_groups = 'false' :: boolean()
                 ,names = [] :: kz_term:ne_binaries()
                 ,regex :: re:mp() | 'undefined'
                 }).

-type pattern() :: #pattern{}.
-type patterns() :: [pattern()].
-type patterns_view_item() :: {kz_term:ne_binary(), kz_json:object()}.
-type patterns_view_items() :: [patterns_view_item()].

-type lookup_ret_number() :: {'ok', kz_term:ne_binary(), kz_term:api_ne_binary(), boolean()} | {'error', any()}.
-type lookup_ret_patterns() :: {'ok', patterns()} | {'error', any()}.

-spec lookup(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> lookup_ret_number().
lookup(Number, AccountId, OwnerId, Strategy) when not is_binary(Number) ->
    lookup(kz_term:to_binary(Number), AccountId, OwnerId, Strategy);
lookup(<<>>, _, _, _) ->
    {'error', 'invalid_number'};
lookup(Number, AccountId, OwnerId, Strategy) ->
    case kz_cache:fetch_local(?CACHE_NAME, ?BLACKLIST_NUMBER_CACHE_KEY(AccountId, OwnerId, Number)) of
        {'ok', 'undefined'} -> {'error', 'not_found'};
        {'ok', {Action, CallerName}} -> {'ok', Action, CallerName, 'false'};
        {'error', 'not_found'} -> do_lookup(Number, AccountId, OwnerId, Strategy)
    end.

-spec do_lookup(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> lookup_ret_number().
do_lookup(Number, AccountId, OwnerId, Strategy) ->
    case kzd_blacklists:fetch_number(AccountId, OwnerId, Number, #{<<"enabled">> => 'true', <<"brief">> => 'true', <<"strategy">> => Strategy}) of
        {'error', 'not_found'} when Number =/= ?NO_MATCH_BL ->
            lookup_patterns(Number, AccountId, OwnerId, Strategy);
        {'error', 'not_found'} = Responce -> Responce;
        {'ok', [JObj] } ->
            Action = kz_json:get_ne_binary_value(<<"action">>, JObj, <<"block">>),
            CallerName = kz_json:get_ne_binary_value(<<"name">>, JObj),
            cache_number(AccountId, OwnerId, Number, Action, CallerName)
    end.

-spec lookup_patterns(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> lookup_ret_number().
lookup_patterns(Number, AccountId, OwnerId, Strategy) ->
    case fetch_patterns(AccountId, OwnerId) of
        {'ok', Patterns} -> lookup_blacklist_patterns(Patterns, Number, AccountId, OwnerId, Strategy);
        _Error -> maybe_use_nomatch(Number, AccountId, OwnerId, Strategy)
    end.

-spec fetch_patterns(kz_term:ne_binary(), kz_term:ne_binary()) -> lookup_ret_patterns().
fetch_patterns(AccountId, OwnerId)->
    case kz_cache:fetch_local(?CACHE_NAME, ?BLACKLIST_PATTERNS_CACHE_KEY(AccountId, OwnerId)) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', _Patterns} = Responce -> Responce;
        {'error', 'not_found'} ->
            lager:debug("blacklist patterns cache is empty for owner ~s, loading from database for account: ~s", [OwnerId, AccountId]),
            load_patterns(AccountId, OwnerId)
    end.

-spec load_patterns(kz_term:ne_binary(), kz_term:api_ne_binary()) -> lookup_ret_patterns().
load_patterns(AccountId, OwnerId) ->
    case kzd_blacklists:fetch_patterns(AccountId, OwnerId, #{<<"enabled">> => 'true', <<"brief">> => 'true'}) of
        {'ok', JObj} -> compile_patterns(AccountId, OwnerId, JObj);
        {'error', _} -> cache_patterns(AccountId, OwnerId, [])
    end.

-spec compile_patterns(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_json:object()) -> lookup_ret_patterns().
compile_patterns(AccountId, OwnerId, JObj) ->
    compile_patterns(AccountId, OwnerId, kz_json:to_proplist(JObj), []).

-spec compile_patterns(kz_term:ne_binary(), kz_term:api_ne_binary(), patterns_view_items(), patterns()) -> lookup_ret_patterns().
compile_patterns(AccountId, OwnerId, [], Patterns) ->
    cache_patterns(AccountId, OwnerId, Patterns);
compile_patterns(AccountId, OwnerId, [{Regex, JObj} | Rest], Acc) ->
    Action = kz_json:get_ne_binary_value(<<"action">>, JObj, <<"block">>),
    CallerName = kz_json:get_ne_binary_value(<<"name">>, JObj),
    case re:compile(Regex) of
        {'ok', {'re_pattern', Groups, _, _, _} = MP}
          when Groups =:= 0 ->
            Pattern = #pattern{action=Action, caller_name=CallerName, regex=MP, has_groups='false'},
            compile_patterns(AccountId, OwnerId, Rest, [Pattern | Acc]);
        {'ok', MP} ->
            {'namelist', Names} = re:inspect(MP, 'namelist'),
            Pattern = #pattern{action=Action, caller_name=CallerName, regex=MP, names=Names, has_groups='true'},
            compile_patterns(AccountId, OwnerId, Rest, [Pattern | Acc]);
        _Err ->
            lager:debug("unexpected result compiling regular expression : ~p", [_Err]),
            compile_patterns(AccountId, OwnerId, Rest, Acc)
    end.

-spec cache_patterns(kz_term:ne_binary(), kz_term:api_ne_binary(), patterns()) -> lookup_ret_patterns().
cache_patterns(AccountId, OwnerId, Patterns) ->
    AccountDb = kz_util:format_account_db(AccountId),
    CacheOptions = [{'origin', [{'db', AccountDb, <<"blacklist">>}]}
                   ,{'expires', ?MILLISECONDS_IN_HOUR}
                   ],
    kz_cache:store_local(?CACHE_NAME, ?BLACKLIST_PATTERNS_CACHE_KEY(AccountId, OwnerId), Patterns, CacheOptions),
    case Patterns of
        [] -> {'error', 'not_found'};
        _ -> {'ok', Patterns}
    end.

-spec cache_number(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> lookup_ret_number().
cache_number(AccountId, OwnerId, Number, Action, CallerName) ->
    AccountDb = kz_util:format_account_db(AccountId),
    CacheOptions = [{'origin', [{'db', AccountDb, <<"blacklist">>}]}
                   ,{'expires', ?MILLISECONDS_IN_HOUR}
                   ],
    kz_cache:store_local(?CACHE_NAME, ?BLACKLIST_NUMBER_CACHE_KEY(AccountId, OwnerId, Number), {Action, CallerName}, CacheOptions),
    case Action of
        'undefined' -> {'error', 'not_found'};
        _ -> {'ok', Action, CallerName, Number =:= ?NO_MATCH_BL}
    end.

-spec maybe_use_nomatch(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> lookup_ret_number().
maybe_use_nomatch(Number, AccountId, OwnerId, Strategy) ->
    case knm_converters:is_reconcilable(Number, AccountId) of
        'true' -> try_nomatch(Number, AccountId, OwnerId, Strategy);
        'false' ->
            lager:info("can't use no_match: number not all digits: ~s", [Number]),
            cache_number(AccountId, OwnerId, Number, 'undefined', 'undefined')
    end.

-spec try_nomatch(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> lookup_ret_number().
try_nomatch(Number, AccountId, OwnerId, Strategy) ->
    case lookup(?NO_MATCH_BL, AccountId, OwnerId, Strategy) of
        {'error', _} -> cache_number(AccountId, OwnerId, Number, 'undefined', 'undefined');
        {'ok', Action, CallerName, 'true'} -> cache_number(AccountId, OwnerId, Number, Action, CallerName)
    end.

-spec lookup_blacklist_patterns(patterns(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> lookup_ret_number().
lookup_blacklist_patterns(Patterns, Number, AccountId, OwnerId, Strategy) ->
    case test_callflow_patterns(Patterns, Number) of
        'no_match' -> maybe_use_nomatch(Number, AccountId, OwnerId, Strategy);
        {_Match, #pattern{action=Action, caller_name=CallerName}} ->
            cache_number(AccountId, OwnerId, Number, Action, CallerName)
    end.

-type test_pattern_acc() ::  {binary(), pattern() | 'undefined'}.

-spec test_callflow_patterns(patterns(), kz_term:ne_binary()) -> 'no_match' | test_pattern_acc().
test_callflow_patterns(Patterns, Number) ->
    test_callflow_patterns(Patterns, Number, {<<>>, 'undefined'}).

-spec test_callflow_patterns(patterns(), kz_term:ne_binary(), test_pattern_acc()) ->
                                    'no_match' | test_pattern_acc().
test_callflow_patterns([], _, {_, 'undefined'}) -> 'no_match';
test_callflow_patterns([], _, Result) -> Result;
test_callflow_patterns([#pattern{regex=Regex}=Pattern |T], Number, {Matched, P}=Result) ->
    case re:run(Number, Regex, match_options(Pattern)) of
        {'match', Groups} ->
            case hd(lists:sort(fun(A, B) -> byte_size(A) >= byte_size(B) end, Groups)) of
                Match when P =:= 'undefined'
                           orelse byte_size(Match) > byte_size(Matched) ->
                    test_callflow_patterns(T, Number, {Match, Pattern});
                _ -> test_callflow_patterns(T, Number, Result)
            end;
        _ ->
            test_callflow_patterns(T, Number, Result)
    end.

-spec match_options(pattern()) -> list().
match_options(#pattern{has_groups='true'}) ->
    [{'capture', 'all_but_first', 'binary'}];
match_options(_) ->
    [{'capture', 'all', 'binary'}].
