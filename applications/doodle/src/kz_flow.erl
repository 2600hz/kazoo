%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_flow).

-export([lookup/1, lookup/2]).
-export([contains_no_match/1]).

-include("doodle.hrl").

-record(pattern, {flow_id :: kz_term:ne_binary()
                 ,has_groups :: boolean()
                 ,names = [] :: kz_term:ne_binaries()
                 ,regex :: re:mp()
                 }).

-type pattern() :: #pattern{}.
-type patterns() :: [pattern()].

%%------------------------------------------------------------------------------
%% @doc Lookup the callflow based on the requested number in the account.
%% @end
%%------------------------------------------------------------------------------

-type lookup_ret() :: {'ok', kzd_flows:doc(), boolean()} | {'error', any()}.

-spec lookup(kapps_im:im()) -> lookup_ret().
lookup(Im) ->
    lookup(kapps_im:to(Im), kapps_im:account_id(Im)).

-spec lookup(kz_term:ne_binary(), kz_term:ne_binary()) -> lookup_ret().
lookup(Number, AccountId) when not is_binary(Number) ->
    lookup(kz_term:to_binary(Number), AccountId);
lookup(<<>>, _) ->
    {'error', 'invalid_number'};
lookup(Number, AccountId) ->
    case kz_cache:fetch_local(?CACHE_NAME, ?MSG_FLOW_CACHE_KEY(Number, AccountId)) of
        {'ok', FlowId} -> return_flow_doc(FlowId, AccountId);
        {'error', 'not_found'} -> do_lookup(Number, AccountId)
    end.

-spec return_flow_doc(kz_term:ne_binary(), kz_term:ne_binary()) -> lookup_ret().
return_flow_doc(FlowId, AccountId) ->
    return_flow_doc(FlowId, AccountId, []).

-spec return_flow_doc(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> lookup_ret().
return_flow_doc(FlowId, AccountId, Props) ->
    Db = kzs_util:format_account_db(AccountId),
    case kz_datamgr:open_cache_doc(Db, FlowId) of
        {'ok', Doc} ->
            {'ok', kz_json:set_values(Props, Doc), contains_no_match(Doc)};
        Error -> Error
    end.

-spec contains_no_match(kzd_callflows:doc()) -> boolean().
contains_no_match(Doc) ->
    lists:any(fun(Number) when Number =:= ?NO_MATCH_FLOW ->
                      'true';
                 (_) ->
                      'false'
              end, kzd_flows:numbers(Doc)).

-spec do_lookup(kz_term:ne_binary(), kz_term:ne_binary()) -> lookup_ret().
do_lookup(Number, AccountId) ->
    lager:info("searching for flow in ~s to satisfy '~s'", [AccountId, Number]),
    Options = [{'lookup_callflows', 'true'}],
    do_lookup(Number, AccountId, ?MSG_LIST_BY_NUMBER, Options).

-spec do_lookup(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> lookup_ret().
do_lookup(Number, AccountId, ViewName, LookupOptions) ->
    Db = kzs_util:format_account_db(AccountId),
    Options = [{'key', Number}, 'include_docs'],
    LookupCallflows = props:is_true('lookup_callflows', LookupOptions, 'true'),
    case kz_datamgr:get_results(Db, ViewName, Options) of
        {'error', _}=E -> E;
        {'ok', []} when ViewName =/= ?CF_LIST_BY_NUMBER
                        andalso LookupCallflows =:= 'true' ->
            do_lookup(Number, AccountId, ?CF_LIST_BY_NUMBER, LookupOptions);
        {'ok', []} when Number =/= ?NO_MATCH_FLOW
                        andalso ViewName =:= ?CF_LIST_BY_NUMBER
                        andalso LookupCallflows =:= 'true' ->
            lookup_patterns(Number, AccountId);
        {'ok', []} when Number =/= ?NO_MATCH_FLOW
                        andalso ViewName =/= ?CF_LIST_BY_NUMBER
                        andalso LookupCallflows =:= 'false' ->
            lookup_patterns(Number, AccountId);
        {'ok', []} ->
            {'error', 'not_found'};
        {'ok', [JObj]} ->
            Flow = kz_json:get_value(<<"doc">>, JObj),
            cache_flow_number(Number, AccountId, Flow);
        {'ok', [JObj | _Rest]} ->
            lager:info("lookup resulted in more than one result, using the first"),
            Flow = kz_json:get_value(<<"doc">>, JObj),
            cache_flow_number(Number, AccountId, Flow)
    end.

-spec cache_flow_number(kz_term:ne_binary(), kz_term:ne_binary(), kzd_callflows:doc()) -> lookup_ret().
cache_flow_number(Number, AccountId, Flow) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    CacheOptions = [{'origin', [{'db', AccountDb, kz_doc:type(Flow)}]}
                   ,{'expires', ?MILLISECONDS_IN_HOUR}
                   ],
    kz_cache:store_local(?CACHE_NAME, ?MSG_FLOW_CACHE_KEY(Number, AccountId), kz_doc:id(Flow), CacheOptions),
    {'ok', Flow, contains_no_match(Flow)}.

-spec maybe_use_nomatch(kz_term:ne_binary(), kz_term:ne_binary()) -> lookup_ret().
%% only route to nomatch when Number is all digits and/or +
maybe_use_nomatch(<<"+", Number/binary>>, AccountId) ->
    maybe_use_nomatch(Number, AccountId);
maybe_use_nomatch(Number, AccountId) ->
    case lists:all(fun is_digit/1, kz_term:to_list(Number)) of
        'true' -> lookup(?NO_MATCH_FLOW, AccountId);
        'false' ->
            lager:info("can't use no_match: number not all digits: ~s", [Number]),
            {'error', 'not_found'}
    end.

-spec is_digit(char()) -> boolean().
is_digit(X) when X >= $0, X =< $9 -> 'true';
is_digit(_) -> 'false'.

-spec fetch_patterns(kz_term:ne_binary()) -> {'ok', patterns()} | {'error', 'not_found'}.
fetch_patterns(AccountId)->
    case kz_cache:fetch_local(?CACHE_NAME, ?MSG_PATTERN_CACHE_KEY(AccountId)) of
        {'ok', _Patterns}= OK -> OK;
        {'error', 'not_found'} -> load_patterns(AccountId)
    end.

-spec load_patterns(kz_term:ne_binary()) -> {'ok', patterns()} | {'error', 'not_found'}.
load_patterns(AccountId) ->
    Db = kzs_util:format_account_db(AccountId),
    case kz_datamgr:get_results(Db, ?MSG_LIST_BY_PATTERN, ['include_docs']) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', JObjs} -> compile_patterns(AccountId, JObjs);
        {'error', _}=_E ->
            lager:error("error getting textflow patterns for account ~s : ~p", [AccountId, _E]),
            {'error', 'not_found'}
    end.

-spec compile_patterns(kz_term:ne_binary(), kz_json:objects()) -> {'ok', patterns()}.
compile_patterns(AccountId, JObjs) ->
    compile_patterns(AccountId, JObjs, []).

compile_patterns(AccountId, [], Acc) ->
    cache_patterns(AccountId, Acc);
compile_patterns(AccountId, [JObj | JObjs], Acc) ->
    Regex = kz_json:get_ne_binary_value(<<"key">>, JObj),
    FlowId = kz_doc:id(JObj),
    case re:compile(Regex) of
        {'ok', {'re_pattern', Groups, _, _, _} = MP}
          when Groups =:= 0 ->
            Pat = #pattern{flow_id=FlowId, regex=MP, has_groups='false'},
            compile_patterns(AccountId, JObjs, [Pat | Acc]);
        {'ok', MP} ->
            {'namelist', Names} = re:inspect(MP, 'namelist'),
            Pat = #pattern{flow_id=FlowId, regex=MP, names=Names, has_groups='true'},
            compile_patterns(AccountId, JObjs, [Pat | Acc]);
        _Err ->
            lager:debug("unexpected result compiling regular expression : ~p", [_Err]),
            compile_patterns(AccountId, JObjs, Acc)
    end.

-spec cache_patterns(kz_term:ne_binary(), patterns()) -> {'ok', patterns()}.
cache_patterns(AccountId, Patterns) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    CacheOptions = [{'origin', [{'db', AccountDb, <<"textflow">>}]}],
    kz_cache:store_local(?CACHE_NAME, ?MSG_PATTERN_CACHE_KEY(AccountId), Patterns, CacheOptions),
    {'ok', Patterns}.

-spec lookup_patterns(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', {kz_json:object(), kz_term:api_binary()}} |
          {'error', any()}.
lookup_patterns(Number, AccountId) ->
    case fetch_patterns(AccountId) of
        {'ok', Patterns} -> lookup_flow_patterns(Patterns, Number, AccountId);
        _Error -> maybe_use_nomatch(Number, AccountId)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec lookup_flow_patterns(patterns(), kz_term:ne_binary(), kz_term:ne_binary()) -> lookup_ret().
lookup_flow_patterns(Patterns, Number, AccountId) ->
    case test_flow_patterns(Patterns, Number) of
        'no_match' -> maybe_use_nomatch(Number, AccountId);
        {Match, #pattern{flow_id=FlowId}=Pattern} ->
            NameMap = get_captured_names(Number, Pattern),
            Props = [{<<"capture_group">>, Match}
                    ,{<<"capture_groups">>, kz_json:from_list(NameMap)}
                    ],
            return_flow_doc(FlowId, AccountId, props:filter_empty(Props))
    end.

-spec get_captured_names(kz_term:ne_binary(), pattern()) -> kz_term:proplist().
get_captured_names(_Number, #pattern{names=[]}) -> [];
get_captured_names(Number, #pattern{regex=Regex, names=Names}) ->
    case re:run(Number, Regex, [{'capture', 'all_names', 'binary'}]) of
        {'match', L} -> props:filter_empty(lists:zip(Names,L));
        _ -> []
    end.

-type test_pattern_acc() ::  {binary(), pattern() | 'undefined'}.

-spec test_flow_patterns(patterns(), kz_term:ne_binary()) -> 'no_match' | test_pattern_acc().
test_flow_patterns(Patterns, Number) ->
    test_flow_patterns(Patterns, Number, {<<>>, 'undefined'}).

-spec test_flow_patterns(patterns(), kz_term:ne_binary(), test_pattern_acc()) ->
          'no_match' | test_pattern_acc().
test_flow_patterns([], _, {_, 'undefined'}) -> 'no_match';
test_flow_patterns([], _, Result) -> Result;
test_flow_patterns([#pattern{regex=Regex}=Pattern |T], Number, {Matched, P}=Result) ->
    case re:run(Number, Regex, match_options(Pattern)) of
        {'match', Groups} ->
            case hd(lists:sort(fun(A, B) -> byte_size(A) >= byte_size(B) end, Groups)) of
                Match when P =:= 'undefined'
                           orelse byte_size(Match) > byte_size(Matched) ->
                    test_flow_patterns(T, Number, {Match, Pattern});
                _ -> test_flow_patterns(T, Number, Result)
            end;
        _ ->
            test_flow_patterns(T, Number, Result)
    end.

-spec match_options(pattern()) -> list().
match_options(#pattern{has_groups='true'}) ->
    [{'capture', 'all_but_first', 'binary'}];
match_options(_) ->
    [{'capture', 'all', 'binary'}].
