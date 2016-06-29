%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(cf_flow).

-export([lookup/1, lookup/2]).


-include("callflow.hrl").
-include_lib("kazoo/src/kz_json.hrl").

-record(pattern, {
                  flow_id :: ne_binary(),
                  has_groups :: boolean(),
                  names = [] :: ne_binaries(),
                  regex :: re:mp()
                 }).

-type pattern() :: #pattern{}.
-type patterns() :: [pattern()].

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% lookup the callflow based on the requested number in the account
%% @end
%%-----------------------------------------------------------------------------
-type lookup_ret() :: {'ok', kz_json:object(), boolean()} | {'error', any()}.

-spec lookup(kapps_call:call()) -> lookup_ret().
lookup(Call) ->
    lookup(kapps_call:request_user(Call), kapps_call:account_id(Call)).

-spec lookup(ne_binary(), ne_binary()) -> lookup_ret().
lookup(Number, AccountId) when not is_binary(Number) ->
    lookup(kz_util:to_binary(Number), AccountId);
lookup(<<>>, _) ->
    {'error', 'invalid_number'};
lookup(Number, AccountId) ->
    case kz_cache:fetch_local(?CACHE_NAME, ?CF_FLOW_CACHE_KEY(Number, AccountId)) of
        {'ok', FlowId} -> return_callflow_doc(FlowId, AccountId);
        {'error', 'not_found'} -> do_lookup(Number, AccountId)
    end.

return_callflow_doc(FlowId, AccountId) ->
    return_callflow_doc(FlowId, AccountId, []).

return_callflow_doc(FlowId, AccountId, Props) ->
    Db = kz_util:format_account_db(AccountId),
    case kz_datamgr:open_cache_doc(Db, FlowId) of
        {'ok', Doc} ->
            {'ok', kz_json:set_values(Props, Doc), FlowId =:= ?NO_MATCH_CF};
        Error -> Error
    end.

do_lookup(Number, AccountId) ->
    Db = kz_util:format_account_db(AccountId),
    lager:info("searching for callflow in ~s to satisfy '~s'", [Db, Number]),
    Options = [{'key', Number}, 'include_docs'],
    case kz_datamgr:get_results(Db, ?LIST_BY_NUMBER, Options) of
        {'error', _}=E -> E;
        {'ok', []} when Number =/= ?NO_MATCH_CF ->
            lookup_patterns(Number, AccountId);
        {'ok', []} -> {'error', 'not_found'};
        {'ok', [JObj]} ->
            Flow = kz_json:get_value(<<"doc">>, JObj),
            cache_callflow_number(Number, AccountId, Flow);
        {'ok', [JObj | _Rest]} ->
            lager:info("lookup resulted in more than one result, using the first"),
            Flow = kz_json:get_value(<<"doc">>, JObj),
            cache_callflow_number(Number, AccountId, Flow)
    end.

cache_callflow_number(Number, AccountId, Flow) ->
    AccountDb = kz_util:format_account_db(AccountId),
    CacheOptions = [{'origin', [{'db', AccountDb, <<"callflow">>}]}
                   ,{'expires', ?MILLISECONDS_IN_HOUR}
                   ],
    kz_cache:store_local(?CACHE_NAME, ?CF_FLOW_CACHE_KEY(Number, AccountId), kz_doc:id(Flow), CacheOptions),
    {'ok', Flow, Number =:= ?NO_MATCH_CF}.

%% only route to nomatch when Number is all digits and/or +
maybe_use_nomatch(<<"+", Number/binary>>, AccountId) ->
    maybe_use_nomatch(Number, AccountId);
maybe_use_nomatch(Number, AccountId) ->
    case lists:all(fun is_digit/1, kz_util:to_list(Number)) of
        'true' -> lookup(?NO_MATCH_CF, AccountId);
        'false' ->
            lager:info("can't use no_match: number not all digits: ~s", [Number]),
            {'error', 'not_found'}
    end.

is_digit(X) when X >= $0, X =< $9 -> 'true';
is_digit(_) -> 'false'.

-spec fetch_patterns(ne_binary()) -> {'ok', patterns()} | {'error', 'not_found'}.
fetch_patterns(AccountId)->
    case kz_cache:fetch_local(?CACHE_NAME, ?CF_PATTERN_CACHE_KEY(AccountId)) of
        {'ok', _Patterns}= OK -> OK;
        {'error', 'not_found'} -> load_patterns(AccountId)
    end.

-spec load_patterns(ne_binary()) -> {'ok', patterns()} | {'error', 'not_found'}.
load_patterns(AccountId) ->
    Db = kz_util:format_account_db(AccountId),
    case kz_datamgr:get_results(Db, ?LIST_BY_PATTERN, ['include_docs']) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', JObjs} -> compile_patterns(AccountId, JObjs);
        {'error', _}=_E ->
            lager:error("error getting callflow patterns for account ~s : ~p", [AccountId, _E]),
            {'error', 'not_found'}
    end.

compile_patterns(AccountId, JObjs) ->
    compile_patterns(AccountId, JObjs, []).

compile_patterns(AccountId, [], Acc) ->
    cache_patterns(AccountId, Acc);
compile_patterns(AccountId, [JObj | JObjs], Acc) ->
    Regex = kz_json:get_value(<<"key">>, JObj),
    FlowId = kz_doc:id(JObj),
    case re:compile(Regex) of
        {'ok', {re_pattern, Groups, _, _, _} = MP}
          when Groups =:= 0 ->
            Pat = #pattern{flow_id=FlowId, regex = MP},
            compile_patterns(AccountId, JObjs, [Pat | Acc]);
        {'ok', MP} ->
            {'namelist', Names} = re:inspect(MP, 'namelist'),
            Pat = #pattern{flow_id=FlowId, regex = MP, names = Names, has_groups = 'true'},
            compile_patterns(AccountId, JObjs, [Pat | Acc]);
        _Err ->
            lager:debug("unexpected result compiling regular expression : ~p", [_Err]),
            compile_patterns(AccountId, JObjs, Acc)
    end.

-spec cache_patterns(ne_binary(), patterns()) -> {'ok', patterns()}.
cache_patterns(AccountId, Patterns) ->
    AccountDb = kz_util:format_account_db(AccountId),
    CacheOptions = [{'origin', [{'db', AccountDb, <<"callflow">>}]}],
    kz_cache:store_local(?CACHE_NAME, ?CF_PATTERN_CACHE_KEY(AccountId), Patterns, CacheOptions),
    {'ok', Patterns}.

-spec lookup_patterns(ne_binary(), ne_binary()) ->
                                      {'ok', {kz_json:object(), api_binary()}} |
                                      {'error', any()}.
lookup_patterns(Number, AccountId) ->
    case fetch_patterns(AccountId) of
        {'ok', Patterns} -> lookup_callflow_patterns(Patterns, Number, AccountId);
        _Error -> maybe_use_nomatch(Number, AccountId)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_callflow_patterns(patterns(), ne_binary(), ne_binary()) -> lookup_ret().
lookup_callflow_patterns(Patterns, Number, AccountId) ->
    case test_callflow_patterns(Patterns, Number) of
        'no_match' -> maybe_use_nomatch(Number, AccountId);
        {Match, #pattern{flow_id=FlowId}=Pattern} ->
            NameMap = get_captured_names(Number, Pattern),
            Props = [{<<"capture_group">>, Match}
                     ,{<<"capture_groups">>, kz_json:from_list(NameMap)}
                    ],
            return_callflow_doc(FlowId, AccountId, Props)
    end.

-spec get_captured_names(ne_binary(), pattern()) -> kz_proplist().
get_captured_names(_Number, #pattern{names=[]}) -> [];
get_captured_names(Number, #pattern{regex=Regex, names=Names}) ->
    case re:run(Number, Regex, [{'capture', 'all_names', 'binary'}]) of
        {'match', L} -> props:filter_empty(lists:zip(Names,L));
        _ -> []
    end.

-type test_pattern_acc() ::  {binary(), pattern()}.

-spec test_callflow_patterns(patterns(), ne_binary()) -> 'no_match' | test_pattern_acc().
test_callflow_patterns(Patterns, Number) ->
test_callflow_patterns(Patterns, Number, {<<>>, #pattern{}}).

-spec test_callflow_patterns(patterns(), ne_binary(), test_pattern_acc()) ->
                                    'no_match' | test_pattern_acc().
test_callflow_patterns([], _, {<<>>, _}) -> 'no_match';
test_callflow_patterns([], _, Result) -> Result;
test_callflow_patterns([ #pattern{regex=Regex}=Pattern |T], Number, {Matched, _}=Result) ->
    case re:run(Number, Regex, match_options(Pattern)) of
        {'match', Groups} ->
            case hd(lists:sort(fun(A, B) -> byte_size(A) >= byte_size(B) end, Groups)) of
                Match when byte_size(Match) > byte_size(Matched) ->
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
