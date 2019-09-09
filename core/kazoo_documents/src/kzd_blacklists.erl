%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_blacklists).

-export([type/0]).
-export([new/0]).
-export([compare_actions/3]).
-export([fetch_number/3, fetch_number/4]).
-export([fetch_patterns/2, fetch_patterns/3]).
-export([action/1, action/2, set_action/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([flags/1, flags/2, set_flags/2]).
-export([name/1, name/2, set_name/2]).
-export([numbers/1, numbers/2, set_numbers/2, format_numbers/1]).
-export([numbers_name/1, numbers_name/2, set_numbers_name/2]).
-export([owner_id/1, owner_id/2, set_owner_id/2, set_owner_id/3]).
-export([patterns/1, patterns/2, set_patterns/2]).
-export([patterns_name/1, patterns_name/2, set_patterns_name/2]).
-export([is_blacklist/1]).
-export([is_valid_owner_id/2]).
-export([should_block_anonymous/1, should_block_anonymous/2, set_should_block_anonymous/2]).

-include("kz_documents.hrl").

-define(LIST_BY_OWNER, <<"blacklists/listing_by_owner">>).
-define(STRICT, <<"strict">>).
-define(RELAXED, <<"relaxed">>).

-define(IS_BLACKLIST_VALUE(CT)
       ,(CT =:= <<"block">>
             orelse CT =:= <<"skip_human">>
             orelse CT =:= <<"ask_human">>
             orelse CT =:= <<"pass">>
        )).

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"blacklists">>).

-spec type() -> kz_term:ne_binary().
type() -> <<"blacklists">>.

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec compare_actions(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
compare_actions(_Strategy, 'undefined', 'undefined') ->
    'undefined';
compare_actions(?STRICT, <<"block">>, _) ->
    <<"block">>;
compare_actions(?STRICT, _, <<"block">>) ->
    <<"block">>;
compare_actions(?STRICT, <<"skip_human">>, _) ->
    <<"skip_human">>;
compare_actions(?STRICT, _, <<"skip_human">>) ->
    <<"skip_human">>;
compare_actions(?STRICT, <<"ask_human">>, _) ->
    <<"ask_human">>;
compare_actions(?STRICT, _, <<"ask_human">>) ->
    <<"ask_human">>;
compare_actions(?STRICT, <<"pass">>, _) ->
    <<"pass">>;
compare_actions(?STRICT, _, <<"pass">>) ->
    <<"pass">>;
compare_actions(?STRICT, Action1, Action2) ->
    lager:info("trying compare bad action values `~s` and `~s`, ignoring", [Action1, Action2]),
    'undefined';
compare_actions(?RELAXED, <<"pass">>, _) ->
    <<"pass">>;
compare_actions(?RELAXED, _, <<"pass">>) ->
    <<"pass">>;
compare_actions(?RELAXED, <<"ask_human">>, _) ->
    <<"ask_human">>;
compare_actions(?RELAXED, _, <<"ask_human">>) ->
    <<"ask_human">>;
compare_actions(?RELAXED, <<"skip_human">>, _) ->
    <<"skip_human">>;
compare_actions(?RELAXED, _, <<"skip_human">>) ->
    <<"skip_human">>;
compare_actions(?RELAXED, <<"block">>, _) ->
    <<"block">>;
compare_actions(?RELAXED, _, <<"block">>) ->
    <<"block">>;
compare_actions(?RELAXED, Action1, Action2) ->
    lager:info("trying compare bad action values `~s` and `~s`, ignoring", [Action1, Action2]),
    'undefined'.

-spec fetch_patterns(kz_term:ne_binary(), kz_term:api_ne_binary()) -> {'ok', kz_json:object()} | {'error', 'not_found'}.
fetch_patterns(AccountId, OwnerId) ->
    fetch_patterns(AccountId, OwnerId, #{}).

-spec fetch_patterns(kz_term:ne_binary(), kz_term:api_ne_binary(), map()) -> {'ok', kz_json:object()} | {'error', 'not_found'}.
fetch_patterns(AccountId, OwnerId, Options) ->
    Db = kz_util:format_account_db(AccountId),
    ViewOptions = get_view_options(?LIST_BY_OWNER, OwnerId, <<"pattern">>),
    case kz_datamgr:get_results(Db, ?LIST_BY_OWNER, ViewOptions) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', JObjs} -> format_view_results(JObjs, Options);
        {'error', _}=_E ->
            lager:error("error getting blacklist patterns for account ~s and owner ~s : ~p", [AccountId, OwnerId, _E]),
            {'error', 'not_found'}
    end.

-spec fetch_number(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> {'ok', kz_json:objects()} | {'error', 'not_found'}.
fetch_number(AccountId, OwnerId, Number) ->
    fetch_number(AccountId, OwnerId, Number, #{}).

-spec fetch_number(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary(), map()) -> {'ok', kz_json:objects()} | {'error', 'not_found'}.
fetch_number(AccountId, OwnerId, Number, Options) ->
    Db = kz_util:format_account_db(AccountId),
    ViewOptions = get_view_options(?LIST_BY_OWNER, OwnerId, <<"number">>, Number),
    case kz_datamgr:get_results(Db, ?LIST_BY_OWNER, ViewOptions) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', [JObj]} ->
            JObjs = kz_json:get_list_value(<<"value">>, JObj),
            case filter_results(JObjs, Options) of
                [] -> {'error', 'not_found'};
                Results -> {'ok', Results}
            end;
        {'error', _}=_E ->
            lager:error("error getting blacklists by number ~s for account ~s and owner ~s : ~p", [Number, AccountId, OwnerId, _E]),
            {'error', 'not_found'}
    end.

-spec filter_results(kz_json:objects(), map()) -> kz_json:objects().
filter_results(JObjs, Options) ->
    Routines = get_view_filters(Options),
    lists:foldr(fun(F, Item) -> F(Item) end
               ,JObjs
               ,Routines
               ).

-spec get_view_options(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
get_view_options(?LIST_BY_OWNER, 'undefined', <<"pattern">> = Type) ->
    props:filter_undefined(
      [{'group', 'true'}
      ,{'group_level', 3}
      ,{'startkey', ['null', Type]}
      ,{'endkey', ['null', Type, kz_json:new()]}
      ]);
get_view_options(?LIST_BY_OWNER, OwnerId, <<"pattern">> = Type) ->
    props:filter_undefined(
      [{'group', 'true'}
      ,{'group_level', 3}
      ,{'startkey', [OwnerId, Type]}
      ,{'endkey', [OwnerId, Type, kz_json:new()]}
      ]).

-spec get_view_options(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:proplist().
get_view_options(?LIST_BY_OWNER, 'undefined', <<"number">> = Type, Number) ->
    props:filter_undefined(
      [{'group', 'true'}
      ,{'group_level', 3}
      ,{'key', ['null', Type, Number]}
      ]);
get_view_options(?LIST_BY_OWNER, OwnerId, <<"number">> = Type, Number) ->
    props:filter_undefined(
      [{'group', 'true'}
      ,{'group_level', 3}
      ,{'key', [OwnerId, Type, Number]}
      ]).

-spec format_view_results(kz_json:objects(), map()) -> {'ok', kz_json:object()} | {'error', 'not_found'}.
format_view_results(JObjs, Options) ->
    format_view_results(JObjs, kz_json:new(), Options).

-spec format_view_results(kz_json:objects(), kz_json:object(), map()) -> {'ok', kz_json:object()} | {'error', 'not_found'}.
format_view_results([], Acc, _Options) ->
    case Acc == kz_json:new() of
        'true' -> {'error', 'not_found'};
        'false' -> {'ok', Acc}
    end;
format_view_results([JObj | JObjs], Acc, Options) ->
    [_Owner, _Type, Key] = kz_json:get_value(<<"key">>, JObj),
    Value = kz_json:get_list_value(<<"value">>, JObj),
    case filter_results(Value, Options) of
        [] -> format_view_results(JObjs, Acc, Options);
        Results ->
            Acc1 = kz_json:insert_value(Key, Results, Acc),
            format_view_results(JObjs, Acc1, Options)
    end.


-spec get_view_filters(map()) -> [fun()].
get_view_filters(Options) ->
    get_view_filters(Options, []).

-spec get_view_filters(map(), list()) -> [fun()].
get_view_filters(#{<<"enabled">> := 'true'} = Options, Acc) ->
    Fun = fun(List) -> lists:filter(fun(JObj) -> kz_json:is_true(<<"enabled">>, JObj, 'true') end, List) end,
    get_view_filters(maps:remove(<<"enabled">>, Options), [Fun | Acc]);
get_view_filters(#{<<"enabled">> := 'false'} = Options, Acc) ->
    Fun = fun(List) -> lists:filter(fun(JObj) -> kz_json:is_false(<<"enabled">>, JObj, 'false') end, List) end,
    get_view_filters(maps:remove(<<"enabled">>, Options), [Fun | Acc]);
get_view_filters(#{<<"strategy">> := ?STRICT} = Options, Acc) ->
    Fun = fun(List) -> filter_by_strategy(?STRICT, List) end,
    get_view_filters(maps:remove(<<"strategy">>, Options), [Fun | Acc]);
get_view_filters(#{<<"strategy">> := ?RELAXED} = Options, Acc) ->
    Fun = fun(List) -> filter_by_strategy(?RELAXED, List) end,
    get_view_filters(maps:remove(<<"strategy">>, Options), [Fun | Acc]);
get_view_filters(#{<<"brief">> := 'true'} = Options, Acc) ->
    %% Brief fucntions must be last
    Fun = fun(List) ->
                  lists:map(fun(JObj) ->
                                    PropList = [{<<"action">>, kz_json:get_ne_binary_value(<<"action">>, JObj, <<"block">>)}
                                               ,{<<"name">>, kz_json:get_ne_binary_value(<<"name">>, JObj)}
                                               ],
                                    kz_json:from_list(PropList)
                            end, List)
          end,
    get_view_filters(maps:remove(<<"brief">>, Options), [Fun | Acc]);
get_view_filters(#{}, Acc) ->
    Acc.

-spec filter_by_strategy(kz_term:ne_binary(), kz_json:objects()) -> kz_json:objects().
filter_by_strategy(Strategy, JObjs) ->
    filter_by_strategy(Strategy, JObjs, 'undefined').

-spec filter_by_strategy(kz_term:ne_binary(), kz_json:objects(), kz_term:api_object()) -> kz_json:objects().
filter_by_strategy(_Strategy, [], Acc) ->
    case kz_json:get_ne_binary_value(<<"action">>, Acc, <<"block">>) of
        Action when ?IS_BLACKLIST_VALUE(Action) -> [Acc];
        Value ->
            lager:error("ignoring not expected blacklist action '~s', blacklist info ~p", [Value, Acc]),
            kz_json:delete_key(<<"action">>, Acc)
    end;
filter_by_strategy(Strategy, [JObj|JObjs], 'undefined') ->
    filter_by_strategy(Strategy, JObjs, JObj);
filter_by_strategy(Strategy, [JObj|JObjs], Acc) ->
    NewAcc = compare_blacklists(Strategy, JObj, Acc),
    filter_by_strategy(Strategy, JObjs, NewAcc).

-spec compare_blacklists(kz_term:ne_binary(), kz_json:object(), kz_term:api_objects()) -> kz_json:object().
compare_blacklists(Strategy, JObj, Acc) ->
    JObjAction = kz_json:get_ne_binary_value(<<"action">>, JObj, <<"block">>),
    AccAction = kz_json:get_ne_binary_value(<<"action">>, Acc, <<"block">>),
    case compare_actions(Strategy, JObjAction, AccAction) of
        JObjAction -> JObj;
        AccAction -> Acc;
        _ ->
            lager:error("skiping not expected blacklist action '~s', blacklist info ~p", [JObjAction, JObj]),
            Acc
    end.

-spec action(doc()) -> binary().
action(Doc) ->
    action(Doc, <<"block">>).

-spec action(doc(), Default) -> binary() | Default.
action(Doc, Default) ->
    kz_json:get_binary_value([<<"action">>], Doc, Default).

-spec set_action(doc(), binary()) -> doc().
set_action(Doc, Action) ->
    kz_json:set_value([<<"action">>], Action, Doc).

-spec enabled(doc()) -> boolean().
enabled(Doc) ->
    enabled(Doc, true).

-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"enabled">>], Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value([<<"enabled">>], Enabled, Doc).

-spec flags(doc()) -> kz_term:api_ne_binaries().
flags(Doc) ->
    flags(Doc, 'undefined').

-spec flags(doc(), Default) -> kz_term:ne_binaries() | Default.
flags(Doc, Default) ->
    kz_json:get_list_value([<<"flags">>], Doc, Default).

-spec set_flags(doc(), kz_term:ne_binaries()) -> doc().
set_flags(Doc, Flags) ->
    kz_json:set_value([<<"flags">>], Flags, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec numbers(doc()) -> kz_json:object().
numbers(Doc) ->
    numbers(Doc, kz_json:new()).

-spec numbers(doc(), Default) -> kz_json:object() | Default.
numbers(Doc, Default) ->
    kz_json:get_json_value([<<"numbers">>], Doc, Default).

-spec set_numbers(doc(), kz_json:object()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value([<<"numbers">>], Numbers, Doc).

-spec format_numbers(kz_term:api_object()) -> kz_term:api_object().
format_numbers('undefined') ->
    'undefined';
format_numbers(Doc) ->
    case is_blacklist(Doc) of
        'true' ->
            Numbers = kz_json:get_value(<<"numbers">>, Doc),
            format_numbers(Numbers);
        'false' -> kz_json:map(fun format_number_map/2, Doc)
    end.

-spec format_number_map(kz_term:ne_binary(), kz_json:object()) ->
                               {kz_term:ne_binary(), kz_json:object()}.
format_number_map(Number, Data) ->
    {knm_converters:normalize(Number), Data}.

-spec numbers_name(doc()) -> kz_term:api_binary().
numbers_name(Doc) ->
    numbers_name(Doc, 'undefined').

-spec numbers_name(doc(), Default) -> binary() | Default.
numbers_name(Doc, Default) ->
    kz_json:get_binary_value([<<"numbers">>, <<"name">>], Doc, Default).

-spec set_numbers_name(doc(), binary()) -> doc().
set_numbers_name(Doc, NumbersName) ->
    kz_json:set_value([<<"numbers">>, <<"name">>], NumbersName, Doc).

-spec owner_id(doc()) -> kz_term:api_ne_binary().
owner_id(Doc) ->
    owner_id(Doc, 'undefined').

-spec owner_id(doc(), Default) -> kz_term:ne_binary() | Default.
owner_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"owner_id">>], Doc, Default).

-spec set_owner_id(doc(), kz_term:api_ne_binary()) -> doc().
set_owner_id(Doc, 'undefined') ->
    kz_json:delete_key([<<"owner_id">>], Doc);
set_owner_id(Doc, OwnerId) ->
    kz_json:set_value([<<"owner_id">>], OwnerId, Doc).

-spec set_owner_id(doc(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> doc().
set_owner_id(Doc, AccountId, AccountId) ->
    set_owner_id(Doc, 'undefined', AccountId);
set_owner_id(Doc, OwnerId, AccountId) ->
    case is_valid_owner_id(OwnerId, AccountId) of
        'true' -> set_owner_id(Doc, OwnerId);
        'false' ->
            lager:notice("provided document id -s of account ~s have type not account, user or device", [OwnerId, AccountId]),
            set_owner_id(Doc, 'undefined')
    end.

-spec is_valid_owner_id(kz_term:api_ne_binary(), kz_term:ne_binary()) -> boolean().
is_valid_owner_id('undefined', AccountId) ->
    %% If owner not defined, then blacklist is owned by account
    kzd_accounts:is_account(AccountId);
is_valid_owner_id(AccountId, AccountId) ->
    kzd_accounts:is_account(AccountId);
is_valid_owner_id(OwnerId, AccountId) ->
    kzd_accounts:is_account(AccountId)
        andalso (kzd_users:is_user(AccountId, OwnerId)
                 orelse kzd_devices:is_device(AccountId, OwnerId)).

-spec patterns(doc()) -> kz_json:object().
patterns(Doc) ->
    patterns(Doc, kz_json:new()).

-spec patterns(doc(), Default) -> kz_json:object() | Default.
patterns(Doc, Default) ->
    kz_json:get_json_value([<<"patterns">>], Doc, Default).

-spec set_patterns(doc(), kz_json:object()) -> doc().
set_patterns(Doc, Patterns) ->
    kz_json:set_value([<<"patterns">>], Patterns, Doc).

-spec patterns_name(doc()) -> kz_term:api_binary().
patterns_name(Doc) ->
    patterns_name(Doc, 'undefined').

-spec patterns_name(doc(), Default) -> binary() | Default.
patterns_name(Doc, Default) ->
    kz_json:get_binary_value([<<"patterns">>, <<"name">>], Doc, Default).

-spec set_patterns_name(doc(), binary()) -> doc().
set_patterns_name(Doc, PatternsName) ->
    kz_json:set_value([<<"patterns">>, <<"name">>], PatternsName, Doc).

-spec is_blacklist(kz_term:api_object()) -> boolean().
is_blacklist('undefined') -> 'false';
is_blacklist(Doc) ->
    kz_doc:type(Doc) =:= type().

-spec should_block_anonymous(doc()) -> kz_term:api_boolean().
should_block_anonymous(Doc) ->
    should_block_anonymous(Doc, 'undefined').

-spec should_block_anonymous(doc(), Default) -> boolean() | Default.
should_block_anonymous(Doc, Default) ->
    kz_json:get_boolean_value([<<"should_block_anonymous">>], Doc, Default).

-spec set_should_block_anonymous(doc(), boolean()) -> doc().
set_should_block_anonymous(Doc, ShouldBlockAnonymous) ->
    kz_json:set_value([<<"should_block_anonymous">>], ShouldBlockAnonymous, Doc).
