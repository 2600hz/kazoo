%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kt_services).
%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ]).

%% Verifiers
-export([
        ]).

%% Appliers
-export([descendant_quantities/2
        ]).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_services/include/kz_service.hrl").

-define(CATEGORY, "services").
-define(ACTIONS, [<<"descendant_quantities">>
                 ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec output_header(ne_binary()) -> kz_csv:row().
output_header(<<"descendant_quantities">>) ->
    [<<"account_id">>
    ,<<"year">>
    ,<<"month">>
    ,<<"category">>
    ,<<"item">>
    ,<<"quantity_bom">>
    ,<<"quantity_eom">>
    ].

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), ne_binary(), ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_list(action(Action)), JObj).

-spec action(ne_binary()) -> kz_proplist().
action(<<"descendant_quantities">>) ->
    [{<<"description">>, <<"List per-month descendant accounts quantities">>}
    ,{<<"doc">>, <<"Attempts to create a month-on-month listing of quantities used by descendant accounts.\n"
                   "This task returns the following fields:\n"
                   "* `account_id`: a sub-account of the creator of this task.\n"
                   "* `year`: integral year as 4 characters.\n"
                   "* `month`: integral month as 2 characters (left-padded with a zero).\n"
                   "* `category`: name of the quantity's category.\n"
                   "* `item`: name of the category's item.\n"
                   "* `quantity_bom`: integral quantity's value or empty.\n"
                   "* `quantity_eom`: integral quantity's value or empty.\n"
                   "Note: some beginning-of-month and end-of-month quantities documents may be missing.\n"
                   "Note: when both an account's BoM & EoM documents for a given month are missing, no rows are a created for this month.\n"
                   "Note: in all other cases the documents' value is printed verbatim: if unset the empty string is returned.\n"
                   "E.g.: an integer quantity (such as 1, 10 or 0 (zero)) represents was the system has. If no quantity was found, the empty value is used.\n"
                 >>}
    ].

%%% Verifiers


%%% Appliers

-spec descendant_quantities(kz_proplist(), task_iterator()) -> task_iterator().
descendant_quantities(Props, 'init') ->
    Descendants = get_descendants(props:get_value('account_id', Props)),
    DescendantsMoDBs = lists:flatmap(fun kapps_util:get_account_mods/1, Descendants),
    lager:debug("found ~p descendants & ~p MoDBs in total"
               ,[length(Descendants), length(DescendantsMoDBs)]),
    {'ok', DescendantsMoDBs};

descendant_quantities(_, []) ->
    'stop';

descendant_quantities(_, [SubAccountMoDB | DescendantsMoDBs]) ->
    ?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, YYYY, MM) = SubAccountMoDB,
    AccountId = ?MATCH_ACCOUNT_RAW(A, B, Rest),
    BoM = modb_service_quantities(SubAccountMoDB, ?SERVICES_BOM),
    EoM = modb_service_quantities(SubAccountMoDB, ?SERVICES_EOM),
    case rows_for_quantities(AccountId, YYYY, MM, BoM, EoM) of
        [] ->
            %% No rows generated: ask worker to skip writing for this step.
            {'ok', DescendantsMoDBs};
        Rows -> {Rows, DescendantsMoDBs}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec rows_for_quantities(ne_binary(), ne_binary(), ne_binary(), kz_json:object(), kz_json:object()) ->
                                 [kz_csv:row()].
rows_for_quantities(AccountId, YYYY, MM, BoM, EoM) ->
    lists:append(
      [quantities_for_items(AccountId, YYYY, MM, Category, BoMItem, EoMItem)
       || Category <- fields(BoM, EoM),
          BoMItem <- [kz_json:get_value(Category, BoM)],
          EoMItem <- [kz_json:get_value(Category, EoM)]
      ]).

-spec quantities_for_items(ne_binary(), ne_binary(), ne_binary(), ne_binary(), api_object(), api_object()) ->
                                  [kz_csv:row()].
quantities_for_items(AccountId, YYYY, MM, Category, BoMItem, EoMItem) ->
    [ [AccountId
      ,YYYY
      ,MM
      ,Category
      ,Item
      ,maybe_integer_to_binary(Item, BoMItem)
      ,maybe_integer_to_binary(Item, EoMItem)
      ]
      || Item <- fields(BoMItem, EoMItem)
    ].

-spec get_descendants(ne_binary()) -> ne_binaries().
get_descendants(AccountId) ->
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {'ok', JObjs} ->
            [Id || JObj <- JObjs,
                   (Id = kz_doc:id(JObj)) =/= AccountId
            ];
        {'error', _R} ->
            lager:debug("unable to get descendants of ~s: ~p", [AccountId, _R]),
            []
    end.

-spec modb_service_quantities(ne_binary(), ne_binary()) -> kz_json:object().
modb_service_quantities(MoDB, Id) ->
    case kz_datamgr:open_doc(MoDB, Id) of
        {'ok', JObj} -> kz_json:get_value(<<"quantities">>, JObj);
        {'error', _R} ->
            lager:debug("could not fetch ~s in modb ~s: ~p", [Id, MoDB, _R]),
            kz_json:new()
    end.

-spec fields(api_object(), api_object()) -> ne_binaries().
fields('undefined', JObjB) ->
    fields(kz_json:new(), JObjB);
fields(JObjA, 'undefined') ->
    fields(JObjA, kz_json:new());
fields(JObjA, JObjB) ->
    lists:usort(kz_json:get_keys(JObjA) ++ kz_json:get_keys(JObjB)).

-spec maybe_integer_to_binary(ne_binary(), api_object()) -> api_non_neg_integer().
maybe_integer_to_binary(_, 'undefined') -> 'undefined';
maybe_integer_to_binary(Item, JObj) ->
    case kz_json:get_integer_value(Item, JObj) of
        'undefined' -> 'undefined';
        Quantity -> integer_to_binary(Quantity)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

bom_1() ->
    kz_json:from_list(
      [{<<"branding">>, kz_json:from_list(
                          [{<<"whitelabel">>, 0}
                          ])
       }
      ,{<<"users">>, kz_json:new()}
      ,{<<"ui_apps">>, kz_json:new()}
      ,{<<"number_services">>, kz_json:new()}
      ,{<<"phone_numbers">>, kz_json:from_list(
                               [{<<"did_us">>, 1}
                               ])
       }
      ,{<<"ledgers">>, kz_json:new()}
      ,{<<"ips">>, kz_json:from_list(
                     [{<<"dedicated">>, 0}
                     ])
       }
      ,{<<"devices">>, kz_json:new()}
      ]).

bom_2() ->
    kz_json:from_list(
      [{<<"branding">>, kz_json:from_list(
                          [{<<"whitelabel">>, 0}
                          ])
       }
      ,{<<"users">>, kz_json:new()}
      ,{<<"ui_apps">>, kz_json:new()}
      ,{<<"number_services">>, kz_json:new()}
      ,{<<"phone_numbers">>, kz_json:new()}
      ,{<<"ledgers">>, kz_json:new()}
      ,{<<"ips">>, kz_json:from_list(
                     [{<<"dedicated">>, 0}
                     ])
       }
      ,{<<"devices">>, kz_json:new()}
      ]).

eom_1() ->
    kz_json:from_list(
      [{<<"branding">>, kz_json:from_list(
                          [{<<"whitelabel">>, 0}
                          ])
       }
      ,{<<"users">>, kz_json:new()}
      ,{<<"ui_apps">>, kz_json:new()}
      ,{<<"number_services">>, kz_json:from_list(
                                 [{<<"local">>, 130}
                                 ])
       }
      ,{<<"phone_numbers">>, kz_json:from_list(
                               [{<<"did_us">>, 1}
                               ])
       }
      ,{<<"ledgers">>, kz_json:new()}
      ,{<<"ips">>, kz_json:from_list(
                     [{<<"dedicated">>, 0}
                     ])
       }
      ,{<<"devices">>, kz_json:new()}
      ]).

rows_for_missing_eom_test() ->
    AccountId = <<"6b71cb72c876b5b1396a335f8f8a2594">>,
    <<YYYY:4/binary, MM:2/binary>> = <<"201504">>,
    Expected =
        [[AccountId, YYYY, MM, <<"branding">>, <<"whitelabel">>, <<"0">>, 'undefined']
        ,[AccountId, YYYY, MM, <<"ips">>, <<"dedicated">>, <<"0">>, 'undefined']
        ],
    ?assertEqual(Expected, rows_for_quantities(AccountId, YYYY, MM, bom_2(), kz_json:new())).

rows_for_missing_bom_test() ->
    AccountId = <<"6b71cb72c876b5b1396a335f8f8a2594">>,
    <<YYYY:4/binary, MM:2/binary>> = <<"201504">>,
    Expected =
        [[AccountId, YYYY, MM, <<"branding">>, <<"whitelabel">>, 'undefined', <<"0">>]
        ,[AccountId, YYYY, MM, <<"ips">>, <<"dedicated">>, 'undefined', <<"0">>]
        ],
    ?assertEqual(Expected, rows_for_quantities(AccountId, YYYY, MM, kz_json:new(), bom_2())).

rows_for_bom_and_eom_test() ->
    AccountId = <<"6b71cb72c876b5b1396a335f8f8a2594">>,
    <<YYYY:4/binary, MM:2/binary>> = <<"201606">>,
    Expected =
        [[AccountId, YYYY, MM, <<"branding">>, <<"whitelabel">>, <<"0">>, <<"0">>]
        ,[AccountId, YYYY, MM, <<"ips">>, <<"dedicated">>, <<"0">>, <<"0">>]
        ,[AccountId, YYYY, MM, <<"number_services">>, <<"local">>, 'undefined', <<"130">>]
        ,[AccountId, YYYY, MM, <<"phone_numbers">>, <<"did_us">>, <<"1">>, <<"1">>]
        ],
    ?assertEqual(Expected, rows_for_quantities(AccountId, YYYY, MM, bom_1(), eom_1())).

-endif.

%%% End of Module.
