%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_services_tasks).
%% behaviour: tasks_provider

-export([help/0
        ,category/0
        ,module/0
        ,output_header/1
        ]).

%% Verifiers
-export([
        ]).

%% Appliers
-export([descendant_quantities/2
        ]).

-include("kazoo_services.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec category() -> ne_binary().
category() -> <<"services">>.

-spec module() -> module().
module() -> kz_util:to_binary(?MODULE).

-spec output_header(atom()) -> kz_csv:row().
output_header('descendant_quantities') ->
    [<<"account_id">>
    ,<<"month">>
    ,<<"category">>
    ,<<"item">>
    ,<<"quantity_bom">>
    ,<<"quantity_eom">>
    ].

-spec help() -> kz_proplist().
help() ->
    [{<<"descendant_quantities">>
     ,kz_json:from_list([{<<"description">>, <<"List per-month descendant accounts quantities">>}
                        ])
     }
    ].

%%% Verifiers


%%% Appliers

-spec descendant_quantities(kz_proplist(), task_iterator()) -> task_iterator().
descendant_quantities(Props, 'init') ->
    Descendants = get_descendants(props:get_value('auth_account_id', Props)),
    DescendantsMoDBs = lists:flatmap(fun kapps_util:get_account_mods/1, Descendants),
    lager:debug("found ~p descendants & ~p MoDBs in total"
               ,[length(Descendants), length(DescendantsMoDBs)]),
    {'ok', DescendantsMoDBs};

descendant_quantities(_, []) ->
    'stop';

descendant_quantities(_, [SubAccountMoDB | DescendantsMoDBs]) ->
    ?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, Year, Month) = SubAccountMoDB,
    AccountId = ?MATCH_ACCOUNT_RAW(A, B, Rest),
    YYYYMM = <<Year/binary, Month/binary>>,
    BoM = modb_service_quantities(SubAccountMoDB, 'true'),
    EoM = modb_service_quantities(SubAccountMoDB, 'false'),
    Data =
        [
         [
          [
           [AccountId
           ,YYYYMM
           ,Category
           ,Item
           ,maybe_integer_to_binary(Item, BoMItem)
           ,maybe_integer_to_binary(Item, EoMItem)
           ]
          ]
          || Item <- fields(BoMItem, EoMItem)
         ]
         || Category <- fields(BoM, EoM),
            BoMItem <- [kz_json:get_value(Category, BoM)],
            EoMItem <- [kz_json:get_value(Category, EoM)],
            BoMItem =/= 'undefined' andalso EoMItem =/= 'undefined'
        ],
    case lists:append(lists:append(Data)) of
        [] ->
            %% No rows generated: ask worker to skip writing for this step.
            {'ok', DescendantsMoDBs};
        Rows -> {Rows, DescendantsMoDBs}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

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

-spec modb_service_quantities(ne_binary(), boolean()) -> kz_json:object().
modb_service_quantities(MoDB, IsBoM) ->
    Id = case IsBoM of
             'true' -> <<"services_bom">>;
             'false' -> <<"services_eom">>
         end,
    case kz_datamgr:open_doc(MoDB, Id) of
        {'ok', JObj} -> kz_json:get_value(<<"quantities">>, JObj);
        {'error', _R} ->
            lager:debug("could not fetch ~s in modb ~s: ~p", [Id, MoDB, _R]),
            kz_json:new()
    end.

-spec fields(kz_json:object(), kz_json:object()) -> ne_binaries().
fields(JObjA, JObjB) ->
    lists:usort(kz_json:get_keys(JObjA) ++ kz_json:get_keys(JObjB)).

-spec maybe_integer_to_binary(ne_binary(), kz_json:object()) -> api_non_neg_integer().
maybe_integer_to_binary(Item, JObj) ->
    case kz_json:get_integer_value(Item, JObj) of
        'undefined' -> 'undefined';
        Quantity -> integer_to_binary(Quantity)
    end.

%%% End of Module.
