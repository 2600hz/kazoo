%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Sergey Korobkov
%%%-------------------------------------------------------------------
-module(kt_resource_selectors).
%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,cleanup/2
        ]).

%% Verifiers
-export([account_id/1
        ]).

%% Appliers
-export([import/8
        ,delete/5
        ]).

-include("tasks.hrl").

-define(CATEGORY, "resource_selectors").
-define(ACTIONS, [<<"import">>
                 ,<<"delete">>
                 ]).

-record(state, {db :: api_binary()
               ,auth_account_id :: api_binary()
               ,bulk_limit = 500 :: non_neg_integer()
               ,processed_rows = 0 :: non_neg_integer()
               ,acc = [] :: api_list()
               }).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".account_id">>, ?MODULE, 'account_id'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".cleanup">>, ?MODULE, 'cleanup'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), ne_binary(), ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_list(action(Action)), JObj).

-spec action(ne_binary()) -> kz_proplist().
action(<<"import">>) ->
    [{<<"description">>, <<"Import stepswitch resource selectors">>}
    ,{<<"doc">>, <<"Creates stepswitch resource selectors from CSV file.">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, [<<"name">>
                       ,<<"selector">>
                       ,<<"resource">>
                       ]}
    ,{<<"optional">>, [<<"value">>
                      ,<<"start_time">>
                      ,<<"stop_time">>
                      ]}
    ];
action(<<"delete">>) ->
    [{<<"description">>, <<"Delete selectors from DB">>}
    ,{<<"doc">>, <<"Delete selectors from DB">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, [<<"name">>
                       ,<<"selector">>
                       ,<<"resource">>
                       ]}
    ,{<<"optional">>, []}
    ].

%%% Verifiers
-spec account_id(ne_binary()) -> boolean().
account_id(?MATCH_ACCOUNT_RAW(_)) -> 'true';
account_id(_) -> 'false'.

%%% Appliers

-spec import(kz_proplist(), task_iterator(), api_binary(), api_binary(),
             api_binary(), api_binary(), api_binary(), api_binary()) -> task_return().
import(Props, 'init', Name, Selector, Resource, Value, Start, Stop) ->
    kz_datamgr:suppress_change_notice(),
    State = #state{db = get_selectors_db(Props)
                  ,auth_account_id = props:get_value('auth_account_id', Props)
                  },
    import(Props, State, Name, Selector, Resource, Value, Start, Stop);
import(_Props
      ,#state{bulk_limit=Limit
             ,acc=Docs
             ,processed_rows=ProcessedRows
             ,auth_account_id=AuthAccountId
             ,db=Db
             }=State
      ,Name, Selector, Resource, Value, Start, Stop
      ) ->
    Doc = generate_selector_doc(AuthAccountId, Resource, Name, Selector, Value, Start, Stop),
    Acc = case (ProcessedRows+1) rem Limit =:= 0 of
              'true' ->
                  do_insert(Db, [Doc | Docs]),
                  [];
              'false' -> [Doc | Docs]
          end,
    {[], State#state{acc=Acc, processed_rows=ProcessedRows+1}};
import(_Props, State, _Name, _Selector, _Resource, _Value, _Start, _Stop) ->
    lager:error("wrong state: ~p",[State]),
    'stop'.

-spec delete(kz_proplists(), task_iterator(), api_binary(), api_binary(), api_binary()) -> task_return().
delete(Props, 'init', Name, Selector, Resource) ->
    kz_datamgr:suppress_change_notice(),
    State = #state{db = get_selectors_db(Props)
                   %% ,bulk_limit = kz_datamgr:max_bulk_insert()
                  },
    delete(Props, State, Name, Selector, Resource);
delete(_Props
      ,#state{bulk_limit=Limit
             ,acc=Keys
             ,processed_rows=ProcessedRows
             ,db=Db
             }=State
      ,Name
      ,Selector
      ,Resource
      ) ->
    Key = [Resource, Name, Selector],
    Acc = case (ProcessedRows+1) rem Limit =:= 0 of
              'true' ->
                  do_delete(Db, [Key | Keys]),
                  [];
              'false' -> [Key | Keys]
          end,
    {[], State#state{acc=Acc, processed_rows=ProcessedRows+1}};
delete(_Props, State, _Name, _Selector, _Resource) ->
    lager:error("wrong state: ~p",[State]),
    'stop'.

-spec cleanup(ne_binary(), any()) -> any().
cleanup(<<"import">>, #state{db=Db, acc=Docs}) ->
    do_insert(Db, Docs),
    kz_datamgr:enable_change_notice();
cleanup(<<"delete">>, #state{db=Db, acc=Keys}) ->
    do_delete(Db, Keys),
    kz_datamgr:enable_change_notice().

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec do_insert(api_binary(), kz_json:objects()) -> 'ok'.
do_insert(_Db, []) -> 'ok';
do_insert(Db, Docs) ->
    case kz_datamgr:save_docs(Db, Docs) of
        {'ok', _Result} -> refresh_selectors_index(Db);
        {'error', 'not_found'} ->
            init_db(Db),
            do_insert(Db, Docs);
        {'error', E} -> lager:error("error adding selectors: ~p",[E])
    end.

-spec do_delete(api_binary(), api_list()) -> 'ok'.
do_delete(_Db, []) -> 'ok';
do_delete(Db, DelKeys) ->
    BulkLimit = kz_datamgr:max_bulk_insert(),
    DelKeysBlocks = split_keys(DelKeys, BulkLimit),
    DelIDs = lists:foldl(fun(Keys, AccIDs) ->
                                 Options = [{'keys', Keys}, 'include_docs'],
                                 case kz_datamgr:get_results(Db, <<"resource_selectors/resource_name_selector_listing">>, Options) of
                                     {'ok', Result} ->
                                         lists:foldl(fun(R, Acc) ->
                                                             ID = kz_json:get_ne_value(<<"id">>, R, []),
                                                             Rev = kz_json:get_ne_value([<<"doc">>,<<"_rev">>], R, []),
                                                             JObj = kz_json:from_list([{<<"_id">>, ID}, {<<"_rev">>, Rev}]),
                                                             [ JObj | Acc ]
                                                     end
                                                    ,AccIDs
                                                    ,Result
                                                    );
                                     {'error', E} ->
                                         lager:error("E: ~p",[E]),
                                         AccIDs
                                 end
                         end
                        ,[]
                        ,DelKeysBlocks
                        ),
    DelIDsBlocks = split_keys(DelIDs, BulkLimit),
    lists:foreach(fun(Block) ->
                          _Result = kz_datamgr:del_docs(Db, Block),
                          refresh_selectors_index(Db)
                  end
                 ,DelIDsBlocks
                 ).

-spec get_selectors_db(kz_proplists()) -> api_binary().
get_selectors_db(Props) ->
    AccountId = props:get_value('account_id', Props),
    AuthAccountId = props:get_value('auth_account_id', Props),
    'true' =  kz_util:is_in_account_hierarchy(AuthAccountId, AccountId, 'true'),
    kz_util:format_resource_selectors_db(AccountId).

-spec split_keys(ne_binaries(), non_neg_integer()) -> [ne_binaries()].
-spec split_keys(ne_binaries(), [ne_binaries()], non_neg_integer()) -> [ne_binaries()].
split_keys(Keys, BlockSize) -> split_keys(Keys, [], BlockSize).
split_keys([], Acc, _BlockSize) -> Acc;
split_keys(Keys, Acc, BlockSize) when length(Keys) =< BlockSize -> [Keys ++ Acc];
split_keys(Keys, Acc, BlockSize) ->
    {Block, Rest} = lists:split(BlockSize, Keys),
    split_keys(Rest, [Block | Acc], BlockSize).

-spec refresh_selectors_index(ne_binary()) -> 'ok'.
refresh_selectors_index(Db) ->
    {'ok', _} = kz_datamgr:all_docs(Db, [{limit, 1}]),
    {'ok', _} = kz_datamgr:get_results(Db, <<"resource_selectors/resource_name_selector_listing">>, [{'limit', 1}]),
    'ok'.

generate_selector_doc(AuthAccountId, Resource, Name, Selector, Value, Start, Stop) ->
    Props = [{<<"pvt_type">>, <<"resource_selector">>}
            ,{<<"name">>, Name}
            ,{<<"selector">>, Selector}
            ,{<<"resource">>, Resource}
            ,{<<"value">>, Value}
            ,{<<"start_time">>, Start}
            ,{<<"stop_time">>, Stop}
            ,{<<"pvt_auth_account_id">>, AuthAccountId}
            ,{<<"pvt_created">>, kz_util:current_tstamp()}
            ],
    kz_json:from_list(props:filter_undefined(Props)).

init_db(Db) when is_binary(Db) ->
    _ = kz_datamgr:db_create(Db),
    {'ok', _} = kz_datamgr:revise_doc_from_file(Db, 'crossbar', "views/resource_selectors.json"),
    'ok'.
