%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_cdrs).
-behavior(gen_task).

%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3

        ,output_header/2
        ,execute/3

        ,cleanup/2
        ]).

-include("tasks.hrl").

-define(MOD_CAT, <<(?CONFIG_CAT)/binary, ".cdrs">>).
-define(DB_DUMP_BULK_SIZE
       ,kapps_config:get_integer(?MOD_CAT, <<"db_page_size">>, 1000)
       ).

-define(CATEGORY, "billing").
-define(ACTIONS, [<<"dump">>]).

-record(dump_args, {base_options :: kz_datamgr:view_options()
                   ,modbs = [] :: kz_term:ne_binaries()
                   ,next_start_key = 'undefined' :: kz_json:api_json_term()

                   ,is_reseller = 'false' :: boolean()
                   ,store_csv = 'false' :: boolean()
                   }).
-type dump_args() :: #dump_args{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".cleanup">>, ?MODULE, 'cleanup'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec cleanup(kz_term:ne_binary(), any()) -> any().
cleanup(?NE_BINARY, _) -> 'ok'.

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_map(action(Action)), JObj).

-spec action(kz_term:ne_binary()) -> map().
action(<<"dump">>) ->
    #{<<"description">> => <<"Create CDR CSV with all CDRs from the requested timeframe">>
     ,<<"doc">> => <<"Retrieving large CDR CSVs via API can be problematic, and finding the right time"
                     " frame to get successful pages is a moving target. This task allows KAZOO to build"
                     " a CSV of a requested timeframe in the background and can notify the client when"
                     " the task has completed instead."
                   >>
     ,<<"expected_content">> => 'undefined' % currently no input, eventually support custom time frames
     }.

%%% Appliers

-spec output_header(kz_term:ne_binary(), kz_tasks:extra_args()) -> kz_term:ne_binaries().
output_header(<<"dump">>, #{req_data := ReqData}) ->
    [K || {K, _} <- kzd_cdrs:csv_headers(kz_json:is_true(<<"is_reseller">>, ReqData, 'false'))].

-spec execute(kz_term:ne_binary(), kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
execute(<<"dump">>, ExtraArgs, Iterator) ->
    dump(ExtraArgs, Iterator).

-spec dump(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
dump(#{'account_id' := AccountId
      ,'req_data' := ReqData
      }=_ExtraArgs
    ,'init'
    ) ->
    FromS = kz_json:get_integer_value(<<"from_s">>, ReqData),
    ToS = kz_json:get_integer_value(<<"to_s">>, ReqData),
    IsReseller = kz_json:is_true(<<"is_reseller">>, ReqData, 'false'),
    ShouldStoreCSV = kz_json:is_true(<<"store_csv">>, ReqData, 'false'),

    MODBs = kazoo_modb:get_range(AccountId, FromS, ToS),
    BaseOptions = [{'startkey', [ToS]}, {'endkey', [FromS]}
                  ,{'page_size', page_size()}
                  ],

    DumpArgs = #dump_args{base_options=BaseOptions
                         ,modbs=MODBs
                         ,next_start_key='undefined'
                         ,is_reseller=IsReseller
                         ,store_csv=ShouldStoreCSV
                         },

    lager:info("starting dump of ~s: ~p", [AccountId, DumpArgs]),

    process_rows(DumpArgs);
dump(#{}=_ExtraArgs, #dump_args{modbs=[_MODB], next_start_key='undefined'}) ->
    lager:info("finished dumping CDRs"),
    'stop';
dump(#{}=_ExtraArgs, #dump_args{modbs=[MODB | MODBs], next_start_key='undefined'}=DumpArgs) ->
    lager:info("finished with ~s", [MODB]),
    process_rows(DumpArgs#dump_args{modbs=MODBs});
dump(#{}=_ExtraArgs, #dump_args{}=DumpArgs) ->
    process_rows(DumpArgs).

-spec process_rows(dump_args()) ->
                          {[iolist()] | kz_datamgr:data_error()
                          ,dump_args()
                          }.
process_rows(#dump_args{modbs=[MODB | _]
                       ,base_options=BaseOptions
                       ,next_start_key=StartKey
                       ,is_reseller=IsReseller
                       }=DumpArgs
            ) ->
    case get_page(MODB, BaseOptions, StartKey) of
        {'ok', [], NextStartKey} ->
            lager:info("no rows found, continuing with next:~p", [NextStartKey]),
            {'ok', DumpArgs#dump_args{next_start_key=NextStartKey}};
        {'ok', Rows, NextStartKey} ->
            lager:info("got ~p rows using ~p (next:~p) from ~s", [length(Rows), StartKey, NextStartKey, MODB]),
            CDRRows = [kzd_cdrs:to_public_csv(CDR, IsReseller) || CDR <- rows_to_cdrs(Rows)],
            {CDRRows, DumpArgs#dump_args{next_start_key=NextStartKey}};
        {'error', E} ->
            lager:error("error getting page from ~p: ~p", [E, StartKey]),
            {E, []}
    end.

rows_to_cdrs(Rows) ->
    [kz_json:get_json_value(<<"doc">>, Row)
     || Row <- Rows
    ].

-spec get_page(kz_term:ne_biary(), kz_datamgr:view_options(), kz_json:api_json_term()) -> kz_datamgr:paginated_results().
get_page(MODB, BaseOptions, 'undefined') ->
    query(MODB, BaseOptions);
get_page(MODB, BaseOptions, NextStartKey) ->
    query(MODB, props:set_value('startkey', NextStartKey, BaseOptions)).

-spec query(kz_term:ne_binary(), kz_datamgr:view_options()) -> kz_datamgr:paginated_results().
query(MODB, ViewOptions) ->
    Options = ['include_docs'
              ,'descending'
               | ViewOptions
              ],
    lager:info("kz_datamgr:paginate_results(~p, ~p, ~p)."
              ,[MODB, <<"cdrs/crossbar_listing">>, Options]
              ),
    kz_datamgr:paginate_results(MODB
                               ,<<"cdrs/crossbar_listing">>
                               ,Options
                               ).

-spec page_size() -> pos_integer().
page_size() ->
    page_size(kapps_config:get_integer(?MOD_CAT, <<"dump_page_size">>)).

-spec page_size(kz_term:api_pos_integer()) -> pos_integer().
page_size('undefined') -> kz_datamgr:max_bulk_read();
page_size(N) when is_integer(N), N > 0 -> N.
