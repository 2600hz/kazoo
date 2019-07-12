%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_cdrs).

%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ,cleanup/2
        ]).

%% Appliers
-export([dump/2]).

-include("tasks.hrl").

-define(MOD_CAT, <<(?CONFIG_CAT)/binary, ".cdrs">>).
-define(DB_DUMP_BULK_SIZE
       ,kapps_config:get_integer(?MOD_CAT, <<"db_page_size">>, 1000)
       ).

-define(CATEGORY, "billing").
-define(ACTIONS, [<<"dump">>]).

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

-spec output_header(kz_term:ne_binary()) -> kz_term:ne_binaries().
output_header(<<"dump">>) ->
    [K || {K, _} <- kzd_cdrs:csv_headers('false')].

-spec dump(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
dump(#{'account_id' := AccountId
      ,'req_data' := ReqData
      }=_ExtraArgs
    ,'init'
    ) ->
    FromS = kz_json:get_integer_value(<<"from_s">>, ReqData),
    ToS = kz_json:get_integer_value(<<"to_s">>, ReqData),

    MODBs = kazoo_modb:get_range(AccountId, FromS, ToS),

    lager:info("starting dump of ~s: ~p", [AccountId,_ExtraArgs]),

    process_rows(MODBs, [{'startkey', [ToS]}, {'endkey', [FromS]}], 'undefined');
dump(#{}=_ExtraArgs, {[_MODB], _BaseOptions, 'undefined'}) ->
    lager:info("finished dumping CDRs"),
    'stop';
dump(#{}=_ExtraArgs, {[MODB | MODBs], BaseOptions, 'undefined'}) ->
    lager:info("finished with ~s", [MODB]),
    process_rows(MODBs, BaseOptions, 'undefined');
dump(#{}=_ExtraArgs, {MODBs, BaseOptions, NextStartKey}) ->
    process_rows(MODBs, BaseOptions, NextStartKey).

-spec process_rows(kz_term:ne_binaries(), kz_datamgr:view_options(), kz_json:api_json_term()) ->
                          {[iolist()] | kz_datamgr:data_error()
                          ,{kz_term:ne_binary(), kz_datamgr:view_options(), kz_json:api_json_term()}
                          }.
process_rows([MODB | _]=MODBs, BaseOptions, 'undefined') ->
    case get_page(MODB, BaseOptions, 'undefined') of
        {'ok', [], NextStartKey} ->
            lager:info("no rows found, continuing with next:~p", [NextStartKey]),
            {'ok', {MODBs, BaseOptions, NextStartKey}};
        {'ok', Rows, NextStartKey} ->
            lager:info("got ~p rows (next:~s) from ~s", [length(Rows), NextStartKey, MODB]),
            CDRRows = [kzd_cdrs:to_public_csv(CDR) || CDR <- rows_to_cdrs(Rows)],
            {CDRRows, {MODBs, BaseOptions, NextStartKey}};
        {'error', E} ->
            lager:error("error getting first page: ~p", [E]),
            {E, []}
    end;
process_rows([MODB |_]=MODBs, BaseOptions, StartKey) ->
    case get_page(MODB, BaseOptions, StartKey) of
        {'ok', [], NextStartKey} ->
            lager:info("no rows found, continuing with next:~p", [NextStartKey]),
            {'ok', {MODBs, BaseOptions, NextStartKey}};
        {'ok', Rows, NextStartKey} ->
            lager:info("got ~p rows using ~s (next:~p) from ~s", [length(Rows), StartKey, NextStartKey, MODB]),
            {[kzd_cdrs:to_public_csv(CDR) || CDR <- rows_to_cdrs(Rows)], {MODBs, BaseOptions, NextStartKey}};
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
