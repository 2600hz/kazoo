%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%% Util functions used by kazoo_couch
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-----------------------------------------------------------------------------
-module(kz_couch_view).


%% View-related
-export([design_compact/3
         ,design_info/3
         ,all_design_docs/3
         ,get_results/4
         ,get_results_count/4
         ,all_docs/2, all_docs/3
        ]).

-include("kz_couch.hrl").

-type ddoc() :: atom() | ne_binary() | {ne_binary(), ne_binary()}.

%%% View-related functions -----------------------------------------------------
-spec design_compact(server(), ne_binary(), ne_binary()) -> boolean().
design_compact(#server{}=Conn, DbName, Design) ->
    case couchbeam:compact(kz_couch_util:get_db(Conn, DbName), Design) of
        {'error', _E} ->
            lager:debug("failed to compact design doc: ~p", [kz_couch_util:format_error(_E)]),
            'false';
        'ok' -> 'true'
    end.

-spec design_info(server(), ne_binary(), ne_binary()) ->
                         {'ok', kz_json:object()} |
                         couchbeam_error().
design_info(#server{}=Conn, DBName, Design) ->
    Db = kz_couch_util:get_db(Conn, DBName),
    do_get_design_info(Db, Design).

-spec all_design_docs(server(), ne_binary(), view_options()) ->
                             {'ok', kz_json:objects()} |
                             couchbeam_error().
all_design_docs(#server{}=Conn, DBName, Options) ->
    Db = kz_couch_util:get_db(Conn, DBName),
    Filter = [{'startkey', <<"_design/">>}
              ,{'endkey', <<"_design0">>}
              | Options
             ],
    do_fetch_results(Db, 'all_docs', Filter).

-spec all_docs(db(), view_options()) -> {'ok', kz_json:objects()} |
                                        couchbeam_error().
all_docs(#db{}=Db, Options) ->
    do_fetch_results(Db, 'all_docs', Options).

-spec all_docs(server(), ne_binary(), view_options()) ->
                      {'ok', kz_json:objects()} |
                      couchbeam_error().
all_docs(#server{}=Conn, DbName, Options) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_fetch_results(Db, 'all_docs', Options).

-spec get_results(server(), ne_binary(), ne_binary(), view_options()) ->
                         {'ok', kz_json:objects() | kz_json:keys()} |
                         couchbeam_error().
get_results(#server{}=Conn, DbName, DesignDoc, ViewOptions) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_fetch_results(Db, DesignDoc, ViewOptions).

%% This function assumes a "reduce" function that returns the count of docs exists
%% Need to see how to get couchbeam to return the "rows" property instead of the result
%% list; that would be better, but for not, setting the view's "reduce" to the _count
%% function will suffice (provided a reduce isn't already defined).
-spec get_results_count(server(), ne_binary(), ne_binary(), view_options()) ->
                               {'ok', integer()} |
                               couchbeam_error().
get_results_count(#server{}=Conn, DbName, DesignDoc, ViewOptions) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_fetch_results_count(Db, DesignDoc, ViewOptions).

%% Design Doc/View internal functions
-spec do_fetch_results(couchbeam_db(), ddoc(), view_options()) ->
                              {'ok', kz_json:objects() | ne_binaries()} |
                              couchbeam_error().
do_fetch_results(Db, DesignDoc, Options)
  when is_binary(DesignDoc) ->
    [DesignName, ViewName | _] = binary:split(DesignDoc, <<"/">>, ['global']),
    do_fetch_results(Db, {DesignName, ViewName}, map_options(Options));
do_fetch_results(Db, DesignDoc, Options) ->
    ?RETRY_504(
       case couchbeam_view:fetch(Db, DesignDoc, Options) of
           {'ok', JObj} -> {'ok', kz_json:get_value(<<"rows">>, JObj, JObj)};
           {'error', _T, E} -> {'error', kz_couch_util:format_error(E)};
           {'error', E} -> {'error', kz_couch_util:format_error(E)}
       end
      ).

-spec map_options(view_options()) -> view_options().
map_options(Options) ->
    lists:map(fun map_view_option/1, Options).

-spec map_view_option(term()) -> term().
map_view_option({K, V})
  when is_binary(K) ->
    {kz_util:to_atom(K, 'true'), V};
map_view_option(KV) -> KV.

-spec do_fetch_results_count(couchbeam_db(), ddoc(), view_options()) ->
                                    {'ok', api(integer())} |
                                    couchbeam_error().
do_fetch_results_count(Db, DesignDoc, Options)
  when is_binary(DesignDoc) ->
    [DesignName, ViewName | _] = binary:split(DesignDoc, <<"/">>, ['global']),
    do_fetch_results_count(Db, {DesignName, ViewName}, map_options(Options));
do_fetch_results_count(Db, DesignDoc, Options) ->
    ?RETRY_504(
       case couchbeam_view:count(Db, DesignDoc, Options) of
           {'error', E} -> {'error', kz_couch_util:format_error(E)};
           N -> {'ok', N}
       end
      ).

-spec do_get_design_info(couchbeam_db(), ne_binary()) ->
                                {'ok', kz_json:object()} |
                                couchbeam_error().
do_get_design_info(#db{}=Db, Design) ->
    ?RETRY_504(couchbeam:design_info(Db, Design)).
