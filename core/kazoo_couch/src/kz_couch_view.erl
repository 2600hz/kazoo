%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Util functions used by kazoo_couch
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_couch_view).


%% View-related
-export([design_compact/3
        ,design_info/3
        ,all_design_docs/2, all_design_docs/3
        ,get_results/4
        ,get_results_count/4
        ,all_docs/1, all_docs/2, all_docs/3
        ,show/5
        ]).

-include("kz_couch.hrl").

-type ddoc() :: 'all_docs' |
                kz_term:ne_binary() |
                {kz_term:ne_binary(), kz_term:ne_binary()}.

-type show_doc() :: binary() |
                    {binary(), binary()}.

%%% View-related functions -----------------------------------------------------
-spec design_compact(server(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
design_compact(#server{}=Conn, DbName, Design) ->
    case couchbeam:compact(kz_couch_util:get_db(Conn, DbName), Design) of
        {'error', _E} ->
            lager:info("failed to compact design doc ~s/~s: ~p", [DbName, Design, kz_couch_util:format_error(_E)]),
            'false';
        'ok' -> 'true'
    end.

-spec design_info(server(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          couchbeam_error().
design_info(#server{}=Conn, DBName, Design) ->
    Db = kz_couch_util:get_db(Conn, DBName),
    do_get_design_info(Db, Design).

-spec all_design_docs(kz_data:connection(), kz_term:ne_binary()) ->
          {'ok', kz_json:objects()} |
          couchbeam_error().
all_design_docs(#server{}=Conn, DBName) ->
    all_design_docs(#server{}=Conn, DBName, []).

-spec all_design_docs(kz_data:connection(), kz_term:ne_binary(), view_options()) ->
          {'ok', kz_json:objects()} |
          couchbeam_error().
all_design_docs(#server{}=Conn, DBName, Options) ->
    Db = kz_couch_util:get_db(Conn, DBName),
    Filter = props:set_values([{'startkey', <<"_design/">>}
                              ,{'endkey', <<"_design0">>}
                              ]
                             ,Options
                             ),
    do_fetch_results(Db, 'all_docs', Filter).

-spec all_docs(db()) -> {'ok', kz_json:objects()} | couchbeam_error().
all_docs(Db) -> all_docs(Db, []).

-spec all_docs(db(), view_options()) -> {'ok', kz_json:objects()} |
          couchbeam_error().
all_docs(#db{}=Db, Options) ->
    do_fetch_results(Db, 'all_docs', Options).

-spec all_docs(server(), kz_term:ne_binary(), view_options()) ->
          {'ok', kz_json:objects()} |
          couchbeam_error().
all_docs(#server{}=Conn, DbName, Options) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_fetch_results(Db, 'all_docs', Options).

-spec get_results(server(), kz_term:ne_binary(), ddoc(), view_options()) ->
          {'ok', kz_json:objects() | kz_json:path()} |
          couchbeam_error().
get_results(#server{}=Conn, DbName, DesignDoc, ViewOptions) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_fetch_results(Db, DesignDoc, ViewOptions).

%% This function assumes a "reduce" function that returns the count of docs exists
%% Need to see how to get couchbeam to return the "rows" property instead of the result
%% list; that would be better, but for not, setting the view's "reduce" to the _count
%% function will suffice (provided a reduce isn't already defined).
-spec get_results_count(server(), 'all_docs' | kz_term:ne_binary(), kz_term:ne_binary(), view_options()) ->
          {'ok', kz_term:api_integer()} |
          couchbeam_error().
get_results_count(#server{}=Conn, DbName, DesignDoc, ViewOptions) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_fetch_results_count(Db, DesignDoc, ViewOptions).

%% Design Doc/View internal functions
-spec do_fetch_results(couchbeam_db(), ddoc(), view_options()) ->
          {'ok', kz_json:objects() | kz_term:ne_binaries()} |
          couchbeam_error().
do_fetch_results(Db, <<DesignDoc/binary>>, Options) ->
    [DesignName, ViewName|_] = binary:split(DesignDoc, <<"/">>, ['global']),
    do_fetch_results(Db, {DesignName, ViewName}, map_options(Options));
do_fetch_results(Db, 'all_docs'=DesignDoc, Options) ->
    fetch_results(Db, DesignDoc, Options);
do_fetch_results(Db, {_DesignName, _ViewName}=DesignDoc, Options) ->
    fetch_results(Db, DesignDoc, Options).

fetch_results(Db, DesignDoc, Options) ->
    ?RETRY_504(
       case couchbeam_view:fetch(Db, DesignDoc, Options) of
           {'ok', JObj} -> {'ok', kz_json:get_value(<<"rows">>, JObj, JObj)};
           {'error', Error, []} -> {'error', Error};
           {'error', Error, Rows} ->
               lager:error("error ~p with results, ~p", [Error, Rows]),
               {'error', Error};
           {'error', _}=Error -> Error
       end
      ).

-spec map_options(view_options()) -> view_options().
map_options(Options) ->
    [map_view_option(O) || O <- Options].

-spec map_view_option(any()) -> any().
map_view_option({K, V})
  when is_binary(K) ->
    {kz_term:to_atom(K, 'true'), V};
map_view_option(KV) -> KV.

-spec do_fetch_results_count(couchbeam_db(), kz_term:ne_binary() | ddoc(), view_options()) ->
          {'ok', kz_term:api_integer()} |
          couchbeam_error().
do_fetch_results_count(Db, <<DesignDoc/binary>>, Options) ->
    [DesignName, ViewName | _] = binary:split(DesignDoc, <<"/">>, ['global']),
    do_fetch_results_count(Db, {DesignName, ViewName}, map_options(Options));
do_fetch_results_count(Db, DesignDoc, Options) ->
    ?RETRY_504(
       case couchbeam_view:count(Db, DesignDoc, Options) of
           {'error', E} -> {'error', kz_couch_util:format_error(E)};
           N -> {'ok', N}
       end
      ).

-spec do_get_design_info(couchbeam_db(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          couchbeam_error().
do_get_design_info(#db{}=Db, Design) ->
    ?RETRY_504(couchbeam:design_info(Db, Design)).

-spec show(server(), kz_term:ne_binary(), show_doc(), binary() | 'null', [{'query_string', binary()}]) ->
          {'ok', kz_json:object()} |
          couchbeam_error().
show(#server{}=Conn, <<DbName/binary>>, <<DesignDoc/binary>>, DocId, Options) ->
    [DesignName, ViewName|_] = binary:split(DesignDoc, <<"/">>, ['global']),
    show(Conn, DbName, {DesignName, ViewName}, DocId, Options);
show(#server{}=Conn, DbName, {_, _}=Design, DocId, Options) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    couchbeam_view:show(Db, Design, DocId, Options).
