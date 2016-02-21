%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_view).


%% View-related
-export([design_compact/3
         ,design_info/3
         ,all_design_docs/3
         ,get_results/4
         ,get_results_count/4
         ,all_docs/3
        ]).

-include("kz_data.hrl").

%%% View-related functions -----------------------------------------------------
-spec design_compact(server(), ne_binary(), ne_binary()) -> boolean().
design_compact({App, Conn}, DbName, Design) -> App:design_compact(Conn, DbName, Design).

-spec design_info(server(), ne_binary(), ne_binary()) -> {'ok', wh_json:object()} | data_error().
design_info({App, Conn}, DBName, Design) -> App:design_info(Conn, DBName, Design).

-spec all_design_docs(server(), ne_binary(), view_options()) ->
                             {'ok', wh_json:objects()} |
                             data_error().
all_design_docs({App, Conn}, DBName, Options) -> App:all_design_docs(Conn, DBName, Options).

-spec all_docs(server(), ne_binary(), view_options()) ->
                      {'ok', wh_json:objects()} |
                      data_error().
all_docs({App, Conn}, DbName, Options) -> App:all_docs(Conn, DbName, Options).

-spec get_results(server(), ne_binary(), ne_binary(), view_options()) ->
                         {'ok', wh_json:objects() | wh_json:keys()} |
                         data_error().
get_results({App, Conn}, DbName, DesignDoc, ViewOptions) ->
    App:get_results(Conn, DbName, DesignDoc, ViewOptions).

%% This function assumes a "reduce" function that returns the count of docs exists
%% Need to see how to get couchbeam to return the "rows" property instead of the result
%% list; that would be better, but for not, setting the view's "reduce" to the _count
%% function will suffice (provided a reduce isn't already defined).
-spec get_results_count(server(), ne_binary(), ne_binary(), view_options()) ->
                               {'ok', integer()} |
                               data_error().
get_results_count({App, Conn}, DbName, DesignDoc, ViewOptions) ->
    App:get_results_count(Conn, DbName, DesignDoc, ViewOptions).
