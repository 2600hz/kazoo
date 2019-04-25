%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc data adapter behaviour
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzs_view).

%% View-related
-export([design_compact/3
        ,design_info/3
        ,all_design_docs/3
        ,get_results/4
        ,get_results_count/4
        ,all_docs/3
        ,doc_type_from_view/2
        ]).

-include("kz_data.hrl").

%%% View-related functions -----------------------------------------------------
-spec design_compact(map(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
design_compact(#{server := {App, Conn}}, DbName, Design) -> App:design_compact(Conn, DbName, Design).

-spec design_info(map(), kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_json:object()} | data_error().
design_info(#{server := {App, Conn}}, DBName, Design) -> App:design_info(Conn, DBName, Design).

-spec all_design_docs(map(), kz_term:ne_binary(), view_options()) ->
                             {'ok', kz_json:objects()} |
                             data_error().
all_design_docs(#{server := {App, Conn}}, DBName, Options) ->
    App:all_design_docs(Conn, DBName, Options).

-spec all_docs(map(), kz_term:ne_binary(), view_options()) ->
                      {'ok', kz_json:objects()} |
                      data_error().
all_docs(#{server := {App, Conn}}, DbName, Options) ->
    App:all_docs(Conn, DbName, Options).

-spec get_results(map(), kz_term:ne_binary(), kz_term:ne_binary(), view_options()) ->
                         {'ok', kz_json:objects() | kz_json:path()} |
                         data_error().
get_results(#{server := {App, Conn}}, DbName, DesignDoc, ViewOptions) ->
    ResultKey = props:get_value('result_key', ViewOptions),
    case App:get_results(Conn, DbName, DesignDoc, ViewOptions) of
        {'ok', JObjs} when is_binary(ResultKey) ->
            {'ok', [kz_json:get_value(ResultKey, JObj) || JObj <- JObjs]};
        Other -> Other
    end.

%% This function assumes a "reduce" function that returns the count of docs exists
%% Need to see how to get couchbeam to return the "rows" property instead of the result
%% list; that would be better, but for not, setting the view's "reduce" to the _count
%% function will suffice (provided a reduce isn't already defined).
-spec get_results_count(map(), kz_term:ne_binary(), kz_term:ne_binary(), view_options()) ->
                               {'ok', integer()} |
                               data_error().
get_results_count(#{server := {App, Conn}}, DbName, DesignDoc, ViewOptions) ->
    App:get_results_count(Conn, DbName, DesignDoc, ViewOptions).

-spec doc_type_from_view(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
doc_type_from_view(<<"faxes">>, _ViewName) -> <<"fax">>;
doc_type_from_view(<<"cdrs">>, _ViewName) -> <<"cdr">>;
doc_type_from_view(<<"recordings">>, _ViewName) -> <<"call_recording">>;
doc_type_from_view(<<"mailbox_messages">>, _ViewName) -> <<"mailbox_message">>;
doc_type_from_view(<<"sms">>, _ViewName) -> <<"sms">>;
doc_type_from_view(_ViewType, _ViewName) -> <<"any">>.
