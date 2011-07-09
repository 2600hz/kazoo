%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2010-2011 VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 8 Dec 2011 by Edouard Swiac <edouard@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_filter).

-export([filter_on_query_string/4]).

-include("../include/crossbar.hrl").


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of Doc ID from the crossbar_listing view,
%% filtered on the query string params
%% @end
%%--------------------------------------------------------------------
-spec(filter_on_query_string/4 :: (DbName :: binary(), View :: term(), QueryString :: list(), list()) -> list(binary())).
filter_on_query_string(DbName, View, QueryString, _Options) ->
    QueryStringBinary = [{list_to_binary(K), list_to_binary(V)} || {K, V} <- QueryString], %% qs from wm are strings
    {ok, AllDocs} = couch_mgr:get_results(DbName, View, [{<<"include_docs">>, true}]),
    UnfilteredDocs = [wh_json:get_value(<<"doc">>, UnfilteredDoc, ?EMPTY_JSON_OBJECT) || UnfilteredDoc <- AllDocs],
    [wh_json:get_value(<<"_id">>, Doc) || Doc <- UnfilteredDocs, filter_doc(Doc, QueryStringBinary)].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if all ofthe requested props are found, false if one is not found
%% @end
%%--------------------------------------------------------------------
-spec(filter_doc/2 :: (Doc :: json_object(), Props :: list(tuple(binary(), binary()))) -> boolean()).
filter_doc(Doc, Props) ->
    Result = lists:foldl(fun filter_prop/2, [], [{Doc, Key, Val} || {Key, Val} <- Props]), %% repack doc,key,val to a tuple to use foldl
    lists:all(fun(Term) -> Term end, Result).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true or false if the prop is found inside the doc
%% @end
%%--------------------------------------------------------------------
-spec(filter_prop/2 :: (tuple(Doc :: json_object(), binary()), Acc :: list())-> function()).
filter_prop({Doc, <<"filter_", Key/binary>>, Val}, Acc) ->
    [(wh_json:get_value(Key, Doc) =:= Val) | Acc];
filter_prop({Doc, <<"created_from">>, Val}, Acc) ->
    [whistle_util:to_integer(wh_json:get_value(<<"pvt_created">>, Doc)) >= whistle_util:to_integer(Val) | Acc];
filter_prop({Doc, <<"created_to">>, Val}, Acc) ->
    [whistle_util:to_integer(wh_json:get_value(<<"pvt_created">>, Doc)) =< whistle_util:to_integer(Val) | Acc];
filter_prop({Doc, <<"modified_from">>, Val}, Acc) ->
    [whistle_util:to_integer(wh_json:get_value(<<"pvt_modified">>, Doc)) >= whistle_util:to_integer(Val) | Acc];
filter_prop({Doc, <<"modified_to">>, Val}, Acc) ->
    [whistle_util:to_integer(wh_json:get_value(<<"pvt_modified">>, Doc)) =< whistle_util:to_integer(Val) | Acc];
filter_prop({_, _, _}, Acc) ->
    Acc.

%% next filters to implement are to range on a prop
%% build_filter_options(<<"range_", Param/binary, "_from">>) -> nyi.
%% build_filter_options(<<"range_", Param/binary, "_to">>) -> nyi.
