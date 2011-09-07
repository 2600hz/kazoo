%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2010-2011 VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 8 Dec 2011 by Edouard Swiac <edouard@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_filter).

-export([filter_on_query_string/3, filter_on_query_string/4]).

-include("crossbar.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of Doc ID from the crossbar_listing view,
%% filtered on the query string params
%% @end
%%--------------------------------------------------------------------
-spec filter_on_query_string/3 :: (DbName, View, QueryParams) -> json_objects() when
      DbName :: binary(),
      View :: binary(),
      QueryParams :: proplist().
filter_on_query_string(DbName, View, QueryParams) ->
    filter_on_query_string(DbName, View, QueryParams, []).

-spec filter_on_query_string/4 :: (DbName :: binary(), View :: binary(), QueryParams :: proplist(), ViewOptions :: proplist()) -> json_objects().
filter_on_query_string(DbName, View, QueryParams, ViewOptions) ->
    QueryParams1 = [{list_to_binary(K), list_to_binary(V)} || {K, V} <- QueryParams],
     %% qs from wm are strings
    {ok, AllDocs} = couch_mgr:get_results(DbName, View,  [{<<"include_docs">>, true} | ViewOptions]),
    case QueryParams of
	[] -> [wh_json:get_value(<<"value">>, Doc, ?EMPTY_JSON_OBJECT) || Doc <- AllDocs];
	_ -> [wh_json:get_value(<<"value">>, Doc, ?EMPTY_JSON_OBJECT) || Doc <- AllDocs,
                                                                filter_doc(wh_json:get_value(<<"doc">>, Doc), QueryParams1)]
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if all of the requested props are found, false if one is not found
%% @end
%%--------------------------------------------------------------------
-spec filter_doc/2 :: (Doc, Props) -> boolean() when
      Doc :: json_object(),
      Props :: proplist().
filter_doc(undefined, _) ->
    false;
filter_doc(Doc, Props) ->
    Result = [filter_prop(Doc, Key, Val) || {Key, Val} <- Props],
    (Result =/= [] andalso lists:all(fun(Term) -> Term end, Result)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true or false if the prop is found inside the doc
%% @end
%%--------------------------------------------------------------------
-spec filter_prop/3 :: (Doc, Key, Val) -> boolean() when
      Doc :: json_object(),
      Key :: binary(),
      Val :: term().
filter_prop(Doc, <<"filter_", Key/binary>>, Val) ->
    wh_json:get_value(Key, Doc) == Val;
filter_prop(Doc, <<"created_from">>, Val) ->
    wh_util:to_integer(wh_json:get_value(<<"pvt_created">>, Doc)) >= wh_util:to_integer(Val);
filter_prop(Doc, <<"created_to">>, Val) ->
    wh_util:to_integer(wh_json:get_value(<<"pvt_created">>, Doc)) =< wh_util:to_integer(Val);
filter_prop(Doc, <<"modified_from">>, Val) ->
    wh_util:to_integer(wh_json:get_value(<<"pvt_modified">>, Doc)) >= wh_util:to_integer(Val);
filter_prop(Doc, <<"modified_to">>, Val) ->
    wh_util:to_integer(wh_json:get_value(<<"pvt_modified">>, Doc)) =< wh_util:to_integer(Val);
filter_prop(_, _, _) ->
    true.

%% next filters to implement are to range on a prop
%% build_filter_options(<<"range_", Param/binary, "_from">>) -> nyi.
%% build_filter_options(<<"range_", Param/binary, "_to">>) -> nyi.
