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
-spec filter_on_query_string/3 :: (ne_binary(), ne_binary(), proplist()) -> json_objects().
filter_on_query_string(DbName, View, QueryParams) ->
    filter_on_query_string(DbName, View, QueryParams, []).

-spec filter_on_query_string/4 :: (ne_binary(), ne_binary(), proplist(), proplist()) -> json_objects().
filter_on_query_string(DbName, View, [], ViewOptions) ->
    {ok, AllDocs} = couch_mgr:get_results(DbName, View,  [{<<"include_docs">>, true} | ViewOptions]),
    [wh_json:get_value(<<"value">>, Doc, wh_json:new()) || Doc <- AllDocs];
filter_on_query_string(DbName, View, QueryParams, ViewOptions) ->
    {ok, AllDocs} = couch_mgr:get_results(DbName, View,  [{<<"include_docs">>, true} | ViewOptions]),
    [wh_json:get_value(<<"value">>, Doc, wh_json:new())
     || Doc <- AllDocs,
	Doc =/= undefined,
	filter_doc(wh_json:get_value(<<"doc">>, Doc), QueryParams)
    ].
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if all of the requested props are found, false if one is not found
%% @end
%%--------------------------------------------------------------------
-spec filter_doc/2 :: (Doc, Props) -> boolean() when
      Doc :: json_object(),
      Props :: proplist().
filter_doc(Doc, Props) ->
    [] =/= [ok || {Key, Val} <- Props, filter_prop(Doc, Key, Val)].

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
    wh_json:get_value(binary:split(Key, <<".">>), Doc) =:= Val;
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
