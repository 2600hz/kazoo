%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Ways to resolve conflicts in DBs. DBName must be the encoded version,
%%% replacing '/' with '%2f' among others
%%% @end
%%% Created : 27 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(couch_conflict).

-export([resolve/1, resolve/2, resolve/3, resolve/4, add_conflict_view/1]).
-export([in_conflict/1, in_conflict/2]).

-define(CONFLICT_VIEW_NAME, <<"conflict/listing_by_id">>).
-define(CONFLICT_VIEW, wh_json:from_list([{<<"_id">>, <<"_design/conflict">>}
					  ,{<<"language">>, <<"javascript">>}
					  ,{<<"views">>, wh_json:from_list([{<<"listing_by_id">>,
									     wh_json:from_list([{<<"map">>, <<"function(doc) { if(doc._conflicts) {emit(doc._conflicts, null); } }">>}])
									    }])}
					 ])).

-include_lib("whistle/include/wh_types.hrl").

-spec add_conflict_view/1 :: (ne_binary()) -> {'ok', json_object()} | {'error', atom()}.
add_conflict_view(DBName) ->
    couch_mgr:ensure_saved(DBName, ?CONFLICT_VIEW).

-spec in_conflict/1 :: (ne_binary()) -> {'ok', json_objects()}.
-spec in_conflict/2 :: (ne_binary(), ne_binary()) -> {'ok', json_objects()}.
in_conflict(DBName) ->
    in_conflict(DBName, ?CONFLICT_VIEW_NAME).
in_conflict(DBName, View) ->
    {ok, _} = couch_mgr:get_results(DBName, View, []).

%% Types of resolution strategies
-type resolution_strategy() :: 'remove_conflicts' | 'merge' | 'user_merge'.
-type merge_fun() :: fun((ne_binary(), json_object()) -> json_object()).

-spec resolve/1 :: (ne_binary()) -> [json_objects(),...].
-spec resolve/2 :: (ne_binary(), ne_binary()) -> [json_objects(),...].
-spec resolve/3 :: (ne_binary(), ne_binary(), resolution_strategy()) -> [json_objects(),...] | json_objects().
-spec resolve/4 :: (ne_binary(), ne_binary(), resolution_strategy(), merge_fun()) -> [json_objects(),...] | [].
resolve(DBName) ->
    resolve(DBName, ?CONFLICT_VIEW_NAME).
resolve(DBName, View) ->
    resolve(DBName, View, remove_conflicts).
resolve(DBName, View, Strategy) ->
    {ok, Ds} = couch_mgr:get_results(DBName, View, []),
    resolve_strategically(Strategy, DBName, Ds).
resolve(DBName, View, Strategy, F) when is_function(F, 2) ->
    {ok, Ds} = couch_mgr:get_results(DBName, View, []),
    resolve_strategically(Strategy, DBName, Ds, F).

-spec resolve_strategically/3 :: (resolution_strategy(), ne_binary(), json_objects()) -> [json_objects(),...].
-spec resolve_strategically/4 :: (resolution_strategy(), ne_binary(), json_objects(), merge_fun()) -> json_objects().
resolve_strategically(_Strategy, _DBName, []) -> [];
resolve_strategically(remove_conflicts, DBName, Ds) ->
    [ begin
	  ID = wh_json:get_value(<<"id">>, D),
	  %% delete the conflicting version
	  [couch_mgr:del_doc(DBName, wh_json:from_list([{<<"_id">>, ID}
							,{<<"_rev">>, R}
						       ]))
	   || R <- wh_json:get_value(<<"key">>, D, [])]
      end
      || D <- Ds];
resolve_strategically(merge, DBName, Ds) ->
    [ merge_doc(DBName, D) || D <- Ds ].

resolve_strategically(user_merge, DBName, Ds, F) ->
    [merge_user_doc(DBName, D, F) || D <- Ds].

-spec merge_doc/2 :: (ne_binary(), json_object()) -> json_object().
merge_doc(DBName, D) ->
    ID = wh_json:get_value(<<"id">>, D),
    F = fun(Rev, Doc) ->
		{ok, ConflictDoc} = couch_mgr:open_doc(DBName, ID, [{<<"_rev">>, Rev}]),
		%% favor Doc values over ConflictDoc
		wh_json:merge_jobjs(Doc, ConflictDoc)
	end,
    merge_user_doc(DBName, D, F).

%% fold all conflicting versions, using fun F to handle the merge, into the existing doc and save.
-spec merge_user_doc/3 :: (ne_binary(), json_object(), fun((ne_binary(), json_object()) -> json_object())) -> json_object().
merge_user_doc(DBName, D, F) ->
    ID = wh_json:get_value(<<"id">>, D),
    {ok, ExistingDoc} = couch_mgr:open_doc(DBName, ID),
    MergedDoc = lists:foldr(F, ExistingDoc, wh_json:get_value(<<"key">>, D, [])),
    {ok, SavedDoc} = couch_mgr:ensure_saved(DBName, MergedDoc),
    SavedDoc.
