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

-export([resolve/2, resolve/3, resolve/4, resolve/5, add_conflict_view/2]).
-export([in_conflict/2, in_conflict/3, default_view/0]).

%% Types of resolution strategies
-type resolution_strategy() :: 'remove_conflicts' | 'merge' | 'user_merge'.
-type merge_fun() :: fun((ne_binary(), wh_json:object()) -> wh_json:object()).

-export_type([resolution_strategy/0, merge_fun/0]).

-define(CONFLICT_VIEW_NAME, <<"conflict/listing_by_id">>).
-define(CONFLICT_VIEW, wh_json:from_list([{<<"_id">>, <<"_design/conflict">>}
                                          ,{<<"language">>, <<"javascript">>}
                                          ,{<<"views">>, wh_json:from_list([{<<"listing_by_id">>,
                                                                             wh_json:from_list([{<<"map">>, <<"function(doc) { if(doc._conflicts) {emit(doc._conflicts, null); } }">>}])
                                                                            }])}
                                         ])).

-include("wh_couch.hrl").

-spec default_view() -> <<_:176>>.
default_view() ->
    ?CONFLICT_VIEW_NAME.

-spec add_conflict_view(#server{}, ne_binary()) -> {'ok', wh_json:object()} | {'error', atom()}.
add_conflict_view(#server{}=Conn, DBName) ->
    couch_util:ensure_saved(Conn, DBName, ?CONFLICT_VIEW, []).

-spec in_conflict(#server{}, ne_binary()) -> {'ok', wh_json:objects()}.
-spec in_conflict(#server{}, ne_binary(), ne_binary()) -> {'ok', wh_json:objects()}.
in_conflict(#server{}=Conn, DBName) ->
    in_conflict(Conn, DBName, ?CONFLICT_VIEW_NAME).
in_conflict(#server{}=Conn, DBName, View) ->
    {ok, _} = couch_util:get_results(Conn, DBName, View, []).

-spec resolve(#server{}, ne_binary()) -> [wh_json:objects(),...].
-spec resolve(#server{}, ne_binary(), ne_binary()) -> [wh_json:objects(),...].
-spec resolve(#server{}, ne_binary(), ne_binary(), resolution_strategy()) -> [wh_json:objects(),...] | wh_json:objects().
-spec resolve(#server{}, ne_binary(), ne_binary(), resolution_strategy(), merge_fun()) -> [wh_json:objects(),...] | [].
resolve(#server{}=Conn, DBName) ->
    resolve(Conn, DBName, ?CONFLICT_VIEW_NAME).
resolve(#server{}=Conn, DBName, View) ->
    resolve(Conn, DBName, View, remove_conflicts).
resolve(#server{}=Conn, DBName, View, Strategy) ->
    {ok, Ds} = couch_util:get_results(Conn, DBName, View, []),
    resolve_strategically(Conn, Strategy, DBName, Ds).
resolve(#server{}=Conn, DBName, View, Strategy, F) when is_function(F, 2) ->
    {ok, Ds} = couch_util:get_results(Conn, DBName, View, []),
    resolve_strategically(Conn, Strategy, DBName, Ds, F).

-spec resolve_strategically(#server{}, resolution_strategy(), ne_binary(), wh_json:objects()) -> [wh_json:objects(),...].
-spec resolve_strategically(#server{}, resolution_strategy(), ne_binary(), wh_json:objects(), merge_fun()) -> wh_json:objects().
resolve_strategically(_Conn, _Strategy, _DBName, []) -> [];
resolve_strategically(#server{}=Conn, remove_conflicts, DBName, Ds) ->
    [ begin
          ID = wh_doc:id(D),
          %% delete the conflicting version
          [couch_util:del_doc(Conn, DBName, wh_json:from_list([{<<"_id">>, ID}
                                                               ,{<<"_rev">>, R}
                                                              ]))
           || R <- wh_json:get_value(<<"key">>, D, [])]
      end
      || D <- Ds];
resolve_strategically(#server{}=Conn, merge, DBName, Ds) ->
    [ merge_doc(Conn, DBName, D) || D <- Ds ].

resolve_strategically(#server{}=Conn, user_merge, DBName, Ds, F) ->
    [merge_user_doc(Conn, DBName, D, F) || D <- Ds].

-spec merge_doc(#server{}, ne_binary(), wh_json:object()) -> wh_json:object().
merge_doc(#server{}=Conn, DBName, D) ->
    ID = wh_doc:id(D),
    F = fun(Rev, Doc) ->
                {ok, ConflictDoc} = couch_util:open_doc(Conn, DBName, ID, [{<<"_rev">>, Rev}]),
                %% favor Doc values over ConflictDoc
                wh_json:merge_jobjs(Doc, ConflictDoc)
        end,
    merge_user_doc(Conn, DBName, D, F).

%% fold all conflicting versions, using fun F to handle the merge, into the existing doc and save.
-spec merge_user_doc(#server{}, ne_binary(), wh_json:object(), fun((ne_binary(), wh_json:object()) -> wh_json:object())) -> wh_json:object().
merge_user_doc(#server{}=Conn, DBName, D, F) ->
    ID = wh_doc:id(D),
    {ok, ExistingDoc} = couch_util:open_doc(Conn, DBName, ID, []),
    MergedDoc = lists:foldr(F, ExistingDoc, wh_json:get_value(<<"key">>, D, [])),
    {ok, SavedDoc} = couch_util:ensure_saved(Conn, DBName, MergedDoc, []),
    SavedDoc.
