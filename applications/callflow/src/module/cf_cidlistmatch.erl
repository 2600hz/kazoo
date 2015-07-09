%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Handles inspection of incoming caller id and branching to a child
%%% callflow node accordingly.
%%%
%%% "data":{
%%%   "id":"01fc63f92d9b89a25dd4ff1039e64497" // match list id
%%% },
%%% "children": {
%%%   "match": { [callflow node to branch to when absolute mode is false and regex matches] },
%%%   "nomatch": { [callflow node to branch to when regex does not match or no child node defined for incoming caller id] },
%%% }
%%% @end
%%% @contributors
%%%   Kozlov Yakov
%%%-------------------------------------------------------------------
-module(cf_cidlistmatch).

-export([handle/2]).

-include("../callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CallerIdNumber = whapps_call:caller_id_number(Call),
    ListEntries = get_list_entries(Data, Call),
    lager:debug("comparing caller id ~s with match list entries", [CallerIdNumber]),
    case match_one_of(ListEntries, CallerIdNumber) of
        {'match', Entry} ->
            lager:info("matched list ~s entry", [wh_doc:id(Entry)]),
            handle_match(Call);
        'nomatch' ->
            handle_no_match(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle a caller id "match" condition
%% @end
%%--------------------------------------------------------------------
-spec handle_match(whapps_call:call()) -> 'ok'.
handle_match(Call) ->
    case is_callflow_child(<<"match">>, Call) of
        'true' -> 'ok';
        'false' -> cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle a caller id "no match" condition
%% @end
%%--------------------------------------------------------------------
-spec handle_no_match(whapps_call:call()) -> 'ok'.
handle_no_match(Call) ->
    case is_callflow_child(<<"nomatch">>, Call) of
        'true' -> 'ok';
        'false' -> cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if the given node name is a callflow child
%% @end
%%--------------------------------------------------------------------
-spec is_callflow_child(ne_binary(), whapps_call:call()) -> boolean().
is_callflow_child(Name, Call) ->
    lager:debug("Looking for callflow child ~s", [Name]),
    case cf_exe:attempt(Name, Call) of
        {'attempt_resp', 'ok'} ->
            lager:debug("found callflow child"),
            'true';
        {'attempt_resp', {'error', _}} ->
            lager:debug("failed to find callflow child"),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if caller id matches list entry pattern.
%% @end
%%--------------------------------------------------------------------
-spec match_one_of(wh_json:objects(), ne_binary()) ->
                          {'match', wh_json:object()} |
                          'nomatch'.
match_one_of([], _CallerIdNumber) -> 'nomatch';
match_one_of([Entry|Rest], CallerIdNumber) ->
    Pattern = wh_json:get_value(<<"pattern">>, Entry),
    case re:run(CallerIdNumber, Pattern) of
        {'match', _} -> {'match', Entry};
        'nomatch' -> match_one_of(Rest, CallerIdNumber)
    end.

-spec get_list_entries(wh_json:object(), whapps_call:call()) -> wh_json:objects().
get_list_entries(Data, Call) ->
    ListId = wh_doc:id(Data),
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_cache_doc(AccountDb, ListId) of
        {'ok', ListJObj} ->
            lager:info("match list loaded: ~s", [ListId]),
            JObj = wh_json:get_ne_value(<<"entries">>, ListJObj),
            lists:map(fun get_list_entries_map/1, wh_json:to_proplist(JObj));
        {'error', Reason} ->
            lager:info("failed to load match list box ~s, ~p", [ListId, Reason]),
            []
    end.

-spec get_list_entries_map({ne_binary(), wh_json:object()}) -> wh_json:object().
get_list_entries_map({K, V}) ->
    wh_json:set_value(<<"id">>, K, V).
