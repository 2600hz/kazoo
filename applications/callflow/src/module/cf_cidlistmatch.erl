%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
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

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CallerIdNumber = kapps_call:caller_id_number(Call),
    ListId = kz_json:get_ne_binary_value(<<"id">>, Data),
    AccountDb = kapps_call:account_db(Call),
    lager:debug("comparing caller id ~s with match list ~s entries in ~s", [CallerIdNumber, ListId, AccountDb]),
    case is_matching_prefix(AccountDb, ListId, CallerIdNumber)
        orelse is_matching_regexp(AccountDb, ListId, CallerIdNumber)
    of
        'true' -> handle_match(Call);
        'false' -> handle_no_match(Call)
    end.

-spec is_matching_prefix(ne_binary(), ne_binary(), ne_binary()) -> boolean().
is_matching_prefix(AccountDb, ListId, Number) ->
    NumberPrefixes = build_keys(Number),
    Keys = [[ListId, X] || X <- NumberPrefixes],
    case kz_datamgr:get_results(AccountDb, <<"lists/match_prefix_in_list">>, [{'keys', Keys}]) of
        {'ok', [_ | _]} -> 'true';
        _ -> 'false'
    end.

-spec is_matching_regexp(ne_binary(), ne_binary(), ne_binary()) -> boolean().
is_matching_regexp(AccountDb, ListId, Number) ->
    case kz_datamgr:get_results(AccountDb, <<"lists/regexps_in_list">>, [{'key', ListId}]) of
        {'ok', Regexps} ->
            Patterns = [kz_json:get_value(<<"value">>, X) || X <- Regexps],
            match_regexps(Patterns, Number);
        _ ->
            'false'
    end.

-spec match_regexps(binaries(), ne_binary()) -> boolean().
match_regexps([Pattern | Rest], Number) ->
    case re:run(Number, Pattern) of
        {'match', _} -> 'true';
        'nomatch' -> match_regexps(Rest, Number)
    end;
match_regexps([], _Number) -> 'false'.

%% TODO: this function from hon_util, may be place it somewhere in library?
-spec build_keys(binary()) -> binaries().
build_keys(<<"+", E164/binary>>) ->
    build_keys(E164);
build_keys(<<D:1/binary, Rest/binary>>) ->
    build_keys(Rest, D, [D]).

-spec build_keys(binary(), binary(), binaries()) -> binaries().
build_keys(<<D:1/binary, Rest/binary>>, Prefix, Acc) ->
    build_keys(Rest, <<Prefix/binary, D/binary>>, [<<Prefix/binary, D/binary>> | Acc]);
build_keys(<<>>, _, Acc) -> Acc.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle a caller id "match" condition
%% @end
%%--------------------------------------------------------------------
-spec handle_match(kapps_call:call()) -> 'ok'.
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
-spec handle_no_match(kapps_call:call()) -> 'ok'.
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
-spec is_callflow_child(ne_binary(), kapps_call:call()) -> boolean().
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
