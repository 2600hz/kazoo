%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%% Look up caller id number for spam score
%%% "data":{
%%%   "username":"nomorobo_username"
%%%   ,"password":"nomorobo_password"
%%% }
%%% ,"children":{
%%%   "0":{...} // is not robocall
%%%   "10":{...} // is robocall
%%% }
%%% Children keys are numbers between 0 and 10, 0 meaning not a robocall
%%% and 10 being definitely a robocall
%%% For instance, say the children keys are "0", "3", "6", and "10"
%%% Spam Score | Branch
%%%     0      |   "0"
%%%     1      |   "0"
%%%     2      |   "0"
%%%     3      |   "3"
%%%     4      |   "3"
%%%     5      |   "3"
%%%     6      |   "6"
%%%     7      |   "6"
%%%     8      |   "6"
%%%     9      |   "6"
%%%    10      |   "10"
%%% The "_" child key is equivalent to "0" in this case.
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cf_nomorobo).

-export([handle/2
        ,nomorobo_req/2
        ]).

-ifdef(TEST).
-export([nomorobo_branches/1
        ,nomorobo_branch/2
        ]).
-endif.

-include("callflow.hrl").

-define(URL, <<"https://api.nomorobo.com/v1/check?From={FROM}&To={TO}">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    case nomorobo_score(Data, Call) of
        'undefined' ->
            continue_to_default(Call);
        Score ->
            continue_to_score(Call, Score)
    end.

-spec nomorobo_score(kz_json:object(), kapps_call:call()) ->
                            api_integer().
nomorobo_score(Data, Call) ->
    URI = nomorobo_uri(Call),

    lager:info("sending request to nomorobo: ~s", [URI]),

    try nomorobo_req(URI, Data) of
        {'ok', 200, _Headers, Body} ->
            nomorobo_score_from_resp(Body);
        {'ok', _Status, _Headers, _Body} ->
            lager:debug("failed to query: ~p: ~s", [_Status, _Body]),
            'undefined';
        {'error', _E} ->
            lager:debug("error querying: ~p", [_E]),
            'undefined'
    catch
        _E:_R ->
            lager:debug("crashed querying nomorobo: ~s: ~p", [_E, _R]),
            'undefined'
    end.

-spec nomorobo_req(ne_binary(), kz_json:object()) -> kz_http:ret().
nomorobo_req(URI, Data) ->
    Username = kz_json:get_binary_value(<<"username">>, Data),
    Password = kz_json:get_binary_value(<<"password">>, Data),
    Options = [{'basic_auth', {Username, Password}}
              ,{'ssl', [{'verify', 'verify_none'}]}
              ],

    kz_http:get(kz_util:to_list(URI), [], Options).

-spec nomorobo_uri(kapps_call:call()) -> ne_binary().
nomorobo_uri(Call) ->
    lists:foldl(fun uri_replace/2
               ,?URL
               ,[{<<"{TO}">>, knm_converters:to_npan(kapps_call:request_user(Call))}
                ,{<<"{FROM}">>, knm_converters:to_npan(kapps_call:caller_id_number(Call))}
                ]).

-spec uri_replace({ne_binary(), ne_binary()}, ne_binary()) -> ne_binary().
uri_replace({S, R}, U) -> binary:replace(U, S, R).

-spec nomorobo_score_from_resp(binary()) -> api_integer().
nomorobo_score_from_resp(Body) ->
    try kz_json:decode(Body) of
        JObj -> trunc(kz_json:get_float_value(<<"score">>, JObj) * 10)
    catch
        _E:_R ->
            lager:debug("failed to decode JSON: ~s: ~p", [_E, _R]),
            lager:debug("JSON: ~s", [Body]),
            'undefined'
    end.

-spec continue_to_score(kapps_call:call(), integer()) -> 'ok'.
continue_to_score(Call, Score) ->
    Keys = nomorobo_branches(cf_exe:get_all_branch_keys(Call)),
    Branch = nomorobo_branch(Score, Keys),
    lager:info("attempting to branch to '~s' from score '~p'", [Branch, Score]),
    cf_exe:continue(Branch, Call).

-spec nomorobo_branch(integer(), integers()) -> ne_binary().
nomorobo_branch(Score, [Lo|Keys]) ->
    branch_to_binary(nomorobo_branch(Score, Lo, Keys)).

-spec branch_to_binary(integer()) -> ne_binary().
branch_to_binary(-1) -> <<"_">>;
branch_to_binary(I) -> kz_util:to_binary(I).

nomorobo_branch(_Score, Branch, []) -> Branch;
nomorobo_branch(Score, _Lo, [K|Ks]) when K =< Score ->
    nomorobo_branch(Score, K, Ks);
nomorobo_branch(Score, Lo, [_K|Ks]) ->
    nomorobo_branch(Score, Lo, Ks).

-spec continue_to_default(kapps_call:call()) -> 'ok'.
continue_to_default(Call) ->
    case nomorobo_branches(cf_exe:get_all_branch_keys(Call)) of
        [-1|_] ->
            lager:info("branching to default child"),
            cf_exe:continue(Call);
        [0|_] ->
            lager:info("branching to '0' child"),
            cf_exe:continue(<<"0">>, Call);
        _ ->
            lager:info("no default branch, done with the call"),
            cf_exe:stop(Call)
    end.

-spec nomorobo_branches({'branch_keys', ne_binaries()}) -> integers().
-spec nomorobo_branches(ne_binaries(), integers()) -> integers().
nomorobo_branches({'branch_keys', Keys}) ->
    nomorobo_branches(Keys, []).

nomorobo_branches([], Branches) ->
    lists:sort(Branches);
nomorobo_branches([<<"_">>|Keys], Branches) ->
    nomorobo_branches(Keys, [-1 | Branches]);
nomorobo_branches([Key|Keys], Branches) ->
    try kz_util:to_integer(Key) of
        I -> nomorobo_branches(Keys, [I | Branches])
    catch
        'error':'badarg' ->
            lager:debug("failed to convert ~s~n", [Key]),
            nomorobo_branches(Keys, Branches)
    end.
