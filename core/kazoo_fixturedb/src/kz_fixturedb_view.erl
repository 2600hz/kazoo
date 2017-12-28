%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_fixturedb_view).

%% View-related
-export([design_info/3
        ,all_design_docs/3
        ,get_results/4
        ,get_results_count/4
        ,all_docs/3
        ]).

-include("kz_fixturedb.hrl").

%%%===================================================================
%%% View-related
%%%===================================================================

-spec design_info(server_map(), ne_binary(), ne_binary()) -> doc_resp().
design_info(_Server, _DbName, Design) ->
    {ok, kz_json:from_list([{<<"name">>, Design}])}.

-spec all_design_docs(server_map(), ne_binary(), kz_data:options()) -> docs_resp().
all_design_docs(Server, DbName, _Options) ->
    Db = kz_fixturedb_server:get_db(Server, DbName),
    Path = kz_fixturedb_util:docs_dir(Db),
    case filelib:wildcard(Path ++ "/_design*") of
        [] ->
            case kz_fixturedb_db:db_exists(Server, DbName) of
                'true' -> {ok, []};
                'false' -> {error, not_found}
            end;
        DesignDocs ->
            {ok, [kz_http_util:urldecode(kz_term:to_binary(filename:basename(D))) || D <- DesignDocs]}
    end.

-spec get_results(server_map(), ne_binary(), ne_binary(), kz_data:options()) -> docs_resp().
get_results(Server, DbName, Design, Options) ->
    Db = kz_fixturedb_server:get_db(Server, DbName),
    %%props:get_first_defined(?DANGEROUS_VIEW_OPTS, Options) =/= undefined
    %%    andalso io:format(user, "~n~s:~b ~s you're testing too much, go home~n", [?MODULE, ?LINE, Design]),
    case kz_fixturedb_util:open_view(Db, kz_term:to_binary(Design), Options) of
        {ok, Result} -> {ok, prepare_view_result(Server, DbName, Result, Options)};
        {error, _} -> {error, invalid_view_name}
    end.

-spec get_results_count(server_map(), ne_binary(), ne_binary(), kz_data:options()) -> {ok, non_neg_integer()} | fixture_error().
get_results_count(Server, DbName, Design, Options) ->
    case get_results(Server, DbName, Design, Options) of
        {ok, JObjs} -> {ok, erlang:length(JObjs)};
        {error, _}=Error -> Error
    end.

-spec all_docs(server_map(), ne_binary(), kz_data:options()) -> docs_resp().
all_docs(Server, DbName, Options) ->
    get_results(Server, DbName, <<"all_docs">>, Options).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec prepare_view_result(server_map(), ne_binary(), kz_json:objects(), kz_data:options()) -> kz_json:objects().
prepare_view_result(Server, DbName, Result, Options) ->
    case props:get_value(include_docs, Options, false) of
        false -> sort_and_limit(Result, Options);
        true ->
            [kz_json:set_value(<<"doc">>, Opened, V)
             || V <- sort_and_limit(Result, Options),
                {OkError, Opened} <- [kz_fixturedb_doc:open_doc(Server, DbName, kz_doc:id(V), Options)],
                OkError =/= error
            ]
    end.

-spec sort_and_limit(kz_json:objects(), kz_data:options()) -> kz_json:objects().
sort_and_limit(Result, Options) ->
    Limit = props:get_integer_value(limit, Options),
    case props:get_value(descending, Options, false) of
        false -> limit_result(Result, Limit);
        true -> limit_result(lists:reverse(Result), Limit)
    end.

-spec limit_result(kz_json:objects(), api_integer()) -> kz_json:objects().
limit_result(Result, Limit) when is_integer(Limit),
                                 Limit > 0 ->
    try lists:split(Limit, Result) of
        {L1, _} -> L1
    catch
        error:badarg -> Result
    end;
limit_result(Result, _) ->
    Result.
