%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license. 
%%% See the NOTICE for more information.

-module(couchbeam_view).
-author('Benoît Chesneau <benoitc@e-engura.org>').


-include("couchbeam.hrl").

-export([count/1, fetch/1, first/1, fold/2, foreach/2]).

%% spec count(View :: view()) -> integer()
%% @doc get number of results in the view
count(#view{db=Db,url=Url, options=Options, method=Method, body=Body,
        headers=Headers}) ->
    #db{options=IbrowseOpts} = Db,
    case couchbeam:db_request(Method, Url, ["200"], IbrowseOpts, Headers, 
            Body) of
        {ok, _, _, RespBody} ->
            {Props} = couchbeam_util:json_decode(RespBody),
            case proplists:get_value("limit", Options) of
            0 ->
                {ok, proplists:get_value(<<"total_rows">>, Props)};
            _ ->
                Rows = proplists:get_value(<<"rows">>, Props),
                {ok, length(Rows)}
            end;
        Error ->
            Error
    end.


%% @doc get all results in a view.
%% @spec fetch(View :: view()) -> term()
fetch(#view{db=Db, url=Url, method=Method, body=Body, headers=Headers}) ->
    #db{options=IbrowseOpts} = Db,
    case couchbeam:db_request(Method, Url, ["200"], IbrowseOpts, Headers, 
            Body) of
        {ok, _, _, RespBody} ->
            JsonBody = couchbeam_util:json_decode(RespBody),
            {ok, JsonBody};
        Error ->
            Error
    end.


%% @doc get first row in results of the view
%% @spec first(View :: view()) -> term()
first(View) ->
    #view{
        db=Db, 
        options=Options, 
        method=Method, 
        body=Body, 
        headers=Headers,
        url_parts = UrlParts
    } = View,
    % make sure we don't override the limit
    Options1 = case proplists:get_value("limit", Options) of
        undefined ->
            [{"limit", 1}|Options];
        _Else ->
            Opts = proplists:delete("limit", Options),
            [{"limit", 1}|Opts]
    end,
    #db{server=Server, options=IbrowseOpts} = Db,
    Url = couchbeam:make_url(Server, UrlParts, Options1),
    case couchbeam:db_request(Method, Url, ["200"], IbrowseOpts, 
            Headers, Body) of
        {ok, _, _, RespBody} ->
            {Props} = couchbeam_util:json_decode(RespBody),
            case proplists:get_value(<<"rows">>, Props) of
                [] ->
                    {ok, []};
                [Row|_] ->
                    {ok, Row}
            end;
        Error ->
            Error
    end.

%% spec fold(View, Fun) -> Acc
%% ViewPid = view()
%% Fun = fun(Row, AccIn) -> AccOut
%% Row = json_term()
%% Acc = AccIn = AccOut = term()
%% @doc Calls Fun on successive keys and values of View Results. Fun must return a new accumulator 
%% which is passed to the next call. [] is returned if the list is empty.
%% The evaluation order is undefined.
fold(View, Fun) ->
    #view{
        db=Db,
        method=Method, 
        body=Body, 
        headers=Headers,
        url = Url
    } = View,
    #db{options=IbrowseOpts} = Db,
    case couchbeam:db_request(Method, Url, ["200"], IbrowseOpts, Headers, 
            Body) of
        {ok, _, _, RespBody} ->
            {Props} = couchbeam_util:json_decode(RespBody),
            Rows = proplists:get_value(<<"rows">>, Props),
            fold_fun(Rows, Fun, []);
        Error ->
            Error
    end.

%% spec foreah(View, Fun) -> void()
%% View = view()
%% Fun = fun(Row) -> void()
%% Row = json_term()
%% @doc Calls Fun(Elem) for each element Row in view results. This function is used 
%% for its side effects and the evaluation order is defined to be the
%% same as the order of the elements in the results.
foreach(View, Fun) ->
     #view{
        db=Db,
        method=Method, 
        body=Body, 
        headers=Headers,
        url = Url
    } = View,
    #db{options=IbrowseOpts} = Db,
    case couchbeam:db_request(Method, Url, ["200"], IbrowseOpts, 
            Headers, Body) of
        {ok, _, _, RespBody} ->
            {Props} = couchbeam_util:json_decode(RespBody),
            Rows = proplists:get_value(<<"rows">>, Props),
            do_foreach(Rows, Fun);
        Error ->
            Error
    end.

%% @private
do_foreach([], _Fun) ->
    ok;
do_foreach([Row|Rest], Fun) ->
    Fun(Row),
    do_foreach(Rest, Fun).

fold_fun([], _Fun, Acc) ->
    Acc;
fold_fun([Row|Rest], Fun, Acc) ->
    fold_fun(Rest, Fun, Fun(Row, Acc)).


