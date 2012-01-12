%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% Created : 08 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(wnm_local).

-export([find_numbers/2]).
-export([acquire_number/3]).

-include("../../include/wh_number_manager.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of avaliable numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers/2 :: (ne_binary(), pos_integer()) -> {ok, json_object} | {error, term()}.
find_numbers(Number, Quanity) when size(Number) < 5 ->
    find_numbers(<<"+1", Number/binary>>, Quanity);
find_numbers(Number, Quanity) ->
    Db = wnm_util:number_to_db_name(Number),
    case couch_mgr:get_results(Db, <<"numbers/status">>, [{<<"startkey">>, [<<"avaliable">>, Number]}
                                                          ,{<<"endkey">>, [<<"avaliable">>, <<Number/binary, "\ufff0">>]}
                                                          ,{<<"limit">>, Quanity}
                                                         ]) of
        {ok, []} -> 
            ?LOG("found no avaliable local numbers"),
            {error, non_avaliable};
        {ok, JObjs} ->
            ?LOG("found ~p avaliable local numbers", [length(JObjs)]),
            {ok, wh_json:from_list([{wh_json:get_value(<<"id">>, JObj), wh_json:new()}
                                    || JObj <- JObjs
                                   ])};
        {error, R}=E ->
            ?LOG("failed to lookup avaliable local numbers: ~p", [R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number/3 :: (ne_binary(), ne_binary(), json_object()) -> {ok, ne_binary(), json_object()}
                                                                           | {error, ne_binary()}.
acquire_number(_, <<"avaliable">>, JObj) ->
    {ok, <<"in_service">>, JObj};
acquire_number(_, <<"claim">>, JObj) ->
    {ok, <<"in_service">>, JObj};
acquire_number(_, _, _) ->
    {error, unavaliable}.
