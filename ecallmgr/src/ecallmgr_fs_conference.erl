%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Track FreeSWITCH conference information and provide accessors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_conference).

-export([show_all/0
         ,new/2
        ]).

-compile([{no_auto_import, [node/1]}]).

-include("ecallmgr.hrl").

-define(NODES_SRV, ecallmgr_fs_nodes).
-define(CHANNEL_TBL, ecallmgr_channels).

-spec show_all() -> wh_json:objects().
show_all() ->
    ets:foldl(fun(Channel, Acc) ->
                      [record_to_json(Channel) | Acc]
              end, [], ?CONFERENCE_TBL).

-spec new(wh_proplist(), atom()) -> 'ok'.
new(Props, Node) ->
    CallId = props:get_value(<<"Unique-ID">>, Props),
    put(callid, CallId),
    gen_server:cast(?NODES_SRV, {new_conference, props_to_record(Props, Node)}).
