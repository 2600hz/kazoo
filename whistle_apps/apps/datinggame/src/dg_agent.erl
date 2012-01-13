%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2012, James Aimonetti
%%% @doc
%%% Handle agents coming online and going offline
%%% @end
%%% Created : 11 Jan 2012 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(dg_agent).

-export([init/0, handle_offline/2, handle_online/2]).

-include("datinggame.hrl").

init() ->
    'ok'.

handle_online(JObj, Props) ->
    Agent = #dg_agent{id = wh_json:get_value(<<"Agent-ID">>, JObj)
                      ,call_id = wh_json:get_value(<<"Call-ID">>, JObj)
                      ,control_queue = wh_json:get_value(<<"Control-Queue">>, JObj)
                      ,skills = wh_json:get_value(<<"Skills">>, JObj, wh_json:new())
                      },

    Srv = props:get_value(server, Props),
    Queue = props:get_value(queue, Props),

    %% send request for channel status for the agent
    dg_util:channel_status(Agent, Queue),

    datinggame_listener:add_agent(Srv, Agent).
    

handle_offline(JObj, Props) ->
    Agent = #dg_agent{id = wh_json:get_value(<<"Agent-ID">>, JObj)
                      ,call_id = wh_json:get_value(<<"Call-ID">>, JObj)
                      },

    Srv = props:get_value(server, Props),
    datinggame_listener:rm_agent(Srv, Agent).
