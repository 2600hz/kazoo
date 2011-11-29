%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% The Hook behaviour
%%%
%%% Hook modules provide several functions for working with webhook_acct
%%% instances and sending/receiving JSON.
%%%
%%% add_binding/1 :: (pid()) -> 'ok'
%%%   Takes, as argument, the webhook_acct server's PID
%%%   
%%%
%%% @end
%%% Created : 29 Nov 2011 by James Aimonetti <james2600hz.org>
%%%-------------------------------------------------------------------
-module(gen_hook).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 0} %% gen_listener:add_responder looks for a init/0
     ,{handle_req, 2} %% when receiving an AMQP event, handle_req/2 is called
     ,{add_binding, 1} %% takes the webhook_acct's PID to invoke the add_binding/2
    ];
behaviour_info(_) ->
    undefined.
