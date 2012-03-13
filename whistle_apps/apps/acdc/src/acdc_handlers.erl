%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_handlers).

-export([handle_new_member/3
        ]).

-include("acdc.hrl").

handle_new_member(JObj, Props, Delivery) ->
    lager:debug("recv ~p", [JObj]),
    lager:debug("delivery: ~p", [Delivery]),

    case crypto:rand_uniform(1, 10) of
        X when X < 8 ->
            lager:debug("acking message"),
            amqp_util:basic_ack(Delivery);
        _ ->
            lager:debug("nacking message"),
            amqp_util:basic_nack(Delivery)
    end.
