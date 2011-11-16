%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created : 12 Nov 2011 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(wh_amqp_params).

-export([host/1]).

-include("amqp_util.hrl").

host(#'amqp_params_network'{host=H}) ->
    H.
