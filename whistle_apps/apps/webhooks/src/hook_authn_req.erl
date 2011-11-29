%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Bind to authn_req, exclude ones not for this account, and call the URI
%%% supplied when a valid authn_req is received, hopefully receive a valid
%%% authn_resp, and send it along to Whistle.
%%% @end
%%% Created : 29 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(hook_authn_req).

-behaviour(gen_hook).

%% gen_hook callbacks
-export([init/0, add_binding/1, handle_req/2]).

-include("webhooks.hrl").

init() ->
    'ok'.

add_binding(Srv) ->
    'ok'.

handle_req(JObj, Props) ->
    'ok'.
