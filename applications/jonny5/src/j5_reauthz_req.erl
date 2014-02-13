%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_reauthz_req).

-export([handle_req/2]).

-include("jonny5.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_authz:reauthz_req_v(JObj),
    wh_util:put_callid(JObj),
    %% TODO: FIX ME!
    'ok'.
