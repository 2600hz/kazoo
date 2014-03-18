%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% James Aimonetti
%%% Peter Defebvre
%%% Ben Wann
%%%-------------------------------------------------------------------
-module(blackhole_util).

-include("blackhole.hrl").

-export([is_authorized/1]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client has
%% provided a valid authentication token
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(bh_context:context()) -> boolean().
is_authorized(Context) ->
    Event = <<"authenticate">>,
    case blackhole_bindings:succeeded(blackhole_bindings:map(Event, Context)) of
        [] ->
            lager:debug("failed to authenticate"),
            'false';
        ['true'|_] ->
            lager:debug("is_authentic: true"),
            'true';
        [{'halt', _}|_] ->
            lager:debug("is_authentic: halt"),
            'false'
    end.
