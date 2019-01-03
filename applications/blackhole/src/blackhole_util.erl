%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Peter Defebvre
%%% @author Ben Wann
%%% @end
%%%-----------------------------------------------------------------------------
-module(blackhole_util).

-include("blackhole.hrl").

-export([is_authenticated/1, is_authorized/1]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_authenticated(bh_context:context()) -> boolean().
is_authenticated(Context) ->
    AuthEvent = <<"blackhole.authenticate">>,
    case blackhole_bindings:succeeded(blackhole_bindings:map(AuthEvent, Context)) of
        [] ->
            lager:debug("failed to authenticate"),
            'false';
        ['true'|_] ->
            lager:debug("is_authenticated: true"),
            'true';
        [{'true', _}|_] ->
            lager:debug("is_authenticated: true"),
            'true';
        [{'halt', _}|_] ->
            lager:debug("is_authenticated: halt"),
            'false'
    end.

-spec is_authorized(bh_context:context()) -> boolean().
is_authorized(Context) ->
    AuthEvent = <<"blackhole.authorize">>,
    case blackhole_bindings:succeeded(blackhole_bindings:map(AuthEvent, Context)) of
        [] ->
            lager:debug("failed to authorize"),
            'false';
        ['true'|_] ->
            lager:debug("is_authorized: true"),
            'true';
        [{'true', _}|_] ->
            lager:debug("is_authorized: true"),
            'true';
        [{'halt', _}|_] ->
            lager:debug("is_authorized: halt"),
            'false'
    end.
