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
-export([maybe_add_binding_to_listener/3
         ,maybe_rm_binding_from_listener/3
        ]).
-export([respond_with_error/1, respond_with_authn_failure/1]).
-export([get_callback_module/1]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client has
%% provided a valid authentication token
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(bh_context:context()) -> boolean().
is_authorized(Context) ->
    lager:debug("context ~p", [Context]),
    Event = <<"blackhole.authenticate">>,
    case blackhole_bindings:succeeded(blackhole_bindings:map(Event, Context)) of
        [] ->
            lager:debug("failed to authenticate"),
            'true';
        ['true'|_] ->
            lager:debug("is_authentic: true"),
            'true';
        [{'halt', _}|_] ->
            lager:debug("is_authentic: halt"),
            'false'
    end.

-spec maybe_add_binding_to_listener(ne_binary(), ne_binary(), bh_context:context()) -> 'ok'.
maybe_add_binding_to_listener(Module, Binding, Context) ->
    try Module:add_amqp_binding(Binding, Context) of
        _ -> 'ok'
    catch
        _:_ -> 'ok'
    end.

maybe_rm_binding_from_listener(Module, Binding, Context) ->
    try Module:rm_amqp_binding(Binding, Context) of
        _ -> 'ok'
    catch
        _:_ -> 'ok'
    end.

-spec get_callback_module(ne_binary()) -> atom().
get_callback_module(Binding) ->
    case binary:split(Binding, <<".">>) of
        [M, _] ->
            try wh_util:to_atom(<<"bh_", M/binary>>, 'true') of
                Module -> 
                    lager:debug("compiled module name ~s", [Module]),
                    Module
            catch
                'error':'badarg' -> 'undefined'
            end;
        _ -> 'undefined'
    end.

respond_with_error(_Context) ->
    lager:debug("error here").

-spec respond_with_authn_failure(bh_context:context()) -> 'ok'.
respond_with_authn_failure(Context) ->
    Token = bh_context:auth_token(Context),
    lager:debug("authn failure token ~s", [Token]).
