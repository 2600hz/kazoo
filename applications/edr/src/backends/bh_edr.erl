%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017, Conversant Ltd
%%% @doc EDR.
%%%
%%% <div class="notice">{@link edr_be_amqp} must be running, otherwise no events will come
%%% through. You can request events with a verbosity or severity
%%% greater than what `edr_be_amqp' has bound to, however you won't
%%% receive any of them.</div>
%%%
%%% @author Max Lay
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_edr).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include_lib("kazoo_edr/include/edr.hrl").
-include("blackhole/src/blackhole.hrl").

-spec init() -> any().
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.edr">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.edr">>, ?MODULE, 'bindings').

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [Severity, Verbosity, _AppName]}) ->
    case {lists:member(Severity, ?EDR_SEVERITY_BINARIES), lists:member(Verbosity, ?EDR_VERBOSITY_BINARIES)} of
        {'false', _} ->
            bh_context:add_error(Context, <<"severity must be one of: ", (kz_binary:join(?EDR_SEVERITY_BINARIES))/binary>>);
        {_, 'false'} ->
            bh_context:add_error(Context, <<"verbosity must be one of: ", (kz_binary:join(?EDR_VERBOSITY_BINARIES))/binary>>);
        {'true', 'true'} ->
            Context
    end;
validate(Context, #{keys := _Keys}) ->
    bh_context:add_error(Context, <<"subscription should be in format: edr.severity.verbosity.app_name">>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,keys := [Severity, Verbosity, AppName]
                    }=Map) ->
    Requested = <<"edr.", Severity/binary, ".", Verbosity/binary, ".", AppName/binary>>,
    Binding = #edr_binding{account_id=AccountId
                          ,severity=kz_term:to_atom(Severity)
                          ,verbosity=kz_term:to_atom(Verbosity)
                          ,app_name=AppName
                          },
    Subscribed = edr_bindings:binding_keys(Binding),
    BindingOptions = kz_json:to_proplist(edr_bindings:bindings_to_json(Binding)),
    Listeners = [{'amqp', 'edr_amqp', BindingOptions}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.
