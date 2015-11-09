%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_number_options).

-export([assign_to/1, assign_to/2
         ,assigned_to/1, assigned_to/2
         ,auth_by/1, auth_by/2
         ,dry_run/1, dry_run/2
         ,module_name/1, module_name/2
         ,ported_in/1, ported_in/2
         ,public_fields/1, public_fields/2
         ,state/1, state/2

         ,default/0
        ]).

-include("knm.hrl").

-type option() :: {'assign_to', ne_binary()} |
                  {'assigned_to', ne_binary()} |
                  {'auth_by', ne_binary()} |
                  {'dry_run', boolean()} |
                  {'module_name', ne_binary()} |
                  {'ported_in', boolean()} |
                  {'public_fields', wh_json:object()} |
                  {'state', ne_binary()}.

-type options() :: [option()].

-export_type([option/0, options/0]).

-spec default() -> options().
default() ->
    [{'auth_by', ?DEFAULT_AUTH_BY}
     ,{'dry_run', 'false'}
    ].

-spec dry_run(options()) -> boolean().
-spec dry_run(options(), Default) -> boolean() | Default.
dry_run(Options) ->
    dry_run(Options, 'false').
dry_run(Options, Default) ->
    props:get_is_true('dry_run', Options, Default).

-spec assigned_to(options()) -> api_binary().
-spec assigned_to(options(), Default) -> ne_binary() | Default.
assigned_to(Options) ->
    assigned_to(Options, 'undefined').
assigned_to(Options, Default) ->
    props:get_binary_value('assigned_to', Options, Default).

-spec assign_to(options()) -> api_binary().
-spec assign_to(options(), Default) -> ne_binary() | Default.
assign_to(Options) ->
    assign_to(Options, 'undefined').
assign_to(Options, Default) ->
    props:get_binary_value('assign_to', Options, Default).

-spec auth_by(options()) -> api_binary().
-spec auth_by(options(), Default) -> ne_binary() | Default.
auth_by(Options) ->
    auth_by(Options, 'undefined').
auth_by(Options, Default) ->
    props:get_binary_value('auth_by', Options, Default).

-spec public_fields(options()) -> api_object().
-spec public_fields(options(), Default) -> wh_json:object() | Default.
public_fields(Options) ->
    public_fields(Options, 'undefined').
public_fields(Options, Default) ->
    props:get_binary_value('public_fields', Options, Default).

-spec state(options()) -> api_binary().
-spec state(options(), Default) -> ne_binary() | Default.
state(Options) ->
    state(Options, 'undefined').
state(Options, Default) ->
    props:get_binary_value('state', Options, Default).

-spec ported_in(options()) -> boolean().
-spec ported_in(options(), Default) -> boolean() | Default.
ported_in(Options) ->
    ported_in(Options, 'false').
ported_in(Options, Default) ->
    props:get_is_true('ported_in', Options, Default).

-spec module_name(options()) -> ne_binary().
-spec module_name(options(), Default) -> ne_binary() | Default.
module_name(Options) ->
    module_name(Options, knm_carriers:default_carrier()).
module_name(Options, Default) ->
    props:get_binary_value('module_name', Options, Default).
