%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_number_options).

-export([assign_to/1, assign_to/2, set_assign_to/2
        ,auth_by/1, auth_by/2
        ,dry_run/1, dry_run/2
        ,batch_run/1, batch_run/2
        ,mdn_run/1
        ,module_name/1
        ,ported_in/1
        ,public_fields/1
        ,state/1, state/2
        ,crossbar/1, crossbar/2

        ,default/0
        ,mdn_options/0

        ,to_phone_number_setters/1
        ]).

-export([account_id/1, set_account_id/2
        ,has_pending_port/1
        ,inbound_cnam_enabled/1
        ,is_local_number/1
        ,number/1
        ,prepend/1
        ,ringback_media_id/1
        ,should_force_outbound/1
        ,transfer_media_id/1
        ]).

-include("knm.hrl").

-type crossbar_option() :: {'services', kz_services:services()} |
                           {'account_id', kz_term:api_ne_binary()} |
                           {'reseller_id', kz_term:api_ne_binary()}.
-type crossbar_options() :: [crossbar_option()].

-type option() :: {'assign_to', kz_term:api_ne_binary()} |
                  {'auth_by', kz_term:api_ne_binary()} |
                  {'batch_run', boolean()} |
                  {'crossbar', crossbar_options()} |
                  {'dry_run', boolean()} |
                  {'mdn_run', boolean()} |
                  {'module_name', kz_term:ne_binary()} |
                  {'ported_in', boolean()} |
                  {'public_fields', kz_json:object()} |
                  {'state', kz_term:ne_binary()}.

-type options() :: [option()].

-type extra_option() :: {'account_id', kz_term:ne_binary()} | %%api
                        {'force_outbound', boolean()} |
                        {'inbound_cnam', boolean()} |
                        {'local', boolean()} |
                        {'number', kz_term:ne_binary()} | %%api
                        {'pending_port', boolean()} |
                        {'prepend', kz_term:api_binary()} | %%|false
                        {'ringback_media', kz_term:api_binary()} |
                        {'transfer_media', kz_term:api_binary()}.

-type extra_options() :: [extra_option()].

-export_type([option/0, options/0
             ,extra_option/0, extra_options/0
             ]).

-spec default() -> options().
default() ->
    [{'auth_by', ?KNM_DEFAULT_AUTH_BY}
    ,{'batch_run', 'false'}
    ,{'mdn_run', 'false'}
    ].

-spec mdn_options() -> options().
mdn_options() ->
    props:set_value('mdn_run', 'true', default()).

-spec to_phone_number_setters(options()) -> knm_phone_number:set_functions().
to_phone_number_setters(Options) ->
    [{setter_fun(Option), Value}
     || {Option, Value} <- props:unique(Options),
        is_atom(Option),
        Option =/= 'crossbar'
    ].

-spec setter_fun(atom()) -> knm_phone_number:set_function().
setter_fun('public_fields') ->
    fun knm_phone_number:reset_doc/2;
setter_fun(Option) ->
    FName = list_to_existing_atom("set_" ++ atom_to_list(Option)),
    fun knm_phone_number:FName/2.

-spec dry_run(options()) -> boolean().
dry_run(Options) ->
    dry_run(Options, 'false').

-spec dry_run(options(), Default) -> boolean() | Default.
dry_run(Options, Default) ->
    maybe_dry_run(props:get_is_true('dry_run', Options, Default)).

-spec maybe_dry_run(DryRun) -> DryRun
              when DryRun :: boolean().
maybe_dry_run('true') ->
    lager:debug("dry_run-ing btw"),
    'true';
maybe_dry_run(DryRun) -> DryRun.

-spec batch_run(options()) -> boolean().
batch_run(Options) ->
    batch_run(Options, 'false').

-spec batch_run(options(), Default) -> boolean() | Default.
batch_run(Options, Default) ->
    R = props:get_is_true('batch_run', Options, Default),
    _ = R
        andalso lager:debug("batch_run-ing btw"),
    R.

-spec mdn_run(options()) -> boolean().
mdn_run(Options) ->
    R = props:get_is_true('mdn_run', Options, 'false'),
    _ = R
        andalso lager:debug("mdn_run-ing btw"),
    R.

-spec assign_to(options()) -> kz_term:api_binary().
assign_to(Options) ->
    assign_to(Options, 'undefined').

-spec assign_to(options(), Default) -> kz_term:ne_binary() | Default.
assign_to(Options, Default) ->
    props:get_binary_value('assign_to', Options, Default).

-spec set_assign_to(options(), kz_term:ne_binary()) -> options().
set_assign_to(Options, AssignTo) ->
    props:set_value('assign_to', AssignTo, Options).

-spec auth_by(options()) -> kz_term:api_binary().
auth_by(Options) ->
    auth_by(Options, 'undefined').

-spec auth_by(options(), Default) -> kz_term:ne_binary() | Default.
auth_by(Options, Default) ->
    props:get_binary_value('auth_by', Options, Default).

-spec public_fields(options()) -> kz_term:api_object().
public_fields(Options) ->
    props:get_value('public_fields', Options, kz_json:new()).

-spec state(options()) -> kz_term:api_ne_binary().
state(Options) ->
    state(Options, 'undefined').

-spec state(options(), Default) -> kz_term:ne_binary() | Default.
state(Options, Default) ->
    props:get_binary_value('state', Options, Default).

-spec crossbar(options()) -> kz_term:api_proplist().
crossbar(Options) ->
    crossbar(Options, 'undefined').

-spec crossbar(options(), Default) -> kz_term:api_proplist() | Default.
crossbar(Options, Default) ->
    props:get_value('crossbar', Options, Default).

-spec ported_in(options()) -> boolean().
ported_in(Options) ->
    props:get_is_true('ported_in', Options, 'false').

-spec module_name(options()) -> kz_term:ne_binary().
module_name(Options) ->
    case props:get_ne_binary_value('module_name', Options) of
        'undefined' -> knm_carriers:default_carrier();
        ModuleName -> ModuleName
    end.

%%------------------------------------------------------------------------------
%% Public get/set extra_options()
%%------------------------------------------------------------------------------
-spec account_id(extra_options()) -> kz_term:ne_binary().
account_id(Props) when is_list(Props) ->
    props:get_ne_binary_value('account_id', Props).

-spec set_account_id(extra_options(), kz_term:ne_binary()) -> extra_options().
set_account_id(Props, AccountId=?MATCH_ACCOUNT_RAW(_)) when is_list(Props) ->
    props:set_value('account_id', AccountId, Props).

-spec has_pending_port(extra_options()) -> boolean().
has_pending_port(Props) when is_list(Props) ->
    props:get_is_true('pending_port', Props).

-spec inbound_cnam_enabled(extra_options()) -> boolean().
inbound_cnam_enabled(Props) when is_list(Props) ->
    props:get_is_true('inbound_cnam', Props).

-spec is_local_number(extra_options()) -> boolean().
is_local_number(Props) when is_list(Props) ->
    props:get_is_true('local', Props).

-spec number(extra_options()) -> kz_term:ne_binary().
number(Props) when is_list(Props) ->
    props:get_ne_binary_value('number', Props).

-spec prepend(extra_options()) -> kz_term:api_binary().
prepend(Props) when is_list(Props) ->
    props:get_value('prepend', Props).

-spec ringback_media_id(extra_options()) -> kz_term:api_binary().
ringback_media_id(Props) when is_list(Props) ->
    props:get_value('ringback_media', Props).

-spec should_force_outbound(extra_options()) -> boolean().
should_force_outbound(Props) when is_list(Props) ->
    props:get_is_true('force_outbound', Props).

-spec transfer_media_id(extra_options()) -> kz_term:api_binary().
transfer_media_id(Props) when is_list(Props) ->
    props:get_value('transfer_media', Props).
