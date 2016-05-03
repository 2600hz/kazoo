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
         ,module_name/1
         ,ported_in/1, ported_in/2
         ,public_fields/1
         ,state/1, state/2
         ,should_delete/1, should_delete/2

         ,default/0
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

-type option() :: {'assign_to', ne_binary()} |
                  {'assigned_to', ne_binary()} |
                  {'auth_by', ne_binary()} |
                  {'dry_run', boolean()} |
                  {'module_name', ne_binary()} |
                  {'ported_in', boolean()} |
                  {'public_fields', kz_json:object()} |
                  {'state', ne_binary()} |
                  {'should_delete', boolean()}.

-type options() :: [option()].

-type extra_option() :: {'account_id', ne_binary()} | %%api
                        {'force_outbound', boolean()} |
                        {'inbound_cnam', boolean()} |
                        {'local', boolean()} |
                        {'number', ne_binary()} | %%api
                        {'pending_port', boolean()} |
                        {'prepend', api(binary())} | %%|false
                        {'ringback_media', api(binary())} |
                        {'transfer_media', api(binary())}.

-type extra_options() :: [extra_option()].

-export_type([option/0, options/0
             ,extra_option/0, extra_options/0
             ]).

-spec default() -> options().
default() ->
    [{'auth_by', ?KNM_DEFAULT_AUTH_BY}
     ,{'dry_run', 'false'}
    ].

-spec dry_run(options()) -> boolean().
-spec dry_run(options(), Default) -> boolean() | Default.
dry_run(Options) ->
    dry_run(Options, 'false').
dry_run(Options, Default) ->
    R = props:get_is_true('dry_run', Options, Default),
    _ = R andalso lager:debug("dry_run-ing btw"),
    R.

-spec assigned_to(options()) -> api(binary()).
-spec assigned_to(options(), Default) -> ne_binary() | Default.
assigned_to(Options) ->
    assigned_to(Options, 'undefined').
assigned_to(Options, Default) ->
    props:get_binary_value('assigned_to', Options, Default).

-spec assign_to(options()) -> api(binary()).
-spec assign_to(options(), Default) -> ne_binary() | Default.
assign_to(Options) ->
    assign_to(Options, 'undefined').
assign_to(Options, Default) ->
    props:get_binary_value('assign_to', Options, Default).

-spec auth_by(options()) -> api(binary()).
-spec auth_by(options(), Default) -> ne_binary() | Default.
auth_by(Options) ->
    auth_by(Options, 'undefined').
auth_by(Options, Default) ->
    props:get_binary_value('auth_by', Options, Default).

-spec public_fields(options()) -> api_object().
public_fields(Options) ->
    props:get_value('public_fields', Options, kz_json:new()).

-spec state(options()) -> api(binary()).
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
module_name(Options) ->
    case props:get_ne_binary_value('module_name', Options) of
        'undefined' -> knm_carriers:default_carrier();
        ModuleName -> ModuleName
    end.

-spec should_delete(options()) -> boolean().
-spec should_delete(options(), Default) -> boolean() | Default.
should_delete(Options) ->
    should_delete(Options, 'false').
should_delete(Options, Default) ->
    props:is_true('should_delete', Options, Default).


%%--------------------------------------------------------------------
%% Public get/set extra_options()
%%--------------------------------------------------------------------
-spec account_id(extra_options()) -> ne_binary().
account_id(Props) when is_list(Props) ->
    props:get_ne_binary_value('account_id', Props).

-spec set_account_id(extra_options(), ne_binary()) -> extra_options().
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

-spec number(extra_options()) -> ne_binary().
number(Props) when is_list(Props) ->
    props:get_ne_binary_value('number', Props).

-spec prepend(extra_options()) -> api(binary()).
prepend(Props) when is_list(Props) ->
    props:get_value('prepend', Props).

-spec ringback_media_id(extra_options()) -> api(binary()).
ringback_media_id(Props) when is_list(Props) ->
    props:get_value('ringback_media', Props).

-spec should_force_outbound(extra_options()) -> boolean().
should_force_outbound(Props) when is_list(Props) ->
    props:get_is_true('force_outbound', Props).

-spec transfer_media_id(extra_options()) -> api(binary()).
transfer_media_id(Props) when is_list(Props) ->
    props:get_value('transfer_media', Props).
%%--------------------------------------------------------------------
