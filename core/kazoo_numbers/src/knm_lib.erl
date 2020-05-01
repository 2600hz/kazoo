%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_lib).

-export([ensure_assign_to_option/1
        ,ensure_can_create/1
        ,ensure_can_load_to_create/1
        ,ensure_numbers_are_not_porting/2
        ,state_for_create/1
        ,allowed_creation_states/1, allowed_creation_states/2
        ]).

-export([feature_inbound_cname/1
        ,feature_prepend/1
        ,find_early_ringback/1
        ,find_transfer_ringback/1
        ,force_outbound_feature/1
        ,is_force_outbound/1
        ]).

-include("knm.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ensure_assign_to_option(knm_options:options()) ->
          kz_either:either(knm_errors:error(), knm_options:options()).
ensure_assign_to_option(Options) ->
    case kzs_util:is_account_id(knm_options:assign_to(Options)) of
        'true' ->
            {'ok', Options};
        'false' ->
            Reason = knm_errors:to_json('assign_failure', 'undefined', 'field_undefined'),
            {'error', Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ensure_can_create(knm_options:options()) ->
          kz_either:either(knm_errors:error(), knm_options:options()).
ensure_can_create(Options) ->
    lager:debug("ensuring requested options and state are valid for creating numbers"),
    SetState = fun(State) -> {'ok', props:set_value('state', State, Options)} end,
    kz_either:pipe(kz_either:bind(state_for_create(Options), SetState)
                  ,[fun ensure_assign_to_option/1
                   ,fun ensure_account_can_create/1
                   ]).

%% @private
-spec ensure_account_can_create(knm_options:options()) ->
          kz_either:either(knm_errors:error(), knm_options:options()).
ensure_account_can_create(Options) ->
    ensure_account_can_create(Options, knm_options:auth_by(Options)).

%% @private
-spec ensure_account_can_create(knm_options:options(), kz_term:api_ne_binary()) ->
          kz_either:either(knm_errors:error(), knm_options:options()).
ensure_account_can_create(Options, ?KNM_DEFAULT_AUTH_BY) ->
    {'ok', Options};
ensure_account_can_create(Options, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    case knm_options:ported_in(Options)
        orelse knm_options:state(Options) =:= ?NUMBER_STATE_PORT_IN
        orelse allow_number_additions(Options, AccountId)
        orelse knm_phone_number:is_admin(AccountId)
    of
        'true' ->
            {'ok', Options};
        'false' ->
            Cause = kz_binary:format("account '~s' is not allowed to create number", [AccountId]),
            Reason = knm_errors:to_json('unauthorized', 'undefined', Cause),
            {'error', Reason}
    end;
ensure_account_can_create(_, _NotAnAccountId) ->
    lager:debug("'~p' is not an account id", [_NotAnAccountId]),
    Cause = <<"cannot determined account is allowed to create number, account id is undefined">>,
    Reason = knm_errors:to_json('unauthorized', 'undefined', Cause),
    {'error', Reason}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ensure_numbers_are_not_porting(kz_term:ne_binaries(), knm_pipe:collection()) ->
          knm_pipe:collection().
ensure_numbers_are_not_porting(Nums, Collection) ->
    lager:debug("ensuring ~b numbers are not belong to any port request", [length(Nums)]),
    case knm_options:ported_in(knm_pipe:options(Collection)) of
        'true' ->
            handle_are_porting(Collection, Nums, {'ok', {[], Nums}});
        'false' ->
            handle_are_porting(Collection, Nums, knm_port_request:are_porting(Nums))
    end.

-type are_porting() :: kz_either:either(kazoo_data:data_error() | 'number_is_porting', {kz_term:ne_binaries(), kz_term:ne_binaries()}).
-spec handle_are_porting(knm_pipe:collection(), kz_term:ne_binaries(), are_porting()) ->
          knm_pipe:collection().
handle_are_porting(Collection, Nums, {'error', _DbError}) ->
    lager:debug("~b number(s) are portin: ~p", [length(Nums), _DbError]),
    FailedAcc = knm_pipe:failed(Collection),
    PortInErrors = maps:from_list([{Num, knm_errors:to_json('number_is_porting', Num)} || Num <- Nums]),
    knm_pipe:set_failed(Collection, maps:merge(FailedAcc, PortInErrors));
handle_are_porting(Collection, _, {'ok', {PortIn, NotPortIn}}) ->
    Options = knm_pipe:options(Collection),
    lager:debug("creating records for ~b number(s)", [length(NotPortIn)]),
    knm_pipe:add_succeeded(handle_are_porting(Collection, PortIn, {'error', 'number_is_porting'})
                          ,[knm_phone_number:from_number_with_options(Num, Options) || Num <- NotPortIn]
                          ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ensure_can_load_to_create(knm_pipe:collection()) -> knm_pipe:collection().
ensure_can_load_to_create(T) ->
    PNs = knm_pipe:todo(T),
    lager:debug("ensuring existing ~b numbers are in valid state to create"
               ,[length(PNs)]
               ),
    lists:foldl(fun ensure_load_create_state/2, knm_pipe:set_todo(T, []), PNs).

-spec ensure_load_create_state(knm_phone_number:record(), knm_pipe:collection()) ->
          knm_pipe:collection().
ensure_load_create_state(PN, Collection) ->
    AllowedStates = [?NUMBER_STATE_AGING
                    ,?NUMBER_STATE_AVAILABLE
                    ,?NUMBER_STATE_PORT_IN
                    ],
    State = knm_phone_number:state(PN),
    case lists:member(State, AllowedStates) of
        'true' ->
            Options = knm_pipe:options(Collection),
            Updates = knm_options:to_phone_number_setters(
                        props:delete('state', Options)
                       ),
            {'ok', Updated} = knm_phone_number:setters(PN, Updates),
            knm_pipe:add_success(Collection, Updated);
        'false' ->
            Num = knm_phone_number:number(PN),
            lager:error("number '~s' is in wrong state '~s' for creating", [Num, State]),
            knm_pipe:set_failed(Collection, Num, knm_errors:to_json('number_exists', Num))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec state_for_create(knm_options:options()) ->
          kz_either:either(knm_errors:error(), kz_term:ne_binary()).
state_for_create(Options) ->
    state_for_create(Options
                    ,knm_options:state(Options, ?NUMBER_STATE_IN_SERVICE)
                    ,knm_options:ported_in(Options)
                    ,knm_options:module_name(Options)
                    ).

%% @private
-spec state_for_create(knm_options:options(), kz_term:api_ne_binary(), boolean(), kz_term:api_ne_binary()) ->
          kz_either:either(knm_errors:error(), kz_term:ne_binary()).
state_for_create(_, ?NUMBER_STATE_PORT_IN = State, _, _) ->
    {'ok', State};
state_for_create(_, _, 'true', _) ->
    {'ok', ?NUMBER_STATE_IN_SERVICE};
state_for_create(_, _, _, ?CARRIER_MDN) ->
    {'ok', ?NUMBER_STATE_IN_SERVICE};
state_for_create(Options, State, _, _) ->
    AuthBy = knm_options:auth_by(Options),
    case lists:member(State, allowed_creation_states(Options, AuthBy)) of
        'true' ->
            lager:debug("picking state ~s for creating number(s) authorized by ~s", [State, AuthBy]),
            {'ok', State};
        'false' ->
            Cause = kz_binary:format("creating number in state '~s' is not allowed", [State]),
            Reason = knm_errors:to_json('unauthorized', 'undefined', Cause),
            {'error', Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allowed_creation_states(kz_term:api_ne_binary()) -> kz_term:ne_binaries().
allowed_creation_states(AuthBy) ->
    allowed_creation_states([], AuthBy).

-spec allowed_creation_states(knm_options:options(), kz_term:api_ne_binary()) -> kz_term:ne_binaries().
allowed_creation_states(_, 'undefined') -> [];
allowed_creation_states(Options, AuthBy) ->
    case {knm_phone_number:is_admin(AuthBy)
         ,allow_number_additions(Options, AuthBy)
         }
    of
        {'true', _} ->
            [?NUMBER_STATE_AGING
            ,?NUMBER_STATE_AVAILABLE
            ,?NUMBER_STATE_IN_SERVICE
            ,?NUMBER_STATE_PORT_IN
            ,?NUMBER_STATE_RESERVED
            ];
        {'false', 'true'} ->
            [?NUMBER_STATE_IN_SERVICE
            ,?NUMBER_STATE_RESERVED
            ];
        _ -> []
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec feature_prepend(knm_phone_number:record()) -> kz_term:api_binary().
feature_prepend(PhoneNumber) ->
    Prepend = knm_phone_number:feature(PhoneNumber, ?FEATURE_PREPEND),
    case kz_json:is_true(?PREPEND_ENABLED, Prepend) of
        'false' -> 'undefined';
        'true' -> kz_json:get_ne_value(?PREPEND_NAME, Prepend)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec feature_inbound_cname(knm_phone_number:record()) -> boolean().
feature_inbound_cname(PhoneNumber) ->
    case knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_INBOUND) of
        'undefined' -> 'false';
        _ ->
            Mod = knm_phone_number:module_name(PhoneNumber),
            Module = kz_term:to_atom(Mod, 'true'),
            try Module:should_lookup_cnam() of
                Boolean -> kz_term:is_true(Boolean)
            catch
                _E:_R -> 'true'
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_early_ringback(knm_phone_number:record()) -> kz_term:api_binary().
find_early_ringback(PhoneNumber) ->
    RingBack = knm_phone_number:feature(PhoneNumber, ?FEATURE_RINGBACK),
    kz_json:get_ne_value(?RINGBACK_EARLY, RingBack).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_transfer_ringback(knm_phone_number:record()) -> kz_term:api_binary().
find_transfer_ringback(PhoneNumber) ->
    RingBack = knm_phone_number:feature(PhoneNumber, ?FEATURE_RINGBACK),
    kz_json:get_ne_value(?RINGBACK_TRANSFER, RingBack).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_force_outbound(knm_phone_number:record()) -> boolean().
is_force_outbound(PN) ->
    is_force_outbound(knm_phone_number:state(PN)
                     ,knm_phone_number:module_name(PN)
                     ,force_outbound_feature(PN)
                     ).

-spec is_force_outbound(kz_term:ne_binary(), kz_term:ne_binary(), boolean()) -> boolean().
is_force_outbound(?NUMBER_STATE_PORT_IN, Module, _ForceOutbound) ->
    knm_config:should_force_port_in_outbound()
        orelse force_module_outbound(Module);
is_force_outbound(?NUMBER_STATE_PORT_OUT, Module, _ForceOutbound) ->
    knm_config:should_force_port_out_outbound()
        orelse force_module_outbound(Module);
is_force_outbound(_State, ?CARRIER_LOCAL, _ForceOutbound) ->
    force_local_outbound();
is_force_outbound(_State, ?CARRIER_MDN, _ForceOutbound) ->
    force_local_outbound();
is_force_outbound(_State, _Module, ForceOutbound) ->
    ForceOutbound.

%% FIXME: move to kpn
-spec force_outbound_feature(knm_phone_number:record()) -> boolean().
force_outbound_feature(PN) ->
    case knm_phone_number:feature(PN, ?FEATURE_FORCE_OUTBOUND) of
        'undefined' -> knm_config:should_force_outbound();
        FO -> kz_term:is_true(FO)
    end.

-spec force_module_outbound(kz_term:ne_binary()) -> boolean().
force_module_outbound(?CARRIER_LOCAL) -> force_local_outbound();
force_module_outbound(?CARRIER_MDN) -> force_local_outbound();
force_module_outbound(_Mod) -> 'false'.

-spec force_local_outbound() -> boolean().
force_local_outbound() ->
    knm_config:should_force_local_outbound().

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-ifdef(TEST).
%% TODO: this is required to simulate reseller account without number_allowed_addition
%% Remove this after fixturedb supports save operation
-define(LOAD_ACCOUNT(Options, AccountId)
       ,(case props:get_value(<<"auth_by_account">>, Options) of
             'undefined' -> kzd_accounts:fetch(AccountId);
             AccountJObj ->
                 {'ok', Fetched} = kzd_accounts:fetch(AccountId),
                 {'ok', kz_json:merge(Fetched, AccountJObj)}
         end)
       ).
-else.
-define(LOAD_ACCOUNT(_Options, AccountId)
       ,kzd_accounts:fetch(AccountId)
       ).
-endif.

-spec allow_number_additions(knm_options:options(), kz_term:ne_binary()) -> boolean().
allow_number_additions(_Options, ?KNM_DEFAULT_AUTH_BY) ->
    'true';
allow_number_additions(_Options, _AccountId) ->
    {'ok', JObj} = ?LOAD_ACCOUNT(_Options, _AccountId),
    kzd_accounts:allow_number_additions(JObj).
