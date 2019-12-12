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
-module(knm_numbers).

%% CRUD Operations
-export([create/2
        ,delete/2
        ,get/1, get/2
        ,release/1, release/2
        ,update/2, update/3
        ]).

%% Special CRUD operations
-export([assign_to_app/2, assign_to_app/3
        ,lookup_account/1
        ,move/2, move/3
        ,reserve/2
        ]).

%% Other operations
-export([account_listing/1
        ,emergency_enabled/1
        ,free/1
        ]).

-include("knm.hrl").

%% {{{ Types
-type lookup_error() :: 'not_reconcilable' |
                        'not_found' |
                        'unassigned' |
                        {'not_in_service', kz_term:ne_binary()} |
                        {'account_disabled', kz_term:ne_binary()}.

-type lookup_return() :: {'ok', kz_term:ne_binary(), knm_options:extra_options()} |
                         {'error', lookup_error()}.

-type number_thing() :: kz_term:ne_binary() | kz_term:ne_binaries().

-type return() :: {'ok', kz_json:objects()} |
                  {'ok', kz_json:objects(), knm_errors:proplist()} |
                  {'dry_run', knm_pipe:quotes()}.
%% }}}

-export_type([lookup_error/0
             ,lookup_return/0
             ,return/0
             ]).

%%%=============================================================================
%%% CRUD functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Attempts to create a new single number or a list of numbers in DB or
%% modify ones.
%% @end
%%------------------------------------------------------------------------------
-spec create(number_thing(), knm_options:options()) -> return().
create(Num, Options) when is_binary(Num) ->
    handle_result(knm_ops:create([Num], Options));
create(Nums, Options) when is_list(Nums) ->
    handle_result(knm_ops:create(Nums, Options)).

%%------------------------------------------------------------------------------
%% @doc Remove number(s) from the system without doing any state checking.
%%
%% Sounds too harsh for you? You are looking for release/1,2.
%% @end
%%------------------------------------------------------------------------------
-spec delete(number_thing(), knm_options:options()) -> return().
delete(Num, Options) when is_binary(Num) ->
    handle_result(knm_ops:delete([Num], Options));
delete(Nums, Options) when is_list(Nums) ->
    handle_result(knm_ops:delete(Nums, Options)).

%%------------------------------------------------------------------------------
%% @doc Attempts to get a single or a list of numbers from DB.
%% @end
%%------------------------------------------------------------------------------
-spec get(number_thing()) -> return().
get(Num) -> get(Num, knm_options:default()).

-spec get(number_thing(), knm_options:options()) -> return().
get(Num, Options) when is_binary(Num) ->
    handle_result(knm_ops:get([Num], Options));
get(Nums, Options) when is_list(Nums) ->
    handle_result(knm_ops:get(Nums, Options)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec release(number_thing()) -> return().
release(Num) -> release(Num, knm_options:default()).

-spec release(number_thing(), knm_options:options()) -> return().
release(Num, Options) when is_binary(Num) ->
    handle_result(knm_ops:release([Num], Options));
release(Nums, Options) when is_list(Nums) ->
    handle_result(knm_ops:release(Nums, Options)).

%%------------------------------------------------------------------------------
%% @doc Attempts to update some phone_number fields.
%%
%% <div class="notice">will always result in a phone_number save.</div>
%% @end
%%------------------------------------------------------------------------------
-spec update(number_thing(), knm_phone_number:set_functions()) -> return().
update(Num, Routines) ->
    update(Num, Routines, knm_options:default()).

-spec update(number_thing(), knm_phone_number:set_functions(), knm_options:options()) -> return().
update(Num, Routines, Options) when is_binary(Num) ->
    handle_result(knm_ops:update([Num], Routines, Options));
update(Nums, Routines, Options) when is_list(Nums) ->
    handle_result(knm_ops:update(Nums, Routines, Options)).

%%%=============================================================================
%%% Special CRUD operation functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec assign_to_app(number_thing(), kz_term:api_ne_binary()) -> return().
assign_to_app(Num, App) ->
    assign_to_app(Num, App, knm_options:default()).

-spec assign_to_app(number_thing(), kz_term:api_ne_binary(), knm_options:options()) -> return().
assign_to_app(Num, App, Options) when is_binary(Num) ->
    handle_result(knm_ops:assign_to_app([Num], App, Options));
assign_to_app(Nums, App, Options) when is_list(Nums) ->
    handle_result(knm_ops:assign_to_app(Nums, App, Options)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec lookup_account(kz_term:api_ne_binary()) -> lookup_return().
lookup_account('undefined') ->
    {'error', 'not_reconcilable'};
lookup_account(Num) ->
    NormalizedNum = knm_converters:normalize(Num),
    Key = {'account_lookup', NormalizedNum},
    case kz_cache:peek_local(?CACHE_NAME, Key) of
        {'ok', Ok} -> Ok;
        {'error', 'not_found'} ->
            case fetch_account_from_number(NormalizedNum) of
                {'ok', _, _}=Ok ->
                    NumberDb = knm_converters:to_db(NormalizedNum),
                    CacheProps = [{'origin', [{'db', NumberDb, NormalizedNum}]}],
                    kz_cache:store_local(?CACHE_NAME, Key, Ok, CacheProps),
                    Ok;
                Else -> Else
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec move(number_thing(), kz_term:ne_binary()) -> return().
move(Num, MoveTo) ->
    move(Num, MoveTo, knm_options:default()).

-spec move(number_thing(), kz_term:ne_binary(), knm_options:options()) -> return().
move(Num, MoveTo, Options) when is_binary(Num) ->
    handle_result(knm_ops:move([Num], MoveTo, Options));
move(Nums, MoveTo, Options) when is_list(Nums) ->
    handle_result(knm_ops:move(Nums, MoveTo, Options)).

%%------------------------------------------------------------------------------
%% @doc Fetches then transitions existing number(s) to the reserved state.
%% @end
%%------------------------------------------------------------------------------
-spec reserve(number_thing(), knm_options:options()) -> return().
reserve(Num, Options) when is_binary(Num) ->
    handle_result(knm_ops:reserve([Num], Options));
reserve(Nums, Options) when is_list(Nums) ->
    handle_result(knm_ops:reserve(Nums, Options)).

%%%=============================================================================
%%% Other functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc List an account's phone numbers and statuses.
%% Does not go through sub accounts.
%% @end
%%------------------------------------------------------------------------------
-spec account_listing(kz_term:ne_binary()) -> [{kz_term:ne_binary(), kz_json:object()}].
account_listing(<<Account/binary>>) ->
    AccountDb = kzs_util:format_account_db(Account),
    case kz_datamgr:get_results(AccountDb, <<"phone_numbers/crossbar_listing">>) of
        {'ok', []} ->
            lager:debug("account ~s holds no numbers", [AccountDb]),
            [];
        {'ok', JObjs} ->
            [{kz_doc:id(JObj), kz_json:get_value(<<"value">>, JObj)}
             || JObj <- JObjs
            ];
        {'error', 'not_found'=_R} ->
            lager:error("error listing numbers for ~s: ~p", [AccountDb, _R]),
            [];
        {'error', R} ->
            lager:error("error listing numbers for ~s: ~p", [AccountDb, R]),
            %% FIXME: why throw?
            throw(R)
    end.

%%------------------------------------------------------------------------------
%% @doc Find an account's phone numbers that have emergency services enabled
%% @end
%%------------------------------------------------------------------------------
-spec emergency_enabled(kz_term:ne_binary()) -> kz_term:ne_binaries().
emergency_enabled(<<Account/binary>>) ->
    [Num || {Num, JObj} <- account_listing(Account),
            Features <- [kz_json:get_list_value(<<"features">>, JObj, [])],
            lists:member(?FEATURE_E911, Features)
    ].

%%------------------------------------------------------------------------------
%% @doc Release all of an account's numbers
%% @end
%%------------------------------------------------------------------------------
-spec free(kz_term:ne_binary()) -> 'ok'.
free(<<Account/binary>>) ->
    AccountDb = kzs_util:format_account_db(Account),
    {Numbers, _NumbersData} = lists:unzip(account_listing(AccountDb)),
    Collection = knm_ops:release(Numbers),
    Ns = knm_pipe:succeeded(Collection),
    Failed = knm_pipe:failed(Collection),
    lager:debug("successfully released ~p from ~s", [[knm_phone_number:number(N) || N <- Ns], Account]),
    lists:foreach(fun ({Num, R}) ->
                          lager:error("error when releasing ~s from ~s: ~p", [Num, Account, R])
                  end
                 ,maps:to_list(Failed)
                 ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_result(knm_pipe:collection()) -> return().
handle_result(Collection) ->
    Successes = [knm_phone_number:to_public_json(PN)
                 || PN <- knm_pipe:succeeded(Collection)
                ],
    case {knm_options:dry_run(knm_pipe:options(Collection))
         ,kz_term:is_empty(knm_pipe:failed(Collection))
         }
    of
        {'true', 'true'} ->
            {'dry_run', knm_pipe:quotes(Collection)};
        {'false', 'true'} ->
            {'ok', Successes};
        {_, 'false'} ->
            {'ok', Successes, knm_errors:failed_to_proplist(knm_pipe:failed(Collection))}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_account_from_number(kz_term:ne_binary()) -> lookup_return().
fetch_account_from_number(Num) ->
    case knm_phone_number:fetch(Num) of
        {'ok', PN} -> check_number(PN);
        {'error', _}=Error -> maybe_fetch_account_from_ports(Num, Error)
    end.

-spec check_number(knm_phone_number:record()) -> lookup_return().
check_number(PN) ->
    AssignedTo = knm_phone_number:assigned_to(PN),
    case kz_term:is_empty(AssignedTo) of
        'true' -> {'error', 'unassigned'};
        'false' ->
            States = [?NUMBER_STATE_PORT_IN
                     ,?NUMBER_STATE_IN_SERVICE
                     ,?NUMBER_STATE_PORT_OUT
                     ,?NUMBER_STATE_RESERVED
                     ],
            case lists:member(knm_phone_number:state(PN), States) of
                'true' -> check_account(PN);
                'false' -> {'error', {'not_in_service', AssignedTo}}
            end
    end.

-spec check_account(knm_phone_number:record()) -> lookup_return().
check_account(PN) ->
    AssignedTo = knm_phone_number:assigned_to(PN),
    case kzd_accounts:is_enabled(AssignedTo) of
        'false' -> {'error', {'account_disabled', AssignedTo}};
        'true' ->
            Props = [{'pending_port', knm_phone_number:state(PN) =:= ?NUMBER_STATE_PORT_IN}
                    ,{'local', knm_phone_number:module_name(PN) =:= ?CARRIER_LOCAL}
                    ,{'number', knm_phone_number:number(PN)}
                    ,{'account_id', AssignedTo}
                    ,{'prepend', knm_lib:feature_prepend(PN)}
                    ,{'inbound_cnam', knm_lib:feature_inbound_cname(PN)}
                    ,{'ringback_media', knm_lib:find_early_ringback(PN)}
                    ,{'transfer_media', knm_lib:find_transfer_ringback(PN)}
                    ,{'force_outbound', knm_lib:is_force_outbound(PN)}
                    ],
            {'ok', AssignedTo, Props}
    end.

-spec maybe_fetch_account_from_ports(kz_term:ne_binary(), {'error', any()}) -> lookup_return().
maybe_fetch_account_from_ports(Num, Error) ->
    case knm_config:should_fetch_account_from_ports() of
        'false' -> Error;
        'true' -> fetch_account_from_ports(Num, Error)
    end.

-spec fetch_account_from_ports(kz_term:ne_binary(), {'error', any()}) -> lookup_return().
fetch_account_from_ports(Num, Error) ->
    case knm_port_request:get(Num) of
        {'error', _E} -> Error;
        {'ok', Port} ->
            AccountId = kz_doc:account_id(Port),
            Props = [{'pending_port', 'true'}
                    ,{'local', 'true'}
                    ,{'number', Num}
                    ,{'account_id', AccountId}
                     %% No prepend
                    ,{'inbound_cnam', 'false'}
                     %% No ringback_media
                     %% No transfer_media
                    ,{'force_outbound', 'true'}
                    ],
            {'ok', AccountId, Props}
    end.
