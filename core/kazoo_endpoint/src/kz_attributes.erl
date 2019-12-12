%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_attributes).

-export([temporal_rules/1]).
-export([groups/1, groups/2]).
-export([caller_id_type/1, caller_id/1, caller_id/2]).
-export([callee_id/2]).
-export([get_endpoint_cid/2, get_endpoint_cid/4]).
-export([moh_attributes/2, moh_attributes/3]).
-export([owner_id/1, owner_id/2]).
-export([presence_id/1, presence_id/2]).
-export([owned_by/2, owned_by/3
        ,owned_by_docs/2, owned_by_docs/3
        ]).
-export([owner_ids/2]).
-export([get_account_external_cid/1]).
-export([maybe_get_assigned_number/3]).
-export([maybe_get_account_default_number/4]).
-export([maybe_prefix_cid/5]).
-export([get_flags/2]).
-export([process_dynamic_flags/2
        ,process_dynamic_flags/3
        ]).

-include("kazoo_endpoint.hrl").

-type cid() :: {kz_term:api_binary(), kz_term:api_binary()}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec temporal_rules(kapps_call:call()) -> kz_json:objects().
temporal_rules(Call) ->
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:get_results(AccountDb, <<"attributes/temporal_rules">>, ['include_docs']) of
        {'ok', JObjs} -> JObjs;
        {'error', _E} ->
            lager:debug("failed to find temporal rules: ~p", [_E]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec groups(kapps_call:call()) -> kz_json:objects().
groups(Call) ->
    groups(Call, []).

-spec groups(kapps_call:call(), kz_term:proplist()) -> kz_json:objects().
groups(Call, ViewOptions) ->
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:get_results(AccountDb, <<"attributes/groups">>, ViewOptions) of
        {'ok', JObjs} -> JObjs;
        {'error', _} -> []
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec caller_id_type(kapps_call:call()) -> kz_term:ne_binary().
caller_id_type(Call) ->
    case kapps_call:inception(Call) of
        'undefined' -> <<"internal">>;
        _ -> <<"external">>
    end.

-spec caller_id(kapps_call:call()) -> cid().
caller_id(Call) ->
    caller_id(caller_id_type(Call), Call).

-spec caller_id(kz_term:ne_binary(), kapps_call:call()) -> cid().
caller_id(Attribute, Call) ->
    CCVs = kapps_call:custom_channel_vars(Call),
    Inception = kapps_call:inception(Call),
    case ((Inception =/= 'undefined'
           andalso not kz_json:is_true(<<"Call-Forward">>, CCVs)
          )
          orelse kz_json:is_true(<<"Retain-CID">>, CCVs))
        andalso kz_term:is_false(kapps_call:kvs_fetch('force_dynamic_cid', 'false', Call))
    of
        'true' ->
            Number = kapps_call:caller_id_number(Call),
            Name = kapps_call:caller_id_name(Call),
            maybe_normalize_cid(Number, Name, 'false', Attribute, Call);
        'false' ->
            maybe_get_dynamic_cid('true', Attribute, Call)
    end.

-spec maybe_get_dynamic_cid(boolean(), kz_term:ne_binary(), kapps_call:call()) -> cid().
maybe_get_dynamic_cid(Validate, Attribute, Call) ->
    case kapps_call:kvs_fetch('dynamic_cid', Call) of
        'undefined' -> maybe_get_endpoint_cid(Validate, Attribute, Call);
        {DynamicCIDNum,  DynamicCIDName} ->
            lager:debug("found dynamic caller id ~s/~s", [DynamicCIDNum, DynamicCIDName]),
            maybe_normalize_cid(DynamicCIDNum, DynamicCIDName, Validate, Attribute, Call);
        DynamicCID ->
            lager:debug("found dynamic caller id number ~s", [DynamicCID]),
            maybe_normalize_cid(DynamicCID, 'undefined', Validate, Attribute, Call)
    end.

-spec maybe_get_endpoint_cid(boolean(), kz_term:ne_binary(), kapps_call:call()) -> cid().
maybe_get_endpoint_cid(Validate, Attribute, Call) ->
    case kz_endpoint:get(Call) of
        {'error', _R} ->
            lager:info("unable to get endpoint: ~p", [_R]),
            maybe_normalize_cid('undefined', 'undefined', Validate, Attribute, Call);
        {'ok', JObj} ->
            get_endpoint_cid(Validate, Attribute, JObj, Call)
    end.

-spec get_endpoint_cid(kz_term:ne_binary(), kz_json:object()) -> cid().
get_endpoint_cid(Attribute, JObj) ->
    Number = get_cid_or_default(Attribute, <<"number">>, JObj),
    Name = get_cid_or_default(Attribute, <<"name">>, JObj),
    {Number, Name}.

-spec get_endpoint_cid(boolean(), kz_term:ne_binary(), kz_json:object(), kapps_call:call()) -> cid().
get_endpoint_cid(Validate, Attribute, JObj, Call) ->
    {Number, Name} =  get_endpoint_cid(Attribute, JObj),
    _ = log_configured_endpoint_cid(Attribute, Name, Number),
    Call1 = maybe_add_originals_to_kvs(Number, Name, Call),
    maybe_use_presence_number(Number, Name, JObj, Validate, Attribute, Call1).

-spec maybe_add_originals_to_kvs(kz_term:api_ne_binary(), kz_term:api_ne_binary(), kapps_call:call()) -> kapps_call:call().
maybe_add_originals_to_kvs('undefined', 'undefined', Call) -> Call;
maybe_add_originals_to_kvs('undefined', ?NE_BINARY=Name, Call) ->
    kapps_call:exec([fun(C) -> kapps_call:kvs_store('original_cid_name', Name, C) end], Call);
maybe_add_originals_to_kvs(?NE_BINARY=Number, 'undefined', Call) ->
    kapps_call:exec([fun(C) -> kapps_call:kvs_store('original_cid_number', Number, C) end], Call);
maybe_add_originals_to_kvs(Number, Name, Call) ->
    Updates = [fun(C) -> kapps_call:kvs_store('original_cid_number', Number, C) end
              ,fun(C) -> kapps_call:kvs_store('original_cid_name', Name, C) end
              ],
    kapps_call:exec(Updates, Call).

-spec log_configured_endpoint_cid(kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary()) -> 'ok'.
log_configured_endpoint_cid(Attribute, 'undefined', 'undefined') ->
    lager:debug("endpoint not configured with an ~s caller id", [Attribute]);
log_configured_endpoint_cid(Attribute, Name, 'undefined') ->
    lager:debug("endpoint configured with ~s caller id name ~s", [Attribute, Name]);
log_configured_endpoint_cid(Attribute, 'undefined', Number) ->
    lager:debug("endpoint configured with ~s caller id number ~s", [Attribute, Number]);
log_configured_endpoint_cid(Attribute, Name, Number) ->
    lager:debug("endpoint configured with ~s caller id <~s> ~s", [Attribute, Name, Number]).

-spec maybe_use_presence_number(kz_term:api_binary(), kz_term:api_binary(), kz_json:object(), boolean(), kz_term:ne_binary(), kapps_call:call()) -> cid().
maybe_use_presence_number('undefined', Name, Endpoint, Validate, <<"internal">> = Attribute, Call) ->
    case maybe_get_presence_number(Endpoint, Call) of
        'undefined' ->
            maybe_format_cid_number('undefined', Name, Endpoint, Validate, Attribute, Call);
        Number ->
            lager:debug("replacing empty caller id number with presence number ~s", [Number]),
            maybe_format_cid_number(Number, Name, Endpoint, Validate, Attribute, Call)
    end;
maybe_use_presence_number(Number, Name, Endpoint, Validate, Attribute, Call) ->
    maybe_format_cid_number(Number, Name, Endpoint, Validate, Attribute, Call).

-spec maybe_format_cid_number(kz_term:api_binary(), kz_term:api_binary(), kz_json:object(), boolean(), kz_term:ne_binary(), kapps_call:call()) -> cid().
maybe_format_cid_number('undefined', Name, Endpoint, Validate, Attribute, Call) ->
    Number = kapps_call:caller_id_number(Call),
    Format = kz_json:get_ne_value([<<"caller_id_options">>, <<"format">>], Endpoint),
    Formatted = kapps_call:maybe_format_caller_id_str(Number, Format),
    maybe_normalize_cid(Formatted, Name, Validate, Attribute, Call);
maybe_format_cid_number(Number, Name, Endpoint, Validate, Attribute, Call) ->
    Format = kz_json:get_ne_value([<<"caller_id_options">>, <<"format">>], Endpoint),
    Formatted = kapps_call:maybe_format_caller_id_str(Number, Format),
    maybe_normalize_cid(Formatted, Name, Validate, Attribute, Call).

-spec maybe_normalize_cid(kz_term:api_binary(), kz_term:api_binary(), boolean(), kz_term:ne_binary(), kapps_call:call()) -> cid().
maybe_normalize_cid('undefined', Name, Validate, Attribute, Call) ->
    Number = kapps_call:caller_id_number(Call),
    lager:debug("replacing empty caller id number with SIP caller id number ~s", [Number]),
    maybe_normalize_cid(Number, Name, Validate, Attribute, Call);
maybe_normalize_cid(Number, 'undefined', Validate, Attribute, Call) ->
    Name = kapps_call:caller_id_name(Call),
    lager:debug("replacing empty caller id name with SIP caller id name ~s", [Name]),
    maybe_normalize_cid(Number, Name, Validate, Attribute, Call);
maybe_normalize_cid(Number, Name, Validate, Attribute, Call) ->
    maybe_prefix_cid(kz_term:to_binary(Number), Name, Validate, Attribute, Call).

-spec maybe_prefix_cid(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), kz_term:ne_binary(), kapps_call:call()) -> cid().
maybe_prefix_cid(Number, Name, Validate, Attribute, Call) ->
    OrigNumber = kapps_call:kvs_fetch('original_cid_number', Number, Call),
    case kapps_call:kvs_fetch('prepend_cid_number', Call) of
        'undefined' -> maybe_prefix_cid_name(Number, Name, Validate, Attribute, Call);
        Prefix ->
            lager:debug("prepending caller id number with '~s'", [Prefix]),
            Prefixed = <<(kz_term:to_binary(Prefix))/binary, OrigNumber/binary>>,
            maybe_prefix_cid_name(Prefixed, Name, Validate, Attribute, Call)
    end.

-spec maybe_prefix_cid_name(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), kz_term:ne_binary(), kapps_call:call()) -> cid().
maybe_prefix_cid_name(Number, Name, Validate, Attribute, Call) ->
    OrigName = kapps_call:kvs_fetch('original_cid_name', Name, Call),
    case kapps_call:kvs_fetch('prepend_cid_name', Call) of
        'undefined' -> maybe_rewrite_cid_number(Number, Name, Validate, Attribute, Call);
        Prefix ->
            lager:debug("prepending caller id name with '~s'", [Prefix]),
            Prefixed = <<(kz_term:to_binary(Prefix))/binary, OrigName/binary>>,
            maybe_rewrite_cid_number(Number, Prefixed, Validate, Attribute, Call)
    end.

-spec maybe_rewrite_cid_number(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), kz_term:ne_binary(), kapps_call:call()) -> cid().
maybe_rewrite_cid_number(Number, Name, Validate, Attribute, Call) ->
    case kapps_call:kvs_fetch('rewrite_cid_number', Call) of
        'undefined' -> maybe_rewrite_cid_name(Number, Name, Validate, Attribute, Call);
        NewNumber ->
            lager:debug("reformatting caller id number from ~s to ~s", [Number, NewNumber]),
            maybe_rewrite_cid_name(NewNumber, Name, Validate, Attribute, Call)
    end.

-spec maybe_rewrite_cid_name(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), kz_term:ne_binary(), kapps_call:call()) -> cid().
maybe_rewrite_cid_name(Number, Name, Validate, Attribute, Call) ->
    case kapps_call:kvs_fetch('rewrite_cid_name', Call) of
        'undefined' -> maybe_ensure_cid_valid(Number, Name, Validate, Attribute, Call);
        NewName ->
            lager:debug("reformatting caller id name from ~s to ~s", [Name, NewName]),
            maybe_ensure_cid_valid(Number, NewName, Validate, Attribute, Call)
    end.

-spec maybe_ensure_cid_valid(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), kz_term:ne_binary(), kapps_call:call()) -> cid().
maybe_ensure_cid_valid(Number, Name, 'true', <<"emergency">>, _Call) ->
    lager:info("determined emergency caller id is <~s> ~s", [Name, Number]),
    {Number, Name};
maybe_ensure_cid_valid(Number, Name, 'true', <<"external">>, Call) ->
    case kapps_config:get_is_true(<<"callflow">>, <<"ensure_valid_caller_id">>, 'false') of
        'true' -> ensure_valid_caller_id(Number, Name, Call);
        'false' ->
            lager:info("determined external caller id is <~s> ~s", [Name, Number]),
            {Number, Name}
    end;
maybe_ensure_cid_valid(Number, Name, _, Attribute, _Call) ->
    lager:info("determined ~s caller id is <~s> ~s", [Attribute, Name, Number]),
    {Number, Name}.

-spec ensure_valid_caller_id(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> cid().
ensure_valid_caller_id(Number, Name, Call) ->
    case is_valid_caller_id(Number, Call) of
        'true' ->
            lager:info("determined valid external caller id is <~s> ~s", [Name, Number]),
            {Number, Name};
        'false' ->
            lager:info("invalid external caller id <~s> ~s", [Name, Number]),
            maybe_get_account_cid(Number, Name, Call)
    end.

-spec get_account_external_cid(kapps_call:call()) -> cid().
get_account_external_cid(Call) ->
    Number = kapps_call:caller_id_number(Call),
    Name = kapps_call:caller_id_name(Call),

    lager:info("current cid number ~s and name ~s", [Number, Name]),

    maybe_get_account_cid(Number, Name, Call).

-spec maybe_get_account_cid(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> cid().
maybe_get_account_cid(Number, Name, Call) ->
    case kzd_accounts:fetch(kapps_call:account_id(Call)) of
        {'error', _} -> maybe_get_assigned_number(Number, Name, Call);
        {'ok', JObj} -> maybe_get_account_external_number(Number, Name, JObj, Call)
    end.

-spec maybe_get_account_external_number(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kapps_call:call()) -> cid().
maybe_get_account_external_number(Number, Name, Account, Call) ->
    External = kz_json:get_ne_value([<<"caller_id">>, <<"external">>, <<"number">>], Account),
    case is_valid_caller_id(External, Call) of
        'true' ->
            lager:info("determined valid account external caller id is <~s> ~s", [Name, External]),
            {External, Name};
        'false' ->
            maybe_get_account_default_number(Number, Name, Account, Call)
    end.

-spec maybe_get_account_default_number(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kapps_call:call()) -> cid().
maybe_get_account_default_number(Number, Name, Account, Call) ->
    Default = kz_json:get_ne_value([<<"caller_id">>, <<"default">>, <<"number">>], Account),
    case is_valid_caller_id(Default, Call) of
        'true' ->
            lager:info("determined valid account default caller id is <~s> ~s", [Name, Default]),
            {Default, Name};
        'false' ->
            maybe_get_assigned_number(Number, Name, Call)
    end.

-spec maybe_get_assigned_number(kz_term:api_ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()|kapps_call:call()) -> cid().
maybe_get_assigned_number(CandidateNumber, Name, ?MATCH_ACCOUNT_ENCODED(_)=AccountDb) ->
    AccountId = kzs_util:format_account_id(AccountDb),
    case knm_numbers:account_listing(AccountDb) of
        [_|_] = NumbersList ->
            Numbers = [Num
                       || {Num,JObj} <- NumbersList,
                          kz_json:get_value(<<"state">>, JObj) =:= ?NUMBER_STATE_IN_SERVICE
                      ],
            case lists:member(CandidateNumber, Numbers) of
                'true' -> {CandidateNumber, Name};
                'false' -> maybe_get_assigned_numbers(AccountId, Numbers, Name)
            end;
        _ ->
            Number = default_cid_number(AccountId),
            lager:warning("no numbers available, proceed with <~s> ~s", [Name, Number]),
            {Number, Name}
    end;
maybe_get_assigned_number(CandidateNumber, Name, Call) ->
    AccountDb = kapps_call:account_db(Call),
    maybe_get_assigned_number(CandidateNumber, Name, AccountDb).

-spec maybe_get_assigned_numbers(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary()) -> cid().
maybe_get_assigned_numbers(AccountId, [], Name) ->
    Number = default_cid_number(AccountId),
    lager:info("failed to find any in-service numbers, using default <~s> ~s", [Name, Number]),
    {Number, Name};
maybe_get_assigned_numbers(_AccountId, [Number|_], Name) ->
    %% This could optionally cycle all found numbers and ensure they valid
    %% but that could be a lot of wasted db lookups...
    lager:info("using first assigned number caller id <~s> ~s", [Name, Number]),
    {Number, Name}.

-spec is_valid_caller_id(kz_term:api_binary(), kapps_call:call()) -> boolean().
is_valid_caller_id('undefined', _) -> 'false';
is_valid_caller_id(Number, Call) ->
    AccountId = kapps_call:account_id(Call),
    case knm_numbers:lookup_account(Number) of
        {'ok', AccountId, _} -> 'true';
        _Else -> 'false'
    end.

-spec maybe_get_presence_number(kz_json:object(), kapps_call:call()) -> kz_term:api_binary().
maybe_get_presence_number(Endpoint, Call) ->
    case presence_id(Endpoint, Call, 'undefined') of
        'undefined' -> 'undefined';
        PresenceId ->
            case binary:split(PresenceId, <<"@">>) of
                [PresenceNumber, _] -> PresenceNumber;
                [_Else] -> PresenceId
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec callee_id(kz_term:api_binary() | kz_json:object(), kapps_call:call()) -> cid().
callee_id(EndpointId, Call) when is_binary(EndpointId) ->
    case kz_endpoint:get(EndpointId, Call) of
        {'ok', Endpoint} -> callee_id(Endpoint, Call);
        {'error', _R} ->
            maybe_normalize_callee('undefined', 'undefined', kz_json:new(), Call)
    end;
callee_id(Endpoint, Call) ->
    Attribute = determine_callee_attribute(Call),
    Number = get_cid_or_default(Attribute, <<"number">>, Endpoint),
    Name = get_cid_or_default(Attribute, <<"name">>, Endpoint),
    maybe_normalize_callee(Number, Name, Endpoint, Call).

-spec maybe_normalize_callee(kz_term:api_binary(), kz_term:api_binary(), kz_json:object(), kapps_call:call()) -> cid().
maybe_normalize_callee('undefined', Name, Endpoint, Call) ->
    maybe_normalize_callee(kapps_call:request_user(Call), Name, Endpoint, Call);
maybe_normalize_callee(Number, 'undefined', Endpoint, Call) ->
    maybe_normalize_callee(Number, default_cid_name(Endpoint, Call), Endpoint, Call);
maybe_normalize_callee(Number, Name, _, _) ->
    lager:info("callee id <~s> ~s", [Name, Number]),
    {Number, Name}.

-spec determine_callee_attribute(kapps_call:call()) -> kz_term:ne_binary().
determine_callee_attribute(Call) ->
    case kapps_call:inception(Call) =/= 'undefined' of
        'true' -> <<"external">>;
        'false' -> <<"internal">>
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec moh_attributes(kz_term:ne_binary(), kapps_call:call()) -> kz_term:api_binary().
moh_attributes(Attribute, Call) ->
    case kz_endpoint:get(Call) of
        {'error', _R} -> 'undefined';
        {'ok', Endpoint} ->
            Value = kz_json:get_ne_value([<<"music_on_hold">>, Attribute], Endpoint),
            maybe_normalize_moh_attribute(Value, Attribute, Call)
    end.

-spec moh_attributes(kz_term:api_binary() | kz_json:object(), kz_term:ne_binary(), kapps_call:call()) -> kz_term:api_binary().
moh_attributes('undefined', _, _) -> 'undefined';
moh_attributes(EndpointId, Attribute, Call) when is_binary(EndpointId) ->
    case kz_endpoint:get(EndpointId, Call) of
        {'error', _} -> 'undefined';
        {'ok', Endpoint} ->
            Value = kz_json:get_ne_value([<<"music_on_hold">>, Attribute], Endpoint),
            maybe_normalize_moh_attribute(Value, Attribute, Call)
    end;
moh_attributes(Endpoint, Attribute, Call) ->
    Value = kz_json:get_ne_value([<<"music_on_hold">>, Attribute], Endpoint),
    maybe_normalize_moh_attribute(Value, Attribute, Call).

-spec maybe_normalize_moh_attribute(kz_term:api_binary(), kz_term:ne_binary(), kapps_call:call()) -> kz_term:api_binary().
maybe_normalize_moh_attribute('undefined', _, _) -> 'undefined';
maybe_normalize_moh_attribute(Value, <<"media_id">>, Call) ->
    MediaId = kz_media_util:media_path(Value, kapps_call:account_id(Call)),
    lager:info("found music_on_hold media_id: '~p'", [MediaId]),
    MediaId;
maybe_normalize_moh_attribute(Value, Attribute, _) ->
    lager:info("found music_on_hold ~s: '~p'", [Attribute, Value]),
    Value.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec owner_id(kapps_call:call()) -> kz_term:api_ne_binary().
owner_id(Call) ->
    case kz_endpoint:get(Call) of
        {'error', _} -> 'undefined';
        {'ok', Endpoint} ->
            case kz_json:get_ne_binary_value(<<"owner_id">>, Endpoint) of
                'undefined' -> 'undefined';
                OwnerId ->
                    lager:info("initiating endpoint is owned by ~s", [OwnerId]),
                    OwnerId
            end
    end.

-spec owner_id(kz_term:api_ne_binary(), kapps_call:call()) -> kz_term:api_ne_binary().
owner_id('undefined', _Call) -> 'undefined';
owner_id(ObjectId, Call) ->
    AccountDb = kapps_call:account_db(Call),
    ViewOptions = [{'key', ObjectId}],
    case kz_datamgr:get_results(AccountDb, <<"attributes/owner">>, ViewOptions) of
        {'ok', [JObj]} -> kz_json:get_value(<<"value">>, JObj);
        {'ok', []} -> 'undefined';
        {'ok', [_|_]=JObjs} ->
            Owners = [kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs],
            lager:debug("object ~s has multiple owners: ~-500p", [ObjectId, Owners]),
            'undefined';
        {'error', _R} ->
            lager:warning("unable to find owner for ~s: ~p", [ObjectId, _R]),
            'undefined'
    end.

-spec owner_ids(kz_term:api_binary(), kapps_call:call()) -> kz_term:ne_binaries().
owner_ids('undefined', _Call) -> [];
owner_ids(ObjectId, Call) ->
    AccountDb = kapps_call:account_db(Call),
    ViewOptions = [{'key', ObjectId}],
    case kz_datamgr:get_results(AccountDb, <<"attributes/owner">>, ViewOptions) of
        {'ok', []} -> [];
        {'ok', [JObj]} -> [kz_json:get_value(<<"value">>, JObj)];
        {'ok', [_|_]=JObjs} ->
            [kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to find owner for ~s: ~p", [ObjectId, _R]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc This function will return the presence id for the endpoint
%% @end
%%------------------------------------------------------------------------------
-spec presence_id(kapps_call:call()) -> kz_term:api_binary().
presence_id(Call) ->
    presence_id(kapps_call:authorizing_id(Call), Call).

-spec presence_id(kz_term:api_binary() | kz_json:object(), kapps_call:call()) -> kz_term:api_binary().
presence_id(EndpointId, Call) when is_binary(EndpointId) ->
    case kz_endpoint:get(EndpointId, Call) of
        {'ok', Endpoint} -> presence_id(Endpoint, Call);
        {'error', _} -> 'undefined'
    end;
presence_id(Endpoint, Call) ->
    Username = kzd_devices:sip_username(Endpoint, kapps_call:request_user(Call)),
    presence_id(Endpoint, Call, Username).

-spec presence_id(kz_json:object(), kapps_call:call(), Default) -> kz_term:ne_binary() | Default.
presence_id(Endpoint, Call, Default) ->
    PresenceId = kzd_devices:presence_id(Endpoint, Default),
    case kz_term:is_empty(PresenceId) of
        'true' -> maybe_fix_presence_id_realm(Default, Endpoint, Call);
        'false' -> maybe_fix_presence_id_realm(PresenceId, Endpoint, Call)
    end.

-spec maybe_fix_presence_id_realm(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) -> kz_term:api_ne_binary().
maybe_fix_presence_id_realm('undefined', _Endpoint, _Call) -> 'undefined';
maybe_fix_presence_id_realm(PresenceId, Endpoint, Call) ->
    case binary:match(PresenceId, <<"@">>) of
        'nomatch' ->
            Realm = kz_endpoint:get_sip_realm(Endpoint
                                             ,kapps_call:account_id(Call)
                                             ,kapps_call:request_realm(Call)
                                             ),
            <<PresenceId/binary, $@, Realm/binary>>;
        _Else -> PresenceId
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec owned_by(kz_term:api_binary(), kapps_call:call()) -> kz_term:api_binaries().
owned_by('undefined', _) -> [];
owned_by(OwnerId, Call) ->
    AccountDb = kapps_call:account_db(Call),
    ViewOptions = [{'startkey', [OwnerId]}
                  ,{'endkey', [OwnerId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(AccountDb, <<"attributes/owned">>, ViewOptions) of
        {'ok', JObjs} -> [kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to find documents owned by ~s: ~p", [OwnerId, _R]),
            []
    end.

-spec owned_by(kz_term:api_binary() | kz_term:api_binaries(), kz_term:ne_binary(), kapps_call:call()) -> kz_term:api_binaries().
owned_by('undefined', _, _) -> [];
owned_by([_|_]=OwnerIds, Type, Call) ->
    Keys = [[OwnerId, Type] || OwnerId <- OwnerIds],
    owned_by_query([{'keys', Keys}], Call);
owned_by(OwnerId, Type, Call) ->
    owned_by_query([{'key', [OwnerId, Type]}], Call).

-spec owned_by_docs(kz_term:api_binary(), kz_term:ne_binary() | kapps_call:call()) ->
          kz_term:api_objects().
owned_by_docs('undefined', _) -> [];
owned_by_docs(OwnerId, ?NE_BINARY=Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    ViewOptions = [{'startkey', [OwnerId]}
                  ,{'endkey', [OwnerId, kz_json:new()]}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"attributes/owned">>, ViewOptions) of
        {'ok', JObjs} -> [kz_json:get_json_value(<<"doc">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to find documents owned by ~s: ~p", [OwnerId, _R]),
            []
    end;
owned_by_docs(OwnerId, Call) ->
    owned_by_docs(OwnerId, kapps_call:account_id(Call)).

-spec owned_by_docs(kz_term:api_binary() | kz_term:api_binaries(), kz_term:ne_binary(), kapps_call:call()) -> kz_term:api_objects().
owned_by_docs('undefined', _, _) -> [];
owned_by_docs([_|_]=OwnerIds, Type, Call) ->
    Keys = [[OwnerId, Type] || OwnerId <- OwnerIds],
    owned_by_query([{'keys', Keys}, 'include_docs'], Call, <<"doc">>);
owned_by_docs(OwnerId, Type, Call) ->
    owned_by_query([{'key', [OwnerId, Type]}, 'include_docs'], Call, <<"doc">>).

-spec owned_by_query(list(), kapps_call:call()) -> kz_term:api_binaries() | kz_json:objects().
owned_by_query(ViewOptions, Call) ->
    owned_by_query(ViewOptions, Call, <<"value">>).

-spec owned_by_query(list(), kapps_call:call(), kz_term:ne_binary()) -> kz_term:api_binaries() | kz_json:objects().
owned_by_query(ViewOptions, Call, ViewKey) ->
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:get_results(AccountDb, <<"attributes/owned">>, ViewOptions) of
        {'ok', JObjs} -> [kz_json:get_value(ViewKey, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to find owned documents (~p) using ~p", [_R, ViewOptions]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_flags(kz_term:ne_binary(), kapps_call:call()) -> kz_term:api_ne_binaries().
get_flags(ApplicationName, Call) ->
    Routines = [fun maybe_get_endpoint_static_flags/3
               ,fun get_account_static_flags/3
               ,fun get_config_static_flags/3
               ,fun maybe_get_endpoint_dynamic_flags/3
               ,fun get_account_dynamic_flags/3
               ,fun get_config_dynamic_flags/3
               ],
    Flags = lists:foldl(fun(F, A) -> F(ApplicationName, Call, A) end, [], Routines),
    sets:to_list(sets:from_list(Flags)).

-spec maybe_get_endpoint_static_flags(kz_term:ne_binary(), kapps_call:call(), kz_term:ne_binaries()) ->
          kz_term:ne_binaries().
maybe_get_endpoint_static_flags(_, Call, Flags) ->
    case kz_endpoint:get(Call) of
        {'error', _R} -> Flags;
        {'ok', Endpoint} ->
            get_endpoint_static_flags(Flags, Endpoint)
    end.

-spec get_endpoint_static_flags(kz_term:ne_binaries(), kz_json:object()) ->
          kz_term:ne_binaries().
get_endpoint_static_flags(Flags, Endpoint) ->
    kzd_devices:outbound_static_flags(Endpoint) ++ Flags.

-spec get_account_static_flags(kz_term:ne_binary(), kapps_call:call(), kz_term:ne_binaries()) ->
          kz_term:ne_binaries().
get_account_static_flags(_, Call, Flags) ->
    AccountId = kapps_call:account_id(Call),
    case kzd_accounts:fetch(AccountId) of
        {'ok', AccountJObj} -> kzd_devices:outbound_static_flags(AccountJObj) ++ Flags;
        {'error', _E} ->
            lager:error("not applying account outbound flags for ~s: ~p", [AccountId, _E]),
            Flags
    end.

-spec get_config_static_flags(kz_term:ne_binary(), kapps_call:call(), kz_term:ne_binaries()) ->
          kz_term:ne_binaries().
get_config_static_flags(ApplicationName, Call, Flags) ->
    kapps_account_config:get_ne_binaries(kapps_call:account_id(Call)
                                        ,ApplicationName
                                        ,<<"outbound_flags">>
                                        ,[]
                                        )
        ++ Flags.

-spec maybe_get_endpoint_dynamic_flags(kz_term:ne_binary(), kapps_call:call(), kz_term:ne_binaries()) ->
          kz_term:ne_binaries().
maybe_get_endpoint_dynamic_flags(_, Call, Flags) ->
    case kz_endpoint:get(Call) of
        {'error', _} -> Flags;
        {'ok', Endpoint} -> get_endpoint_dynamic_flags(Call, Flags, Endpoint)
    end.

-spec get_endpoint_dynamic_flags(kapps_call:call(), kz_term:ne_binaries(), kz_json:object()) ->
          kz_term:ne_binaries().
get_endpoint_dynamic_flags(Call, Flags, Endpoint) ->
    case kzd_devices:outbound_dynamic_flags(Endpoint) of
        [] -> Flags;
        DynamicFlags -> process_dynamic_flags(DynamicFlags, Flags, Call)
    end.

-spec get_account_dynamic_flags(kz_term:ne_binary(), kapps_call:call(), kz_term:ne_binaries()) ->
          kz_term:ne_binaries().
get_account_dynamic_flags(_, Call, Flags) ->
    AccountId = kapps_call:account_id(Call),
    case kzd_accounts:fetch(AccountId) of
        {'ok', AccountJObj} ->
            process_dynamic_flags(kzd_devices:outbound_dynamic_flags(AccountJObj), Flags, Call);
        {'error', _E} ->
            lager:error("not applying account dynamic flags for ~s: ~p", [AccountId, _E]),
            Flags
    end.

-spec get_config_dynamic_flags(kz_types:ne_binary(), kapps_call:call(), kz_term:ne_binaries()) ->
          kz_term:ne_binaries().
get_config_dynamic_flags(ApplicationName, Call, Flags) ->
    DynamicFlags = kapps_account_config:get_ne_binaries(kapps_call:account_id(Call)
                                                       ,ApplicationName
                                                       ,<<"dynamic_flags">>
                                                       ,[]
                                                       ),
    process_dynamic_flags(DynamicFlags, Flags, Call).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_dynamic_flags(kz_term:ne_binaries(), kapps_call:call()) -> kz_term:ne_binaries().
process_dynamic_flags(DynamicFlags, Call) ->
    process_dynamic_flags(DynamicFlags, [], Call).

-spec process_dynamic_flags(kz_term:ne_binaries(), kz_term:ne_binaries(), kapps_call:call()) ->
          kz_term:ne_binaries().
process_dynamic_flags([], Flags, _) -> Flags;
process_dynamic_flags([<<"zone">>|DynamicFlags], Flags, Call) ->
    Zone = kz_term:to_binary(kz_nodes:local_zone()),
    lager:debug("adding dynamic flag ~s", [Zone]),
    process_dynamic_flags(DynamicFlags, [Zone|Flags], Call);
process_dynamic_flags([<<"custom_channel_vars.", Key/binary>>|DynamicFlags], Flags, Call) ->
    CCVs = kz_json:normalize_jobj(kapps_call:custom_channel_vars(Call)),
    case kz_json:get_ne_binary_value(Key, CCVs) of
        'undefined' -> process_dynamic_flags(DynamicFlags, Flags, Call);
        Flag ->
            lager:debug("adding dynamic flag ~s", [Flag]),
            process_dynamic_flags(DynamicFlags, [Flag|Flags], Call)
    end;
process_dynamic_flags([DynamicFlag|DynamicFlags], Flags, Call) ->
    case is_flag_exported(DynamicFlag) of
        'false' -> process_dynamic_flags(DynamicFlags, Flags, Call);
        'true' ->
            Fun = kz_term:to_atom(DynamicFlag),
            Flag =  kapps_call:Fun(Call),
            lager:debug("adding dynamic flag ~s", [Flag]),
            process_dynamic_flags(DynamicFlags, [Flag|Flags], Call)
    end.

-spec is_flag_exported(kz_term:ne_binary()) -> boolean().
is_flag_exported(Flag) ->
    kz_module:is_exported('kapps_call', Flag, 1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec default_cid_number(kz_term:ne_binary()) -> kz_term:ne_binary().
default_cid_number(AccountId) ->
    kapps_config:get_ne_binary(?CONFIG_CAT
                              ,<<"default_caller_id_number">>
                              ,kz_privacy:anonymous_caller_id_number(AccountId)
                              ).

-spec default_cid_name(kz_json:object(), kapps_call:call()) -> kz_term:ne_binary().
default_cid_name('undefined', Call) ->
    AccountId = kapps_call:account_id(Call),
    kapps_config:get_ne_binary(?CONFIG_CAT
                              ,<<"default_caller_id_name">>
                              ,kapps_call:unknown_caller_id_name(AccountId)
                              );
default_cid_name(<<_/binary>> = Name, _Call) -> Name;
default_cid_name(Endpoint, Call) ->
    default_cid_name(kz_json:get_ne_value(<<"name">>, Endpoint), Call).

-spec get_cid_or_default(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:api_ne_binary().
get_cid_or_default(<<"emergency">>, Property, Endpoint) ->
    case kz_json:get_first_defined([[<<"caller_id">>, <<"emergency">>, Property]
                                   ,[<<"caller_id">>, <<"external">>, Property]
                                   ]
                                  ,Endpoint
                                  )

    of
        'undefined' -> kz_json:get_ne_value([<<"default">>, Property], Endpoint);
        Value -> Value
    end;
get_cid_or_default(Attribute, Property, Endpoint) ->
    case kz_json:get_ne_value([<<"caller_id">>, Attribute, Property], Endpoint) of
        'undefined' -> kz_json:get_ne_value([<<"default">>, Property], Endpoint);
        Value -> Value
    end.
