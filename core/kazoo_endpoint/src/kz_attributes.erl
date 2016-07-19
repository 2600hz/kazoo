%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_attributes).

-export([temporal_rules/1]).
-export([groups/1, groups/2]).
-export([caller_id/2]).
-export([callee_id/2]).
-export([moh_attributes/2, moh_attributes/3]).
-export([owner_id/1, owner_id/2]).
-export([presence_id/1, presence_id/2]).
-export([owned_by/2, owned_by/3
        ,owned_by_docs/2, owned_by_docs/3
        ]).
-export([owner_ids/2]).
-export([maybe_get_assigned_number/3]).
-export([maybe_get_account_default_number/4]).

-include("kazoo_endpoint.hrl").

-define(CALLER_PRIVACY(CCVs)
       ,(kz_json:is_true(<<"Caller-Privacy-Number">>, CCVs, 'false')
         orelse kz_json:is_true(<<"Caller-Privacy-Name">>, CCVs, 'false')
        )
       ).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec temporal_rules(kapps_call:call()) -> kz_json:objects().
temporal_rules(Call) ->
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:get_results(AccountDb, <<"attributes/temporal_rules">>, ['include_docs']) of
        {'ok', JObjs} -> JObjs;
        {'error', _E} ->
            lager:debug("failed to find temporal rules: ~p", [_E]),
            []
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec groups(kapps_call:call()) -> kz_json:objects().
-spec groups(kapps_call:call(), kz_proplist()) -> kz_json:objects().
groups(Call) ->
    groups(Call, []).
groups(Call, ViewOptions) ->
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:get_results(AccountDb, <<"attributes/groups">>, ViewOptions) of
        {'ok', JObjs} -> JObjs;
        {'error', _} -> []
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec caller_id(ne_binary(), kapps_call:call()) ->
                       {api_binary(), api_binary()}.
caller_id(Attribute, Call) ->
    CCVs = kapps_call:custom_channel_vars(Call),
    Inception = kapps_call:inception(Call),
    case (Inception =/= 'undefined'
          andalso not kz_json:is_true(<<"Call-Forward">>, CCVs))
        orelse kz_json:is_true(<<"Retain-CID">>, CCVs)
    of
        'true' ->
            Number = kapps_call:caller_id_number(Call),
            Name = kapps_call:caller_id_name(Call),
            lager:info("retaining original caller id <~s> ~s", [Name, Number]),
            maybe_normalize_cid(Number, Name, 'false', Attribute, Call);
        'false' ->
            maybe_get_dynamic_cid('true', Attribute, Call)
    end.

-spec maybe_get_dynamic_cid(boolean(), ne_binary(), kapps_call:call()) ->
                                   {api_binary(), api_binary()}.
maybe_get_dynamic_cid(Validate, Attribute, Call) ->
    case kapps_call:kvs_fetch('dynamic_cid', Call) of
        'undefined' -> maybe_get_endpoint_cid(Validate, Attribute, Call);
        DynamicCID ->
            lager:debug("found dynamic caller id number ~s", [DynamicCID]),
            maybe_normalize_cid(DynamicCID, 'undefined', Validate, Attribute, Call)
    end.

-spec maybe_get_endpoint_cid(boolean(), ne_binary(), kapps_call:call()) ->
                                    {api_binary(), api_binary()}.
maybe_get_endpoint_cid(Validate, Attribute, Call) ->
    case kz_endpoint:get(Call) of
        {'error', _R} ->
            lager:info("unable to get endpoint: ~p", [_R]),
            maybe_normalize_cid('undefined', 'undefined', Validate, Attribute, Call);
        {'ok', JObj} ->
            Number = get_cid_or_default(Attribute, <<"number">>, JObj),
            Name = get_cid_or_default(Attribute, <<"name">>, JObj),
            _ = log_configured_endpoint_cid(Attribute, Name, Number),
            maybe_use_presence_number(Number, Name, JObj, Validate, Attribute, Call)
    end.

-spec log_configured_endpoint_cid(ne_binary(), api_binary(), api_binary()) -> 'ok'.
log_configured_endpoint_cid(Attribute, 'undefined', 'undefined') ->
    lager:debug("endpoint not configured with an ~s caller id", [Attribute]);
log_configured_endpoint_cid(Attribute, Name, 'undefined') ->
    lager:debug("endpoint configured with ~s caller id name ~s", [Attribute, Name]);
log_configured_endpoint_cid(Attribute, 'undefined', Number) ->
    lager:debug("endpoint configured with ~s caller id number ~s", [Attribute, Number]);
log_configured_endpoint_cid(Attribute, Name, Number) ->
    lager:debug("endpoint configured with ~s caller id <~s> ~s", [Attribute, Name, Number]).

-spec maybe_use_presence_number(api_binary(), api_binary(), kz_json:object(), boolean(), ne_binary(), kapps_call:call()) ->
                                       {api_binary(), api_binary()}.
maybe_use_presence_number('undefined', Name, Endpoint, Validate, <<"internal">> = Attribute, Call) ->
    case maybe_get_presence_number(Endpoint, Call) of
        'undefined' -> maybe_normalize_cid('undefined', Name, Validate, Attribute, Call);
        Number ->
            lager:debug("replacing empty caller id number with presence number ~s", [Number]),
            maybe_normalize_cid(Number, Name, Validate, Attribute, Call)
    end;
maybe_use_presence_number(Number, Name, _Endpoint, Validate, Attribute, Call) ->
    maybe_normalize_cid(Number, Name, Validate, Attribute, Call).

-spec maybe_normalize_cid(api_binary(), api_binary(), boolean(), ne_binary(), kapps_call:call()) ->
                                 {api_binary(), api_binary()}.
maybe_normalize_cid('undefined', Name, Validate, Attribute, Call) ->
    Number = kapps_call:caller_id_number(Call),
    lager:debug("replacing empty caller id number with SIP caller id number ~s", [Number]),
    maybe_normalize_cid(Number, Name, Validate, Attribute, Call);
maybe_normalize_cid(Number, 'undefined', Validate, Attribute, Call) ->
    Name = kapps_call:caller_id_name(Call),
    lager:debug("replacing empty caller id name with SIP caller id name ~s", [Name]),
    maybe_normalize_cid(Number, Name, Validate, Attribute, Call);
maybe_normalize_cid(Number, Name, Validate, Attribute, Call) ->
    maybe_prefix_cid_number(kz_util:to_binary(Number), Name, Validate, Attribute, Call).

-spec maybe_prefix_cid_number(ne_binary(), ne_binary(), boolean(), ne_binary(), kapps_call:call()) ->
                                     {api_binary(), api_binary()}.
maybe_prefix_cid_number(Number, Name, Validate, Attribute, Call) ->
    case kapps_call:kvs_fetch('prepend_cid_number', Call) of
        'undefined' -> maybe_prefix_cid_name(Number, Name, Validate, Attribute, Call);
        Prefix ->
            lager:debug("prepending caller id number with ~s", [Prefix]),
            Prefixed = <<(kz_util:to_binary(Prefix))/binary, Number/binary>>,
            maybe_prefix_cid_name(Prefixed, Name, Validate, Attribute, Call)
    end.

-spec maybe_prefix_cid_name(ne_binary(), ne_binary(), boolean(), ne_binary(), kapps_call:call()) ->
                                   {api_binary(), api_binary()}.
maybe_prefix_cid_name(Number, Name, Validate, Attribute, Call) ->
    case kapps_call:kvs_fetch('prepend_cid_name', Call) of
        'undefined' -> maybe_rewrite_cid_number(Number, Name, Validate, Attribute, Call);
        Prefix ->
            lager:debug("prepending caller id name with ~s", [Prefix]),
            Prefixed = <<(kz_util:to_binary(Prefix))/binary, Name/binary>>,
            maybe_rewrite_cid_number(Number, Prefixed, Validate, Attribute, Call)
    end.

-spec maybe_rewrite_cid_number(ne_binary(), ne_binary(), boolean(), ne_binary(), kapps_call:call()) ->
                                      {api_binary(), api_binary()}.
maybe_rewrite_cid_number(Number, Name, Validate, Attribute, Call) ->
    case kapps_call:kvs_fetch('rewrite_cid_number', Call) of
        'undefined' -> maybe_rewrite_cid_name(Number, Name, Validate, Attribute, Call);
        NewNumber ->
            lager:debug("reformating caller id number from ~s to ~s", [Number, NewNumber]),
            maybe_rewrite_cid_name(NewNumber, Name, Validate, Attribute, Call)
    end.

-spec maybe_rewrite_cid_name(ne_binary(), ne_binary(), boolean(), ne_binary(), kapps_call:call()) ->
                                    {api_binary(), api_binary()}.
maybe_rewrite_cid_name(Number, Name, Validate, Attribute, Call) ->
    case kapps_call:kvs_fetch('rewrite_cid_name', Call) of
        'undefined' -> maybe_ensure_cid_valid(Number, Name, Validate, Attribute, Call);
        NewName ->
            lager:debug("reformating caller id name from ~s to ~s", [Name, NewName]),
            maybe_ensure_cid_valid(Number, NewName, Validate, Attribute, Call)
    end.

-spec maybe_ensure_cid_valid(ne_binary(), ne_binary(), boolean(), ne_binary(), kapps_call:call()) ->
                                    {api_binary(), api_binary()}.
maybe_ensure_cid_valid(Number, Name, 'true', <<"emergency">>, _Call) ->
    lager:info("determined emergency caller id is <~s> ~s", [Name, Number]),
    {Number, Name};
maybe_ensure_cid_valid(Number, Name, 'true', <<"external">>, Call) ->
    case kapps_config:get_is_true(<<"callflow">>, <<"ensure_valid_caller_id">>, 'false') of
        'true' -> ensure_valid_caller_id(Number, Name, Call);
        'false' ->
            lager:info("determined external caller id is <~s> ~s", [Name, Number]),
            maybe_cid_privacy(Number, Name, Call)
    end;
maybe_ensure_cid_valid(Number, Name, _, Attribute, Call) ->
    lager:info("determined ~s caller id is <~s> ~s", [Attribute, Name, Number]),
    maybe_cid_privacy(Number, Name, Call).

-spec maybe_cid_privacy(api_binary(), api_binary(), kapps_call:call()) ->
                               {api_binary(), api_binary()}.
maybe_cid_privacy(Number, Name, Call) ->
    case kz_util:is_true(kapps_call:kvs_fetch('cf_privacy', Call))
        orelse ?CALLER_PRIVACY(kapps_call:custom_channel_vars(Call))
    of
        'true' ->
            lager:info("overriding caller id to maintain privacy"),
            {kapps_config:get_non_empty(<<"callflow">>
                                       ,<<"privacy_number">>
                                       ,kz_util:anonymous_caller_id_number()
                                       )
            ,kapps_config:get_non_empty(<<"callflow">>
                                       ,<<"privacy_name">>
                                       ,kz_util:anonymous_caller_id_name()
                                       )
            };
        'false' -> {Number, Name}
    end.

-spec ensure_valid_caller_id(ne_binary(), ne_binary(), kapps_call:call()) ->
                                    {api_binary(), api_binary()}.
ensure_valid_caller_id(Number, Name, Call) ->
    case is_valid_caller_id(Number, Call) of
        'true' ->
            lager:info("determined valid external caller id is <~s> ~s", [Name, Number]),
            {Number, Name};
        'false' ->
            lager:info("invalid external caller id <~s> ~s", [Name, Number]),
            maybe_get_account_cid(Number, Name, Call)
    end.

-spec maybe_get_account_cid(ne_binary(), ne_binary(), kapps_call:call()) ->
                                   {api_binary(), api_binary()}.
maybe_get_account_cid(Number, Name, Call) ->
    case kz_account:fetch(kapps_call:account_id(Call)) of
        {'error', _} -> maybe_get_assigned_number(Number, Name, Call);
        {'ok', JObj} -> maybe_get_account_external_number(Number, Name, JObj, Call)
    end.

-spec maybe_get_account_external_number(ne_binary(), ne_binary(), kz_json:object(), kapps_call:call()) ->
                                               {api_binary(), api_binary()}.
maybe_get_account_external_number(Number, Name, Account, Call) ->
    External = kz_json:get_ne_value([<<"caller_id">>, <<"external">>, <<"number">>], Account),
    case is_valid_caller_id(External, Call) of
        'true' ->
            lager:info("determined valid account external caller id is <~s> ~s", [Name, Number]),
            {External, Name};
        'false' ->
            maybe_get_account_default_number(Number, Name, Account, Call)
    end.

-spec maybe_get_account_default_number(ne_binary(), ne_binary(), kz_json:object(), kapps_call:call()) ->
                                              {api_binary(), api_binary()}.
maybe_get_account_default_number(Number, Name, Account, Call) ->
    Default = kz_json:get_ne_value([<<"caller_id">>, <<"default">>, <<"number">>], Account),
    case is_valid_caller_id(Default, Call) of
        'true' ->
            lager:info("determined valid account default caller id is <~s> ~s", [Name, Number]),
            {Default, Name};
        'false' ->
            maybe_get_assigned_number(Number, Name, Call)
    end.

-spec maybe_get_assigned_number(ne_binary(), ne_binary(), ne_binary()|kapps_call:call()) ->
                                       {api_binary(), api_binary()}.
maybe_get_assigned_number(CandidateNumber, Name, ?MATCH_ACCOUNT_ENCODED(_)=AccountDb) ->
    case knm_numbers:account_listing(AccountDb) of
        [_|_] = NumbersList ->
            Numbers = [Num
                       || {Num,JObj} <- NumbersList,
                          kz_json:get_value(<<"state">>, JObj) =:= ?NUMBER_STATE_IN_SERVICE
                      ],
            case lists:member(CandidateNumber, Numbers) of
                'true' ->
                    {CandidateNumber, Name};
                'false' ->
                    maybe_get_assigned_numbers(Numbers, Name)
            end;
        _ ->
            Number = default_cid_number(),
            lager:warning("no numbers available, proceed with <~s> ~s", [Name, Number]),
            {Number, Name}
    end;
maybe_get_assigned_number(CandidateNumber, Name, Call) ->
    AccountDb = kapps_call:account_db(Call),
    maybe_get_assigned_number(CandidateNumber, Name, AccountDb).

-spec maybe_get_assigned_numbers(ne_binaries(), ne_binary()) -> {api_binary(), api_binary()}.
maybe_get_assigned_numbers([], Name) ->
    Number = default_cid_number(),
    lager:info("failed to find any in-service numbers, using default <~s> ~s", [Name, Number]),
    {Number, Name};
maybe_get_assigned_numbers([Number|_], Name) ->
    %% This could optionally cycle all found numbers and ensure they valid
    %% but that could be a lot of wasted db lookups...
    lager:info("using first assigned number caller id <~s> ~s", [Name, Number]),
    {Number, Name}.

-spec is_valid_caller_id(api_binary(), kapps_call:call()) -> boolean().
is_valid_caller_id('undefined', _) -> 'false';
is_valid_caller_id(Number, Call) ->
    AccountId = kapps_call:account_id(Call),
    case knm_number:lookup_account(Number) of
        {'ok', AccountId, _} -> 'true';
        _Else -> 'false'
    end.

-spec maybe_get_presence_number(kz_json:object(), kapps_call:call()) -> api_binary().
maybe_get_presence_number(Endpoint, Call) ->
    case presence_id(Endpoint, Call, 'undefined') of
        'undefined' -> 'undefined';
        PresenceId ->
            case binary:split(PresenceId, <<"@">>) of
                [PresenceNumber, _] -> PresenceNumber;
                [_Else] -> PresenceId
            end
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec callee_id(api_binary() | kz_json:object(), kapps_call:call()) ->
                       {api_binary(), api_binary()}.
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

-spec maybe_normalize_callee(api_binary(), api_binary(), kz_json:object(), kapps_call:call()) ->
                                    {api_binary(), api_binary()}.
maybe_normalize_callee('undefined', Name, Endpoint, Call) ->
    maybe_normalize_callee(kapps_call:request_user(Call), Name, Endpoint, Call);
maybe_normalize_callee(Number, 'undefined', Endpoint, Call) ->
    maybe_normalize_callee(Number, default_cid_name(Endpoint), Endpoint, Call);
maybe_normalize_callee(Number, Name, _, _) ->
    lager:info("callee id <~s> ~s", [Name, Number]),
    {Number, Name}.

-spec determine_callee_attribute(kapps_call:call()) -> ne_binary().
determine_callee_attribute(Call) ->
    case kapps_call:inception(Call) =/= 'undefined' of
        'true' -> <<"external">>;
        'false' -> <<"internal">>
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec moh_attributes(ne_binary(), kapps_call:call()) -> api_binary().
-spec moh_attributes(api_binary() | kz_json:object(), ne_binary(), kapps_call:call()) -> api_binary().

moh_attributes(Attribute, Call) ->
    case kz_endpoint:get(Call) of
        {'error', _R} -> 'undefined';
        {'ok', Endpoint} ->
            Value = kz_json:get_ne_value([<<"music_on_hold">>, Attribute], Endpoint),
            maybe_normalize_moh_attribute(Value, Attribute, Call)
    end.

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

-spec maybe_normalize_moh_attribute(api_binary(), ne_binary(), kapps_call:call()) -> api_binary().
maybe_normalize_moh_attribute('undefined', _, _) -> 'undefined';
maybe_normalize_moh_attribute(Value, <<"media_id">>, Call) ->
    MediaId = kz_media_util:media_path(Value, Call),
    lager:info("found music_on_hold media_id: '~p'", [MediaId]),
    MediaId;
maybe_normalize_moh_attribute(Value, Attribute, _) ->
    lager:info("found music_on_hold ~s: '~p'", [Attribute, Value]),
    Value.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec owner_id(kapps_call:call()) -> api_binary().
-spec owner_id(api_binary(), kapps_call:call()) -> api_binary().

owner_id(Call) ->
    case kz_endpoint:get(Call) of
        {'error', _} -> 'undefined';
        {'ok', Endpoint} ->
            case kz_json:get_ne_value(<<"owner_id">>, Endpoint) of
                'undefined' -> 'undefined';
                OwnerId ->
                    lager:info("initiating endpoint is owned by ~s", [OwnerId]),
                    OwnerId
            end
    end.

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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return the precense id for the endpoint
%% @end
%%--------------------------------------------------------------------
-spec presence_id(kapps_call:call()) -> api_binary().
presence_id(Call) ->
    presence_id(kapps_call:authorizing_id(Call), Call).

-spec presence_id(api_binary() | kz_json:object(), kapps_call:call()) -> api_binary().
presence_id(EndpointId, Call) when is_binary(EndpointId) ->
    case kz_endpoint:get(EndpointId, Call) of
        {'ok', Endpoint} -> presence_id(Endpoint, Call);
        {'error', _} -> 'undefined'
    end;
presence_id(Endpoint, Call) ->
    Username = kz_device:sip_username(Endpoint, kapps_call:request_user(Call)),
    presence_id(Endpoint, Call, Username).

-spec presence_id(kz_json:object(), kapps_call:call(), Default) -> ne_binary() | Default.
presence_id(Endpoint, Call, Default) ->
    PresenceId = kz_device:presence_id(Endpoint, Default),
    case kz_util:is_empty(PresenceId) of
        'true' -> Default;
        'false' -> maybe_fix_presence_id_realm(PresenceId, Endpoint, Call)
    end.

-spec maybe_fix_presence_id_realm(ne_binary(), kz_json:object(), kapps_call:call()) -> ne_binary().
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

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec owned_by(api_binary(), kapps_call:call()) -> api_binaries().
-spec owned_by(api_binary() | api_binaries(), ne_binary(), kapps_call:call()) -> api_binaries().

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

owned_by('undefined', _, _) -> [];
owned_by([_|_]=OwnerIds, Type, Call) ->
    Keys = [[OwnerId, Type] || OwnerId <- OwnerIds],
    owned_by_query([{'keys', Keys}], Call);
owned_by(OwnerId, Type, Call) ->
    owned_by_query([{'key', [OwnerId, Type]}], Call).

-spec owned_by_docs(api_binary(), kapps_call:call()) -> api_objects().
-spec owned_by_docs(api_binary() | api_binaries(), ne_binary(), kapps_call:call()) -> api_objects().

owned_by_docs('undefined', _) -> [];
owned_by_docs(OwnerId, Call) ->
    AccountDb = kapps_call:account_db(Call),
    ViewOptions = [{'startkey', [OwnerId]}
                  ,{'endkey', [OwnerId, kz_json:new()]}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"attributes/owned">>, ViewOptions) of
        {'ok', JObjs} -> [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to find documents owned by ~s: ~p", [OwnerId, _R]),
            []
    end.

owned_by_docs('undefined', _, _) -> [];
owned_by_docs([_|_]=OwnerIds, Type, Call) ->
    Keys = [[OwnerId, Type] || OwnerId <- OwnerIds],
    owned_by_query([{'keys', Keys}, 'include_docs'], Call, <<"doc">>);
owned_by_docs(OwnerId, Type, Call) ->
    owned_by_query([{'key', [OwnerId, Type]}, 'include_docs'], Call, <<"doc">>).

-spec owned_by_query(list(), kapps_call:call()) -> api_binaries().
-spec owned_by_query(list(), kapps_call:call(), ne_binary()) -> api_binaries().
owned_by_query(ViewOptions, Call) ->
    owned_by_query(ViewOptions, Call, <<"value">>).
owned_by_query(ViewOptions, Call, ViewKey) ->
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:get_results(AccountDb, <<"attributes/owned">>, ViewOptions) of
        {'ok', JObjs} -> [kz_json:get_value(ViewKey, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to find owned documents (~p) using ~p", [_R, ViewOptions]),
            []
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec default_cid_number() -> ne_binary().
default_cid_number() ->
    kapps_config:get(?CONFIG_CAT
                    ,<<"default_caller_id_number">>
                    ,kz_util:anonymous_caller_id_number()
                    ).

-spec default_cid_name(kz_json:object()) -> ne_binary().
default_cid_name('undefined') ->
    kapps_config:get(?CONFIG_CAT
                    ,<<"default_caller_id_name">>
                    ,kz_util:anonymous_caller_id_name()
                    );
default_cid_name(<<_/binary>> = Name) -> Name;
default_cid_name(Endpoint) ->
    default_cid_name(kz_json:get_ne_value(<<"name">>, Endpoint)).

-spec get_cid_or_default(ne_binary(), ne_binary(), kz_json:object()) -> api_binary().
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
