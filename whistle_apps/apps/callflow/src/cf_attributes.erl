%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_attributes).

-include("callflow.hrl").

-export([temporal_rules/1]).
-export([caller_id/2]).
-export([callee_id/2]).
-export([moh_attributes/2, moh_attributes/3]).
-export([owner_id/1, owner_id/2]).
-export([presence_id/1, presence_id/2]).

-export([owned_by/2, owned_by/3, fetch_owned_by/2, fetch_owned_by/3]).


-export([flush_attributes/1]).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec temporal_rules/1 :: (whapps_call:call()) -> wh_json:objects().
temporal_rules(Call) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/temporal_rules">>, [include_docs]) of
        {ok, JObjs} -> JObjs;
        {error, _} -> []
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec caller_id/2 :: (ne_binary(), whapps_call:call()) -> {api_binary(), api_binary()}.
caller_id(Attribute, Call) ->
    CCVs = whapps_call:custom_channel_vars(Call),
    Inception = whapps_call:inception(Call),
    case (Inception =:= <<"off-net">>
              andalso not wh_json:is_true(<<"Call-Forward">>, CCVs))
        orelse wh_json:is_true(<<"Retain-CID">>, CCVs)
    of
        true ->
            lager:debug("retaining original caller id"),
            Number = whapps_call:caller_id_number(Call),
            Name = whapps_call:caller_id_name(Call),
            maybe_normalize_cid(Number, Name, false, Attribute, Call);
        false ->
            maybe_get_endpoint_cid(true, Attribute, Call)
    end.

-spec maybe_get_endpoint_cid/3 :: (boolean(), ne_binary(), whapps_call:call()) -> {api_binary(), api_binary()}.
maybe_get_endpoint_cid(Validate, Attribute, Call) ->
    case cf_endpoint:get(Call) of
        {error, _R} ->
            lager:debug("unable to get endpoint: ~p", [_R]),
            maybe_normalize_cid(undefined, undefined, Validate, Attribute, Call);
        {ok, JObj} ->
            Number = get_cid_or_default(Attribute, <<"number">>, JObj),
            Name = get_cid_or_default(Attribute, <<"name">>, JObj),
            maybe_normalize_cid(Number, Name, Validate, Attribute, Call)
    end.

-spec maybe_normalize_cid/5 :: (api_binary(), api_binary(), boolean(), ne_binary(), whapps_call:call()) ->
                                       {api_binary(), api_binary()}.
maybe_normalize_cid(undefined, Name, Validate, Attribute, Call) ->
    maybe_normalize_cid(whapps_call:caller_id_number(Call), Name, Validate, Attribute, Call);
maybe_normalize_cid(Number, undefined, Validate, Attribute, Call) ->
    maybe_normalize_cid(Number, whapps_call:caller_id_name(Call), Validate, Attribute, Call);
maybe_normalize_cid(Number, Name, Validate, Attribute, Call) ->
    maybe_prefix_cid_number(wh_util:to_binary(Number), Name, Validate, Attribute, Call).

-spec maybe_prefix_cid_number/5 :: (ne_binary(), ne_binary(), boolean(), ne_binary(), whapps_call:call()) ->
                                           {api_binary(), api_binary()}.
maybe_prefix_cid_number(Number, Name, Validate, Attribute, Call) ->
    case whapps_call:kvs_fetch(prepend_cid_number, Call) of
        undefined -> maybe_prefix_cid_name(Number, Name, Validate, Attribute, Call);
        Prefix ->
            Prefixed = <<(wh_util:to_binary(Prefix))/binary, Number/binary>>,
            maybe_prefix_cid_name(Prefixed, Name, Validate, Attribute, Call)
    end.

-spec maybe_prefix_cid_name/5 :: (ne_binary(), ne_binary(), boolean(), ne_binary(), whapps_call:call()) ->
                                         {api_binary(), api_binary()}.
maybe_prefix_cid_name(Number, Name, Validate, Attribute, Call) ->
    case whapps_call:kvs_fetch(prepend_cid_name, Call) of
        undefined -> maybe_ensure_cid_valid(Number, Name, Validate, Attribute, Call);
        Prefix ->
            Prefixed = <<(wh_util:to_binary(Prefix))/binary, Name/binary>>,
            maybe_ensure_cid_valid(Number, Prefixed, Validate, Attribute, Call)
    end.

-spec maybe_ensure_cid_valid/5 :: (ne_binary(), ne_binary(), boolean(), ne_binary(), whapps_call:call()) -> {api_binary(), api_binary()}.
maybe_ensure_cid_valid(Number, Name, true, <<"external">>, Call) ->
    case whapps_config:get_is_true(<<"callflow">>, <<"ensure_valid_caller_id">>, false) of
        true -> ensure_valid_caller_id(Number, Name, Call);
        false ->
            lager:debug("external caller id <~s> ~s", [Name, Number]),
            {Number, Name}
    end;
maybe_ensure_cid_valid(Number, Name, _, Attribute, _) ->
    lager:debug("~s caller id <~s> ~s", [Attribute, Name, Number]),
    {Number, Name}.

-spec ensure_valid_caller_id/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> {api_binary(), api_binary()}.
ensure_valid_caller_id(Number, Name, Call) ->
    case is_valid_caller_id(Number, Call) of
        true ->
            lager:debug("valid external caller id <~s> ~s", [Name, Number]),
            {Number, Name};
        false ->
            maybe_get_account_cid(Number, Name, Call)
    end.

-spec maybe_get_account_cid/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> {api_binary(), api_binary()}.
maybe_get_account_cid(Number, Name, Call) ->
    AccountDb = whapps_call:account_db(Call),
    AccountId = whapps_call:account_id(Call),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {error, _} -> maybe_get_assigned_number(Number, Name, Call);
        {ok, JObj} ->
            maybe_get_account_external_number(Number, Name, JObj, Call)
    end.

-spec maybe_get_account_external_number/4 :: (ne_binary(), ne_binary(), wh_json:object(), whapps_call:call()) -> {api_binary(), api_binary()}.
maybe_get_account_external_number(Number, Name, Account, Call) ->
    External = wh_json:get_ne_value([<<"caller_id">>, <<"external">>, <<"number">>], Account),
    case is_valid_caller_id(External, Call) of
        true ->
            lager:debug("valid account external caller id <~s> ~s", [Name, Number]),
            {External, Name};
        false ->
            maybe_get_account_default_number(Number, Name, Account, Call)
    end.

-spec maybe_get_account_default_number/4 :: (ne_binary(), ne_binary(), wh_json:object(), whapps_call:call()) -> {api_binary(), api_binary()}.
maybe_get_account_default_number(Number, Name, Account, Call) ->
    Default = wh_json:get_ne_value([<<"caller_id">>, <<"default">>, <<"number">>], Account),
    case is_valid_caller_id(Default, Call) of
        true ->
            lager:debug("valid account default caller id <~s> ~s", [Name, Number]),
            {Default, Name};
        false ->
            maybe_get_assigned_number(Number, Name, Call)
    end.

-spec maybe_get_assigned_number/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> {api_binary(), api_binary()}.
maybe_get_assigned_number(_, Name, Call) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_cache_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC) of
        {error, _} ->
            Number = default_cid_number(),
            lager:debug("could not open phone_numbers doc, using <~s> ~s", [Name, Number]),
            {Number, Name};
        {ok, JObj} ->
            PublicJObj = wh_json:public_fields(JObj),
            Numbers = [Num
                       || Num <- wh_json:get_keys(PublicJObj)
                              ,Num =/= <<"id">>
                              ,(not wh_json:is_true([Num, <<"on_subaccount">>], JObj))
                              ,(wh_json:get_value([Num, <<"state">>], JObj) =:= <<"in_service">>)
                      ],
            maybe_get_assigned_numbers(Numbers, Name, Call)
    end.

-spec maybe_get_assigned_numbers/3 :: ([] | [ne_binary(),...], ne_binary(), whapps_call:call()) -> {api_binary(), api_binary()}.
maybe_get_assigned_numbers([], Name, _) ->
    Number = default_cid_number(),
    lager:debug("failed to find any in-service numbers, using default <~s> ~s", [Name, Number]),
    {Number, Name};
maybe_get_assigned_numbers([Number|_], Name, _) ->
    %% This could optionally cycle all found numbers and ensure they valid
    %% but that could be a lot of wasted db lookups...
    lager:debug("first assigned number caller id <~s> ~s", [Name, Number]),
    {Number, Name}.

-spec is_valid_caller_id/2 :: (api_binary(), whapps_call:call()) -> boolean().
is_valid_caller_id(undefined, _) -> false;
is_valid_caller_id(Number, Call) ->
    AccountId = whapps_call:account_id(Call),
    case wh_number_manager:lookup_account_by_number(Number) of
        {ok, AccountId, _} -> true;
        _Else -> false
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec callee_id/2 :: (api_binary() | wh_json:object(), whapps_call:call()) ->
                             {api_binary(), api_binary()}.
callee_id(EndpointId, Call) when is_binary(EndpointId) ->
    case cf_endpoint:get(EndpointId, Call) of
        {ok, Endpoint} -> callee_id(Endpoint, Call);
        {error, _R} ->
            maybe_normalize_callee(undefined, undefined, wh_json:new(), Call)
    end;
callee_id(Endpoint, Call) ->
    Attribute = determine_callee_attribute(Call),
    Number = get_cid_or_default(Attribute, <<"number">>, Endpoint),
    Name = get_cid_or_default(Attribute, <<"name">>, Endpoint),
    maybe_normalize_callee(Number, Name, Endpoint, Call).


-spec maybe_normalize_callee/4 :: (api_binary(), api_binary(), wh_json:object(), whapps_call:call()) ->
                                          {api_binary(), api_binary()}.
maybe_normalize_callee(undefined, Name, Endpoint, Call) ->
    maybe_normalize_callee(whapps_call:request_user(Call), Name, Endpoint, Call);
maybe_normalize_callee(Number, undefined, Endpoint, Call) ->
    maybe_normalize_callee(Number, default_cid_name(Endpoint), Endpoint, Call);
maybe_normalize_callee(Number, Name, _, _) ->
    lager:debug("callee id <~s> ~s", [Name, Number]),
    {Number, Name}.

-spec determine_callee_attribute/1 :: (whapps_call:call()) -> ne_binary().
determine_callee_attribute(Call) ->
    case whapps_call:inception(Call) =:= <<"off-net">> of
        true -> <<"external">>;
        false -> <<"internal">>
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec moh_attributes/2 :: (ne_binary(), whapps_call:call()) -> api_binary().
-spec moh_attributes/3 :: (api_binary() | wh_json:object(), ne_binary(), whapps_call:call()) -> api_binary().

moh_attributes(Attribute, Call) ->
    case cf_endpoint:get(Call) of
        {error, _R} ->
            lager:debug("unable to get endpoint: ~p", [_R]),
            undefined;
        {ok, Endpoint} ->
            Value = wh_json:get_ne_value([<<"music_on_hold">>, Attribute], Endpoint),
            maybe_normalize_moh_attribute(Value, Attribute, Call)
    end.

moh_attributes(EndpointId, Attribute, Call) when is_binary(EndpointId) ->
    case cf_endpoint:get(EndpointId, Call) of
        {error, _} -> undefined;
        {ok, Endpoint} ->
            Value = wh_json:get_ne_value([<<"music_on_hold">>, Attribute], Endpoint),
            maybe_normalize_moh_attribute(Value, Attribute, Call)
    end;
moh_attributes(Endpoint, Attribute, Call) ->
    Value = wh_json:get_ne_value([<<"music_on_hold">>, Attribute], Endpoint),
    maybe_normalize_moh_attribute(Value, Attribute, Call).


-spec maybe_normalize_moh_attribute/3 :: (api_binary(), ne_binary(), whapps_call:call()) -> api_binary().
maybe_normalize_moh_attribute(undefined, _, _) -> undefined;
maybe_normalize_moh_attribute(Value, <<"media_id">>, Call) ->
    MediaId = cf_util:correct_media_path(Value, Call),
    lager:debug("found music_on_hold media_id: '~p'", [MediaId]),
    MediaId;
maybe_normalize_moh_attribute(Value, Attribute, _) ->
    lager:debug("found music_on_hold ~s: '~p'", [Attribute, Value]),
    Value.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec owner_id/1 :: (whapps_call:call()) -> api_binary().
-spec owner_id/2 :: (api_binary(), whapps_call:call()) -> api_binary().

owner_id(Call) ->
    case cf_endpoint:get(Call) of
        {error, _} -> undefined;
        {ok, Endpoint} ->
            case wh_json:get_ne_value(<<"owner_id">>, Endpoint) of
                undefined -> undefined;
                OwnerId ->
                    lager:debug("initiating endpoint is owned by ~s", [OwnerId]),
                    OwnerId
            end
    end.

owner_id(undefined, _Call) -> undefined;
owner_id(ObjectId, Call) ->
    AccountDb = whapps_call:account_db(Call),
    %% TODO: this cache need to be flushed if the ownership changes...
    case couch_mgr:open_cache_doc(AccountDb, ObjectId) of
        {error, _} -> undefined;
        {ok, JObj} ->
            case wh_json:get_ne_value(<<"owner_id">>, JObj) of
                undefined -> undefined;
                OwnerId ->
                    lager:debug("object ~s is owned by ~s", [ObjectId, OwnerId]),
                    OwnerId
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return the precense id for the endpoint
%% @end
%%--------------------------------------------------------------------
-spec presence_id/1 :: (whapps_call:call()) -> api_binary().
-spec presence_id/2 :: (api_binary() | wh_json:object(), whapps_call:call()) -> api_binary().

presence_id(Call) ->
    presence_id(whapps_call:authorizing_id(Call), Call).

presence_id(EndpointId, Call) when is_binary(EndpointId) ->
    case cf_endpoint:get(EndpointId, Call) of
        {ok, Endpoint} -> presence_id(Endpoint, Call);
        {error, _} -> undefined
    end;
presence_id(Endpoint, Call) ->
    <<(wh_json:get_binary_value([<<"sip">>, <<"username">>], Endpoint, whapps_call:request_user(Call)))/binary
      ,$@, (cf_util:get_sip_realm(Endpoint, whapps_call:account_id(Call), whapps_call:request_realm(Call)))/binary>>.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec owned_by/2 :: (api_binary(), whapps_call:call()) -> list().
-spec owned_by/3 :: (api_binary(), atom() | string() | ne_binary(), whapps_call:call()) -> list().

-spec fetch_owned_by/2 :: (api_binary(), whapps_call:call()) ->
                                  list().
-spec fetch_owned_by/3 :: (api_binary(), atom() | string() | ne_binary(), whapps_call:call()) ->
                                  list().

owned_by(undefined, _) -> [];
owned_by(OwnerId, Call) ->
    Attributes = fetch_attributes(owned, Call),
    [V || {[I, _], V} <- Attributes, I =:= OwnerId].

owned_by(undefined, _, _) -> [];
owned_by(OwnerId, false, Call) ->
    wh_cache:erase_local(?CALLFLOW_CACHE, {?MODULE, whapps_call:account_db(Call), owned}),
    owned_by(OwnerId, Call);
owned_by(OwnerId, Attribute, Call) when not is_binary(Attribute) ->
    owned_by(OwnerId, wh_util:to_binary(Attribute), Call);
owned_by(OwnerId, Attribute, Call) ->
    Attributes = fetch_attributes(owned, Call),
    [V || {[I, T], V} <- Attributes, I =:= OwnerId, T =:= Attribute].

fetch_owned_by(OwnerId, Call) ->
    wh_cache:erase_local(?CALLFLOW_CACHE, {?MODULE, whapps_call:account_db(Call), owned}),
    owned_by(OwnerId, Call).

fetch_owned_by(OwnerId, Attribute, Call) ->
    wh_cache:erase_local(?CALLFLOW_CACHE, {?MODULE, whapps_call:account_db(Call), owned}),
    owned_by(OwnerId, Attribute, Call).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec default_cid_number/0 :: () -> ne_binary().
default_cid_number() ->
    whapps_config:get(?CF_CONFIG_CAT, <<"default_caller_id_number">>, ?DEFAULT_CALLER_ID_NUMBER).

-spec default_cid_name/1 :: (wh_json:object()) -> ne_binary().
default_cid_name(Endpoint) ->
    case wh_json:get_ne_value(<<"name">>, Endpoint) of
        undefined -> whapps_config:get_non_empty(<<"callflow">>, <<"default_caller_id_name">>, <<"unknown">>);
        Name -> Name
    end.

-spec get_cid_or_default/3 :: (ne_binary(), ne_binary(), wh_json:object()) -> api_binary().
get_cid_or_default(Attribute, Property, Endpoint) ->
    case wh_json:get_ne_value([<<"caller_id">>, Attribute, Property], Endpoint) of
        undefined -> wh_json:get_ne_value([<<"default">>, Property], Endpoint);
        Value -> Value
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_attributes/2 :: (atom(), whapps_call:call() | api_binary()) -> proplist().
fetch_attributes(_Attribute, undefined) -> [];
fetch_attributes(Attribute, ?NE_BINARY = AccountDb) ->
    case wh_cache:peek_local(?CALLFLOW_CACHE, {?MODULE, AccountDb, Attribute}) of
        {ok, Attributes} -> Attributes;
        {error, not_found} ->
            case couch_mgr:get_all_results(AccountDb, <<"cf_attributes/", (wh_util:to_binary(Attribute))/binary>>) of
                {ok, JObjs} ->
                    Props = [{wh_json:get_value(<<"key">>, JObj), wh_json:get_value(<<"value">>, JObj)}
                             || JObj <- JObjs],
                    wh_cache:store_local(?CALLFLOW_CACHE, {?MODULE, AccountDb, Attribute}, Props, 900),
                    Props;
                {error, R} ->
                    lager:debug("unable to fetch attribute ~s: ~p", [Attribute, R]),
                    []
            end
    end;
fetch_attributes(Attribute, Call) ->
    fetch_attributes(Attribute, whapps_call:account_db(Call)).

-spec flush_attributes/1 :: (ne_binary()) -> any().
flush_attributes(AccountDb) ->
    Keys = wh_cache:filter_local(?CALLFLOW_CACHE
                                 ,fun({?MODULE, AcctDb, _}, _) when AcctDb =:= AccountDb -> true;
                                     (_,_) -> false
                                  end
                                ),
    [wh_cache:erase_local(?CALLFLOW_CACHE, Key) || Key <- Keys].
