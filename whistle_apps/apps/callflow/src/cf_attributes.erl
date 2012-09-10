%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_attributes).

-include("callflow.hrl").

-export([temporal_rules/1]).
-export([call_forward/2, call_forward/3]).
-export([caller_id/2, caller_id/3, caller_id/4]).
-export([callee_id/2, callee_id/3, callee_id/4]).
-export([caller_id_attributes/3, caller_id_attributes/4]).
-export([media_attributes/3, media_attributes/4]).
-export([moh_attributes/2, moh_attributes/3, moh_attributes/4]).
-export([owner_id/1, owner_id/2, fetch_owner_id/2]).
-export([owned_by/2, owned_by/3, fetch_owned_by/2, fetch_owned_by/3]).
-export([friendly_name/2, friendly_name/3]).
-export([presence_id/1, presence_id/2]).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec temporal_rules/1 :: (whapps_call:call()) -> wh_json:json_objects().
temporal_rules(Call) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/temporal_rules">>, [include_docs]) of
        {ok, JObjs} -> JObjs;
        {error, _} -> []
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% TODO: This should use caching, however we need an 'absolute' expiration
%% or on a busy system call forwarding will not appear to disable....
%% @end
%%-----------------------------------------------------------------------------
-spec call_forward/2 :: (cf_api_binary() | wh_json:json_object(), whapps_call:call()) ->
                                'undefined' | wh_json:json_object().
-spec call_forward/3 :: (cf_api_binary(), cf_api_binary(), cf_api_binary() | whapps_call:call()) ->
                                'undefined' | wh_json:json_object().
call_forward(Endpoint, Call) when is_tuple(Endpoint) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    call_forward(EndpointId, Call);
call_forward(EndpointId, Call) ->
    OwnerId = owner_id(EndpointId, Call),
    call_forward(EndpointId, OwnerId, Call).

call_forward(_EndpointId, _OwnerId, undefined) -> 'undefined';
call_forward(EndpointId, OwnerId, ?NE_BINARY = AccountDb) ->
    CallFwd = case couch_mgr:get_all_results(AccountDb, <<"cf_attributes/call_forward">>) of
                  {ok, []} -> [];
                  {ok, JObjs} ->
                      [{Key, wh_json:get_value(<<"value">>, CF)}
                       || CF <- JObjs
                              ,wh_json:is_true([<<"value">>, <<"enabled">>], CF)
                              ,(begin
                                    Key = wh_json:get_value(<<"key">>, CF),
                                    lists:member(Key, [EndpointId, OwnerId])
                                end)
                              ,not wh_util:is_empty(wh_json:get_value([<<"value">>, <<"number">>], CF))
                      ];
                  {error, R}->
                      lager:debug("failed to load call fowarding objects: ~p", [R]),
                      []
              end,
    case props:get_value(EndpointId, CallFwd) of
        undefined ->
            case props:get_value(OwnerId, CallFwd) of
                undefined -> undefined;
                Fwd ->
                    lager:debug("found enabled call forwarding on ~s", [OwnerId]),
                    Fwd
            end;
        Fwd ->
            lager:debug("found enabled call forwarding on ~s", [EndpointId]),
            Fwd
    end;
call_forward(EndpointId, OwnerId, Call) ->
    call_forward(EndpointId, OwnerId, whapps_call:account_db(Call)).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec caller_id/2 :: (ne_binary(), whapps_call:call()) ->
                             {cf_api_binary(), cf_api_binary()}.
-spec caller_id/3 :: (ne_binary() | wh_json:json_object(), ne_binary(), whapps_call:call()) ->
                             {cf_api_binary(), cf_api_binary()}.
-spec caller_id/4 :: (cf_api_binary(), cf_api_binary(), ne_binary(), whapps_call:call()) ->
                             {cf_api_binary(), cf_api_binary()}.

caller_id(Attribute, Call) ->
    caller_id(whapps_call:authorizing_id(Call), whapps_call:kvs_fetch(owner_id, Call), Attribute, Call).

caller_id(Endpoint, Attribute, Call) when is_tuple(Endpoint) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    caller_id(EndpointId, Attribute, Call);
caller_id(EndpointId, Attribute, Call) ->
    OwnerId = owner_id(EndpointId, Call),
    caller_id(EndpointId, OwnerId, Attribute, Call).

caller_id(EndpointId, OwnerId, <<"external">> = Attribute, Call) ->
    CID = get_cid(EndpointId, OwnerId, Attribute, Call),
    CIDNumber = case wh_json:get_ne_value(<<"number">>, CID) of
                    undefined -> whapps_call:caller_id_number(Call);
                    Number -> Number
                end,
    CIDName = case wh_json:get_ne_value(<<"name">>, CID) of
                  undefined -> friendly_name(EndpointId, OwnerId, Call);
                  Name -> Name
              end,
    case whapps_config:get_is_true(<<"callflow">>, <<"ensure_valid_caller_id">>, false) of
        true -> {ensure_valid_caller_id(CIDNumber, Call), CIDName};
        false ->
            lager:debug("found CID ~s in phone_numbers doc", [CIDNumber]),
            {CIDNumber, CIDName}
    end;
caller_id(EndpointId, OwnerId, Attribute, Call) ->
    CID = get_cid(EndpointId, OwnerId, Attribute, Call),
    CIDNum = case wh_json:get_ne_value(<<"number">>, CID) of
                 undefined -> whapps_call:caller_id_number(Call);
                 Number -> Number
             end,
    CIDName = case wh_json:get_ne_value(<<"name">>, CID) of
                  undefined -> friendly_name(EndpointId, OwnerId, Call);
                  Name -> Name
              end,
    
    lager:debug("attempting to prepend caller id ~s '~s'", [CIDNum, CIDName]),
    CIDNum1 = prepend_caller_id_number(Call, CIDNum),
    CIDName1 = prepend_caller_id_name(Call, CIDName),

    lager:debug("using caller id ~s '~s'", [CIDNum1, CIDName1]),
    {CIDNum1, CIDName1}.

-spec ensure_valid_caller_id/2 :: (ne_binary(), whapps_call:call()) -> ne_binary().
ensure_valid_caller_id(CIDNumber, Call) ->
    {ok, PNJObj} = couch_mgr:open_cache_doc(whapps_call:account_db(Call), ?WNM_PHONE_NUMBER_DOC),

    case has_caller_id(CIDNumber, PNJObj) of
        true -> CIDNumber;
        false ->
            lager:debug("failed to find CID ~s in phone_numbers doc", [CIDNumber]),
            {ok, AcctDoc} = couch_mgr:open_cache_doc(whapps_call:account_db(Call), whapps_call:account_id(Call)),

            ensure_valid_default_caller_id(wh_json:get_value([<<"caller_id">>
                                                             ,<<"external">>
                                                             ,<<"number">>
                                                             ], AcctDoc)
                                           ,PNJObj)
    end.

-spec ensure_valid_default_caller_id/2 :: (cf_api_binary(), wh_json:json_object()) -> ne_binary().
ensure_valid_default_caller_id(undefined, PNJObj) ->
    lager:debug("failed to find default account CID, finding first number in service"),
    first_caller_id_number(PNJObj, <<"in_service">>);
ensure_valid_default_caller_id(DefaultAcctCID, PNJObj) ->
    case has_caller_id(DefaultAcctCID, PNJObj) of
        false -> cf_util:default_caller_id_number();
        true ->
            lager:debug("using default account CID ~s", [DefaultAcctCID]),
            DefaultAcctCID
    end.

-spec first_caller_id_number/2 :: (wh_json:json_object(), ne_binary()) -> ne_binary().
first_caller_id_number(PNJObj, NumState) ->
    Pub = wh_json:public_fields(PNJObj),
    case [Num || {Num, NumJObj} <- wh_json:to_proplist(Pub),
                 wh_json:is_json_object(NumJObj) andalso
                     wh_json:get_value(<<"state">>, NumJObj) =:= NumState
         ] of
        [] ->
            lager:debug("failed to find any in-service numbers, using default"),
            cf_util:default_caller_id_number();
        [ActiveNum|_] ->
            lager:debug("setting to first number in service: ~s", [ActiveNum]),
            ActiveNum
    end.

has_caller_id(CIDNumber, PNJObj) ->
    case wh_json:get_value(wnm_util:to_e164(CIDNumber), PNJObj) of
        undefined -> false;
        _ -> true
    end.

-spec get_cid/4 :: (ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> wh_json:json_object().
get_cid(EndpointId, OwnerId, Attribute, Call) ->
    AccountId = whapps_call:account_id(Call),
    CCVs = whapps_call:custom_channel_vars(Call),
    Inception = whapps_call:inception(Call),
    Ids = [EndpointId, OwnerId, AccountId],
    case (Inception =:= <<"off-net">> andalso not wh_json:is_true(<<"Call-Forward">>, CCVs))
        orelse wh_json:is_true(<<"Retain-CID">>, CCVs) of
        true ->
            lager:debug("retaining original caller id"),
            wh_json:from_list([{<<"number">>, whapps_call:caller_id_number(Call)}
                               ,{<<"name">>, whapps_call:caller_id_name(Call)}
                              ]);
        false ->
            lager:debug("find ~s caller id on ~s", [Attribute, wh_util:join_binary(Ids)]),
            Attributes = fetch_attributes(caller_id, Call),
            case search_attributes(Attribute, Ids, Attributes) of
                undefined ->
                    case search_attributes(<<"default">>, [AccountId], Attributes) of
                        undefined -> wh_json:new();
                        {_Id, Value} -> lager:debug("found default caller id on ~s", [_Id]), Value
                    end;
                {_Id, Value} -> lager:debug("found ~s caller id on ~s", [Attribute, _Id]), Value
            end
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec callee_id/2 :: (ne_binary() | wh_json:json_object(), whapps_call:call()) -> {cf_api_binary(), cf_api_binary()}.
-spec callee_id/3 :: (ne_binary() | wh_json:json_object() | 'undefined', ne_binary(), whapps_call:call()) -> {cf_api_binary(), cf_api_binary()}.
-spec callee_id/4 :: (cf_api_binary(), cf_api_binary(), ne_binary(), whapps_call:call()) -> {cf_api_binary(), cf_api_binary()}.

callee_id(Endpoint, Call) ->
    case whapps_call:inception(Call) of
        <<"off-net">> ->
            callee_id(Endpoint, <<"external">>, Call);
        _ ->
            callee_id(Endpoint, <<"internal">>, Call)
    end.

callee_id(Endpoint, Attribute, Call) when is_tuple(Endpoint) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    callee_id(EndpointId, Attribute, Call);
callee_id(EndpointId, Attribute, Call) ->
    OwnerId = owner_id(EndpointId, Call),
    callee_id(EndpointId, OwnerId, Attribute, Call).

callee_id(EndpointId, OwnerId, Attribute, Call) ->
    AccountId = whapps_call:account_id(Call),
    Ids = [EndpointId, OwnerId, AccountId],
    lager:debug("find ~s callee id on ~s", [Attribute, wh_util:join_binary(Ids)]),
    Attributes = fetch_attributes(caller_id, Call),
    CID = case search_attributes(Attribute, Ids, Attributes) of
                      undefined ->
                          case search_attributes(<<"default">>, [AccountId], Attributes) of
                              undefined ->
                                  wh_json:new();
                              {Id, Value} ->
                                  lager:debug("found default callee id on ~s", [Id]),
                                  Value
                          end;
                      {Id, Value} ->
                          lager:debug("found ~s callee id on ~s", [Attribute, Id]),
                          Value
          end,
    CIDNum = case wh_json:get_ne_value(<<"number">>, CID) of
                 undefined -> whapps_call:request_user(Call);
                 Number -> Number
             end,
    CIDName = case wh_json:get_ne_value(<<"name">>, CID) of
                  undefined -> friendly_name(EndpointId, OwnerId, Call);
                  Name -> Name
              end,
    lager:debug("using callee id ~s '~s'", [CIDNum, CIDName]),
    {CIDNum, CIDName}.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec caller_id_attributes/3 :: (ne_binary() | wh_json:json_object(), ne_binary(), whapps_call:call()) -> undefined | ne_binary() | wh_json:json_object().
-spec caller_id_attributes/4 :: (cf_api_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> undefined | ne_binary() | wh_json:json_object().

caller_id_attributes(Endpoint, Attribute, Call) when is_tuple(Endpoint) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    caller_id_attributes(EndpointId, Attribute, Call);
caller_id_attributes(EndpointId, Attribute, Call) ->
    OwnerId = owner_id(EndpointId, Call),
    caller_id_attributes(EndpointId, OwnerId, Attribute, Call).

caller_id_attributes(EndpointId, OwnerId, Attribute, Call) ->
    Ids = [EndpointId, OwnerId, whapps_call:account_id(Call)],
    lager:debug("find caller id attr ~s on ~s", [Attribute, wh_util:join_binary(Ids)]),
    Attributes = fetch_attributes(caller_id, Call),
    case search_attributes(Attribute, Ids, Attributes) of
        undefined ->
            lager:debug("unable to find caller id attribute ~s", [Attribute]),
            undefined;
        {Id, Value} ->
            lager:debug("found caller id attribute ~s on ~s: '~p'", [Attribute, Id, Value]),
            Value
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec media_attributes/3 :: (ne_binary() | wh_json:json_object() | 'undefined', ne_binary(), whapps_call:call()) -> undefined | ne_binary() | list().
-spec media_attributes/4 :: (cf_api_binary(), cf_api_binary(), ne_binary(), whapps_call:call()) -> undefined | ne_binary() | list().

media_attributes(Endpoint, Attribute, Call) when is_tuple(Endpoint) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    media_attributes(EndpointId, Attribute, Call);
media_attributes(EndpointId, Attribute, Call) ->
    OwnerId = owner_id(EndpointId, Call),
    media_attributes(EndpointId, OwnerId, Attribute, Call).

media_attributes(EndpointId, OwnerId, <<"codecs">>, Call) ->
    Audio = media_attributes(EndpointId, OwnerId, <<"audio">>, Call),
    Video = media_attributes(EndpointId, OwnerId, <<"video">>, Call),
    Audio ++ Video;
media_attributes(EndpointId, OwnerId, Attribute, Call) ->
    Ids = [EndpointId, OwnerId, whapps_call:account_id(Call)],
    lager:debug("find media attr ~s on ~s", [Attribute, wh_util:join_binary(Ids)]),
    Attributes = fetch_attributes(media_options, Call),
    case search_attributes(Attribute, Ids, Attributes) of
        undefined when Attribute =:= <<"audio">>; Attribute =:= <<"video">> ->
            []; 
        undefined ->
            lager:debug("unable to find media attribute ~s", [Attribute]),
            undefined;
        {Id, Value} when Attribute =:= <<"audio">>; Attribute =:= <<"video">> ->
            Codecs = wh_json:get_value(<<"codecs">>, Value, []),
            _ = [lager:debug("found media attribute ~s codec ~s on ~s", [Attribute, Codec, Id]) || Codec <- Codecs],
            Codecs;
        {Id, Value} ->
            lager:debug("found media attribute ~s on ~s: '~p'", [Attribute, Id, Value]),
            Value
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec moh_attributes/2 :: (ne_binary(), whapps_call:call()) -> cf_api_binary().
-spec moh_attributes/3 :: (ne_binary() | wh_json:json_object(), ne_binary(), whapps_call:call()) -> cf_api_binary().
-spec moh_attributes/4 :: (cf_api_binary(), cf_api_binary(), ne_binary(), whapps_call:call()) -> cf_api_binary().

moh_attributes(Attribute, Call) ->
    moh_attributes(whapps_call:authorizing_id(Call), Attribute, Call).

moh_attributes(Endpoint, Attribute, Call) when is_tuple(Endpoint) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    moh_attributes(EndpointId, Attribute, Call);
moh_attributes(EndpointId, Attribute, Call) ->
    OwnerId = owner_id(EndpointId, Call),
    moh_attributes(EndpointId, OwnerId, Attribute, Call).

moh_attributes(EndpointId, OwnerId, Attribute, Call) ->
    Ids = [EndpointId, OwnerId, whapps_call:account_id(Call)],
    lager:debug("find moh attr ~s on ~s", [Attribute, wh_util:join_binary(Ids)]),
    Attributes = fetch_attributes(moh_options, Call),
    case search_attributes(Attribute, Ids, Attributes) of
        undefined ->
            lager:debug("unable to find moh attribute ~s", [Attribute]),
            undefined;
        {Id, Value} when Attribute =:= <<"media_id">> ->
            MediaId = cf_util:correct_media_path(Value, Call),
            lager:debug("found moh attribute ~s on ~s: '~p'", [Attribute, Id, MediaId]),
            MediaId;
        {Id, Value} ->
            lager:debug("found moh attribute ~s on ~s: '~p'", [Attribute, Id, Value]),
            Value
    end.
 
%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec owner_id/1 :: (whapps_call:call()) -> cf_api_binary().
-spec owner_id/2 :: (cf_api_binary(), whapps_call:call()) -> cf_api_binary().
-spec fetch_owner_id/2 :: (cf_api_binary(), whapps_call:call()) -> cf_api_binary().
owner_id(Call) ->
    owner_id(whapps_call:authorizing_id(Call), Call).

owner_id(undefined, _Call) ->
    undefined;
owner_id(ObjectId, Call) ->
    Attributes = fetch_attributes(owner, Call),
    case props:get_value(ObjectId, Attributes) of
        undefined ->
            lager:debug("object ~s has no owner", [ObjectId]),
            undefined;
        Value ->
            lager:debug("object ~s is owned by ~s", [ObjectId, Value]),
            Value
    end.

fetch_owner_id(ObjectId, Call) ->
    wh_cache:erase_local(?CALLFLOW_CACHE, {?MODULE, whapps_call:account_db(Call), owner}),
    owner_id(ObjectId, Call).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec owned_by/2 :: (cf_api_binary(), whapps_call:call()) -> list().
-spec owned_by/3 :: (cf_api_binary(), atom() | string() | ne_binary(), whapps_call:call()) -> list().

-spec fetch_owned_by/2 :: (cf_api_binary(), whapps_call:call()) ->
                                  list().
-spec fetch_owned_by/3 :: (cf_api_binary(), atom() | string() | ne_binary(), whapps_call:call()) ->
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
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec friendly_name/2 :: (ne_binary() | wh_json:json_object(), whapps_call:call()) -> ne_binary().
-spec friendly_name/3 :: (ne_binary() | wh_json:json_object() | 'undefined', ne_binary(), whapps_call:call()) -> ne_binary().

friendly_name(Endpoint, Call) when is_tuple(Endpoint) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    friendly_name(EndpointId, Call);
friendly_name(EndpointId, Call) ->
    OwnerId = owner_id(EndpointId, Call),
    friendly_name(EndpointId, OwnerId, Call).

friendly_name(EndpointId, OwnerId, Call) ->
    Ids = [OwnerId, EndpointId],
    lager:debug("find friendly name on ~s", [wh_util:join_binary(Ids)]),
    Attributes = fetch_attributes(friendly_name, Call),
    case search_attributes(<<"friendly_name">>, Ids, Attributes) of
        undefined ->
            lager:debug("unable to find a usable friendly name"),
            whapps_call:caller_id_name(Call);
        {Id, Value} ->
            lager:debug("using name '~s' from ~s", [Value, Id]),
            Value
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return the precense id for the endpoint
%% @end
%%--------------------------------------------------------------------
-spec presence_id/1 :: (whapps_call:call()) ->
                               cf_api_binary().
-spec presence_id/2 :: (cf_api_binary() | wh_json:json_object(), whapps_call:call()) ->
                               cf_api_binary().

presence_id(Call) ->
    presence_id(whapps_call:authorizing_id(Call), Call).

presence_id(undefined, _) ->
    undefined;
presence_id(EndpointId, Call) when is_binary(EndpointId) ->
    case cf_endpoint:get(EndpointId, Call) of
        {ok, Endpoint} -> presence_id(Endpoint, Call);
        {error, _} -> undefined
    end;
presence_id(Endpoint, Call) ->
    <<(wh_json:get_binary_value([<<"sip">>, <<"username">>], Endpoint, whapps_call:request_user(Call)))/binary
      ,$@, (cf_util:get_sip_realm(Endpoint, whapps_call:account_id(Call), whapps_call:request_realm(Call)))/binary>>.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec search_attributes/3 :: (cf_api_binary(), wh_json:json_strings(), proplist()) ->
                                     'undefined' |
                                     {ne_binary(), ne_binary() | wh_json:json_object()}.
search_attributes(_, _, []) -> undefined;
search_attributes(_, [], _) -> undefined;
search_attributes(Attribute, [undefined|T], Attributes) ->
    search_attributes(Attribute, T, Attributes);
search_attributes(Attribute, [Id|T], Attributes) ->
    case fetch_sub_key(Attribute, Id, Attributes) of
        undefined -> search_attributes(Attribute, T, Attributes);
        Value -> {Id, Value}
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_sub_key/3 :: (cf_api_binary(), cf_api_binary(), proplist()) ->
                                 cf_api_binary() | wh_json:json_object().
fetch_sub_key(Attribute, Id, Attributes) ->
    fetch_sub_key(Attribute, props:get_value(Id, Attributes)).
fetch_sub_key(_, undefined) ->
    undefined;
fetch_sub_key(Attribute, JObj) ->
    wh_json:get_value(Attribute, JObj).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_attributes/2 :: (atom(), whapps_call:call() | cf_api_binary()) -> proplist().
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

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec prepend_caller_id_name/2 :: (whapps_call:call(), ne_binary()) -> ne_binary().
prepend_caller_id_name(Call, CIDName) ->
    case whapps_call:kvs_fetch(prepend_cid_name, Call) of
        undefined -> CIDName;
        Prefix -> <<Prefix/binary, CIDName/binary>>
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec prepend_caller_id_number/2 :: (whapps_call:call(), ne_binary()) -> ne_binary().
prepend_caller_id_number(Call, CIDNum) ->
    case whapps_call:kvs_fetch(prepend_cid_number, Call) of
        undefined -> CIDNum;
        Prefix -> <<Prefix/binary, CIDNum/binary>>
    end.
