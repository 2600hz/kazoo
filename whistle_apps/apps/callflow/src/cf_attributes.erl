%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% @end
%%% Created : 7 April 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_attributes).

-include("callflow.hrl").

-export([temporal_rules/1]).
-export([call_forward/2, call_forward/3]).
-export([caller_id/2, caller_id/3, caller_id/4]).
-export([callee_id/2, callee_id/3, callee_id/4]).
-export([caller_id_attributes/3, caller_id_attributes/4]).
-export([media_attributes/3, media_attributes/4]).
-export([moh_attributes/3, moh_attributes/4]).
-export([owner_id/2, fetch_owner_id/2]).
-export([owned_by/2, owned_by/3, fetch_owned_by/2, fetch_owned_by/3]).
-export([friendly_name/2, friendly_name/3]).
-export([presence_id/1, presence_id/2]).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec temporal_rules/1 :: (#cf_call{}) -> json_objects().
temporal_rules(#cf_call{account_db=Db}) ->
    case couch_mgr:get_results(Db, {<<"cf_attributes">>, <<"temporal_rules">>}
                               ,[{<<"include_docs">>, true}]) of
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
-spec call_forward/2 :: ('undefined' | ne_binary() | json_object(), #cf_call{}) -> 'undefined' | json_object().
-spec call_forward/3 :: ('undefined' | ne_binary(), 'undefined' | ne_binary(), #cf_call{}) -> 'undefined' | json_object().

call_forward(Endpoint, Call) when is_tuple(Endpoint) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    call_forward(EndpointId, Call);
call_forward(EndpointId, Call) ->
    OwnerId = owner_id(EndpointId, Call),
    call_forward(EndpointId, OwnerId, Call).

call_forward(EndpointId, OwnerId, #cf_call{account_db=Db}) ->
    CallFwd = case couch_mgr:get_all_results(Db, {<<"cf_attributes">>, <<"call_forward">>}) of
                  {ok, JObj} ->
                      [{Key, wh_json:get_value(<<"value">>, CF)}
                       || CF <- JObj
                              ,wh_json:is_true([<<"value">>, <<"enabled">>], CF)
                              ,(begin
                                    Key = wh_json:get_value(<<"key">>, CF),
                                    lists:member(Key, [EndpointId, OwnerId])
                                end)
                              ,not wh_util:is_empty(wh_json:get_value([<<"value">>, <<"number">>], CF))
                      ];
                  {error, R}->
                      ?LOG("failed to load call fowarding objects: ~p", [R]),
                      []
              end,
    case props:get_value(EndpointId, CallFwd) of
        undefined ->
            Fwd = props:get_value(OwnerId, CallFwd),
            Fwd =/= undefined andalso ?LOG("found enabled call forwarding on ~s", [OwnerId]),
            Fwd;
        Fwd ->
            ?LOG("found enabled call forwarding on ~s", [EndpointId]),
            Fwd
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec caller_id/2 :: (ne_binary(), #cf_call{}) -> tuple(cf_api_binary(), cf_api_binary()).
-spec caller_id/3 :: (ne_binary() | json_object(), ne_binary(), #cf_call{}) -> tuple(cf_api_binary(), cf_api_binary()).
-spec caller_id/4 :: (ne_binary() | 'undefined', ne_binary() | 'undefined', ne_binary(), #cf_call{}) -> tuple(cf_api_binary(), cf_api_binary()).

caller_id(Attribute, #cf_call{authorizing_id=EndpointId, owner_id=OwnerId}=Call) ->
    caller_id(EndpointId, OwnerId, Attribute, Call).

caller_id(Endpoint, Attribute, Call) when is_tuple(Endpoint) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    caller_id(EndpointId, Attribute, Call);
caller_id(EndpointId, Attribute, Call) ->
    OwnerId = owner_id(EndpointId, Call),
    caller_id(EndpointId, OwnerId, Attribute, Call).

caller_id(EndpointId, OwnerId, Attribute, #cf_call{account_id=AccountId, channel_vars=CCVs, inception=Inception
                                                   ,cid_number=OrgNum, cid_name=OrgName}=Call) ->
    Ids = [Id || Id <- [EndpointId, OwnerId, AccountId], Id =/= undefined],
    CID = case (Inception =:= <<"off-net">> andalso not wh_json:is_true(<<"Call-Forward">>, CCVs))
              orelse wh_json:is_true(<<"Retain-CID">>, CCVs) of
              true ->
                  ?LOG("retaining original caller id"),
                  wh_json:from_list([{<<"number">>, OrgNum}, {<<"name">>, OrgName}]);
              false ->
                  ?LOG("find ~s caller id on ~s", [Attribute, wh_util:join_binary(Ids)]),
                  Attributes = fetch_attributes(caller_id, Call),
                  case search_attributes(Attribute, Ids, Attributes) of
                      undefined ->
                          case search_attributes(<<"default">>, [AccountId], Attributes) of
                              undefined ->
                                  ?EMPTY_JSON_OBJECT;
                              {Id, Value} ->
                                  ?LOG("found default caller id on ~s", [Id]),
                                  Value
                          end;
                      {Id, Value} ->
                          ?LOG("found ~s caller id on ~s", [Attribute, Id]),
                          Value
                  end
          end,
    CIDNum = case wh_json:get_ne_value(<<"number">>, CID) of
                 undefined -> OrgNum;
                 Number -> Number
             end,
    CIDName = case wh_json:get_ne_value(<<"name">>, CID) of
                  undefined -> friendly_name(EndpointId, OwnerId, Call);
                  Name -> Name
              end,
    ?LOG("using caller id ~s '~s'", [CIDNum, CIDName]),
    {CIDNum, CIDName}.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec callee_id/2 :: (ne_binary() | json_object(), #cf_call{}) -> {cf_api_binary(), cf_api_binary()}.
-spec callee_id/3 :: (ne_binary() | json_object() | 'undefined', ne_binary(), #cf_call{}) -> {cf_api_binary(), cf_api_binary()}.
-spec callee_id/4 :: (ne_binary() | 'undefined', ne_binary() | 'undefined', ne_binary(), #cf_call{}) -> {cf_api_binary(), cf_api_binary()}.

callee_id(Endpoint, #cf_call{inception=Inception}=Call) ->
    case Inception of
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

callee_id(EndpointId, OwnerId, Attribute, #cf_call{account_id=AccountId, request_user=RUser}=Call) ->
    Ids = [Id || Id <- [EndpointId, OwnerId, AccountId], Id =/= undefined],
    ?LOG("find ~s callee id on ~s", [Attribute, wh_util:join_binary(Ids)]),
    Attributes = fetch_attributes(caller_id, Call),
    CID = case search_attributes(Attribute, Ids, Attributes) of
                      undefined ->
                          case search_attributes(<<"default">>, [AccountId], Attributes) of
                              undefined ->
                                  ?EMPTY_JSON_OBJECT;
                              {Id, Value} ->
                                  ?LOG("found default callee id on ~s", [Id]),
                                  Value
                          end;
                      {Id, Value} ->
                          ?LOG("found ~s callee id on ~s", [Attribute, Id]),
                          Value
          end,
    CIDNum = case wh_json:get_ne_value(<<"number">>, CID) of
                 undefined -> RUser;
                 Number -> Number
             end,
    CIDName = case wh_json:get_ne_value(<<"name">>, CID) of
                  undefined -> friendly_name(EndpointId, OwnerId, Call);
                  Name -> Name
              end,
    ?LOG("using callee id ~s '~s'", [CIDNum, CIDName]),
    {CIDNum, CIDName}.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec caller_id_attributes/3 :: (ne_binary() | json_object(), ne_binary(), #cf_call{}) -> undefined | ne_binary().
-spec caller_id_attributes/4 :: (ne_binary() | 'undefined', ne_binary(), ne_binary(), #cf_call{}) -> undefined | ne_binary().

caller_id_attributes(Endpoint, Attribute, Call) when is_tuple(Endpoint) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    caller_id_attributes(EndpointId, Attribute, Call);
caller_id_attributes(EndpointId, Attribute, Call) ->
    OwnerId = owner_id(EndpointId, Call),
    caller_id_attributes(EndpointId, OwnerId, Attribute, Call).

caller_id_attributes(EndpointId, OwnerId, Attribute, #cf_call{account_id=AccountId}=Call) ->
    Ids = [Id || Id <- [EndpointId, OwnerId, AccountId], Id =/= undefined],
    ?LOG("find caller id attr ~s on ~s", [Attribute, wh_util:join_binary(Ids)]),
    Attributes = fetch_attributes(caller_id_options, Call),
    case search_attributes(Attribute, Ids, Attributes) of
        undefined ->
            ?LOG("unable to find caller id attribute ~s", [Attribute]),
            undefined;
        {Id, Value} ->
            ?LOG("found caller id attribute ~s on ~s: '~p'", [Attribute, Id, Value]),
            Value
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec media_attributes/3 :: (ne_binary() | json_object() | 'undefined', ne_binary(), #cf_call{}) -> undefined | ne_binary() | list().
-spec media_attributes/4 :: (ne_binary() | 'undefined', ne_binary() | 'undefined', ne_binary(), #cf_call{}) -> undefined | ne_binary() | list().

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
media_attributes(EndpointId, OwnerId, Attribute, #cf_call{account_id=AccountId}=Call) ->
    Ids = [Id || Id <- [EndpointId, OwnerId, AccountId], Id =/= undefined],
    ?LOG("find media attr ~s on ~s", [Attribute, wh_util:join_binary(Ids)]),
    Attributes = fetch_attributes(media_options, Call),
    case search_attributes(Attribute, Ids, Attributes) of
        undefined when Attribute =:= <<"audio">>; Attribute =:= <<"video">> ->
            []; 
        undefined ->
            ?LOG("unable to find media attribute ~s", [Attribute]),
            undefined;
        {Id, Value} when Attribute =:= <<"audio">>; Attribute =:= <<"video">> ->
            Codecs = wh_json:get_value(<<"codecs">>, Value, []),
            _ = [?LOG("found media attribute ~s codec ~s on ~s", [Attribute, Codec, Id]) || Codec <- Codecs],
            Codecs;
        {Id, Value} ->
            ?LOG("found media attribute ~s on ~s: '~p'", [Attribute, Id, Value]),
            Value
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec moh_attributes/3 :: (ne_binary() | json_object(), ne_binary(), #cf_call{}) -> 'undefined' | ne_binary().
-spec moh_attributes/4 :: (ne_binary() | 'undefined', ne_binary() | 'undefined', ne_binary(), #cf_call{}) -> 'undefined' | ne_binary().

moh_attributes(Endpoint, Attribute, Call) when is_tuple(Endpoint) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    moh_attributes(EndpointId, Attribute, Call);
moh_attributes(EndpointId, Attribute, Call) ->
    OwnerId = owner_id(EndpointId, Call),
    moh_attributes(EndpointId, OwnerId, Attribute, Call).

moh_attributes(EndpointId, OwnerId, Attribute, #cf_call{account_id=AccountId}=Call) ->
    Ids = [Id || Id <- [EndpointId, OwnerId, AccountId], Id =/= undefined],
    ?LOG("find moh attr ~s on ~s", [Attribute, wh_util:join_binary(Ids)]),
    Attributes = fetch_attributes(moh_options, Call),
    case search_attributes(Attribute, Ids, Attributes) of
        undefined ->
            ?LOG("unable to find moh attribute ~s", [Attribute]),
            undefined;
        {Id, Value} when Attribute =:= <<"media_id">> ->
            MediaId = cf_util:correct_media_path(Value, Call),
            ?LOG("found moh attribute ~s on ~s: '~p'", [Attribute, Id, MediaId]),
            MediaId;
        {Id, Value} ->
            ?LOG("found moh attribute ~s on ~s: '~p'", [Attribute, Id, Value]),
            Value
    end.
 
%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec owner_id/2 :: ('undefined' | ne_binary(), #cf_call{}) -> 'undefined' | ne_binary().
-spec fetch_owner_id/2 :: ('undefined' | ne_binary(), #cf_call{}) -> 'undefined' | ne_binary().

owner_id(undefined, _Call) ->
    undefined;
owner_id(ObjectId, Call) ->
    Attributes = fetch_attributes(owner, Call),
    case props:get_value(ObjectId, Attributes) of
        undefined ->
            ?LOG("object ~s has no owner", [ObjectId]),
            undefined;
        Value ->
            ?LOG("object ~s is owned by ~s", [ObjectId, Value]),
            Value
    end.

fetch_owner_id(ObjectId, #cf_call{account_db=Db}=Call) ->
    wh_cache:erase({?MODULE, Db, owner}),
    owner_id(ObjectId, Call).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec owned_by/2 :: ('undefined' | ne_binary(), #cf_call{}) -> list().
-spec owned_by/3 :: ('undefined' | ne_binary(), atom() | string() | ne_binary(), #cf_call{}) -> list().

-spec fetch_owned_by/2 :: ('undefined' | ne_binary(), #cf_call{}) -> list().
-spec fetch_owned_by/3 :: ('undefined' | ne_binary(), atom() | string() | ne_binary(), #cf_call{}) -> list().

owned_by(undefined, _) ->
    [];
owned_by(OwnerId, Call) ->
    Attributes = fetch_attributes(owned, Call),
    [V || {[I, _], V} <- Attributes, I =:= OwnerId].

owned_by(undefined, _, _) ->
    [];
owned_by(OwnerId, false, #cf_call{account_db=Db}=Call) ->
    wh_cache:erase({?MODULE, Db, owned}),
    owned_by(OwnerId, Call);
owned_by(OwnerId, Attribute, Call) when not is_binary(Attribute) ->
    owned_by(OwnerId, wh_util:to_binary(Attribute), Call);
owned_by(OwnerId, Attribute, Call) ->
    Attributes = fetch_attributes(owned, Call),
    [V || {[I, T], V} <- Attributes, I =:= OwnerId, T =:= Attribute].

fetch_owned_by(OwnerId, #cf_call{account_db=Db}=Call) ->
    wh_cache:erase({?MODULE, Db, owned}),
    owned_by(OwnerId, Call).

fetch_owned_by(OwnerId, Attribute, #cf_call{account_db=Db}=Call) ->
    wh_cache:erase({?MODULE, Db, owned}),
    owned_by(OwnerId, Attribute, Call).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec friendly_name/2 :: (ne_binary() | json_object(), #cf_call{}) -> ne_binary().
-spec friendly_name/3 :: (ne_binary() | json_object() | 'undefined', ne_binary(), #cf_call{}) -> ne_binary().

friendly_name(Endpoint, Call) when is_tuple(Endpoint) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    friendly_name(EndpointId, Call);
friendly_name(EndpointId, Call) ->
    OwnerId = owner_id(EndpointId, Call),
    friendly_name(EndpointId, OwnerId, Call).

friendly_name(EndpointId, OwnerId, #cf_call{cid_name=CIDName}=Call) ->
    Ids = [Id || Id <- [OwnerId, EndpointId], Id =/= undefined],
    ?LOG("find friendly name on ~s", [wh_util:join_binary(Ids)]),
    Attributes = fetch_attributes(friendly_name, Call),
    case search_attributes(<<"friendly_name">>, Ids, Attributes) of
        undefined ->
            ?LOG("unable to find a usable friendly name"),
            CIDName;
        {Id, Value} ->
            ?LOG("using name '~s' from ~s", [Value, Id]),
            Value
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return the precense id for the endpoint
%% @end
%%--------------------------------------------------------------------
-spec presence_id/1 :: (#cf_call{}) -> 'undefined' | ne_binary().
-spec presence_id/2 :: ('undefined' | ne_binary() | json_object(), #cf_call{}) -> 'undefined' | ne_binary().

presence_id(#cf_call{authorizing_id=AuthId}=Call) ->
    presence_id(AuthId, Call).

presence_id(undefined, _) ->
    undefined;
presence_id(EndpointId, Call) when is_binary(EndpointId) ->
    case cf_endpoint:get(EndpointId, Call) of
        {ok, Endpoint} ->
            presence_id(Endpoint, Call);
        {error, _} ->
            undefined
    end;
presence_id(Endpoint, #cf_call{request_user=RUser, request_realm=RRealm, account_id=AccountId}) ->
    <<(wh_json:get_value([<<"sip">>, <<"username">>], Endpoint, RUser))/binary
      ,$@, (cf_util:get_sip_realm(Endpoint, AccountId, RRealm))/binary>>.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec search_attributes/3 :: (cf_api_binary(), [ne_binary(),...] | [], proplist()) -> 'undefined' | {ne_binary(), ne_binary()}.
search_attributes(_, _, []) ->
    undefined;
search_attributes(_, [], _) ->
    undefined;
search_attributes(Attribute, [undefined|T], Attributes) ->
    search_attributes(Attribute, T, Attributes);
search_attributes(Attribute, [Id|T], Attributes) ->
    case fetch_sub_key(Attribute, Id, Attributes) of
        undefined ->
            search_attributes(Attribute, T, Attributes);
        Value ->
            {Id, Value}
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_sub_key/3 :: (cf_api_binary(), cf_api_binary(), proplist()) -> cf_api_binary().
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
-spec fetch_attributes/2 :: (atom(), #cf_call{}) -> proplist().
fetch_attributes(Attribute, #cf_call{account_db=Db}) ->
    case wh_cache:peek({?MODULE, Db, Attribute}) of
        {ok, Attributes} ->
            Attributes;
        {error, not_found} ->
            case couch_mgr:get_all_results(Db, {<<"cf_attributes">>, wh_util:to_binary(Attribute)}) of
                {ok, JObjs} ->
                    Props = [{wh_json:get_value(<<"key">>, JObj), wh_json:get_value(<<"value">>, JObj)}
                                  || JObj <- JObjs],
                    wh_cache:store({?MODULE, Db, Attribute}, Props, 900),
                    Props;
                {error, R} ->
                    ?LOG("unable to fetch attribute ~s: ~p", [Attribute, R]),
                    []
            end
    end.
