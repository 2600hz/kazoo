%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% @end
%%% Created : 7 April 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_attributes).

-include("callflow.hrl").

-export([call_forward/2]).
-export([caller_id/4]).
-export([callee_id/4]).
-export([caller_id_options/4]).
-export([owner_id/2, owned_by/2, owned_by/3]).
-export([temporal_rules/1]).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec temporal_rules/1 :: (Call) -> 'undefined' | json_objects() when
      Call :: #cf_call{}.
temporal_rules(#cf_call{account_db=Db}) ->
    case couch_mgr:get_results(Db, {<<"cf_attributes">>, <<"temporal_rules">>}, [{<<"include_docs">>, true}]) of
        {ok, JObj} -> JObj;
        {error, _} -> []
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% TODO: This should use caching, however we need an 'absolute' expiration
%% or on a busy system call forwarding will not appear to disable....
%% @end
%%-----------------------------------------------------------------------------
-spec call_forward/2 :: (DeviceId, Call) -> undefined | json_object() when
      DeviceId :: cf_api_binary(),
      Call :: #cf_call{}.
call_forward(DeviceId, #cf_call{account_db=Db}=Call) ->
    OwnerId = owner_id(DeviceId, Call),
    CallFwd = case couch_mgr:get_all_results(Db, get_view(call_forward)) of
                  {ok, JObj} ->
                      [{Key, wh_json:get_value(<<"value">>, CF)}
                       || CF <- JObj
                              ,wh_json:is_true([<<"value">>, <<"enabled">>], CF)
                              ,(begin
                                    Key = wh_json:get_value(<<"key">>, CF),
                                    lists:member(Key, [DeviceId, OwnerId])
                                end)];
                  _ ->
                      []
              end,
    case props:get_value(DeviceId, CallFwd) of
        undefined ->
            Fwd = props:get_value(OwnerId, CallFwd),
            Fwd =/= undefined andalso ?LOG("found enabled call forwarding on ~s", [OwnerId]),
            Fwd;
        Fwd ->
            ?LOG("found enabled call forwarding on ~s", [DeviceId]),
            Fwd
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec caller_id/4 :: (CIDType, DeviceId, OwnerId, Call) -> tuple(cf_api_binary(), cf_api_binary()) when
      CIDType :: cf_api_binary(),
      DeviceId :: cf_api_binary(),
      OwnerId :: cf_api_binary(),
      Call :: #cf_call{}.
caller_id(CIDType, DeviceId, OwnerId, #cf_call{account_id=AccountId, cid_number=Num, cid_name=Name, channel_vars=CCVs}=Call) ->
    Ids = [begin ?LOG("looking for caller id type ~s on doc ~s", [CIDType, Id]), Id end
           || Id <- [DeviceId, OwnerId, AccountId], Id =/= undefined],
    case wh_util:is_true(wh_json:get_value(<<"Retain-CID">>, CCVs)) of
        true ->
            ?LOG("retaining caller id ~s '~s'", [Num, Name]),
            {Num, Name};
        false ->
            Attributes = fetch_attributes(caller_id, 3600, Call),
            CID = case search_attributes(CIDType, Ids, Attributes) of
                      undefined ->
                          search_attributes(<<"default">>, [AccountId], Attributes);
                      Property -> Property
                  end,
            CIDNumber = wh_json:get_value(<<"number">>, CID, Num),
            CIDName = wh_json:get_value(<<"name">>, CID, Num),
            ?LOG("using caller id ~s '~s'", [CIDNumber, CIDName]),
            {CIDNumber, CIDName}
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec callee_id/4 :: (CIDType, DeviceId, OwnerId, Call) -> tuple(cf_api_binary(), cf_api_binary()) when
      CIDType :: cf_api_binary(),
      DeviceId :: cf_api_binary(),
      OwnerId :: cf_api_binary(),
      Call :: #cf_call{}.
callee_id(CIDType, DeviceId, OwnerId, #cf_call{account_id=AccountId, request_user=Num}=Call) ->
    Ids = [begin ?LOG("looking for callee id type ~s on doc ~s", [CIDType, Id]), Id end
           || Id <- [DeviceId, OwnerId, AccountId], Id =/= undefined],
    Attributes = fetch_attributes(caller_id, 3600, Call),
    CID = case search_attributes(CIDType, Ids, Attributes) of
              undefined ->
                  search_attributes(<<"default">>, [AccountId], Attributes);
              Property -> Property
          end,
    CIDNumber = wh_json:get_value(<<"number">>, CID, Num),
    CIDName = wh_json:get_value(<<"name">>, CID, Num),
    ?LOG("using callee id ~s '~s'", [CIDNumber, CIDName]),
    {CIDNumber, CIDName}.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec caller_id_options/4 :: (Option, DeviceId, OwnerId, Call) -> cf_api_binary() when
      Option :: binary(),
      DeviceId :: cf_api_binary(),
      OwnerId :: cf_api_binary(),
      Call :: #cf_call{}.
caller_id_options(Option, DeviceId, OwnerId, #cf_call{account_id=AccountId}=Call) ->
    Ids = [begin ?LOG("looking for caller id option ~s on doc ~s", [Option, Id]), Id end
           || Id <- [DeviceId, OwnerId, AccountId], Id =/= undefined],
    Attributes = fetch_attributes(caller_id_options, 3600, Call),
    case search_attributes(Option, Ids, Attributes) of
        undefined ->
            undefined;
        Property -> Property
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec owner_id/2 :: (ObjectId, Call) -> undefined | json_object() when
      ObjectId :: undefined | binary(),
      Call :: #cf_call{}.
owner_id(undefined, _) ->
    undefined;
owner_id(ObjectId, #cf_call{account_db=Db})->
    Id = wh_util:to_binary(ObjectId),
    case couch_mgr:get_results(Db, {<<"cf_attributes">>, <<"owner">>}, [{<<"key">>, Id}]) of
        {ok, []} ->
            undefined;
        {ok, JObj} ->
            wh_json:get_value(<<"value">>, hd(JObj));
        {error, _} ->
            undefined
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec owned_by/2 :: (OwnerId, Call) -> undefined | list() when
      OwnerId :: undefined | binary(),
      Call :: #cf_call{}.
owned_by(undefined, _) ->
    undefined;
owned_by(OwnerId, #cf_call{account_db=Db})->
    Id = wh_util:to_binary(OwnerId),

    case couch_mgr:get_results(Db, <<"cf_attributes/owned">>, [{<<"start_key">>, [Id, false]}, {<<"end_key">>, [Id, ?EMPTY_JSON_OBJECT]} ]) of
        {ok, []} ->
            undefined;
        {ok, JObj} ->
	    [wh_json:get_value(<<"id">>, D)  || D <- JObj];
        {error, _} ->
            undefined
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% Returns a list of doc ID of the specified type for thiw owner
%% @end
%%-----------------------------------------------------------------------------
-spec owned_by/3 :: (OwnerId, Call, Type) -> undefined | list() when
      OwnerId :: undefined | binary(),
      Call :: #cf_call{},
      Type :: atom().
owned_by(undefined, _, _) ->
    undefined;
owned_by(OwnerId, #cf_call{account_db=Db}, Type)->
    Id = wh_util:to_binary(OwnerId),
    T = wh_util:to_binary(Type),

    case couch_mgr:get_results(Db, <<"cf_attributes/owned">>, [{<<"key">>, [Id, T]}]) of
        {ok, []} ->
            undefined;
        {ok, JObj} ->
	    [wh_json:get_value(<<"id">>, D)  || D <- JObj];
        {error, _} ->
            undefined
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec search_attributes/3 :: (Key, Ids, Attributes) -> cf_api_binary() when
      Key :: cf_api_binary(),
      Ids :: list(),
      Attributes :: proplist().
search_attributes(_, _, []) ->
    undefined;
search_attributes(_, [], _) ->
    undefined;
search_attributes(Key, [undefined|T], Attributes) ->
    search_attributes(Key, T, Attributes);
search_attributes(Key, [Id|T], Attributes) ->
    case fetch_sub_key(Key, Id, Attributes) of
        undefined ->
            search_attributes(Key, T, Attributes);
        Property -> Property
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_sub_key/3 :: (Key, Id, Attributes) -> cf_api_binary() when
      Key :: cf_api_binary(),
      Id :: cf_api_binary(),
      Attributes :: proplist().
fetch_sub_key(Key, Id, Attributes) ->
    fetch_sub_key(Key, props:get_value(Id, Attributes)).
fetch_sub_key(_, undefined) ->
    undefined;
fetch_sub_key(Key, JObj) ->
    wh_json:get_value(Key, JObj).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_attributes/3 :: (Attribute, Expires, Call) -> list() when
      Attribute :: atom(),
      Expires :: non_neg_integer(),
      Call :: #cf_call{}.
fetch_attributes(Attribute, Expires, #cf_call{account_db=Db}) ->
    case wh_cache:peek({cf_attribute, Db, Attribute}) of
        {ok, Attributes} ->
            Attributes;
        {error, not_found} ->
            case couch_mgr:get_all_results(Db, get_view(Attribute)) of
                {ok, JObj} ->
                    Properties = [{wh_json:get_value(<<"key">>, Property), wh_json:get_value(<<"value">>, Property)}
                                  || Property <- JObj],
                    wh_cache:store({cf_attribute, Db, Attribute}, Properties, Expires),
                    Properties;
                {error, _} ->
                    []
            end
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec get_view/1 :: (Attribute) -> tuple(binary(), binary()) when
      Attribute :: atom().
get_view(Attribute) ->
    {<<"cf_attributes">>, wh_util:to_binary(Attribute)}.
