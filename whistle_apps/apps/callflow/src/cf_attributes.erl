%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% @end
%%% Created : 7 April 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_attributes).

-include("callflow.hrl").

-export([call_forward/3]).
-export([caller_id/4]).
-export([callee_id/2, callee_id/3, callee_id/4]).
-export([caller_id_attributes/3, caller_id_attributes/4]).
-export([media_attributes/3, media_attributes/4]).
-export([owner_id/2, owned_by/2, owned_by/3]).
-export([temporal_rules/1]).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec temporal_rules/1 :: (Call) -> undefined | json_objects() when
      Call :: #cf_call{}.
temporal_rules(#cf_call{account_db=Db}) ->
    case couch_mgr:get_results(Db, get_view(temporal_rules), [{<<"include_docs">>, true}]) of
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
-spec call_forward/3 :: (EndpointId, OwnerId, Call) -> undefined | json_object() when
      EndpointId :: cf_api_binary(),
      OwnerId :: cf_api_binary(),
      Call :: #cf_call{}.
call_forward(EndpointId, OwnerId, #cf_call{account_db=Db}) ->
    CallFwd = case couch_mgr:get_all_results(Db, get_view(call_forward)) of
                  {ok, JObj} ->
                      [{Key, wh_json:get_value(<<"value">>, CF)}
                       || CF <- JObj
                              ,wh_json:is_true([<<"value">>, <<"enabled">>], CF)
                              ,(begin
                                    Key = wh_json:get_value(<<"key">>, CF),
                                    lists:member(Key, [EndpointId, OwnerId])
                                end)];
                  _ ->
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
-spec caller_id/4 :: (CIDType, EndpointId, OwnerId, Call) -> tuple(cf_api_binary(), cf_api_binary()) when
      CIDType :: cf_api_binary(),
      EndpointId :: cf_api_binary(),
      OwnerId :: cf_api_binary(),
      Call :: #cf_call{}.
caller_id(CIDType, EndpointId, OwnerId, #cf_call{account_id=AccountId, cid_number=Num, cid_name=Name, channel_vars=CCVs}=Call) ->
    Ids = [begin ?LOG("looking for caller id type ~s on doc ~s", [CIDType, Id]), Id end
           || Id <- [EndpointId, OwnerId, AccountId], Id =/= undefined],
    case wh_util:is_true(wh_json:get_value(<<"Retain-CID">>, CCVs)) of
        true ->
            ?LOG("retaining caller id ~s '~s'", [Num, Name]),
            {Num, Name};
        false ->
            Attributes = fetch_attributes(caller_id, 3600, Call),
            CID = case search_attributes(CIDType, Ids, Attributes) of
                      undefined ->
                          search_attributes(<<"default">>, [AccountId], Attributes);
                      Value -> Value
                  end,
            CIDNumber = wh_json:get_value(<<"number">>, CID, Num),
            CIDName = wh_json:get_value(<<"name">>, CID, Name),
            ?LOG("using caller id ~s '~s'", [CIDNumber, CIDName]),
            {CIDNumber, CIDName}
   end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec callee_id/2 :: (ne_binary() | json_object(), #cf_call{}) -> tuple(cf_api_binary(), cf_api_binary()).
-spec callee_id/3 :: (ne_binary() | json_object(), ne_binary(), #cf_call{}) -> tuple(cf_api_binary(), cf_api_binary()).
-spec callee_id/4 :: (ne_binary(), ne_binary(), ne_binary(), #cf_call{}) -> tuple(cf_api_binary(), cf_api_binary()).

callee_id(Endpoint, #cf_call{inception=Inception}=Call) ->
    case Inception of
        <<"off-net">> ->
            callee_id(Endpoint, <<"external">>, Call);
        _ ->
            callee_id(Endpoint, <<"internal">>, Call)
    end.

callee_id(Endpoint, Type, #cf_call{account_db=Db}=Call) when is_binary(Endpoint) ->
    case couch_mgr:open_doc(Db, Endpoint) of
        {ok, JObj} ->
            callee_id(JObj, Type, Call);
        {error, R} ->
            ?LOG("unable to load endpoint ~s for callee id: ~p", [Endpoint, R]),
            {undefined, undefined}
    end;
callee_id(Endpoint, Type, #cf_call{request_user=RUser}=Call) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    OwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
    callee_id(EndpointId, OwnerId, Type
              ,Call#cf_call{request_realm=wh_json:get_value(<<"name">>, Endpoint, RUser)}).

callee_id(EndpointId, OwnerId, Type, #cf_call{account_id=AccountId, request_user=RUser
                                              ,request_realm=RRealm}=Call) ->
    Ids = [begin ?LOG("looking for callee id type ~s on doc ~s", [Type, Id]), Id end
           || Id <- [EndpointId, OwnerId, AccountId], Id =/= undefined],
    Attributes = fetch_attributes(caller_id, 3600, Call),
    CID = case search_attributes(Type, Ids, Attributes) of
              undefined ->
                  search_attributes(<<"default">>, [AccountId], Attributes);
              Value -> Value
          end,
    CIDNumber = wh_json:get_value(<<"number">>, CID, RUser),
    CIDName = wh_json:get_value(<<"name">>, CID, RRealm),
    ?LOG("using callee id ~s '~s'", [CIDNumber, CIDName]),
    {CIDNumber, CIDName}.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec caller_id_attributes/3 :: (ne_binary() | json_object(), ne_binary(), #cf_call{}) -> undefined | ne_binary().
-spec caller_id_attributes/4 :: (ne_binary(), ne_binary(), ne_binary(), #cf_call{}) -> undefined | ne_binary().

caller_id_attributes(Endpoint, Attribute, #cf_call{account_db=Db}=Call) when is_binary(Endpoint) ->
    case couch_mgr:open_doc(Db, Endpoint) of
        {ok, JObj} ->
            caller_id_attributes(JObj, Attribute, Call);
        {error, R} ->
            ?LOG("unable to load endpoint ~s for caller id attribute ~s: ~p", [Endpoint, Attribute, R]),
            undefined
    end;
caller_id_attributes(Endpoint, Attribute, Call) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    OwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
    caller_id_attributes(EndpointId, OwnerId, Attribute, Call).

caller_id_attributes(EndpointId, OwnerId, Attribute, #cf_call{account_id=AccountId}=Call) ->
    Ids = [begin ?LOG("looking for caller id attribute ~s on doc ~s", [Attribute, Id]), Id end
           || Id <- [EndpointId, OwnerId, AccountId], Id =/= undefined],
    Attributes = fetch_attributes(caller_id_options, 3600, Call),
    case search_attributes(Attribute, Ids, Attributes) of
        undefined ->
            undefined;
        Value -> Value
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec media_attributes/3 :: (ne_binary() | json_object(), ne_binary(), #cf_call{}) -> undefined | ne_binary() | list().
-spec media_attributes/4 :: (ne_binary(), ne_binary(), ne_binary(), #cf_call{}) -> undefined | ne_binary() | list().

media_attributes(Endpoint, Attribute, #cf_call{account_db=Db}=Call) when is_binary(Endpoint) ->
    case couch_mgr:open_doc(Db, Endpoint) of
        {ok, JObj} ->
            media_attributes(JObj, Attribute, Call);
        {error, R} ->
            ?LOG("unable to load endpoint ~s for media attribute ~s: ~p", [Endpoint, Attribute, R]),
            undefined
    end;
media_attributes(Endpoint, Attribute, Call) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    OwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
    media_attributes(EndpointId, OwnerId, Attribute, Call).


media_attributes(EndpointId, OwnerId, <<"codecs">>, Call) ->
    Audio = media_attributes(EndpointId, OwnerId, <<"audio">>, Call),
    Video = media_attributes(EndpointId, OwnerId, <<"video">>, Call),
    wh_json:get_value(<<"codecs">>, Audio, []) ++ wh_json:get_value(<<"codecs">>, Video, []);
media_attributes(EndpointId, OwnerId, Attribute, #cf_call{account_id=AccountId}=Call) ->
    Ids = [begin ?LOG("looking for media attribute ~s on doc ~s", [Attribute, Id]), Id end
           || Id <- [EndpointId, OwnerId, AccountId], Id =/= undefined],
    Attributes = fetch_attributes(media_options, 3600, Call),
    case search_attributes(Attribute, Ids, Attributes) of
        undefined ->
            undefined;
        Value -> Value
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
-spec search_attributes/3 :: (Attribute, Ids, Attributes) -> cf_api_binary() when
      Attribute :: cf_api_binary(),
      Ids :: list(),
      Attributes :: proplist().
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
        Value -> Value
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_sub_key/3 :: (Attribute, Id, Attributes) -> cf_api_binary() when
      Attribute :: cf_api_binary(),
      Id :: cf_api_binary(),
      Attributes :: proplist().
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
-spec fetch_attributes/3 :: (Attribute, Expires, Call) -> proplist() when
      Attribute :: atom(),
      Expires :: non_neg_integer(),
      Call :: #cf_call{}.
fetch_attributes(Attribute, Expires, #cf_call{account_db=Db}) ->
    case wh_cache:peek({cf_attribute, Db, Attribute}) of
        {ok, Attributes} ->
            Attributes;
        {error, not_found} ->
            case couch_mgr:get_results(Db, get_view(Attribute), [{<<"stale">>, <<"ok">>}]) of
                {ok, JObjs} ->
                    Props = [{wh_json:get_value(<<"key">>, JObj), wh_json:get_value(<<"value">>, JObj)}
                                  || JObj <- JObjs],
                    wh_cache:store({cf_attribute, Db, Attribute}, Props, Expires),
                    Props;
                {error, R} ->
                    ?LOG("unable to fetch attribute ~s: ~p", [Attribute, R]),
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
