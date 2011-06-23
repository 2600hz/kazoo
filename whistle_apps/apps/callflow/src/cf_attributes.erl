%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 7 April 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_attributes).

-include("callflow.hrl").

-export([call_forward/3]).
-export([caller_id/4]).
-export([callee_id/4]).
-export([caller_id_options/4]).
-export([owner_id/2]).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec(call_forward/3 :: (DeviceId :: cf_api_binary(), OwnerId :: cf_api_binary(), Call :: #cf_call{}) -> undefined | json_object()).
call_forward(DeviceId, OwnerId, Call) ->
    Attributes = fetch_attributes(call_forward, 60, Call),
    case props:get_value(DeviceId, Attributes) of
        undefined -> props:get_value(OwnerId, Attributes);
        Property -> Property
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec(caller_id/4 :: (CIDType :: cf_api_binary(), DeviceId :: cf_api_binary(), OwnerId :: cf_api_binary(), Call :: #cf_call{})
                     -> tuple(cf_api_binary(), cf_api_binary())).
caller_id(CIDType, DeviceId, OwnerId, #cf_call{account_id=AccountId, cid_number=Num}=Call) ->
    Ids = [begin ?LOG("looking for caller id type ~s on doc ~s", [CIDType, Id]), Id end
           || Id <- [DeviceId, OwnerId, AccountId], Id =/= undefined],
    Attributes = fetch_attributes(caller_id, 3600, Call),
    CID = case search_attributes(CIDType, Ids, Attributes) of
              undefined ->
                  search_attributes(<<"default">>, [AccountId], Attributes);
              Property -> Property
          end,
    {wh_json:get_value(<<"number">>, CID, Num), wh_json:get_value(<<"name">>, CID, <<>>)}.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec(callee_id/4 :: (CIDType :: cf_api_binary(), DeviceId :: cf_api_binary(), OwnerId :: cf_api_binary(), Call :: #cf_call{})
                     -> tuple(cf_api_binary(), cf_api_binary())).
callee_id(CIDType, DeviceId, OwnerId, #cf_call{account_id=AccountId, request_user=Num}=Call) ->
    Ids = [begin ?LOG("looking for callee id type ~s on doc ~s", [CIDType, Id]), Id end
           || Id <- [DeviceId, OwnerId, AccountId], Id =/= undefined],
    Attributes = fetch_attributes(caller_id, 3600, Call),
    CID = case search_attributes(CIDType, Ids, Attributes) of
              undefined ->
                  search_attributes(<<"default">>, [AccountId], Attributes);
              Property -> Property
          end,
    {wh_json:get_value(<<"number">>, CID, Num), wh_json:get_value(<<"name">>, CID, Num)}.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec(caller_id_options/4 :: (Option :: binary(), DeviceId :: cf_api_binary(), OwnerId :: cf_api_binary(), #cf_call{})
                             -> cf_api_binary()).
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
owner_id(undefined, _) ->
    undefined;
owner_id(ObjectId, #cf_call{account_db=Db})->
    Id = whistle_util:to_binary(ObjectId),
    case couch_mgr:get_results(Db, {<<"cf_attributes">>, <<"owner">>}, [{<<"key">>, Id}]) of
        {ok, []} ->
            undefined;
        {ok, JObj} ->
            wh_json:get_value(<<"value">>, hd(JObj));
        {error, _} ->
            undefined
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec(search_attributes/3 :: (Key :: cf_api_binary(), Ids :: list(), Attributes :: proplist())
                             -> cf_api_binary()).
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
-spec(fetch_sub_key/3 :: (Key :: cf_api_binary(), Id :: cf_api_binary(), Attributes :: proplist())
                         -> cf_api_binary()).
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
-spec(fetch_attributes/3 :: (Attribute :: atom(), Expires :: non_neg_integer(), Call :: #cf_call{}) -> list()).
fetch_attributes(Attribute, Expires, #cf_call{account_db=Db}) ->
    case wh_cache:fetch({cf_attribute, Db, Attribute}) of
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
-spec(get_view/1 :: (Attribute :: atom()) -> tuple(binary(), binary())).
get_view(Attribute) ->
    {<<"cf_attributes">>, whistle_util:to_binary(Attribute)}.
