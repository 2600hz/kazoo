%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, Edouard Swiac
%%% @doc
%%% Utilities fonction for whistle documents (crossbar docs without a context)
%%% @end
%%% Created : 14 July 2011 by Edouard Swiac <edouard@2600hz.org>
%%%-------------------------------------------------------------------
-module(wh_doc).

-include("wh_couch.hrl").

-include_lib("whistle/include/wh_types.hrl"). % get the whistle types

-export([update_pvt_parameters/2, update_pvt_parameters/3, public_fields/1, private_fields/1]).
-export([update_pvt_modified/1]).
-export([jobj_to_list/1]).

-define(PVT_FUNS, [fun add_pvt_vsn/3, fun add_pvt_account_id/3, fun add_pvt_account_db/3
		   ,fun add_pvt_created/3, fun add_pvt_modified/3, fun add_pvt_type/3
		  ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is used to update the private timestamps, and db
%% parameters on all crossbar documents
%% @end
%%--------------------------------------------------------------------
-spec(update_pvt_parameters/2 :: (JObj0 :: json_object(), DBName :: binary()) -> json_object()).
update_pvt_parameters(JObj0, DBName) ->
    update_pvt_parameters(JObj0, DBName, []).

-spec(update_pvt_parameters/3 :: (JObj0 :: json_object(), DBName :: binary(), Options :: proplist()) -> json_object()).
update_pvt_parameters(JObj0, DBName, Options) ->
    lists:foldl(fun(Fun, JObj) -> Fun(JObj, DBName, Options) end, JObj0, ?PVT_FUNS).

add_pvt_vsn(JObj, _, Options) ->
    case Vsn = proplists:get_value(crossbar_doc_vsn, Options, undefined) of
        undefined -> JObj;
        _ -> wh_json:set_value(<<"pvt_vsn">>, Vsn, JObj)
    end.

add_pvt_account_db(JObj, DBName, _) ->
    wh_json:set_value(<<"pvt_account_db">>, DBName, JObj).

add_pvt_account_id(JObj, DBName, _) ->
    wh_json:set_value(<<"pvt_account_id">>, whapps_util:get_db_name(DBName, raw), JObj).

add_pvt_type(JObj, _, Options) ->
    case Type = proplists:get_value(type, Options, undefined) of
        undefined -> JObj;
        _ -> wh_json:set_value(<<"pvt_type">>, Type, JObj)
    end.

add_pvt_created(JObj, _, _) ->
    case wh_json:get_value(<<"_rev">>, JObj) of
        undefined ->
            Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            wh_json:set_value(<<"pvt_created">>, Timestamp, JObj);
        _ ->
            JObj
    end.

update_pvt_modified(JObj) ->
    add_pvt_modified(JObj,null,null).
add_pvt_modified(JObj, _, _) ->
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    wh_json:set_value(<<"pvt_modified">>, Timestamp, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any private fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec(public_fields/1 :: (Json :: json_object()|json_objects()) -> json_object()|json_objects()).
public_fields([{struct, _}|_]=Json)->
    lists:map(fun public_fields/1, Json);
public_fields({struct, Prop}) ->
    {struct, [ Tuple || {K, _}=Tuple <- Prop, not is_private_key(K)]};
public_fields(Json) ->
    ?LOG_SYS("Unhandled JSON format in public_fields: ~p", [Json]),
    Json.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return a boolean, true if the provided key is
%% considered private; otherwise false
%% @end
%%--------------------------------------------------------------------
-spec(is_private_key/1 :: (Key :: binary()) -> boolean()).
is_private_key(<<"_", _/binary>>) -> true;
is_private_key(<<"pvt_", _/binary>>) -> true;
is_private_key(_) -> false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any public fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec(private_fields/1 :: (Json :: json_object()|json_objects()) -> json_object()|json_objects()).
private_fields([{struct, _}|_]=Json)->
    lists:map(fun public_fields/1, Json);
private_fields({struct, Prop}) ->
    {struct, [ Tuple || {K,_}=Tuple <- Prop, is_private_key(K)]};
private_fields(Json) ->
    ?LOG_SYS("Unhandled JSON format in private fields: ~p", [Json]),
    Json.

jobj_to_list({struct, List}=_) ->
    List.
