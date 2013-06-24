%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, VoIP INC
%%% @doc
%%% Utilities for manipulating Kazoo/Whistle documents
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_doc).

-include("wh_couch.hrl").

-include_lib("whistle/include/wh_types.hrl"). % get the whistle types

-export([update_pvt_parameters/2, update_pvt_parameters/3
         ,public_fields/1
         ,private_fields/1
        ]).
-export([update_pvt_modified/1]).

-define(PVT_FUNS, [fun add_pvt_vsn/3
                   ,fun add_pvt_account_id/3
                   ,fun add_pvt_account_db/3
                   ,fun add_pvt_created/3
                   ,fun add_pvt_modified/3
                   ,fun add_pvt_type/3
                  ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is used to update the private timestamps, and db
%% parameters on all crossbar documents
%% @end
%%--------------------------------------------------------------------
-spec update_pvt_parameters(wh_json:object(), ne_binary()) ->
                                   wh_json:object().
-spec update_pvt_parameters(wh_json:object(), ne_binary(), wh_proplist()) ->
                                   wh_json:object().
update_pvt_parameters(JObj0, DBName) ->
    update_pvt_parameters(JObj0, DBName, []).
update_pvt_parameters(JObj0, DBName, Options) ->
    Opts = case props:get_value('now', Options) of
               'undefined' -> [{'now', wh_util:current_tstamp()} | Options];
               _ -> Options
           end,
    lists:foldl(fun(Fun, JObj) -> Fun(JObj, DBName, Opts) end, JObj0, ?PVT_FUNS).

add_pvt_vsn(JObj, _, Options) ->
    case props:get_value('crossbar_doc_vsn', Options) of
        'undefined' -> JObj;
        Vsn -> wh_json:set_value(<<"pvt_vsn">>, Vsn, JObj)
    end.

add_pvt_account_db(JObj, DBName, Opts) ->
    case props:get_value('account_db', Opts) of
        'undefined' -> wh_json:set_value(<<"pvt_account_db">>, DBName, JObj);
        Db -> wh_json:set_value(<<"pvt_account_db">>, Db, JObj)
    end.

add_pvt_account_id(JObj, DBName, Opts) ->
    case props:get_value('account_id', Opts) of
        'undefined' -> wh_json:set_value(<<"pvt_account_id">>, wh_util:format_account_id(DBName, 'raw'), JObj);
        Id -> wh_json:set_value(<<"pvt_account_id">>, Id, JObj)
    end.

add_pvt_type(JObj, _, Options) ->
    case props:get_value('type', Options) of
        'undefined' -> JObj;
        Type -> wh_json:set_value(<<"pvt_type">>, Type, JObj)
    end.

add_pvt_created(JObj, _, Opts) ->
    case wh_json:get_value(<<"_rev">>, JObj) of
        'undefined' -> wh_json:set_value(<<"pvt_created">>, props:get_value('now', Opts), JObj);
        _ -> JObj
    end.

update_pvt_modified(JObj) ->
    add_pvt_modified(JObj, 'undefined', [{'now', wh_util:current_tstamp()}]).
add_pvt_modified(JObj, _, Opts) ->
    wh_json:set_value(<<"pvt_modified">>, props:get_value('now', Opts), JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any private fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec public_fields(wh_json:object() | wh_json:objects()) ->
                           wh_json:object() | wh_json:objects().
public_fields(JObjs) when is_list(JObjs) -> [public_fields(J) || J <- JObjs];
public_fields(JObj) -> wh_json:filter(fun({K, _}) -> not is_private_key(K) end, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return a boolean, true if the provided key is
%% considered private; otherwise false
%% @end
%%--------------------------------------------------------------------
-spec is_private_key(binary()) -> boolean().
is_private_key(<<"_", _/binary>>) -> 'true';
is_private_key(<<"pvt_", _/binary>>) -> 'true';
is_private_key(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any public fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec private_fields(wh_json:object() | wh_json:objects()) ->
                            wh_json:object() | wh_json:objects().
private_fields(JObjs) when is_list(JObjs) -> [public_fields(JObj) || JObj <- JObjs];
private_fields(JObj) -> wh_json:filter(fun({K, _}) -> is_private_key(K) end, JObj).
