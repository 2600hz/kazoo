%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_doc).
-author('Benoît Chesneau <benoitc@e-engura.org>').
-include_lib("couchbeam/include/couchbeam.hrl").

-export([set_value/3, get_value/2, get_value/3,
         delete_value/2, extend/2, extend/3]).
-export([get_id/1, get_rev/1, get_idrev/1, is_saved/1]).

%% @spec get_id(Doc::json_obj()) -> binary()
%% @doc get document id.
get_id(Doc) ->
    get_value(<<"_id">>, Doc).

%% @spec get_rev(Doc::json_obj()) -> binary()
%% @doc get document revision.
get_rev(Doc) ->
    get_value(<<"_rev">>, Doc).

%% @spec get_idrev(Doc::json_obj()) -> {DocId, DocRev}
%% @doc get  a tuple containing docucment id and revision.
get_idrev(Doc) ->
    DocId = get_value(<<"_id">>, Doc),
    DocRev = get_value(<<"_rev">>, Doc),
    {DocId, DocRev}.

%% @spec is_saved(Doc::json_obj()) -> boolean()
%% @doc If document have been saved (revision is defined) return true,
%% else, return false.
is_saved(Doc) ->
    case get_value(<<"_rev">>, Doc) of
        undefined -> false;
        _ -> true
    end.

%% @spec set_value(Key::key_val(), Value::term(), JsonObj::json_obj()) -> term()
%% @doc set a value for a key in jsonobj. If key exists it will be updated.
set_value(Key, Value, JsonObj) when is_list(Key)->
    set_value(list_to_binary(Key), Value, JsonObj);
set_value(Key, Value, JsonObj) when is_binary(Key) ->
    {Props} = JsonObj,
    case proplists:is_defined(Key, Props) of
        true -> set_value1(Props, Key, Value, []);
        false-> {lists:reverse([{Key, Value}|lists:reverse(Props)])}
    end.

%% @spec get_value(Key::key_val(), JsonObj::json_obj()) -> term()
%% @type key_val() = lis() | binary()
%% @doc Returns the value of a simple key/value property in json object
%% Equivalent to get_value(Key, JsonObj, undefined).
get_value(Key, JsonObj) ->
    get_value(Key, JsonObj, undefined).


%% @spec get_value(Key::lis() | binary(), JsonObj::json_obj(), Default::term()) -> term()
%% @doc Returns the value of a simple key/value property in json object
%% function from erlang_couchdb
get_value(Key, JsonObj, Default) when is_list(Key) ->
    get_value(list_to_binary(Key), JsonObj, Default);
get_value(Key, JsonObj, Default) when is_binary(Key) ->
    {Props} = JsonObj,
    proplists:get_value(Key, Props, Default).

%% @spec delete_value(Key::key_val(), JsonObj::json_obj()) -> json_obj()
%% @doc Deletes all entries associated with Key in json object.
delete_value(Key, JsonObj) when is_list(Key) ->
    delete_value(list_to_binary(Key), JsonObj);
delete_value(Key, JsonObj) when is_binary(Key) ->
    {Props} = JsonObj,
    Props1 = proplists:delete(Key, Props),
    {Props1}.

%% @spec extend(Key::binary(), Value::json_term(), JsonObj::json_obj()) -> json_obj()
%% @doc extend a jsonobject by key, value
extend(Key, Value, JsonObj) ->
    extend({Key, Value}, JsonObj).

%% @spec extend(Prop::property(), JsonObj::json_obj()) -> json_obj()
%% @type property() = json_obj() | tuple()
%% @doc extend a jsonobject by a property or list of property
extend([], JsonObj) ->
    JsonObj;
extend([Prop|R], JsonObj)->
    NewObj = extend(Prop, JsonObj),
    extend(R, NewObj);
extend({Key, Value}, JsonObj) ->
    set_value(Key, Value, JsonObj).

%% @private
set_value1([], _Key, _Value, Acc) ->
    {lists:reverse(Acc)};
set_value1([{K, V}|T], Key, Value, Acc) ->
    Acc1 = if
        K =:= Key ->
            [{Key, Value}|Acc];
        true ->
            [{K, V}|Acc]
        end,
    set_value1(T, Key, Value, Acc1).

