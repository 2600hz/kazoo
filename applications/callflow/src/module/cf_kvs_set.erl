%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-module(cf_kvs_set).

-export([handle/2]).
-export([get_kv/2, get_kv/3]).
-export([kvs_to_props/1]).

-include("../callflow.hrl").

-define(KVS_DB, <<"kvs_collections">>).
-define(COLLECTION_KVS, <<"Custom-KVS">>).
-define(COLLECTION_MODE, <<"KVS-Mode">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Call2 = set_kvs(Data, Call),
    cf_exe:set_call(Call2),
    cf_exe:continue(Call2).

get_kv(Key, Call) ->
    get_kv(?COLLECTION_KVS, Key, Call).

get_kv(Collection, Key, Call) ->
    wh_json:get_value(Key, get_collection(Collection, Call)).
    
set_kvs(Data, Call) ->
    Call2 = set_kvs_mode(wh_json:get_value(<<"kvs_mode">>, Data, 'undefined'), Call),
    Data2 = wh_json:delete_key(<<"kvs_mode">>, Data),
    Keys = wh_json:get_keys(Data2),
    fold_set_kvs(Call2, Data2, Keys).

fold_set_kvs(Call, _, []) ->
    Call;
fold_set_kvs(Call, Data, [Key|Keys]) ->
    Call2 = set_kvs_collection(Key, evaluate(wh_json:get_value(Key, Data), Call), Call),
    fold_set_kvs(Call2, Data, Keys).
    
set_kvs_mode('undefined', Call) -> set_collection(?COLLECTION_MODE, <<"kvs_mode">>, 'undefined', Call);
set_kvs_mode(Mode, Call) -> set_collection(?COLLECTION_MODE, <<"kvs_mode">>, Mode, Call).

evaluate(<<"$", Key/binary>>, Call) ->
    get_kv(Key, Call);
evaluate(Value, Call) ->
    case wh_json:is_json_object(Value) of
        'true' -> evaluate_ui(wh_json:get_keys(Value), Value, Call);
        'false' -> Value
    end.

evaluate_ui([<<"type">>, <<"value">>], Value, _Call) ->
    %evaluate(wh_json:get_value(<<"value">>, Value), Call);
    Value;
evaluate_ui(_, Value, _) ->
    Value.

-spec get_kvs_collection(whapps_call:call()) -> api_binary().
get_kvs_collection(Call) ->
    get_collection(?COLLECTION_KVS, Call).

get_collection(Collection, Call) ->
    wh_json:get_value(Collection, whapps_call:kvs_fetch(?KVS_DB, wh_json:new(), Call), wh_json:new()).

-spec set_kvs_collection(ne_binary(), ne_binary(), whapps_call:call()) -> whapps_call:call().
set_kvs_collection(Key, Value, Call) ->
    set_collection(?COLLECTION_KVS, Key, Value, Call).
    
set_collection(Collection, Key, Value, Call) ->
    Collections = whapps_call:kvs_fetch(?KVS_DB, wh_json:new(), Call),
    OldCollection = get_collection(Collection, Call),
    NewCollection = wh_json:set_value(Key, Value, OldCollection),
    whapps_call:kvs_store(
    	?KVS_DB,
        wh_json:set_value(Collection, NewCollection, Collections),
        Call
    ).
    
kvs_to_props(Call) ->
    case wh_json:get_value(<<"kvs_mode">>, get_collection(?COLLECTION_MODE, Call), 'undefined') of
        <<"json">> ->
            Collection = get_kvs_collection(Call),
            Keys = wh_json:get_keys(Collection),
            [{Key, wh_json:encode(wh_json:get_value(Key, Collection))} || Key <- Keys];
        _ ->
            [{<<"Custom-KVS">>, get_kvs_collection(Call)}]
    end.
