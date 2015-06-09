%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_phone_number).

-include("knm.hrl").

-export([
    fetch/1
    ,save/1
]).

-export([
    to_json/1
    ,to_public_json/1
    ,from_json/1
]).

-export([
    setters/2
    ,number/1, set_number/2
    ,number_db/1 ,set_number_db/2
    ,assigned_to/1 ,set_assigned_to/2
    ,prev_assigned_to/1 ,set_prev_assigned_to/2
    ,used_by/1 ,set_used_by/2
    ,features/1 ,set_features/2
    ,feature/2 ,set_feature/3
    ,state/1 ,set_state/2
    ,reserve_history/1 ,set_reserve_history/2
    ,ported_in/1 ,set_ported_in/2
    ,module_name/1 ,set_module_name/2
    ,region/1 ,set_region/2
]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary()) -> {'ok', number()} | {'error', _}.
fetch(Num) ->
    NormalizedNum = knm_number_converter:normalize(Num),
    NumberDb = knm_number_converter:to_db(NormalizedNum),
    case couch_mgr:open_doc(NumberDb, NormalizedNum) of
        {'error', _R}=E ->
            lager:error("failed to open ~s in ~s", [NormalizedNum, NumberDb]),
            E;
        {'ok', JObj} ->
            {'ok', from_json(JObj)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(number()) -> {'ok', number()} | {'error', _}.
save(Number) ->
    NumberDb = number_db(Number),
    JObj = to_json(Number),
    case couch_mgr:ensure_saved(NumberDb, JObj) of
        {'error', _R}=E ->
            lager:error("failed to save ~s in ~s", [number(Number), NumberDb]),
            E;
        {'ok', _} ->
            hangle_assignment(Number)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_public_json(number()) -> wh_json:object().
to_public_json(Number) ->
    wh_json:foldl(
        fun
            (<<"_id">>, Value, Acc) ->
                wh_json:set_value(<<"id">>, Value, Acc);
            (<<"pvt_", Key/binary>>, Value, Acc) ->
                wh_json:set_value(Key, Value, Acc);
            (Key, Value, Acc) ->
                wh_json:set_value(Key, Value, Acc)
        end
        ,wh_json:new()
        ,to_json(Number)
    ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_json(number()) -> wh_json:object().
to_json(#number{doc=JObj}=N) ->
    Now = wh_util:current_tstamp(),
    wh_json:from_list(
        props:filter_undefined([
            {<<"_id">>, number(N)}
            ,{?PVT_DB_NAME, number_db(N)}
            ,{?PVT_ASSIGNED_TO, assigned_to(N)}
            ,{?PVT_PREVIOUSLY_ASSIGNED_TO, prev_assigned_to(N)}
            ,{?PVT_USED_BY, used_by(N)}
            ,{?PVT_FEATURES, features(N)}
            ,{?PVT_STATE, state(N)}
            ,{?PVT_RESERVE_HISTORY, reserve_history(N)}
            ,{?PVT_PORTED_IN, ported_in(N)}
            ,{?PVT_MODULE_NAME, module_name(N)}
            ,{?PVT_REGION, region(N)}
            ,{?PVT_MODIFIED, Now}
            ,{?PVT_CREATED, wh_json:get_value(<<"pvt_created">>, JObj, Now)}
            ,{?PVT_TYPE, <<"number">>}
        ])
    ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_json(wh_json:object()) -> number().
from_json(JObj) ->
    #number{
        number=wh_json:get_value(<<"_id">>, JObj)
        ,number_db=wh_json:get_value(?PVT_DB_NAME, JObj)
        ,assigned_to=wh_json:get_value(?PVT_ASSIGNED_TO, JObj)
        ,prev_assigned_to=wh_json:get_value(?PVT_PREVIOUSLY_ASSIGNED_TO, JObj)
        ,used_by=wh_json:get_value(?PVT_USED_BY, JObj)
        ,features=wh_json:get_value(?PVT_FEATURES, JObj)
        ,state=wh_json:get_value(?PVT_STATE, JObj)
        ,reserve_history=wh_json:get_value(?PVT_RESERVE_HISTORY, JObj)
        ,ported_in=wh_json:get_value(?PVT_PORTED_IN, JObj)
        ,module_name=wh_json:get_value(?PVT_MODULE_NAME, JObj)
        ,region=wh_json:get_value(?PVT_REGION, JObj)
        ,doc=JObj
    }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec setters(number(), wh_proplist()) -> number().
setters(Number, Props) ->
    lists:foldl(
        fun({Fun, Value}, Acc) ->
            Fun(Acc, Value)
        end
        ,Number
        ,Props
    ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec number(number()) -> ne_binary().
number(Number) ->
    Number#number.number.

-spec set_number(number(), ne_binary()) -> number().
set_number(N, Number) ->
    N#number{number=Number}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec number_db(number()) -> ne_binary().
number_db(Number) ->
    Number#number.number_db.

-spec set_number_db(number(), ne_binary()) -> number().
set_number_db(N, NumberDb) ->
    N#number{number_db=NumberDb}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assigned_to(number()) -> ne_binary().
assigned_to(Number) ->
    Number#number.assigned_to.

-spec set_assigned_to(number(), ne_binary()) -> number().
set_assigned_to(N, AccountId) ->
    N#number{assigned_to=AccountId}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prev_assigned_to(number()) -> ne_binary().
prev_assigned_to(Number) ->
    Number#number.prev_assigned_to.

-spec set_prev_assigned_to(number(), ne_binary()) -> number().
set_prev_assigned_to(N, AccountId) ->
    N#number{prev_assigned_to=AccountId}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec used_by(number()) -> ne_binary().
used_by(Number) ->
    Number#number.used_by.

-spec set_used_by(number(), ne_binary()) -> number().
set_used_by(N, UsedBy) ->
    N#number{used_by=UsedBy}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec features(number()) -> ne_binary().
features(Number) ->
    Number#number.features.

-spec set_features(number(), ne_binary()) -> number().
set_features(N, Features) ->
    N#number{features=Features}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec feature(number(), ne_binary()) -> ne_binary().
feature(Number, Feature) ->
    wh_json:get_value(Feature, Number#number.features).

-spec set_feature(number(), ne_binary(), _) -> number().
set_feature(N, Feature, Data) ->
    Features = features(N),
    N#number{features=wh_json:set_value(Feature, Data, Features)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec state(number()) -> ne_binary().
state(Number) ->
    Number#number.state.

-spec set_state(number(), ne_binary()) -> number().
set_state(N, State) ->
    N#number{state=State}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec reserve_history(number()) -> ne_binaries().
reserve_history(Number) ->
    Number#number.reserve_history.

-spec set_reserve_history(number(), ne_binaries()) -> number().
set_reserve_history(N, History) ->
    N#number{reserve_history=History}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec ported_in(number()) -> boolean().
ported_in(Number) ->
    Number#number.ported_in.

-spec set_ported_in(number(), boolean()) -> number().
set_ported_in(N, Ported) ->
    N#number{ported_in=Ported}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec module_name(number()) -> ne_binary().
module_name(Number) ->
    Number#number.module_name.

-spec set_module_name(number(), ne_binary()) -> number().
set_module_name(N, Name) ->
    N#number{module_name=Name}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec region(number()) -> ne_binary().
region(Number) ->
    Number#number.region.

-spec set_region(number(), ne_binary()) -> number().
set_region(N, Region) ->
    N#number{region=Region}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec hangle_assignment(number()) -> {'ok', number()} | {'error', _}.
hangle_assignment(Number) ->
    lager:debug("handling assignment for ~s", [number(Number)]),
    case maybe_assign(Number) of
        {'error', _R}=E ->E;
        {'ok', _} ->
            maybe_unassign(Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_assign(number()) -> {'ok', number()} | {'error', _}.
maybe_assign(Number) ->
    AssignedTo = assigned_to(Number),
    case wh_util:is_empty(AssignedTo) of
        'true' ->
            lager:debug("assign_to is is empty for ~s, ignoring", [number(Number)]),
            {'ok', Number};
        'false' -> assign(Number, AssignedTo)
    end.

-spec assign(number(), ne_binary()) -> {'ok', number()} | {'error', _}.
assign(Number, AssignedTo) ->
    AccountDb = wh_util:format_account_id(AssignedTo, 'encoded'),
    case couch_mgr:ensure_saved(AccountDb, to_json(Number)) of
        {'error', _R}=E ->
            lager:error("failed to assign number ~s to ~s", [number(Number), AccountDb]),
            E;
        {'ok', _} ->
            lager:debug("assigned number ~s to ~s", [number(Number), AccountDb]),
            {'ok', Number}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_unassign(number()) -> {'ok', number()} | {'error', _}.
maybe_unassign(Number) ->
    PrevAssignedTo = prev_assigned_to(Number),
    case wh_util:is_empty(PrevAssignedTo) of
        'true' ->
            lager:debug("prev_assign_to is is empty for ~s, ignoring", [number(Number)]),
            {'ok', Number};
        'false' -> unassign(Number, PrevAssignedTo)
    end.

-spec unassign(number(), ne_binary()) -> {'ok', number()} | {'error', _}.
unassign(Number, PrevAssignedTo) ->
    AccountDb = wh_util:format_account_id(PrevAssignedTo, 'encoded'),
    case couch_mgr:del_doc(AccountDb, to_json(Number)) of
        {'error', _R}=E ->
            lager:error("failed to unassign number ~s from ~s", [number(Number), AccountDb]),
            E;
        {'ok', _} ->
            lager:debug("unassigned number ~s from ~s", [number(Number), AccountDb]),
            {'ok', Number}
    end.


