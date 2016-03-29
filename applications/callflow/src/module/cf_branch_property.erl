%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz INC
%%% @doc
%%%
%%% "data":{
%%%   "action":"branch", % the only one action supported currently
%%%   "scope": "[device|user|account]",
%%%   "property": "whatever_field_you_want"
%%% }
%%%
%%% Branch to child whose name pointed in callflow's data section
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_branch_property).

-include("../callflow.hrl").

%% API
-export([handle/2]).

-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    ChildName = maybe_lookup_child_name(Data, Call, wh_json:get_binary_value(<<"property">>, Data)),
    branch_to_child(ChildName, Call).

-spec maybe_lookup_child_name(wh_json:object(), whapps_call:call(), api_binary()) -> api_binary().
maybe_lookup_child_name(_Data, _Call, 'undefined') ->
    lager:debug("value for \"property\" is unset"),
    'undefined';
maybe_lookup_child_name(Data, Call, Property) ->
    choose_lookup_variant(wh_json:get_binary_value(<<"scope">>, Data), Call, Property).

-spec branch_to_child(api_binary(), whapps_call:call()) -> 'ok'.
branch_to_child('undefined', Call) -> cf_exe:continue(Call);
branch_to_child(ChildName, Call) -> cf_exe:continue(ChildName, Call).

-spec choose_lookup_variant(api_binary(), whapps_call:call(), ne_binary()) -> api_binary().
choose_lookup_variant(<<"device">> = Scope, Call, Field) -> targeted_lookup(Scope, Call, Field);
choose_lookup_variant(<<"user">> = Scope, Call, Field) -> targeted_lookup(Scope, Call, Field);
choose_lookup_variant(<<"account">> = Scope, Call, Field) -> targeted_lookup(Scope, Call, Field);
choose_lookup_variant(_, Call, Field) ->
    case cf_endpoint:get(Call) of
        {'ok', JObj} -> wh_json:get_value(Field, JObj);
        _Else ->
            lager:debug("can not lookup endpoint for caller"),
            'undefined'
    end.

-spec targeted_lookup(ne_binary(), whapps_call:call(), ne_binary()) -> api_binary().
targeted_lookup(<<"account">>, Call, Field) -> lookup_childname(Call, whapps_call:account_id(Call), Field);
targeted_lookup(Scope, Call, Field) ->
    DocId = check_auth_type(Scope, Call, whapps_call:authorizing_type(Call)),
    lookup_childname(Call, DocId, Field).

check_auth_type(Scope, Call, Scope) -> whapps_call:authorizing_id(Call);
check_auth_type(_Scope, Call, <<"device">>) -> lookup_device_owner(Call, whapps_call:authorizing_id(Call));
check_auth_type(_Scope, _Call, <<"user">>) -> 'undefined';
check_auth_type(_Scope, _Call, _AuthType) ->
      lager:debug("unsupported authorizing type: ~s", [_AuthType]),
      'undefined'.

-spec lookup_device_owner(whapps_call:call(), ne_binary()) -> api_binary().
lookup_device_owner(Call, DeviceId) ->
    case kz_datamgr:open_cache_doc(whapps_call:account_db(Call), DeviceId) of
        {'ok', JObj} -> kz_device:owner_id(JObj);
        _Else ->
            lager:debug("can not open doc ~s/~s", [whapps_call:account_db(Call), DeviceId]),
            'undefined'
    end.

-spec lookup_childname(whapps_call:call(), ne_binary(), ne_binary()) -> api_binary().
lookup_childname(_Call, 'undefined', _Field) ->
    lager:debug("can not find document for current scope"),
    'undefined';
lookup_childname(Call, DocId, Field) ->
    case kz_datamgr:open_cache_doc(whapps_call:account_db(Call), DocId) of
        {'ok', JObj} -> wh_json:get_binary_value(Field, wh_json:public_fields(JObj));
        _Else ->
            lager:debug("can not open doc ~s/~s", [whapps_call:account_db(Call), DocId]),
            'undefined'
    end.
