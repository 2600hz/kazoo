%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz INC
%%% @doc
%%%
%%% "data":{
%%%   "action":"branch", % the only one action supported currently
%%%   "scope": "[device|user|account]",
%%%   "field": "whatever_field_you_want"
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
    AuthId = whapps_call:authorizing_id(Call),
    Scope = wh_json:get_binary_value(<<"scope">>, Data, <<"device">>),
    PointedField = wh_json:get_binary_value(<<"field">>, Data),
    GetDocIdFun = case {Scope, whapps_call:authorizing_type(Call)} of
                      {<<"account">>, _} -> fun() -> whapps_call:account_id(Call) end;
                      {Same, Same} -> fun() -> AuthId end;
                      {_, <<"device">>} -> fun() -> lookup_device_owner(Call, AuthId) end;
                      {_, <<"user">>} -> fun() -> 'undefined' end;
                      _Else ->
                          lager:debug("unsupported authorizing type: ~s", [_Else]),
                          fun() -> 'undefined' end
                  end,
    DocId = case is_binary(PointedField) of
                'true' -> GetDocIdFun();
                'false' ->
                    lager:debug("property \"field\" unset"),
                    'undefined'
            end,
    ChildName = case is_binary(DocId) of
                    'true' -> lookup_childname(Call, DocId, PointedField);
                    'false' ->
                        lager:debug("can not find document for current scope"),
                        'undefined'
                end,
    case is_binary(ChildName) of
        'true' -> cf_exe:continue(ChildName, Call);
        'false' -> cf_exe:continue(Call)
    end.

-spec lookup_device_owner(whapps_call:call(), ne_binary()) -> api_binary().
lookup_device_owner(Call, DeviceId) ->
    case kz_datamgr:open_cache_doc(whapps_call:account_db(Call), DeviceId) of
        {'ok', JObj} -> kz_device:owner_id(JObj);
        _Else ->
            lager:debug("can not open doc ~s/~s", [whapps_call:account_db(Call), DeviceId]),
            'undefined'
    end.

-spec lookup_childname(whapps_call:call(), ne_binary(), ne_binary()) -> api_binary().
lookup_childname(Call, DocId, Field) ->
    case kz_datamgr:open_cache_doc(whapps_call:account_db(Call), DocId) of
        {'ok', JObj} -> wh_json:get_binary_value(Field, wh_json:public_fields(JObj));
        _Else ->
            lager:debug("can not open doc ~s/~s", [whapps_call:account_db(Call), DocId]),
            'undefined'
    end.
