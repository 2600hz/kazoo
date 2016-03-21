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
    Scope = wh_json:get_binary_value(<<"scope">>, Data),
    PointedField = wh_json:get_binary_value(<<"property">>, Data),
    ChildName = case is_binary(PointedField) of
                    'true' -> lookup_child_name(Scope, Call, PointedField);
                    'false' ->
                        lager:debug("value for \"property\" is unset"),
                        'undefined'
                end,
    case is_binary(ChildName) of
        'true' -> cf_exe:continue(ChildName, Call);
        'false' -> cf_exe:continue(Call)
    end.

-spec lookup_child_name(api_binary(), whapps_call:call(), ne_binary()) -> api_binary().
lookup_child_name(Scope, Call, Field) ->
    case Scope of
        <<"device">> -> targeted_lookup(Scope, Call, Field);
        <<"user">> -> targeted_lookup(Scope, Call, Field);
        <<"account">> -> targeted_lookup(Scope, Call, Field);
        _ -> automatic_lookup(Call, Field)
    end.

-spec automatic_lookup(whapps_call:call(), ne_binary()) -> api_binary().
automatic_lookup(Call, Field) ->
    case cf_endpoint:get(Call) of
        {'ok', JObj} -> wh_json:get_value(Field, JObj);
        _Else ->
            lager:debug("can not lookup endpoint for caller"),
            'undefined'
    end.

-spec targeted_lookup(ne_binary(), whapps_call:call(), ne_binary()) -> api_binary().
targeted_lookup(Scope, Call, Field) ->
    AuthId = whapps_call:authorizing_id(Call),
    DocId = case {Scope, whapps_call:authorizing_type(Call)} of
                      {<<"account">>, _} -> whapps_call:account_id(Call);
                      {Same, Same} -> AuthId;
                      {_, <<"device">>} -> lookup_device_owner(Call, AuthId);
                      {_, <<"user">>} -> 'undefined';
                      _Else ->
                          lager:debug("unsupported authorizing type: ~s", [_Else]),
                          'undefined'
                  end,
    case is_binary(DocId) of
        'true' -> lookup_childname(Call, DocId, Field);
        'false' ->
            lager:debug("can not find document for current scope(~s)", [Scope]),
            'undefined'
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
