%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc Discover available tasks.
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_tasks_help).

-export([help/0, help/1, help/2]).

-export([handle_lookup_req/2]).

-include("tasks.hrl").
-include_lib("kazoo_tasks/include/task_fields.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec help() -> kz_json:object().
help() ->
    HelpJObj = tasks_bindings:fold(<<"tasks.help">>, [kz_json:new()]),
    parse_apis(HelpJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec help(kz_term:ne_binary()) -> {'ok', kz_json:object()} |
          kz_tasks:help_error().
help(Category=?NE_BINARY) ->
    HelpJObj = tasks_bindings:fold(<<"tasks.help">>, [kz_json:new(), Category]),
    JObj = parse_apis(HelpJObj),
    case kz_json:is_empty(JObj) of
        'true' -> {'error', 'unknown_category'};
        'false' -> {'ok', kz_json:get_value(Category, JObj)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec help(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_json:object()} |
          kz_tasks:help_error().
help(Category=?NE_BINARY, Action=?NE_BINARY) ->
    HelpJObj = tasks_bindings:fold(<<"tasks.help">>, [kz_json:new(), Category, Action]),
    JObj = parse_apis(HelpJObj),
    case kz_json:is_empty(JObj) of
        'true' -> {'error', 'unknown_category_action'};
        'false' -> {'ok', kz_json:get_value([Category, Action], JObj)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_lookup_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_lookup_req(JObj, _Props) ->
    'true' = kapi_tasks:lookup_req_v(JObj),
    Help =
        case get_help(JObj) of
            {'error', _R} ->
                lager:debug("lookup_req error: ~s", [_R]),
                kz_json:new();
            JOk -> JOk
        end,
    Resp = kz_json:from_list(
             [{<<"Help">>, Help}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    kapi_tasks:publish_lookup_resp(kz_api:server_id(JObj), Resp).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_help(kz_json:object()) -> kz_json:object() |
          kz_tasks:help_error().
get_help(JObj) ->
    case {kapi_tasks:category(JObj), kapi_tasks:action(JObj)} of
        {'undefined', 'undefined'} -> help();
        {Category, 'undefined'} -> lookup_result(Category, help(Category));
        {Category, Action} -> lookup_result(Category, Action, help(Category, Action))
    end.

lookup_result(_, {'error', _}=E) -> E;
lookup_result(Category, {'ok', JObj}) ->
    kz_json:from_list([{Category, JObj}]).

lookup_result(_, _, {'error', _}=E) -> E;
lookup_result(Category, Action, {'ok', JObj}) ->
    kz_json:set_value([Category, Action], JObj, kz_json:new()).

-spec parse_apis(kz_json:object()) -> kz_json:object().
parse_apis(HelpJObj) ->
    parse_apis(kz_json:to_proplist(HelpJObj), kz_json:new()).

-spec parse_apis(kz_term:proplist(), kz_json:object()) -> kz_json:object().
parse_apis([], Acc) -> Acc;
parse_apis([{Category, Actions}|HelpProps], Acc) ->
    lists:foreach(fun verify_unicity_map/1, kz_json:to_proplist(Actions)),
    NewAcc = kz_json:set_value(Category, Actions, Acc),
    parse_apis(HelpProps, NewAcc).

-spec verify_unicity_map({kz_term:ne_binary(), kz_json:object()}) -> 'ok'.
verify_unicity_map({_Action, API}) ->
    Fields0 = kz_tasks:possible_fields(API),
    Fields = [kz_term:to_lower_binary(Field) || Field <- Fields0],
    case
        length(lists:usort(Fields)) == length(Fields)
        andalso Fields == Fields0
    of
        'true' -> 'ok';
        'false' ->
            lager:error("action '~s' has duplicate or uppercase fields", [_Action])
    end.

%%% End of Module.
