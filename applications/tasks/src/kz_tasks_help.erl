%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%% Discover available tasks.
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_tasks_help).

-export([help/0, help/1, help/2]).

-export([handle_lookup_req/2]).

-include("tasks.hrl").
-include_lib("kazoo_tasks/include/task_fields.hrl").
-include_lib("kazoo/src/kz_json.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec help() -> kz_json:object().
help() ->
    JObjs = tasks_bindings:map(<<"tasks.help.*">>, []),
    parse_apis(JObjs).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec help(ne_binary()) -> {'ok', kz_json:object()} |
                           kz_tasks:help_error().
help(Category=?NE_BINARY) ->
    JObjs = tasks_bindings:map(<<"tasks.help.", Category/binary>>, []),
    JObj = parse_apis(JObjs),
    case kz_json:is_empty(JObj) of
        'true' -> {'error', 'unknown_category'};
        'false' -> {'ok', kz_json:get_value(Category, JObj)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec help(ne_binary(), ne_binary()) -> {'ok', kz_json:object()} |
                                        kz_tasks:help_error().
help(Category=?NE_BINARY, Action=?NE_BINARY) ->
    case help(Category) of
        {'error', _}=Error -> Error;
        {'ok', JObj} ->
            case kz_json:get_value(Action, JObj) of
                'undefined' -> {'error', 'unknown_action'};
                J -> {'ok', J}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_lookup_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_lookup_req(JObj, _Props) ->
    'true' = kapi_tasks:lookup_req_v(JObj),
    Help =
        case get_help(JObj) of
            {'error', _R} ->
                lager:debug("lookup_req error: ~s", [_R]),
                kz_json:new();
            {'ok', JOk} -> JOk;
            JOk -> JOk
        end,
    Resp = kz_json:from_list(
             [{<<"Help">>, Help}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    kapi_tasks:publish_lookup_resp(kz_api:server_id(JObj), Resp).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_help(kz_json:object()) -> kz_tasks:help_error().
get_help(JObj) ->
    case {kapi_tasks:category(JObj), kapi_tasks:action(JObj)} of
        {'undefined', 'undefined'} -> help();
        {Category, 'undefined'} -> help(Category);
        {Category, Action} -> help(Category, Action)
    end.

-spec parse_apis(kz_proplist()) -> kz_json:object().
parse_apis(JObjs) ->
    parse_apis(JObjs, kz_json:new()).

-spec parse_apis(kz_json:objects(), kz_json:object()) -> kz_json:object().
parse_apis([], Acc) -> Acc;
parse_apis([JObj|JObjs], Acc) ->
    [Category] = kz_json:get_keys(JObj),
    Actions = kz_json:get_value(Category, JObj),
    lists:foreach(fun verify_unicity_map/1, kz_json:to_proplist(Actions)),
    NewAcc = kz_json:set_value(Category, Actions, Acc),
    parse_apis(JObjs, NewAcc).

-spec verify_unicity_map({ne_binary(), kz_json:object()}) -> 'ok'.
verify_unicity_map({_Action, API}) ->
    Fields0 = kz_tasks:mandatory(API) ++ kz_tasks:optional(API),
    Fields = [kz_util:to_lower_binary(Field) || Field <- Fields0],
    case
        length(lists:usort(Fields)) == length(Fields)
        andalso Fields == Fields0
    of
        'true' -> 'ok';
        'false' ->
            lager:error("action '~s' has duplicate or uppercase fields", [_Action])
    end.

%%% End of Module.
