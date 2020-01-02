%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_storage).

-include("kazoo_apps.hrl").

-define(CONFIG_CAT, <<"call_command">>).
-define(STORAGE_TIMEOUT(App), kapps_config:get_integer(?CONFIG_CAT, [<<"store_file">>, kz_term:to_binary(App), <<"save_timeout_ms">>], 5 * ?MILLISECONDS_IN_MINUTE, <<"default">>)).
-define(STORAGE_RETRIES(App), kapps_config:get_integer(?CONFIG_CAT, [<<"store_file">>, kz_term:to_binary(App), <<"retries">>], 5, <<"default">>)).

%%==============================================================================
%% API functions
%%==============================================================================

-export([store_file/3, store_file/4]).

-spec store_file(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary() | function()) -> 'ok' | {'error', any()}.
store_file(Node, Filename, Url) ->
    store_file(Node, Filename, Url, #{}).

-spec store_file(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary() | function(), map()) -> 'ok' | {'error', any()}.
store_file(Node, Filename, Url, Map) ->
    App = kz_util:calling_app(),
    store_file(Filename, Url, storage_retries(App), storage_timeout(App), Map#{media_server => Node}).

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec store_file_args(kz_term:ne_binary(), kz_term:ne_binary() | function()) -> kz_term:proplist().
store_file_args(Filename, UrlFun) ->
    Url = case is_function(UrlFun, 0) of
              'true' -> UrlFun();
              'false' -> UrlFun
          end,
    [{<<"File-Name">>, Filename}
    ,{<<"Url">>, Url}
    ,{<<"Http-Method">>, <<"put">>}
    ].

-spec store_file(kz_term:ne_binary(), kz_term:ne_binary() | function(), pos_integer(), timeout(), map()) ->
          'ok' | {'error', any()}.
store_file(Filename, Url, Tries, Timeout, #{media_server := Node}=Map) ->
    Msg = case kz_maps:get('alert_msg', Map) of
              'undefined' ->
                  io_lib:format("Error Storing File ~s From Media Server ~s",[Filename, Node]);
              ErrorMsg ->ErrorMsg
          end,
    {AppName, AppVersion} = kz_util:calling_app_version(),
    API = fun() -> [{<<"Command">>, <<"send_http">>}
                   ,{<<"Args">>, kz_json:from_list(store_file_args(Filename, Url))}
                   ,{<<"FreeSWITCH-Node">>, Node}
                    | kz_api:default_headers(AppName, AppVersion)
                   ]
          end,
    do_store_file(Tries, Timeout, API, Msg, Map).

-spec do_store_file(pos_integer(), timeout(), function(), kz_term:ne_binary(), map()) ->
          'ok' | {'error', any()}.
do_store_file(Tries, Timeout, API, Msg, #{media_server := Node}=Map) ->
    Payload = API(),
    case kz_amqp_worker:call(Payload, fun kapi_switch:publish_command/1, fun kapi_switch:fs_reply_v/1, Timeout) of
        {'ok', JObj} ->
            case kz_json:get_ne_binary_value(<<"Result">>, JObj) of
                <<"success">> -> 'ok';
                <<"error">> ->
                    Error = kz_json:get_first_defined([[<<"Event-Data">>, <<"API-Error">>], <<"Error">>], JObj, <<"error not available">>),
                    retry_store_file(Tries - 1, Timeout, API, Msg, Error, maybe_add_debug_data(JObj, Map));
                _Other ->
                    Error = kz_term:to_binary(io_lib:format("unhandled return ('~s') from store file", [_Other])),
                    retry_store_file(Tries - 1, Timeout, API, Msg, Error, maybe_add_debug_data(JObj, Map))
            end;
        {'returned', _JObj, Basic} ->
            Error = io_lib:format("message returned from amqp. is ~s down ?", [Node]),
            retry_store_file(Tries - 1, Timeout, API, Msg, kz_term:to_binary(Error), Map#{basic_return =>kz_json:to_proplist(Basic)});
        {'timeout', _JObj} ->
            Error = io_lib:format("timeout publishing message to amqp. is ~s down ?", [Node]),
            retry_store_file(Tries - 1, Timeout, API, Msg, kz_term:to_binary(Error), Map);
        {'error', Err} ->
            Error = io_lib:format("error publishing message to amqp. is ~s down ? : ~p", [Node, Err]),
            retry_store_file(Tries - 1, Timeout, API, Msg, kz_term:to_binary(Error), Map)
    end.

-spec retry_store_file(integer(), timeout(), kz_term:proplist() | function()
                      ,kz_term:ne_binary(), kz_term:ne_binary(), map()) ->
          'ok' | {'error', any()}.
retry_store_file(0, _Timeout, _API, Msg, Error, Map) ->
    lager:critical("~s : ~s", [Msg, Error]),
    kz_notify:detailed_alert(kz_term:to_binary(Msg)
                            ,kz_term:to_binary(Error)
                            ,maps:to_list(Map#{store_error => Error})
                            ,[]
                            ),
    {'error', Error};
retry_store_file(Tries, Timeout, API, Msg, Error, Map) ->
    lager:critical("~s : ~s", [Msg, Error]),
    timer:sleep(5 * ?MILLISECONDS_IN_SECOND),
    do_store_file(Tries, Timeout, API, Msg, Map).

maybe_add_debug_data(JObj, Map) ->
    case kz_json:get_value(<<"Event-Data">>, JObj) of
        'undefined' -> Map;
        Data -> Map#{error_details => Data}
    end.

-spec storage_timeout(kz_term:ne_binary()) -> pos_integer().
storage_timeout(App) ->
    ?STORAGE_TIMEOUT(App).

-spec storage_retries(kz_term:ne_binary()) -> pos_integer().
storage_retries(App) ->
    ?STORAGE_RETRIES(App).
