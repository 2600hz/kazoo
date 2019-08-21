%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_route_req).

-export([handle_req/2]).

-include("doodle.hrl").

-define(DEFAULT_ROUTE_WIN_TIMEOUT, 3000).
-define(ROUTE_WIN_TIMEOUT_KEY, <<"route_win_timeout">>).
-define(ROUTE_WIN_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, ?ROUTE_WIN_TIMEOUT_KEY, ?DEFAULT_ROUTE_WIN_TIMEOUT)).

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    'true' = kapi_route:req_v(JObj),
    Call = kapps_call:from_route_req(JObj),
    case is_binary(kapps_call:account_id(Call))
    of
        'false' -> 'ok';
        'true' ->
            lager:info("received a request asking if doodle can route this message"),
            AllowNoMatch = allow_no_match(Call),
            case kz_flow:lookup(Call) of
                %% if NoMatch is false then allow the callflow or if it is true and we are able allowed
                %% to use it for this call
                {'ok', Flow, NoMatch} when (not NoMatch)
                                           orelse AllowNoMatch ->
                    NewFlow = maybe_prepend_preflow(Call, Flow),
                    maybe_reply_to_req(JObj, Props, Call, NewFlow, NoMatch);
                {'ok', _, 'true'} ->
                    lager:info("only available flow is a nomatch for a unauthorized call", []);
                {'error', R} ->
                    lager:info("unable to find flow ~p", [R])
            end
    end.

-spec maybe_prepend_preflow(kapps_call:call(), kz_json:object()) -> kz_json:object().
maybe_prepend_preflow(Call, CallFlow) ->
    AccountDb = kapps_call:account_db(Call),
    case kzd_accounts:fetch(AccountDb) of
        {'error', _E} ->
            lager:warning("could not open account doc ~s : ~p", [AccountDb, _E]),
            CallFlow;
        {'ok', Doc} ->
            case kzd_accounts:preflow_id(Doc) of
                'undefined' -> CallFlow;
                PreflowId   -> kzd_callflows:prepend_preflow(CallFlow, PreflowId)
            end
    end.

-spec allow_no_match(kapps_call:call()) -> boolean().
allow_no_match(Call) ->
    kapps_call:custom_channel_var(<<"Referred-By">>, Call) =/= 'undefined'
        orelse allow_no_match_type(Call).

-spec allow_no_match_type(kapps_call:call()) -> boolean().
allow_no_match_type(Call) ->
    case kapps_call:authorizing_type(Call) of
        'undefined' -> 'false';
        <<"resource">> -> 'false';
        <<"sys_info">> -> 'false';
        _ -> 'true'
    end.

-spec maybe_reply_to_req(kz_json:object(), kz_term:proplist(), kapps_call:call(), kz_json:object(), boolean()) ->
                                'ok'.
maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch) ->
    lager:info("callflow ~s in ~s satisfies request"
              ,[kz_doc:id(Flow), kapps_call:account_id(Call)]),
    {Name, Cost} = bucket_info(Call, Flow),

    case kz_buckets:consume_tokens(?APP_NAME, Name, Cost) of
        'false' ->
            lager:debug("bucket ~s doesn't have enough tokens(~b needed) for this call", [Name, Cost]);
        'true' ->
            ControllerQ = props:get_value('queue', Props),
            UpdatedCall = update_call(Flow, NoMatch, ControllerQ, Call, JObj),
            send_route_response(Flow, JObj, UpdatedCall)
    end.

-spec bucket_info(kapps_call:call(), kz_json:object()) -> {kz_term:ne_binary(), pos_integer()}.
bucket_info(Call, Flow) ->
    case kz_json:get_value(<<"pvt_bucket_name">>, Flow) of
        'undefined' -> {bucket_name_from_call(Call, Flow), bucket_cost(Flow)};
        Name -> {Name, bucket_cost(Flow)}
    end.

-spec bucket_name_from_call(kapps_call:call(), kz_json:object()) -> kz_term:ne_binary().
bucket_name_from_call(Call, Flow) ->
    <<(kapps_call:account_id(Call))/binary, ":", (kz_doc:id(Flow))/binary>>.

-spec bucket_cost(kz_json:object()) -> pos_integer().
bucket_cost(Flow) ->
    Min = kapps_config:get_integer(?CONFIG_CAT, <<"min_bucket_cost">>, 1),
    case kz_json:get_integer_value(<<"pvt_bucket_cost">>, Flow) of
        'undefined' -> Min;
        N when N < Min -> Min;
        N -> N
    end.

-spec send_route_response(kz_json:object(), kz_json:object(), kapps_call:call()) -> 'ok'.
send_route_response(_Flow, JObj, Call) ->
    lager:info("doodle knows how to route the message! sending sms response"),
    Resp = props:filter_undefined([{?KEY_MSG_ID, kz_api:msg_id(JObj)}
                                  ,{?KEY_MSG_REPLY_ID, kapi_route:fetch_id(JObj)}
                                  ,{<<"Routes">>, []}
                                  ,{<<"Method">>, <<"sms">>}
                                   | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_route:publish_resp(ServerId, P) end,
    case kz_amqp_worker:call(Resp
                            ,Publisher
                            ,fun kapi_route:win_v/1
                            ,?ROUTE_WIN_TIMEOUT
                            )
    of
        {'ok', RouteWin} ->
            lager:info("doodle has received a route win, taking control of the text"),
            doodle_route_win:execute_text_flow(RouteWin, kapps_call:from_route_win(RouteWin, Call));
        {'error', _E} ->
            lager:info("doodle didn't received a route win, exiting : ~p", [_E])
    end.

-spec update_call(kz_json:object(), boolean(), kz_term:ne_binary(), kapps_call:call(), kz_json:object()) -> kapps_call:call().
update_call(Flow, NoMatch, ControllerQ, Call, JObj) ->
    Updaters = [{fun kapps_call:kvs_store_proplist/2
                ,[{'cf_flow_id', kz_doc:id(Flow)}
                 ,{'cf_flow', kz_json:get_value(<<"flow">>, Flow)}
                 ,{'cf_capture_group', kz_json:get_ne_value(<<"capture_group">>, Flow)}
                 ,{'cf_no_match', NoMatch}
                 ,{'cf_metaflow', kz_json:get_value(<<"metaflows">>, Flow)}
                 ,{'flow_status', <<"queued">>}
                 ]
                }
               ,{fun kapps_call:set_controller_queue/2, ControllerQ}
               ,{fun kapps_call:set_application_name/2, ?APP_NAME}
               ,{fun kapps_call:set_application_version/2, ?APP_VERSION}
               ,fun(C) -> cache_resource_types(Flow, C, JObj) end
               ],
    kapps_call:exec(Updaters, Call).

-spec cache_resource_types(kz_json:object(), kapps_call:call(), kz_json:object()) -> kapps_call:call().
cache_resource_types(Flow, Call, JObj) ->
    lists:foldl(fun(K, C1) ->
                        kapps_call:kvs_store(K, kz_json:get_value(K, JObj), C1)
                end
               ,Call
               ,cache_resource_types(kapps_call:resource_type(Call), Flow, Call, JObj)
               ).

-spec cache_resource_types(kz_term:ne_binary(), kz_json:object(), kapps_call:call(), kz_json:object()) -> kz_term:ne_binaries().
cache_resource_types(<<"sms">>, _Flow, _Call, _JObj) ->
    [<<"Message-ID">>, <<"Body">>];
cache_resource_types(_Other, _Flow, _Call, _JObj) -> [].
