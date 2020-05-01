%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_search).
-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-export([find/1
        ,next/1
        ,discovery/1, discovery/2

        ,quantity/1
        ,prefix/1, prefix/2
        ,normalized_prefix/1, normalized_prefix/2
        ,query_options/1, query_options/2
        ,dialcode/1
        ,country/1
        ,offset/1
        ,query_id/1
        ,account_id/1
        ,reseller_id/1
        ]).

-include("knm.hrl").

-type option() :: {'quantity', pos_integer()} |
                  {'prefix', kz_term:ne_binary()} |
                  {'dialcode', kz_term:ne_binary()} |
                  {'country', knm_util:country_iso3166a2()} |
                  {'offset', non_neg_integer()} |
                  {'blocks', boolean()} |

                  %% FIXME: knm_managed.erl:76:
                  %% The test `<<_:8,_:_*8>> =:= undefined'
                  %% can never evaluate to `true'
                  {'account_id', kz_term:api_ne_binary()} |

                  {'query_id', kz_term:ne_binary()} |
                  {'reseller_id', kz_term:api_ne_binary()}. %% Same dialyzer warning

-type result() :: {kz_term:ne_binary(), {kz_term:ne_binary(), module(), kz_term:ne_binary(), kz_json:object()}}.
-type results() :: [result()].
-type mod_response() ::
        kz_either:either(kz_term:ne_binary() | atom(), results()) |
        {'bulk', results()}.

-type options() :: [option()].
-export_type([option/0, options/0
             ,results/0
             ,result/0
             ,mod_response/0
             ]).

-define(MAX_SEARCH, kapps_config:get_pos_integer(?KNM_CONFIG_CAT, <<"maximum_search_quantity">>, 500)).
-define(NUMBER_SEARCH_TIMEOUT
       ,kapps_config:get_pos_integer(?KNM_CONFIG_CAT, <<"number_search_timeout_ms">>, 5 * ?MILLISECONDS_IN_SECOND)
       ).

-define(POLLING_INTERVAL, 5000).

-define(EOT, '$end_of_table').

-type state() :: #{node => kz_term:ne_binary()
                  ,cache => ets:tid() | atom()
                  }.

-define(ETS_DISCOVERY_CACHE, 'knm_discovery_cache').
-define(ETS_DISCOVERY_CACHE_OPTIONS, ['bag', 'named_table', {'read_concurrency', 'true'}]).

-define(BINDINGS, [{'self', []}
                  ,{'discovery', ['federate']}
                  ]).
-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
-ifdef(TEST).
start_link() ->
    gen_listener:start_link({'local', ?MODULE}
                           ,?MODULE
                           ,[]
                           ,[]).
-else.
start_link() ->
    gen_listener:start_link({'local', ?MODULE}
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[]).
-endif.
%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state(), timeout()}.
init([]) ->
    State = #{node => kz_term:to_binary(node())
             ,cache => ets:new(?ETS_DISCOVERY_CACHE, ?ETS_DISCOVERY_CACHE_OPTIONS)
             },
    {'ok', State, ?POLLING_INTERVAL}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'first', Options}, _From, State) ->
    QueryId = query_id(Options),
    flush(QueryId),
    {'reply', next(Options), State, ?POLLING_INTERVAL};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State, ?POLLING_INTERVAL}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener',{'created_queue', Queue}}, State) ->
    {'noreply', State#{queue => Queue}, ?POLLING_INTERVAL};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'federators_consuming', _AreFederatorsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'reset_search',QID}, #{cache := Cache} = State) ->
    lager:debug("resetting query id ~s", [QID]),
    ets:delete(Cache, QID),
    {'noreply', State, ?POLLING_INTERVAL};
handle_cast({'add_result', Numbers}, #{cache := Cache} = State) ->
    ets:insert(Cache, Numbers),
    {'noreply', State, ?POLLING_INTERVAL};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, ?POLLING_INTERVAL}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State, ?POLLING_INTERVAL}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, #{node := Node}) ->
    _ = case kz_api:node(JObj) =/= Node
            andalso kz_api:event_name(JObj)
        of
            <<"flush">> -> kz_process:spawn(fun handle_flush/1, [JObj]);
            <<"request">> -> kz_process:spawn(fun handle_search/1, [JObj]);
            <<"number">> -> kz_process:spawn(fun handle_number/1, [JObj]);
            _ -> 'ok'
        end,
    'ignore'.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #{}) ->
    lager:debug("terminating number search : ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec flush(kz_term:ne_binary()) -> 'ok'.
-ifdef(TEST).
flush(_QID) -> 'ok'.
-else.
flush(QID) ->
    Payload = [{<<"Query-ID">>, QID}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:cast(Payload, fun kapi_discovery:publish_flush/1).
-endif.

-spec find(options()) -> kz_json:objects().
find(Options) ->
    find(Options, offset(Options)).

find(Options, 0) ->
    first(Options);
find(Options, _) ->
    do_find(Options, is_local(query_id(Options))).

do_find(Options, 'true') ->
    next(Options);
do_find(Options, 'false') ->
    Payload = [{<<"Query-ID">>, query_id(Options)}
              ,{<<"Prefix">>, normalized_prefix(Options)}
              ,{<<"Quantity">>, quantity(Options)}
              ,{<<"Offset">>, offset(Options)}
              ,{<<"Account-ID">>, account_id(Options)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    case kz_amqp_worker:call(Payload
                            ,fun kapi_discovery:publish_req/1
                            ,fun kapi_discovery:resp_v/1
                            )
    of
        {'ok', JObj} -> kapi_discovery:results(JObj);
        {'error', _Error} ->
            lager:debug("error requesting search from amqp: ~p", [_Error]),
            []
    end.

-spec first(options()) -> kz_json:objects().
first(Options) ->
    Carriers = knm_carriers:available_carriers(Options),
    lager:debug("contacting, in order: ~p", [Carriers]),
    QID = query_id(Options),
    gen_listener:cast(?MODULE, {'reset_search', QID}),
    Self = self(),
    Opts = [{'quantity', ?MAX_SEARCH}
           ,{'offset', 0}
           ,{'normalized_prefix', normalized_prefix(Options)}
            | Options
           ],
    lists:foreach(fun(Carrier) -> search_spawn(Self, Carrier, Opts) end, Carriers),
    wait_for_search(length(Carriers)),
    gen_listener:call(?MODULE, {'first', Options}).

-spec search_spawn(pid(), atom(), kz_term:proplist()) -> any().
search_spawn(Pid, Carrier, Options) ->
    F = fun() -> Pid ! {Carrier, search_carrier(Carrier, Options)} end,
    kz_process:spawn(F).

-spec search_carrier(atom(), kz_term:proplist()) -> any().
search_carrier(Carrier, Options) ->
    Prefix = normalized_prefix(Options),
    Quantity = quantity(Options),
    catch (Carrier:find_numbers(Prefix, Quantity, Options)).

-spec wait_for_search(integer()) -> 'ok'.
wait_for_search(0) -> 'ok';
wait_for_search(N) ->
    receive
        {_Carrier, {'ok', []}} ->
            lager:debug("~s found no numbers", [_Carrier]),
            wait_for_search(N - 1);
        {_Carrier, {'ok', Numbers}} ->
            lager:debug("~s found numbers", [_Carrier]),
            gen_listener:cast(?MODULE, {'add_result', Numbers}),
            wait_for_search(N - 1);
        {_Carrier, {'bulk', Numbers}} ->
            lager:debug("~s found bulk numbers", [_Carrier]),
            gen_listener:cast(?MODULE, {'add_result', Numbers}),
            wait_for_search(N - 1);
        {_Carrier, {'error', 'not_available'}} ->
            lager:debug("~s had no results", [_Carrier]),
            wait_for_search(N - 1);
        _Other ->
            lager:debug("unexpected search result ~p", [_Other]),
            wait_for_search(N - 1)
    after ?NUMBER_SEARCH_TIMEOUT ->
            lager:debug("timeout (~B) collecting responses from search providers", [?NUMBER_SEARCH_TIMEOUT]),
            wait_for_search(N - 1)
    end.

-spec next(options()) -> kz_json:objects().
next(Options) ->
    QID = query_id(Options),
    Quantity = quantity(Options),
    Offset = offset(Options),
    MatchSpec = [{{QID,'$1'},[],['$1']}],
    QLH = qlc:keysort(1, ets:table(?ETS_DISCOVERY_CACHE, [{'traverse', {'select', MatchSpec}}])),
    QLC = qlc:cursor(QLH),
    _ = Offset > 0
        andalso qlc:next_answers(QLC, Offset),
    Results = qlc:next_answers(QLC, Quantity),
    qlc:delete_cursor(QLC),
    lager:debug("returning ~B results", [length(Results)]),
    [kz_json:from_list(
       [{<<"number">>, Num}
       ,{<<"state">>, State}
       ])
     || {Num, _ModuleName, State, _CarrierData} <- Results
    ].

%%------------------------------------------------------------------------------
%% @doc Create a number in a discovery state.
%% @end
%%------------------------------------------------------------------------------
-ifndef(TEST).
-spec create_discovery(kz_term:ne_binary(), module(), kz_json:object(), knm_options:options()) ->
          knm_phone_number:record().
create_discovery(DID=?NE_BINARY, Carrier, Data, Options0) ->
    Options = [{'state', ?NUMBER_STATE_DISCOVERY}
              ,{'module_name', kz_term:to_binary(Carrier)}
               | Options0
              ],
    {'ok', PhoneNumber} =
        knm_phone_number:setters(knm_phone_number:from_number_with_options(DID, Options)
                                ,[{fun knm_phone_number:set_carrier_data/2, Data}]
                                ),
    PhoneNumber.

-spec create_discovery(kz_json:object(), knm_options:options()) -> knm_phone_number:record().
create_discovery(JObj, Options) ->
    knm_phone_number:from_json_with_options(JObj, Options).
-endif.

-spec quantity(options()) -> pos_integer().
quantity(Options) ->
    Quantity = props:get_integer_value('quantity', Options, 1),
    min(Quantity, ?MAX_SEARCH).

-spec prefix(options()) -> kz_term:ne_binary().
prefix(Options) ->
    props:get_ne_binary_value('prefix', Options).

-spec prefix(options(), kz_term:ne_binary()) -> kz_term:ne_binary().
prefix(Options, Default) ->
    props:get_ne_binary_value('prefix', Options, Default).

-spec query_options(options()) -> kz_term:api_object().
query_options(Options) ->
    props:get_value('query_options', Options).

-spec query_options(options(), kz_term:api_object()) -> kz_term:api_object().
query_options(Options, Default) ->
    props:get_value('query_options', Options, Default).

-spec normalized_prefix(options()) -> kz_term:ne_binary().
normalized_prefix(Options) ->
    JObj = query_options(Options, kz_json:new()),
    Dialcode = dialcode(Options),
    Prefix = kz_json:get_ne_binary_value(<<"Prefix">>, JObj, prefix(Options)),
    normalized_prefix(Options, <<Dialcode/binary, Prefix/binary>>).

-spec normalized_prefix(options(), kz_term:ne_binary()) -> kz_term:ne_binary().
normalized_prefix(Options, Default) ->
    props:get_ne_binary_value('normalized_prefix', Options, Default).

-spec dialcode(options()) -> kz_term:ne_binary().
dialcode(Options) ->
    Default = knm_util:prefix_for_country(country(Options)),
    props:get_ne_binary_value('dialcode', Options, Default).

-spec country(options()) -> knm_util:country_iso3166a2().
country(Options) ->
    case props:get_ne_binary_value('country', Options, ?KNM_DEFAULT_COUNTRY) of
        <<_:8, _:8>>=Country -> Country;
        _Else ->
            lager:debug("~p is not iso3166a2, using default"),
            ?KNM_DEFAULT_COUNTRY
    end.

-spec query_id(options()) -> kz_term:api_binary().
query_id(Options) ->
    props:get_ne_binary_value('query_id', Options).

-spec offset(options()) -> non_neg_integer().
offset(Options) ->
    props:get_integer_value('offset', Options, 0).

-spec account_id(options()) -> kz_term:api_ne_binary().
account_id(Options) ->
    props:get_value('account_id', Options).

-spec reseller_id(options()) -> kz_term:api_ne_binary().
reseller_id(Options) ->
    props:get_value('reseller_id', Options).

-spec is_local(kz_term:ne_binary()) -> boolean().
is_local(QID) ->
    ets:match_object(?ETS_DISCOVERY_CACHE, {QID, '_'}) =/= [].

-spec discovery(kz_term:ne_binary()) ->
          {'ok', knm_phone_number:record()} |
          {'error', any()}.
discovery(Num) ->
    discovery(Num, []).

-spec discovery(kz_term:ne_binary(), knm_options:options()) ->
          {'ok', knm_phone_number:record()} |
          {'error', any()}.
discovery(Num, Options) ->
    case local_discovery(Num, Options) of
        {'ok', _}=OK -> OK;
        {'error', 'not_found'} -> remote_discovery(Num, Options)
    end.

-spec local_discovery(kz_term:ne_binary(), knm_options:options()) ->
          {'ok', knm_phone_number:record()} |
          {'error', any()}.
-ifdef(TEST).
local_discovery(_Num, _Options) -> {'error', 'not_found'}.
-else.
local_discovery(Num, Options) ->
    case ets:match_object(?ETS_DISCOVERY_CACHE, {'_', {Num, '_', ?NUMBER_STATE_DISCOVERY, '_'}}) of
        [] -> {'error', 'not_found'};
        [{_QID, {Num, Carrier, _, Data}} | _] ->
            {'ok', create_discovery(Num, Carrier, Data, Options)}
    end.
-endif.

-spec remote_discovery(kz_term:ne_binary(), knm_options:options()) ->
          {'ok', knm_phone_number:record()} |
          {'error', any()}.
-ifdef(TEST).
remote_discovery(_Num, _Options) -> {'error', 'not_found'}.
-else.
remote_discovery(Number, Options) ->
    Payload = [{<<"Number">>, Number}
              ,{<<"Account-ID">>, knm_options:account_id(Options)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    case kz_amqp_worker:call(Payload
                            ,fun kapi_discovery:publish_number_req/1
                            ,fun kapi_discovery:resp_v/1
                            )
    of
        {'ok', JObj} -> {'ok', create_discovery(kapi_discovery:results(JObj), Options)};
        {'error', _Error} ->
            lager:debug("error requesting number from amqp: ~p", [_Error]),
            {'error', 'not_found'}
    end.
-endif.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
handle_flush(JObj) ->
    'true' = kapi_discovery:flush_v(JObj),
    QID = kapi_discovery:query_id(JObj),
    gen_listener:cast(?MODULE, {'reset_search',QID}).

-spec handle_search(kz_json:object()) -> 'ok'.
handle_search(JObj) ->
    'true' = kapi_discovery:req_v(JObj),
    handle_search(JObj, is_local(kapi_discovery:query_id(JObj))).

handle_search(JObj, 'false') ->
    lager:debug("query id ~s not handled in this node", [kapi_discovery:query_id(JObj)]);
handle_search(JObj, 'true') ->
    lager:debug("query id ~s handled in this node", [kapi_discovery:query_id(JObj)]),
    'true' = kapi_discovery:req_v(JObj),
    QID = kapi_discovery:query_id(JObj),
    Offset = kapi_discovery:offset(JObj),
    Quantity = kapi_discovery:quantity(JObj),
    Prefix = kapi_discovery:prefix(JObj),
    AccountId = kz_api:account_id(JObj),
    Options = [{'quantity', Quantity}
              ,{'prefix', Prefix}
              ,{'offset', Offset}
              ,{'account_id', AccountId}
              ,{'query_id', QID}
              ],
    Results = find(Options),
    Payload = [{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              ,{<<"Query-ID">>, QID}
              ,{<<"Results">>, Results}
               | kz_api:default_headers(kz_api:server_id(JObj), ?APP_NAME, ?APP_VERSION)
              ],
    Publisher = fun(P) -> kapi_discovery:publish_resp(kz_api:server_id(JObj), P) end,
    kz_amqp_worker:cast(Payload, Publisher).

-spec handle_number(kz_json:object()) -> 'ok'.
handle_number(JObj) ->
    'true' = kapi_discovery:number_req_v(JObj),
    Number = kapi_discovery:number(JObj),
    case local_discovery(Number, []) of
        {'error', 'not_found'} -> 'ok';
        {'ok', PN} ->
            Payload = [{<<"Msg-ID">>, kz_api:msg_id(JObj)}
                      ,{<<"Results">>, knm_phone_number:to_json(PN)}
                       | kz_api:default_headers(kz_api:server_id(JObj), ?APP_NAME, ?APP_VERSION)
                      ],
            Publisher = fun(P) -> kapi_discovery:publish_resp(kz_api:server_id(JObj), P) end,
            kz_amqp_worker:cast(Payload, Publisher)
    end.
