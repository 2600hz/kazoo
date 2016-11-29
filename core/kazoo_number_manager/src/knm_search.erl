%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(knm_search).

-include_lib("kazoo_json/include/kazoo_json.hrl").
-include("knm.hrl").

-export([find/1
        ,next/1

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

-define(DEFAULT_CARRIER_MODULES, [?CARRIER_LOCAL]).

-type option() :: {'quantity', pos_integer()} |
                  {'prefix', ne_binary()} |
                  {'dialcode', ne_binary()} |
                  {'country', knm_util:country_iso3166a2()} |
                  {'offset', non_neg_integer()} |
                  {'blocks', boolean()} |
                  {'account_id', ne_binary()} |
                  {'query_id', ne_binary()} |
                  {'ets', ets:tid()} |
                  {'reseller_id', ne_binary()}.
-type options() :: [option()].
-export_type([option/0, options/0]).


-define(DEFAULT_CARRIER_MODULE
       ,kapps_config:get_binary(?KNM_CONFIG_CAT, <<"available_module_name">>, ?CARRIER_LOCAL)).
-define(CARRIER_MODULES
       ,kapps_config:get(?KNM_CONFIG_CAT, <<"carrier_modules">>, ?DEFAULT_CARRIER_MODULES)).
-define(CARRIER_MODULES(AccountId)
       ,kapps_account_config:get(AccountId, ?KNM_CONFIG_CAT, <<"carrier_modules">>, ?CARRIER_MODULES)).

-define(MAX_QUANTITY, kapps_config:get_integer(?KNM_CONFIG_CAT, <<"maximum_search_quantity">>, 50)).

-define(MAX_SEARCH, 500).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find(options()) -> kz_json:objects().


find(Options) ->
    Carriers = available_carriers(Options),
    ETS = props:get_value('ets', Options),
    QID = query_id(Options),
    ets:delete(ETS, QID),

    Self = self(),
    Opts = [{'quantity', ?MAX_SEARCH}
           ,{'offset', 0}
            | Options
           ],
    lists:foreach(fun(Carrier) -> search_spawn(Self, Carrier, Opts) end, Carriers),
    wait_for_search(length(Carriers), Options),
    next(Options).

-spec search_spawn(pid(), atom(), kz_proplist()) -> any().
search_spawn(Pid, Carrier, Options) ->
    F = fun() -> Pid ! search_carrier(Carrier, Options) end,
    kz_util:spawn(F).

-spec search_carrier(atom(), kz_proplist()) -> any().
search_carrier(Carrier, Options) ->
    Prefix = normalized_prefix(Options),
    Quantity = quantity(Options),
    catch(Carrier:find_numbers(Prefix, Quantity, Options)).

-spec wait_for_search(integer(), options()) -> 'ok'.
wait_for_search(0, _Options) -> 'ok';
wait_for_search(N, Options) ->
    ETS = props:get_value('ets', Options),
    receive
        {'ok', Numbers} ->
            ets:insert(ETS, Numbers),
            wait_for_search(N - 1, Options);
        _ -> wait_for_search(N - 1, Options)
    after 5000 ->
            lager:debug("timeout (~B) collecting responses from search providers", [5000]),
            wait_for_search(N - 1, Options)
    end.

-spec next(options()) -> kz_json:objects().
next(Options) ->
    ETS = props:get_value('ets', Options),
    QID = query_id(Options),
    Quantity = quantity(Options),
    Offset = offset(Options),
    MatchSpec = [{{QID,'$1'},[],['$1']}],
    QLH = qlc:keysort(1, ets:table(ETS, [{'traverse', {'select', MatchSpec}}])),
    QLC = qlc:cursor(QLH),
    _ = case Offset > 0 of
            'true' -> qlc:next_answers(QLC, Offset);
            _ -> 'true'
        end,
    Results = qlc:next_answers(QLC, Quantity),
    qlc:delete_cursor(QLC),
    kz_util:spawn(fun ensure_states/2, [Options, Results]),
    lager:debug("returning ~B results", [length(Results)]),
    [kz_json:from_list([{<<"number">>, Num}]) || {Num, _, _, _} <- Results].

ensure_states(Options0, Numbers) ->
    Prefix = normalized_prefix(Options0),
    QKeys = [ [State, kz_util:to_binary(Mod), Num] || {Num, Mod, State, _} <- Numbers],
    DB = knm_converters:to_db(Prefix),
    Options = [{'keys', QKeys}],
    case kz_datamgr:get_result_keys(DB, <<"numbers/status">>, Options) of
        {'error', Error} -> lager:critical("error querying number keys : ~p", [Error]); 
        {'ok', []} -> create_discovery(DB, Numbers);
        {'ok', DBKeys} ->
            Filter = [Num ||[_, _, Num] <- DBKeys],
            Filtered = [N || {Num, _, _, _}=N <- Numbers, not lists:member(Num, Filter)],
            create_discovery(DB, Filtered)
    end.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a number/list in a discovery (or given) state.
%% @end
%%--------------------------------------------------------------------
create_discovery(DB, Numbers) ->
    Docs = [create_discovery(Num, Mod, Data) || {Num, Mod, ?NUMBER_STATE_DISCOVERY, Data} <- Numbers],
    kz_datamgr:suppress_change_notice(),
    kz_datamgr:save_docs(DB, Docs).

-spec create_discovery(ne_binary(), module(), kz_json:object()) -> kz_json:object().
create_discovery(DID=?NE_BINARY, Carrier, Data) ->
    Options = [{'state', ?NUMBER_STATE_DISCOVERY}
              ,{'module_name', kz_util:to_binary(Carrier)}
              ],
    {'ok', PhoneNumber} =
        knm_phone_number:setters(knm_phone_number:new(DID, Options)
                                ,[{fun knm_phone_number:set_carrier_data/2, Data}
                                 ]),
    knm_phone_number:to_json(PhoneNumber).


-spec quantity(options()) -> pos_integer().
quantity(Options) ->
    props:get_integer_value('quantity', Options, 1).

-spec prefix(options()) -> ne_binary().
-spec prefix(options(), ne_binary()) -> ne_binary().
prefix(Options) ->
    props:get_ne_binary_value('prefix', Options).
prefix(Options, Default) ->
    props:get_ne_binary_value('prefix', Options, Default).

-spec query_options(options()) -> api_object().
-spec query_options(options(), api_object()) -> api_object().
query_options(Options) ->
    props:get_value('query_options', Options).
query_options(Options, Default) ->
    props:get_value('query_options', Options, Default).

-spec normalized_prefix(options()) -> ne_binary().
-spec normalized_prefix(options(), ne_binary()) -> ne_binary().
normalized_prefix(Options) ->
    JObj = query_options(Options, kz_json:new()),
    Default = kz_json:get_ne_binary_value(<<"Prefix">>, JObj, prefix(Options)),
    normalized_prefix(Options, Default).
normalized_prefix(Options, Default) ->
    props:get_ne_binary_value('normalized_prefix', Options, Default).

-spec dialcode(options()) -> ne_binary().
dialcode(Options) ->
    props:get_ne_binary_value('dialcode', Options).

-spec country(options()) -> knm_util:country_iso3166a2().
country(Options) ->
    case props:get_ne_binary_value('country', Options, ?KNM_DEFAULT_COUNTRY) of
        <<_:8, _:8>>=Country -> Country;
        _Else ->
            lager:debug("~p is not iso3166a2, using default"),
            ?KNM_DEFAULT_COUNTRY
    end.

-spec query_id(options()) -> api_binary().
query_id(Options) ->
    props:get_ne_binary_value('query_id', Options).

-spec offset(options()) -> non_neg_integer().
offset(Options) ->
    props:get_integer_value('offset', Options, 0).

-spec account_id(options()) -> api_ne_binary().
account_id(Options) ->
    props:get_value('account_id', Options).

-spec reseller_id(options()) -> ne_binary().
reseller_id(Options) ->
    props:get_value('reseller_id', Options).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec keep_only_reachable([ne_binary()]) -> atoms().
keep_only_reachable(ModuleNames) ->
    lager:debug("resolving carrier modules: ~p", [ModuleNames]),
    [Module
     || M <- ModuleNames,
        (Module = kz_util:try_load_module(M)) =/= 'false'
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc Create a list of all available carrier modules
%%--------------------------------------------------------------------
-spec available_carriers(options()) -> atoms().
available_carriers(Options) ->
    case account_id(Options) of
        'undefined' ->
            keep_only_reachable(?CARRIER_MODULES);
        _AccountId ->
            ResellerId = reseller_id(Options),
            First = [?CARRIER_RESERVED, ?CARRIER_RESERVED_RESELLER, ?CARRIER_LOCAL],
            keep_only_reachable(First ++ (?CARRIER_MODULES(ResellerId) -- First))
    end.
