%% @private
%% @doc Dictionary server
-module(eradius_dict).
-export([start_link/0, lookup/1, load_tables/0, lookup_by_name/3]).
-export_type([attribute/0, attr_value/0, table_name/0, attribute_id/0, attribute_type/0,
              attribute_prim_type/0, attribute_encryption/0, vendor_id/0, value_id/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-compile([{parse_transform, lager_transform}]).

-include("eradius_dict.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(SERVER, ?MODULE).
-define(TABLENAME_ID_TO_NAME, ?MODULE).
-define(TABLENAME_NAME_TO_ID, eradius_dict2).

-type table_name() :: atom() | string().
-type attribute_id() :: pos_integer() | {vendor_id(), pos_integer()}.
-type attribute_encryption() :: 'no' | 'scramble' | 'salt_crypt'.
-type attribute_type() :: attribute_prim_type() | {tagged, attribute_prim_type()}.
-type attribute_prim_type() :: 'string' | 'integer' | 'integer64' | 'ipaddr' | 'ipv6addr'
                             | 'ipv6prefix' | 'date' | 'abinary' | 'binary' | 'octets'.

-type value_id() :: {attribute_id(), pos_integer()}.
-type vendor_id() :: pos_integer().

-type attribute()  :: #attribute{} | attribute_id().
-type attr_value() :: term().

-record(state, {}).

%% ------------------------------------------------------------------------------------------
%% -- API
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec lookup(attribute_id() | value_id()) -> [#attribute{} | #value{} | #vendor{}].
lookup(Id) ->
    ets:lookup(?TABLENAME_ID_TO_NAME, Id).

-spec lookup_by_name(binary(), 'attribute2' | 'value2' | 'vendor2', binary()) -> [tuple()].
lookup_by_name(AccountId, Type, Name) when is_binary(AccountId) and is_atom(Type) and is_binary(Name) ->
    ets:lookup(?TABLENAME_NAME_TO_ID, {AccountId, Type, Name}).

% TODO: need to complete the spec
-spec load_tables() -> ok.
load_tables() ->
    gen_server:call(?SERVER, load_tables, infinity).

%% ------------------------------------------------------------------------------------------
%% -- gen_server callbacks
init([]) ->
    create_tables(),
    {ok, #state{}}.

create_tables() ->
    ets:new(?TABLENAME_ID_TO_NAME, [named_table, {keypos, 2}, protected]),
    ets:new(?TABLENAME_NAME_TO_ID, [named_table, {keypos, 1}, protected]).

handle_call(load_tables, _From, State) ->
    {reply, do_load_tables(), State}.

%% unused callbacks
handle_cast(_Msg, State)   -> {noreply, State}.
handle_info(_Info, State)  -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, _NewVsn, _State) -> {ok, state}.

%% ------------------------------------------------------------------------------------------
%% -- gen_server callbacks

% TODO: need change the spec
-spec do_load_tables() -> ok.
do_load_tables() ->
    {ok, Accounts} = couch_mgr:get_all_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_id">>),
    lists:foreach(fun(JObj) ->
                        load_account_tables(wh_json:get_value(<<"id">>, JObj))
                    end, Accounts),
    load_system_tables().

load_account_tables(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    {ok, AaaDoc} = couch_mgr:open_doc(AccountDb, <<"aaa">>),
    ServersList = wh_json:get_value(<<"servers">>, AaaDoc),
    DictsList1 = [wh_json:get_value(<<"dicts">>, Server) || Server <- ServersList],
    DictsList = lists:usort(lists:flatten(DictsList1)),
    load_db_tables(AccountId, DictsList, <<"aaa/fetch_dicts">>).

load_system_tables() ->
    {ok, AaaDoc} = couch_mgr:open_doc(<<"system_config">>, <<"circlemaker">>),
    ServersList = wh_json:get_value(<<"servers">>, AaaDoc),
    DictsList1 = [wh_json:get_value(<<"dicts">>, Server) || Server <- ServersList],
    DictsList = lists:usort(lists:flatten(DictsList1)),
    load_db_tables(<<"system_config">>, DictsList, <<"aaa/fetch_system_dicts">>).

load_db_tables(AccountId, DictList, DesignDoc) ->
    {ok, Dicts} = couch_mgr:get_all_results(?KZ_AAA_DICTS_DB, DesignDoc),
    lists:foreach(fun(Dict) ->
        Value = wh_json:get_value(<<"value">>, Dict),
        DictName = wh_json:get_value(<<"name">>, Value),
        Owner = wh_json:get_value(<<"owner">>, Value, <<"system_config">>),
        case (AccountId == Owner) and lists:member(DictName, DictList) of
            true ->
                DictId = wh_json:get_value(<<"id">>, Value),
                {ok, Doc} = couch_mgr:open_doc(?KZ_AAA_DICTS_DB, DictId),
                process_dict(wh_json:get_value(<<"value">>, Doc), AccountId),
                lager:info("Loaded RADIUS table ~p for account ~p", [DictName, AccountId]);
            _ ->
                []
        end
    end, Dicts).

process_dict(AVPList, AccountId) ->
    DefsList = lists:map(fun(JObj) ->
        [Key] = wh_json:get_keys(JObj),
        Val = wh_json:get_value(Key, JObj),
        process_entry(Key, Val, AccountId)
    end, AVPList),
    lists:foreach(fun({Def1, Def2}) ->
        ets:insert(?TABLENAME_ID_TO_NAME, Def1),
        ets:insert(?TABLENAME_NAME_TO_ID, Def2)
    end, DefsList).

process_entry(<<"vendor">>, Val, AccountId) ->
    [VendorName] = wh_json:get_keys(Val),
    VendorId = wh_json:get_integer_value(VendorName, Val),
    Def1 = {vendor, VendorId, binary_to_list(VendorName)},
    Def2 = {{AccountId, vendor2, binary_to_list(VendorName)}, VendorId},
    {Def1, Def2};
process_entry(<<"attr">>, Val, AccountId) ->
    VendorId = wh_json:get_integer_value(<<"vid">>, Val),
    AttrId = wh_json:get_integer_value(<<"id">>, Val),
    Id = case VendorId of
             undefined -> AttrId;
             _ -> {VendorId, AttrId}
         end,
    Type = wh_json:get_atom_value(<<"type">>, Val),
    AttrName = wh_json:get_string_value(<<"name">>, Val),
    Enc = wh_json:get_atom_value(<<"enc">>, Val),
    Def1 = {attribute, Id, Type, AttrName, Enc},
    Def2 = {{AccountId, attribute2, AttrName}, Id, Type, Enc},
    {Def1, Def2};
process_entry(<<"val">>, Val, AccountId) ->
    VendorId = wh_json:get_integer_value(<<"vid">>, Val),
    AttrId = wh_json:get_integer_value(<<"id">>, Val),
    Value = wh_json:get_integer_value(<<"val">>, Val),
    ValueName = wh_json:get_string_value(<<"name">>, Val),
    Id = case VendorId of
             undefined -> {AttrId, Value};
             _ -> {{VendorId, AttrId}, Value}
         end,
    Def1 = {value, Id, ValueName},
    Def2 = {{AccountId, value2, ValueName}, Id},
    {Def1, Def2}.
