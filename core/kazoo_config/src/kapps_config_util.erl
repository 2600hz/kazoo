-module(kapps_config_util).
-include_lib("kazoo/include/kz_types.hrl").
-export([
    get_config/2,
    get_reseller_config/2,
    account_schema_name/1,
    account_schema/1,
    system_schema/1
]).

-spec doc_id(ne_binary()) -> ne_binary().
doc_id(Config) -> kapps_account_config:config_doc_id(Config).

-spec get_config(ne_binary(), ne_binary()) -> kz_json:object().
get_config(Account, Config) ->
    Programm = [
        fun load_config_from_account/2,
        fun load_config_from_reseller/2,
        fun load_config_from_system/2,
        fun load_default_config/2
    ],
    Schema = account_schema(Config),
    kz_json_schema:filter(get_config(Account, Config, Programm), Schema).

-spec get_config(ne_binary(), ne_binary(), [fun()]) -> kz_json:object().
get_config(Account, Config, Programm) ->
    Confs = [ maybe_new(P(Account, Config)) || P <- Programm ],
    kz_json:merge_recursive(lists:reverse(Confs)).

-spec get_reseller_config(ne_binary(), ne_binary()) -> kz_json:object().
get_reseller_config(Account, Config) ->
    Programm = [
        fun load_config_from_reseller/2,
        fun load_config_from_system/2,
        fun load_default_config/2
    ],
    Schema = account_schema(Config),
    kz_json_schema:filter(get_config(Account, Config, Programm), Schema).

-spec load_config_from_account(ne_binary(), ne_binary()) -> {ok, kz_json:object()} | {error, any()}.
load_config_from_account(Account, Config) ->
    AccountDb = kz_util:format_account_id(Account, encoded),
    kz_datamgr:open_cache_doc(AccountDb, doc_id(Config), [{cache_failures, [not_found]}]).

-spec load_config_from_reseller(ne_binary(), ne_binary()) -> {ok, kz_json:object()} | {error, any()}.
load_config_from_reseller(Account, Config) ->
    ResellerId = kz_services:find_reseller_id(Account),
    load_config_from_account(ResellerId, Config).

-spec load_config_from_system(ne_binary(), ne_binary()) -> {ok, kz_json:object()} | {error, any()}.
load_config_from_system(_Account, Config) ->
    kz_json:get_value(<<"default">>, ensure_value(kapps_config:get_category(Config))).

-spec load_default_config(ne_binary(), ne_binary()) -> {ok, kz_json:object()}.
load_default_config(_Account, Config) ->
    Schema = system_schema(Config),
    {ok, kz_doc:set_id(kz_json_schema:default_object(Schema), doc_id(Config))}.

ensure_value({ok, JObj}) -> JObj;
ensure_value(_) -> kz_json:new().

-spec maybe_new({ok, kz_json:object()}) -> kz_json:object().
maybe_new({ok, JObj}) -> JObj;
maybe_new(_) -> kz_json:new().

-spec account_schema_name(ne_binary()) -> ne_binary().
account_schema_name(Config) when is_binary(Config) -> <<"account_config.", Config/binary>>.

-spec system_schema_name(ne_binary()) -> ne_binary().
system_schema_name(Config) when is_binary(Config) -> <<"system_config.", Config/binary>>.

-spec system_schema(ne_binary()) -> kz_json:object().
system_schema(Config) when is_binary(Config) ->
    Name = system_schema_name(Config),
    {ok, Schema} = kz_json_schema:load(Name),
    Schema.

-spec account_schema(ne_binary()) -> kz_json:object().
account_schema(Config) when is_binary(Config) ->
    Name = account_schema_name(Config),
    {ok, Schema} = kz_json_schema:load(Name),
    kz_json:set_value(<<"additionalProperties">>, false, Schema).
