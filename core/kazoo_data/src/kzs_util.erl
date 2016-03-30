%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_util).


%% -export([maybe_add_rev/3
%%          ,maybe_add_pvt_type/3
%%          ,db_classification/1
%%         ]).

-export([db_classification/1
        ]).

-include("kz_data.hrl").

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec db_classification(text()) -> db_classifications().
db_classification(Db) when not is_binary(Db) ->
    db_classification(wh_util:to_binary(Db));
db_classification(<<"ts">>) -> 'depreciated';
db_classification(<<"crossbar_schemas">>) -> 'deprecated';
db_classification(<<"registrations">>) -> 'deprecated';
db_classification(<<"crossbar%2Fsessions">>) -> 'deprecated';
db_classification(<<"sms">>) -> 'deprecated';
db_classification(<<"signups">>) -> 'system'; %% Soon to be deprecated
db_classification(?WH_PROVISIONER_DB) -> 'system'; %% Soon to be deprecated
db_classification(?WH_ACCOUNTS_DB) -> 'aggregate';
db_classification(?KZ_TOKEN_DB) -> 'aggregate';
db_classification(?WH_SIP_DB) -> 'aggregate';
db_classification(?WH_FAXES_DB) -> 'aggregate';
db_classification(?KZ_ACDC_DB) -> 'aggregate';
db_classification(?WH_SERVICES_DB) -> 'aggregate';
db_classification(?KZ_PORT_REQUESTS_DB) -> 'aggregate';
db_classification(?KZ_WEBHOOKS_DB) -> 'aggregate';
db_classification(<<"numbers/", _/binary>>) -> 'numbers';
db_classification(<<"numbers%2F", _/binary>>) -> 'numbers';
db_classification(<<"numbers%2f", _/binary>>) -> 'numbers';
db_classification(?MATCH_MODB_SUFFIX_UNENCODED(_A,_B,_Rest,_Year,_Month)) -> 'modb';% these only need to match
db_classification(?MATCH_MODB_SUFFIX_ENCODED(_A,_B,_Rest,_Year,_Month)) -> 'modb';%   "account..." then the
db_classification(?MATCH_MODB_SUFFIX_encoded(_A,_B,_Rest,_Year,_Month)) -> 'modb';%   right size.
db_classification(?MATCH_ACCOUNT_UNENCODED(_AccountId)) -> 'account';
db_classification(?MATCH_ACCOUNT_encoded(_AccountId)) -> 'account';
db_classification(?MATCH_ACCOUNT_ENCODED(_AccountId)) -> 'account';
db_classification(?WH_RATES_DB) -> 'system';
db_classification(?WH_OFFNET_DB) -> 'system';
db_classification(?WH_ANONYMOUS_CDR_DB) -> 'system';
db_classification(?WH_DEDICATED_IP_DB) -> 'system';
db_classification(?WH_CONFIG_DB) -> 'system';
db_classification(?WH_MEDIA_DB) -> 'system';
db_classification(?WH_SCHEMA_DB) -> 'system';
db_classification(?KZ_OAUTH_DB) -> 'system';
db_classification(?KZ_DATA_DB) -> 'system';
db_classification(_Database) ->
    lager:debug("unknown type for database ~s", [_Database]),
    'undefined'.
