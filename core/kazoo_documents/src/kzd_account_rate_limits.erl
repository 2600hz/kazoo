-module(kzd_account_rate_limits).

-export([new/0]).
-export([account/1, account/2, set_account/2]).
-export([device/1, device/2, set_device/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec account(doc()) -> api_object().
-spec account(doc(), Default) -> kz_json:object() | Default.
account(Doc) ->
    account(Doc, 'undefined').
account(Doc, Default) ->
    kz_json:get_json_value([<<"account">>], Doc, Default).

-spec set_account(doc(), kz_json:object()) -> doc().
set_account(Doc, Account) ->
    kz_json:set_value([<<"account">>], Account, Doc).

-spec device(doc()) -> api_object().
-spec device(doc(), Default) -> kz_json:object() | Default.
device(Doc) ->
    device(Doc, 'undefined').
device(Doc, Default) ->
    kz_json:get_json_value([<<"device">>], Doc, Default).

-spec set_device(doc(), kz_json:object()) -> doc().
set_device(Doc, Device) ->
    kz_json:set_value([<<"device">>], Device, Doc).
