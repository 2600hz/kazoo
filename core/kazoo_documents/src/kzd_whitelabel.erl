-module(kzd_whitelabel).

-export([new/0]).
-export([company_name/1, company_name/2, set_company_name/2]).
-export([domain/1, domain/2, set_domain/2]).
-export([fake_api_url/1, fake_api_url/2, set_fake_api_url/2]).
-export([hide_credits/1, hide_credits/2, set_hide_credits/2]).
-export([hide_powered/1, hide_powered/2, set_hide_powered/2]).
-export([hide_registration/1, hide_registration/2, set_hide_registration/2]).
-export([inbound_trunks_price/1, inbound_trunks_price/2, set_inbound_trunks_price/2]).
-export([nav/1, nav/2, set_nav/2]).
-export([outbound_trunks_price/1, outbound_trunks_price/2, set_outbound_trunks_price/2]).
-export([port/1, port/2, set_port/2]).
-export([twoway_trunks_price/1, twoway_trunks_price/2, set_twoway_trunks_price/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec company_name(doc()) -> api_binary().
-spec company_name(doc(), Default) -> binary() | Default.
company_name(Doc) ->
    company_name(Doc, 'undefined').
company_name(Doc, Default) ->
    kz_json:get_binary_value(<<"company_name">>, Doc, Default).

-spec set_company_name(doc(), binary()) -> doc().
set_company_name(Doc, CompanyName) ->
    kz_json:set_value(<<"company_name">>, CompanyName, Doc).

-spec domain(doc()) -> api_binary().
-spec domain(doc(), Default) -> binary() | Default.
domain(Doc) ->
    domain(Doc, 'undefined').
domain(Doc, Default) ->
    kz_json:get_binary_value(<<"domain">>, Doc, Default).

-spec set_domain(doc(), binary()) -> doc().
set_domain(Doc, Domain) ->
    kz_json:set_value(<<"domain">>, Domain, Doc).

-spec fake_api_url(doc()) -> api_binary().
-spec fake_api_url(doc(), Default) -> binary() | Default.
fake_api_url(Doc) ->
    fake_api_url(Doc, 'undefined').
fake_api_url(Doc, Default) ->
    kz_json:get_binary_value(<<"fake_api_url">>, Doc, Default).

-spec set_fake_api_url(doc(), binary()) -> doc().
set_fake_api_url(Doc, FakeAPIUrl) ->
    kz_json:set_value(<<"fake_api_url">>, FakeAPIUrl, Doc).

-spec hide_credits(doc()) -> boolean().
-spec hide_credits(doc(), Default) -> boolean() | Default.
hide_credits(Doc) ->
    hide_credits(Doc, false).
hide_credits(Doc, Default) ->
    kz_json:get_boolean_value(<<"hide_credits">>, Doc, Default).

-spec set_hide_credits(doc(), boolean()) -> doc().
set_hide_credits(Doc, HideCredits) ->
    kz_json:set_value(<<"hide_credits">>, HideCredits, Doc).

-spec hide_powered(doc()) -> boolean().
-spec hide_powered(doc(), Default) -> boolean() | Default.
hide_powered(Doc) ->
    hide_powered(Doc, false).
hide_powered(Doc, Default) ->
    kz_json:get_boolean_value(<<"hide_powered">>, Doc, Default).

-spec set_hide_powered(doc(), boolean()) -> doc().
set_hide_powered(Doc, HidePowered) ->
    kz_json:set_value(<<"hide_powered">>, HidePowered, Doc).

-spec hide_registration(doc()) -> boolean().
-spec hide_registration(doc(), Default) -> boolean() | Default.
hide_registration(Doc) ->
    hide_registration(Doc, false).
hide_registration(Doc, Default) ->
    kz_json:get_boolean_value(<<"hide_registration">>, Doc, Default).

-spec set_hide_registration(doc(), boolean()) -> doc().
set_hide_registration(Doc, HideRegistration) ->
    kz_json:set_value(<<"hide_registration">>, HideRegistration, Doc).

-spec inbound_trunks_price(doc()) -> api_binary().
-spec inbound_trunks_price(doc(), Default) -> binary() | Default.
inbound_trunks_price(Doc) ->
    inbound_trunks_price(Doc, 'undefined').
inbound_trunks_price(Doc, Default) ->
    kz_json:get_binary_value(<<"inbound_trunks_price">>, Doc, Default).

-spec set_inbound_trunks_price(doc(), binary()) -> doc().
set_inbound_trunks_price(Doc, InboundTrunksPrice) ->
    kz_json:set_value(<<"inbound_trunks_price">>, InboundTrunksPrice, Doc).

-spec nav(doc()) -> api_object().
-spec nav(doc(), Default) -> kz_json:object() | Default.
nav(Doc) ->
    nav(Doc, 'undefined').
nav(Doc, Default) ->
    kz_json:get_json_value(<<"nav">>, Doc, Default).

-spec set_nav(doc(), kz_json:object()) -> doc().
set_nav(Doc, Nav) ->
    kz_json:set_value(<<"nav">>, Nav, Doc).

-spec outbound_trunks_price(doc()) -> api_binary().
-spec outbound_trunks_price(doc(), Default) -> binary() | Default.
outbound_trunks_price(Doc) ->
    outbound_trunks_price(Doc, 'undefined').
outbound_trunks_price(Doc, Default) ->
    kz_json:get_binary_value(<<"outbound_trunks_price">>, Doc, Default).

-spec set_outbound_trunks_price(doc(), binary()) -> doc().
set_outbound_trunks_price(Doc, OutboundTrunksPrice) ->
    kz_json:set_value(<<"outbound_trunks_price">>, OutboundTrunksPrice, Doc).

-spec port(doc()) -> api_object().
-spec port(doc(), Default) -> kz_json:object() | Default.
port(Doc) ->
    port(Doc, 'undefined').
port(Doc, Default) ->
    kz_json:get_json_value(<<"port">>, Doc, Default).

-spec set_port(doc(), kz_json:object()) -> doc().
set_port(Doc, Port) ->
    kz_json:set_value(<<"port">>, Port, Doc).

-spec twoway_trunks_price(doc()) -> api_binary().
-spec twoway_trunks_price(doc(), Default) -> binary() | Default.
twoway_trunks_price(Doc) ->
    twoway_trunks_price(Doc, 'undefined').
twoway_trunks_price(Doc, Default) ->
    kz_json:get_binary_value(<<"twoway_trunks_price">>, Doc, Default).

-spec set_twoway_trunks_price(doc(), binary()) -> doc().
set_twoway_trunks_price(Doc, TwowayTrunksPrice) ->
    kz_json:set_value(<<"twoway_trunks_price">>, TwowayTrunksPrice, Doc).
