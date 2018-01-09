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
-export([nav_help/1, nav_help/2, set_nav_help/2]).
-export([nav_learn_more/1, nav_learn_more/2, set_nav_learn_more/2]).
-export([outbound_trunks_price/1, outbound_trunks_price/2, set_outbound_trunks_price/2]).
-export([port/1, port/2, set_port/2]).
-export([port_features/1, port_features/2, set_port_features/2]).
-export([hide_port/1, hide_port/2, set_hide_port/2]).
-export([port_loa/1, port_loa/2, set_port_loa/2]).
-export([port_resporg/1, port_resporg/2, set_port_resporg/2]).
-export([port_support_email/1, port_support_email/2, set_port_support_email/2]).
-export([port_terms/1, port_terms/2, set_port_terms/2]).
-export([twoway_trunks_price/1, twoway_trunks_price/2, set_twoway_trunks_price/2]).

-export([fetch/1]).

-include("kz_documents.hrl").

-define(ID, <<"whitelabel">>).

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
    kz_json:get_binary_value([<<"company_name">>], Doc, Default).

-spec set_company_name(doc(), binary()) -> doc().
set_company_name(Doc, CompanyName) ->
    kz_json:set_value([<<"company_name">>], CompanyName, Doc).

-spec domain(doc()) -> api_binary().
-spec domain(doc(), Default) -> binary() | Default.
domain(Doc) ->
    domain(Doc, 'undefined').
domain(Doc, Default) ->
    kz_json:get_binary_value([<<"domain">>], Doc, Default).

-spec set_domain(doc(), binary()) -> doc().
set_domain(Doc, Domain) ->
    kz_json:set_value([<<"domain">>], Domain, Doc).

-spec fake_api_url(doc()) -> api_binary().
-spec fake_api_url(doc(), Default) -> binary() | Default.
fake_api_url(Doc) ->
    fake_api_url(Doc, 'undefined').
fake_api_url(Doc, Default) ->
    kz_json:get_binary_value([<<"fake_api_url">>], Doc, Default).

-spec set_fake_api_url(doc(), binary()) -> doc().
set_fake_api_url(Doc, FakeAPIUrl) ->
    kz_json:set_value([<<"fake_api_url">>], FakeAPIUrl, Doc).

-spec hide_credits(doc()) -> boolean().
-spec hide_credits(doc(), Default) -> boolean() | Default.
hide_credits(Doc) ->
    hide_credits(Doc, false).
hide_credits(Doc, Default) ->
    kz_json:get_boolean_value([<<"hide_credits">>], Doc, Default).

-spec set_hide_credits(doc(), boolean()) -> doc().
set_hide_credits(Doc, HideCredits) ->
    kz_json:set_value([<<"hide_credits">>], HideCredits, Doc).

-spec hide_powered(doc()) -> boolean().
-spec hide_powered(doc(), Default) -> boolean() | Default.
hide_powered(Doc) ->
    hide_powered(Doc, false).
hide_powered(Doc, Default) ->
    kz_json:get_boolean_value([<<"hide_powered">>], Doc, Default).

-spec set_hide_powered(doc(), boolean()) -> doc().
set_hide_powered(Doc, HidePowered) ->
    kz_json:set_value([<<"hide_powered">>], HidePowered, Doc).

-spec hide_registration(doc()) -> boolean().
-spec hide_registration(doc(), Default) -> boolean() | Default.
hide_registration(Doc) ->
    hide_registration(Doc, false).
hide_registration(Doc, Default) ->
    kz_json:get_boolean_value([<<"hide_registration">>], Doc, Default).

-spec set_hide_registration(doc(), boolean()) -> doc().
set_hide_registration(Doc, HideRegistration) ->
    kz_json:set_value([<<"hide_registration">>], HideRegistration, Doc).

-spec inbound_trunks_price(doc()) -> api_binary().
-spec inbound_trunks_price(doc(), Default) -> binary() | Default.
inbound_trunks_price(Doc) ->
    inbound_trunks_price(Doc, 'undefined').
inbound_trunks_price(Doc, Default) ->
    kz_json:get_binary_value([<<"inbound_trunks_price">>], Doc, Default).

-spec set_inbound_trunks_price(doc(), binary()) -> doc().
set_inbound_trunks_price(Doc, InboundTrunksPrice) ->
    kz_json:set_value([<<"inbound_trunks_price">>], InboundTrunksPrice, Doc).

-spec nav(doc()) -> api_object().
-spec nav(doc(), Default) -> kz_json:object() | Default.
nav(Doc) ->
    nav(Doc, 'undefined').
nav(Doc, Default) ->
    kz_json:get_json_value([<<"nav">>], Doc, Default).

-spec set_nav(doc(), kz_json:object()) -> doc().
set_nav(Doc, Nav) ->
    kz_json:set_value([<<"nav">>], Nav, Doc).

-spec nav_help(doc()) -> api_binary().
-spec nav_help(doc(), Default) -> binary() | Default.
nav_help(Doc) ->
    nav_help(Doc, 'undefined').
nav_help(Doc, Default) ->
    kz_json:get_binary_value([<<"nav">>, <<"help">>], Doc, Default).

-spec set_nav_help(doc(), binary()) -> doc().
set_nav_help(Doc, NavHelp) ->
    kz_json:set_value([<<"nav">>, <<"help">>], NavHelp, Doc).

-spec nav_learn_more(doc()) -> api_binary().
-spec nav_learn_more(doc(), Default) -> binary() | Default.
nav_learn_more(Doc) ->
    nav_learn_more(Doc, 'undefined').
nav_learn_more(Doc, Default) ->
    kz_json:get_binary_value([<<"nav">>, <<"learn_more">>], Doc, Default).

-spec set_nav_learn_more(doc(), binary()) -> doc().
set_nav_learn_more(Doc, NavLearnMore) ->
    kz_json:set_value([<<"nav">>, <<"learn_more">>], NavLearnMore, Doc).

-spec outbound_trunks_price(doc()) -> api_binary().
-spec outbound_trunks_price(doc(), Default) -> binary() | Default.
outbound_trunks_price(Doc) ->
    outbound_trunks_price(Doc, 'undefined').
outbound_trunks_price(Doc, Default) ->
    kz_json:get_binary_value([<<"outbound_trunks_price">>], Doc, Default).

-spec set_outbound_trunks_price(doc(), binary()) -> doc().
set_outbound_trunks_price(Doc, OutboundTrunksPrice) ->
    kz_json:set_value([<<"outbound_trunks_price">>], OutboundTrunksPrice, Doc).

-spec port(doc()) -> api_object().
-spec port(doc(), Default) -> kz_json:object() | Default.
port(Doc) ->
    port(Doc, 'undefined').
port(Doc, Default) ->
    kz_json:get_json_value([<<"port">>], Doc, Default).

-spec set_port(doc(), kz_json:object()) -> doc().
set_port(Doc, Port) ->
    kz_json:set_value([<<"port">>], Port, Doc).

-spec port_features(doc()) -> api_binary().
-spec port_features(doc(), Default) -> binary() | Default.
port_features(Doc) ->
    port_features(Doc, 'undefined').
port_features(Doc, Default) ->
    kz_json:get_binary_value([<<"port">>, <<"features">>], Doc, Default).

-spec set_port_features(doc(), binary()) -> doc().
set_port_features(Doc, PortFeatures) ->
    kz_json:set_value([<<"port">>, <<"features">>], PortFeatures, Doc).

-spec hide_port(doc()) -> boolean().
-spec hide_port(doc(), Default) -> boolean() | Default.
hide_port(Doc) ->
    hide_port(Doc, 'false').
hide_port(Doc, Default) ->
    kz_json:is_true(<<"hide_port">>, Doc, Default).

-spec set_hide_port(doc(), boolean()) -> doc().
set_hide_port(Doc, Hide) ->
    kz_json:set_value(<<"hide_port">>, Hide, Doc).

-spec port_loa(doc()) -> api_binary().
-spec port_loa(doc(), Default) -> binary() | Default.
port_loa(Doc) ->
    port_loa(Doc, 'undefined').
port_loa(Doc, Default) ->
    kz_json:get_binary_value([<<"port">>, <<"loa">>], Doc, Default).

-spec set_port_loa(doc(), binary()) -> doc().
set_port_loa(Doc, PortLoa) ->
    kz_json:set_value([<<"port">>, <<"loa">>], PortLoa, Doc).

-spec port_resporg(doc()) -> api_binary().
-spec port_resporg(doc(), Default) -> binary() | Default.
port_resporg(Doc) ->
    port_resporg(Doc, 'undefined').
port_resporg(Doc, Default) ->
    kz_json:get_binary_value([<<"port">>, <<"resporg">>], Doc, Default).

-spec set_port_resporg(doc(), binary()) -> doc().
set_port_resporg(Doc, PortResporg) ->
    kz_json:set_value([<<"port">>, <<"resporg">>], PortResporg, Doc).

-spec port_support_email(doc()) -> api_binary().
-spec port_support_email(doc(), Default) -> binary() | Default.
port_support_email(Doc) ->
    port_support_email(Doc, 'undefined').
port_support_email(Doc, Default) ->
    kz_json:get_binary_value([<<"port">>, <<"support_email">>], Doc, Default).

-spec set_port_support_email(doc(), binary()) -> doc().
set_port_support_email(Doc, PortSupportEmail) ->
    kz_json:set_value([<<"port">>, <<"support_email">>], PortSupportEmail, Doc).

-spec port_terms(doc()) -> api_binary().
-spec port_terms(doc(), Default) -> binary() | Default.
port_terms(Doc) ->
    port_terms(Doc, 'undefined').
port_terms(Doc, Default) ->
    kz_json:get_binary_value([<<"port">>, <<"terms">>], Doc, Default).

-spec set_port_terms(doc(), binary()) -> doc().
set_port_terms(Doc, PortTerms) ->
    kz_json:set_value([<<"port">>, <<"terms">>], PortTerms, Doc).

-spec twoway_trunks_price(doc()) -> api_binary().
-spec twoway_trunks_price(doc(), Default) -> binary() | Default.
twoway_trunks_price(Doc) ->
    twoway_trunks_price(Doc, 'undefined').
twoway_trunks_price(Doc, Default) ->
    kz_json:get_binary_value([<<"twoway_trunks_price">>], Doc, Default).

-spec set_twoway_trunks_price(doc(), binary()) -> doc().
set_twoway_trunks_price(Doc, TwowayTrunksPrice) ->
    kz_json:set_value([<<"twoway_trunks_price">>], TwowayTrunksPrice, Doc).

-spec fetch(api_binary()) ->
                   {'ok', kz_json:object()} |
                   {'error', any()}.
fetch('undefined') ->
    {'error', 'account_id_undefined'};
fetch(Account) ->
    AccoundDb = kz_util:format_account_db(Account),
    kz_datamgr:open_cache_doc(AccoundDb, ?ID).
