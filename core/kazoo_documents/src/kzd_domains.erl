%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_domains).

-export([new/0
        ,default/0
        ,format/2
        ,format_host/2
        ,format_mapping/2
        ,mappings/1
        ,name/1
        ,save/1, save/2
        ]).

-export([a/1, a/2, set_a/2]).
-export([cname/1, cname/2, set_cname/2]).
-export([mx/1, mx/2, set_mx/2]).
-export([naptr/1, naptr/2, set_naptr/2]).
-export([srv/1, srv/2, set_srv/2]).
-export([txt/1, txt/2, set_txt/2]).

-export([cname_host_mappings/2, cname_host_mappings/3
        ,mx_host_mappings/2, mx_host_mappings/3
        ,a_record_host_mappings/2, a_record_host_mappings/3
        ,naptr_host_mappings/2, naptr_host_mappings/3
        ,srv_host_mappings/2, srv_host_mappings/3
        ,txt_host_mappings/2, txt_host_mappings/3

        ,cname_hosts/1
        ,mx_hosts/1
        ,a_record_hosts/1
        ,naptr_hosts/1
        ,srv_hosts/1
        ,txt_hosts/1
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"domains">>).
-define(KEY_MAPPINGS, <<"mapping">>).
-define(DOMAIN_PLACEHOLDER, <<"{{domain}}">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec default() -> kz_json:object().
default() ->
    kz_json:load_fixture_from_file(?APP, "fixtures", "domains.json").

-spec save(doc()) -> {'ok', doc()} |
          {'error', any()}.
save(Domains) ->
    save(Domains, 'undefined').

-spec save(doc(), kz_term:api_object()) -> {'ok', doc()} |
          {'error', any()}.
save(Domains, PvtFields) ->
    case is_valid(Domains) of
        'true' ->
            try_save(Domains, PvtFields);
        {'false', Errors} ->
            {'error', Errors}
    end.

-spec try_save(doc(), kz_term:api_object()) -> {'ok', doc()} |
          {'error', any()}.
try_save(Domains, PvtFields) ->
    case kapps_config:update_default(<<"whitelabel">>
                                    ,<<"domains">>
                                    ,Domains
                                    ,[{'pvt_fields', PvtFields}]
                                    )
    of
        {'error', _E}=E -> E;
        _ ->
            {'ok', kapps_config:get_json(<<"whitelabel">>, <<"domains">>)}
    end.

-spec is_valid(doc()) ->
          'true' | {'false', _}.
is_valid(Domains) ->
    is_valid(Domains, kz_json_schema:load(<<"domains">>)).

-spec is_valid(doc(), {'ok', kz_json:object()} | {'error', any()}) ->
          'true' | {'false', any()}.
is_valid(_Domains, {'error', E}) ->
    lager:warning("failed to find domains JSON schema"),
    {'false', E};
is_valid(Domains, {'ok', SchemaJObj}) ->
    case kz_json_schema:validate(SchemaJObj
                                ,Domains
                                )
    of
        {'ok', _JObj} -> 'true';
        {'error', Errors} ->
            lager:debug("failed to validate the domains document"),
            {'false', Errors}
    end.

-spec a(doc()) -> kz_term:api_object().
a(Doc) ->
    a(Doc, 'undefined').

-spec a(doc(), Default) -> kz_json:object() | Default.
a(Doc, Default) ->
    kz_json:get_json_value([<<"A">>], Doc, Default).

-spec set_a(doc(), kz_json:object()) -> doc().
set_a(Doc, A) ->
    kz_json:set_value([<<"A">>], A, Doc).

-spec a_record_hosts(doc()) -> kz_term:ne_binaries().
a_record_hosts(Domains) ->
    kz_json:get_keys(a(Domains)).

-spec a_record_host_mappings(doc(), kz_term:ne_binary()) -> kz_term:ne_binaries().
a_record_host_mappings(Domains, Host) ->
    a_record_host_mappings(Domains, Host, []).

-spec a_record_host_mappings(doc(), kz_term:ne_binary(), Default) -> kz_term:ne_binaries() | Default.
a_record_host_mappings(Domains, Host, Default) ->
    host_mappings(a(Domains), Host, Default).

-spec cname(doc()) -> kz_term:api_object().
cname(Doc) ->
    cname(Doc, 'undefined').

-spec cname(doc(), Default) -> kz_json:object() | Default.
cname(Doc, Default) ->
    kz_json:get_json_value([<<"CNAME">>], Doc, Default).

-spec set_cname(doc(), kz_json:object()) -> doc().
set_cname(Doc, Cname) ->
    kz_json:set_value([<<"CNAME">>], Cname, Doc).

-spec cname_hosts(doc()) -> kz_term:ne_binaries().
cname_hosts(Domains) ->
    kz_json:get_keys(cname(Domains)).

-spec cname_host_mappings(doc(), kz_term:ne_binary()) -> kz_term:ne_binaries().
cname_host_mappings(Domains, Host) ->
    cname_host_mappings(Domains, Host, []).

-spec cname_host_mappings(doc(), kz_term:ne_binary(), Default) -> kz_term:ne_binaries() | Default.
cname_host_mappings(Domains, Host, Default) ->
    host_mappings(cname(Domains), Host, Default).

-spec mx(doc()) -> kz_term:api_object().
mx(Doc) ->
    mx(Doc, 'undefined').

-spec mx(doc(), Default) -> kz_json:object() | Default.
mx(Doc, Default) ->
    kz_json:get_json_value([<<"MX">>], Doc, Default).

-spec set_mx(doc(), kz_json:object()) -> doc().
set_mx(Doc, Mx) ->
    kz_json:set_value([<<"MX">>], Mx, Doc).

-spec mx_hosts(doc()) -> kz_term:ne_binaries().
mx_hosts(Domains) ->
    kz_json:get_keys(mx(Domains)).

-spec mx_host_mappings(doc(), kz_term:ne_binary()) -> kz_term:ne_binaries().
mx_host_mappings(Domains, Host) ->
    mx_host_mappings(Domains, Host, []).

-spec mx_host_mappings(doc(), kz_term:ne_binary(), Default) -> kz_term:ne_binaries() | Default.
mx_host_mappings(Domains, Host, Default) ->
    host_mappings(mx(Domains), Host, Default).

-spec naptr(doc()) -> kz_term:api_object().
naptr(Doc) ->
    naptr(Doc, 'undefined').

-spec naptr(doc(), Default) -> kz_json:object() | Default.
naptr(Doc, Default) ->
    kz_json:get_json_value([<<"NAPTR">>], Doc, Default).

-spec set_naptr(doc(), kz_json:object()) -> doc().
set_naptr(Doc, Naptr) ->
    kz_json:set_value([<<"NAPTR">>], Naptr, Doc).

-spec naptr_hosts(doc()) -> kz_term:ne_binaries().
naptr_hosts(Domains) ->
    kz_json:get_keys(naptr(Domains)).

-spec naptr_host_mappings(doc(), kz_term:ne_binary()) -> kz_term:ne_binaries().
naptr_host_mappings(Domains, Host) ->
    naptr_host_mappings(Domains, Host, []).

-spec naptr_host_mappings(doc(), kz_term:ne_binary(), Default) -> kz_term:ne_binaries() | Default.
naptr_host_mappings(Domains, Host, Default) ->
    host_mappings(naptr(Domains), Host, Default).

-spec srv(doc()) -> kz_term:api_object().
srv(Doc) ->
    srv(Doc, 'undefined').

-spec srv(doc(), Default) -> kz_json:object() | Default.
srv(Doc, Default) ->
    kz_json:get_json_value([<<"SRV">>], Doc, Default).

-spec set_srv(doc(), kz_json:object()) -> doc().
set_srv(Doc, Srv) ->
    kz_json:set_value([<<"SRV">>], Srv, Doc).

-spec srv_hosts(doc()) -> kz_term:ne_binaries().
srv_hosts(Domains) ->
    kz_json:get_keys(srv(Domains)).

-spec srv_host_mappings(doc(), kz_term:ne_binary()) -> kz_term:ne_binaries().
srv_host_mappings(Domains, Host) ->
    srv_host_mappings(Domains, Host, []).

-spec srv_host_mappings(doc(), kz_term:ne_binary(), Default) -> kz_term:ne_binaries() | Default.
srv_host_mappings(Domains, Host, Default) ->
    host_mappings(srv(Domains), Host, Default).

-spec txt(doc()) -> kz_term:api_object().
txt(Doc) ->
    txt(Doc, 'undefined').

-spec txt(doc(), Default) -> kz_json:object() | Default.
txt(Doc, Default) ->
    kz_json:get_json_value([<<"TXT">>], Doc, Default).

-spec set_txt(doc(), kz_json:object()) -> doc().
set_txt(Doc, Txt) ->
    kz_json:set_value([<<"TXT">>], Txt, Doc).

-spec txt_hosts(doc()) -> kz_term:ne_binaries().
txt_hosts(Domains) ->
    kz_json:get_keys(txt(Domains)).

-spec txt_host_mappings(doc(), kz_term:ne_binary()) -> kz_term:ne_binaries().
txt_host_mappings(Domains, Host) ->
    txt_host_mappings(Domains, Host, []).

-spec txt_host_mappings(doc(), kz_term:ne_binary(), Default) -> kz_term:ne_binaries() | Default.
txt_host_mappings(Domains, Host, Default) ->
    host_mappings(txt(Domains), Host, Default).

-spec format(doc(), kz_term:ne_binary()) -> doc().
format(Domains, WhitelabelDomain) ->
    kz_json:map(fun(DomainType, DomainConfig) ->
                        format_domain_type(DomainType, DomainConfig, WhitelabelDomain)
                end
               ,Domains
               ).

-spec format_domain_type(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) ->
          {kz_term:ne_binary(), kz_json:object()}.
format_domain_type(DomainType, DomainConfig, WhitelabelDomain) ->
    {DomainType
    ,kz_json:map(fun(Host, HostConfig) ->
                         format_host_config(Host, HostConfig, WhitelabelDomain)
                 end
                ,DomainConfig
                )
    }.

-spec format_host_config(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) ->
          {kz_term:ne_binary(), kz_json:object()}.
format_host_config(Host, HostConfig, WhitelabelDomain) ->
    {format_host(Host, WhitelabelDomain)
    ,format_host_mappings(HostConfig, WhitelabelDomain)
    }.

-spec format_host_mappings(kz_json:object(), kz_term:ne_binary()) ->
          kz_json:object().
format_host_mappings(HostConfig, WhitelabelDomain) ->
    Mappings =
        [format_mapping(Mapping, WhitelabelDomain)
         || Mapping <- kz_json:get_value(?KEY_MAPPINGS, HostConfig, [])
        ],
    kz_json:set_value(?KEY_MAPPINGS, Mappings, HostConfig).

-spec format_host(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
format_host(DomainHost, WhitelabelDomain) ->
    binary:replace(DomainHost, ?DOMAIN_PLACEHOLDER, WhitelabelDomain).

-spec format_mapping(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
format_mapping(Mapping, WhitelabelDomain) ->
    binary:replace(Mapping, ?DOMAIN_PLACEHOLDER, WhitelabelDomain).

-spec mappings(kz_json:object()) -> kz_term:ne_binaries().
mappings(JObj) ->
    kz_json:get_value(?KEY_MAPPINGS, JObj).

-spec name(kz_json:object()) -> kz_term:api_binary().
name(JObj) ->
    kz_json:get_value(<<"name">>, JObj).

-spec host_mappings(kz_term:api_object(), kz_term:ne_binary(), Default) ->
          kz_term:ne_binaries() | Default.
host_mappings('undefined', _Host, Default) -> Default;
host_mappings(Domain, Host, Default) ->
    kz_json:get_value([Host, <<"mapping">>], Domain, Default).
