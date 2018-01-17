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
-export([cname/1, cname/2, set_cname/2
        ,cname_hosts/1
        ,cname_host_mappings/2, cname_host_mappings/3
        ]).
-export([mx/1, mx/2, set_mx/2]).
-export([naptr/1, naptr/2, set_naptr/2]).
-export([srv/1, srv/2, set_srv/2]).
-export([txt/1, txt/2, set_txt/2]).

-include("kz_documents.hrl").

-define(KEY_MAPPINGS, <<"mapping">>).
-define(DOMAIN_PLACEHOLDER, <<"{{domain}}">>).

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec default() -> kz_json:object().
default() ->
    kz_json:load_fixture_from_file(?APP, "fixtures", "domains.json").

-spec save(doc()) -> {'ok', doc()} |
                     {'error', any()}.
-spec save(doc(), kz_term:api_object()) -> {'ok', doc()} |
                                           {'error', any()}.
save(Domains) ->
    save(Domains, 'undefined').
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
-spec is_valid(doc(), {'ok', kz_json:object()} | {'error', any()}) ->
                      'true' | {'false', any()}.
is_valid(Domains) ->
    is_valid(Domains, kz_json_schema:load(<<"domains">>)).
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
-spec a(doc(), Default) -> kz_json:object() | Default.
a(Doc) ->
    a(Doc, 'undefined').
a(Doc, Default) ->
    kz_json:get_json_value([<<"A">>], Doc, Default).

-spec set_a(doc(), kz_json:object()) -> doc().
set_a(Doc, A) ->
    kz_json:set_value([<<"A">>], A, Doc).

-spec cname(doc()) -> kz_term:api_object().
-spec cname(doc(), Default) -> kz_json:object() | Default.
cname(Doc) ->
    cname(Doc, 'undefined').
cname(Doc, Default) ->
    kz_json:get_json_value([<<"CNAME">>], Doc, Default).

-spec set_cname(doc(), kz_json:object()) -> doc().
set_cname(Doc, Cname) ->
    kz_json:set_value([<<"CNAME">>], Cname, Doc).

-spec cname_hosts(doc()) -> kz_term:ne_binaries().
cname_hosts(Domains) ->
    kz_json:get_keys(<<"CNAME">>, Domains).

-spec cname_host_mappings(doc(), kz_term:ne_binary()) -> kz_term:ne_binaries().
-spec cname_host_mappings(doc(), kz_term:ne_binary(), Default) -> kz_term:ne_binaries() | Default.
cname_host_mappings(Domains, Host) ->
    cname_host_mappings(Domains, Host, []).

cname_host_mappings(Domains, Host, Default) ->
    kz_json:get_value([<<"CNAME">>, Host, <<"mappings">>], Domains, Default).

-spec mx(doc()) -> kz_term:api_object().
-spec mx(doc(), Default) -> kz_json:object() | Default.
mx(Doc) ->
    mx(Doc, 'undefined').
mx(Doc, Default) ->
    kz_json:get_json_value([<<"MX">>], Doc, Default).

-spec set_mx(doc(), kz_json:object()) -> doc().
set_mx(Doc, Mx) ->
    kz_json:set_value([<<"MX">>], Mx, Doc).

-spec naptr(doc()) -> kz_term:api_object().
-spec naptr(doc(), Default) -> kz_json:object() | Default.
naptr(Doc) ->
    naptr(Doc, 'undefined').
naptr(Doc, Default) ->
    kz_json:get_json_value([<<"NAPTR">>], Doc, Default).

-spec set_naptr(doc(), kz_json:object()) -> doc().
set_naptr(Doc, Naptr) ->
    kz_json:set_value([<<"NAPTR">>], Naptr, Doc).

-spec srv(doc()) -> kz_term:api_object().
-spec srv(doc(), Default) -> kz_json:object() | Default.
srv(Doc) ->
    srv(Doc, 'undefined').
srv(Doc, Default) ->
    kz_json:get_json_value([<<"SRV">>], Doc, Default).

-spec set_srv(doc(), kz_json:object()) -> doc().
set_srv(Doc, Srv) ->
    kz_json:set_value([<<"SRV">>], Srv, Doc).

-spec txt(doc()) -> kz_term:api_object().
-spec txt(doc(), Default) -> kz_json:object() | Default.
txt(Doc) ->
    txt(Doc, 'undefined').
txt(Doc, Default) ->
    kz_json:get_json_value([<<"TXT">>], Doc, Default).

-spec set_txt(doc(), kz_json:object()) -> doc().
set_txt(Doc, Txt) ->
    kz_json:set_value([<<"TXT">>], Txt, Doc).

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
