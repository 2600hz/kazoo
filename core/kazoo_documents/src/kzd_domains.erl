%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Domains document for white-label
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_domains).

-export([new/0
	,default/0
	,save/1, save/2

	,cname/1, cname/2
	,cname_hosts/1, cname_host/2, cname_host/3
	,cname_host_mappings/2, cname_host_mappings/3
	,set_cname/2, add_cname_host/3

	,a_record/1, a_record/2
	,a_record_hosts/1, a_record_host/2, a_record_host/3
	,a_record_host_mappings/2, a_record_host_mappings/3
	,set_a_record/2, add_a_record_host/3

	,naptr/1, naptr/2
	,naptr_hosts/1, naptr_host/2, naptr_host/3
	,naptr_host_mappings/2, naptr_host_mappings/3
	,set_naptr/2, add_naptr_host/3

	,srv/1, srv/2
	,srv_hosts/1, srv_host/2, srv_host/3
	,srv_host_mappings/2, srv_host_mappings/3
	,set_srv/2, add_srv_host/3

	,format/2
	,format_host/2
	,format_mapping/2

	,mappings/1
	,name/1
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(KEY_CNAME, <<"CNAME">>).
-define(KEY_A_RECORD, <<"A">>).
-define(KEY_NAPTR, <<"NAPTR">>).
-define(KEY_SRV, <<"SRV">>).
-define(KEY_MX, <<"MX">>).
-define(KEY_TXT, <<"TXT">>).

-define(KEY_MAPPINGS, <<"mapping">>).
-define(KEY_NAME, <<"name">>).

-define(DOMAIN_PLACEHOLDER, <<"{{domain}}">>).

-spec new() -> doc().
new() ->
    kz_json:from_list([{?KEY_CNAME, kz_json:new()}
		      ,{?KEY_A_RECORD, kz_json:new()}
		      ,{?KEY_NAPTR, kz_json:new()}
		      ,{?KEY_SRV, kz_json:new()}
		      ,{?KEY_MX, kz_json:new()}
		      ,{?KEY_TXT, kz_json:new()}
                      ]).

-spec default() -> kz_json:object().
default() ->
    PrivDir = code:priv_dir('kazoo_documents'),
    {'ok', FixtureJSON} = file:read_file(
                            filename:join([PrivDir, "fixtures", "domains.json"])
                           ),
    kz_json:decode(FixtureJSON).

-spec cname(doc()) -> api_object().
-spec cname(doc(), Default) -> kz_json:object() | Default.
cname(Domains) ->
    cname(Domains, 'undefined').
cname(Domains, Default) ->
    kz_json:get_json_value(?KEY_CNAME, Domains, Default).

-spec cname_hosts(doc()) -> ne_binaries().
cname_hosts(Domains) ->
    kz_json:get_keys(?KEY_CNAME, Domains).

-spec cname_host(doc(), ne_binary()) -> api_object().
-spec cname_host(doc(), ne_binary(), Default) -> kz_json:object() | Default.
cname_host(Domains, Host) ->
    cname_host(Domains, Host, 'undefined').
cname_host(Domains, Host, Default) ->
    kz_json:get_value([?KEY_CNAME, Host], Domains, Default).

-spec cname_host_mappings(doc(), ne_binary()) -> ne_binaries().
-spec cname_host_mappings(doc(), ne_binary(), Default) -> ne_binaries() | Default.
cname_host_mappings(Domains, Host) ->
    cname_host_mappings(Domains, Host, []).

cname_host_mappings(Domains, Host, Default) ->
    kz_json:get_value([?KEY_CNAME, Host, ?KEY_MAPPINGS], Domains, Default).

-spec set_cname(doc(), kz_json:object()) -> doc().
set_cname(Domains, CNAME) ->
    kz_json:set_value(?KEY_CNAME, CNAME, Domains).

-spec add_cname_host(doc(), ne_binary(), kz_json:object()) -> doc().
add_cname_host(Domains, Host, Settings) ->
    kz_json:set_value([?KEY_CNAME, Host], Settings, Domains).

-spec a_record(doc()) -> api_object().
-spec a_record(doc(), Default) -> kz_json:object() | Default.
a_record(Domains) ->
    a_record(Domains, 'undefined').
a_record(Domains, Default) ->
    kz_json:get_json_value(?KEY_A_RECORD, Domains, Default).

-spec a_record_hosts(doc()) -> ne_binaries().
a_record_hosts(Domains) ->
    kz_json:get_keys(?KEY_A_RECORD, Domains).

-spec a_record_host(doc(), ne_binary()) -> api_object().
-spec a_record_host(doc(), ne_binary(), Default) -> kz_json:object() | Default.
a_record_host(Domains, Host) ->
    a_record_host(Domains, Host, 'undefined').
a_record_host(Domains, Host, Default) ->
    kz_json:get_value([?KEY_A_RECORD, Host], Domains, Default).

-spec a_record_host_mappings(doc(), ne_binary()) -> ne_binaries().
-spec a_record_host_mappings(doc(), ne_binary(), Default) -> ne_binaries() | Default.
a_record_host_mappings(Domains, Host) ->
    a_record_host_mappings(Domains, Host, []).

a_record_host_mappings(Domains, Host, Default) ->
    kz_json:get_value([?KEY_A_RECORD, Host, ?KEY_MAPPINGS], Domains, Default).

-spec set_a_record(doc(), kz_json:object()) -> doc().
set_a_record(Domains, A_RECORD) ->
    kz_json:set_value(?KEY_A_RECORD, A_RECORD, Domains).

-spec add_a_record_host(doc(), ne_binary(), kz_json:object()) -> doc().
add_a_record_host(Domains, Host, Settings) ->
    kz_json:set_value([?KEY_A_RECORD, Host], Settings, Domains).

-spec naptr(doc()) -> api_object().
-spec naptr(doc(), Default) -> kz_json:object() | Default.
naptr(Domains) ->
    naptr(Domains, 'undefined').
naptr(Domains, Default) ->
    kz_json:get_json_value(?KEY_NAPTR, Domains, Default).

-spec naptr_hosts(doc()) -> ne_binaries().
naptr_hosts(Domains) ->
    kz_json:get_keys(?KEY_NAPTR, Domains).

-spec naptr_host(doc(), ne_binary()) -> api_object().
-spec naptr_host(doc(), ne_binary(), Default) -> kz_json:object() | Default.
naptr_host(Domains, Host) ->
    naptr_host(Domains, Host, 'undefined').
naptr_host(Domains, Host, Default) ->
    kz_json:get_value([?KEY_NAPTR, Host], Domains, Default).

-spec naptr_host_mappings(doc(), ne_binary()) -> ne_binaries().
-spec naptr_host_mappings(doc(), ne_binary(), Default) -> ne_binaries() | Default.
naptr_host_mappings(Domains, Host) ->
    naptr_host_mappings(Domains, Host, []).

naptr_host_mappings(Domains, Host, Default) ->
    kz_json:get_value([?KEY_NAPTR, Host, ?KEY_MAPPINGS], Domains, Default).

-spec set_naptr(doc(), kz_json:object()) -> doc().
set_naptr(Domains, NAPTR) ->
    kz_json:set_value(?KEY_NAPTR, NAPTR, Domains).

-spec add_naptr_host(doc(), ne_binary(), kz_json:object()) -> doc().
add_naptr_host(Domains, Host, Settings) ->
    kz_json:set_value([?KEY_NAPTR, Host], Settings, Domains).

-spec srv(doc()) -> api_object().
-spec srv(doc(), Default) -> kz_json:object() | Default.
srv(Domains) ->
    srv(Domains, 'undefined').
srv(Domains, Default) ->
    kz_json:get_json_value(?KEY_SRV, Domains, Default).

-spec srv_hosts(doc()) -> ne_binaries().
srv_hosts(Domains) ->
    kz_json:get_keys(?KEY_SRV, Domains).

-spec srv_host(doc(), ne_binary()) -> api_object().
-spec srv_host(doc(), ne_binary(), Default) -> kz_json:object() | Default.
srv_host(Domains, Host) ->
    srv_host(Domains, Host, 'undefined').
srv_host(Domains, Host, Default) ->
    kz_json:get_value([?KEY_SRV, Host], Domains, Default).

-spec srv_host_mappings(doc(), ne_binary()) -> ne_binaries().
-spec srv_host_mappings(doc(), ne_binary(), Default) -> ne_binaries() | Default.
srv_host_mappings(Domains, Host) ->
    srv_host_mappings(Domains, Host, []).

srv_host_mappings(Domains, Host, Default) ->
    kz_json:get_value([?KEY_SRV, Host, ?KEY_MAPPINGS], Domains, Default).

-spec set_srv(doc(), kz_json:object()) -> doc().
set_srv(Domains, SRV) ->
    kz_json:set_value(?KEY_SRV, SRV, Domains).

-spec add_srv_host(doc(), ne_binary(), kz_json:object()) -> doc().
add_srv_host(Domains, Host, Settings) ->
    kz_json:set_value([?KEY_SRV, Host], Settings, Domains).

-spec save(doc()) -> {'ok', doc()} |
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

-spec try_save(doc(), api_object()) ->
                      {'ok', doc()} |
                      {'error', any()}.
try_save(Domains, PvtFields) ->
    case kapps_config:update_default(
           <<"whitelabel">>
				    ,<<"domains">>
				    ,Domains
				    ,[{'pvt_fields', PvtFields}]
          )
    of
        {'error', _E}=E -> E;
        _ ->
            {'ok', kapps_config:get(<<"whitelabel">>, <<"domains">>)}
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

-spec format(doc(), ne_binary()) -> doc().
format(Domains, WhitelabelDomain) ->
    kz_json:map(fun(DomainType, DomainConfig) ->
                        format_domain_type(DomainType, DomainConfig, WhitelabelDomain)
                end
	       ,Domains
               ).

-spec format_domain_type(ne_binary(), kz_json:object(), ne_binary()) ->
				{ne_binary(), kz_json:object()}.
format_domain_type(DomainType, DomainConfig, WhitelabelDomain) ->
    {DomainType
    ,kz_json:map(fun(Host, HostConfig) ->
			 format_host_config(Host, HostConfig, WhitelabelDomain)
		 end
		,DomainConfig
		)
    }.

-spec format_host_config(ne_binary(), kz_json:object(), ne_binary()) ->
                                {ne_binary(), kz_json:object()}.
format_host_config(Host, HostConfig, WhitelabelDomain) ->
    {format_host(Host, WhitelabelDomain)
    ,format_host_mappings(HostConfig, WhitelabelDomain)
    }.

-spec format_host_mappings(kz_json:object(), ne_binary()) ->
                                  kz_json:object().
format_host_mappings(HostConfig, WhitelabelDomain) ->
    Mappings =
        [format_mapping(Mapping, WhitelabelDomain)
         || Mapping <- kz_json:get_value(?KEY_MAPPINGS, HostConfig, [])
        ],
    kz_json:set_value(?KEY_MAPPINGS, Mappings, HostConfig).

-spec format_host(ne_binary(), ne_binary()) -> ne_binary().
format_host(DomainHost, WhitelabelDomain) ->
    binary:replace(DomainHost, ?DOMAIN_PLACEHOLDER, WhitelabelDomain).

-spec format_mapping(ne_binary(), ne_binary()) -> ne_binary().
format_mapping(Mapping, WhitelabelDomain) ->
    binary:replace(Mapping, ?DOMAIN_PLACEHOLDER, WhitelabelDomain).

-spec mappings(kz_json:object()) -> ne_binaries().
mappings(JObj) ->
    kz_json:get_value(?KEY_MAPPINGS, JObj).

-spec name(kz_json:object()) -> api_binary().
name(JObj) ->
    kz_json:get_value(?KEY_NAME, JObj).
