%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Account document
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_domains_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(DOMAIN, <<"2600hz.com">>).

-define(CNAME
       ,<<"{\"CNAME\":{\"portal.{{domain}}\":{\"name\":\"Web GUI\",\"mapping\":[\"ui.zswitch.net\"]},\"api.{{domain}}\":{\"name\":\"API\",\"mapping\":[\"api.zswitch.net\"]}}}">>
       ).

-define(FAIL_CNAME
       ,<<"{\"CNAME\":{\"portal.{{wrong}}\":{\"name\":\"Web GUI\",\"mapping\":[\"ui.zswitch.net\"]},\"api.{{still_wrong}}\":{\"name\":\"API\",\"mapping\":[\"api.zswitch.net\"]}}}">>
       ).

-define(A_RECORD
       ,<<"{\"A\":{\"us-east.{{domain}}\":{\"name\":\"Primary Proxy\",\"zone\": \"us-east\",\"mapping\":[\"127.0.0.1\"]},\"us-central.{{domain}}\":{\"name\":\"Secondary Proxy\",\"zone\": \"us-central\",\"mapping\":[\"127.0.0.1\"]},\"us-west.{{domain}}\":{\"name\":\"Tertiary Proxy\",\"zone\": \"us-west\",\"mapping\":[\"127.0.0.1\"]}}}">>
       ).

-define(NAPTR
       ,<<"{\"NAPTR\":{\"proxy-east.{{domain}}\":{\"name\":\"East NAPTR\",\"mapping\":[\"10 100 \\\"S\\\" \\\"SIP+D2U\\\" \\\"\\\" _sip._udp.proxy-east.{{domain}}.\"]},\"proxy-central.{{domain}}\":{\"name\":\"Central NAPTR\",\"mapping\":[\"10 100 \\\"S\\\" \\\"SIP+D2U\\\" \\\"\\\" _sip._udp.proxy-central.{{domain}}.\"]},\"proxy-west.{{domain}}\":{\"name\":\"West NAPTR\",\"mapping\":[\"10 100 \\\"S\\\" \\\"SIP+D2U\\\" \\\"\\\" _sip._udp.proxy-west.{{domain}}.\"]}}}">>
       ).

-define(SRV
       ,<<"{\"SRV\":{\"_sip._udp.proxy-east.{{domain}}\":{\"name\":\"East SRV\",\"mapping\":[\"10 10 7000 us-east.{{domain}}.\",\"15 15 7000 us-central.{{domain}}.\",\"20 20 7000 us-west.{{domain}}.\"]}}}">>
       ).

domains_test_() ->
    {'foreach'
    ,fun init/0
    ,fun stop/1
    ,[fun format_host/1
     ,fun cname/1
     ,fun fail_cname/1
     ,fun a_record/1
     ,fun naptr/1
     ,fun srv/1
     ,fun default/1
     ]
    }.

-define(DOMAINS_SCHEMA, <<"domains">>).
-define(HOSTS_SCHEMA, <<"domain_hosts">>).

-record(state, {domains
               ,domain_hosts
               ,loader_fun
               }).

init() ->
    DomainsSchema = crossbar_load(?DOMAINS_SCHEMA),
    DomainHostsSchema = crossbar_load(?HOSTS_SCHEMA),

    LoaderFun = fun A(?DOMAINS_SCHEMA) -> DomainsSchema;
                    A(?HOSTS_SCHEMA) -> DomainHostsSchema;
                    A(X) when not is_binary(X) -> A(kz_term:to_binary(X));
                    A(X) -> io:format("error: schema ~p not found", [X]),
                            'undefined'
                end,

    #state{domains=DomainsSchema
          ,domain_hosts=DomainHostsSchema
          ,loader_fun=LoaderFun
          }.

crossbar_load(Filename) ->
    File = <<Filename/binary,".json">>,
    kz_json:load_fixture_from_file(crossbar, "couchdb/schemas", File).

stop(_) -> 'ok'.

format_host(_) ->
    [{"Verify host replacement happens"
     ,?_assertEqual(<<"api.", (?DOMAIN)/binary>>
                   ,kzd_domains:format_host(<<"api.{{domain}}">>
                                           ,?DOMAIN
                                           )
                   )
     }
    ].

cname(#state{domains=DomainsSchema
            ,loader_fun=LoaderFun
            }
     ) ->
    CNAME = kz_json:decode(?CNAME),

    Hosts = kzd_domains:cname_hosts(CNAME),

    [{"Validate cname property in domains object"
     ,?_assertEqual({'ok', CNAME}
                   ,kz_json_schema:validate(DomainsSchema
                                           ,CNAME
                                           ,[{'schema_loader_fun', LoaderFun}]
                                           )
                   )
     }
    ,{"Validate list of hosts"
     ,?_assertEqual([<<"portal.{{domain}}">>
                    ,<<"api.{{domain}}">>
                    ]
                   ,Hosts
                   )
     }
     | validate_cname_hosts(CNAME, Hosts)
    ].

validate_cname_hosts(CNAME, Hosts) ->
    lists:flatten(
      lists:map(fun(H) -> validate_cname_host(CNAME, H) end
               ,Hosts
               )
     ).

validate_cname_host(CNAME, Host) ->
    _HostMappings = kzd_domains:cname_host_mappings(CNAME, Host),
    WhitelabelHost = kzd_domains:format_host(Host, ?DOMAIN),

    [{"Verify CNAME whitelabel host"
     ,?_assert('nomatch' =/= binary:match(WhitelabelHost, ?DOMAIN))
     }
    ].

fail_cname(#state{domains=DomainsSchema
                 ,loader_fun=LoaderFun
                 }
          ) ->
    CNAME = kz_json:decode(?FAIL_CNAME),

    Hosts = kzd_domains:cname_hosts(CNAME),

    [{"Validate badly formed host fails domains validation"
     ,?_assertMatch({'error'
                    ,[{'data_invalid'
                      ,_
                      ,'no_extra_properties_allowed'
                      ,_
                      ,_
                      }
                     ,{'data_invalid'
                      ,_
                      ,'no_extra_properties_allowed'
                      ,_
                      ,_
                      }
                     ]
                    }
                   ,kz_json_schema:validate(DomainsSchema
                                           ,CNAME
                                           ,[{'schema_loader_fun', LoaderFun}]
                                           )
                   )
     }
    ,{"Validate list of hosts"
     ,?_assertEqual([<<"portal.{{wrong}}">>
                    ,<<"api.{{still_wrong}}">>
                    ]
                   ,Hosts
                   )
     }
    ].

a_record(#state{domains=DomainsSchema
               ,loader_fun=LoaderFun
               }) ->
    A_RECORD = kz_json:decode(?A_RECORD),

    Hosts = kzd_domains:a_record_hosts(A_RECORD),

    [{"Validate a_record property in domains object"
     ,?_assertEqual({'ok', A_RECORD}
                   ,kz_json_schema:validate(DomainsSchema
                                           ,A_RECORD
                                           ,[{'schema_loader_fun', LoaderFun}]
                                           )
                   )
     }
    ,{"Validate list of hosts"
     ,?_assertEqual([<<"us-east.{{domain}}">>
                    ,<<"us-central.{{domain}}">>
                    ,<<"us-west.{{domain}}">>
                    ]
                   ,Hosts
                   )
     }
     | validate_a_record_hosts(A_RECORD, Hosts)
    ].

validate_a_record_hosts(A_RECORD, Hosts) ->
    lists:flatten(
      lists:map(fun(H) -> validate_a_record_host(A_RECORD, H) end
               ,Hosts
               )
     ).

validate_a_record_host(A_RECORD, Host) ->
    _HostMappings = kzd_domains:a_record_host_mappings(A_RECORD, Host),
    WhitelabelHost = kzd_domains:format_host(Host, ?DOMAIN),

    [{"Verify whitelabel host"
     ,?_assert('nomatch' =/= binary:match(WhitelabelHost, ?DOMAIN))
     }
    ].

naptr(#state{domains=DomainsSchema
            ,loader_fun=LoaderFun
            }) ->
    NAPTR = kz_json:decode(?NAPTR),

    Hosts = kzd_domains:naptr_hosts(NAPTR),

    [{"Validate naptr property in domains object"
     ,?_assertEqual({'ok', NAPTR}
                   ,kz_json_schema:validate(DomainsSchema
                                           ,NAPTR
                                           ,[{'schema_loader_fun', LoaderFun}]
                                           )
                   )
     }
    ,{"Validate list of hosts"
     ,?_assertEqual([<<"proxy-east.{{domain}}">>
                    ,<<"proxy-central.{{domain}}">>
                    ,<<"proxy-west.{{domain}}">>
                    ]
                   ,Hosts
                   )
     }
     | validate_naptr_hosts(NAPTR, Hosts)
    ].

validate_naptr_hosts(NAPTR, Hosts) ->
    lists:flatten(
      lists:map(fun(H) -> validate_naptr_host(NAPTR, H) end
               ,Hosts
               )
     ).

validate_naptr_host(NAPTR, Host) ->
    WhitelabelHost = kzd_domains:format_host(Host, ?DOMAIN),

    [{"Verify naptr whitelabel host"
     ,?_assertEqual('true', 'nomatch' =/= binary:match(WhitelabelHost, ?DOMAIN))
     }
     | validate_naptr_host_mappings(NAPTR, Host)
    ].

validate_naptr_host_mappings(NAPTR, Host) ->
    [validate_mapping(Mapping, <<"naptr">>)
     || Mapping <- kzd_domains:naptr_host_mappings(NAPTR, Host)
    ].

validate_mapping(Mapping, Type) ->
    Formatted = kzd_domains:format_mapping(Mapping, ?DOMAIN),
    {label("Verify ~s whitelabel mapping '~s' -> '~s'", [Type, Mapping, Formatted])
    ,?_assertEqual('true', 'nomatch' =/= binary:match(Formatted, ?DOMAIN))
    }.

srv(#state{domains=DomainsSchema
          ,loader_fun=LoaderFun
          }) ->
    SRV = kz_json:decode(?SRV),

    Hosts = kzd_domains:srv_hosts(SRV),

    [{"Validate srv property in domains object"
     ,?_assertEqual({'ok', SRV}
                   ,kz_json_schema:validate(DomainsSchema
                                           ,SRV
                                           ,[{'schema_loader_fun', LoaderFun}]
                                           )
                   )
     }
    ,{"Validate list of SRV hosts"
     ,?_assertEqual([<<"_sip._udp.proxy-east.{{domain}}">>]
                   ,Hosts
                   )
     }
     | validate_srv_hosts(SRV, Hosts)
    ].

validate_srv_hosts(SRV, Hosts) ->
    lists:flatten(
      lists:map(fun(H) -> validate_srv_host(SRV, H) end
               ,Hosts
               )
     ).

validate_srv_host(SRV, Host) ->
    WhitelabelHost = kzd_domains:format_host(Host, ?DOMAIN),

    [{label("Verify srv whitelabel host '~s' -> '~s'", [Host, WhitelabelHost])
     ,?_assert('nomatch' =/= binary:match(WhitelabelHost, ?DOMAIN))
     }
     | validate_srv_host_mappings(SRV, Host)
    ].

validate_srv_host_mappings(SRV, Host) ->
    [validate_mapping(Mapping, <<"srv">>)
     || Mapping <- kzd_domains:srv_host_mappings(SRV, Host)
    ].

label(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

default(#state{domains=DomainsSchema
              ,loader_fun=LoaderFun
              }) ->
    Default = kzd_domains:default(),

    case kz_json_schema:validate(DomainsSchema
                                ,Default
                                ,[{'schema_loader_fun', LoaderFun}]
                                )
    of
        {'ok', _} ->
            [{"Fixture is valid", ?_assert('true')}];
        {'error', _E} ->
            ?debugFmt("schema error: ~p~n", [_E]),
            [{"Fixture is invalid", ?_assert('false')}]
    end.
