%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%% Domains document for white-label
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_domains).

-export([new/0
         ,save/1

         ,cnam/1, cnam/2
         ,cnam_host/2, cnam_host/3
         ,set_cnam/2, add_cnam_host/3
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

-define(KEY_CNAM, <<"CNAM">>).
-define(KEY_A, <<"A">>).
-define(KEY_NAPTR, <<"NAPTR">>).
-define(KEY_SRV, <<"SRV">>).
-define(KEY_MX, <<"MX">>).
-define(KEY_TXT, <<"TXT">>).

-spec new() -> doc().
new() ->
    wh_json:from_list([{?KEY_CNAM, wh_json:new()}
                       ,{?KEY_A, wh_json:new()}
                       ,{?KEY_NAPTR, wh_json:new()}
                       ,{?KEY_SRV, wh_json:new()}
                       ,{?KEY_MX, wh_json:new()}
                       ,{?KEY_TXT, wh_json:new()}
                      ]).

-spec cnam(doc()) -> api_object().
-spec cnam(doc(), Default) -> wh_json:object() | Default.
cnam(Domains) ->
    cnam(Domains, 'undefined').
cnam(Domains, Default) ->
    wh_json:get_json_value(Domains, Default).

-spec cnam_host(doc(), ne_binary()) -> api_object().
-spec cnam_host(doc(), ne_binary(), Default) -> wh_json:object() | Default.
cnam_host(Domains, Host) ->
    cnam_host(Domains, Host, 'undefined').
cnam_host(Domains, Host, Default) ->
    wh_json:get_value([?KEY_CNAM, Host], Domains, Default).

-spec set_cnam(doc(), wh_json:object()) -> doc().
set_cnam(Domains, CNAM) ->
    wh_json:set_value(?KEY_CNAM, CNAM, Domains).

-spec add_cnam_host(doc(), ne_binary(), wh_json:object()) -> doc().
add_cnam_host(Domains, Host, Settings) ->
    wh_json:set_value([?KEY_CNAM, Host], Settings, Domains).

-spec save(doc()) -> {'ok', doc()} |
                     {'error', _}.
save(Domains) ->
    case is_valid(Domains) of
        'true' ->
            try_save(Domains);
        {'false', Errors} ->
            {'error', Errors}
    end.

-spec try_save(doc()) -> doc() |
                         {'error', _}.
try_save(Domains) ->
    case whapps_config:set_default(<<"whitelabel">>, <<"domains">>, Domains) of
        {'error', _E}=E -> E;
        _ -> whapps_config:get(<<"whitelabel">>, <<"domains">>)
    end.

-spec is_valid(doc()) ->
                      'true' | {'false', _}.
-spec is_valid(doc(), {'ok', wh_json:object()} | {'error', _}) ->
                      'true' | {'false', _}.
is_valid(Domains) ->
    is_valid(Domains, wh_json_schema:load(<<"domains">>)).
is_valid(_Domains, {'error', E}) ->
    lager:warning("failed to find domains JSON schema"),
    {'false', E};
is_valid(Domains, {'ok', SchemaJObj}) ->
    case wh_json_schema:validate(SchemaJObj
                                 ,Domains
                                )
    of
        {'ok', _JObj} -> 'true';
        {'error', Errors} ->
            lager:debug("failed to validate the domains document"),
            {'false', Errors}
    end.

%% {
%%    "CNAM":{
%%       "portal.{{whitelabel_domain}}":{
%%          "name":"Web GUI",
%%          "mapping":[
%%             "ui.zswitch.net"
%%          ]
%%       },
%%       "api.{{whitelabel_domain}}":{
%%          "name":"API",
%%          "mapping":[
%%             "api.zswitch.net"
%%          ]
%%       }
%%    },
%%    "A":{
%%       "us-east.{{whitelabel_domain}}":{
%%          "name":"Primary Proxy",
%%          "zone": "us-east",
%%          "mapping":[
%%             "8.36.70.3"
%%          ]
%%       },
%%       "us-central.{{whitelabel_domain}}":{
%%          "name":"Secondary Proxy",
%%          "zone": "us-central",
%%          "mapping":[
%%             "166.78.105.67"
%%          ]
%%       },
%%       "us-west.{{whitelabel_domain}}":{
%%          "name":"Tertiary Proxy",
%%          "zone": "us-west",
%%          "mapping":[
%%             "8.30.173.3"
%%          ]
%%       }
%%    },
%%    "NAPTR":{
%%       "proxy-east.{{whitelabel_domain}}":{
%%          "name":"East NAPTR",
%%          "mapping":[
%%             "10 100 \"S\" \"SIP+D2U\" \"\" _sip._udp.proxy-east.{{whitelabel_domain}}."
%%          ]
%%       },
%%       "proxy-central.{{whitelabel_domain}}":{
%%          "name":"Central NAPTR",
%%          "mapping":[
%%             "10 100 \"S\" \"SIP+D2U\" \"\" _sip._udp.proxy-central.{{whitelabel_domain}}."
%%          ]
%%       },
%%       "proxy-west.{{whitelabel_domain}}":{
%%          "name":"West NAPTR",
%%          "mapping":[
%%             "10 100 \"S\" \"SIP+D2U\" \"\" _sip._udp.proxy-west.{{whitelabel_domain}}."
%%          ]
%%       }
%%    },
%%    "SRV":{
%%       "_sip._udp.proxy-east.{{whitelabel_domain}}":{
%%          "name":"East SRV",
%%          "mapping":[
%%             "10 10 7000 us-east.{{whitelabel_domain}}.",
%%             "15 15 7000 us-central.{{whitelabel_domain}}.",
%%             "20 20 7000 us-west.{{whitelabel_domain}}."
%%          ]
%%       }
%%    },
%%    "MX":{

%%    },
%%    "TXT":{

%%    }
%% }
