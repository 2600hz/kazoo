%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wnm_vitelity).

-export([find_numbers/3
         ,acquire_number/1
         ,disconnect_number/1
        ]).

-include("../wnm.hrl").

-define(WNM_VITELITY_CONFIG_CAT, <<(?WNM_CONFIG_CAT)/binary, ".vitelity">>).

-define(TOLLFREE_URI_TEMPLATE, whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"tollfree_uri">>
                                                 ,<<"http://api.vitelity.net/api.php?login={login}&pass={password}&cmd={cmd}">>)).

-define(NPA_URI_TEMPLATE, whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"npa_uri">>
                                                ,<<"http://api.vitelity.net/api.php?login={login}&pass={password}&cmd={cmd}&npa={npa}">>)).

-define(NPAXX_URI_TEMPLATE, whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"npaxx_uri">>
                                                  ,<<"http://api.vitelity.net/api.php?login={login}&pass={password}&cmd={cmd}&npanxx={npanxx}">>)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the Bandwidth.com system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
                          {'ok', wh_json:object()} |
                          {'error', term()}.
find_numbers(Number, Quanity, Opts) ->
    case props:get_value('tollfree', Opts, 'false') of
        'true' -> find_tollfree(Number, Quanity, add_tollfree_options(Quanity, Opts));
        'false' -> find_local(Number, Quanity, add_local_options(Number, Opts))
    end.

-spec add_tollfree_options(pos_integer(), wh_proplist()) -> wh_proplist().
add_tollfree_options(Quantity, Opts) ->
    TollFreeOpts = [{'cmd', <<"listtollfree">>}
                    ,{'limit', Quantity}
                    ,{'as_xml', <<"yes">>}
                    ,{'uri_template', ?TOLLFREE_URI_TEMPLATE}
                    | default_options()
                   ],
    lists:foldl(fun add_options_fold/2, Opts, TollFreeOpts).

-spec add_local_options(ne_binary(), wh_proplist()) -> wh_proplist().
add_local_options(Number, Opts) when byte_size(Number) =< 3 ->
    LocalOpts = [{'npa', Number}
                 ,{'cmd', <<"listnpa">>}
                 ,{'withrates', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"withrates">>)}
                 ,{'type', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"type">>)}
                 ,{'provider', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"provider">>)}
                 ,{'as_xml', <<"yes">>}
                 ,{'cnam', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"require_cnam">>)}
                 ,{'uri_template', ?NPA_URI_TEMPLATE}
                 | default_options()
                ],
    lists:foldl(fun add_options_fold/2, Opts, LocalOpts);
add_local_options(Number, Opts) ->
    LocalOpts = [{'npanxx', Number}
                 ,{'cmd', <<"listnpanxx">>}
                 ,{'withrates', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"withrates">>)}
                 ,{'type', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"type">>)}
                 ,{'provider', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"provider">>)}
                 ,{'as_xml', <<"yes">>}
                 ,{'cnam', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"require_cnam">>)}
                 ,{'uri_template', ?NPAXX_URI_TEMPLATE}
                 | default_options()
                ],
    lists:foldl(fun add_options_fold/2, Opts, LocalOpts).

-spec default_options() -> wh_proplist().
default_options() ->
    [{'login', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"login">>)}
     ,{'password', whapps_config:get(?WNM_VITELITY_CONFIG_CAT, <<"password">>)}
    ].

-spec add_options_fold({atom(), api_binary()}, wh_proplist()) -> wh_proplist().
add_options_fold({_K, 'undefined'}, Opts) -> Opts;
add_options_fold({K, V}, Opts) ->
    props:insert_value(K, V, Opts).

find_tollfree(Number, Quantity, Opts) ->
    case query_vitelity(build_uri(Opts)) of
        {'ok', Numbers} ->
            Numbers;
        {'error', _}=E ->
            E
    end.

find_local(Number, Quantity, Opts) ->
    case query_vitelity(build_uri(Opts)) of
        {'ok', Numbers} ->
            Numbers;
        {'error', _}=E ->
            E
    end.

-spec build_uri(wh_proplist()) -> ne_binary().
build_uri(Opts) ->
    lists:foldl(fun build_uri_fold/2, props:get_value('uri_template', Opts), Opts).

-spec build_uri_fold({atom(), ne_binary()}, ne_binary()) -> ne_binary().
build_uri_fold({Key, Replace}, T) ->
    Search = <<"{", (wh_util:to_binary(Key))/binary, "}">>,
    binary:replace(T, Search, Replace, ['global']).

query_vitelity(URI) ->
    lager:debug("querying ~s", [URI]),
    case ibrowse:send_req(wh_util:to_list(URI), [], 'post') of
        {'ok', "200", _RespHeaders, RespXML} ->
            lager:debug("recv 200: ~s", [RespXML]),
            xml_to_numbers(RespXML);
        {'ok', _RespCode, _RespHeaders, RespXML} ->
            lager:debug("recv ~s: ~s", [_RespCode, RespXML]),
            xml_to_error(RespXML);
        {'error', _R}=E ->
            lager:debug("error querying: ~p", [_R]),
            E
    end.

xml_to_numbers(XML) ->
    try xmerl_scan:string(XML) of
        {XMLEls, _} -> xml_els_to_numbers(XMLEls)
    catch
        _E:_R ->
            lager:debug("failed to decode xml: ~s: ~p", [_E, _R]),
            {'error', 'xml_decode_failed'}
    end.

xml_to_error(XML) ->
    try xmerl_scan:string(XML) of
        {XMLEls, _} -> xml_els_to_error(XMLEls)
    catch
        _E:_R ->
            lager:debug("failed to decode xml: ~s: ~p", [_E, _R]),
            {'error', 'xml_decode_failed'}
    end.

xml_els_to_numbers(#xmlElement{name='content'
                               ,content=Children
                              }) ->
    xml_els_to_numbers(Children);
xml_els_to_numbers([#xmlText{}|Children]) ->
    xml_els_to_numbers(Children);
xml_els_to_numbers([#xmlElement{name='status'
                                ,content=Status
                               }
                    | Children]) ->

             [{xmlText,[{content,1}],1,[]," ",text},
              {xmlElement,status,status,[],
                          {xmlNamespace,[],[]},
                          [{content,1}],
                          2,[],
                          [{xmlText,[{status,2},{content,1}],1,[],"ok",text}],
                          [],"/home/james/local/git/2600hz/kazoo/scripts",undeclared},
              {xmlText,[{content,1}],3,[]," ",text},
              {xmlElement,numbers,numbers,[],
                          {xmlNamespace,[],[]},
                          [{content,1}],
                          4,[],
                          [{xmlText,[{numbers,4},{content,...}],1,[],[...],...},
                           {xmlElement,did,did,[],...},
                           {xmlText,[{...}|...],3,...},
                           {xmlElement,did,...},
                           {xmlText,...}],
                          [],undefined,undeclared},
              {xmlText,[{content,1}],5,[]," ",text}],
             [],"/home/james/local/git/2600hz/kazoo/scripts",undeclared},
 []}



%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(wnm_number()) -> wnm_number().
acquire_number(#number{auth_by=AuthBy
                       ,assigned_to=AssignedTo
                       ,module_data=Data
                      }=N) ->
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(wnm_number()) -> wnm_number().
disconnect_number(Number) -> Number.
