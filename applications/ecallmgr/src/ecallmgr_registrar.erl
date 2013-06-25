%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_registrar).

-export([reg_success/2]).
-export([lookup_contact/2]).
-export([endpoint_node/2]).
-export([lookup/3]).

-define(SERVER, ?MODULE).

-define(CONTACT_KEY(Realm, Username), {?MODULE, 'contact', Username, Realm}).
-define(NODE_KEY(Realm, Username), {?MODULE, 'node', Username, Realm}).
-define(LOOKUP_KEY(Realm, Username), {?MODULE, 'lookup', Username, Realm}).

-include("ecallmgr.hrl").

-spec reg_success(wh_proplist(), atom()) -> 'ok'.
reg_success(Props, Node) ->
    Username = props:get_value(<<"username">>, Props),
    Realm = props:get_value(<<"realm">>, Props),
    CacheProps = [{'expires', ecallmgr_util:get_expires(Props)}],
    wh_cache:store_local(?ECALLMGR_REG_CACHE, ?NODE_KEY(Realm, Username), Node, CacheProps),

    case props:get_value(<<"contact">>, Props) of
        'undefined' -> 'ok';
        RawContact ->
            [User, AfterAt] = binary:split(RawContact, <<"@">>),
            AfterUnquoted = wh_util:to_binary(mochiweb_util:unquote(AfterAt)),
            Contact = binary:replace(<<User/binary, "@", AfterUnquoted/binary>>, [<<"<">>, <<">">>], <<>>, ['global']),
            wh_cache:store_local(?ECALLMGR_REG_CACHE, ?CONTACT_KEY(Realm, Username), Contact, CacheProps)
    end.

-spec lookup_contact(ne_binary(), ne_binary()) ->
                            {'ok', ne_binary()} |
                            {'error', 'not_found'}.
lookup_contact(Realm, Username) ->
    case wh_cache:peek_local(?ECALLMGR_REG_CACHE, ?CONTACT_KEY(Realm, Username)) of
        {'ok', _}=OK -> OK;
        {'error', 'not_found'} ->
            case lookup(Realm, Username, [<<"Contact">>]) of
                [{<<"Contact">>, Contact}] -> {'ok', Contact};
                {'error', _R} ->
                    lager:notice("failed to find contact for ~s@~s: ~p", [Username, Realm, _R]),
                    {'error', 'not_found'}
            end
    end.

-spec endpoint_node(ne_binary(), ne_binary()) ->
                           {'ok', atom()} |
                           {'error', 'not_found'}.
endpoint_node(Realm, Username) ->
    case wh_cache:fetch_local(?ECALLMGR_REG_CACHE, ?NODE_KEY(Realm, Username)) of
        {'ok', Node} -> {'ok', Node};
        {'error', 'not_found'} ->
            case lookup(Realm, Username, [<<"FreeSWITCH-Nodename">>]) of
                [{<<"FreeSWITCH-Nodename">>, Node}] -> {'ok', wh_util:to_atom(Node, 'true')};
                {'error', _R} ->
                    lager:notice("failed to find node name for ~s@~s: ~p", [Username, Realm, _R]),
                    {'error', 'not_found'}
            end
    end.

-spec lookup(ne_binary(), ne_binary(), ne_binaries()) ->
                    wh_proplist() |
                    {'error', 'not_found'}.
lookup(Realm, Username, Fields) ->
    case maybe_query_registrar(Realm, Username) of
        {'error', _R} -> {'error', 'not_found'};
        {'ok', Props} when Fields =:= [] -> Props;
        {'ok', Props} ->
            FilterFun = fun({K, _}=V, Acc) ->
                                case lists:member(K, Fields) of
                                    'true' -> [V | Acc];
                                    'false' -> Acc
                                end
                        end,
            lists:foldr(FilterFun, [], Props)
    end.

-spec maybe_query_registrar(ne_binary(), ne_binary()) ->
                                   {'ok', wh_proplist()} |
                                   {'error', 'not_found'}.
maybe_query_registrar(Realm, Username) ->
    case wh_cache:peek_local(?ECALLMGR_REG_CACHE, ?LOOKUP_KEY(Realm, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} -> query_registrar(Realm, Username)
    end.

-spec query_registrar(ne_binary(), ne_binary()) ->
                             {'ok', wh_proplist()} |
                             {'error', 'not_found'}.
query_registrar(Realm, Username) ->
    lager:debug("looking up registration information for ~s@~s", [Username, Realm]),
    ReqResp = wh_amqp_worker:call_collect(?ECALLMGR_AMQP_POOL
                                          ,[{<<"Username">>, Username}
                                            ,{<<"Realm">>, Realm}
                                            ,{<<"Fields">>, []}
                                            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                           ]
                                          ,fun wapi_registration:publish_query_req/1
                                          ,{'registrar', fun wapi_registration:query_resp_v/1}
                                         ),
    case ReqResp of
        {'ok', [RespJObj|_]} ->
            maybe_received_registration(RespJObj, Realm, Username);
        _Else ->
            lager:debug("did not receive a valid registrar response", []),
            {'error', 'not_found'}
    end.

-spec maybe_received_registration(wh_json:object(), ne_binary(), ne_binary()) -> 
                                         {'ok', wh_proplist()} |
                                         {'error', 'not_found'}.
maybe_received_registration(RespJObj, Realm, Username) ->
    case wapi_registration:query_resp_v(RespJObj) of
        'false' -> {'error', 'not_found'};
        'true' ->
            lager:debug("received registration information"),
            JObj = wh_json:get_value(<<"Fields">>, RespJObj, wh_json:new()),
            Props = wh_json:to_proplist(JObj),
            CacheProps = [{'expires', ecallmgr_util:get_expires(Props)}],
            wh_cache:store_local(?ECALLMGR_REG_CACHE, ?LOOKUP_KEY(Realm, Username), Props, CacheProps),
            Contact = wh_json:get_value(<<"Contact">>, JObj),
            wh_cache:store_local(?ECALLMGR_REG_CACHE, ?CONTACT_KEY(Realm, Username), Contact, CacheProps),
            Node = wh_util:to_atom(wh_json:get_value(<<"FreeSWITCH-Nodename">>, JObj), 'true'),
            wh_cache:store_local(?ECALLMGR_REG_CACHE, ?NODE_KEY(Realm, Username), Node, CacheProps),
            {'ok', Props}
    end.
