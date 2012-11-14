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

-define(CONTACT_KEY(Realm, Username), {?MODULE, contact, Username, Realm}).
-define(NODE_KEY(Realm, Username), {?MODULE, node, Username, Realm}).
-define(LOOKUP_KEY(Realm, Username), {?MODULE, lookup, Username, Realm}).

-include("ecallmgr.hrl").

-spec reg_success/2 :: (wh_proplist(), atom()) -> 'ok'.
reg_success(Props, Node) ->
    Username = props:get_value(<<"username">>, Props),
    Realm = props:get_value(<<"realm">>, Props),
    [User, AfterAt] = binary:split(props:get_value(<<"contact">>, Props), <<"@">>),
    AfterUnquoted = wh_util:to_binary(mochiweb_util:unquote(AfterAt)),
    Contact = binary:replace(<<User/binary, "@", AfterUnquoted/binary>>, [<<"<">>, <<">">>], <<>>, [global]),
    wh_cache:store_local(?ECALLMGR_REG_CACHE, ?CONTACT_KEY(Realm, Username), Contact, ecallmgr_util:get_expires(Props)),
    wh_cache:store_local(?ECALLMGR_REG_CACHE, ?NODE_KEY(Realm, Username), Node, ecallmgr_util:get_expires(Props)).

-spec lookup_contact/2 :: (ne_binary(), ne_binary()) ->
                                  {'ok', ne_binary()} |
                                  {'error', 'timeout'}.
lookup_contact(Realm, Username) ->
    case wh_cache:peek_local(?ECALLMGR_REG_CACHE, ?CONTACT_KEY(Realm, Username)) of
        {ok, Contact} -> {ok, Contact};
        {error, not_found} ->
            case lookup(Realm, Username, [<<"Contact">>]) of
                [{<<"Contact">>, Contact}] -> {ok, Contact};
                {error, _R}=E ->
                    lager:notice("failed to find registration for ~s@~s: ~p", [Username, Realm, _R]),
                    E
            end
    end.

-spec endpoint_node/2 :: (ne_binary(), ne_binary()) ->
                                 {'ok', atom()} |
                                 {'error', 'not_found'}.
endpoint_node(Realm, Username) ->
    wh_cache:fetch_local(?ECALLMGR_REG_CACHE, ?NODE_KEY(Realm, Username)).

-spec lookup/3 :: (ne_binary(), ne_binary(), [ne_binary(),...]) ->
                          wh_proplist() |
                          {'error', 'timeout'}.
lookup(Realm, Username, Fields) ->
    lager:debug("looking up registration information for ~s@~s", [Username, Realm]),
    FilterFun = fun({K, _}=V, Acc) ->
                        case lists:member(K, Fields) of
                            true -> [V | Acc];
                            false -> Acc
                        end
                end,
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,[{<<"Username">>, Username}
                                    ,{<<"Realm">>, Realm}
                                    ,{<<"Fields">>, []}
                                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                   ]
                                  ,fun wapi_registration:publish_query_req/1
                                  ,fun wapi_registration:query_resp_v/1),
    case ReqResp of
        {error, _R} ->
            lager:debug("did not receive registrar response: ~p", [_R]),
            {error, timeout};
        {ok, RespJObj} ->
            lager:debug("received registration information"),
            RegFields = wh_json:to_proplist(wh_json:get_value(<<"Fields">>, RespJObj, wh_json:new())),
            lists:foldr(FilterFun, [], RegFields)
    end.
