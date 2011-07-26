%%%-------------------------------------------------------------------
%%% File    : yadis.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Yadis resource discovery
%%%
%%% Created : 17 Sep 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(yadis).

-export([retrieve/1]).

-include("openid.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(HTTP_HEADERS, [{"Accept", "application/xrds+xml"},
                       {"Connection", "close"},
                       {"User-Agent", "Erlang/openid"}]).

-define(HTTP_OPTIONS, [{relaxed, true}]).
-define(REQ_OPTIONS, []).

%% ------------------------------------------------------------
%% API
%% ------------------------------------------------------------

     
retrieve(Identifier) ->
    
    Normalized = openid_utils:normalize_id(Identifier),
    IsXRI = lists:member(hd(Normalized), ?XRI_GCTX_SYMBOLS),
    
    URL = case IsXRI of
              true -> resolve(Normalized);
              false -> Normalized
          end,
    
    case openid_http:get(URL) of
	{ok, 200, Headers, Body} ->
            DescriptorURL = get_descriptor_url(Headers, Body),
            XRDS = handle_response(DescriptorURL, Headers, Body),
            case XRDS of 
                none -> 
                    {none, Normalized, Body};
                #openid_xrds{} -> 

                    % XXX Todo -- Normalize DescriptorURL as claimedID 
                    % (2.0 spec #7.2.4)

                    ClaimedID = case IsXRI of
                                    true -> Normalized;
                                    false -> DescriptorURL
                                end,
                    XRDS#openid_xrds{origID=Identifier, 
				     isXRI=IsXRI,
				     claimedID=ClaimedID}
            end;
        Other ->
            {error, {http_error, {Normalized, Other}}}
    end.


%% ------------------------------------------------------------
%% Retrieval details
%% ------------------------------------------------------------

resolve(Identifier) -> "http://xri.net/" ++ Identifier ++ "?_xrd_r=application/xrds+xml".


handle_response(none, Headers, Body) ->
    get_xrds(?GVD("content-type", Headers, none), Body);
handle_response(URL, _Headers, _Body) ->
    try_descriptor_url(URL).


get_xrds("application/xrds" ++ _Rest, Body) -> munge_xrds(Body);
get_xrds("text/xml" ++ _Rest, Body) -> munge_xrds(Body); % Against the spec, but LiveJournal does it.
get_xrds(_Other, _Body) -> none.


try_descriptor_url(none) -> {error, no_descriptor_url};
try_descriptor_url(URL) -> retrieve_step_two(URL).


retrieve_step_two(YadisURL) ->
    case openid_http:get(YadisURL) of
	{ok, 200, Headers, Body} ->
            get_xrds(?GVD("content-type", Headers, none), Body);
        Other ->
            {error, {http_error, {step_two, YadisURL, Other}}}
    end.



get_descriptor_url(Headers, Body) when is_list(Headers) ->
    case ?GVD("x-xrds-location", Headers, none) of
        none ->
            case ?GVD("content-type", Headers, none) of
                "application/xrds+xml" ++ _Rest -> none;
                none -> none;
                _MaybeHTML -> get_descriptor_url(Body)
            end;
        URL -> URL
    end.

get_descriptor_url(Body) ->
    case openid_utils:get_tags(Body, "meta", "http-equiv", "x-xrds-location") of
        [] -> none;
        [Tag|_] -> ?GVD("content", Tag, none)
    end.


%% ------------------------------------------------------------
%% XRDS
%% ------------------------------------------------------------

munge_xrds(String) ->
    {Doc, _} = xmerl_scan:string(String),
    CanonicalID = get_canonical_id(Doc),
    Services = [S || {_P, S} <- lists:sort(
      fun({P1,_},{P2,_}) -> P1 < P2 end,
      [munge_service(S) || S <- xmerl_xpath:string("XRD/Service", Doc)])],
    #openid_xrds{canonicalID=CanonicalID, services=Services}.

munge_service(Service) ->
    Priority = get_priority(Service#xmlElement.attributes),
    Types = [get_text(T) || T <- xmerl_xpath:string("Type", Service)],
    LocalID = get_local_id(Service),
    URIs = [U || {_P, U} <- lists:sort(
                              fun({P1,_},{P2,_}) -> P1 < P2 end,
                              [{get_priority(U#xmlElement.attributes), get_text(U)}
                               || U <- xmerl_xpath:string("URI", Service)])],
    {Priority, #openid_xrdservice{types=Types, uris=URIs, localID=LocalID}}.

get_text(#xmlElement{content=[]}) -> "";
get_text(#xmlElement{content=[Value|_]}) -> Value#xmlText.value.

get_priority([#xmlAttribute{name=priority, value=Value}|_]) -> list_to_integer(Value);
get_priority([_|Rest]) -> get_priority(Rest);
get_priority([]) -> none.

get_canonical_id(Doc) ->
    case xmerl_xpath:string("XRD/CanonicalID", Doc) of
        [] -> none;
        [#xmlElement{content=[Value|_]}|_] -> Value#xmlText.value
    end.
        

get_local_id(Service) ->     
    get_local_id(Service, ["LocalID", "Delegate"]).

get_local_id(_, []) ->
    none;
get_local_id(Service, [Tag|Rest]) ->
    case xmerl_xpath:string(Tag, Service) of
        [] -> get_local_id(Service, Rest);
        [#xmlElement{content=[Value|_]}|_] -> Value#xmlText.value
    end.
