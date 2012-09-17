-module(openid_http).
-include_lib("ibrowse/include/ibrowse.hrl").
-export([get/1, post/3]).

-define(MAX_REDIRECTS, 5). %% too generous?

get(URL) ->
    get(URL, ?MAX_REDIRECTS).

get(URL, Redirects) ->
    ReqHeaders = [{"Accept", "application/xrds+xml;level=1, */*"},
		  {"Connection", "close"},
		  {"User-Agent", "Erlang/erl_openid"}],
    ResponseRaw = ibrowse:send_req(URL, ReqHeaders, get),
    Response = normalise_response(ResponseRaw),
    case Response of
	{ok, Rcode, RespHeaders, _Body}
	  when Rcode > 300 andalso Rcode < 304 andalso Redirects > 0 ->
	    case get_redirect_url(URL, RespHeaders) of
		undefined -> Response;
		URL -> Response;
		NewURL -> get(NewURL, Redirects - 1)
	    end;
	Response -> Response
    end.

post(URL, ContentType, Body) ->
    Raw = ibrowse:send_req(URL, [{content_type, ContentType}], post, Body),
    normalise_response(Raw).

get_redirect_url(OldURL, Headers) ->
    Location = proplists:get_value("location", Headers),
    case Location of
	"http://" ++ _ -> Location;
	"https://" ++ _ -> Location;
	[$/|_] = Location ->
	    #url{protocol=Protocol, host=Host, port=Port} = ibrowse_lib:parse_url(OldURL),
	    PortFrag = case {Protocol, Port} of
			   {http, 80} -> "";
			   {https, 443} -> "";
			   _ -> ":" ++ integer_to_list(Port)
		       end,
	    atom_to_list(Protocol) ++ "://" ++ Host ++ PortFrag ++ Location;
	_ -> undefined
    end.

normalise_response({ok, RcodeList, Headers, Body}) ->
    RcodeInt = list_to_integer(RcodeList),
    LowHeaders = [ {string:to_lower(K), V} || {K, V} <- Headers ],
    {ok, RcodeInt, LowHeaders, Body};
normalise_response(X) -> X.
