-module(evtpub_resource).

-export([init/1]).
-export([to_html/2, to_json/2, to_xml/2, to_text/2]).
-export([from_json/2, from_xml/2, from_text/2]).
-export([generate_etag/2, encodings_provided/2, finish_request/2, is_authorized/2]).
-export([content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {
	  content_types_provided = [] :: list(tuple(atom(), list(string()))) | []
	  ,amqp_host = "" :: string()
	 }).

init(Opts) ->
    Provided = lists:foldr(fun({Fun, L}, Acc) ->
				   lists:foldr(fun(EncType, Acc1) -> [ {EncType, Fun} | Acc1 ] end, Acc, L)
			   end, [], props:get_value(content_types_provided, Opts, [])),
    {ok, #context{
       content_types_provided = Provided
       ,amqp_host = props:get_value(amqp_host, Opts)
      }}.

content_types_provided(RD, #context{content_types_provided=CTP}=Context) ->
    Path = wrq:disp_path(RD),
    Mime = webmachine_util:guess_mime(Path),
    io:format("Guessed MIME: ~p~n", [Mime]),
    {CTP, RD, Context}.

is_authorized(RD, Context) ->
    case crossbar_session:is_authorized(RD) of
        true -> {true, RD, Context};
        {false, Redirect} ->
            RD0 = wrq:do_redirect(true, RD),
            RD1 = wrq:set_resp_header("Location", Redirect, RD0),
            {{halt, 307}, RD1, Context}
    end.

generate_etag(RD, Context) ->
    { mochihex:to_hex(crypto:md5(wrq:resp_body(RD))), RD, Context }.

encodings_provided(RD, Context) ->
    { [{"identity", fun(X) -> X end},
       {"gzip", fun(X) -> zlib:gzip(X) end}],
      RD, Context}.

finish_request(RD, Context) ->
    {true, RD, Context}.
%%    {true, sbm_session:finish(Context#context.session, RD), Context}.

%% Convert the input to Erlang
from_text(RD, Context) ->
    {true, RD, Context}.

from_json(RD, Context) ->
    {true, RD, Context}.

from_xml(RD, Context) ->
    {true, RD, Context}.


%% Generate the response
to_html(RD, Context) ->
    Page = "evtpub html",
    {Page, RD, Context}.

to_json(RD, Context) ->
    {mochijson2:encode("evtpub json"), RD, Context}.

to_xml(RD, Context) ->
    {"<xml><evtpub type='xml' /></xml>", RD, Context}.

to_text(RD, Context) ->
    {"evtpub txt", RD, Context}.
