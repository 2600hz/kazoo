%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Event subscription REST point
%%%
%%% Client requests an e
%%%
%%% @end
%%% Created :  9 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(evtsub_resource).

-export([init/1]).
-export([to_html/2, to_json/2, to_xml/2, to_text/2]).
-export([from_json/2, from_xml/2, from_text/2]).
-export([generate_etag/2, encodings_provided/2, finish_request/2, is_authorized/2]).
-export([content_types_provided/2]).

-include("crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(context, {
	  content_types_provided = [] :: list(tuple(atom(), list(string()))) | []
	  ,amqp_host = "" :: string()
	  ,session = #session{} :: #session{}
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
    {CTP, RD, Context}.

is_authorized(RD, Context) ->
    S = crossbar_session:start_session(RD),
    case crossbar_session:is_authorized(S) of
        true -> {true, RD, Context#context{session=S}};
        false ->
            RD0 = wrq:do_redirect(true, RD),
            RD1 = wrq:set_resp_header("Location", "/", RD0),
            {{halt, 307}, RD1, Context#context{session=S}}
    end.

generate_etag(RD, Context) ->
    { mochihex:to_hex(crypto:md5(wrq:resp_body(RD))), RD, Context }.

encodings_provided(RD, Context) ->
    { [{"identity", fun(X) -> X end},
       {"gzip", fun(X) -> zlib:gzip(X) end}],
      RD, Context}.

finish_request(RD, #context{session=S}=Context) ->
    {true, crossbar_session:finish_session(S, RD), Context}.

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
