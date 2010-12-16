%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Event subscription REST point
%%%
%%% Client requests an event subscription (assuming authed^2 (authenticated
%%% and authorized)).
%%%
%%% evtsub creates the proper queue on AMQP bound to the proper exchange with
%%% the proper binding, but does not consume messages off the queue yet. Queue
%%% identifier is placed in client's session, persisted to couch.
%%%
%%% Client requests events; evtsub creates a consumer of the queue, pulls off
%%% a limited number of messages (100 max?), and returns them in bulk to the
%%% client. evtsub also disconnects the consumer to keep messages queued in AMQP-
%%% land.
%%%
%%% Client requests event cancellation (or auth token times out, an event which
%%% evtsub has subscribed to within Crossbar); evtsub removes AMQP queue.
%%%
%%% @end
%%% Created :  9 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(evtsub_resource).

-export([init/1]).
-export([to_html/2, to_json/2, to_xml/2, to_text/2]).
-export([from_json/2, from_xml/2, from_text/2]).
-export([generate_etag/2, encodings_provided/2, finish_request/2, is_authorized/2]).
-export([content_types_provided/2, content_types_accepted/2, resource_exists/2, allowed_methods/2]).
-export([process_post/2]). %, post_is_create/2, create_path/2]).

-import(logger, [format_log/3]).

-include("crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(context, {
	  content_types_provided = [] :: list(tuple(atom(), list(string()))) | []
          ,content_types_accepted = [] :: list(tuple(atom(), list(string()))) | []
	  ,amqp_host = "" :: string()
	  ,session = #session{} :: #session{}
	  ,req_params = [] :: proplist()
          ,request = undefined % function to call in evtsub.erl
	 }).

init(Opts) ->
    Opts1 = lists:foldl(fun({_, OptsTmp}, Acc) -> lists:merge(OptsTmp, Acc) end, Opts
			,crossbar_bindings:run(<<"evtsub.init">>, Opts)),
    Provided = lists:foldr(fun({Fun, L}, Acc) ->
				   lists:foldr(fun(EncType, Acc1) -> [ {EncType, Fun} | Acc1 ] end, Acc, L)
			   end, [], props:get_value(content_types_provided, Opts1, [])),
    Accepted = lists:foldr(fun({Fun, L}, Acc) ->
				   lists:foldr(fun(EncType, Acc1) -> [ {EncType, Fun} | Acc1 ] end, Acc, L)
			   end, [], props:get_value(content_types_accepted, Opts1, [])),
    {ok % {wmtrace, "/tmp"} %% for debugging, then run wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp"). in the VM
                            %% Run your request, then go to http://HOST/wmtrace/ and select a file to view 
                            %% Remove tracing by running wmtrace_resource:remove_dispatch_rules(). in the VM
     ,#context{
       content_types_provided = Provided
       ,content_types_accepted = Accepted
       ,amqp_host = props:get_value(amqp_host, Opts1)
      }}.

resource_exists(RD, Context) ->
    crossbar_bindings:run(<<"evtsub.resource_exists">>, []),
    case erlang:whereis(evtsub) of
	undefined -> {false, RD, Context};
	P when is_pid(P) ->
	    case wrq:path_info(request, RD) of
		undefined -> {{error, "Invalid request"}, RD, Context};
		Req when is_list(Req) ->
		    try
			Fun = list_to_existing_atom(Req),
			case erlang:function_exported(evtsub, Fun, 1) andalso is_authed(RD, Context) of
			    {true, Context1} ->
				format_log(info, "EVTSUB_R: ~p/1 found and authed.~n", [Fun]),
				{true, RD, Context1#context{request=Fun}};
			    {false, Context2} ->
				{{error, 417}, RD, Context2};
			    false ->
				format_log(info, "EVTSUB_R: ~p/1 not exported~n", [Fun]),
				{false, RD, Context}
			end
		    catch
			error:badarg ->
			    format_log(info, "EVTSUB_R: ~p doesn't exist~n", [Req]),
			    {false, RD, Context}
		    end
	    end
    end.

-spec(is_authed/2 :: (RD :: #wm_reqdata{}, Context :: #context{}) -> tuple(boolean(), #context{})).
is_authed(RD, Context) ->
    S0 = crossbar_session:start_session(RD),
    S = S0#session{account_id = <<"test">>},
    Params = crossbar_util:get_request_params(RD),
    AuthenticatedRes = crossbar_bindings:run(<<"evtsub.is_authenticated">>, {S, Params}),
    IsAuthenticated = lists:any(fun({true, _}) -> true; ({timeout, _}) -> true; (_) -> false end, AuthenticatedRes),
    AuthorizedRes = crossbar_bindings:run(<<"evtsub.is_authorized">>, {S, Params}),
    IsAuthorized = lists:any(fun({true, _}) -> true; ({timeout, _}) -> true; (_) -> false end, AuthorizedRes),

    format_log(info, "EVTSUB_R: IsAuthen: ~p IsAuthor: ~p~n", [IsAuthenticated, IsAuthorized]),

    {IsAuthenticated andalso IsAuthorized, Context#context{session=S, req_params=Params}}.

content_types_provided(RD, #context{content_types_provided=CTP}=Context) ->
    crossbar_bindings:run(<<"evtsub.content_types_provided">>, CTP),
    {CTP, RD, Context}.

content_types_accepted(RD, #context{content_types_accepted=CTA}=Context) ->
    crossbar_bindings:run(<<"evtsub.content_types_accepted">>, CTA),
    {CTA, RD, Context}.

allowed_methods(RD, Context) ->
    crossbar_bindings:run(<<"evtsub.allowed_methods">>, []),
    {['POST', 'PUT', 'DELETE'], RD, Context}.

is_authorized(RD, Context) ->
    %% TODO - process results of bindings
    {true, RD, Context}.

generate_etag(RD, Context) ->
    crossbar_bindings:run(<<"evtsub.generate_etag">>, []),
    { mochihex:to_hex(crypto:md5(wrq:resp_body(RD))), RD, Context }.

encodings_provided(RD, Context) ->
    crossbar_bindings:run(<<"evtsub.encodings_provided">>, []),
    { [ {"identity", fun(X) -> X end}
       ,{"gzip", fun(X) -> zlib:gzip(X) end}]
      ,RD, Context}.

process_post(RD, #context{request=Fun, req_params=Params}=Context) ->
    crossbar_bindings:run(<<"evtsub.process_post">>, []),
    format_log(info, "EVTSUB_R: process_post params ~p~n", [Params]),
    Resp = crossbar_util:winkstart_envelope(evtsub:Fun(Params)),
    {true, wrq:append_to_response_body(Resp, RD), Context}.

finish_request(RD, #context{session=S}=Context) ->
    crossbar_bindings:run(<<"evtsub.finish_request">>, RD),
    {true, crossbar_session:finish_session(S, RD), Context}.

%% Convert the input to Erlang
from_text(RD, Context) ->
    format_log(info, "EVTSUB_R: from_text: QS: ~p ReqB: ~p~n", [wrq:req_qs(RD), wrq:req_body(RD)]),
    {true, RD, Context}.

from_json(RD, #context{request=Fun, req_params=Params}=Context) ->
    Res = crossbar_util:winkstart_envelope(evtsub:Fun(Params)),
    format_log(info, "EVTSUB_R: from_json: Params ~p Res: ~p~n", [Params, Res]),
    {true, wrq:append_to_response_body(Res, RD), Context}.

from_xml(RD, Context) ->
    format_log(info, "EVTSUB_R: from_xml: QS: ~p ReqB: ~p~n", [wrq:req_qs(RD), wrq:req_body(RD)]),
    {true, RD, Context}.

%% Generate the response
to_html(RD, #context{request=Fun, req_params=Params}=Context) ->
    format_log(info, "EVTSUB_R: to_html: ReqB: ~p~n", [wrq:resp_body(RD)]),
    Res = evtsub:Fun(Params),
    Page = io_lib:format("<html><body><span>add: </span><strong>~s</strong></body></html>", [Res]),
    {Page, RD, Context}.

to_json(RD, #context{request=Fun, req_params=Params}=Context) ->
    Resp = crossbar_util:winkstart_envelope(evtsub:Fun(Params)),
    format_log(info, "EVTSUB_R: to_json: ~s~n", [Resp]),
    {Resp, RD, Context}.

to_xml(RD, Context) ->
    format_log(info, "EVTSUB_R: to_xml: ReqB: ~p~n", [wrq:resp_body(RD)]),
    {"<xml><evtsub type='xml' /></xml>", RD, Context}.

to_text(RD, #context{request=Fun, req_params=Params}=Context) ->
    format_log(info, "EVTSUB_R: to_text: ReqB: ~p~n", [wrq:resp_body(RD)]),
    {io_lib:format("txt ~p", [evtsub:Fun(Params)]), RD, Context}.
