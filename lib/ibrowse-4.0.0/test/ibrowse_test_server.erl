%%% File    : ibrowse_test_server.erl
%%% Author  : Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%% Description : A server to simulate various test scenarios
%%% Created : 17 Oct 2010 by Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>

-module(ibrowse_test_server).
-export([
         start_server/2,
         stop_server/1
        ]).

-record(request, {method, uri, version, headers = [], body = []}).

-define(dec2hex(X), erlang:integer_to_list(X, 16)).

start_server(Port, Sock_type) ->
    Fun = fun() ->
                  register(server_proc_name(Port), self()),
                  case do_listen(Sock_type, Port, [{active, false},
                                                   {reuseaddr, true},
                                                   {nodelay, true},
                                                   {packet, http}]) of
                      {ok, Sock} ->
                          do_trace("Server listening on port: ~p~n", [Port]),
                          accept_loop(Sock, Sock_type);
                      Err ->
                          erlang:error(
                            lists:flatten(
                              io_lib:format(
                                "Failed to start server on port ~p. ~p~n",
                                [Port, Err]))),
                          exit({listen_error, Err})
                  end
          end,
    spawn_link(Fun).

stop_server(Port) ->
    server_proc_name(Port) ! stop,
    ok.

server_proc_name(Port) ->
    list_to_atom("ibrowse_test_server_"++integer_to_list(Port)).

do_listen(tcp, Port, Opts) ->
    gen_tcp:listen(Port, Opts);
do_listen(ssl, Port, Opts) ->
    application:start(crypto),
    application:start(ssl),
    ssl:listen(Port, Opts).

do_accept(tcp, Listen_sock) ->
    gen_tcp:accept(Listen_sock);
do_accept(ssl, Listen_sock) ->
    ssl:ssl_accept(Listen_sock).

accept_loop(Sock, Sock_type) ->
    case do_accept(Sock_type, Sock) of
        {ok, Conn} ->
            Pid = spawn_link(
              fun() ->
                      server_loop(Conn, Sock_type, #request{})
              end),
            set_controlling_process(Conn, Sock_type, Pid),
            Pid ! {setopts, [{active, true}]},
            accept_loop(Sock, Sock_type);
        Err ->
            Err
    end.

set_controlling_process(Sock, tcp, Pid) ->
    gen_tcp:controlling_process(Sock, Pid);
set_controlling_process(Sock, ssl, Pid) ->
    ssl:controlling_process(Sock, Pid).

setopts(Sock, tcp, Opts) ->
    inet:setopts(Sock, Opts);
setopts(Sock, ssl, Opts) ->
    ssl:setopts(Sock, Opts).

server_loop(Sock, Sock_type, #request{headers = Headers} = Req) ->
    receive
        {http, Sock, {http_request, HttpMethod, HttpUri, HttpVersion}} ->
            server_loop(Sock, Sock_type, Req#request{method = HttpMethod,
                                                     uri = HttpUri,
                                                     version = HttpVersion});
        {http, Sock, {http_header, _, _, _, _} = H} ->
            server_loop(Sock, Sock_type, Req#request{headers = [H | Headers]});
        {http, Sock, http_eoh} ->
            process_request(Sock, Sock_type, Req),
            server_loop(Sock, Sock_type, #request{});
        {http, Sock, {http_error, Err}} ->
            do_trace("Error parsing HTTP request:~n"
                     "Req so far : ~p~n"
                     "Err        : ", [Req, Err]),
            exit({http_error, Err});
        {setopts, Opts} ->
            setopts(Sock, Sock_type, Opts),
            server_loop(Sock, Sock_type, Req);
        {tcp_closed, Sock} ->
            do_trace("Client closed connection~n", []),
            ok;
        stop ->
            ok;
        Other ->
            do_trace("Recvd unknown msg: ~p~n", [Other]),
            exit({unknown_msg, Other})
    after 5000 ->
            do_trace("Timing out client connection~n", []),
            ok
    end.

do_trace(Fmt, Args) ->
    do_trace(get(my_trace_flag), Fmt, Args).

do_trace(true, Fmt, Args) ->
    io:format("~s -- " ++ Fmt, [ibrowse_lib:printable_date() | Args]);
do_trace(_, _, _) ->
    ok.

process_request(Sock, Sock_type,
                #request{method='GET',
                         headers = Headers,
                         uri = {abs_path, "/ibrowse_stream_once_chunk_pipeline_test"}} = Req) ->
    Req_id = case lists:keysearch("X-Ibrowse-Request-Id", 3, Headers) of
                 false ->
                     "";
                 {value, {http_header, _, _, _, Req_id_1}} ->
                     Req_id_1
             end,
    Req_id_header = ["x-ibrowse-request-id: ", Req_id, "\r\n"],
    do_trace("Recvd req: ~p~n", [Req]),
    Body = string:join([integer_to_list(X) || X <- lists:seq(1,100)], "-"),
    Chunked_body = chunk_request_body(Body, 50),
    Resp_1 = [<<"HTTP/1.1 200 OK\r\n">>,
              Req_id_header,
              <<"Transfer-Encoding: chunked\r\n\r\n">>],
    Resp_2 = Chunked_body,
    do_send(Sock, Sock_type, Resp_1),
    timer:sleep(100),
    do_send(Sock, Sock_type, Resp_2);
process_request(Sock, Sock_type,
                #request{method='GET',
                         headers = _Headers,
                         uri = {abs_path, "/ibrowse_inac_timeout_test"}} = Req) ->
    do_trace("Recvd req: ~p. Sleeping for 30 secs...~n", [Req]),
    timer:sleep(30000),
    do_trace("...Sending response now.~n", []),
    Resp = <<"HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n">>,
    do_send(Sock, Sock_type, Resp);
process_request(Sock, Sock_type,
                #request{method='HEAD',
                         headers = _Headers,
                         uri = {abs_path, "/ibrowse_head_transfer_enc"}}) ->
    Resp = <<"HTTP/1.1 400 Bad Request\r\nServer: Apache-Coyote/1.1\r\nTransfer-Encoding: chunked\r\nDate: Wed, 04 Apr 2012 16:53:49 GMT\r\nConnection: close\r\n\r\n0\r\n\r\n">>,
    do_send(Sock, Sock_type, Resp);
process_request(Sock, Sock_type,
                #request{method='HEAD',
                         headers = _Headers,
                         uri = {abs_path, "/ibrowse_head_test"}}) ->
    Resp = <<"HTTP/1.1 200 OK\r\nServer: Apache-Coyote/1.1\r\nTransfer-Encoding: chunked\r\nDate: Wed, 04 Apr 2012 16:53:49 GMT\r\nConnection: close\r\n\r\n">>,
    do_send(Sock, Sock_type, Resp);
process_request(Sock, Sock_type, Req) ->
    do_trace("Recvd req: ~p~n", [Req]),
    Resp = <<"HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n">>,
    do_send(Sock, Sock_type, Resp).

do_send(Sock, tcp, Resp) ->
    gen_tcp:send(Sock, Resp);
do_send(Sock, ssl, Resp) ->
    ssl:send(Sock, Resp).


%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

chunk_request_body(Body, _ChunkSize) when is_tuple(Body) orelse
                                          is_function(Body) ->
    Body;
chunk_request_body(Body, ChunkSize) ->
    chunk_request_body(Body, ChunkSize, []).

chunk_request_body(Body, _ChunkSize, Acc) when Body == <<>>; Body == [] ->
    LastChunk = "0\r\n",
    lists:reverse(["\r\n", LastChunk | Acc]);
chunk_request_body(Body, ChunkSize, Acc) when is_binary(Body),
                                              size(Body) >= ChunkSize ->
    <<ChunkBody:ChunkSize/binary, Rest/binary>> = Body,
    Chunk = [?dec2hex(ChunkSize),"\r\n",
             ChunkBody, "\r\n"],
    chunk_request_body(Rest, ChunkSize, [Chunk | Acc]);
chunk_request_body(Body, _ChunkSize, Acc) when is_binary(Body) ->
    BodySize = size(Body),
    Chunk = [?dec2hex(BodySize),"\r\n",
             Body, "\r\n"],
    LastChunk = "0\r\n",
    lists:reverse(["\r\n", LastChunk, Chunk | Acc]);
chunk_request_body(Body, ChunkSize, Acc) when length(Body) >= ChunkSize ->
    {ChunkBody, Rest} = split_list_at(Body, ChunkSize),
    Chunk = [?dec2hex(ChunkSize),"\r\n",
             ChunkBody, "\r\n"],
    chunk_request_body(Rest, ChunkSize, [Chunk | Acc]);
chunk_request_body(Body, _ChunkSize, Acc) when is_list(Body) ->
    BodySize = length(Body),
    Chunk = [?dec2hex(BodySize),"\r\n",
             Body, "\r\n"],
    LastChunk = "0\r\n",
    lists:reverse(["\r\n", LastChunk, Chunk | Acc]).

split_list_at(List, N) ->
    split_list_at(List, N, []).

split_list_at([], _, Acc) ->
    {lists:reverse(Acc), []};
split_list_at(List2, 0, List1) ->
    {lists:reverse(List1), List2};
split_list_at([H | List2], N, List1) ->
    split_list_at(List2, N-1, [H | List1]).

