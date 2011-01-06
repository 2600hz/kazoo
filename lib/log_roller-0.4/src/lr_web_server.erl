%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(lr_web_server).
-export([start_link/1, loop/2]).

-include("log_roller.hrl").

-define(DEFAULT_ADDRESS, "127.0.0.1").
-define(DEFAULT_PORT, 8888).
-define(CONTENT_TYPE, {"Content-Type", "text/html"}).

start_link(Args) when is_list(Args) ->
	Address = get_arg_value(address, Args, ?DEFAULT_ADDRESS),
	Port = get_arg_value(port, Args, ?DEFAULT_PORT),
	DocRoot = get_arg_value(doc_root, Args),
	Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
    Result = mochiweb_http:start([{loop, Loop}, {ip, Address}, {port, Port}]),
    io:format("mochiweb_http:start/1 (~p, ~p, ~p): ~p~n", [Address, Port, DocRoot, Result]),
    Result.

loop(Req, DocRoot) ->
	PathTokens = [Req:get(method)|string:tokens(Req:get(path), "/")],
	case handle(PathTokens, Req, DocRoot) of
		no_match ->
			"/" ++ Path = Req:get(path),
			Req:serve_file(Path, DocRoot);
		Response ->
			Response
	end.
		        
handle(['GET'], Req, DocRoot) ->
	Req:serve_file("logs.html", DocRoot);
	    
handle(['GET', "logs"], Req, _DocRoot) ->
	serve_logs(Req:parse_qs(), Req);
	
handle(['POST', "logs"], Req, _DocRoot) ->
	serve_logs(Req:parse_post(), Req);

handle(['GET', "servers"], Req, _DocRoot) ->
	Content = tab_content(atom_to_list(default_server())),
	Req:respond({200, [?CONTENT_TYPE], Content});

handle(['GET', "servers", ServerName], Req, _DocRoot) ->
	Content = tab_content(ServerName),
	Req:respond({200, [?CONTENT_TYPE], Content});

handle(['GET', "nodes"], Req, _DocRoot) ->
	NodeOptions = lr_nodes:render({data, get_nodes(default_server()), [node()|nodes()]}),
	Req:respond({200, [?CONTENT_TYPE], NodeOptions});

handle(['GET', "nodes", Server], Req, _DocRoot) ->
	NodeOptions = lr_nodes:render({data, get_nodes(list_to_atom(Server)), [node()|nodes()]}),
	Req:respond({200, [?CONTENT_TYPE], NodeOptions});
	
handle(['GET', "tail"], Req, _DocRoot) ->
    tail_logs(Req:parse_qs(), Req);

handle(['POST', "tail"], Req, _DocRoot) ->
    tail_logs(Req:parse_qs(), Req);
	
handle(_, _, _) -> no_match.
		
serve_logs(Params, Req) ->
	Dict = opts(Params, dict:new()),
	Max = 
	    case dict:find(max, Dict) of
			{ok, Value1} -> Value1;
			error -> infinity
		end,
	ServerName =
		case dict:find(server, Dict) of
			{ok, Value2} -> Value2;
			error -> default_server()
		end,
	Response = Req:respond({200, [{"Transfer-Encoding", "chunked"}, {"Content-Type", "text/html"}], chunked}),
	fetch_logs(Response, ServerName, dict:to_list(Dict), Max),
	Response:write_chunk("").
		
fetch_logs(Resp, Cont, Opts, Max) ->
	case (catch lrb:fetch(Cont, [{use_cache, false}|Opts])) of
		{'EXIT', Error} ->
			Resp:write_chunk(io_lib:format("cont: ~p, error: ~p~n", [Cont, Error]));
		{Cont1, Logs} when is_record(Cont1, continuation) ->
	        case ?GET_CSTATE(Cont1, num_items) of
			    NumItems when is_integer(NumItems), NumItems < Max ->
			        do_write_chunk(Resp, Logs),
			        fetch_logs(Resp, Cont1, Opts, Max);
			    NumItems when is_integer(NumItems), NumItems >= Max ->
			        PrevNumItems = 
			            if
			                is_record(Cont, continuation) ->
			                    ?GET_CSTATE(Cont, num_items);
			                true ->
			                    0
			            end,
			        Logs2 = 
    			        if
    			            is_integer(PrevNumItems) ->
    			                {Logs1, _} = lists:split(Max - PrevNumItems, Logs),
    			                Logs1;
    			            true ->
    			                Logs
    			        end,
		            do_write_chunk(Resp, Logs2)
			end;
        {_, Logs} ->
			do_write_chunk(Resp, Logs)
	end.

tail_logs(Params, Req) ->
    Dict = opts(Params, dict:new()),
    ServerName =
		case dict:find(server, Dict) of
			{ok, Value} -> Value;
			error -> default_server()
		end,
    Response = Req:respond({200, [{"Transfer-Encoding", "chunked"}, {"Content-Type", "text/html"}], chunked}),
    lr_tail:add_response(ServerName, dict:to_list(Dict), Response),
    receive x -> x end,
    Response:write_chunk("").

do_write_chunk(_, []) -> ok;

do_write_chunk(Resp, Logs) ->
    Content = lr_logs:render({data, Logs}),
    Resp:write_chunk(Content).

default_server() ->
	gen_server:call(hd(pg2:get_members(log_roller_server)), name, 5000).
	
tab_content(ServerName) ->
	Names = [gen_server:call(Pid, name, 5000) || Pid <- pg2:get_members(log_roller_server)],
	lr_tabs:render({data, ServerName, Names}).

opts([], Dict) -> 
	Dict;
opts([{_, []}|Tail], Dict) ->
	opts(Tail, Dict);
opts([{Key,Val}|Tail], Dict) ->
	Key1 = dict_key(Key),
	Val1 = dict_val(Key1, Val),
	case dict:is_key(Key1, Dict) of
		true ->
			opts(Tail, dict:append_list(Key1, Val1, Dict));
		false ->
			opts(Tail, dict:store(Key1, Val1, Dict))
	end.	

%% internal functions
get_arg_value(Key, Args, Default) ->
	case proplists:get_value(Key, Args) of
		undefined -> 
			case application:get_env(log_roller_server, Key) of
				{ok, Val} -> Val;
				undefined -> Default
			end;
		Val -> Val
	end.
	
get_arg_value(doc_root, Args) ->
    case proplists:get_value(doc_root, Args) of
		undefined -> 
			case application:get_env(log_roller_server, doc_root) of
				{ok, Val} -> Val;
				undefined ->
				    case code:lib_dir(log_roller) of
                        {error, bad_name} -> 
                            {ok, Dir} = file:get_cwd(), 
                            Dir ++ "/public";
                        LibDir -> 
                            LibDir ++ "/public"
                    end
			end;
		Val -> Val
	end.
	
dict_key("server") -> server;
dict_key("max") -> max;
dict_key("type") -> types;
dict_key("node") -> nodes;
dict_key("cache") -> cache;
dict_key("grep") -> grep.

dict_val(cache, Val) -> list_to_atom(Val);
dict_val(server, Val) -> list_to_atom(Val);
dict_val(max, Val) -> 
	case (catch list_to_integer(Val)) of
		{'EXIT', _} -> infinity;
		Int -> Int
	end;
dict_val(types, Val) -> [list_to_atom(Val), list_to_atom(Val ++ "_report")];
dict_val(nodes, Val) -> [list_to_atom(Val)];
dict_val(grep, Val) -> Val.

get_nodes(_Server) ->
	lists:sort([node()|nodes()]).