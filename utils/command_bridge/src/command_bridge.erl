%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% A really simple escript to accept RPC request and push them
%%% into a running whistle virtual machine.
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%------------------------------------------------------------------
-module(command_bridge).

-include_lib("whistle/include/wh_types.hrl").

-export([main/1]).

-define(WHAPPS_VM_ARGS, "/opt/whistle/whistle/whistle_apps/conf/vm.args").
-define(ECALL_VM_ARGS, "/opt/whistle/whistle/ecallmgr/conf/vm.args").
-define(MAX_CHARS, round(math:pow(2012, 80))).

-spec main/1 :: (string()) -> no_return().
main(CommandLineArgs) ->
    main(CommandLineArgs, 0).

main(CommandLineArgs, Loops) ->
    os:cmd("epmd -daemon"),
    net_kernel:stop(),
    case net_kernel:start([my_name(), longnames]) of
        {error, _} when Loops < 3 ->
            io:format(standard_error, "Unable to start command bridge network kernel, try again~n", []),
            halt(1);
        {error, _} ->
            main(CommandLineArgs, Loops + 1);
        _Else ->
            {ok, Options, Args} = parse_args(CommandLineArgs),
            Verbose = proplists:get_value(verbose, Options) =/= undefined,
            Target = get_target(Options, Verbose),
            Module = list_to_atom(proplists:get_value(module, Options, "nomodule")),
            Function = list_to_atom(proplists:get_value(function, Options, "nofunction")),
            Timeout = proplists:get_value(timeout, Options, 5) * 1000,
            Verbose andalso io:format(standard_io, "Running ~s:~s(~s)~n", [Module, Function, string:join(Args, ", ")]),
            case rpc:call(Target, Module, Function, [list_to_binary(Arg) || Arg <- Args], Timeout) of
                {badrpc, {'EXIT',{undef, _}}} ->
                    io:format(standard_error, "Invalid command or wrong number of arguments, please try again~n", []),
                    halt(1);
                {badrpc, Reason} ->
                    String = io_lib:print(Reason, 1, ?MAX_CHARS, -1),
                    io:format(standard_error, "Command failed: ~s~n", [String]),
                    halt(1);
                Result when Verbose ->
                    String = io_lib:print(Result, 1, ?MAX_CHARS, -1),
                    io:format(standard_io, "Result: ~s~n", [String]),
                    erlang:halt(0);
                Result ->
                    String = io_lib:print(Result, 1, ?MAX_CHARS, -1),
                    io:format(standard_io, "~s~n", [String]),
                    erlang:halt(0)
            end
    end.

-spec get_target/2 :: (proplist(), boolean()) -> atom().
get_target(Options, Verbose) ->
    Node = proplists:get_value(node, Options),
    Host = get_host(Options),
    Cookie = get_cookie(Options, Node),
    Target = list_to_atom(Node ++ "@" ++ Host),
    case net_adm:ping(Target) of
        pong ->
            Verbose andalso io:format(standard_io, "Connected to service '~s' with cookie '~s'~n", [Target, Cookie]),
            Target;
        pang ->
            io:format(standard_error, "Connection to service failed!~n", []),
            print_ping_failed(Target, Cookie);
        Else ->
            io:format(standard_error, "Connection to service failed: ~p~n", [Else]),
            print_ping_failed(Target, Cookie)
    end.

-spec get_cookie/2 :: (proplist(), string()) -> 'ok'.
get_cookie(Options, Node) ->
    Cookie = case {Node, proplists:get_value(cookie, Options, "")} of
                 {"whistle_apps", ""} -> get_cookie_from_vmargs(?WHAPPS_VM_ARGS);
                 {"ecallmgr", ""} -> get_cookie_from_vmargs(?ECALL_VM_ARGS);
                 {_, ""} -> print_no_setcookie();
                 {_, C} -> C
             end,
    erlang:set_cookie(node(), list_to_atom(Cookie)),
    Cookie.

-spec get_cookie_from_vmargs/1 :: (string()) -> string().
get_cookie_from_vmargs(File) ->
    case file:read_file(File) of
        {error, _} -> print_no_setcookie();
        {ok, Bin} ->
            case re:run(Bin, <<"-setcookie (.*)\\n">>, [{capture, [1], list}]) of
                {match, [Cookie]} -> Cookie;
                _Else -> print_no_setcookie()
            end
    end.

-spec get_host/1 :: (proplist()) -> string().
get_host(Options) ->
    Host = proplists:get_value(host, Options),
    case inet:gethostbyname(Host) of
        {ok, _} -> Host;
        {error,nxdomain} ->
            io:format(standard_error, "Unable to resolve host '~s'~n", [Host]),
            print_unresolvable_host(Host);
        {error, Reason} ->
            io:format(standard_error, "Unable to resolve host '~s': ~p~n", [Host, Reason]),
            print_unresolvable_host(Host)
    end.

-spec my_name/0 :: () -> atom().
my_name() ->
    Localhost = net_adm:localhost(),
    TimeStamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Name = "command_bridge_" ++ integer_to_list(TimeStamp) ++ "@" ++ Localhost,
    list_to_atom(Name).

-spec parse_args/1 :: (string()) -> {'ok', proplist(), list()}.
parse_args(CommandLineArgs) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, CommandLineArgs) of
        {ok, {Options, _}} when not is_list(Options) ->
            display_help(1);
        {ok, {Options, Args}} ->
            {ok, Options, Args};
        {error, {_, _}} ->
            display_help(1)
    end.

-spec print_no_setcookie/0 :: () -> no_return().
print_no_setcookie() ->
    io:format(standard_io, "Unable to automatically determine cookie~n", []),
    io:format(standard_io, "Please provide the cookie of the node you are connecting to~n", []),
    io:format(standard_io, "\"./command_bridge -c <cookie>\"~n", []),
    halt(1). 

-spec print_ping_failed/2 :: (string(), string()) -> no_return(). 
print_ping_failed(Target, Cookie) ->
    io:format(standard_io, "Failed to connect to service '~s' with cookie '~s'~n", [Target, Cookie]),
    io:format(standard_io, "  Possible fixes:~n", []),
    io:format(standard_io, "    * Ensure the whistle service you are trying to connect to is running on the host~n", []),
    io:format(standard_io, "    * Ensure that you are using the same cookie as the whistle node, \"./command_bridge -c <cookie>\"~n", []),
    io:format(standard_io, "    * Verify that the hostname being used is a whistle node~n", []),
    halt(1).

-spec print_unresolvable_host/1 :: (string()) -> no_return(). 
print_unresolvable_host(Host) ->
    io:format(standard_io, "If you can not run \"ping ~s\" then this program will not be able to connect.~n", [Host]),
    io:format(standard_io, "  Possible fixes:~n", []),
    io:format(standard_io, "    * Use \"./command_bridge -h <hostname>\" argument of this script to specify a different host~n", []),
    io:format(standard_io, "    * Add \"{IP_OF_WHISTLE_NODE}  ~s\" to your /etc/hosts file~n", [Host]),
    io:format(standard_io, "    * Create a DNS record for \"~s\"~n", [Host]),
    halt(1).

-spec display_help/1 :: (non_neg_integer()) -> no_return().
display_help(Return) ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "command_bridge", "[args ...]"),
    erlang:halt(Return).

-spec option_spec_list/0 :: () -> proplist().
option_spec_list() ->
    [{help, $?, "help", undefined, "Show the program options"},
     {host, $h, "host", {string, net_adm:localhost()}, "System hostname, defaults to system hostname"},
     {node, $n, "node", {string, "whistle_apps"}, "Node name, default \"whistle_apps\""},
     {cookie, $c, "cookie", {string, ""}, "Erlang cookie"},
     {timeout, $t, "timeout", {integer, 5}, "Command timeout, default 5"},
     {verbose, $v, "verbose", undefined, "Be verbose"},
     {module, undefined, undefined, string, "The name of the remote module"},
     {function, undefined, undefined, string, "The name of the remote module's function"}
    ].
