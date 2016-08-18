%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%% A really simple escript to accept RPC request and push them
%%% into a running kazoo virtual machine.
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   Pierre Fenoll
%%%------------------------------------------------------------------
-module(sup).

-export([main/1]).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").

-define(MAX_CHARS, round(math:pow(2012, 80))).


%%% API

-spec main(string()) -> no_return().
main(CommandLineArgs) ->
    main(CommandLineArgs, 0).

main(CommandLineArgs, Loops) ->
    _ = os:cmd("epmd -daemon"),
    _ = net_kernel:stop(),
    case net_kernel:start([my_name(), long_or_short_name()]) of
        {'error', _} when Loops < 3 ->
            stderr("Unable to start command bridge network kernel, try again", []),
            halt(1);
        {'error', _} ->
            main(CommandLineArgs, Loops + 1);
        {'ok', _} ->
            {'ok', Options, Args} = parse_args(CommandLineArgs),
            lists:member('help', Options)
                andalso print_help(),
            Verbose = proplists:get_value('verbose', Options) =/= 'undefined',
            Target = get_target(Options, Verbose),
            Module =
                case proplists:get_value('module', Options) of
                    'undefined' -> print_invalid_cli_args();
                    M -> list_to_atom(M)
                end,
            Function =
                case proplists:get_value('function', Options) of
                    'undefined' -> print_invalid_cli_args();
                    F -> list_to_atom(F)
                end,
            Timeout = case proplists:get_value('timeout', Options) of 0 -> 'infinity'; T -> T * 1000 end,
            Verbose
                andalso stdout("Running ~s:~s(~s)", [Module, Function, string:join(Args, ", ")]),
            case rpc:call(Target, Module, Function, [list_to_binary(Arg) || Arg <- Args], Timeout) of
                {'badrpc', {'EXIT',{'undef', _}}} ->
                    print_invalid_cli_args();
                {'badrpc', Reason} ->
                    String = io_lib:print(Reason, 1, ?MAX_CHARS, -1),
                    stderr("Command failed: ~s", [String]),
                    halt(1);
                'no_return' ->
                    halt(0);
                Result when Verbose ->
                    String = io_lib:print(Result, 1, ?MAX_CHARS, -1),
                    stdout("Result: ~s", [String]),
                    halt(0);
                Result ->
                    String = io_lib:print(Result, 1, ?MAX_CHARS, -1),
                    stdout("~s", [String]),
                    halt(0)
            end
    end.


%%% Internals

-spec get_target(proplist(), boolean()) -> atom().
get_target(Options, Verbose) ->
    Node = proplists:get_value('node', Options),
    Host = get_host(Options),
    Cookie = get_cookie(Options, list_to_atom(Node)),
    Target = list_to_atom(Node ++ "@" ++ Host),
    case net_adm:ping(Target) of
        'pong' ->
            Verbose
                andalso stdout("Connected to service ~s with cookie ~s", [Target, Cookie]),
            Target;
        'pang' ->
            stderr("Connection to service failed!", []),
            print_ping_failed(Target, Cookie)
    end.

-spec get_cookie(proplist(), atom()) -> atom().
get_cookie(Options, Node) ->
    CookieStr =
        case { props:get_value('cookie', Options, "")
             , kazoo_config_init:read_cookie(Node)
             }
        of
            {C, []} when C =/= "" -> C;
            {_, [C]} -> C;
            {"", []} -> print_no_setcookie()
        end,
    Cookie = kz_util:to_atom(CookieStr, 'true'),
    'true' = erlang:set_cookie(node(), Cookie),
    Cookie.

-spec get_host(proplist()) -> nonempty_string().
get_host(Options) ->
    Host = proplists:get_value('host', Options),
    case inet:gethostbyname(Host) of
        {'ok', _} -> Host;
        {'error', 'nxdomain'} ->
            stderr("Unable to resolve host '~s'", [Host]),
            print_unresolvable_host(Host);
        {'error', Reason} ->
            stderr("Unable to resolve host '~s': ~p", [Host, Reason]),
            print_unresolvable_host(Host)
    end.

-spec my_name() -> atom().
my_name() ->
    list_to_atom("sup_" ++ os:getpid() ++ "@" ++ localhost()).

-spec localhost() -> nonempty_string().
localhost() ->
    net_adm:localhost().

-spec long_or_short_name() -> 'longnames' | 'shortnames'.
long_or_short_name() ->
    IsDot = fun ($.) -> 'true'; (_) -> 'false' end,
    case lists:any(IsDot, localhost()) of
        'true' -> 'longnames';
        'false' -> 'shortnames'
    end.

-spec print_invalid_cli_args() -> no_return().
print_invalid_cli_args() ->
    stderr("Invalid command or wrong number of arguments, please try again", []),
    halt(1).

-spec parse_args(string()) -> {'ok', proplist(), list()}.
parse_args(CommandLineArgs) ->
    case getopt:parse(option_spec_list(), CommandLineArgs) of
        {'ok', {Options, Args}} when is_list(Options) ->
            {'ok', Options, Args};
        {'ok', {_, _}} ->
            print_help();
        {'error', {_, _}} ->
            print_help()
    end.

-spec print_no_setcookie() -> no_return().
print_no_setcookie() ->
    stdout("Unable to automatically determine cookie", []),
    stdout("Please provide the cookie of the node you are connecting to", []),
    stdout("`sup -c <cookie>`", []),
    halt(1).

-spec print_ping_failed(string(), atom()) -> no_return().
print_ping_failed(Target, Cookie) ->
    stdout("Failed to connect to service ~s with cookie ~s", [Target, Cookie]),
    stdout("  Possible fixes:", []),
    stdout("    * Ensure the Kazoo service you are trying to connect to is running on the host", []),
    stdout("    * Ensure that you are using the same cookie as the Kazoo node, `sup -c <cookie>`", []),
    stdout("    * Verify that the hostname being used is a Kazoo node", []),
    halt(1).

-spec print_unresolvable_host(string()) -> no_return().
print_unresolvable_host(Host) ->
    stdout("If you cannot run `ping ~s` then this program will not be able to connect.", [Host]),
    stdout("  Possible fixes:", []),
    stdout("    * Use `sup -h <hostname>` argument of this script to specify a different host", []),
    stdout("    * Add \"{IP_OF_KAZOO_NODE}  ~s\" to your /etc/hosts file", [Host]),
    stdout("    * Create a DNS record for \"~s\"", [Host]),
    halt(1).

-spec print_help() -> no_return().
print_help() ->
    getopt:usage(option_spec_list(), "sup", "[args ...]"),
    halt(1).

-spec stdout(string(), list()) -> 'ok'.
stdout(Format, Things) ->
    io:format('standard_io', Format++"\n", Things).

-spec stderr(string(), list()) -> 'ok'.
stderr(Format, Things) ->
    io:format('standard_error', Format++"\n", Things).

-spec option_spec_list() -> list().
option_spec_list() ->
    [{'help', $?, "help", 'undefined', "Show the program options"}
    ,{'host', $h, "host", {'string', localhost()}, "System hostname"}
    ,{'node', $n, "node", {'string', "kazoo_apps"}, "Node name"}
    ,{'cookie', $c, "cookie", {'string', "change_me"}, "Erlang cookie"}
    ,{'timeout', $t, "timeout", {'integer', 0}, "Command timeout"}
    ,{'verbose', $v, "verbose", 'undefined', "Be verbose"}
    ,{'module', 'undefined', 'undefined', 'string', "The name of the remote module"}
    ,{'function', 'undefined', 'undefined', 'string', "The name of the remote module's function"}
    ].

%%% End of Module
