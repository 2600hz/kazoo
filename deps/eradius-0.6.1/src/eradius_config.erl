-module(eradius_config).
% Eradius API's:
-export([validate_new_config/0, validate_new_config/2, validate_config/1]).
% Config validating API functions:
-export([get_app_env/2, validate_ip/1, validate_port/1, validate_ports/1,
         map_helper/3, map_helper/2, ok_error_helper/2, validate_secret/1]).

%% ------------------------------------------------------------------------------------------
%% -- config validation
-define(pos_int(X), is_integer(X), X >= 0).
-define(ip4_address_num(X), ?pos_int(X), X < 256).
-define(ip4_address(T), ?ip4_address_num(element(1, T)), ?ip4_address_num(element(2, T)),
                        ?ip4_address_num(element(3, T)), ?ip4_address_num(element(4, T))).
-define(valid_atom(Value), Value =/= invalid).
-define(valid(X), is_tuple(X), ?valid_atom(element(1, X))).
-define(is_io(IO), is_list(IO) orelse is_binary(IO)).
-define(invalid(ErrorMsg, ErrorValue), {invalid, io_lib:format(ErrorMsg, ErrorValue)}).

validate_new_config() ->
    validate_new_config(get_app_env(servers), get_app_env(session_nodes)).

validate_new_config({invalid, _} = Invalid, _Nodes) -> Invalid;
validate_new_config(_Servers, {invalid, _} = Invalid) -> Invalid;
validate_new_config(Servers, Nodes) ->
    validate_new_config_start(Servers, check_root(Nodes)).

validate_new_config_start(_Servers, {invalid, _} = Invalid) -> Invalid;
validate_new_config_start(Servers, Nodes) ->
    map_helper(fun(Server) -> validate_new_server_config(Server, Nodes) end, Servers, flatten).

validate_new_server_config({Name, {IP, ListOfPorts}}, Nodes) ->
    validate_new_server_config(get_app_env(Name), validate_ip(IP), validate_ports(ListOfPorts), Nodes).

validate_new_server_config({invalid, _} = Invalid, _IP, _ListOfPorts, _Nodes) -> Invalid;
validate_new_server_config(_NasList, {invalid, _} = Invalid, _ListOfPorts, _Nodes) -> Invalid;
validate_new_server_config(_NasList, _IP, {invalid, _} = Invalid, _Nodes) -> Invalid;
validate_new_server_config(NasList, IP, ListOfPorts, Nodes) ->
    case validate_new_nas_list(NasList, {IP, ListOfPorts, Nodes}) of
        {invalid, _} = Invalid ->
            Invalid;
        Values ->
            lists:map(fun(Port) -> {{IP, Port}, Values} end, ListOfPorts)
    end.

validate_new_nas_list(NasLists, ServerConfig) ->
    map_helper(fun(NasList) -> validate_behavior_naslist(NasList, ServerConfig) end, NasLists, flatten).

validate_behavior_naslist({Behavior, ListOfNases}, {_IP, _ListOfPorts, Nodes}) ->
    validate_behavior_nases(validate_behavior(Behavior), validate_naslist(ListOfNases, Nodes)).

validate_behavior_nases({invalid, _} = Invalid, _) -> Invalid;
validate_behavior_nases(_, {invalid, _} = Invalid) -> Invalid;
validate_behavior_nases(Behavior, Nases) ->
    build_nas_behavior_list(Behavior, Nases).

validate_behavior({Nas, Args}) ->
    validate_behavior({get_app_env(radius_callback), Nas, Args});
validate_behavior({{invalid, _} = Invalid, _Nas, _Args}) ->
    Invalid;
validate_behavior({Module, Nas, _Args} = Value) when is_atom(Module) andalso ?is_io(Nas) ->
    Value;
validate_behavior({Module, _, _}) when is_atom(Module) ->
    ?invalid("bad NAS Id in Behavior specifification: ~p", [Module]);
validate_behavior({Module, _, _}) ->
    ?invalid("bad module in Behavior specifification: ~p", [Module]);
validate_behavior(Term) ->
    ?invalid("bad Term in Behavior specifification: ~p", [Term]).

validate_naslist(ListOfNases, Nodes) -> map_helper(fun(Nas) -> validate_nas(Nas, Nodes) end, ListOfNases).

validate_nas({IP, Secret}, Nodes) ->
    validate_nas({IP, Secret, []}, Nodes);
validate_nas({IP, Secret, Options}, Nodes) ->
    validate_nas({proplists:get_value(nas_id, Options), IP, Secret, proplists:get_value(group, Options)}, Nodes);
validate_nas({NasId, IP, Secret, undefined}, {root, Nodes}) ->
    validate_nas(NasId, validate_ip(IP), Secret, root, Nodes);
validate_nas({NasId, IP, Secret, GroupName}, Nodes) when is_list(Nodes) ->
    validate_nas(NasId, validate_ip(IP), Secret, GroupName, proplists:get_value(GroupName, Nodes));
validate_nas(Term, _) ->
    ?invalid("bad term in NAS specification: ~p", [Term]).

validate_nas(_NasId, {invalid, _} = Invalid, _Secret, _Name, _Nodes) -> Invalid;

validate_nas(NasId, IP, Secret, Name, undefined) ->
    validate_nas(NasId, IP, Secret, Name, validate_handler_nodes(Name));
validate_nas(_NasId, IP, _Secret, Name, {invalid, _}) ->
    ?invalid("group ~p for nas ~p is undefined", [Name, IP]);
validate_nas(NasId, IP, Secret, _Name, Nodes) when ?is_io(Secret) andalso (?is_io(NasId) orelse NasId == undefined) ->
    {NasId, IP, validate_secret(Secret), Nodes};
validate_nas(NasId, _IP, Secret, _Name, _) when ?is_io(Secret) ->
    ?invalid("bad nas id name: ~p", [NasId]);
validate_nas(_NasId, _IP, Secret, _Name, _) ->
    ?invalid("bad RADIUS secret: ~p", [Secret]).

% --------------------------------------------------------------------------------------------------
% -- direct validation function

validate_ip(IP) when is_list(IP) ->
    ok_error_helper(inet_parse:address(IP), {"bad IP address: ~p", [IP]});
validate_ip(IP) when ?ip4_address(IP) ->
    IP;
validate_ip(X) ->
    ?invalid("bad IP address: ~p", [X]).

validate_ports(Ports) -> map_helper(fun validate_port/1, Ports).
validate_port(Port) when is_list(Port) -> validate_port(catch list_to_integer(Port));
validate_port(Port) when ?pos_int(Port) -> Port;
validate_port(Port) when is_integer(Port) -> ?invalid("port number out of range: ~p", [Port]);
validate_port(Port) -> ?invalid("bad port number: ~p", [Port]).

check_root([First | _] = AllNodes) when is_tuple(First) ->
    map_helper(fun({Name, List}) ->
                       case validate_handler_nodes(List) of
                           {invalid, _} = Invalid ->
                               Invalid;
                           Value ->
                               {Name, Value}
                       end
               end, AllNodes);
check_root(Nodes) ->
    case validate_handler_nodes(Nodes) of
        {invalid, _} = Invalid ->
            Invalid;
        Values ->
            {root, Values}
    end.

% --------------------------------------------------------------------------------------------------
% -- build right format function

build_nas_behavior_list({Module, Nas, Args}, ListOfNases) ->
    lists:map(fun({undefined, IP, Secret, Nodes}) ->
                      {build_nasname(Nas, IP), IP, Secret, Nodes, Module, Args};
                 ({NasName, IP, Secret, Nodes}) ->
                      {NasName, IP, Secret, Nodes, Module, Args}
              end, ListOfNases).

build_nasname(Nas, IP) ->
    NasBinary = tob(Nas),
    IPString = inet_parse:ntoa(IP),
    <<NasBinary/binary, "_", (list_to_binary(IPString))/binary>>.

tob(Integer) when is_integer(Integer) -> tob(integer_to_list(Integer));
tob(List) when is_list(List) -> list_to_binary(List);
tob(Binary) -> Binary.

-type valid_nas()    :: {inet:ip_address(), binary(), list(atom()), module(), term()}.
-type valid_server() :: {eradius_server_mon:server(), list(valid_nas())}.
-type valid_config() :: list(valid_server()).

-spec validate_config(list(term())) -> valid_config() | {invalid, io_lib:chars()}.
validate_config(Config) ->
    case Config of
        [Server | _] ->
            case Server of
                {List, SecondList} when is_list(List) and is_list(SecondList) ->
                    validate_server_config(dedup_keys(Config));
                %% Check format of new command
                {_Name, ServerConf} when is_tuple(ServerConf) ->
                    validate_new_config()
            end;
        [] ->
            validate_server_config(dedup_keys(Config))
    end.

-spec validate_server_config(list(term())) -> valid_config() | {invalid, io_lib:chars()}.
validate_server_config([]) ->
    [];
validate_server_config([{Server, NasList} | ConfigRest]) ->
    case validate_server(Server) of
        E = {invalid, _} ->
            E;
        ValidServer ->
            case validate_nas_list(NasList) of
                E = {invalid, _} ->
                    E;
                ValidNasList ->
                    case validate_server_config(ConfigRest) of
                        E = {invalid, _} ->
                            E;
                        ValidConfigRest ->
                            [{ValidServer, ValidNasList} | ValidConfigRest]
                    end
            end
    end;
validate_server_config([InvalidTerm | _ConfigRest]) -> ?invalid("bad term in server list: ~p", [InvalidTerm]).

validate_server({IP, Port}) when is_list(Port) ->
    case (catch list_to_integer(Port)) of
        {'EXIT', _} ->
            {invalid, io_lib:format("bad port number: ~p", [Port])};
        Num when ?pos_int(Num) ->
            validate_server({IP, Num});
        Num ->
            {invalid, io_lib:format("port number out of range: ~p", [Num])}
    end;
validate_server({IP, Port}) when is_list(IP), ?pos_int(Port) ->
    case inet_parse:ipv4_address(IP) of
        {ok, Address} ->
            {Address, Port};
        {error, einval} ->
            {invalid, io_lib:format("bad IP address: ~p", [IP])}
    end;
validate_server({IP, Port}) when ?ip4_address(IP), ?pos_int(Port) ->
    {IP, Port};
validate_server(String) when is_list(String) ->
    %% TODO: IPv6 address support
    case string:tokens(String, ":") of
        [IP, Port] ->
            validate_server({IP, Port});
        _ ->
            {invalid, io_lib:format("bad address/port combination: ~p", [String])}
    end;
validate_server(X) ->
    {invalid, io_lib:format("bad address/port combination: ~p", [X])}.

validate_nas_list([]) ->
    [];
validate_nas_list([{NasAddress, Secret, HandlerNodes, Module, Args} | NasListRest]) when is_list(NasAddress) ->
    case inet_parse:ipv4_address(NasAddress) of
        {ok, ValidAddress} ->
            validate_nas_list([{ValidAddress, Secret, HandlerNodes, Module, Args} | NasListRest]);
        {error, einval} ->
            {invalid, io_lib:format("bad IP address in NAS specification: ~p", [NasAddress])}
    end;
validate_nas_list([{NasAddress, Secret, HandlerNodes, Module, Args} | NasListRest]) when ?ip4_address(NasAddress) ->
    case validate_secret(Secret) of
        E = {invalid, _} ->
            E;
        ValidSecret ->
            case validate_handler_nodes(HandlerNodes) of
                E = {invalid, _} ->
                    E;
                ValidHandlerNodes ->
                    case Module of
                        _ when is_atom(Module) ->
                            case validate_nas_list(NasListRest) of
                                E = {invalid, _} ->
                                    E;
                                ValidNasListRest ->
                                    [{build_nasname("", NasAddress), NasAddress, ValidSecret, ValidHandlerNodes, Module, Args} | ValidNasListRest]
                            end;
                        _Else ->
                            {invalid, io_lib:format("bad module in NAS specifification: ~p", [Module])}
                    end
            end
    end;
validate_nas_list([{InvalidAddress, _, _, _, _} | _NasListRest]) ->
    {invalid, io_lib:format("bad IP address in NAS specification: ~p", [InvalidAddress])};
validate_nas_list([OtherTerm | _NasListRest]) ->
    {invalid, io_lib:format("bad term in NAS specification: ~p", [OtherTerm])}.

validate_secret(Secret) when is_list(Secret) ->
    unicode:characters_to_binary(Secret);
validate_secret(Secret) when is_binary(Secret) ->
    Secret;
validate_secret(OtherTerm) ->
    {invalid, io_lib:format("bad RADIUS secret: ~p", [OtherTerm])}.

validate_handler_nodes(local) ->
    local;
validate_handler_nodes("local") ->
    local;
validate_handler_nodes([]) ->
    {invalid, "empty node list"};
validate_handler_nodes(NodeL) when is_list(NodeL) ->
    validate_node_list(NodeL);
validate_handler_nodes(OtherTerm) ->
    {invalid, io_lib:format("bad node list: ~p", [OtherTerm])}.

validate_node_list([]) ->
    [];
validate_node_list([Node | Rest]) when is_atom(Node) ->
    case validate_node_list(Rest) of
        E = {invalid, _} ->
            E;
        ValidRest ->
            [Node | ValidRest]
    end;
validate_node_list([OtherTerm | _]) ->
    {invalid, io_lib:format("bad term in node list: ~p", [OtherTerm])}.

dedup_keys(Proplist) ->
    dedup_keys1(lists:keysort(1, Proplist)).

dedup_keys1(Proplist) ->
    lists:foldr(fun ({K, V1}, [{K, V2} | R]) ->
                        [{K, plmerge(V1, V2)} | R];
                    ({K, V}, R) ->
                        [{K, V} | R]
                end, [], Proplist).

plmerge(List1, List2) ->
    M1 = [{K, V} || {K, V} <- List1, not proplists:is_defined(K, List2)],
    lists:keysort(1, M1 ++ List2).

% --------------------------------------------------------------------------------------------------
% -- helpers

get_app_env(Env) ->
    get_app_env(eradius, Env).
get_app_env(App, Env) ->
    case application:get_env(App, Env) of
        {ok, Value} ->
            Value;
        _ ->
            ?invalid("config parameter: ~p is undefined for application ~p", [Env, App])
    end.

map_helper(Fun, Values) ->
    map_helper(Fun, Values, no).
map_helper(Fun, Values, Type) ->
    map_helper(Fun, Values, Type, []).
map_helper(_Fun, [], _Type, Values) -> lists:reverse(Values);
map_helper(Fun, [Head | Tail], Type, Values) ->
    case Fun(Head) of
        {invalid, _} = Error ->
            Error;
        Result when Type =/= no andalso is_list(Result) ->
            map_helper(Fun, Tail, Type, Result ++ Values);
        Result ->
            map_helper(Fun, Tail, Type, [Result | Values])
    end.

ok_error_helper({error, _Error}, {Msg, Value}) when is_list(Msg) -> ?invalid(Msg, Value);
ok_error_helper({error, _Error}, ErrorMsg) when is_list(ErrorMsg) -> ErrorMsg;
ok_error_helper({ok, Value}, _ErrorMessage) -> Value;
ok_error_helper(Value, _ErrorMessage) -> Value.
