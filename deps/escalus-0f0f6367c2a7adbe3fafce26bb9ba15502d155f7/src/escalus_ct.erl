-module(escalus_ct).

%% @doc This module abstracts away the calls to Common Test.
%%
%% The module doesn't try to simulate all of them as, in general,
%% the user should be aware whether the code he/she writes is run
%% within a Common Test suite or not.
%%
%% Where applicable functions return false if Common Test is not available
%% and the value of the inner call to Common Test otherwise.
%% @end

-export([add_log_link/3,
         fail/1,
         get_config/1,
         is_ct_available/0]).

%% What about that?
-export([rpc_call/6]).

-define(APPNAME, escalus).

-spec add_log_link(any(), any(), any()) -> ok | false.
add_log_link(Heading, File, Type) ->
    is_ct_available() andalso ct_logs:add_link(Heading, File, Type).

-spec fail(any()) -> no_return().
fail(Reason) ->
    case is_ct_available() of
        true -> ct:fail(Reason);
        false -> error({escalus_error, Reason})
    end.

-spec get_config(any()) -> any() | undefined | no_return().
get_config(Required) ->
    case is_ct_available() of
        true -> ct:get_config(Required);
        false -> consult_config_file(Required)
    end.

%% If performance becomes an issue the result of file:consult/1
%% might be cached and lists:keyfind/3 used in place of proplists:get_value/2
%% (watch out - these return different values on lookup failure).
consult_config_file(Option) ->
    case application:get_env(?APPNAME, config_file) of
        undefined ->
            error({escalus_error, no_config_file});
        {ok, ConfigFile} ->
            Path = interpret_config_file_path(ConfigFile),
            {ok, Config} = file:consult(Path),
            proplists:get_value(Option, Config)
    end.

interpret_config_file_path("/" ++ _ = AbsPath) ->
    AbsPath;
interpret_config_file_path(RelPath) ->
    case code:is_loaded(?MODULE) of
        {file, EscalusBeamPath} ->
            GetProjectDir = fun(Path) ->
                                    filename:dirname(filename:dirname(Path))
                            end,
            ProjectDir = GetProjectDir(EscalusBeamPath),
            filename:join([ProjectDir, RelPath]);
        _ ->
            error({escalus_error, beam_not_loaded})
    end.

rpc_call(Node, Module, Function, Args, TimeOut, Cookie) ->
    case is_ct_available() of
        true ->
            ct_rpc:call(Node, Module, Function, Args, TimeOut, Cookie);
        false ->
            %% TODO: don't error out, should be easy to simulate ct_rpc:call/6
            error({escalus_error, common_test_unavailable})
    end.

-spec is_ct_available() -> boolean().
is_ct_available() ->
    case application:get_env(?APPNAME, common_test) of
        %% For a transitional period let's assume that unless
        %% {common_test, false} is defined, Common Test is available.
        undefined ->
            true;
        {ok, true} ->
            true;
        _ ->
            false
    end.
