%% -----------------------------------------------------------------------------
%%
%% Hamcrest Erlang.
%%
%% Copyright (c) 2010 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2010 Tim Watson.
%%
%% Generates hamcrest.hrl header file during build process. Loaded by rebar.
%% -----------------------------------------------------------------------------
-module(header_generator).
-export([post_compile/2]).

post_compile(_, AppFile) ->
    case lists:suffix("hamcrest.app", AppFile) of
        true ->
            code:add_patha(filename:join(rebar_utils:get_cwd(), "ebin")),
            Exports = [ F || F <- hamcrest_matchers:module_info(exports),
                             F /= module_info ],
            rebar_log:log(debug, "Adding header exports/imports: ~p~n", [Exports]),
            Imports = [ io_lib:format("~s/~p", [F,A]) || {F, A} <- Exports ],
            Expr = lists:flatten(lists:foldl(
                fun(In, Acc) -> lists:concat([In, ", ", Acc]) end, "", Imports)),
            ImportList = string:substr(Expr, 1, length(Expr) - 2),
            PWD = rebar_utils:get_cwd(),
            Path = filename:join(PWD, "priv/build/templates/hamcrest.hrl.src"),
            {ok, Bin} = file:read_file(Path),
            Res = rebar_templater:render(Bin, dict:from_list([{imports, ImportList}])),
            ResBin = erlang:iolist_to_binary(Res),
            Dest = filename:absname(filename:join(["include", "hamcrest.hrl"])),
            case file:read_file(Dest) of
                {ok, ResBin} ->
                    rebar_log:log(info, "Header file(s) untouched.~n", []),
                    ok;
                _ ->
                    case file:write_file(Dest, ResBin) of
                        ok ->
                            rebar_log:log(info, "Header file(s) generated.~n", []),
                            ok;
                        {error, WriteError} ->
                            rebar_utils:abort("Failed to write ~p: ~p~n", [Dest, WriteError])
                    end
            end;
        false ->
            ok
    end.
