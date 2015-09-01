%%==============================================================================
%% Copyright 2014 Ulf Wiger
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
%% @author Ulf Wiger <ulf@wiger.net>
%% @copyright 2014-2015 Ulf Wiger
%% @end
%% =============================================================================
%% Modified 2012 by Beads Land-Trujillo:  get_git_branch/0, redirect_href/3
%% =============================================================================

%% @doc EDoc Doclet module for producing Markdown.

-module(edown_doclet).

-export([run/2]).

-import(edoc_report, [report/2, warning/2]).

-include_lib("edoc/include/edoc_doclet.hrl").

-define(EDOC_APP, edoc).
-define(DEFAULT_FILE_SUFFIX, ".md").
-define(INDEX_FILE, "README.md").
-define(OVERVIEW_FILE, "overview.edoc").
-define(OVERVIEW_SUMMARY, "overview-summary.md").
-define(STYLESHEET, "stylesheet.css").
-define(IMAGE, "erlang.png").
-define(NL, "\n").

-include_lib("xmerl/include/xmerl.hrl").

%% Sources is the list of inputs in the order they were found.
%% Modules are sorted lists of atoms without duplicates. (They
%% usually include the data from the edoc-info file in the target
%% directory, if it exists.)

%% @spec (Command::doclet_gen() | doclet_toc(), edoc_context()) -> ok
%% @doc Main doclet entry point.
%%
%% Also see {@link //edoc/edoc:layout/2} for layout-related options, and
%% {@link //edoc/edoc:get_doc/2} for options related to reading source
%% files.
%%
%% Options:
%% <dl>
%%  <dt>{@type {file_suffix, string()@}}
%%  </dt>
%%  <dd>Specifies the suffix used for output files. The default value is
%%      `".md"'.
%%  </dd>
%%  <dt>{@type {hidden, bool()@}}
%%  </dt>
%%  <dd>If the value is `true', documentation of hidden modules and
%%      functions will also be included. The default value is `false'.
%%  </dd>
%%  <dt>{@type {overview, //edoc/edoc:filename()@}}
%%  </dt>
%%  <dd>Specifies the name of the overview-file. By default, this doclet
%%      looks for a file `"overview.edoc"' in the target directory.
%%  </dd>
%%  <dt>{@type {private, bool()@}}
%%  </dt>
%%  <dd>If the value is `true', documentation of private modules and
%%      functions will also be included. The default value is `false'.
%%  </dd>
%%  <dt>{@type {stylesheet, string()@}}
%%  </dt>
%%  <dd>Specifies the URI used for referencing the stylesheet. The
%%      default value is `"stylesheet.css"'. If an empty string is
%%      specified, no stylesheet reference will be generated.
%%  </dd>
%%  <dt>{@type {stylesheet_file, //edoc/edoc:filename()@}}
%%  </dt>
%%  <dd>Specifies the name of the stylesheet file. By default, this
%%      doclet uses the file `"stylesheet.css"' in the `priv'
%%      subdirectory of the EDoc installation directory. The named file
%%      will be copied to the target directory.
%%  </dd>
%%  <dt>{@type {title, string()@}}
%%  </dt>
%%  <dd>Specifies the title of the overview-page.
%%  </dd>
%% </dl>

%% INHERIT-OPTIONS: title/2
%% INHERIT-OPTIONS: sources/5
%% INHERIT-OPTIONS: overview/4
%% INHERIT-OPTIONS: copy_stylesheet/2
%% INHERIT-OPTIONS: stylesheet/1

run(#doclet_gen{}=Cmd, Ctxt) ->
    gen(Cmd#doclet_gen.sources,
	Cmd#doclet_gen.app,
	modules(Cmd),
	Ctxt);
run(#doclet_toc{}=Cmd, Ctxt) ->
    toc(Cmd#doclet_toc.paths, Ctxt).

gen(Sources, App, Modules, Ctxt) ->
    Dir = Ctxt#context.dir,
    Env = Ctxt#context.env,
    Options0 = Ctxt#context.opts,
    Options = set_app_default([{layout,edown_layout} |
			       Options0] ++
				  [{file_suffix,".md"}]),

    Title = title(App, Options),
    %% CSS = stylesheet(Options),
    {Modules1, Error} = sources(Sources, Dir, Modules, Env, Options),
    Data = overview(Dir, Title, Env, Options),
    Text = edown_lib:export(Data, Options),
    write_file(Text, Dir, right_suffix(?INDEX_FILE, Options)),
    write_info_file(App, Modules1, Dir),
    copy_stylesheet(Dir, Options),
    copy_image(Dir, Options),
    make_top_level_README(Data, Options),
    %% handle postponed error during processing of source files
    case Error of
	true -> exit(error);
	false -> ok
    end.

right_suffix(File, Options) ->
    Base = filename:basename(File, filename:extension(File)),
    Ext = proplists:get_value(file_suffix, Options),
    Base ++ Ext.

set_app_default(Opts) ->
    case lists:keyfind(app_default,1,Opts) of
	false ->
	    [{app_default, "http://www.erlang.org/doc/man"}|Opts];
	_ ->
	    Opts
    end.

make_top_level_README(Data, Options) ->
    case proplists:get_value(top_level_readme, Options) of
	undefined ->
	    ok;
	{Path, BaseHRef} ->
            Dir = filename:dirname(Path),
            Filename = filename:basename(Path),
	    make_top_level_README(Data, Dir, Filename, BaseHRef,
                                  get_git_branch(), Options);
	{Path, BaseHRef, Branch} ->
            Dir = filename:dirname(Path),
            Filename = filename:basename(Path),
	    make_top_level_README(Data, Dir, Filename, BaseHRef, Branch, Options)
    end.

target(Options) ->
    proplists:get_value(edown_target, Options, github).

%% make_top_level_README(Data, Dir, F, BaseHRef) ->
%%     Branch = get_git_branch(),
%%     make_top_level_README(Data, Dir, F, BaseHRef, Branch).

make_top_level_README(Data, Dir, F, BaseHRef, Branch, Options) ->
    Target = target(Options),
    Exp = [xmerl_lib:expand_element(D) || D <- Data],
    New = [xmerl_lib:mapxml(
	     fun(#xmlElement{name = a,
			     attributes = Attrs} = E) ->
		     case redirect_href(Attrs, Branch, BaseHRef, Target) of
			 {true, Attrs1} ->
			     E#xmlElement{attributes = Attrs1};
			 false ->
			     E
		     end;
		(Other) ->
		     Other
	     end, Exp1) || Exp1 <- Exp],
    Text = edown_lib:export(New, Options),
    write_file(Text, Dir, F).

redirect_href(Attrs, Branch, BaseHRef, Target) ->
    {Prefix, URIArgs} = href_redirect_parts(Target, BaseHRef, Branch),
    case lists:keyfind(href, #xmlAttribute.name, Attrs) of
	false ->
	    false;
	#xmlAttribute{value = "/" ++ _} ->
	    false;
	#xmlAttribute{value = Href} = A ->
	    case re:run(Href, ":", []) of
		{match, _} ->
		    false;
		nomatch ->
                    case Href of
                        [$# | _]	->
                            HRef1 = do_redirect(?INDEX_FILE ++ Href,
                                                Prefix, URIArgs);
                        _Else ->
                            HRef1 = do_redirect(Href, Prefix, URIArgs)
                    end,
		    {true,
		     lists:keyreplace(
		       href, #xmlAttribute.name, Attrs,
		       A#xmlAttribute{value = HRef1})}
	    end
    end.

href_redirect_parts(github, BaseHRef, Branch) ->
    {BaseHRef ++ "/blob/" ++ Branch ++ "/", []};
href_redirect_parts(stash, BaseHRef, Branch) ->
    {BaseHRef ++ "/browse/", "?at=refs/heads/" ++ Branch}.


do_redirect(Href, Prefix, Args) ->
    case filename:split(Href) of
	[_] ->
	    Prefix ++ "doc/" ++ Href ++ Args;
	_ ->
	    Prefix ++ Href ++ Args
    end.

get_git_branch() ->
    case os:cmd("git rev-parse --abbrev-ref HEAD") of
	"fatal:" ++ _ -> "master";  % sensible default
	Git ->
	    case string:tokens(Git, " \n") of
		[Branch]	-> Branch;
		Other		-> erlang:error({cannot_get_git_branch, Other})
	    end
    end.

%% Tried to display logo in a table on top of page, but not working.
%% Presumably, this hits some limitation of Markdown
%%
%% logo() ->
%%     {img, [{src, "erlang.png"},{alt,["Erlang logo"]}],[]}.

%% NEW-OPTIONS: title
%% DEFER-OPTIONS: run/2

title(App, Options) ->
    proplists:get_value(title, Options,
			if App == ?NO_APP ->
				"Overview";
			   true ->
				io_lib:fwrite("Application: ~s", [App])
			end).


%% Processing the individual source files.

%% NEW-OPTIONS: file_suffix, private, hidden
%% INHERIT-OPTIONS: edoc:layout/2
%% INHERIT-OPTIONS: edoc:get_doc/3
%% DEFER-OPTIONS: run/2

sources(Sources, Dir, Modules, Env, Options) ->
    Suffix = proplists:get_value(file_suffix, Options,
				 ?DEFAULT_FILE_SUFFIX),
    Private = proplists:get_bool(private, Options),
    Hidden = proplists:get_bool(hidden, Options),
    {Ms, E} = lists:foldl(fun (Src, {Set, Error}) ->
				  source(Src, Dir, Suffix, Env, Set,
					 Private, Hidden, Error, Options)
			  end,
			  {sets:new(), false}, Sources),
    {[M || M <- Modules, sets:is_element(M, Ms)], E}.


%% Generating documentation for a source file, adding its name to the
%% set if it was successful. Errors are just flagged at this stage,
%% allowing all source files to be processed even if some of them fail.

source({M, _, Name, Path}, Dir, Suffix, Env, Set, Private, Hidden,
       Error, Options) ->
    %% Old style, remove package reference
    source({M, Name, Path}, Dir, Suffix, Env, Set, Private, Hidden,
	   Error, Options);
source({M, Name, Path}, Dir, Suffix, Env, Set, Private, Hidden,
       Error, Options) ->
    File = filename:join(Path, Name),
    Enc = guess_encoding(File),
    case catch {ok, edoc:get_doc(File, Env, Options)} of
	{ok, {Module, Doc}} ->
	    check_name(Module, M, File),
	    case ((not is_private(Doc)) orelse Private)
		andalso ((not is_hidden(Doc)) orelse Hidden) of
		true ->
		    Text = edoc:layout(Doc, Options),
		    Name1 = atom_to_list(M) ++ Suffix,
		    write_file(Text, Dir, Name1, Name, Enc),
		    {sets:add_element(Module, Set), Error};
		false ->
		    {Set, Error}
	    end;
	R ->
	    report("skipping source file '~s': ~W.", [File, R, 15]),
	    {Set, true}
    end.

guess_encoding(File) ->
    try epp:read_encoding(File) of
        none -> epp:default_encoding();
        Enc  -> Enc
    catch
        _:_ ->
            epp:default_encoding()
    end.

write_file(Text, Dir, F) ->
    write_file(Text, Dir, F, F, auto).

write_file(Text, Dir, LastName, Name, Enc) ->
    %% edoc_lib:write_file/5 (with encoding support) was added in OTP R16B
    %% -- and removed in 18.0; we reuse the check to detect 18, since we don't
    %% -- care about pre-R16
    case lists:member({write_file,5}, edoc_lib:module_info(exports)) of
        true ->
            edoc_lib:write_file(Text, Dir, LastName, '',
                                [{encoding, encoding(Enc, Name)}]);
        false ->
            edoc_lib:write_file(Text, Dir, LastName,
				[{encoding, encoding(Enc, Name)}])
    end.

write_info_file(App, Modules, Dir) ->
    case erlang:function_exported(edoc_lib, write_info_file, 4) of
	true ->
	    edoc_lib:write_info_file(App, [], Modules, Dir);
	false ->
	    edoc_lib:write_info_file(App, Modules, Dir)
    end.

encoding(auto, Name) ->
    edoc_lib:read_encoding(Name, []);
encoding(Enc, _) ->
    Enc.


check_name(M, M0, File) ->
    N = M,
    N0 = M0,
    case N of
	[$? | _] ->
	    %% A module name of the form '?...' is assumed to be caused
	    %% by the epp_dodger parser when the module declaration has
	    %% the form '-module(?MACRO).'; skip the filename check.
	    ok;
	_ ->
	    if N =/= N0 ->
		    warning("file '~s' actually contains module '~s'.",
			    [File, M]);
	       true ->
		    ok
	    end
    end.

%% NEW-OPTIONS: overview
%% INHERIT-OPTIONS: read_file/4
%% INHERIT-OPTIONS: edoc_lib:run_layout/2
%% INHERIT-OPTIONS: edoc_extract:file/4
%% DEFER-OPTIONS: run/2

overview(Dir, Title, Env, Opts) ->
    File = proplists:get_value(overview, Opts,
			       filename:join(Dir, ?OVERVIEW_FILE)),
    Tags = read_file(File, overview, Env, Opts),
    Data = edoc_data:overview(Title, Tags, Env, Opts),
    F = fun (M) ->
		M:overview(Data, Opts)
	end,
    _Markdown = edoc_lib:run_layout(F, Opts).

copy_image(Dir, Options) ->
    case proplists:get_value(image, Options) of
	O when O==undefined; O==?IMAGE ->
	    case code:priv_dir(?EDOC_APP) of
		PrivDir when is_list(PrivDir) ->
		    From = filename:join(PrivDir, ?IMAGE),
		    edoc_lib:copy_file(From, filename:join(Dir, ?IMAGE));
		_ ->
		    report("cannot find default image file.", []),
		    exit(error)
	    end;
	"" ->
	    ok
    end.

%% NEW-OPTIONS: stylesheet_file
%% DEFER-OPTIONS: run/2

copy_stylesheet(Dir, Options) ->
    case proplists:get_value(stylesheet, Options) of
	O when O==undefined; O==?STYLESHEET ->
	    From = case proplists:get_value(stylesheet_file, Options) of
		       File when is_list(File) ->
			   File;
		       _ ->
			   case code:priv_dir(?EDOC_APP) of
			       PrivDir when is_list(PrivDir) ->
				   filename:join(PrivDir, ?STYLESHEET);
			       _ ->
				   report("cannot find default "
					  "stylesheet file.", []),
				   exit(error)
			   end
		   end,
	    edoc_lib:copy_file(From, filename:join(Dir, ?STYLESHEET));
	"" ->
	    ok
    end.

%% NEW-OPTIONS: stylesheet
%% DEFER-OPTIONS: run/2

%% stylesheet(Options) ->
%%     case proplists:get_value(stylesheet, Options) of
%% 	"" ->
%% 	    [];
%% 	S ->
%% 	    Ref = case S of
%% 		      undefined ->
%% 			  ?STYLESHEET;
%% 		      "" ->
%% 			  "";    % no stylesheet
%% 		      S when is_list(S) ->
%% 			  S;
%% 		      _ ->
%% 			  report("bad value for option 'stylesheet'.",
%% 				 []),
%% 			  exit(error)
%% 		  end,
%% 	    [{link, [{rel, "stylesheet"},
%% 		     {type, "text/css"},
%% 		     {href, Ref},
%% 		     {title, "EDoc"}], []},
%% 	     ?NL]
%%     end.

is_private(E) ->
    case get_attrval(private, E) of
 	"yes" -> true;
 	_ -> false
    end.

is_hidden(E) ->
    case get_attrval(hidden, E) of
 	"yes" -> true;
 	_ -> false
    end.

get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
	[#xmlAttribute{value = V}] ->
	    V;
	[] -> ""
    end.

get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].

%% Read external source file. Fails quietly, returning empty tag list.

%% INHERIT-OPTIONS: edoc_extract:file/4

read_file(File, Context, Env, Opts) ->
    case edoc_extract:file(File, Context, Env, Opts) of
	{ok, Tags} ->
	    Tags;
	{error, _} ->
	    []
    end.


%% TODO: FIXME: meta-level index generation

%% Creates a Table of Content from a list of Paths (ie paths to applications)
%% and an overview file.

-define(EDOC_DIR, "doc").
-define(INDEX_DIR, "doc/index").
-define(CURRENT_DIR, ".").

toc(_Paths, _Ctxt) ->
    erlang:error(nyi).
    %% Opts = Ctxt#context.opts,
    %% Dir = Ctxt#context.dir,
    %% Env = Ctxt#context.env,
    %% app_index_file(Paths, Dir, Env, Opts).

modules({doclet_gen,_,_,_,Ms,_}) ->  % pre-18
    Ms;
modules({doclet_gen,_,_,Ms}) ->  % since 18
    Ms.
