%%%-------------------------------------------------------------------
%%% File:      erlydtl_library.erl
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2014 Andreas Stenius
%%% @doc
%%% ErlyDTL library behaviour.
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2014 Andreas Stenius
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2014 by Andreas Stenius
%%%-------------------------------------------------------------------
-module(erlydtl_library).
-author('Andreas Stenius <kaos@astekk.se>').

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

-type exported_fun() :: Name::atom().
-type external_fun() :: {Module::atom(), exported_fun()}.
-type library_function() :: exported_fun() | external_fun().
-type inventory_item() :: exported_fun() | {Name::atom(), library_function()}.

-callback inventory(filters|tags) -> list(inventory_item()).
-callback version() -> 1.
