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
%% -----------------------------------------------------------------------------

-ifdef(namespaced_types).
    -type hc_set()    :: sets:set().
    -type hc_gb_set() :: gb_sets:set().
-else.
    -type hc_set()    :: set().
    -type hc_gb_set() :: gb_set().
-endif.

-record('hamcrest.matchspec', {
    matcher     =   undefined  :: fun((term()) -> boolean()),
    expected    =   undefined  :: term(),
    desc        =   ""         :: term()
}).

-define(MATCHER(MatchFun, Expected, Desc),
        #'hamcrest.matchspec'{
          matcher=MatchFun,
          expected=Expected,
          desc=Desc
        }).

-define(HECKLE(M,F,A),
  application:set_env(hamcrest, heckle, [M, F, A])).

-define(NOHECKLE, application:unset_env(hamcrest, heckle)).
