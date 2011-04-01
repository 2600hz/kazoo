%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%% Handles dialplan actions
%%%
%%% @end
%%% Created:       21 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%% Last Modified: 23 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%%============================================================================
-module(cf_dialplan).

%% API
-export([handle/2]).

-import(logger, [format_log/3]).

-include("../callflow.hrl").

-define(APP_NAME, <<"cf_dialplan">>).
-define(APP_VERSION, <<"0.1">>).

handle (_Data, _Call) ->
    {continue}.

%%%
%%%============================================================================
%%%== END =====
%%%============================================================================
