-include("exml.hrl").

-record(xmlstreamstart, {name :: binary(),
                         attrs = [] :: [exml:attr()]}).

-record(xmlstreamend, {name :: binary()}).
