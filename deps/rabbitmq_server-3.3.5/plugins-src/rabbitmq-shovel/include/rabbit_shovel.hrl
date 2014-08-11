%%  The contents of this file are subject to the Mozilla Public License
%%  Version 1.1 (the "License"); you may not use this file except in
%%  compliance with the License. You may obtain a copy of the License
%%  at http://www.mozilla.org/MPL/
%%
%%  Software distributed under the License is distributed on an "AS IS"
%%  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%  the License for the specific language governing rights and
%%  limitations under the License.
%%
%%  The Original Code is RabbitMQ.
%%
%%  The Initial Developer of the Original Code is GoPivotal, Inc.
%%  Copyright (c) 2007-2014 GoPivotal, Inc.  All rights reserved.
%%

-record(endpoint,
        {uris,
         resource_declaration
        }).

-record(shovel,
        {sources,
         destinations,
         prefetch_count,
         ack_mode,
         publish_fields,
         publish_properties,
         queue,
         reconnect_delay,
         delete_after = never
        }).
