-ifndef(WH_API_HEADERS).

-ifndef(WHISTLE_TYPES_INCLUDED).
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-endif.

%% For dialplan messages, what does the Invite-Format param accept as values?
-define(INVITE_FORMAT_TUPLE, {<<"Invite-Format">>, [<<"username">>, <<"e164">>, <<"npan">>, <<"1npan">>, <<"route">>]}).

%%% *_HEADERS defines a list of Keys that must exist in every message of type *
%%% (substitute AUTHN_REQ, AUTHN_RESP, etc, for *) to be considered valid.
%%%
%%% OPTIONAL_*_HEADERS defines a list of Keys that will be included in the final
%%% message if included in the passed in Proplist.
%%%
%%% *_VALUES defines a proplist of {Key, Value} pairs where Key is either in
%%% *_HEADERS or OPTIONAL_*_HEADERS, and Value is either a singular value or a list
%%% of values that the resulting message can have, given Key.
%%% If Value is not a list, a direct match is required to validate;
%%% if Value is a list of singular values, the set value must be a member of the Value list
%%% eg: -define(FOO_HEADERS, [<<"bar">>]).
%%%     -define(OPTIONAL_FOO_HEADERS, [<<"baz">>]).
%%%     -define(FOO_VALUES, [{<<"bar">>, <<"yes">>}, {<<"baz">>, [<<"abe">>, <<"bea">>, <<"eab">>]}]).
%%%     when foo_v(Prop) is called, Prop MUST contain key <<"bar">> with value <<"yes">>, and MAY
%%%     contain key <<"baz">>; if <<"baz">> exists, it can only have values <<"abe">>, <<"bea">>, or <<"eab">>.
%%%     foo_v([]) -> fails because key <<"bar">> is missing
%%%     foo_v([{<<"bar">>, <<"no">>}]) -> fails because <<"bar">> can only have value <<"yes">>
%%%     foo_v([{<<"bar">>, <<"yes">>}]) -> passes!
%%%     foo_v([{<<"baz">>, <<"abe">>}]) -> fails, no key <<"bar">>
%%%     foo_v([{<<"bar">>, <<"no">>, }, {<<"baz">>, <<"abe">>}]) -> fails, <<"bar">> can only be <<"yes">>
%%%     foo_v([{<<"bar">>, <<"yes">>, }, {<<"baz">>, <<"zzz">>}]) -> fails, <<"zzz">> is not in ?FOO_VALUES
%%%     foo_v([{<<"bar">>, <<"yes">>, }, {<<"baz">>, <<"eab">>}]) -> passes!
%%%
%%% *_TYPES defines a proplist of {Key, Type} pairs where Key is either in
%%% *_HEADERS or OPTIONAL_*_HEADERS, and Type defines a function that validates a passed in value
%%% is an appropriate type for the given Key, returning a boolean. If Key is not in the passed-in
%%% message, true is returned without running the Type fun.
%%% @spec Type :: function(Value :: any()) -> boolean()
%%%
%%% eg: -define(FOO_TYPES, [{<<"baz">>, fun(V) -> lists:member(V, proplists:get_value(<<"baz">>, ?FOO_VALUES)) end}]).
%%%   would define a function to validate the value of key <<"baz">> in the same way ?FOO_VALUES does.
%%%
%%% All four macros must be defined; OPTIONAL, VALUES, and TYPES can be empty lists.

%% Default Headers
%% All messages MUST include the DEFAULT_HEADERS list.
-define(DEFAULT_HEADERS, [<<"Server-ID">>, <<"Event-Category">>, <<"Event-Name">>
                              , <<"App-Name">>, <<"App-Version">>]).
-define(OPTIONAL_DEFAULT_HEADERS, [<<"Raw-Headers">>, <<"Destination-Server">>
                                  , <<"Geo-Location">>, <<"Access-Group">>
                                  , <<"Tenant-ID">>]).
-define(DEFAULT_VALUES, []).
-define(DEFAULT_TYPES, [{<<"Server-ID">>, fun is_binary/1}
                        ,{<<"Event-Category">>, fun is_binary/1}
                        ,{<<"Event-Name">>, fun is_binary/1}
                        ,{<<"App-Name">>, fun is_binary/1}
                        ,{<<"App-Version">>, fun is_binary/1}
                        ,{<<"Raw-Headers">>, fun is_binary/1}
                        ,{<<"Destination-Server">>, fun is_binary/1}
                        ,{<<"Geo-Location">>, fun is_binary/1}
                        ,{<<"Access-Group">>, fun is_binary/1}
                        ,{<<"Tenant-ID">>, fun is_binary/1}
                        ]).

%% Error Responses
-define(ERROR_RESP_HEADERS, [<<"Msg-ID">>, <<"Error-Message">>]).
-define(OPTIONAL_ERROR_RESP_HEADERS, []).
-define(ERROR_RESP_VALUES, [{<<"Event-Category">>, <<"error">>}]).
-define(ERROR_RESP_TYPES, []).

%%% Dialplan Commands and related definitions

%% Conference Discovery
-define(CONF_DISCOVERY_REQ_HEADERS, [<<"Account-ID">>, <<"Call-ID">>, <<"Control-Queue">>]).
-define(OPTIONAL_CONF_DISCOVERY_REQ_HEADERS, [<<"Conference-ID">>, <<"Moderator">>]).
-define(CONF_DISCOVERY_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                    ,{<<"Event-Name">>, <<"discovery">>}
                                    ,{<<"Moderator">>, [<<"true">>, <<"false">>]}
                                   ]).
-define(CONF_DISCOVERY_REQ_TYPES, [{<<"Call-ID">>, fun is_binary/1}
                                   ,{<<"Control-Queue">>, fun is_binary/1}
                                   ,{<<"Conference-ID">>, fun is_binary/1}
                                  ]).

%% Conference Participants
-define(CONF_PARTICIPANTS_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>]).
-define(OPTIONAL_CONF_PARTICIPANTS_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_PARTICIPANTS_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                       ,{<<"Event-Name">>, <<"command">>}
                                       ,{<<"Application-Name">>, <<"participants">>}
                                      ]).
-define(CONF_PARTICIPANTS_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

-define(CONF_PARTICIPANTS_RESP_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>]).
-define(OPTIONAL_CONF_PARTICIPANTS_RESP_HEADERS, [<<"Insert-At">>, <<"Participants">>, <<"Error">>]).
-define(CONF_PARTICIPANTS_RESP_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"participants">>}
                               ,{<<"Application-Name">>, <<"participants">>}
                              ]).
-define(CONF_PARTICIPANTS_RESP_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

%% Conference Play
-define(CONF_PLAY_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Media-Name">>]).
-define(OPTIONAL_CONF_PLAY_REQ_HEADERS, [<<"Insert-At">>, <<"Participant-ID">>]).
-define(CONF_PLAY_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"play">>}
                              ]).
-define(CONF_PLAY_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                              ,{<<"Media-Name">>, fun is_binary/1}
                              ,{<<"Participant-ID">>, fun is_binary/1}
                             ]).

%% Conference Deaf Participant
-define(CONF_DEAF_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_CONF_DEAF_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_DEAF_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"deaf">>}
                              ]).
-define(CONF_DEAF_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                              ,{<<"Participant-ID">>, fun is_binary/1}
                             ]).

%% Conference Undeaf Participant
-define(CONF_UNDEAF_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_CONF_UNDEAF_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_UNDEAF_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"undeaf">>}
                                ]).
-define(CONF_UNDEAF_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                ,{<<"Participant-ID">>, fun is_binary/1}
                               ]).

%% Conference Mute Participant
-define(CONF_MUTE_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_CONF_MUTE_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_MUTE_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"mute">>}
                              ]).
-define(CONF_MUTE_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                              ,{<<"Participant-ID">>, fun is_binary/1}
                             ]).

%% Conference Unmute Participant
-define(CONF_UNMUTE_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_CONF_UNMUTE_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_UNMUTE_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"unmute">>}
                                ]).
-define(CONF_UNMUTE_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                ,{<<"Participant-ID">>, fun is_binary/1}
                               ]).

%% Conference Kick Participant
-define(CONF_KICK_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_CONF_KICK_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_KICK_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"kick">>}
                              ]).
-define(CONF_KICK_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                              ,{<<"Participant-ID">>, fun is_binary/1}
                             ]).

%% Conference Move Participant
-define(CONF_MOVE_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-From">>, <<"Conference-To">>, <<"Participant-ID">>]).
-define(OPTIONAL_CONF_MOVE_REQ_HEADERS, [<<"Insert-At">>]).
-define(CONF_MOVE_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"command">>}
                               ,{<<"Application-Name">>, <<"move">>}
                              ]).
-define(CONF_MOVE_REQ_TYPES, [{<<"Conference-From">>, fun is_binary/1}
                               ,{<<"Conference-To">>, fun is_binary/1}
                               ,{<<"Participant-ID">>, fun is_binary/1}
                              ]).

%% Conference Relate Participant
-define(CONF_RELATE_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>, <<"Correlate-ID">>]).
-define(OPTIONAL_CONF_RELATE_REQ_HEADERS, [<<"Insert-At">>, <<"Relationship">>]).
-define(CONF_RELATE_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"kick">>}
                                 ,{<<"Relationship">>, [<<"deaf">>, <<"mute">>, <<"reset">>]}
                                ]).
-define(CONF_RELATE_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                ,{<<"Participant-ID">>, fun is_binary/1}
                                ,{<<"Correlate-ID">>, fun is_binary/1}
                               ]).

%% [{FreeSWITCH-Flage-Name, Whistle-Flag-Name}]
%% Conference-related entry flags
%% convert from FS conference flags to Whistle conference flags
-define(CONFERENCE_FLAGS, [{<<"mute">>, <<"Mute">>}
                           ,{<<"deaf">>, <<"Deaf">>}
                           ,{<<"moderator">>, <<"Moderator">>}
                           ]).
-define(WH_API_HEADERS, true).
-endif.
