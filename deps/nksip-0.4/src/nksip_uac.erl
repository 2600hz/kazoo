%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Request sending functions as UAC.
%%
%% In case of using a SIP URI as destination, is is possible to include
%% custom headers: "<sip:host;method=REGISTER?contact=*&expires=10>"
%% 
-module(nksip_uac).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").

-export([options/3, options/2, register/3, invite/3, invite/2, ack/2, bye/2, cancel/1]).
-export([info/2, update/2, subscribe/2, subscribe/3, notify/2]).
-export([message/3, message/2, refer/3, refer/2, publish/3, publish/2]).
-export([request/3, request/2, refresh/2, stun/3]).
-export_type([result/0, ack_result/0, error/0, cancel_error/0]).

-import(nksip_uac_lib, [send/4, send_dialog/3]).


%% ===================================================================
%% Types
%% ===================================================================

% -type opt() ::  
%     dialog_opt() |
%     {from, nksip:user_uri()} | {to, nksip:user_uri()} | {user_agent, binary()} |
%     {call_id, binary()} | {cseq, nksip:cseq()} | {route, nksip:user_uri()}.

% -type dialog_opt() ::  
%     {meta, [nksip_response:field()]} | async | {callback, function()} | 
%     get_response | get_request | 
%     {contact, nksip:user_uri()} | contact | {content_type, binary()} | 
%     {headers, [nksip:header()]} | {body, nksip:body()} | {local_host, auto|binary()}.

% -type register_opt() ::
%     {expires, non_neg_integer()} | unregister | unregister_all.

% -type invite_opt() ::
%     {expires, pos_integer()} |
%     {prack, function()}.

% -type subscribe_opt() ::
%     {event, binary()} |
%     {expires, non_neg_integer()}.

% -type notify_reason() ::
%     deactivated | probation | rejected | timeout | giveup | noresource | invariant.

% -type notify_opt() ::
%     {event, binary()} |
%     {state, active | pending | {terminated, notify_reason()} | 
%      {terminated, notify_reason(), pos_integer()}}.

% -type message_opt() ::
%     {expires, non_neg_integer()}.

% -type refer_opt() ::
%     {refer_to, string()|binary()}.

% -type publish_opt() ::
%     {event, binary()} |
%     {expires, non_neg_integer()} |
%     {sip_etag, binary()}.

-type result() ::  
    {async, nksip:id()} | {ok, nksip:response_code(), nksip_lib:optslist()} | 
    {resp, nksip:response()}.
    
-type ack_result() ::
    ok | async.

-type error() :: 
    invalid_uri | invalid_from | invalid_to | invalid_route |
    invalid_contact | invalid_cseq | invalid_content_type | invalid_require |
    invalid_accept | invalid_event |
    unknown_dialog | bad_event | request_pending | service_unavailable | 
    nksip_call_router:sync_error().

-type cancel_error() :: 
    unknown_request | invalid_request | nksip_call_router:sync_error().



%% ===================================================================
%% Public
%% ===================================================================

%% @doc Sends an OPTIONS request.
%%
%% OPTIONS requests are usually sent to get the current set of SIP features 
%% and codecs the remote party supports, and to detect if it is <i>up</i>, 
%% it has failed or it is not responding requests for any reason. 
%% It can also be used to measure the remote party response time. 
%%
%% When `Dest' is a <i>SIP Uri</i> the request will be sent outside any dialog.
%% If it is a dialog specification, it will be sent inside that dialog.
%% Recognized options are described in {@link opt()} when sent outside any dialog,
%% and {@link dialog_opt()} when sent inside a dialog.
%%
%% NkSIP has an automatic remote <i>pinging</i> feature that can be activated 
%% on any SipApp (see {@link nksip_sipapp_auto:start_ping/5}).
%%
-spec options(term()|nksip:app_id(), nksip:user_uri(), nksip_lib:optslist()) ->
    result() | {error, error()}.

options(App, Uri, Opts) ->
    Opts1 = [supported, allow, allow_event | Opts],
    send(App, 'OPTIONS', Uri, Opts1).


-spec options(nksip:id(), nksip_lib:optslist()) ->
    result() | {error, error()}.

options(Id, Opts) ->
    Opts1 = [supported, allow, allow_event | Opts],
    send_dialog('OPTIONS', Id, Opts1).


%% @doc Sends a REGISTER request.
%%
%% This function is used to send a new REGISTER request to any registrar server,
%% to register a new `Contact', delete a current registration or get the list of 
%% current registered contacts from the registrar.
%%
%% When `Dest' is a <i>SIP Uri</i> the request will be sent outside any dialog.
%% If it is a dialog specification, it will be sent inside that dialog.
%% Recognized options are described in {@link opt()} when sent outside any dialog,
%% and {@link dialog_opt()} when sent inside a dialog.
%%
%% Additional recognized options are defined in {@link register_opt()}:
%%  
%% <table border="1">
%%      <tr><th>Key</th><th>Type</th><th>Default</th><th>Description</th></tr>
%%      <tr>
%%          <td>`expires'</td>
%%          <td>`integer()'</td>
%%          <td></td>
%%          <td>If defined it will generate a <i>Expires</i> header</td>
%%      </tr>
%%      <tr>
%%          <td>`unregister'</td>
%%          <td></td>
%%          <td></td>
%%          <td>If present, unregisters the contact (sets <i>Expires</i> to 0)</td>
%%      </tr>
%%      <tr>
%%          <td>`unregister_all'</td>
%%          <td></td>
%%          <td></td>
%%          <td>If present, unregisters all registered contacts for this user (sets
%%              <i>Contact</i> to <i>*</i> and <i>Expires</i> to 0)</td>
%%      </tr>
%%      <tr>
%%          <td>`reg_id</td>
%%          <td></td>
%%          <td></td>
%%          <td>If present, </td>
%%      </tr>
%% </table>
%% 
%% You will usually want to include a `contact' option to generate a valid
%% <i>Contact</i> header.
%%
%% Keep in mind that, once you send a REGISTER requests, following refreshers
%% should have the same `Call-ID' and an incremented `CSeq' headers. 
%% The default value for `contact' parameter would be `auto' in this case.
%%
%% NkSIP offers also an automatic SipApp registration facility 
%% (see {@link nksip:start/4}).
-spec register(term()|nksip:app_id(), nksip:user_uri(), nksip_lib:optslist()) ->
    result() | {error, error()}.

register(App, Uri, Opts) ->
    Opts1 = case lists:member(unregister_all, Opts) of
        true ->
            [{contact, <<"*">>}, {expires, 0}| Opts -- [unregister_all]];
        false ->
            case lists:member(unregister, Opts) of
                true -> [contact, {expires, 0}| Opts -- [unregister]];
                false -> Opts
            end
    end,
    Opts2 = [to_as_from, supported, allow, allow_event | Opts1],
    send(App, 'REGISTER', Uri, Opts2).


%% @doc Sends an INVITE request.
%%
%% This functions sends a new session invitation to another endpoint or proxy. 
%% When the first provisional response from the remote party is received
%% (as 180 <i>Ringing</i>) a new dialog will be started, and the corresponding callback
%% {@link nksip_sipapp:dialog_update/3} in the callback module will be called. 
%% If this response has also a valid SDP body, a new session will be associated 
%% with the dialog and the corresponding callback {@link nksip_sipapp:session_update/3}
%% will also be called.
%%
%% When the first 2xx response is received, the dialog is confirmed. 
%% <b>You must then call {@link ack/3} immediately</b>, offering an 
%% SDP body if you haven't done it in the INVITE request.
%%
%% The dialog is destroyed when a BYE is sent or received, or a 408 <i>Timeout</i> 
%% or 481 <i>Call Does Not Exist</i> response is received. 
%% If a secondary 2xx response is received (usually because a proxy server 
%% has forked the request) NkSIP will automatically acknowledge it and send BYE. 
%% If a 3xx-6xx response is received instead of a 2xx response, the <i>early dialog</i> 
%% is destroyed. You should not call {@link ack/2} in this case, 
%% as NkSIP will do it for you automatically.
%%
%% After a dialog has being established, you can send new INVITE requests
%% (called <i>reINVITEs</i>) <i>inside</i> this dialog.
%%
%% When `Dest' is a <i>SIP Uri</i> the request will be sent outside any dialog.
%% If it is a dialog specification, it will be sent inside that dialog.
%% Recognized options are described in {@link opt()} when sent outside any dialog,
%% and {@link dialog_opt()} when sent inside a dialog.
%%
%% Additional recognized options are defined in {@link invite_opt()}:
%%
%% <table border="1">
%%      <tr><th>Key</th><th>Type</th><th>Default</th><th>Description</th></tr>
%%      <tr>
%%          <td>`expires'</td>
%%          <td>`integer()'</td>
%%          <td></td>
%%          <td>If included it will generate a `Expires' header.</td>
%%      </tr>
%%      <tr>
%%          <td>`prack'</td>
%%          <td><code>`fun/2'</code></td>
%%          <td></td>
%%          <td>If included, this function will be called when 
%%          a reliable provisional response has been received, and before 
%%          sending the corresponding PRACK.
%%          It will be called as `{RemoteSDP, Response}' where 
%%          <code>RemoteSDP :: `<<>>' | {@link nksip_sdp:sdp()} and Response :: {@link nksip:response()}</code>.
%%          If RemoteSDP is a SDP, it is an offer and you must supply an answer as 
%%          function return. If it is `<<>>', you can return `<<>>' or send a new offer.
%%          If this option is not included, PRACKs will be sent with no body.</td>
%%      </tr>
%%      <tr>
%%          <td>`session_expires'</td>
%%          <td>`integer()'</td>
%%          <td><code>SipApp's config</code></td>
%%          <td>Diable session timers (using 0 value) or modify current value for
%%          session timer (global config parameter `session_timer').</td>
%%      </tr>
%% </table>
%%
%% A `contact' option will be automatically added if no contact is defined.
%%
%% If `Expires' header is used, NkSIP will CANCEL the request if no final response 
%% has been received in this period in seconds. The default value for `contact' parameter 
%% would be `auto' in this case.
%%
%% If you want to be able to <i>CANCEL</i> the request, you should use the `async'
%% option.
%%
%% NkSIP will automatically start a session timer (according to RFC4028). Use
%% option `session_expires' to 0 to disable. If the session timer is active, and
%% a 422 (Session Interval Too Small) is received, NkSIP will automatically resend
%% the request updating Session-Expires header.
%%
%% If a 491 response is received, it usually means that the remote party is 
%% starting another reINVITE transaction right now. You should call 
%% {@link nksip_response:wait_491()} and try again.
%%
%% The first returned value is allways {dialog_id, DialogId}, even if the
%% `fields' option is not used.

-spec invite(term()|nksip:app_id(), nksip:user_uri(), nksip_lib:optslist()) ->
    result() | {error, error()}.

invite(App, Uri, Opts) ->
    Opts1 = [supported, allow, allow_event | Opts],
    send(App, 'INVITE', Uri, Opts1).


-spec invite(nksip:id(), nksip_lib:optslist()) ->
    result() | {error, error()}.

invite(Id, Opts) ->
    Opts1 = [supported, allow, allow_event | Opts],
    send_dialog('INVITE', Id, Opts1).



%% @doc Sends an <i>ACK</i> after a successful <i>INVITE</i> response.
%%
%% After sending an INVITE and receiving a successfully (2xx) response, 
%% you must call this function immediately to send the mandatory ACK request. 
%% NkSIP won't send it for you automatically in case of a successful response, 
%% because you may want to include a SDP body if you didn't do it in the INVITE request.
%%
%% To specify the dialog you should use the dialog's id from 
%% the return of the {@link invite/3} call or using
%% {@link nksip_sipapp:dialog_update/3} callback function. 
%% Valid options are `fields', `callback', `async', `content_type', `headers' and 
%% `body'.
%%
%% For sync requests, it will return `ok' if the request could be sent or
%% `{error, Error}' if an error is detected. For async requests, it will return 
%% `async'. If a callback is defined, it will be called as `ok' or `{error, Error}'.    
%%
-spec ack(nksip:id(), nksip_lib:optslist()) ->
    ack_result() | {error, error()}.

ack(Id, Opts) ->
    send_dialog('ACK', Id, Opts).


%% @doc Sends an <i>BYE</i> for a current dialog, terminating the session.
%%
%% You need to know the dialog's id of the dialog you want to hang up.
%% You can get it from the return of the initial {@link invite/3}, or using 
%% {@link nksip_sipapp:dialog_update/3} callback function.
%%
%% Valid options are defined in {@link dialog_opt()}.
%%
-spec bye(nksip:id(), nksip_lib:optslist()) -> 
    result() | {error, error()}.

bye(Id, Opts) ->
    send_dialog('BYE', Id, Opts).


%% @doc Sends an <i>INFO</i> for a current dialog.
%%
%% Sends an INFO request. Doesn't change the state of the current session.
%% You need to know the dialog's id. You can get it from the return of the initial 
%% {@link invite/3}, or using {@link nksip_sipapp:dialog_update/3} callback function.
%%
%% Valid options are defined in {@link dialog_opt()}.
%%
-spec info(nksip:id(), nksip_lib:optslist()) -> 
    result() | {error, error()}.

info(Id, Opts) ->
    send_dialog('INFO', Id, Opts).


%% @doc Sends an <i>CANCEL</i> for a currently ongoing <i>INVITE</i> request.
%%
%% You can use this function to send a CANCEL requests to abort a currently 
%% <i>calling</i> INVITE request, using the `ReqId' obtained when calling 
%% {@link invite/3} <i>asynchronously</i>. 
%% The CANCEL request will eventually be received at the remote end, and, 
%% if it hasn't yet answered the matching INVITE request, 
%% it will finish it with a 487 code. 
%%
%% This call is always asychronous. It returns a soon as the request is
%% received and the cancelling INVITE is found.
%%
-spec cancel(nksip:id()) ->
    ok | {error, cancel_error()}.

cancel(ReqId) ->
    nksip_call:cancel(ReqId).


%% @doc Sends a UPDATE on a currently ongoing dialog.
%%
%% This function sends a in-dialog UPDATE, allowing to change the media
%% session before the dialog has been confirmed.
%%
%% A session timer will be started (see {@link invite/3}).
%%
%% Valid options are defined in {@link dialog_opt()}.
%%
-spec update(nksip:id(), nksip_lib:optslist()) ->
    result() | {error, error()}.

update(Id, Opts) ->
    Opts1 = [supported, accept, allow | Opts],
    send_dialog('UPDATE', Id, Opts1).


%% @doc Sends a update on a currently ongoing dialog using INVITE.
%%
%% This function sends a in-dialog INVITE, using the same current
%% parameters of the dialog, only to refresh it. The current local SDP version
%% will be incremented before sending it.
%%
%% Available options are the same as {@link reinvite/2} and also:
%% <ul>
%%  <li>`active': activate the medias on SDP (sending `a=sendrecv')</li>
%%  <li>`inactive': deactivate the medias on SDP (sending `a=inactive')</li>
%%  <li>`hold': activate the medias on SDP (sending `a=sendonly')</li>
%% </ul>
%%
-spec refresh(nksip:id(), nksip_lib:optslist()) ->
    result() | {error, error()}.

refresh(Id, Opts) ->
    Body1 = case nksip_lib:get_value(body, Opts) of
        undefined ->
            case nksip_dialog:field(Id, invite_local_sdp) of
                #sdp{} = SDP -> SDP;
                _ -> <<>>
            end;
        Body ->
            Body
    end,
    Op = case lists:member(active, Opts) of
        true -> 
            sendrecv;
        false ->
            case lists:member(inactive, Opts) of
                true -> 
                    inactive;
                false ->
                    case lists:member(hold, Opts) of
                        true -> sendonly;
                        false -> none
                    end
            end
    end,
    Body2 = case Body1 of
        #sdp{} when Op /= none -> nksip_sdp:update(Body1, Op);
        #sdp{} -> nksip_sdp:increment(Body1);
        _ -> Body1
    end,
    Opts2 = nksip_lib:delete(Opts, [body, active, inactive, hold]),
    invite(Id, [{body, Body2}|Opts2]).


%% @doc Sends an SUBSCRIBE request.
%%
%% This functions sends a new subscription request to the other party.
%% If the remote party returns a 2xx response, it means that the subscription
%% has been accepted, and a NOTIFY request should arrive inmediatly. 
%% After the reception of the NOTIFY, the subscription state will change and 
%% NkSIP will call {@link nksip_sipapp:dialog_update/3}.
%%
%% In case of 2xx response, the first returned value is allways 
%% `{subscription_id, SubscriptionId}', even if the `fields' option is not used.
%%
%% When `Dest' is a <i>SIP Uri</i> the request will be sent outside any dialog,
%% creating a new dialog and a new subscription.
%% If it is a <i>dialog specification</i>, it will be sent inside that dialog, creating a
%% new 'subscription usage'.
%% If it is a <i>subscription specification</i>, it will send as a re-SUBSCRIBE, using
%% the same <i>Event</i> and <i>Expires</i> as the last <i>SUBSCRIBE</i> and
%% refreshing the subscription in order to avoid its expiration.
%%
%% Recognized options are described in {@link opt()} 
%% when sent outside any dialog, and {@link dialog_opt()} when sent inside a dialog.
%% Additional recognized options are defined in {@link subscribe_opt()}:
%%
%% <table border="1">
%%      <tr><th>Key</th><th>Type</th><th>Default</th><th>Description</th></tr>
%%      <tr>
%%          <td>`event'</td>
%%          <td>`binary()'</td>
%%          <td></td>
%%          <td>Generates the mandatory <i>Event</i> header for the event package
%%          we want to use (like `{event "MyEvent}' or `{event, "MyEvent;id=first"}'.
%%          Don't use it in case of re-subscriptions.</td>
%%      </tr>
%%      <tr>
%%          <td>`expires'</td>
%%          <td>`integer()'</td>
%%          <td></td>
%%          <td>If included, it will generate a <i>Expires</i> proposing a 
%%          expiration time to the server. Don't use in re-subscriptions 
%%          to use the same expire as last SUBSCRIBE.</td>
%%      </tr>
%% </table>
%%
%% After a 2xx response, you should send a new re-SUBSCRIBE request to
%% refresh the subscription before the indicated Expires, 
%% calling this function again but using the subscription specification.
%%
%% When half the time before expire has been completed, NkSIP will call callback
%% {@link nksip_sipapp:dialog_update/3} as 
%% `{subscription_state, SubscriptionId, middle_timer}'.
-spec subscribe(term()|nksip:app_id(), nksip:user_uri(), nksip_lib:optslist()) ->
    result() | {error, error()}.

subscribe(App, Uri, Opts) ->
    % event and expires options are detected later
    Opts1 = [supported, allow, allow_event | Opts],
    send(App, 'SUBSCRIBE', Uri, Opts1).


-spec subscribe(nksip:id(), nksip_lib:optslist()) ->
    result() | {error, error()}.

subscribe(Id, Opts) ->
    % event and expires options are detected later
    Opts1 = [supported, allow, allow_event | Opts],
    send_dialog('SUBSCRIBE', Id, Opts1).




%% @doc Sends an <i>NOTIFY</i> for a current server subscription.
%%
%% When your SipApp accepts a incoming SUBSCRIBE request, replying a 2xx response,
%% you should send a NOTIFY inmediatly. You have to use the subscription's id
%% from the call to callback `subscribe/3'.
%%
%% Valid options are defined in {@link dialog_opt()} and {@link notify_opt()}.
%% NkSIP will include the mandatory <i>Event</i> and 
%% <i>Subscription-State</i> headers for you, 
%% depending on the following parameters:
%%
%% <table border="1">
%%      <tr><th>Key</th><th>Type</th><th>Default</th><th>Description</th></tr>
%%      <tr>
%%          <td>`state'</td>
%%          <td>`active|pending|{terminated,Reason}|{terminated,Reason,Retry} 
%%               (see bellow)'</td>
%%          <td>`active'</td>
%%          <td>Generates the mandatory <i>Subscription-State</i> header</td>
%%      </tr>
%% </table>
%%
%% Valid states are the following:
%% <ul>
%%   <li>`active': the subscription is active. NkSIP will add a `expires' parameter
%%       indicating the remaining time.</li>
%%   <li>`pending': the subscription has not yet been authorized. A `expires' parameter
%%       will be added.</li>
%%   <li>`terminated': the subscription has been terminated. You must use a reason:
%%       <ul>
%%          <li>`deactivated': the remote party should retry again inmediatly.</li>
%%          <li>`probation': the remote party should retry again. You can use
%%              `Retry' to inform of the minimum time for a new try.</li>
%%          <li>`rejected': the remote party should no retry again.</li>
%%          <li>`timeout': the subscription has timed out, the remote party can 
%%              send a new one inmediatly.</li>
%%          <li>`giveup': we have not been able to authorize the request. The remote
%%              party can try again. You can use `Retry'.</li>
%%          <li>`noresource': the subscription has ended because of the resource 
%%              does not exists any more. Do not retry.</li>
%%          <li>`invariant': the subscription has ended because of the resource 
%%              is not going to change soon. Do not retry.</li>
%%       </ul></li>
%% </ul> 
%%

-spec notify(nksip:id(), nksip_lib:optslist()) -> 
    result() | {error, error()} |  {error, invalid_state}.

notify(Id, Opts) ->
    SS = case nksip_lib:get_value(state, Opts, active) of
        active ->
            {active, undefined};
        pending -> 
            {pending, undefined};
        {terminated, Reason} 
            when Reason==deactivated; Reason==probation; Reason==rejected; 
                 Reason==timeout; Reason==giveup; Reason==noresource; 
                 Reason==invariant ->
            {terminated, Reason, undefined};
        {terminated, probation, Retry} when is_integer(Retry), Retry>0 ->
            {terminated, probation, Retry};
        {terminated, giveup, Retry} when is_integer(Retry), Retry>0 ->
            {terminated, giveup, Retry};
        _ ->
            invalid
    end,
    case SS of
        invalid -> 
            {error, invalid_state};
        _ -> 
            Opts1 = nksip_lib:delete(Opts, state),
            send_dialog('NOTIFY', Id, [{subscription_state, SS}|Opts1])
    end.


%% @doc Sends an MESSAGE request.
%%
%% When `Dest' is a <i>SIP Uri</i> the request will be sent outside any dialog.
%% If it is a dialog specification, it will be sent inside that dialog.
%% Recognized options are described in {@link opt()} when sent outside any dialog,
%% and {@link dialog_opt()} when sent inside a dialog.
%%
%% Additional recognized options are defined in {@link message_opt()}:
%%
%% <table border="1">
%%      <tr><th>Key</th><th>Type</th><th>Default</th><th>Description</th></tr>
%%      <tr>
%%          <td>`expires'</td>
%%          <td>`integer()'</td>
%%          <td></td>
%%          <td>If included it will generate a <i>Expires</i> header. NkSIP will 
%%              also add a <i>Date</i> header.</td>
%%      </tr>
%% </table>
%%

-spec message(term()|nksip:app_id(), nksip:user_uri(), nksip_lib:optslist()) ->
    result() | {error, error()}.

message(App, Uri, Opts) ->
    Opts1 = case lists:keymember(expires, 1, Opts) of
        true -> [date|Opts];
        _ -> Opts
    end,
    send(App, 'MESSAGE', Uri, Opts1).


-spec message(nksip:id(), nksip_lib:optslist()) ->
    result() | {error, error()}.

message(Id, Opts) ->
    Opts1 = case lists:keymember(expires, 1, Opts) of
        true -> [date|Opts];
        _ -> Opts
    end,
    send_dialog('MESSAGE', Id, Opts1).



%% @doc Sends an <i>REFER</i> for a remote party
%%
%% Asks the remote party to start a new connection to the indicated uri in
%% `refer_to' parameter. If a 2xx response is received, the remote
%% party has agreed and will start a new connection. A new subscription will
%% be stablished, and you will start to receive NOTIFYs.
%%
%% Implement the callback function {@link nksip_sipapp:notify/4} to receive
%% them, filtering using the indicated `subscription_id'
%%
%% In case of 2xx response, the first returned value is allways 
%% `{subscription_id, SubscriptionId}', even if the `fields' option is not used.
%%
%% When `Dest' is a <i>SIP Uri</i> the request will be sent outside any dialog,
%% creating a new dialog and a new subscription.
%% If it is a <i>dialog specification</i>, it will be sent inside that dialog, creating a
%% new 'subscription usage'.
%%
%% Recognized options are described in {@link opt()} 
%% when sent outside any dialog, and {@link dialog_opt()} when sent inside a dialog.
%% Additional recognized options are defined in {@link notify_opt()}:
%%
%% <table border="1">
%%      <tr><th>Key</th><th>Type</th><th>Default</th><th>Description</th></tr>
%%      <tr>
%%          <td>`refer_to'</td>
%%          <td>{@link nksip:user_uri()}</td>
%%          <td></td>
%%          <td>Generates the mandatory <i>Refer-To</i> header</td>
%%      </tr>
%% </table>
%%

-spec refer(term()|nksip:app_id(), nksip:user_uri(), nksip_lib:optslist()) -> 
    result() | {error, error()} |  {error, invalid_refer_to}.

refer(App, Uri, Opts) ->
    case nksip_lib:get_binary(refer_to, Opts) of
        <<>> ->
            {error, invalid_refer_to};
        ReferTo ->
            Opts1 = [{insert, "refer-to", ReferTo} | nksip_lib:delete(Opts, refer_to)],
            send(App, 'REFER', Uri, Opts1)
    end.


-spec refer(nksip:id(), nksip_lib:optslist()) -> 
    result() | {error, error()} |  {error, invalid_refer_to}.

refer(Id, Opts) ->
    case nksip_lib:get_binary(refer_to, Opts) of
        <<>> ->
            {error, invalid_refer_to};
        ReferTo ->
            Opts1 = [{insert, "refer-to", ReferTo} | nksip_lib:delete(Opts, refer_to)],
            send_dialog('REFER', Id, Opts1)
    end.


%% @doc Sends an PUBLISH request.
%%
%% This functions sends a new publishing to the other party, using a
%% remote supported event package and including a body.
%%
%% If the remote party returns a 2xx response, it means that the publishing
%% has been accepted, and the body has been stored. A SIP-ETag header will 
%% be returned (a `sip_etag' parameter will always be returned in `Meta'). 
%% You can use this parameter to update the stored information (sending a
%% new body), or deleting it (using `{expires, 0}')
%%
%% When `Dest' is a <i>SIP Uri</i> the request will be sent outside any dialog.
%% If it is a <i>dialog specification</i>, it will be sent inside that dialog.
%%
%% Recognized options are described in {@link opt()} 
%% when sent outside any dialog, and {@link dialog_opt()} when sent inside a dialog.
%% Additional recognized options are defined in {@link publish_opt()}:
%%
%% <table border="1">
%%      <tr><th>Key</th><th>Type</th><th>Default</th><th>Description</th></tr>
%%      <tr>
%%          <td>`event'</td>
%%          <td>`string()|binary()'</td>
%%          <td></td>
%%          <td>Generates the mandatory <i>Event</i> header for the event package
%%          we want to use (like `{event "MyEvent}' or `{event, "MyEvent;id=first"}'.
%%          Don't use it in case of re-subscriptions.</td>
%%      </tr>
%%      <tr>
%%          <td>`expires'</td>
%%          <td>`integer()'</td>
%%          <td></td>
%%          <td>If included, it will generate a <i>Expires</i> proposing a 
%%          expiration time to the server. Send a value of `0' to expire
%%          the published information.</td>
%%      </tr>
%%      <tr>
%%          <td>`sip_etag</td>
%%          <td>`string()|binary()'</td>
%%          <td></td>
%%          <td>If included, it will generate a <i>SIP-If-Math</i> header,
%%          to update a published information or expire it.</td>
%%      </tr>
%% </table>
%%
-spec publish(term()|nksip:app_id(), nksip:user_uri(), nksip_lib:optslist()) ->
    result() | {error, error()}.

publish(App, Uri, Opts) ->
    % event and expires options are detected later
    Opts1 = case nksip_lib:get_binary(sip_etag, Opts) of
        <<>> -> Opts;
        ETag -> [{insert, "sip-if-match", ETag}|Opts]
    end,
    Opts2 = [supported, allow, allow_event | Opts1],
    send(App, 'PUBLISH', Uri, Opts2).


-spec publish(nksip:id(), nksip_lib:optslist()) ->
    result() | {error, error()}.

publish(Id, Opts) ->
    % event and expires options are detected later
    Opts1 = case nksip_lib:get_binary(sip_etag, Opts) of
        <<>> -> Opts;
        ETag -> [{insert, "sip-if-match", ETag}|Opts]
    end,
    Opts2 = [supported, allow, allow_event | Opts1],
    send_dialog('PUBLISH', Id, Opts2).



%% @doc Sends a request constructed from a SIP-Uri
%%
%% This function constructs and send an out-of-dialog request from a SIP-Uri.
%% Common options in {@link opt()} are supported.
%%

-spec request(term()|nksip:app_id(), nksip:user_uri(), nksip_lib:optslist()) -> 
    result() | {error, error()}.

request(App, Dest, Opts) ->
    send(App, undefined, Dest, Opts).


-spec request(nksip:id(), nksip_lib:optslist()) -> 
    result() | {error, error()}.

request(Id, Opts) ->
    send_dialog(undefined, Id, Opts).





%% @doc Sends a <i>STUN</i> binding request.
%%
%% Use this function to send a STUN binding request to a remote STUN or 
%% STUN-enabled SIP server, in order to get our remote ip and port.
%% If the remote server is a standard STUN server, use port 3478 
%% (i.e. `sip:stunserver.org:3478'). If it is a STUN server embedded into a SIP UDP
%% server, use a standard SIP uri.
%%
-spec stun(term()|nksip:app_id(), nksip:user_uri(), nksip_lib:optslist()) ->
    {ok, {LocalIp, LocalPort}, {RemoteIp, RemotePort}} | {error, Error}
    when LocalIp :: inet:ip_address(), LocalPort :: inet:port_number(),
         RemoteIp :: inet:ip_address(), RemotePort :: inet:port_number(),
         Error :: unknown_core | invalid_uri | no_host | service_unavailable.

stun(App, UriSpec, _Opts) ->
    case nksip:find_app(App) of
        {ok, AppId} ->
            case nksip_transport:get_listening(AppId, udp, ipv4) of
                [] -> 
                    {error, no_udp_transport};
                [{#transport{listen_ip=LIp, listen_port=LPort}, Pid}|_] ->
                    case nksip_parse:uris(UriSpec) of
                        [Uri] ->
                            Transp = nksip_dns:resolve(Uri),
                            case nksip_lib:extract(Transp, udp) of
                                [{udp, Ip, Port, _}|_] -> 
                                    case nksip_transport_udp:send_stun_sync(Pid, Ip, Port) of
                                        {ok, SIp, SPort} ->
                                            {ok, {LIp, LPort}, {SIp, SPort}};
                                        error ->
                                            {error, service_unavailable}
                                    end;
                                _ ->
                                    {error, no_host}
                            end;
                        _ ->
                            {error, invalid_uri}
                    end
            end;
        not_found ->
            {error, unkown_sipapp}
    end.

