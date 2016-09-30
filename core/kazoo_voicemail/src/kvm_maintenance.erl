%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Mailbox maintenance
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_maintenance).

-export([migrate/0
        % ,migrate/1
        % ,migrate/2
        ]).

-include("kz_voicemail.hrl").

-define(LOG(Format, Args),
        lager:debug(Format, Args),
        io:format(Format ++ "\n", Args)
       ).

%%--------------------------------------------------------------------
%% @public
%% @doc Migrate all messages in vmbox into the new modb format
%% @end
%%--------------------------------------------------------------------
-spec migrate() -> startlink_ret().
migrate() ->
    _ = process_flag('trap_exit', 'true'),
    {'ok', Pid} = kvm_migrate_crawler:start(self()),
    link(Pid),
    receive
        'done' -> 'ok';
        {'EXIT', Pid, 'normal'} -> 'ok';
        {'EXIT', Pid, _Reason} ->
            io:format("********** migration process died with reason ~p **********", [_Reason])
    end.

% -spec migrate(ne_binary()) -> 'ok'.
% -spec migrate(ne_binary(), ne_binary() | ne_binaries() | kz_json:object()) -> 'ok'.
% migrate(AccountId) ->
%     kvm_migrate_account:migrate(AccountId).

% migrate(AccountId, <<_/binary>> = BoxId) ->
%     migrate(AccountId, [BoxId]);
% migrate(AccountId, BoxIds) when is_list(BoxIds) ->
%     kvm_migrate_account:migrate(AccountId, BoxIds);
% migrate(AccountId, Box) ->
%     migrate(AccountId, kz_doc:id(Box)).
