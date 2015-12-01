# Escalus [![Build Status](https://travis-ci.org/esl/escalus.svg?branch=master)](https://travis-ci.org/esl/escalus)

Escalus is an Erlang XMPP client library.
It began as a tool for convenient testing of XMPP servers,
but can also be used as a standalone Erlang application.

Escalus is aimed at checking correctness of XMPP server behaviour,
in contrast to tools such as Tsung which are about stress testing
and don't verify correctness.

This tool, escalus, is used by [ESL's amoc](https://github.com/esl/amoc) for load tests against [ESL's MongooseIM](https://github.com/esl/MongooseIM).


# Quick start

The [test/example_SUITE.erl][example_SUITE] file contains a minimalistic
example of an Escalus test suite.

[example_SUITE]: /test/example_SUITE.erl

You should include `escalus.hrl` file and the Common Test header:

    -include_lib("escalus/include/escalus.hrl").
    -include_lib("common_test/include/ct.hrl").

Escalus contains functions `escalus:init_per_suite/1`,
`escalus:end_per_suite/1`, `escalus:init_per_testcase` and
`escalus:end_per_testcase` which should be called in
appropriate Common Test callback functions.
Calling `escalus:init_per_testcase` is mandatory as this function
initializes the runtime support for `escalus:story`
(i.e. `escalus_cleaner` -- actually, you can do it manually if you know
what you're doing).

You can specify users that will take part in your tests
in Common Test config files,
look at [test/test.config][test_config] file that comes with Escalus:

    {escalus_users, [
        {alice, [
            {username, "alice"},
            {server, "localhost"},
            {password, "makota"}]},
        {bob, [
            {username, "bob"},
            {server, "localhost"},
            {password, "bobcat"}]}
    ]}.

[test_config]: /test/test.config

Escalus can create and delete those users in two ways:
 * using in-band registration [XEP-0077](http://xmpp.org/extensions/xep-0077.html)
  when it is supported by the server and has no limits on number of registrations
  per second (configure `registration_timeout` to `infinity` in case of ejabberd).
 * using erlang rpc calls to the ejabberd_admin:register/2 function (incase of MongooseIM or ejabberd as the tested server and the in-band registration is disabled)

You create and delete the users by calling `escalus:create_users/1`
and `escalus:delete_users/1`:

    init_per_group(_GroupName, Config) ->
        escalus:create_users(Config).

    end_per_group(_GroupName, Config) ->
        escalus:delete_users(Config).

In our exemplary test case it is done in `init_per_group` and `end_per_group`
functions, but you could as well do it in `init-/end_per_suite` if you prefer.
Deleting users should clean all their data (e.g. roster buddies), so it
improves test isolation, but takes longer.

In most of the test cases you will use `escalus:story/3` function.
Story wraps all the test and does the cleanup and initialisation:

    messages_story(Config) ->
        escalus:story(Config, [1, 1], fun(Alice, Bob) ->

            %% Alice sends a message to Bob
            escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

            %% Bob gets the message
            escalus:assert(is_chat_message, [<<"OH, HAI!">>],
                           escalus:wait_for_stanza(Bob))

        end).

The story above involves two users (second argument is a two-element list)
each having one resource (list contains ones). As you see from
the config files, those users are Alice and Bob. Escalus logs in
users at the beginning of the story and logs them out after it ends
(either successfully or by crash).

It's also possible to designate users taking part in a story more
specifically:

    messages_story(Config) ->
        escalus:story(Config, [{alice, 1}, {kate, 1}], fun(Alice, Kate) ->
            ...
        end).

That allows one to choose users which are not consecutive
in [test/test.config][test_config].

Inside the story you can use `escalus:send/2` function to send
stanzas, functions from `escalus_stanza` module to create them
and `escalus:wait_for_stanza` to receive them.

`wait_for_stanza` makes test fail if no stanza arrives
up to a timeout. There is also `wait_for_stanzas` function which
takes number of stanzas N as an argument and returns N-element or
shorter list of stanzas, returning less stanzas instead of crashing.
Both `wait_for_stanza` and `wait_for_stanzas` can take an extra argument --
timeout in milliseconds. The default timeout value is one second.

You make assertions using `escalus:assert/3` function.
First argument is the predicate. It can be a fun,
a `{module, function}` tuple or an atom. Atoms refer
to functions from `escalus_pred` module. Second
argument is a parameter list and third is a stanza that
we assert things about. There is `escalus:assert/2`
function that is equivalent to `assert/3` with empty
parameter list. Calling `escalus:assert(Pred, [Param1, Param2], Stanza)`
makes sure that Pred(Param1, Param2, Stanza) yields true.
Stanza is separate from parameters to improve error reporting.


# Escalus as a standalone application

It's possible to use Escalus as a standalone application,
i.e. outside a Common Test test suite (and without any reliance
on the `common_test` application and its modules).
If you use **rebar3** tool there are only few steps to generate
full escalus release. Just type in your bash shell:

    rebar3 release

and wait until it finishes. It is now possible to start
erlang shell with command:

    $ESCALUS_ROOT/_build/default/rel/escalus/bin/escalus

You can now enjoy usage of escalus application in your erlang release!
In order to use escalus as standalone application **without rebar3**
some prerequisites must be met.

Firstly, Escalus must be started just like any other application:

    > application:ensure_all_started(escalus).

This makes predefined environment variables from `escalus.app` available
for access by `application:get_env`.
These options and their respective values for running without Common Test are:

    {env, [%% Set config_file in case of using Escalus without Common Test
           %% (i.e. common_test is false).
           {common_test, false},
           {config_file, "priv/escalus.config"}]}

To recap:

-   `common_test` must be false - this will tell Escalus not to rely on
    modules available in the `common_test` application;
    this also disallows the use of `escalus_ejabberd:rpc` and similar
    functions,

-   `config_file` **must** be set to a configuration file location;
    this location may be absolute or relative (in which case the file will
    be searched for relative to the project directory).

Note, that in a real security-conscious setting you probably shouldn't
store clear text user passwords in this file (though that's exactly what
the example does - remember Escalus is still mostly a testing tool).

If you don't want to rely on the application resource file
(`escalus.app`/`escalus.app.src`) you can set both of these options just
after loading Escalus:

    > application:ensure_all_started(escalus).
    > application:set_env(escalus, common_test, false).
    > application:set_env(escalus, config_file, "/absolute/or/relative/path").

Keep in mind that calling `application:ensure_all_started(escalus)` will
overwrite the values with stuff from `escalus.app`.
Set the variables after the application is started.

## Config file location

If the `config_file` value starts with `/` it's  interpreted as an
absolute path and left as is.
Otherwise, it's interpreted as a relative path to the project directory.
The project directory is the directory one level higher than the directory
containing `ejabberd_ct.beam`.
In case of a standard Git checkout the project directory is simply `escalus`.

    escalus/
    ├── .git/
    ├── ...
    ├── docs/
    ├── ebin/
    │   ├── ...
    │   ├── escalus_ct.beam
    │   └── ...
    ├── src/
    └── ...

## Example shell session

Fire an Erlang shell:

    erl -pa ebin deps/*/ebin

### Basic example

Run example:

    application:ensure_all_started(escalus).
    application:set_env(escalus, common_test, false).
    {ok, FileConfig} = file:consult("priv/escalus.config").
    Config = escalus_cleaner:start(FileConfig).
    FaxSpec = escalus_users:get_options(Config, fax).
JFull = proplists:get_value(jid, FaxSpec).
BareJID = proplists:get_value(short_jid, FaxSpec).
Resource = proplists:get_value(resource, FaxSpec).

{ok, Fax} = escalus_client:start(Config, FaxSpec, Resource).

Document = <<"<iq type='set' to='",BareJID/binary,"'>"
                 ,   "<subscribe xmlns='google:push'>"
                 ,      "<item channel='cloudprint.google.com' from='cloudprint.google.com'/>"
                 ,   "</subscribe>"
                 ,"</iq>"
               >>.

Sub = escalus_stanza:from_xml(Document).

escalus_client:send(Fax, Sub).
escalus_client:wait_for_stanzas(Fax,1).    

Pre = lists:foldr(fun({F,A},B)-> F(B, A) end, escalus_stanza:presence(<<"available">>), [{fun escalus_stanza:to/2, JFull}, {fun escalus_stanza:from/2, BareJID}]).
escalus_client:send(Fax, Pre).

    {ok, Fax, _, _} = escalus_connection:start(FaxSpec).
    

    escalus_connection:send(Carol, escalus_stanza:chat_to(alice, "hi")).
    escalus_connection:stop(Carol).

### Story example

Please note that `escalus:story/3` and `escalus:create_users/2` are intended to be used in a testing environment, i.e. with Common Test available. Specifically, `escalus:create_users/2` will not work without Common Test and with non-XMPP registration method chosen (i.e. RPC based user registration). In case of MongooseIM or ejabberd, please ensure `mod_register` is enabled (and, depending on your scenario, probably configured not to send a _welcome message_).

Run example:

    X2SFun = fun(X) -> lists:flatten(io_lib:format("~p~n", [X])) end.
    {ok, Config0} = file:consult("priv/escalus.config").
    application:ensure_all_started(escalus).
    application:set_env(escalus, common_test, false).
    escalus:create_users(Config0, {by_name, [alice, mike]}).
    Config = escalus_event:start(escalus_cleaner:start(Config0)).
    SendFun = fun(A, B) -> escalus:send(A, escalus_stanza:chat_to(B, "hi")), ok end.
    RecvFun = fun(B) -> [S] = escalus:wait_for_stanzas(B, 1), {ok, S} end.
    StoryFun = fun(A, B) -> SendFun(A, B), {ok, S} = RecvFun(B), erlang:display(X2SFun(S)) end.
    escalus:story(Config, [{mike, 1}, {alice,1}], StoryFun).
    escalus_cleaner:stop(escalus_event:stop(Config)).
    escalus:delete_users(Config, {by_name, [alice, mike]}).


# Naming

According to [Wikipedia](https://en.wikipedia.org/wiki/Characters_in_Romeo_and_Juliet#House_of_Escalus), Prince Escalus, of the House Escalus, is the voice of authority in Verona, and appears only three times within the text and only to administer justice.

It follows the great tradition to use characters of William Shakespeare's Romeo and Juliet in the XMPP specifications.
