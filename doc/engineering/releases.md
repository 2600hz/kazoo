/*
Section: Kazoo
Title: Releases
*/

# How to use Erlang releases with Kazoo

Kazoo is bundled and shipped as an Erlang release.
This means Erlang/OTP's and Kazoo's code are mixed together under `_rel/`, thus creating a standalone Kazoo.

[relx](https://github.com/erlware/relx/wiki) builds the release using
* `rel/relx.config.script` (this file in fact generates `rel/relx.config`)
* `rel/vm.args` has all the [BEAM VM arguments](http://erlang.org/doc/man/erl.html#emu_flags) used for running the release
* `rel/sys.config` (or `/etc/kazoo/app.config` when present) which defines environment values for Erlang apps.

Information on Erlang releases and live-updates:
* [LYSE's first chapter on releases](http://www.erlang.org/doc/design_principles/release_structure.html)
* [OTP's man page on releases](http://www.erlang.org/doc/design_principles/release_structure.html)

## Makefile commands

All the following commands have to be run from the root directory of the Kazoo repository.

### Build the release

    make build-release

Creates `rel/relx.config`, the file Relx uses then to generate the release under `_rel`.

This folder contains two binaries under `_rel/kazoo/bin`: `kazoo` and `kazoo-$VSN`.
These are the exact same binary.

### Node types

The release can be spawned as either one of the following node types:
* `ecallmgr`: when booting, the VM will start the `ecallmgr` application and its dependencies
* `whistle_apps`: when booting, the VM will start the `whistle_apps` application and its dependencies

Once booted, the node listens to its assigned ports, writes to the system logs, does everything Kazoo did when it wasn't a release.
Releases add no scoping nor sandboxing capabilities.

`REL` is the Makefile variable that stipulates the node type of the starting release.
It defaults to `whistle_apps` and is used as if calling `erl` with `-name` option set to `$(REL)@$(hostname)`.

### Start a node

Once built, start a release in "attached mode":

    make release

Which is equivalent to

    ACT=console REL=whistle_apps make release

There are different ways to start/stop a release (set the Makefile `ACT` variable accordingly):
* `console` (default value): starts a node killable with `^G q`
* `start`: creates a pipe that can be connected to later with the command `attach`
* `attach`: show the REPL of a `start`-ed node
* `stop`
* `foreground`

For local development you will likely use `console`. In production you will want `start` or `foreground`.

The Erlang VM calls `fsync` on every line of output in `start` mode, so `foreground` might be better for your use cases.

To open a console with a node started with `foreground`, use `remote_console`.


### Service a production node

If the `ecallmgr` died, this would start another VM:

    REL=ecallmgr ACT=start  make release


When troubleshooting, this attaches a REPL to the running faulty `ecallmgr` node:

    REL=ecallmgr ACT=attach make release


This gracefully stops an `ecallmgr` node:

    REL=ecallmgr ACT=stop   make release


### Read a release's cookie from Kazoo's configuration

    REL=whistle_apps make read-release-cookie

This hits `/etc/kazoo/config.ini` or the file at `$KAZOO_CONFIG`
and reads the cookie value for release `REL`.

Note: the release does not even need to be started!
