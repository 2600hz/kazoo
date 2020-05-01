# SUP

SUP is a command-line tool for interacting with a KAZOO node.

## Talking to KAZOO

SUP is built to talk to Erlang virtual machines (BEAM) and provide the user (you) with a command-line interface that is more familiar than connecting to the VM's shell and executing Erlang code directly on the VM.

SUP also provides some protection from foot-guns.

KAZOO provides a number of code modules, ending with the suffix `_maintenance`, callable from the SUP tool. Other modules can be called (provided any arguments from SUP are in the proper format and type expected by the Erlang module) as well but the maintenance mdoules are specifically built for SUP calls.

## Command line options

```shell
sup -h
Usage: sup [-?] [-n [<node>]] [-c [<cookie>]] [-t [<timeout>]] [-v]
           [<module>] [<function>] [-s [<use_short>]] [args ...]

  -?, --help       Show the program options
  -n, --node       Node name [default: kazoo_apps]
  -c, --cookie     Erlang cookie [default: change_me]
  -t, --timeout    Command timeout [default: 0]
  -v, --verbose    Be verbose
  <module>         The name of the remote module
  <function>       The name of the remote module's function
  -s, --use_short  Force using shortnames [default: undefined]
```

## Bash auto-completion

From the KAZOO root, you can run `make sup_completion` to generate a `{ROOT}/sup.bash` file that will contain the current auto-completion options for SUP. These are the maintenance modules and some other oft-used modules (such as for setting `system_config` values).

## Remote hosts

The ability to connect to remote hosts was removed from SUP in [2016](https://github.com/2600hz/kazoo/pull/2555/).

To permit this functionality, the port for the [EPMD](http://erlang.org/doc/man/epmd.html) service, 4369, needed to be exposed in firewalls. In addition, the distribution ports given to the Erlang VMs (that SUP uses to connect on), would need to be opened up as well. As Erlang VMs are only "protected" by a cookie, this was not sufficiently secure enough to allow these ports to be open to the wider Internet.

For now, SSH to the host and run SUP locally. You could also play with reverse tunnels but that can get tricky if you have EPMD running locally.
