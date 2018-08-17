
# Using Dialyzer on Kazoo

## Makefile commands

After building the Kazoo project or some of its sub-directories one is able to run Dialyzer on it.
Dialyzer will check `-spec`s and inferred type information to try and find unexpected behaviour or bugs.

### Run Dialyzer on applications/ and core/

This command, run from the root directory of the Kazoo repository,
will find BEAM files under `applications/` and `core/` and scan them with Dialyzer.

    make dialyze-kazoo

One can run Dialyzer only on `applications/` or `core/` with

    make dialyze-apps

or

    make dialyze-core

Note: all these Makefile targets depend on target `dialyze`.

### Run Dialyzer on particular files

Say you modified only a handful of files (be it even in different directories),
you can check only those files against Dialyzer using this command:

    make dialyze TO_DIALYZE='my_dir/ebin/my_file.beam other_dir/ebin/other_file.beam'

Note that Dialyzer is only run against BEAM files, but `make dialyze` will try and find the corresponding
BEAM files if given `*.erl` files or a `directory/`.

Thus note that your changed files need to be compiled for `make dialyze` to find and scan them.

Run Dialyzer on source files or whole directories:

    make dialyze TO_DIALYZE='my_dir/ebin path/to/src/file.erl'

### Where is the PLT?

This command, run from the root directory, will generate the PLT file Dialyzer needs.
This PLT includes OTP's main applications as well as Kazoo's `deps/`.

Note that including `core/` in the PLT will slow down Dialyzer significantly (and may crash if
your system does not have enough memory) while not adding much benefits to the scanning process.

    make build-plt

Any `make dialyze` command will automatically build the PLT, if it does not exist already.

Note that this PLT file is dependent on the Erlang version running,
which means after switching to another Erlang version the PLT needs to be regenerated.
One can easily switch between Erlang versions with `kerl` (not part of the Kazoo project).


## False positives and filters

Using a not up-to-date Dialyzer may give a lot of false positives:
* warnings on properly spec-ed functions
* or warnings about specs that match a "bigger" type than it could match (`warn_contract_supertype`)
* Particularly on R15B02 to R16B, warnings about `ets` types

To remove these there are filters implemented under `script/check-dialyzer.escript`.
This `escript` is what is behind the scenes of `make dialyze`.
One can call it directly and give it paths in place of using `make` and passing the `TO_DIALYZE` variable.

## Scripts to use

Other ways to use Dialyzer include:

- `scripts/dialyze-changed.bash`: dialyzes the changed erl files as compared to `origin/master`. You can supply additional beam files as arguments to the script as well.
- `scripts/dialyze-usage.bash': given a module name (like `kz_json`), find all modules that use the supplied module and dialyze them together. This should suss out any modules that use the supplied module in an incorrect way (as well as plenty of other issues too, probably)