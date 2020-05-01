# The Kazoo Makefile

Kazoo utilizes [GNU `make`](https://www.gnu.org/software/make/) for its build system. There is a project-level Makefile and each application (in `core/` and `applications/`) have their own Makefiles with any customizations needed by that particular application.

## Default target

`make` from the root directory will build Kazoo's Erlang dependencies, the core libs, and finally the applications.

## `make deps`

Fetches the 3rd-party Erlang applications and builds them in `deps/`

The Makefile will create a file `make/.deps.mk.{HASH}` on first build of the `deps/` where `{HASH}` is the sha1 of the `make/deps.mk`. Now, when the `make/deps.mk` file gets changed (when deps are added/removed/updated or when switching branches that have different deps (like from master to 4.x for instance)) the `deps/` directory will be rebuilt to ensure your branch is working with the correct set of dependencies.

## `make kazoo`

Builds the Erlang applications in `core/` followed by the applications in `applications/`.

This build process is [parallelizable](https://www.gnu.org/software/make/manual/html_node/Parallel.html) by specifying the `JOBS` environment variable:

```bash
JOBS=4
make kazoo

# or

JOBS=4 make kazoo
```

## Cleanup

### `make clean`

Cleans the ebin/ directories of `core/` and `applications`, as well as any application-specific cleanup.

### `make clean-deps`

Cleans the `deps/` directory. `make deps` is required to build it again.

### `make sparkly-clean`

Cleans up kazoo, releases, and deps in one target.

## `make compile-test`

Compiles `core/` and `applications/` in test mode; typically used to run the project's test suites via `make eunit`, `make test` or `make proper`.

## `make eunit`,  `make proper`, and `make test`

Runs the EUnit tests and PropEr tests.

## `make tags`

Builds a [TAGS file](https://en.wikipedia.org/wiki/Ctags) that is useful in navigating between modules and functions within the modules.

In Emacs, `visit-tags-table` will load the TAGS table and `erlang-find-tag` is useful in finding the `module:function` or `function` under point. You can override `M-.` (typically `find-tag`) to be Erlang-aware. [`EDTS`](https://github.com/tjarvstrand/edts) has a `edts-find-source-under-point` that will also respect the arity of the call under point.

### `make clean-tags`

Removes the TAGS file.

## Erlang Releases

### `make build-*-release`

Building an Erlang release comes in a couple flavors:

- `make build-release`: Production release
- `make build-dev-release`: Symlinks source-tree files into the release; any recompiled beams will be auto-loaded in the running release
- `make build-ci-release`: Release built for use in CI systems (CircleCI mainly)
- `make build-dist-release`: Release built for distribution (RPM/DEB)

### `make release`

Runs the now-built release. Change the Erlang VM name using the `REL` variable: `make release REL=foo` will start `foo@host.com`.

### `make clean-release`

Removes the `_rel/` where a built release may exist.

## Dialyzer

Typically running Dialyzer on the whole project will be painful unless you have tons of memory and CPU. More often, we want to dialyze changed files. There are a couple ways to do it:

### `make ci-dialyze`

Runs the equivalent Dialyzer run that CI runs. Just runs Dialyzer on source files that have changed, and only in batches of 5 at a time.

### `make dialyze-hard`

This is a great option if you have a beefier computer available. It will take the source files changed, run a first pass to find all modules being called, and will make a second pass using all the changed and called modules together in one big run.

!!! warning
    CPU/memory/time intensive.

### `make dialyze-changed`

Dialyzes all the changed files (compared to the parent branch) in a batch (vs 5 at a time like CI does) but doesn't pull in unknown modules like `dialyze-hard` does. Still memory/CPU intensive but not as bad as `hard` mode.

### Scripts

There's also a couple scripts which are nice:

#### `./scripts/dialyze-changed.bash`
Basically `dialyze-easy`; dialyzes all changed files together in one run.

Add additional `.beam` and `ebin/` paths for more accurate checks.

```bash
./scripts/dialyze-changed.bash

./scripts/dialyze-changed core/kazoo_stdlib/ebin core/kazoo_amqp/ebin/gen_listener.beam
```

#### `./scripts/dialyze-usage.bash`

Searches the codebase for usage of the module and includes those source files.

`./scripts/dialyze-usage.bash kz_json` would search for all modules using `kz_json` and dialyze them all together.

## `make xref`

Uses [xref](http://erlang.org/doc/man/xref.html) to check for calls to modules/functions/arities that don't exist.

## `make sup_completion`

Builds a bash auto-completion file for use with `SUP`.

## `make elvis`

Runs [Elvis style checker](https://github.com/inaka/elvis).

## `make fmt`

Runs the erlang-formatter over Erlang files (.erl/.hrl/.escript) and formats the source accordingly.

```bash
TO_FMT="path/to/foo.erl path/to/bar.erl" make fmt
```

If `TO_FMT` is unspecified, the changed source files will be used.

### `make fmt-all`

Runs `fmt` on all files in the project.

## `make app_applications`

Ensures the `{APP}.app.src` has an accurate list in the `applications` key.

## `make code_checks`

Runs various code-checking scripts to ensure project standards are met.

- No raw Erlang JSON terms are present (typically `{'struct', [...]}` and `{[...]}`). `kz_json` should be used and the data structure should be considered opaque.
- Dead-simple spell checker looking for low-hanging fruit.
- Auto-migration of older modules/functions to newer ones (or more appropriately-named ones)
- [Edoc](http://erlang.org/doc/apps/edoc/chapter.html) checks
- Auto-conversion of `get_stacktrace/0` usage

## `make apis`

Runs a host of scripts to auto-generate code, JSON schemas, and documentation.

## `make docs`

Attempts to build the docs site with `mkdocs` (basically a theme-less version of `docs.2600hz.com`) to make sure the index files (`doc/mkdocs/*.yml`) are present and internal linking between docs is consistent.

## `make circle`

Runs an equivalent pass of CircleCI locally.

## `make pest` and `make pest-all`

[PEST](https://github.com/okeuday/pest#usage) - Primitive Erlang Security Tool

Runs the security checks against changed (or all project) files and reports potential security threats. Not included in CI as most of the reports are speculative and probably not actionable (yet anyway)

## Spell checking

With Kazoo's international audience, it is helpful to have a spellchecker available to help all contributors find and fix spelling mistakes.

### `make splchk`

Runs the spell checker ([GNU `aspell`](http://aspell.net/)) against the `doc/` folder's Markdown files. If run within an application, will check that application's `doc/` folder's Markdown files.

### `make splchk-code`

When run from the project root, checks the `scripts` Escript source files.

When run in an application, checks `src` and `include` for `.erl` and `.hrl`.

This checker just looks at Erlang comments (prefixed by %) and atom/string/binary literals. So anything after `%` or between `'...'`, `"..."`, or `<<"...">>`.

## Changing the base branch

Several targets calculate the difference between a base branch (`origin/master` by default) and the current branch. Should you need to change the base branch you can prefix the command with `BASE_BRANCH="upstream/branch" make {TARGET}`
