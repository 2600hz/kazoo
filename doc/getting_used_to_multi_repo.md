# Getting used to multi-repo

INTRODUCTION

## Kazoo new source directory structure

Write something useful here

## Compiling in multi-repo

Explain what is going on with each app Makefile and dep.mk, etc....

## Working with `git` in multi-repo

As you work with multi-repo, especially when your code changes involves many apps or even the core, you start noticing some repetitive tasks like commit and push and updating the pull requests.

For making these kind of task more tolerable, there is small and handy `scripts/kz_git.sh` shell script which wraps around Git command. You simply give this script the directory name of an app (or multiple apps) and the git command you want to run against them and it loop over the folders and run the command. This script can also used together with `gh` [the official GitHub CLI](https://cli.github.com) or `hub` [an unofficial GitHub CLI](https://github.com/github/hub) (which is more mature and powerful and it also wraps `git`) commands.

The script tries to not get in your way and offers running `git` and other commands like normal command.

```
    __ _______         _ __
   / //_/__  /  ____ _(_) /_
  / ,<    / /  / __ `/ / __/
 / /| |  / /__/ /_/ / / /_
/_/ |_| /____/\__, /_/\__/ ☢
Welcome to   /____/ the FUTURE

A simple wrapper around git and pull request commands
to provide an easy way to develop on Kazoo multi-repo structure

Usage:

        kz_git.sh [OPTIONS]+ <ACTION> [ACTION_ARGS]+

    Where ACTION can be one of these commands to run in specified
    Kazoo source folders:

        git        Runs git command
        gh         Runs gh command (https://github.com/cli/cli)
        hub        Runs hub command (https://github.com/github/hub)

Example:

    kz_git.sh -kapps crossbar,teletype git status --branch --short

Options:

    -kapps app_name[,app_name...]:   add a comma separated list of Kazoo apps to working directories list
    -kcore:                          add "core" directory to working directories list
    -kroot:                          add kazoo source root directory to working directories list
    -a, --all-apps:                  add all Kazoo apps to working directories list
    -A, -All:                        add "core", all kazoo apps and kazoo source root directory to
                                         list of working directories. This option is default.
    -kchanged:                       Loop over working directories and prints the name of files which are
                                         different than "$BASE_BRANCH". "master" is the default BASE_BRANCH
                                     This does not run git/hub/gh commands
    -get-root                        print path to Kazoo source directory and exit
    -q                               be quiet, but still print error messages
    -qq                              be more quiet, also do not print errors
    -help:                           shows help
```


It is recommended to sym-link this script to `kgit` and put it in your path. `scripts/kz_git.completion.bash` provides bash completion for `kz_git.sh`. You can source this directly in your shell or add to your `~/.bashrc`.

**Some examples**

```
# run git status in all directories
$ kgit git status

# too noisy output, huh? make it short:
$ kgit git status --branch -s

# some git subcommands use pager, you can trun this off by "--no-pager"
# see latest commit message for core, crossbar, stepswitch and teletype
$ kgit -kcore -kapps crossbar,stepswitch,teletype git --no-pager log -1
# less noisy
$ kgit -q -kcore -kapps crossbar,stepswitch,teletype git --no-pager log -1 --format

# commit all changes just for core and root directory
# this will opens up your editor for each directory for commit message
# you can use "-m" to add message. you can even add multiple "-m" which converts
# each one to a paragraph
$ kgit -kcore -kroot git commit -a

# pull all apps
$ kgit -a git pull
```

Please find out usage examples for `gh` and `hub` here:

* [`gh` Examples in use](https://cli.github.com/manual/examples)
* [`hub` Usage examples](https://hub.github.com)
