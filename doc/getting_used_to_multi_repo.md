# Getting used to multi-repo

INTRODUCTION

## Kazoo new source directory structure

Write something useful here

## Compiling in multi-repo

Explain what is going on with each app Makefile and dep.mk, etc....

## Working with `git` in multi-repo

As you work with multi-repo, especially when your coding session involves many apps or even the core, you start noticing some repetitive tasks like commit and push and updating the pull requests.

For making these kind of task more tolerable, there is small and handy `scripts/kz_git.sh` shell script which wraps around Git command. You simply give this script the directory name of an app (or multiple apps) and the git command you want to run against them and it loop over the folders and run the command. This script can also used together with `gh` [the official GitHub CLI](https://cli.github.com) or `hub` [an unofficial but more powerful GitHub CLI](https://github.com/github/hub) commands.

The script tries to not get in your way and offers running `git` and other commands like normal command.

```
Usage:
    kz_git.sh [OPTIONS]+ <ACTION> [ACTION_ARGS]+

Where ACTION can be one of these commands to run in specified
Kazoo source folders:

    git        Runs git command
    gh         Runs gh command (https://cli.github.com)
    hub        Runs hub command (https://github.com/github/hub)
```


