# Kazoo Installation Guide

This is a guide to building Kazoo from source on a Debian 8 (Jessie) base installation. Other GNU/Linux distros should work similarly, though the dependencies may differ a bit. If you want to just install and use Kazoo (and not build it) try using the [installation instructions](https://docs.2600hz.com/sysadmin/doc/install/install_via_centos7/). The rest of this guide assumes you want to run a development environment for Kazoo.


## Dependencies


### Packages Required

```shell
sudo apt-get install build-essential libxslt-dev \
     zip unzip expat zlib1g-dev libssl-dev curl \
     libncurses5-dev git-core libexpat1-dev \
     htmldoc
```

Note: `htmldoc` is required only if [you want to be able to download PDFs](./announcements.md#company-directory-pdf).

1.  Docs-related

    When running `make docs`, some Python libraries are useful:

    ```shell
    sudo apt-get install python2.7 python-yaml
    sudo pip install mkdocs mkdocs-bootstrap mkdocs-bootswatch pymdown-extensions
    ```

    You can also run a local version of the docs with `make docs-serve` which will start a local server so you can view how the docs are rendered.

    If you have a custom theme, you can copy it to `doc/mkdocs/theme` and build the docs again. When you serve the docs the theme should have been applied to the site.


### Erlang

Kazoo 5.x targets Erlang 21+ (specifically 21.3 but consult [`make/erlang_version`](https://github.com/2600hz/kazoo/blob/master/make/erlang_version) to be sure). There are a couple ways to install Erlang:

1.  From Source

    I prefer to use a tool like [kerl](https://github.com/kerl/kerl) to manage my installations. If you want to play around with multiple versions of Erlang while hacking on Kazoo, this is probably the best way.

```shell
    curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
    chmod +x kerl
    mv kerl /usr/bin
    kerl list releases
    kerl build 21.3 21.3 # this takes a while
    kerl install 21.3 /usr/local/otp-21.3
    . /usr/local/otp-19.3/activate
```

You will probably want to add the `activate` command to your `.bashrc` or similar to make sure the proper OTP version is running.

2.  Erlang Solutions

    Install from the [Erlang Solutions](https://www.erlang-solutions.com/resources/download.html) packages. These tend to be kept up-to-date better than the default distro's packages.


## Building Kazoo


### Short version

```shell
cd /opt
git clone https://github.com/2600Hz/kazoo.git
cd kazoo
make
```


### Longer version

1.  Clone the Kazoo repo:

    ```shell
      git clone https://github.com/2600Hz/kazoo.git
    ```

2.  Build Kazoo:

    ```shell
    cd kazoo
    make
    ```

3.  Additional make targets

    When developing, one can `cd` into any app directory (within `applications/` or `core/`) and run:

    -   `make` (`make all` or `make clean`)
    -   `make xref` to look for calls to undefined functions (uses [Xref](http://www.erlang.org/doc/apps/tools/xref_chapter.html))
    -   `make dialyze` to statically type-check the app (uses [Dialyzer](http://www.erlang.org/doc/man/dialyzer.html))
    -   `make test` runs the app / sub-apps test suite, if any.
        -   **Note:** make sure to `make clean all` after running your tests, as test BEAMs are generated in `ebin/`!

4.  Running the tests

    To run the full test suite it is advised to:

    1.  `cd` into the root of the project
    2.  `make compile-test` to compile every app with the `TEST` macro defined
        -   *This way apps can call code from other apps in a kind of `TEST` mode*

    3.  `make eunit` (instead of `make test`) to run the test suite without recompiling each app
    4.  `make proper` to run the test suite, including property-based tests (uses [PropEr](https://github.com/manopapad/proper))

5.  Generate an Erlang release

    `make build-release` will generate a [deployable release](http://learnyousomeerlang.com/release-is-the-word)

    -   [More on using releases with Kazoo](https://github.com/2600Hz/kazoo/blob/master/doc/engineering/releases.md)


## SUP

The SUP command (`sup`) is found under `core/sup/priv/sup` and should be copied or symlinked to `/usr/bin/sup` (or somewhere in your `$PATH`). It is a shell file that calls `sup.escript`.

```shell
sudo ln -s core/sup/priv/sup /usr/bin/sup
```

Make sure that the path to Kazoo's installation directory is right (in `/usr/bin/sup`). Otherwise you can change it by setting the `KAZOO_ROOT` environment variable (not set by default). If one needs `KAZOO_ROOT`, an alias should be created:

```shell
alias sup='KAZOO_ROOT=/opt/kazoo sup'
```

1.  Auto-completion

    `make sup_completion` creates `sup.bash`: a Bash completion file for the SUP command

    -   Copy or symlink this file to `/etc/bash_completion.d/sup.bash`
