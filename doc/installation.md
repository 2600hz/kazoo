# Kazoo Installation Guide

This is a guide to building Kazoo from source on a Debian 8 (Jessie) base installation. Other GNU/Linux distros should work similarly, though the dependencies may differ a bit. If you want to just install and use Kazoo (and not build it) try using the [installation instructions](https://docs.2600hz.com/sysadmin/doc/install/install_via_centos7/). The rest of this guide assumes you want to run a development environment for Kazoo.

If your development is on macOS, here are [extra steps](https://github.com/2600hz/kazoo/blob/master/doc/engineering/installing-on-mac.md) for set up.

## Dependencies

### Erlang

Erlang/OTP is required for both run time and build time. Usually the official Erlang package provided by distributions is not up-to-date, please follow this section to install it from source code or using other options.

* Kazoo version 4.3 requires Erlang version 19 specifically 19.3.x
* Kazoo version 5.x is currently targets Erlang version 21 (specifically 21.3)
* Kazoo master branch targets Erlang 21+ (specifically 21.3 but consult [`make/erlang_version`](https://github.com/2600hz/kazoo/blob/master/make/erlang_version) to be sure).

#### Installing Erlang/OTP from source code

We recommend to use a tool like [kerl](https://github.com/kerl/kerl) to manage Erlang/OTP installations. If you want to play around with multiple versions of Erlang while hacking on Kazoo, this is probably the best way. For compiling Erlang you need some dependencies, see [Build time requirements](#build-time-requirements)

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

#### Installing from Erlang Solutions repository

Most OS package managers provide pre-built binary packages. You can also download the latest stable releases [from Erlang Solutions](https://www.erlang-solutions.com/resources/download.html). Erlang Solutions provides [pre-built binary packages](https://www.erlang-solutions.com/resources/download.html) for OS X, Windows, Ubuntu, Debian, Fedora, CentOS, Raspbian and other operating systems.

* For Homebrew on maxOS: `brew install erlang`
* For Ubuntu and Debian: `apt-get install erlang`
* For CentOS/RHEL/Fedora: `yum install erlang`
* For FreeBSD: `pkg install erlang`


### Packages Required

#### Build time requirements

* General Linux development build packages (GNU Make v4+ is recommended):
    * `autoconf`, `automake`, `make`, `gcc`, `readline`
    * `openssl`, `libcurl`, `ncurses`, `zlib`, `bzip2`, `expat`
    * `git`
* Build time script requirements
    * `cpio` required for building docs
    * Python 3.5+
    * Python 3 packages for building docs:
        * `yaml`, `markdown`, `pyembed`
        * `mkdocs-bootstrap`, `mkdocs-bootswatch`, `pymdown-extensions`
    * `couchdb` (`validate-js` required `couchjs` command from CouchDB 2 to validate CouchDB view JavaScript codes)
    * Python 3 packages `jsonschema`, `jsbeautifier` for validating JSON schemas and formatting CouchDB views
    * `silversearcher-ag` required by `scripts/edocify.escript`

#### Run time requirements

Main functionality requirements:

* CouchDB 2
    * Please consult [CouchDB installation doc](https://docs.couchdb.org/en/stable/install/index.html)
    * **Note:** Old abandoned BigCouch can be used too but this is not recommended for new clusters
* RabbitMQ
    * Please consult [RabbitMQ installation doc](https://www.rabbitmq.com/download.html)
    * Recommended official 2600Hz [RabbitMQ configurations](https://github.com/2600hz/kazoo-configs-rabbitmq)
    * **Note:** Kazoo 5 (master) requires `rabbitmq_consistent_hash_exchange` plug-in to be enabled for Kazoo Fax application

Voice/Video and SIP functionality requirements:

* FreeSWITCH version 1.10+
    * It is required `mod_kazoo` plug-in to be enabled
    * Official 2600Hz [FreeSWITCH configurations](https://github.com/2600hz/kazoo-configs-freeswitch) is required
    * It is recommended to install FreeSWITCH from [2600Hz CentOS repository](https://docs.2600hz.com/sysadmin/doc/install/install_via_centos7) to ease the installation.
* Kamailio version 5.2+
    * It is required `kazoo` module to be enabled
    * Official 2600Hz [Kamailio configurations](https://github.com/2600hz/kazoo-configs-kamailio) is required
    * It is recommended to install Kamailio from [2600Hz CentOS repository](https://docs.2600hz.com/sysadmin/doc/install/install_via_centos7) to ease the installation.
    * If you're installing the official Kamailio packages instead, you have to change the database in 2600Hz Kamailio configurations to use `sqlite` or you're choice of database.


Other packages:

* `zip`, `unzip`
* `htmldoc` required if [you want to be able to download PDFs](./announcements.md#company-directory-pdf)
* `sox` for normalizing audio files (Debian needs `libsox-fmt-all` to support MP3 audio format)
* `ghostscript`, `imagemagick` `libtiff-tools`, `libreoffice-writer` with Java Run time environment for Fax functionality and fax media file conversions

Useful commands:

* `sudo`, `less`, `whois`, `strace`, `tcpdump`, `net-tools`
* `ntpdate`, `bind-utils`, `which`, `file`, `psmisc`, `iproute`, `lsof`


### Installing dependencies on Debian 9+

```shell
## Adding official CouchDB 2 repo
sudo apt-get install apt-transport-https
echo "deb https://apache.bintray.com/couchdb-deb stretch main" | sudo tee -a /etc/apt/sources.list
curl -L https://couchdb.apache.org/repo/bintray-pubkey.asc | sudo apt-key add -

## Kazoo buildtime dependencies
sudo apt-get install \
    build-essential libxslt-dev \
    zip unzip expat zlib1g-dev libssl-dev curl \
    libncurses5-dev git-core libexpat1-dev \
    python3-yaml python3-markdown python3-jsonschema python3-pip \
    python3-jsbeautifier \
    cpio mkdocs \
    couchdb \
    silversearcher-ag jq

## doc build target
sudo pip3 install pyembed mkdocs-bootstrap mkdocs-bootswatch pymdown-extensions

## Kazoo runtime dependencies
sudo apt-get install \
    htmldoc sox libsox-fmt-all ghostscript \
    imagemagick libtiff-tools openjdk-8-jre libreoffice-writer

## Linking couchjs so validate-js script can find it
sudo ln -s /opt/couchdb/bin/couchjs /usr/bin/couchjs

## don't forget to uncomment "en_US.UTF-8 UTF-8" line (and your choice of language)
## from /etc/locale.gen file and run the command below:
sudo locale-gen
```


### Installing dependencies on CentOS 7

```shell
## Adding useful repos
sudo cat <<'EOF' > /etc/yum.repos.d/bintray-apache-couchdb-rpm.repo
[bintray--apache-couchdb-rpm]
name=bintray--apache-couchdb-rpm
baseurl=http://apache.bintray.com/couchdb-rpm/el7/$basearch/
gpgcheck=0
repo_gpgcheck=0
enabled=1
EOF


sudo yum install epel-release
sudo yum update

## kazoo master (5.0+) needs git2
sudo yum remove -y git*
sudo yum install https://centos7.iuscommunity.org/ius-release.rpm

## Kazoo buildtime dependencies
sudo yum install \
    autoconf automake bzip2-devel elfutils expat-devel gcc-c++ gcc \
    git2u-all glibc-devel libcurl libcurl-devel libstdc++-devel \
    libxslt make ncurses-devel openssl openssl-devel patch \
    patchutils readline readline-devel unzip wget zip zlib-devel \
    python36-pip the_silver_searcher jq cpio

## installing required (and optional docs building) python packages
sudo pip3 install pyyaml markdown jsonschema jsbeautifier

## doc build target
sudo pip3 install pyembed mkdocs-bootstrap mkdocs-bootswatch pymdown-extensions

## Kazoo runtime dependencies
sudo yum install \
    htmldoc sox ghostscript \
    ImageMagick libtiff-tools libreoffice-writer

## make sure UTF-8 locale is set correctly
localedef -v -c -i en_US -f UTF-8 en_US.UTF-8
```

## Building Kazoo

### Short version

```shell
git clone https://github.com/2600Hz/kazoo.git
cd kazoo
make
```

### Building and serving local version of documentation site

You following command to build documentation site:

```shell
make docs
```

You can also run a local version of the docs with `make docs-serve` which will start a local server so you can view how the docs are rendered.
If you have a custom theme, you can copy it to `doc/mkdocs/theme` and build the docs again. When you serve the docs the theme should have been applied to the site.


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
    -   `make apis` to build document accessors, Crossbar API OpenAPI, JSON schemas and etc...
    -   `make xref` to look for calls to undefined functions (uses [Xref](http://www.erlang.org/doc/apps/tools/xref_chapter.html))
    -   `make dialyze` to statically type-check the app (uses [Dialyzer](http://www.erlang.org/doc/man/dialyzer.html))
    -   `make fmt` to format Erlang source codes to the standard
    -   `make fmt-views-all` to format JavaScript codes inside CouchDB view files
    -   `make test` runs the app / sub-apps test suite, if any.
        -   **Note:** make sure to `make clean all` after running your tests, as test BEAMs are generated in `ebin/`!
    -   Consult `Makefile` for more Makefile targets

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

6.  Generate an Erlang development release

    `make build-dev-release` will generate a development release with symlinking beams files and reload them when they re-compiled to ease development

7.  Start an Erlang development release

    CouchDB2 and RabbitMQ server have to be up and running prior to start Kazoo. The Development Environment Dependency section describes how these components should be built or installed.

    `make release REL=<node name>` will start Kazoo dev release with an Erlang shell accessible.

    The node name will become the longname in distributed Erlang, therefore, it is very important to set up local domain or DNS to resolve FQDN. To verify long name resolution, `erl -name test` should be able to start an Erlang shell with `node@fqdn` as the prompt. It's not uncommon for a dev machine without DNS set up that can result `erl -name test` to crash, which has been seen on Mac.

    After the dev shell starts, we can verify Kazoo's epmd registration with the proper longname by this command under Kazoo Erlang shell expected to return a valid port instead of `noport`.
    `erlang-shell> erl_epmd:port_please("<your node name>", "<fqdn of the server>").

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

## Development Environment Dependency
Other dependencies include CouchDB and RabbitMQ. `yum` or `apt` install rabbitmq-server is sufficient. We recommend to clone the latest [CouchDB 2](https://github.com/apache/couchdb/) and follow [the build instructions](https://github.com/apache/couchdb/blob/master/INSTALL.Unix.md).

To run a cluster of CouchDB nodes, please `yum` or `apt` install haproxy.

To start out fresh, we recommend to run admin party with CouchDB that allows request to be made by anyone without user credential authentication. This is an example command to start 3 nodes cluster with haproxy automatically started and admin party enabled.

```shell
dev/run --with-admin-party-please -n 3 --with-haproxy
```

Once CouchDB, rabbitmq-server, and kazoo are all started. You should be able to verify their names registered with epmd with this command.

```shell
epmd -names
```
