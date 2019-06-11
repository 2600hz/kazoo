# Installing on Mac (for development)

Some additional dependencies are required to get things up and running on Mac. The easiest way to
install these dependencies is via `brew` so install [brew|https://brew.sh/] if you haven't already.

Now install the dependencies listed in the installation.md file using `brew install <dependency name>`.
Some have different names on mac, such as:

* build-essential 
* zlib1g-dev 
* libssl-dev

## Additional Dependencies

Some applications are very old or configured differently in Mac OS, so use brew 
to install these to bring them up to date:
Note: Make sure to follow the directions on symlinking executables or adding executables 
to your path(brew will tell you about it after install completes)

* gnu-sed
* md5sha1sum
* make

Next install libreoffice, which can be done via `brew cask install libreoffice`.

### Fix executable paths

gnu-sed, md5 etc need their executables added to your path. When you install 
them via brew they give you the instructions on what to add.

You will also want to add a symlink for libreoffice somewhere in your path because the default 
executable name is `soffice` but kazoo expects it to be `libreoffice`. Something like:

```$ ln -s /usr/local/bin/soffice /usr/local/bin/libreoffice```
