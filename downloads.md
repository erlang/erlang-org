---
layout: downloads
---
{% assign sortedReleases = site.data.releases | sort: "release" | reverse %}
{% assign release = sortedReleases | first %}

### Download Erlang/OTP

The latest version of Erlang/OTP is {{ release.latest.name }} and it can be found [here]({{ release.latest.src }}).

Take a look at the [release description]( {% link _releases/{{ release.release }}.md %}) to see what changes Erlang/OTP {{ release.release }} brings. 

For a description of the changes that 

You can also clone the latest v

<!--end_excerpt-->

### Compiling Erlang from source

You can build Erlang from source on your own, following the [building and installation instructions](https://github.com/erlang/otp/blob/{{ release.latest.tag_name }}/HOWTO/INSTALL.md). In a nutshell to install a pre-built archive you need only do:

```sh
./configure && make && make install
```

If you clone the release from git, there may be some additional steps needed depending
on which version of Erlang/OTP you are compiling. So always make sure to read the
build and install instruction of the release you are compiling.

You can also use third-party tools such as [Kerl](https://github.com/kerl/kerl) or [asdf](https://github.com/asdf-vm/asdf-erlang) to compile Erlang. They help to remove the differences between Erlang/OTP releases and the OS you are compiling on.

### Source Versions and Windows Binaries for Patches

Detailed information about all released OTP versions since OTP 17.0 can be found at the [OTP Versions Tree](https://erlang.org/download/otp_versions_tree.html) page. This information includes a link to the GitHub source tag, and a link to the README. As of OTP 23, Windows binaries can also be downloaded from here for all patches.

### Pre-built Binary Packages

Most OS package managers provide pre-built binary packages.

* For Homebrew on OS X: `brew install erlang`
* For MacPorts on OS X: `port install erlang`
* For Ubuntu and Debian: `apt-get install erlang`
* For Fedora: `yum install erlang`
* For FreeBSD: `pkg install erlang`

The OS provided packages tend to not contain the latest version of Erlang/OTP.
So [Erlang Solutions](https://www.erlang-solutions.com/downloads/) provides up to date
pre-built packages for the latest stable releases. Erlang Solutions provides pre-built binary packages for Ubuntu, Debian, Fedora, CentOS and other operating systems.

### License

Since Erlang/OTP 18.0, Erlang/OTP is released under [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0). The older releases prior to Erlang/OTP 18.0 were released under [Erlang Public License (EPL)](/EPLICENSE), a derivative work of the Mozilla Public License (MPL).

