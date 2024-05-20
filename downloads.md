---
layout: downloads
---
{% assign sortedReleases = site.data.releases | sort: "release" | reverse %}
{% assign release = sortedReleases | first %}

### Download Erlang/OTP

The latest version of Erlang/OTP is [{{ release.latest.name }}]({% link _patches/OTP-{{ release.latest.name }}.html %}). To install Erlang you can either build it [from source](#source) or use a [pre-built package](#prebuilt).

Take a look at the [Erlang/OTP {{ release.release }} release description]({% link _releases/{{ release.release }}.md %}) to see what changes Erlang/OTP {{ release.release }} brings over the previous major version.

The Erlang/OTP version scheme is described in the [Erlang/OTP Systems Principles Guide]({% link doc/system/versions.html %}#version-scheme).

<!--end_excerpt-->

### Compiling Erlang from source <a href="#source" name="source">#</a>

You can build Erlang from source on your own, following the [building and installation instructions]({% link doc/system/install.html %}). In a nutshell to install a pre-built archive you need only do:

```bash
./configure && make && make install
```

If you clone the release from git, there may be some additional steps needed depending on which version of Erlang/OTP you are compiling. So always make sure to read the build and install instruction of the release you are compiling.

You can also use third-party tools such as [Kerl](https://github.com/kerl/kerl) or [asdf](https://github.com/asdf-vm/asdf-erlang) to compile Erlang. They help to remove the differences between Erlang/OTP releases and the OS you are compiling on.

### Pre-built Binary Packages <a href="#prebuilt" name="prebuilt">#</a>

Most OS package managers provide pre-built binary packages.

* For Homebrew on macOS: `brew install erlang`
* For MacPorts on macOS: `port install erlang`
* For Ubuntu and Debian: `apt-get install erlang`
* For Fedora: `yum install erlang`
* For ArchLinux and Manjaro: `pacman -S erlang`
* For FreeBSD: `pkg install erlang`

The OS provided packages tend to not contain the latest version of Erlang/OTP.
So [Erlang Solutions](https://www.erlang-solutions.com/downloads/) provides up to date pre-built packages for the latest stable releases. Erlang Solutions provides pre-built binary packages for Ubuntu, Debian, Fedora, CentOS and other operating systems.

### License

Since Erlang/OTP 18.0, Erlang/OTP is released under [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0). The older releases prior to Erlang/OTP 18.0 were released under [Erlang Public License (EPL)](/EPLICENSE), a derivative work of the Mozilla Public License (MPL).

