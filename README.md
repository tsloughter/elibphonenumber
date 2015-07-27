elibphonenumber
===============

Erlang port for use [libphonenumber](https://github.com/googlei18n/libphonenumber) from erlang


## Documentation

[doc](http://artefactop.github.io/elibphonenumber/ "documentation")

## Compile and test

You need compile libphonenumber and install it before using the project. 

For Ubuntu

Find and download the Debian packages for your system. For example:

```
http://packages.ubuntu.com/utopic/libre2-1
http://packages.ubuntu.com/utopic/libre2-dev
```

You need to download both the libre2-dev and libre2-1 packages. Once downloaded, install them with:

```sh
sudo dpkg -i libre2*.deb
```

Then install all deps:

```sh
sudo apt-get install cmake cmake-curses-gui libprotobuf-dev libgtest-dev libre2-dev libicu-dev libboost-dev libboost-thread-dev libboost-system-dev protobuf-compiler
```

Compile libphonenumber

```sh
cd libphonenumber/cpp
mkdir build
cd build
cmake ..
make
make install
```

Update ldconfig path

```sh
﻿nano /etc/ld.so.conf.d/usrlocal.conf
﻿add inside the file: usr/local/lib
﻿ldconfig -v
```

Run the erlang lib test

```sh
rebar compile eunit
```
