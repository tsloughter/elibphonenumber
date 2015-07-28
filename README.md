elibphonenumber
===============

Erlang port of [libphonenumber](https://github.com/googlei18n/libphonenumber)

To change the `libphonenumber` version mobify the deps commit id in `build_deps.sh`.

## Compile and test

You need compile `libphonenumber` and install it before using the project.
The rebar file automatically is doing this for Ubuntu (tested on 14.04 x64). For all other OS's you need to do it manually. 

For `Ubuntu` in case the rebar fails you can try to do the following:

Find and download the Debian packages for `libre2-1` and `libre2-dev`. For example:

```sh
http://packages.ubuntu.com/utopic/libre2-1
http://packages.ubuntu.com/utopic/libre2-dev
```

Once downloaded, install them with:

```sh
sudo dpkg -i libre2*.deb
```

Then install all other deps required:

```sh
sudo apt-get install cmake cmake-curses-gui libprotobuf-dev libgtest-dev libre2-dev libicu-dev libboost-dev libboost-thread-dev libboost-system-dev protobuf-compiler
```

Compile `libphonenumber`

```sh
git clone https://github.com/googlei18n/libphonenumber.git
cd libphonenumber/cpp
mkdir build
cd build
cmake ..
make
make install
```

Update ldconfig path

```sh
sudo nano /etc/ld.so.conf.d/usrlocal.conf
add inside the file: /usr/local/lib
sudo ldconfig -v
```

Run the erlang lib test

```sh
rebar compile eunit
```
