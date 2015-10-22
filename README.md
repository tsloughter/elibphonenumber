elibphonenumber
===============

Erlang port of [libphonenumber](https://github.com/googlei18n/libphonenumber)

##Note

- This project is active. I'm doing updates very offen because I see the original author is no longer maintaining it.
- In case you want to discuss about the project you can find me on [WowApp][1]

To change the `libphonenumber` version mobify in rebar.config the argument sent to `build_deps.sh`

## Compile

You need compile `libphonenumber` and install it before compiling the project.

The rebar file automatically is doing this for Ubuntu (tested on 14.04 x64). For all other OS's you need to do it manually.

For `Ubuntu` in case you are not running rebar with sudo you need to run at least once the following:

    wget http://mt.archive.ubuntu.com/ubuntu/pool/universe/r/re2/libre2-1_20140304+dfsg-2_amd64.deb -O libre2-1.deb
    wget http://es.archive.ubuntu.com/ubuntu/pool/universe/r/re2/libre2-dev_20140304+dfsg-2_amd64.deb -O libre2-dev.deb
    sudo dpkg -i libre2*.deb
    sudo apt-get install cmake cmake-curses-gui libprotobuf-dev libgtest-dev libre2-dev libicu-dev libboost-dev libboost-thread-dev libboost-system-dev protobuf-compiler

## Run the tests

```sh
rebar compile eunit
```

[1]:https://www.wowapp.com/w/silviu/Silviu-Caragea
