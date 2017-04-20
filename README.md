elibphonenumber
===============

Erlang port of [libphonenumber](https://github.com/googlei18n/libphonenumber)

##Note

- This project is active. I'm doing updates very offen because I see the original author is no longer maintaining it.
- Compatible with both `rebar` and `rebar3`
- To change the `libphonenumber` version modify in `rebar.config` the `TAG` argument sent to `make`

## Compile

You need compile `libphonenumber` and install it before compiling the project.

The `rebar` or `rebar3` automatically will do this for Ubuntu (tested on 14.04 x64) and Mac OS. 
For all other OS's you need to do it manually or adjust the `build_deps.sh`.

For `Ubuntu` in case you are not running `rebar` with sudo you need to run at least once the following:

    wget http://mt.archive.ubuntu.com/ubuntu/pool/universe/r/re2/libre2-1_20140304+dfsg-2_amd64.deb -O libre2-1.deb
    wget http://es.archive.ubuntu.com/ubuntu/pool/universe/r/re2/libre2-dev_20140304+dfsg-2_amd64.deb -O libre2-dev.deb
    sudo dpkg -i libre2*.deb
    sudo apt-get install cmake cmake-curses-gui libprotobuf-dev libgtest-dev libre2-dev libicu-dev libboost-dev libboost-thread-dev libboost-system-dev protobuf-compiler

On `Mac OS` make sure you have brew installed    

Also you need to use cmake 3.x so you need to :
    
    sudo apt-get remove cmake cmake-data
    sudo -E add-apt-repository -y ppa:george-edison55/cmake-3.x
    sudo -E apt-get update
    sudo apt-get install cmake

## Run the tests

```sh
rebar compile eunit
```
