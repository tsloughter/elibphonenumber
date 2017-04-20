elibphonenumber
===============

Erlang port of [libphonenumber][1]

## Notes

- This project is active. I'm doing updates very offen because I see the original author is no longer maintaining it.
- Compatible with both `rebar` and `rebar3`
- To change the `libphonenumber` version modify in `rebar.config` the `TAG` argument sent to `make`

## Compile

You need compile `libphonenumber` and install it before compiling the project.

Currently this is automatically done on Ubuntu (tested on 14.04 x64) and Mac OS using `rebar` and `rebar3`.
For all other OS's you need to do it manually or adjust the `build_deps.sh`.

##### Mac Os

On `Mac OS` make sure you have brew installed and rebar will automatically run:

```sh
brew install boost cmake icu4c pkg-config protobuf wget
```
 
##### Ubuntu
 
For `Ubuntu` in case you are not running `rebar` with sudo you need to run at least once the following:

```sh
wget http://mt.archive.ubuntu.com/ubuntu/pool/universe/r/re2/libre2-1_20140304+dfsg-2_amd64.deb -O libre2-1.deb
wget http://es.archive.ubuntu.com/ubuntu/pool/universe/r/re2/libre2-dev_20140304+dfsg-2_amd64.deb -O libre2-dev.deb
sudo dpkg -i libre2*.deb
sudo apt-get install cmake cmake-curses-gui libprotobuf-dev libgtest-dev libre2-dev 
sudo apt-get install libicu-dev libboost-dev libboost-thread-dev libboost-system-dev protobuf-compiler
```

Under Ubuntu 14.04 TLS also you need to run the following to install cmake 3.x :
    
```sh
sudo apt-get remove cmake cmake-data
sudo -E add-apt-repository -y ppa:george-edison55/cmake-3.x
sudo -E apt-get update
sudo apt-get install cmake
```    

## Get carrier for number
    
In order to do this make sure the application is started then use `phonenumber_to_carrier:carrier_for_number/2` method    
    
```erlang 
application:ensure_all_started(elibphonenumber).
phonenumber_to_carrier:carrier_for_number(<<"44743655551">>, <<"en">>).
```    

## Run the tests

```sh
rebar compile eunit
```

[1]: https://github.com/googlei18n/libphonenumber