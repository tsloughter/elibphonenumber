#!/usr/bin/env bash

DEPS_LOCATION=_build/deps
DESTINATION=libphonenumber

if [ -d "$DEPS_LOCATION/$DESTINATION" ]; then
    echo "libphonenumber fork already exist. delete $DEPS_LOCATION for a fresh checkout."
    exit 0
fi

OS=$(uname -s)
KERNEL=$(echo $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}') | awk '{print $1;}')

LIB_PHONE_NUMBER_REPO=https://github.com/googlei18n/libphonenumber.git
LIB_PHONE_NUMBER_REV=$1

echo "Use repo ${LIB_PHONE_NUMBER_REPO} and revision ${LIB_PHONE_NUMBER_REV}"

function fail_check
{
    "$@"
    local status=$?
    if [ $status -ne 0 ]; then
        echo "error with $1" >&2
        exit 1
    fi
}

qmake_unix()
{
    export CFLAGS=-fPIC
    export CXXFLAGS=-fPIC
	cmake -DCMAKE_INSTALL_PREFIX:PATH=install  ..
}

qmake_darwin()
{
    ICU_PATH=/usr/local/Cellar/icu4c
    CMD_VERSION=`ls ${ICU_PATH}`
    ICU_VERSION=`echo $CMD_VERSION | awk '{print $0}'`

    echo "ICU_PATH=${ICU_PATH}"
    echo "ICU_VERSION=${ICU_VERSION}"

    export CFLAGS="-fPIC -Wno-deprecated-declarations"
    export CXXFLAGS="-fPIC -Wno-deprecated-declarations"

	fail_check cmake -DCMAKE_INSTALL_PREFIX:PATH=install \
	      -DGTEST_SOURCE_DIR=../../../googletest/googletest/ \
          -DGTEST_INCLUDE_DIR=../../../googletest/googletest/include/ \
          -DICU_UC_INCLUDE_DIR=${ICU_PATH}/${ICU_VERSION}/include/ \
          -DICU_UC_LIB=${ICU_PATH}/${ICU_VERSION}/lib/libicuuc.dylib \
          -DICU_I18N_INCLUDE_DIR=${ICU_PATH}/${ICU_VERSION}/include/ \
          -DICU_I18N_LIB=${ICU_PATH}/${ICU_VERSION}/lib/libicui18n.dylib \
          -DUSE_STD_MAP=ON \
    ..
}

install_libphonenumber()
{
	fail_check git clone ${LIB_PHONE_NUMBER_REPO} ${DESTINATION}
	pushd ${DESTINATION}
	fail_check git checkout ${LIB_PHONE_NUMBER_REV}
	popd

	mkdir -p ${DESTINATION}/cpp/build
	pushd ${DESTINATION}/cpp/build

	case $OS in
        Linux)
            qmake_unix
        ;;

        Darwin)
            qmake_darwin
        ;;

        *)
        echo "Your system $OS is not supported"
        exit 1
    esac

	fail_check make -j 8
	fail_check make install
	popd
}

copy_resources()
{
    fail_check cp -R $DEPS_LOCATION/libphonenumber/resources/carrier priv/carrier
}

mkdir -p $DEPS_LOCATION
pushd $DEPS_LOCATION

case $OS in
  Linux)
     case $KERNEL in

        CentOS)
            echo "Linux, CentOS not supported yet"
            exit 1
            ;;

        Ubuntu)
            echo "Linux, Ubuntu"
			wget http://mt.archive.ubuntu.com/ubuntu/pool/universe/r/re2/libre2-1_20140304+dfsg-2_amd64.deb -O libre2-1.deb
			wget http://es.archive.ubuntu.com/ubuntu/pool/universe/r/re2/libre2-dev_20140304+dfsg-2_amd64.deb -O libre2-dev.deb
			sudo dpkg -i libre2*.deb
			sudo apt-get install cmake cmake-curses-gui libprotobuf-dev libgtest-dev libre2-dev libicu-dev libboost-dev libboost-thread-dev libboost-system-dev protobuf-compiler
			install_libphonenumber
            ;;

        *)
            echo "Your system $KERNEL is not supported"
     esac
        ;;
  Darwin)
        brew install boost cmake icu4c pkg-config protobuf wget
        fail_check git clone https://github.com/google/googletest.git
        install_libphonenumber
        pushd ${DESTINATION}/cpp/build/install/lib
        rm -rf *.dylib
        popd
        ;;
  *)
        echo "Your system $OS is not supported"
        exit 1
esac

popd

mkdir priv
copy_resources