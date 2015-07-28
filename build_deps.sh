#!/usr/bin/env bash
OS=$(uname -s)
KERNEL=$(echo $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}') | awk '{print $1;}')

##echo $OS
##echo $KERNEL

install_libphonenumber()
{
	git clone https://github.com/googlei18n/libphonenumber.git
	git reset --hard 433dac969a5ede2eb7b60dfc7f8af53c8c4bf947

	mkdir -p libphonenumber/cpp/build
	pushd libphonenumber/cpp/build
	cmake ..
	make
	ssh-askpass Sudo Password | sudo -S make install
	popd
}

mkdir -p deps
pushd deps

case $OS in
  Linux)
     case $KERNEL in
       CentOS)
          echo "Linux, CentOS not supported yet"
          ;;
       Ubuntu)
         echo "Linux, Ubuntu"
			wget http://mt.archive.ubuntu.com/ubuntu/pool/universe/r/re2/libre2-1_20140304+dfsg-2_amd64.deb -O libre2-1.deb
			wget http://es.archive.ubuntu.com/ubuntu/pool/universe/r/re2/libre2-dev_20140304+dfsg-2_amd64.deb -O libre2-dev.deb
			sudo dpkg -i libre2*.deb
			sudo apt-get install cmake cmake-curses-gui libprotobuf-dev libgtest-dev libre2-dev libicu-dev libboost-dev libboost-thread-dev libboost-system-dev protobuf-compiler
			sudo grep -q -F '/usr/local/lib' /etc/ld.so.conf.d/usrlocal.conf || echo '/usr/local/lib' | sudo tee --append /etc/ld.so.conf.d/usrlocal.conf > /dev/null
	        ldconfig -v
			install_libphonenumber
         ;;
       *) echo "Your system $KERNEL is not supported"
     esac
     ;;
  Darwin)
     echo "Mac OS not supported yet"
     ;;
  *) echo "Your system $OS is not supported"
esac

popd
