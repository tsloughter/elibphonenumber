
PROJECT_NIF_NAME=phonenumber_util_nif

include nif.mk

CXXFLAGS +=  -fPIC -g -Wextra -Werror -fno-exceptions -fno-rtti -Wno-missing-field-initializers -Wno-unused-parameter \
 		     -I $(BASEDIR)/_build/deps/libphonenumber/cpp/build/install/include  \
 		     -DI18N_PHONENUMBERS_NO_THREAD_SAFETY

LDFLAGS +=  -L$(BASEDIR)/_build/deps/libphonenumber/cpp/build/install/lib/ \
			-lstdc++ \
			-l:libphonenumber.a \
			-lboost_system \
			-lboost_thread \
			-lboost_date_time \
			-licui18n \
			-licuuc \
			-lprotobuf