PROJECT = erlorg

IGNORE_DEPS += emongo merl

DEPS = cowlib ranch cowboy lager mixer erlydtl sumo ecdate localtime qdate trane zipper

dep_cowlib    = git https://github.com/ninenines/cowlib.git          1.0.0
dep_ranch     = git https://github.com/ninenines/ranch.git           1.0.0
dep_cowboy    = git https://github.com/ninenines/cowboy.git          1.0.1
dep_lager     = git https://github.com/basho/lager.git               3.0.0-RC1
dep_mixer     = git https://github.com/opscode/mixer.git             0.1.1
dep_sync      = git https://github.com/rustyio/sync.git              9c78e7b
dep_erlydtl   = git https://github.com/erlydtl/erlydtl               0.11.0
dep_sumo      = git https://github.com/inaka/sumo_db                 0.3.13
dep_ecdate    = git https://github.com/erlware/erlware_commons.git   v0.12.0
dep_localtime = git https://github.com/dmitryme/erlang_localtime.git 458cedf2a
dep_qdate     = git https://github.com/choptastic/qdate.git          0.4.2
dep_trane     = git https://github.com/massemanet/trane.git          1.0.0
dep_zipper    = git https://github.com/inaka/zipper.git              0.1.3

TEST_DEPS = shotgun xref_runner meck

SHELL_DEPS = sync

dep_shotgun     = git https://github.com/inaka/shotgun         0.1.10
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.3
dep_meck        = git https://github.com/eproxus/meck.git      0.8.3

all::
	@if [ ! -f rel/ops.config ]; then echo "[]." > rel/ops.config; fi

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

CT_OPTS = -cover test/erlorg.spec -erl_args -config rel/test.config -config rel/ops_test.config

SHELL_OPTS = -name ${PROJECT}@`hostname` -s sync -s ${PROJECT} -config rel/sys.config

quicktests: ERLC_OPTS = $(TEST_ERLC_OPTS)
quicktests: clean app build-ct-suites
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS) ; \
	fi
	$(gen_verbose) rm -f test/*.beam

test-shell: app build-ct-suites
	erl -pa ebin/ -pa deps/**/ebin -pa test/ -name ${PROJECT}@`hostname` -s sync -s ${PROJECT} -config rel/sys.config
