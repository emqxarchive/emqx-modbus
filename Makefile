PROJECT = emq_modbus
PROJECT_DESCRIPTION = Modbus-TCP Gateway
PROJECT_VERSION = 0.2

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd master

TEST_DEPS = cuttlefish lager
dep_cuttlefish = git https://github.com/emqtt/cuttlefish
dep_lager      = git https://github.com/basho/lager master

ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

app.config::
	cuttlefish -l info -e etc/ -c etc/emq_modbus.conf -i priv/emq_modbus.schema -d data
