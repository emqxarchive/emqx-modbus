PROJECT = emqx_modbus
PROJECT_DESCRIPTION = EMQ X Modbus-TCP Gateway
PROJECT_VERSION = 0.5

DEPS = esockd jsx
dep_esockd = git https://github.com/emqtt/esockd           emqx30
dep_jsx    = git https://github.com/talentdeficit/jsx.git  master

BUILD_DEPS = emqx
dep_emqx = git https://github.com/emqtt/emqttd X

TEST_DEPS = cuttlefish lager
dep_cuttlefish = git https://github.com/emqtt/cuttlefish
dep_lager      = git https://github.com/basho/lager master

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

app.config::
	cuttlefish -l info -e etc/ -c etc/emqx_modbus.conf -i priv/emqx_modbus.schema -d data
