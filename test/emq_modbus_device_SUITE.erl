%%--------------------------------------------------------------------
%% Copyright (c) 2012-2016 Feng Lee <feng@emqtt.io>.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emq_modbus_device_SUITE).

-compile(export_all).

-import(emq_modbus_controle, [start_link/1, stop/0, send_response/2]).

-define(MODBUS_PORT, 56923).


all() ->
    [{group, client}].

groups() ->
    [{client, [],
      [case01, case02, case03, case04, case05, case06]}].

init_per_suite(Config) ->
    lager_common_test_backend:bounce(info),
    Config.

end_per_suite(Config) ->
    Config.

start_test_env(CaseNumber) ->
    test_modbus_server:start_link(?MODBUS_PORT),
    test_modbus_server:set_case(CaseNumber),
    ensure_server(),
    test_broker_api:start_link().

stop_test_env() ->
    test_modbus_server:stop(),
    test_broker_api:stop().

ensure_server() ->
    receive
        go -> []
    end .

case01(_Config) ->
    start_test_env(1),
    emq_modbus_control:start_link("company3", "Edge58", 1, false),
    emq_modbus_device:connect("localhost", ?MODBUS_PORT, "deviceA"),

    <<"/company3/modbus_request/Edge58/+">> = test_broker_api:get_subscrbied_topic(),
    test_broker_api:dispatch(emq_modbus_control, <<"/company3/modbus_request/Edge58/deviceA">>, <<1, 7, 8, 9, 10>>),

    timer:sleep(200),

    {<<"deviceA">>, 1, false, <<"/company3/modbus_response/Edge58/deviceA">>, <<1, 7, 20, 30, 40>>} = test_broker_api:get_published_msg(),

    emq_modbus_device:disconnect("deviceA"),
    emq_modbus_control:stop(),
    stop_test_env().


case02(_Config) ->
    test_broker_api:start_link(),

    Vals = [{company, "Abc"},{edgename, "E1"},
            {qos, 2}, {retain, false},
            {device, [{"192.168.0.1", 502, "dev1"}, {"192.168.0.2", 502, "dev2"}]} ],
    [application:set_env(emq_modbus, Par, Value) || {Par, Value} <- Vals],

    ok = application:start(emq_modbus),
    true = is_process_alive(whereis(emq_modbus_control)),
    true = is_process_alive(whereis(emq_modbus_sup)),
    true = is_process_alive(whereis(emq_modbus_device_dev1)),
    true = is_process_alive(whereis(emq_modbus_device_dev2)),
    application:stop(emq_modbus),

    <<"/Abc/modbus_request/E1/+">> = test_broker_api:get_subscrbied_topic(),

    test_broker_api:stop().


case03(_Config) ->
    start_test_env(3),
    emq_modbus_control:start_link("company3", "Edge58", 1, false),
    emq_modbus_device:connect("localhost", ?MODBUS_PORT, "deviceNOTEXIST"),

    <<"/company3/modbus_request/Edge58/+">> = test_broker_api:get_subscrbied_topic(),
    test_broker_api:dispatch(emq_modbus_control, <<"/company3/modbus_request/Edge58/deviceA">>, <<1, 7, 8, 9, 10>>),

    timer:sleep(1000),

    true = is_process_alive(whereis(emq_modbus_device_deviceNOTEXIST)),
    undefined = test_broker_api:get_published_msg(),

    emq_modbus_device:disconnect("deviceNOTEXIST"),
    emq_modbus_control:stop(),
    stop_test_env().


case04(_Config) ->
    start_test_env(4),
    emq_modbus_control:start_link("company3", "Edge58", 1, false),
    emq_modbus_device:connect("localhost", ?MODBUS_PORT, "deviceA"),

    <<"/company3/modbus_request/Edge58/+">> = test_broker_api:get_subscrbied_topic(),
    test_broker_api:dispatch(emq_modbus_control, <<"/company3/modbus_request/Edge58/deviceA">>, <<1, 7, 8, 9, 10>>),

    timer:sleep(200),

    true = is_process_alive(whereis(emq_modbus_device_deviceA)),
    undefined = test_broker_api:get_published_msg(),

    emq_modbus_device:disconnect("deviceA"),
    emq_modbus_control:stop(),
    stop_test_env().


case05(_Config) ->
    start_test_env(5),
    emq_modbus_control:start_link("company3", "Edge58", 1, false),
    emq_modbus_device:connect("localhost", ?MODBUS_PORT, "deviceA"),

    <<"/company3/modbus_request/Edge58/+">> = test_broker_api:get_subscrbied_topic(),
    test_broker_api:dispatch(emq_modbus_control, <<"/company3/modbus_request/Edge58/deviceA">>, <<1, 7, 8, 9, 10>>),

    timer:sleep(12000),

    true = is_process_alive(whereis(emq_modbus_device_deviceA)),
    undefined = test_broker_api:get_published_msg(),

    emq_modbus_device:disconnect("deviceA"),
    emq_modbus_control:stop(),
    stop_test_env().


case06(_Config) ->
    start_test_env(6),
    emq_modbus_control:start_link("company3", "Edge58", 1, false),
    emq_modbus_device:connect("localhost", ?MODBUS_PORT, "deviceA"),

    <<"/company3/modbus_request/Edge58/+">> = test_broker_api:get_subscrbied_topic(),
    test_broker_api:dispatch(emq_modbus_control, <<"/company3/modbus_request/Edge58/deviceA">>, <<1, 7, 8, 9, 10>>),

    timer:sleep(200),

    true = is_process_alive(whereis(emq_modbus_device_deviceA)),
    undefined = test_broker_api:get_published_msg(),

    emq_modbus_device:disconnect("deviceA"),
    emq_modbus_control:stop(),
    stop_test_env().


