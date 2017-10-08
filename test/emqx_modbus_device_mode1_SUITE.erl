%%%-------------------------------------------------------------------
%%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-------------------------------------------------------------------

-module(emqx_modbus_device_mode1_SUITE).

-compile(export_all).

-import(emqx_modbus_controle, [start_link/1, stop/0, send_response/2]).

-define(MODBUS_PORT, 56923).
-include_lib("eunit/include/eunit.hrl").

-define(UNIT, 0).
-define(RESERVE, 0).
-define(LOGT(Format, Args),
    lager:debug("TEST_SUITE: " ++ Format, Args)).

-define(WHEREIS_DEVICE(X), emqx_modbus_registry:whereis_name(X)).

all() -> [mode1_case01, mode1_case02, mode1_case03, mode1_case04, mode1_case05, mode1_case06, mode1_case07, mode1_case08].


init_per_suite(Config) ->
    lager_common_test_backend:bounce(debug),
    Config.

end_per_suite(Config) ->
    Config.

start_test_env(TestFunc) ->
    start_test_env(TestFunc, undefined).
start_test_env(TestFunc, TestFunc2) ->
    test_modbus_server:start_link({?MODBUS_PORT, TestFunc, TestFunc2}),
    test_mqtt_broker:start_link().

stop_test_env() ->
    test_modbus_server:stop(),
    test_mqtt_broker:stop().


mode1_case01(_Config) ->
    Vals = [{mode, 1}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    DeviceName = <<"deviceA">>,
    Tid = 0, UnitId = 2, FunCode = 1,

    ServerHandler = fun (Server) ->
                        ?LOGT("modbus server is reading data ...", []),
                        {ok, InData} = gen_tcp:recv(Server, 0),
                        ?LOGT("modbus server get data ~p", [InData]),
                        C = <<7, 8, 9, 10>>, Size = byte_size(C)+2,
                        ?assertEqual(<<Tid:16, 0:16, Size:16, UnitId:8, FunCode:8, C/binary>>, InData),
                        Body = <<7, 20, 30, 40>>, Len = byte_size(Body)+2,
                        OutData = <<Tid:16, ?RESERVE:16, Len:16, UnitId:8, FunCode:8, Body/binary>>,
                        ?LOGT("modbus server send data ~p", [OutData]),
                        ok = gen_tcp:send(Server, OutData),
                        timer:sleep(300)
                    end,
    start_test_env(ServerHandler),
    emqx_modbus_registry:start_link(),
    emqx_modbus_control:start_link("company3", 1),
    emqx_modbus_device_mode1:connect("localhost", ?MODBUS_PORT, "deviceA"),

    <<"company3/modbus_request">> = test_mqtt_broker:get_subscrbied_topic(),
    Data35 = <<7, 8, 9, 10>>,
    Json =  jsx:encode([
                            {<<"device">>, DeviceName},
                            {<<"uid">>, UnitId},
                            {<<"msgid">>, 37},
                            {<<"funcode">>, FunCode},
                            {<<"data">>, base64:encode(Data35)}
                        ]),
    test_mqtt_broker:dispatch(emqx_modbus_control, <<"company3/modbus_request">>, Json),

    timer:sleep(200),

    JsonOutput =  jsx:encode([
                                {<<"device">>, <<"deviceA">>},
                                {<<"uid">>, 2},
                                {<<"msgid">>, 37},
                                {<<"funcode">>, 1},
                                {<<"data">>, base64:encode(<<7, 20, 30, 40>>)}
                            ]),
    ?assertEqual({<<"company3/modbus_response">>, JsonOutput}, test_mqtt_broker:get_published_msg()),
    1 = test_modbus_server:visitors(),

    emqx_modbus_device_mode1:disconnect("deviceA"),
    emqx_modbus_control:stop(),
    emqx_modbus_registry:stop(),
    stop_test_env().


mode1_case02(_Config) ->
    test_mqtt_broker:start_link(),

    Vals = [{mode, 1}, {company, "Abc"},
            {device, [{"192.168.0.1", 502, "dev1"}, {"192.168.0.2", 502, "dev2"}]} ],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    {ok, _} = application:ensure_all_started(emqx_modbus),
    true = is_process_alive(whereis(emqx_modbus_control)),
    true = is_process_alive(whereis(emqx_modbus_registry)),
    true = is_process_alive(whereis(emqx_modbus_sup)),
    true = is_process_alive(?WHEREIS_DEVICE(<<"dev1">>)),
    true = is_process_alive(?WHEREIS_DEVICE(<<"dev2">>)),
    ok = application:stop(emqx_modbus),
    ok = application:stop(esockd),

    <<"Abc/modbus_request">> = test_mqtt_broker:get_subscrbied_topic(),

    test_mqtt_broker:stop(),
    timer:sleep(1000).


mode1_case03(_Config) ->
    Vals = [{mode, 1}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    ServerHandler = fun (_Server) -> ok  end,
    start_test_env(ServerHandler),
    emqx_modbus_registry:start_link(),
    emqx_modbus_control:start_link("company3", 1),
    emqx_modbus_device_mode1:connect("localhost", ?MODBUS_PORT, "deviceNOTEXIST"),

    timer:sleep(500),
    <<"company3/modbus_request">> = test_mqtt_broker:get_subscrbied_topic(),

    Json =  jsx:encode([
        {<<"device">>, <<"deviceA">>},
        {<<"uid">>, 255},
        {<<"msgid">>, 37},
        {<<"funcode">>, 1},
        {<<"data">>, base64:encode(<<7, 8, 9, 10>>)}
    ]),
    test_mqtt_broker:dispatch(emqx_modbus_control, <<"company3/modbus_request">>, Json),

    timer:sleep(1000),

    true = is_process_alive(?WHEREIS_DEVICE(<<"deviceNOTEXIST">>)),
    undefined = test_mqtt_broker:get_published_msg(),

    emqx_modbus_device_mode1:disconnect("deviceNOTEXIST"),
    emqx_modbus_control:stop(),
    emqx_modbus_registry:stop(),
    stop_test_env().


mode1_case04(_Config) ->
    Vals = [{mode, 1}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    ServerHandler = fun (Server) ->
                        {ok, InData} = gen_tcp:recv(Server, 0),
                        C = <<1, 7, 8, 9, 10>>, Size = byte_size(C)+1,
                        <<Tid:16, 0:16, Size:16, Unit:8, C/binary>> = InData,
                        Body = <<32>>, Len = byte_size(Body)+1,
                        OutData = <<Tid:16, 42:16, Len:16, Unit:8, Body/binary>>,  % Body error
                        ok = gen_tcp:send(Server, OutData)
                    end,
    start_test_env(ServerHandler),
    emqx_modbus_registry:start_link(),
    emqx_modbus_control:start_link("company3", 1),
    emqx_modbus_device_mode1:connect("localhost", ?MODBUS_PORT, "deviceA"),
    timer:sleep(500),

    <<"company3/modbus_request">> = test_mqtt_broker:get_subscrbied_topic(),

    Json =  jsx:encode([
        {<<"device">>, <<"deviceA">>},
        {<<"uid">>, 255},
        {<<"msgid">>, 37},
        {<<"funcode">>, 1},
        {<<"data">>, base64:encode(<<7, 8, 9, 10>>)}
    ]),
    test_mqtt_broker:dispatch(emqx_modbus_control, <<"company3/modbus_request">>, Json),

    timer:sleep(200),

    true = is_process_alive(?WHEREIS_DEVICE(<<"deviceA">>)),
    undefined = test_mqtt_broker:get_published_msg(),

    emqx_modbus_device_mode1:disconnect("deviceA"),
    emqx_modbus_control:stop(),
    emqx_modbus_registry:stop(),
    stop_test_env().


mode1_case05(_Config) ->
    Vals = [{mode, 1}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    ServerHandler = fun (Server) ->
                        {ok, InData} = gen_tcp:recv(Server, 0),
                        C = <<1, 7, 8, 9, 10>>, Size = byte_size(C)+1,
                        <<Tid:16, 0:16, Size:16, Unit:8, C/binary>> = InData,
                        Body = <<>>, Len = byte_size(Body)+1,
                        OutData = <<Tid:16, 42:16, Len:16, Unit:8, Body/binary>>,  % reserved flag error
                        ok = gen_tcp:send(Server, OutData)   % server send a malformed response
                    end,
    start_test_env(ServerHandler),
    emqx_modbus_registry:start_link(),
    emqx_modbus_control:start_link("company3", 1),
    emqx_modbus_device_mode1:connect("localhost", ?MODBUS_PORT, "deviceA"),
    timer:sleep(500),

    <<"company3/modbus_request">> = test_mqtt_broker:get_subscrbied_topic(),
    Json =  jsx:encode([
        {<<"device">>, <<"deviceA">>},
        {<<"uid">>, 255},
        {<<"msgid">>, 37},
        {<<"funcode">>, 1},
        {<<"data">>, base64:encode(<<7, 8, 9, 10>>)}
    ]),
    test_mqtt_broker:dispatch(emqx_modbus_control, <<"company3/modbus_request">>, Json),

    timer:sleep(12000),

    true = is_process_alive(?WHEREIS_DEVICE(<<"deviceA">>)),
    undefined = test_mqtt_broker:get_published_msg(),

    emqx_modbus_device_mode1:disconnect("deviceA"),
    emqx_modbus_control:stop(),
    emqx_modbus_registry:stop(),
    stop_test_env().


mode1_case06(_Config) ->
    Vals = [{mode, 1}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    ServerHandler = fun (Server) ->
                        {ok, InData} = gen_tcp:recv(Server, 0),
                        C = <<1, 7, 8, 9, 10>>, Size = byte_size(C)+1,
                        <<Tid:16, 0:16, Size:16, Unit:8, C/binary>> = InData,
                        Body = <<>>, Len = byte_size(Body)+1,
                        OutData = <<Tid:16, ?RESERVE:16, Len:16, Unit:8, Body/binary>>,  % empty body
                        ok = gen_tcp:send(Server, OutData)
                    end,
    start_test_env(ServerHandler),
    emqx_modbus_registry:start_link(),
    emqx_modbus_control:start_link("company3", 1),
    emqx_modbus_device_mode1:connect("localhost", ?MODBUS_PORT, "deviceA"),
    timer:sleep(500),

    <<"company3/modbus_request">> = test_mqtt_broker:get_subscrbied_topic(),
    Json =  jsx:encode([
        {<<"device">>, <<"deviceA">>},
        {<<"uid">>, 255},
        {<<"msgid">>, 37},
        {<<"funcode">>, 1},
        {<<"data">>, base64:encode(<<7, 8, 9, 10>>)}
    ]),
    test_mqtt_broker:dispatch(emqx_modbus_control, <<"company3/modbus_request">>, Json),

    timer:sleep(200),

    true = is_process_alive(?WHEREIS_DEVICE(<<"deviceA">>)),
    undefined = test_mqtt_broker:get_published_msg(),

    emqx_modbus_device_mode1:disconnect("deviceA"),
    emqx_modbus_control:stop(),
    emqx_modbus_registry:stop(),
    stop_test_env().


mode1_case07(_Config) ->
    Vals = [{mode, 1}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    ServerHandler = fun (Server) ->
                        {ok, InData} = gen_tcp:recv(Server, 0),
                        gen_tcp:close(Server),    % simulate tcp error
                        C = <<1, 7, 8, 9, 10>>, Size = byte_size(C)+1,
                        <<_Tid:16, 0:16, Size:16, _Unit:8, C/binary>> = InData
                    end,
    start_test_env(ServerHandler),
    emqx_modbus_registry:start_link(),
    emqx_modbus_control:start_link("company3", 1),
    emqx_modbus_device_mode1:connect("localhost", ?MODBUS_PORT, "deviceA"),
    timer:sleep(500),

    <<"company3/modbus_request">> = test_mqtt_broker:get_subscrbied_topic(),
    Json =  jsx:encode([
        {<<"device">>, <<"deviceA">>},
        {<<"uid">>, 255},
        {<<"msgid">>, 37},
        {<<"funcode">>, 1},
        {<<"data">>, base64:encode(<<7, 8, 9, 10>>)}
    ]),
    test_mqtt_broker:dispatch(emqx_modbus_control, <<"company3/modbus_request">>, Json),

    timer:sleep(200),

    true = is_process_alive(?WHEREIS_DEVICE(<<"deviceA">>)),
    undefined = test_mqtt_broker:get_published_msg(),
    2 = test_modbus_server:visitors(),

    emqx_modbus_device_mode1:disconnect("deviceA"),
    emqx_modbus_control:stop(),
    emqx_modbus_registry:stop(),
    stop_test_env().


mode1_case08(_Config) ->
    Vals = [{mode, 1}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    ServerHandler = fun (Server) ->
                        gen_tcp:close(Server)    % simulate tcp error
                    end,
    ServerHandler2 = fun (Server) ->  % normal handler
                        {ok, InData} = gen_tcp:recv(Server, 0),
                        C = <<1, 7, 8, 9, 10>>, Size = byte_size(C)+1,
                        <<Tid:16, 0:16, Size:16, Unit:8, C/binary>> = InData,
                        Body = <<1, 7, 20, 30, 40>>, Len = byte_size(Body)+1,
                        OutData = <<Tid:16, ?RESERVE:16, Len:16, Unit:8, Body/binary>>,
                        ok = gen_tcp:send(Server, OutData)
                    end,
    start_test_env(ServerHandler, ServerHandler2),
    emqx_modbus_registry:start_link(),
    emqx_modbus_control:start_link("company3", 1),
    emqx_modbus_device_mode1:connect("localhost", ?MODBUS_PORT, "deviceA"),

    timer:sleep(300),

    <<"company3/modbus_request">> = test_mqtt_broker:get_subscrbied_topic(),
    Json =  jsx:encode([
        {<<"device">>, <<"deviceA">>},
        {<<"uid">>, 255},
        {<<"msgid">>, 37},
        {<<"funcode">>, 1},
        {<<"data">>, base64:encode(<<7, 8, 9, 10>>)}
    ]),
    test_mqtt_broker:dispatch(emqx_modbus_control, <<"company3/modbus_request">>, Json),

    timer:sleep(200),

    true = is_process_alive(?WHEREIS_DEVICE(<<"deviceA">>)),
    JsonOutput =  jsx:encode([
        {<<"device">>, <<"deviceA">>},
        {<<"uid">>, 255},
        {<<"msgid">>, 37},
        {<<"funcode">>, 1},
        {<<"data">>, base64:encode(<<7, 20, 30, 40>>)}
    ]),
    ?assertEqual({<<"company3/modbus_response">>, JsonOutput}, test_mqtt_broker:get_published_msg()),
    2 = test_modbus_server:visitors(),

    emqx_modbus_device_mode1:disconnect("deviceA"),
    emqx_modbus_control:stop(),
    emqx_modbus_registry:stop(),
    stop_test_env().




