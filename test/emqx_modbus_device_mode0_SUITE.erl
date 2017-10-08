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

-module(emqx_modbus_device_mode0_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(MODBUS_PORT, 5502).
-define(UNIT, 0).
-define(RESERVE, 0).
-define(LOGT(Format, Args),
    lager:debug("TEST_SUITE: " ++ Format, Args)).

-define(WHEREIS_DEVICE(X), emqx_modbus_registry:whereis_name(X)).

all() -> [mode0_case01, mode0_case02, mode0_case03, mode0_case04, mode0_case05, mode0_case06, mode0_case07,
    mode0_case08_keepalive, mode0_case09_kickout_old_device].


init_per_suite(Config) ->
    lager_common_test_backend:bounce(debug),
    Config.

end_per_suite(Config) ->
    Config.

start_test_env() ->
    test_mqtt_broker:start_link().

stop_test_env() ->
    test_mqtt_broker:stop().


mode0_case01(_Config) ->
    Vals = [{mode, 0}, {port, ?MODBUS_PORT}, {company, "company3"}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    Tid = 0, UnitId = 2, FunCode = 1,

    start_test_env(),
    {ok, _Started} = application:ensure_all_started(emqx_modbus),
    {ok,Sock} = gen_tcp:connect("localhost", ?MODBUS_PORT, [binary, {active,false}, {send_timeout, 5000}]),

    % register device identity
    DeviceName = <<"010203040506">>,
    ok = gen_tcp:send(Sock, <<16#C3, 16#25, 16#9D, 16#A9, DeviceName:12/binary>>),
    timer:sleep(300),

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

    {ok, Packet} = gen_tcp:recv(Sock, 0),
    Size = byte_size(Data35)+2,
    ?assertEqual(<<Tid:16, 0:16, Size:16, UnitId:8, FunCode:8, Data35/binary>>, Packet),

    Data59 = <<7, 20, 30, 40>>,
    Size2 = byte_size(Data59)+2,
    RspPacket = <<Tid:16, 0:16, Size2:16, UnitId:8, FunCode:8, Data59/binary>>,
    ok = gen_tcp:send(Sock, RspPacket),

    timer:sleep(300),

    JsonOutput =  jsx:encode([
                                {<<"device">>, DeviceName},
                                {<<"uid">>, 2},
                                {<<"msgid">>, 37},
                                {<<"funcode">>, 1},
                                {<<"data">>, base64:encode(Data59)}
                            ]),
    ?LOGT("JsonOutput=~p", [JsonOutput]),
    ?assertEqual({<<"company3/modbus_response">>, JsonOutput}, test_mqtt_broker:get_published_msg()),

    ok = application:stop(emqx_modbus),
    ok = application:stop(esockd),
    stop_test_env().




mode0_case02(_Config) ->
    Vals = [{mode, 0}, {port, ?MODBUS_PORT}, {company, "company3"}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    Tid = 0, UnitId = 2, FunCode = 1,

    start_test_env(),
    {ok, _Started} = application:ensure_all_started(emqx_modbus),
    {ok,Sock} = gen_tcp:connect("localhost", ?MODBUS_PORT, [binary, {active,false}, {send_timeout, 1000}]),

    % register device identity
    DeviceName = <<"010203040506">>,
    ok = gen_tcp:send(Sock, <<16#C3, 16#25, 16#9D, 16#A9, DeviceName:12/binary>>),
    timer:sleep(300),

    <<"company3/modbus_request">> = test_mqtt_broker:get_subscrbied_topic(),
    Data35 = <<7, 8, 9, 10>>,
    Json =  jsx:encode([
        {<<"device">>, <<"deviceNotExist">>},
        {<<"uid">>, UnitId},
        {<<"msgid">>, 37},
        {<<"funcode">>, FunCode},
        {<<"data">>, base64:encode(Data35)}
    ]),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% try to send modbus request to a not-exist device, and no response is expected
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    test_mqtt_broker:dispatch(emqx_modbus_control, <<"company3/modbus_request">>, Json),

    timer:sleep(200),

    {error, _} = gen_tcp:recv(Sock, 0, 500),

    ?assertEqual(undefined, test_mqtt_broker:get_published_msg()),

    ok = application:stop(emqx_modbus),
    ok = application:stop(esockd),
    stop_test_env().



mode0_case03(_Config) ->
    Vals = [{mode, 0}, {port, ?MODBUS_PORT}, {company, "company3"}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    Tid = 0, UnitId = 2, FunCode = 1,

    start_test_env(),
    {ok, _Started} = application:ensure_all_started(emqx_modbus),
    {ok,Sock} = gen_tcp:connect("localhost", ?MODBUS_PORT, [binary, {active,false}, {send_timeout, 5000}]),

    % register device identity
    DeviceName = <<"010203040506">>,
    ok = gen_tcp:send(Sock, <<16#C3, 16#25, 16#9D, 16#A9, DeviceName:12/binary>>),

    timer:sleep(300),

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

    {ok, Packet} = gen_tcp:recv(Sock, 0, 500),
    Size = byte_size(Data35)+2,
    ?assertEqual(<<Tid:16, 0:16, Size:16, UnitId:8, FunCode:8, Data35/binary>>, Packet),

    Data59 = <<7, 20, 30, 40>>,
    Size2 = byte_size(Data59)+2,  ErrorVer = 23,
    RspPacket = <<Tid:16, ErrorVer:16, Size2:16, UnitId:8, FunCode:8, Data59/binary>>,
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% send an error packet to cloud, and expect device process to discard this packet
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ok = gen_tcp:send(Sock, RspPacket),

    timer:sleep(300),

    ?assertEqual(undefined, test_mqtt_broker:get_published_msg()),
    true = is_process_alive(?WHEREIS_DEVICE(DeviceName)),

    ok = application:stop(emqx_modbus),
    ok = application:stop(esockd),
    stop_test_env().



mode0_case04(_Config) ->
    Vals = [{mode, 0}, {port, ?MODBUS_PORT}, {company, "company3"}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    Tid = 0, UnitId = 2, FunCode = 1,

    start_test_env(),
    {ok, _Started} = application:ensure_all_started(emqx_modbus),
    {ok,Sock} = gen_tcp:connect("localhost", ?MODBUS_PORT, [binary, {active,false}, {send_timeout, 5000}]),

    % register device identity
    DeviceName = <<"010203040506">>,
    ok = gen_tcp:send(Sock, <<16#C3, 16#25, 16#9D, 16#A9, DeviceName:12/binary>>),

    timer:sleep(300),

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

    {ok, Packet} = gen_tcp:recv(Sock, 0, 500),
    Size = byte_size(Data35)+2,
    ?assertEqual(<<Tid:16, 0:16, Size:16, UnitId:8, FunCode:8, Data35/binary>>, Packet),

    Data59 = <<>>,  % empty body, this is an error
    Size2 = byte_size(Data59)+2,  ErrorVer = 23,
    RspPacket = <<Tid:16, ErrorVer:16, Size2:16, UnitId:8, FunCode:8, Data59/binary>>,
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% send an error packet to cloud, and expect device process to discard this packet
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ok = gen_tcp:send(Sock, RspPacket),

    timer:sleep(300),

    ?assertEqual(undefined, test_mqtt_broker:get_published_msg()),
    true = is_process_alive(?WHEREIS_DEVICE(DeviceName)),

    ok = application:stop(emqx_modbus),
    ok = application:stop(esockd),
    stop_test_env().



mode0_case05(_Config) ->
    Vals = [{mode, 0}, {port, ?MODBUS_PORT}, {company, "company3"}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    Tid = 0, UnitId = 2, FunCode = 1,

    start_test_env(),
    {ok, _Started} = application:ensure_all_started(emqx_modbus),
    {ok,Sock} = gen_tcp:connect("localhost", ?MODBUS_PORT, [binary, {active,false}, {send_timeout, 5000}]),

    % register device identity
    DeviceName = <<"010203040506">>,
    ok = gen_tcp:send(Sock, <<16#C3, 16#25, 16#9D, 16#A9, DeviceName:12/binary>>),

    timer:sleep(300),

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

    {ok, Packet} = gen_tcp:recv(Sock, 0, 500),
    Size = byte_size(Data35)+2,
    ?assertEqual(<<Tid:16, 0:16, Size:16, UnitId:8, FunCode:8, Data35/binary>>, Packet),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% close socket, simulating a connection error, and expect device process to shutdown
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ok = gen_tcp:close(Sock),

    timer:sleep(300),

    ?assertEqual(undefined, test_mqtt_broker:get_published_msg()),
    undefined = ?WHEREIS_DEVICE(DeviceName),

    ok = application:stop(emqx_modbus),
    ok = application:stop(esockd),
    stop_test_env().



mode0_case06(_Config) ->
    Vals = [{mode, 0}, {port, ?MODBUS_PORT}, {company, "company3"}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    Tid = 0, UnitId = 2, FunCode = 1,

    start_test_env(),
    {ok, _Started} = application:ensure_all_started(emqx_modbus),
    {ok,Sock} = gen_tcp:connect("localhost", ?MODBUS_PORT, [binary, {active,false}, {send_timeout, 5000}]),

    % register device identity
    DeviceName = <<"010203040506">>,
    ok = gen_tcp:send(Sock, <<16#C3, 16#25, 16#9D, 16#A9, DeviceName:12/binary>>),

    timer:sleep(300),

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

    {ok, Packet} = gen_tcp:recv(Sock, 0, 500),
    Size = byte_size(Data35)+2,
    ?assertEqual(<<Tid:16, 0:16, Size:16, UnitId:8, FunCode:8, Data35/binary>>, Packet),

    ok = gen_tcp:close(Sock),  % close socket, and expect device process to shutdown

    timer:sleep(300),

    % device process should shutdown itself
    undefined = ?WHEREIS_DEVICE(DeviceName),
    ?assertEqual(undefined, test_mqtt_broker:get_published_msg()),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% client connect again
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    {ok,Sock2} = gen_tcp:connect("localhost", ?MODBUS_PORT, [binary, {active,false}, {send_timeout, 5000}]),
    % register device identity
    ok = gen_tcp:send(Sock2, <<16#C3, 16#25, 16#9D, 16#A9, DeviceName:12/binary>>),
    timer:sleep(300),
    test_mqtt_broker:dispatch(emqx_modbus_control, <<"company3/modbus_request">>, Json),
    timer:sleep(100),
    {ok, Packet2} = gen_tcp:recv(Sock2, 0, 500),
    ?assertEqual(<<Tid:16, 0:16, Size:16, UnitId:8, FunCode:8, Data35/binary>>, Packet),
    Data59 = <<7, 20, 30, 40>>,
    Size2 = byte_size(Data59)+2,
    RspPacket = <<Tid:16, 0:16, Size2:16, UnitId:8, FunCode:8, Data59/binary>>,
    ok = gen_tcp:send(Sock2, RspPacket),

    timer:sleep(300),

    JsonOutput =  jsx:encode([
        {<<"device">>, DeviceName},
        {<<"uid">>, 2},
        {<<"msgid">>, 37},
        {<<"funcode">>, 1},
        {<<"data">>, base64:encode(Data59)}
    ]),
    ?assertEqual({<<"company3/modbus_response">>, JsonOutput}, test_mqtt_broker:get_published_msg()),

    ok = application:stop(emqx_modbus),
    ok = application:stop(esockd),
    stop_test_env().




mode0_case07(_Config) ->
    Vals = [{mode, 0}, {port, ?MODBUS_PORT}, {company, "company3"}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],

    Tid = 0, UnitId = 2, FunCode = 1,

    start_test_env(),
    {ok, _Started} = application:ensure_all_started(emqx_modbus),
    {ok,Sock} = gen_tcp:connect("localhost", ?MODBUS_PORT, [binary, {active,false}, {send_timeout, 5000}]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% no registration here
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    DeviceName = <<"010203040506">>,

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

    %% device is not registered, it can not receive modbus request from cloud
    {error, timeout} = gen_tcp:recv(Sock, 0, 500),
    ?assertEqual(undefined, test_mqtt_broker:get_published_msg()),

    ok = application:stop(emqx_modbus),
    ok = application:stop(esockd),
    stop_test_env().




mode0_case08_keepalive(_Config) ->
    Vals = [{mode, 0}, {port, ?MODBUS_PORT}, {company, "company3"}, {keepalive, 1}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],


    start_test_env(),
    {ok, _Started} = application:ensure_all_started(emqx_modbus),
    {ok,Sock} = gen_tcp:connect("localhost", ?MODBUS_PORT, [binary, {active,false}, {send_timeout, 5000}]),

    % register device identity
    DeviceName = <<"010203040506">>,
    ok = gen_tcp:send(Sock, <<16#C3, 16#25, 16#9D, 16#A9, DeviceName:12/binary>>),
    timer:sleep(700),
    true = is_process_alive(?WHEREIS_DEVICE(DeviceName)),

    ok = gen_tcp:send(Sock, <<16#C3, 16#25, 16#9D, 16#AA>>),  % keepalive packet
    timer:sleep(700),
    true = is_process_alive(?WHEREIS_DEVICE(DeviceName)),

    ok = gen_tcp:send(Sock, <<16#C3, 16#25, 16#9D, 16#AA>>),  % keepalive packet
    timer:sleep(700),
    true = is_process_alive(?WHEREIS_DEVICE(DeviceName)),

    ok = gen_tcp:send(Sock, <<16#C3, 16#25, 16#9D, 16#AA>>),  % keepalive packet
    timer:sleep(700),
    true = is_process_alive(?WHEREIS_DEVICE(DeviceName)),

    %% keepalive timeout, and device process should terminate
    timer:sleep(2000),

    ?assertEqual(undefined, ?WHEREIS_DEVICE(DeviceName)),


    ok = application:stop(emqx_modbus),
    ok = application:stop(esockd),
    stop_test_env().



mode0_case09_kickout_old_device(_Config) ->
    Vals = [{mode, 0}, {port, ?MODBUS_PORT}, {company, "company3"}, {keepalive, 1000}],
    [application:set_env(emqx_modbus, Par, Value) || {Par, Value} <- Vals],


    start_test_env(),
    {ok, _Started} = application:ensure_all_started(emqx_modbus),
    {ok,Sock} = gen_tcp:connect("localhost", ?MODBUS_PORT, [binary, {active,false}, {send_timeout, 5000}]),

    % register device identity
    DeviceName = <<"010203040506">>,
    ok = gen_tcp:send(Sock, <<16#C3, 16#25, 16#9D, 16#A9, DeviceName:12/binary>>),
    timer:sleep(700),
    FirstPid = ?WHEREIS_DEVICE(DeviceName),
    true = is_process_alive(FirstPid),



    % same device connect again, it should kick out the old one
    {ok,Sock2} = gen_tcp:connect("localhost", ?MODBUS_PORT, [binary, {active,false}, {send_timeout, 5000}]),
    DeviceName = <<"010203040506">>,
    ok = gen_tcp:send(Sock2, <<16#C3, 16#25, 16#9D, 16#A9, DeviceName:12/binary>>),
    timer:sleep(700),
    SecondPid = ?WHEREIS_DEVICE(DeviceName),
    ?assertEqual(false, is_process_alive(FirstPid)),
    ?assertNotEqual(FirstPid, SecondPid),
    ?assertEqual(true, is_process_alive(SecondPid)),


    UnitId = 16, FunCode = 39, Tid = 0, MsgId = 112,
    <<"company3/modbus_request">> = test_mqtt_broker:get_subscrbied_topic(),
    Data35 = <<7, 8, 9, 10>>,
    Json =  jsx:encode([
        {<<"device">>, DeviceName},
        {<<"uid">>, UnitId},
        {<<"msgid">>, MsgId},
        {<<"funcode">>, FunCode},
        {<<"data">>, base64:encode(Data35)}
    ]),
    test_mqtt_broker:dispatch(emqx_modbus_control, <<"company3/modbus_request">>, Json),

    timer:sleep(200),

    {ok, Packet} = gen_tcp:recv(Sock2, 0),
    Size = byte_size(Data35)+2,
    ?assertEqual(<<Tid:16, 0:16, Size:16, UnitId:8, FunCode:8, Data35/binary>>, Packet),

    Data59 = <<7, 20, 30, 40>>,
    Size2 = byte_size(Data59)+2,
    RspPacket = <<Tid:16, 0:16, Size2:16, UnitId:8, FunCode:8, Data59/binary>>,
    ok = gen_tcp:send(Sock2, RspPacket),

    timer:sleep(300),

    JsonOutput =  jsx:encode([
        {<<"device">>, DeviceName},
        {<<"uid">>, UnitId},
        {<<"msgid">>, MsgId},
        {<<"funcode">>, FunCode},
        {<<"data">>, base64:encode(Data59)}
    ]),
    ?LOGT("JsonOutput=~p", [JsonOutput]),
    ?assertEqual({<<"company3/modbus_response">>, JsonOutput}, test_mqtt_broker:get_published_msg()),

    ok = application:stop(emqx_modbus),
    ok = application:stop(esockd),
    stop_test_env().
