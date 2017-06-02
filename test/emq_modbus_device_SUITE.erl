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
-include_lib("eunit/include/eunit.hrl").

-define(UNIT, 0).
-define(RESERVE, 0).
-define(LOGT(Format, Args),
    lager:debug("TEST_SUITE: " ++ Format, Args)).

-define(WHEREIS_DEVICE(X), emq_modbus_registry:whereis_name(X)).


all() ->
    [case01, case02, case03, case04, case05, case06, case07, case08, case09].

init_per_suite(Config) ->
    lager_common_test_backend:bounce(debug),
    Config.

end_per_suite(Config) ->
    Config.

start_test_env(TestFunc) ->
    start_test_env(TestFunc, undefined).
start_test_env(TestFunc, TestFunc2) ->
    test_modbus_server:start_link({?MODBUS_PORT, TestFunc, TestFunc2}),
    test_broker_api:start_link().

stop_test_env() ->
    test_modbus_server:stop(),
    test_broker_api:stop().


case01(_Config) ->
    ServerHandler = fun (Server) ->
                        {ok, InData} = gen_tcp:recv(Server, 0),
                        C = <<7, 8, 9, 10>>, Size = byte_size(C)+1,
                        <<Tid:16, 0:16, Size:16, Unit:8, C/binary>> = InData,
                        Body = <<7, 20, 30, 40>>, Len = byte_size(Body)+1,
                        OutData = <<Tid:16, ?RESERVE:16, Len:16, Unit:8, Body/binary>>,
                        ok = gen_tcp:send(Server, OutData)
                    end,
    start_test_env(ServerHandler),
    emq_modbus_registry:start_link(),
    emq_modbus_control:start_link("company3", "Edge58"),
    emq_modbus_device:connect("localhost", ?MODBUS_PORT, "deviceA"),

    <<"/company3/modbus_request/Edge58/+">> = test_broker_api:get_subscrbied_topic(),
    test_broker_api:dispatch(emq_modbus_control, <<"/company3/modbus_request/Edge58/deviceA">>, <<1, 7, 8, 9, 10>>),

    timer:sleep(200),

    {<<"/company3/modbus_response/Edge58/deviceA">>, <<1, 7, 20, 30, 40>>} = test_broker_api:get_published_msg(),
    1 = test_modbus_server:visitors(),

    emq_modbus_device:disconnect("deviceA"),
    emq_modbus_control:stop(),
    emq_modbus_registry:stop(),
    stop_test_env().


case02(_Config) ->
    test_broker_api:start_link(),

    Vals = [{company, "Abc"},{edgename, "E1"},
            {device, [{"192.168.0.1", 502, "dev1"}, {"192.168.0.2", 502, "dev2"}]} ],
    [application:set_env(emq_modbus, Par, Value) || {Par, Value} <- Vals],

    ok = application:start(emq_modbus),
    true = is_process_alive(whereis(emq_modbus_control)),
    true = is_process_alive(whereis(emq_modbus_registry)),
    true = is_process_alive(whereis(emq_modbus_sup)),
    true = is_process_alive(?WHEREIS_DEVICE(<<"dev1">>)),
    true = is_process_alive(?WHEREIS_DEVICE(<<"dev2">>)),
    application:stop(emq_modbus),

    <<"/Abc/modbus_request/E1/+">> = test_broker_api:get_subscrbied_topic(),

    test_broker_api:stop(),
    timer:sleep(1000).


case03(_Config) ->
    ServerHandler = fun (_Server) -> ok  end,
    start_test_env(ServerHandler),
    emq_modbus_registry:start_link(),
    emq_modbus_control:start_link("company3", "Edge58"),
    emq_modbus_device:connect("localhost", ?MODBUS_PORT, "deviceNOTEXIST"),

    timer:sleep(500),
    <<"/company3/modbus_request/Edge58/+">> = test_broker_api:get_subscrbied_topic(),
    test_broker_api:dispatch(emq_modbus_control, <<"/company3/modbus_request/Edge58/deviceA">>, <<1, 7, 8, 9, 10>>),

    timer:sleep(1000),

    true = is_process_alive(?WHEREIS_DEVICE(<<"deviceNOTEXIST">>)),
    undefined = test_broker_api:get_published_msg(),

    emq_modbus_device:disconnect("deviceNOTEXIST"),
    emq_modbus_control:stop(),
    emq_modbus_registry:stop(),
    stop_test_env().


case04(_Config) ->
    ServerHandler = fun (Server) ->
                        {ok, InData} = gen_tcp:recv(Server, 0),
                        C = <<7, 8, 9, 10>>, Size = byte_size(C)+1,
                        <<Tid:16, 0:16, Size:16, Unit:8, C/binary>> = InData,
                        Body = <<32>>, Len = byte_size(Body)+1,
                        OutData = <<Tid:16, 42:16, Len:16, Unit:8, Body/binary>>,  % Body error
                        ok = gen_tcp:send(Server, OutData)
                    end,
    start_test_env(ServerHandler),
    emq_modbus_registry:start_link(),
    emq_modbus_control:start_link("company3", "Edge58"),
    emq_modbus_device:connect("localhost", ?MODBUS_PORT, "deviceA"),
    timer:sleep(500),

    <<"/company3/modbus_request/Edge58/+">> = test_broker_api:get_subscrbied_topic(),
    test_broker_api:dispatch(emq_modbus_control, <<"/company3/modbus_request/Edge58/deviceA">>, <<1, 7, 8, 9, 10>>),

    timer:sleep(200),

    true = is_process_alive(?WHEREIS_DEVICE(<<"deviceA">>)),
    undefined = test_broker_api:get_published_msg(),

    emq_modbus_device:disconnect("deviceA"),
    emq_modbus_control:stop(),
    emq_modbus_registry:stop(),
    stop_test_env().


case05(_Config) ->
    ServerHandler = fun (Server) ->
                        {ok, InData} = gen_tcp:recv(Server, 0),
                        C = <<7, 8, 9, 10>>, Size = byte_size(C)+1,
                        <<Tid:16, 0:16, Size:16, Unit:8, C/binary>> = InData,
                        Body = <<>>, Len = byte_size(Body)+1,
                        OutData = <<Tid:16, 42:16, Len:16, Unit:8, Body/binary>>,  % reserved flag error
                        ok = gen_tcp:send(Server, OutData)   % server send a malformed response
                    end,
    start_test_env(ServerHandler),
    emq_modbus_registry:start_link(),
    emq_modbus_control:start_link("company3", "Edge58"),
    emq_modbus_device:connect("localhost", ?MODBUS_PORT, "deviceA"),
    timer:sleep(500),

    <<"/company3/modbus_request/Edge58/+">> = test_broker_api:get_subscrbied_topic(),
    test_broker_api:dispatch(emq_modbus_control, <<"/company3/modbus_request/Edge58/deviceA">>, <<1, 7, 8, 9, 10>>),

    timer:sleep(12000),

    true = is_process_alive(?WHEREIS_DEVICE(<<"deviceA">>)),
    undefined = test_broker_api:get_published_msg(),

    emq_modbus_device:disconnect("deviceA"),
    emq_modbus_control:stop(),
    emq_modbus_registry:stop(),
    stop_test_env().


case06(_Config) ->
    ServerHandler = fun (Server) ->
                        {ok, InData} = gen_tcp:recv(Server, 0),
                        C = <<7, 8, 9, 10>>, Size = byte_size(C)+1,
                        <<Tid:16, 0:16, Size:16, Unit:8, C/binary>> = InData,
                        Body = <<>>, Len = byte_size(Body)+1,
                        OutData = <<Tid:16, ?RESERVE:16, Len:16, Unit:8, Body/binary>>,  % empty body
                        ok = gen_tcp:send(Server, OutData)
                    end,
    start_test_env(ServerHandler),
    emq_modbus_registry:start_link(),
    emq_modbus_control:start_link("company3", "Edge58"),
    emq_modbus_device:connect("localhost", ?MODBUS_PORT, "deviceA"),
    timer:sleep(500),

    <<"/company3/modbus_request/Edge58/+">> = test_broker_api:get_subscrbied_topic(),
    test_broker_api:dispatch(emq_modbus_control, <<"/company3/modbus_request/Edge58/deviceA">>, <<1, 7, 8, 9, 10>>),

    timer:sleep(200),

    true = is_process_alive(?WHEREIS_DEVICE(<<"deviceA">>)),
    undefined = test_broker_api:get_published_msg(),

    emq_modbus_device:disconnect("deviceA"),
    emq_modbus_control:stop(),
    emq_modbus_registry:stop(),
    stop_test_env().


case07(_Config) ->
    ServerHandler = fun (Server) ->
                        {ok, InData} = gen_tcp:recv(Server, 0),
                        gen_tcp:close(Server),    % simulate tcp error
                        C = <<7, 8, 9, 10>>, Size = byte_size(C)+1,
                        <<_Tid:16, 0:16, Size:16, _Unit:8, C/binary>> = InData
                    end,
    start_test_env(ServerHandler),
    emq_modbus_registry:start_link(),
    emq_modbus_control:start_link("company3", "Edge58"),
    emq_modbus_device:connect("localhost", ?MODBUS_PORT, "deviceA"),
    timer:sleep(500),

    <<"/company3/modbus_request/Edge58/+">> = test_broker_api:get_subscrbied_topic(),
    test_broker_api:dispatch(emq_modbus_control, <<"/company3/modbus_request/Edge58/deviceA">>, <<1, 7, 8, 9, 10>>),

    timer:sleep(200),

    true = is_process_alive(?WHEREIS_DEVICE(<<"deviceA">>)),
    undefined = test_broker_api:get_published_msg(),
    2 = test_modbus_server:visitors(),

    emq_modbus_device:disconnect("deviceA"),
    emq_modbus_control:stop(),
    emq_modbus_registry:stop(),
    stop_test_env().


case08(_Config) ->
    ServerHandler = fun (Server) ->
                        gen_tcp:close(Server)    % simulate tcp error
                    end,
    ServerHandler2 = fun (Server) ->  % normal handler
                        {ok, InData} = gen_tcp:recv(Server, 0),
                        C = <<7, 8, 9, 10>>, Size = byte_size(C)+1,
                        <<Tid:16, 0:16, Size:16, Unit:8, C/binary>> = InData,
                        Body = <<7, 20, 30, 40>>, Len = byte_size(Body)+1,
                        OutData = <<Tid:16, ?RESERVE:16, Len:16, Unit:8, Body/binary>>,
                        ok = gen_tcp:send(Server, OutData)
                    end,
    start_test_env(ServerHandler, ServerHandler2),
    emq_modbus_registry:start_link(),
    emq_modbus_control:start_link("company3", "Edge58"),
    emq_modbus_device:connect("localhost", ?MODBUS_PORT, "deviceA"),

    timer:sleep(300),

    <<"/company3/modbus_request/Edge58/+">> = test_broker_api:get_subscrbied_topic(),
    test_broker_api:dispatch(emq_modbus_control, <<"/company3/modbus_request/Edge58/deviceA">>, <<1, 7, 8, 9, 10>>),

    timer:sleep(200),

    true = is_process_alive(?WHEREIS_DEVICE(<<"deviceA">>)),
    {<<"/company3/modbus_response/Edge58/deviceA">>, <<1, 7, 20, 30, 40>>} = test_broker_api:get_published_msg(),
    2 = test_modbus_server:visitors(),

    emq_modbus_device:disconnect("deviceA"),
    emq_modbus_control:stop(),
    emq_modbus_registry:stop(),
    stop_test_env().




case09(_Config) ->
    ServerHandler = fun (Server) ->
                        ?LOGT("server connection start to receive~n", []),
                        case gen_tcp:recv(Server, 0) of
                            {ok, InData} ->
                                ?LOGT("server receive ~p~n", [InData]),
                                C = <<7, 8, 9, 10>>, Size = byte_size(C)+1,
                                <<Tid:16, 0:16, Size:16, Unit:8, C/binary>> = InData,
                                Body = <<7, 20, 30, 40>>, Len = byte_size(Body)+1,
                                OutData = <<Tid:16, ?RESERVE:16, Len:16, Unit:8, Body/binary>>,
                                ok = gen_tcp:send(Server, OutData);
                            {error, closed} -> ?LOGT("server socket closed~n", [])
                        end
                    end,
    start_test_env(ServerHandler, ServerHandler),

    Vals = [{company, "company3"},{edgename, "Edge58"},
        {device, [{"localhost", ?MODBUS_PORT, "deviceA"}]} ],
    [application:set_env(emq_modbus, Par, Value) || {Par, Value} <- Vals],

    ok = application:start(emq_modbus),

    timer:sleep(300),
    emq_modbus_device:send_request(deviceA, {1, 2, 3}), % make device process crashed
    ?LOGT("DO NOT be surprised! emq_modbus_device is inteded to get crashed!!!!!", []),
    timer:sleep(1800),

    <<"/company3/modbus_request/Edge58/+">> = test_broker_api:get_subscrbied_topic(),
    test_broker_api:dispatch(emq_modbus_control, <<"/company3/modbus_request/Edge58/deviceA">>, <<1, 7, 8, 9, 10>>),

    timer:sleep(1200),

    {<<"/company3/modbus_response/Edge58/deviceA">>, <<1, 7, 20, 30, 40>>} = test_broker_api:get_published_msg(),
    2 = test_modbus_server:visitors(),

    stop_test_env(),
    application:stop(emq_modbus).
