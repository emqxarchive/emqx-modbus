%%--------------------------------------------------------------------
%% Copyright (c) 2016-2017 Feng Lee <feng@emqtt.io>. All Rights Reserved.
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

-module(emq_modbus_app).

-author("Feng Lee <feng@emqtt.io>").

-behaviour(application).

-export([start/2, stop/1]).

-define(APP, emq_modbus).

start(_Type, _Args) ->
    Company = application:get_env(?APP, company, "CompanyX"),
    EdgeName = application:get_env(?APP, edgename, "EdgeUnknown"),
    Qos = application:get_env(?APP, qos, 1),
    Retain = application:get_env(?APP, retain, false),

    Ret = emq_modbus_sup:start_link(Company, EdgeName, Qos, Retain),

    DeviceList = application:get_env(?APP, device, []),
    case length(DeviceList) of
        0 -> [];
        _ -> start_device(DeviceList)
    end,
    Ret.

stop(_State) ->
    ok.

start_device(DeviceList) ->
    lists:map(fun ({Host, Port, DeviceName}) ->
        emq_modbus_device_sup:start_device(Host, Port, DeviceName)
              end,
        DeviceList).
