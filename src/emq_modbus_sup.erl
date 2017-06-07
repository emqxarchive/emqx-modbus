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
-module(emq_modbus_sup).

-behavior(supervisor).

-export([start_link/3, init/1]).

%% @doc Start modbus device Supervisor.
-spec(start_link(list(), list(), list()) -> {ok, pid()}).
start_link(CompanyName, EdgeName, DeviceList) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {CompanyName, EdgeName, DeviceList}).


init({CompanyName, EdgeName, DeviceList}) ->
    {ok, {{one_for_one, 50, 3600},
        [
            {emq_modbus_registry, {emq_modbus_registry, start_link, []},
                permanent, 5000, worker, [emq_modbus_registry]},
            {emq_modbus_device_sup, {emq_modbus_device_sup, start_link, [DeviceList]},
                permanent, 5000, supervisor, [emq_modbus_device_sup]},
            {emq_modbus_control, {emq_modbus_control, start_link, [CompanyName, EdgeName]},
                permanent, 5000, worker, [emq_modbus_device]}
        ]}}.





