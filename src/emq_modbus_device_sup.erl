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
-module(emq_modbus_device_sup).

-behavior(supervisor).

-include("emq_modbus.hrl").

-export([start_link/1, init/1]).

-define(CHILD(Host, Port, Name),
        {{modbus_device, Name}, {emq_modbus_device, connect, [Host, Port, Name]},
            permanent, 5000, worker, [emq_modbus_device]}).

%% @doc Start modbus device Supervisor.
-spec(start_link(list()) -> {ok, pid()}).
start_link(DeviceList) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, DeviceList).


init(DeviceList) ->
    Childs = [ ?CHILD(Host, Port, list_to_binary(DeviceName)) || {Host, Port, DeviceName} <- DeviceList],
    {ok, {{one_for_one, 128, 60}, Childs}}.





