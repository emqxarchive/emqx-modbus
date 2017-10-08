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

-module(emqx_modbus_sup).

-behavior(supervisor).

-export([start_link/4, init/1]).

%% @doc Start modbus device Supervisor.
-spec(start_link(list(), list(), integer(), integer()) -> {ok, pid()}).
start_link(CompanyName, DeviceList, Mode, Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {CompanyName, DeviceList, Mode, Port}).

init({CompanyName, DeviceList, Mode, Port}) ->
    ChildList = [
        {emqx_modbus_registry, {emqx_modbus_registry, start_link, []},
            permanent, 5000, worker, [emqx_modbus_registry]},
        {emqx_modbus_control, {emqx_modbus_control, start_link, [CompanyName, Mode]},
            permanent, 5000, worker, [emqx_modbus_control]}
    ],
    ChildSpecs = case Mode of
                     1 ->
                         ChildList ++ [{emqx_modbus_device_sup,
                                       {emqx_modbus_device_sup, start_link, [DeviceList]},
                                        permanent, 5000, supervisor, [emqx_modbus_device_sup]}];
                     _ ->
                         SockOpts = [{acceptors, 8},
                                     {max_clients, 1000000},
                                     {sockopts, [binary, {reuseaddr, true}, {nodelay, false}]}],
                         ChildList ++ [{esockd,
                                       {esockd, open, [tcp, Port, SockOpts,
                                           {emqx_modbus_device_mode0, start_link, []}]},
                                       permanent, 5000, supervisor, [esockd]}]
                 end,
    {ok, {{one_for_one, 10, 3600}, ChildSpecs}}.





