%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015 Feng Lee <feng@emqtt.io>, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc
%%% interface of emqttd.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(emq_modbus_sup).

-behavior(supervisor).

-export([start_link/4, init/1]).

%% @doc Start modbus device Supervisor.
-spec(start_link(list(), list(), integer(), boolean()) -> {ok, pid()}).
start_link(CompanyName, EdgeName, Qos, Retain) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [CompanyName, EdgeName, Qos, Retain]).


init([CompanyName, EdgeName, Qos, Retain]) ->
    {ok, {{one_for_all, 50, 3600},
        [
            {emq_modbus_device_sup, {emq_modbus_device_sup, start_link, []},
                temporary, 5000, worker, [emq_modbus_device_sup]},
            {emq_modbus_control, {emq_modbus_control, start_link, [CompanyName, EdgeName, Qos, Retain]},
                temporary, 5000, worker, [emq_modbus_device]}
        ]}}.





