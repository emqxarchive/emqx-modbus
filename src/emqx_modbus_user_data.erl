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

-module(emqx_modbus_user_data).

-include("emqx_modbus.hrl").

-export([handle_dl_data/2, handle_ul_data/1]).

%% input is a mqtt payload from EMQ engine
%% output is a modbus frame going to
handle_dl_data(_MqttTopic, MqttPayload) ->
    Json = jsx:decode(MqttPayload),
    DeviceName = proplists:get_value(<<"device">>, Json),
    UnitId = proplists:get_value(<<"uid">>, Json),
    MsgId = proplists:get_value(<<"msgid">>, Json),
    Code = proplists:get_value(<<"funcode">>, Json),
    Data = proplists:get_value(<<"data">>, Json),
    {DeviceName, #modbus_req{msgid = MsgId, uid = UnitId, funcode = Code, data = base64:decode(Data)}}.

%% input is a message from modbus frame
%% output is a mqtt payload going to EMQ engine
handle_ul_data(#modbus_rsp{device_name = DeviceName, uid = UnitId, msgid = MsgId, funcode = Funcode, data = Data}) ->
    jsx:encode([{<<"device">>, DeviceName},
                {<<"uid">>, UnitId},
                {<<"msgid">>, MsgId},
                {<<"funcode">>, Funcode},
                {<<"data">>, base64:encode(Data)}]).
