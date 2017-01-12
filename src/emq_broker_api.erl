%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2016-2017 Feng Lee <feng@emqtt.io>, All Rights Reserved.
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
-module(emq_broker_api).

-include_lib("emqttd/include/emqttd.hrl").

%% modbus API Exports
-export([subscribe/1, publish/5]).

-ifdef(TEST).

subscribe(Topic) ->
    test_broker_api:subscribe(Topic).

publish(ClientId, Qos, Retain, Topic, Payload) ->
    test_broker_api:publish(ClientId, Qos, Retain, Topic, Payload).

-else.

subscribe(Topic) ->
    emqttd:subscribe(Topic).

publish(ClientId, Qos, Retain, Topic, Payload) ->
    Msg = emqttd_message:make(ClientId, Qos, Topic, Payload),
    emqttd:publish(Msg#mqtt_message{retain  = Retain}).

-endif.









