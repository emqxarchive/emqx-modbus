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

-module(emq_modbus_mqtt_adapter).

-include("emq_modbus.hrl").
-include_lib("emqttd/include/emqttd.hrl").
-include_lib("emqttd/include/emqttd_protocol.hrl").

-export([proto_init/0, proto_subscribe/2, proto_publish/3, proto_deliver_ack/2]).

-define(MODBUS_CLIENTID, <<"edge_modbus_client">>).


proto_init() ->
    Channel = {{0, 0, 0, 0}, 0},
    SendFun = fun(_Packet) -> ok end,
    PktOpts = [{max_clientid_len, 36}, {max_packet_size, 512}],
    Proto = emqttd_protocol:init(Channel, SendFun, PktOpts),
    ConnPkt = #mqtt_packet_connect{client_id  = ?MODBUS_CLIENTID,
        clean_sess = true,
        keep_alive = 0},
    case emqttd_protocol:received(?CONNECT_PACKET(ConnPkt), Proto) of
        {ok, Proto1}  -> Proto1;
        Other         -> error(Other)
    end.

proto_subscribe(Topic, Proto) ->
    ?LOG(debug, "subscribe Topic=~p", [Topic]),
    case emqttd_protocol:received(?SUBSCRIBE_PACKET(1, [{Topic, ?QOS1}]), Proto) of
        {ok, Proto1}  -> Proto1;
        Other         -> error(Other)
    end.

proto_publish(Topic, Payload, Proto) ->
    ?LOG(debug, "publish Topic=~p, Payload=~p", [Topic, Payload]),
    Publish = #mqtt_packet{header   = #mqtt_packet_header{type = ?PUBLISH, qos = ?QOS1},
        variable = #mqtt_packet_publish{topic_name = Topic, packet_id = 1},
        payload  = Payload},
    case emqttd_protocol:received(Publish, Proto) of
        {ok, Proto1}  -> Proto1;
        Other         -> error(Other)
    end.

proto_deliver_ack(#mqtt_message{qos = ?QOS0, pktid = _PacketId}, Proto) ->
    Proto;
proto_deliver_ack(#mqtt_message{qos = ?QOS1, pktid = PacketId}, Proto) ->
    case emqttd_protocol:received(?PUBACK_PACKET(?PUBACK, PacketId), Proto) of
        {ok, NewProto} -> NewProto;
        Other          -> error(Other)
    end;
proto_deliver_ack(#mqtt_message{qos = ?QOS2, pktid = PacketId}, Proto) ->
    case emqttd_protocol:received(?PUBACK_PACKET(?PUBREC, PacketId), Proto) of
        {ok, NewProto} ->
            case emqttd_protocol:received(?PUBACK_PACKET(?PUBCOMP, PacketId), NewProto) of
                {ok, CurrentProto} -> CurrentProto;
                Another            -> error(Another)
            end;
        Other          -> error(Other)
    end.







