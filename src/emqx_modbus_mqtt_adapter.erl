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

%%
%% @doc This module is the adapter to EMQ core.
%%

-module(emqx_modbus_mqtt_adapter).

-include("emqx_modbus.hrl").

-include_lib("emqx/include/emqx.hrl").

-include_lib("emqx/include/emqx_mqtt.hrl").

-export([proto_init/0, proto_subscribe/2, proto_publish/3, proto_deliver_ack/2]).

-define(MODBUS_CLIENTID, <<"edge_modbus_client">>).

proto_init() ->
    Channel = {{0, 0, 0, 0}, 0},
    SendFun = fun(_Packet) -> ok end,
    PktOpts = [{max_clientid_len, 36}, {max_packet_size, 512}],
    Proto = emqx_protocol:init(Channel, SendFun, PktOpts),
    ConnPkt = #mqtt_packet_connect{client_id  = ?MODBUS_CLIENTID,
                                   clean_sess = true, keep_alive = 0},
    case emqx_protocol:received(?CONNECT_PACKET(ConnPkt), Proto) of
        {ok, Proto1} -> Proto1;
        Other        -> error(Other)
    end.

proto_subscribe(Topic, Proto) ->
    ?LOG(debug, "subscribe Topic=~p", [Topic]),
    case emqx_protocol:received(?SUBSCRIBE_PACKET(1, [{Topic, ?QOS1}]), Proto) of
        {ok, Proto1}  -> Proto1;
        Other         -> error(Other)
    end.

proto_publish(Topic, Payload, Proto) ->
    ?LOG(debug, "publish Topic=~p, Payload=~p", [Topic, Payload]),
    Publish = #mqtt_packet{header   = #mqtt_packet_header{type = ?PUBLISH, qos = ?QOS1},
                           variable = #mqtt_packet_publish{topic_name = Topic, packet_id = 1},
                           payload  = Payload},
    case emqx_protocol:received(Publish, Proto) of
        {ok, Proto1}  -> Proto1;
        Other         -> error(Other)
    end.

proto_deliver_ack(#mqtt_message{qos = ?QOS0, pktid = _PacketId}, Proto) ->
    Proto;
proto_deliver_ack(#mqtt_message{qos = ?QOS1, pktid = PacketId}, Proto) ->
    case emqx_protocol:received(?PUBACK_PACKET(?PUBACK, PacketId), Proto) of
        {ok, NewProto} -> NewProto;
        Other          -> error(Other)
    end;
proto_deliver_ack(#mqtt_message{qos = ?QOS2, pktid = PacketId}, Proto) ->
    case emqx_protocol:received(?PUBACK_PACKET(?PUBREC, PacketId), Proto) of
        {ok, NewProto} ->
            case emqx_protocol:received(?PUBACK_PACKET(?PUBCOMP, PacketId), NewProto) of
                {ok, CurrentProto} -> CurrentProto;
                Another            -> error(Another)
            end;
        Other ->
            error(Other)
    end.

