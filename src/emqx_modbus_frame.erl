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

-module(emqx_modbus_frame).

-include("emqx_modbus.hrl").

-export([init/0, parse/2, serialize/4]).

-record(state, {tid_dl, init_parser, parser}).

-define(MAX_MODBUS_SIZE, 260).

%%TODO: Rewrite the parser.
init() ->
    InitParser = fun(Data) -> parse_header(Data) end,
    #state{tid_dl = 0, init_parser = InitParser, parser = InitParser}.

parse(<<>>, State)->
    {more_data, State};
parse(Data, State=#state{init_parser = InitParser, parser = Parser}) ->
    case Parser(Data) of
        {more_data, NewParser} ->
            {more_data, State#state{parser = NewParser}};
        {ok, Frame=#modbus_frame{}, NextParser} ->
            {ok, Frame, State#state{parser = NextParser}};
        {error, _Reason} ->
            {error, State#state{parser = InitParser}};
        Id = #modbus_identity{} ->
            {id, Id, State#state{parser = InitParser}};
        keepalive_packet ->
            {keepalive_packet, State#state{parser = InitParser}}
    end.

parse_header(Data) when byte_size(Data) < 8 ->
    {more_data, fun(Bin) -> parse_header(<<Data/binary, Bin/binary>>) end};
parse_header(<<Tid:16, 0:16, Len:16, Uid:8, Rest/binary>>) ->
    if
        Len =< 1 -> {error, length_is_too_small};
        Len > ?MAX_MODBUS_SIZE -> {error, length_exceed_limit};
        true ->
            parse_pdu(Rest, #mbap_header{tid = Tid, length = Len, unit_id = Uid})
    end;
parse_header(<<16#C3, 16#25, 16#9D, 16#A9, Id:12/binary>>) ->
    #modbus_identity{id=Id};
parse_header(<<16#C3, 16#25, 16#9D, 16#A9>>) ->
    keepalive_packet;
parse_header(<<_Tid:16, Ver:16, _Len:16, _Uid:8, _FunCode:8, _Rest/binary>>) when Ver > 0 ->
    {error, invalid_proto_version}.

parse_pdu(<<>>, Header) ->
    {more_data, fun(Bin) -> parse_pdu(Bin, Header) end};
parse_pdu(InData, Header=#mbap_header{length = Len}) ->
    Size = Len - 2,
    case InData of
        <<FunCode:8, Data:Size/binary, Rest/binary>> ->
            {ok, #modbus_frame{header = Header, funcode = FunCode, payload = Data}, fun(Bin) -> parse_pdu(<<Rest/binary, Bin/binary>>, Header) end};
        Other ->
            {more_data, fun(Bin2) -> parse_pdu(<<Other/binary, Bin2/binary>>, Header) end}
    end.

serialize(Uid, FunCode, Payload, State=#state{tid_dl = Tid}) ->
    Len = 2 + size(Payload),
    {<<Tid:16, 0:16, Len:16, Uid:8, FunCode:8, Payload/binary>>, next_id(State)}.

next_id(State = #state{tid_dl = Tid}) when Tid >= 16#EFFF ->
    State#state{tid_dl = 1};

next_id(State = #state{tid_dl = Tid}) ->
    State#state{tid_dl = Tid+1}.

