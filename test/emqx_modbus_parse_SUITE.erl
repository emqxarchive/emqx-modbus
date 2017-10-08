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

-module(emqx_modbus_parse_SUITE).

-compile(export_all).

-include("emqx_modbus.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LOGT(Format, Args),
    lager:debug("TEST_SUITE: " ++ Format, Args)).



all() -> [case01, case02, case03, case04, case05, case06, case07,
    case11, case12, case13, case14].


init_per_suite(Config) ->
    lager_common_test_backend:bounce(debug),
    Config.

end_per_suite(Config) ->
    Config.


case01(_Config) ->
    State = emqx_modbus_frame:init(),
    Tid = 2, UnitId= 3, FunCode = 4, Body = <<1,2,3>>,
    Size = byte_size(Body)+2,
    RawFrame = <<Tid:16, 0:16, Size:16, UnitId:8, FunCode:8, Body/binary>>,
    {ok, Frame, NewState} = emqx_modbus_frame:parse(RawFrame, State),
    Record = #modbus_frame{header = #mbap_header{tid = Tid, unit_id = UnitId, length = Size, proto_id = 0}, funcode = FunCode, payload = Body},
    ?assertEqual(Record, Frame),
    {RawFrameOutput, _} = emqx_modbus_frame:serialize(UnitId, FunCode, Body, NewState),
    ?assertEqual(<<0:16, 0:16, Size:16, UnitId:8, FunCode:8, Body/binary>>, RawFrameOutput).


case02(_Config) ->
    State = emqx_modbus_frame:init(),
    Tid = 2, UnitId= 3, FunCode = 4, Body = <<1,2,3>>,
    Size = byte_size(Body)+2,
    RawFrame1 = <<Tid:16>>, RawFrame2 = <<0:16, Size:16, UnitId:8, FunCode:8, Body/binary>>,
    {more_data, NewState1} = emqx_modbus_frame:parse(RawFrame1, State),
    {ok, Frame, _NewState2} = emqx_modbus_frame:parse(RawFrame2, NewState1),
    Record = #modbus_frame{header = #mbap_header{tid = Tid, unit_id = UnitId, length = Size, proto_id = 0}, funcode = FunCode, payload = Body},
    ?assertEqual(Record, Frame).


case03(_Config) ->
    State = emqx_modbus_frame:init(),
    Tid = 2, UnitId= 3, FunCode = 4, Body = <<1,2,3>>,
    Size = byte_size(Body)+2,
    RawFrame1 = <<Tid:16, 0:16>>, RawFrame2 = <<Size:16, UnitId:8, FunCode:8, Body/binary>>,
    {more_data, NewState1} = emqx_modbus_frame:parse(RawFrame1, State),
    {ok, Frame, _NewState2} = emqx_modbus_frame:parse(RawFrame2, NewState1),
    Record = #modbus_frame{header = #mbap_header{tid = Tid, unit_id = UnitId, length = Size, proto_id = 0}, funcode = FunCode, payload = Body},
    ?assertEqual(Record, Frame).

case04(_Config) ->
    State = emqx_modbus_frame:init(),
    Tid = 2, UnitId= 3, FunCode = 4, Body = <<1,2,3>>,
    Size = byte_size(Body)+2,
    RawFrame1 = <<Tid:16, 0:16, Size:16>>, RawFrame2 = <<UnitId:8, FunCode:8, Body/binary>>,
    {more_data, NewState1} = emqx_modbus_frame:parse(RawFrame1, State),
    {ok, Frame, _NewState2} = emqx_modbus_frame:parse(RawFrame2, NewState1),
    Record = #modbus_frame{header = #mbap_header{tid = Tid, unit_id = UnitId, length = Size, proto_id = 0}, funcode = FunCode, payload = Body},
    ?assertEqual(Record, Frame).

case05(_Config) ->
    State = emqx_modbus_frame:init(),
    Tid = 2, UnitId= 3, FunCode = 4, Body = <<1,2,3>>,
    Size = byte_size(Body)+2,
    RawFrame1 = <<Tid:16, 0:16, Size:16, UnitId:8>>, RawFrame2 = <<FunCode:8, Body/binary>>,
    {more_data, NewState1} = emqx_modbus_frame:parse(RawFrame1, State),
    {ok, Frame, _NewState2} = emqx_modbus_frame:parse(RawFrame2, NewState1),
    Record = #modbus_frame{header = #mbap_header{tid = Tid, unit_id = UnitId, length = Size, proto_id = 0}, funcode = FunCode, payload = Body},
    ?assertEqual(Record, Frame).

case06(_Config) ->
    State = emqx_modbus_frame:init(),
    Tid = 2, UnitId= 3, FunCode = 4, Body = <<1,2,3>>,
    Size = byte_size(Body)+2,
    RawFrame1 = <<Tid:16, 0:16, Size:16, UnitId:8, FunCode:8>>, RawFrame2 = <<Body/binary>>,
    {more_data, NewState1} = emqx_modbus_frame:parse(RawFrame1, State),
    {ok, Frame, _NewState2} = emqx_modbus_frame:parse(RawFrame2, NewState1),
    Record = #modbus_frame{header = #mbap_header{tid = Tid, unit_id = UnitId, length = Size, proto_id = 0}, funcode = FunCode, payload = Body},
    ?assertEqual(Record, Frame).


case07(_Config) ->
    State = emqx_modbus_frame:init(),
    Id = <<"abcdefgijklm">>,
    RawFrame1 = <<16#C3, 16#25, 16#9D, 16#A9, Id:12/binary>>,
    {id, Frame, _NewState2} = emqx_modbus_frame:parse(RawFrame1, State),
    Record = #modbus_identity{id = Id},
    ?assertEqual(Record, Frame).


case11(_Config) ->
    State = emqx_modbus_frame:init(),
    Tid = 2, UnitId= 3, FunCode = 4, Body = <<1,2,3>>,
    Size = 1,
    RawFrame = <<Tid:16, 0:16, Size:16, UnitId:8, FunCode:8, Body/binary>>,
    ?assertMatch({error, _}, emqx_modbus_frame:parse(RawFrame, State)).

case12(_Config) ->
    State = emqx_modbus_frame:init(),
    Tid = 2, UnitId= 3, FunCode = 4, Body = <<1,2,3>>,
    Size = 300,
    RawFrame = <<Tid:16, 0:16, Size:16, UnitId:8, FunCode:8, Body/binary>>,
    ?assertMatch({error, _}, emqx_modbus_frame:parse(RawFrame, State)).

case13(_Config) ->
    State = emqx_modbus_frame:init(),
    Tid = 2, UnitId= 3, FunCode = 4, Body = <<1,2,3>>,
    Size = byte_size(Body)+2,
    RawFrame = <<Tid:16, 99:16, Size:16, UnitId:8, FunCode:8, Body/binary>>,
    {error, NewState} = emqx_modbus_frame:parse(RawFrame, State),
    {ok, Frame, _NewState2} = emqx_modbus_frame:parse(<<Tid:16, 0:16, Size:16, UnitId:8, FunCode:8, Body/binary>>, NewState),
    Record = #modbus_frame{header = #mbap_header{tid = Tid, unit_id = UnitId, length = Size, proto_id = 0}, funcode = FunCode, payload = Body},
    ?assertEqual(Record, Frame).

case14(_Config) ->
    State = emqx_modbus_frame:init(),
    State1 = random_test_body(State),
    State2 = random_test_body(State1),
    State3 = random_test_body(State2),
    State4 = random_test_body(State3),
    random_test_body(State4).


random_test_body(State) ->
    Data = generate_random_binary(),
    case catch emqx_modbus_frame:parse(Data, State) of
        {ok, _Msg, State1} -> State1;
        {more_data, State2} -> State2;
        {error, State3} -> State3;
        {id, _, State4} -> State4
    end.


generate_random_binary() ->
    Len = rand:uniform(300),
    gen_next(Len, <<>>).

gen_next(0, Acc) ->
    Acc;
gen_next(N, Acc) ->
    Byte = rand:uniform(256) - 1,
    gen_next(N-1, <<Acc/binary, Byte:8>>).


