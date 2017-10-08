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

-define(APP, emqx_modbus).

-define(MODBUS_VIA_MODULE, emqx_modbus_registry).

-define(LOG(Level, Format, Args), lager:Level("MODBUS-TCP: " ++ Format, Args)).

%%------------------------------------------------------------------------------
%% Modbus Port
%%------------------------------------------------------------------------------

-define(MODBUS_PORT, 502).

%%------------------------------------------------------------------------------
%% Modbus Socket Options
%%------------------------------------------------------------------------------

-define(MODBUS_SOCKOPTS, [
    binary,
    {packet,    raw},
    {reuseaddr, true},
    {nodelay,   true}
]).

%%------------------------------------------------------------------------------
%% Modbus Function Code
%%------------------------------------------------------------------------------

-define(FUN_CODE_READ_COILS,  16#01).
-define(FUN_CODE_READ_INPUTS, 16#02).
-define(FUN_CODE_READ_HREGS,  16#03).
-define(FUN_CODE_READ_IREGS,  16#04).
-define(FUN_CODE_WRITE_COIL,  16#05).
-define(FUN_CODE_WRITE_HREG,  16#06).
-define(FUN_CODE_WRITE_COILS, 16#0f).
-define(FUN_CODE_WRITE_HREGS, 16#10).

%%------------------------------------------------------------------------------
%% Modbus MBAP Header
%%------------------------------------------------------------------------------

-define(MBAP_LENGTH, 6).

%%------------------------------------------------------------------------------
%% MBAP Header
%%------------------------------------------------------------------------------

-record(mbap_header,
        { tid         :: 1..16#ffff,
          proto_id = 0,
          length      :: 1..16#ffff,
          unit_id     :: byte()
        }).

-type(mbap_header() :: #mbap_header{}).

%%------------------------------------------------------------------------------
%% Modbus Request
%%------------------------------------------------------------------------------

-record(modbus_req,
        { msgid    :: byte(),
          uid      :: byte(),
          funcode  :: byte(),
          data     :: binary()
        }).

-type(modbus_req() :: #modbus_req{}).

%%------------------------------------------------------------------------------
%% Modbus Response
%%------------------------------------------------------------------------------

-record(modbus_rsp,
        { device_name :: binary(),
          uid         :: byte(),
          msgid       :: byte(),
          funcode     :: byte(),
          data        :: binary()
        }).

-type(modbus_rsp() :: #modbus_rsp{}).

%%------------------------------------------------------------------------------
%% Modbus Frame
%%------------------------------------------------------------------------------

-record(modbus_frame,
        { header  :: mbap_header(),
          funcode :: byte(),
          payload :: binary()
        }).

-type(modbus_frame() :: #modbus_frame{}).

%%------------------------------------------------------------------------------
%% Modbus Identity (customed by EMQ)
%%------------------------------------------------------------------------------

-record(modbus_identity, {id :: binary()}).
