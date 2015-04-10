-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Contains components that can simulate various interfaces used in the RapidIO
-- IP library project.
-- 
-- To Do:
-- - Add Symbol-testport from TestRioSerial here.
-- 
-- Author(s): 
-- - Magnus Rosenius, magro732@opencores.org 
-- 
-------------------------------------------------------------------------------
-- 
-- Copyright (C) 2013 Authors and OPENCORES.ORG 
-- 
-- This source file may be used and distributed without 
-- restriction provided that this copyright statement is not 
-- removed from the file and that any derivative work contains 
-- the original copyright notice and the associated disclaimer. 
-- 
-- This source file is free software; you can redistribute it 
-- and/or modify it under the terms of the GNU Lesser General 
-- Public License as published by the Free Software Foundation; 
-- either version 2.1 of the License, or (at your option) any 
-- later version. 
-- 
-- This source is distributed in the hope that it will be 
-- useful, but WITHOUT ANY WARRANTY; without even the implied 
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
-- PURPOSE. See the GNU Lesser General Public License for more 
-- details. 
-- 
-- You should have received a copy of the GNU Lesser General 
-- Public License along with this source; if not, download it 
-- from http://www.opencores.org/lgpl.shtml 
-- 
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use std.textio.all;
use work.rio_common.all;

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
package TestPortPackage is
  -----------------------------------------------------------------------------
  -- Types used in simulations.
  -----------------------------------------------------------------------------
  type ByteArray is array (natural range <>) of
    std_logic_vector(7 downto 0);
  type HalfwordArray is array (natural range <>) of
    std_logic_vector(15 downto 0);
  type WordArray is array (natural range <>) of
    std_logic_vector(31 downto 0);
  type DoublewordArray is array (natural range <>) of
    std_logic_vector(63 downto 0);

  -- Type defining a RapidIO frame.
  type RioFrame is record
    length : natural range 0 to 69;
    payload : WordArray(0 to 68);
  end record;
  type RioFrameArray is array (natural range <>) of RioFrame;

  -- Type defining a RapidIO payload.
  type RioPayload is record
    length : natural range 0 to 133;
    data : HalfwordArray(0 to 132);
  end record;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  
  constant ADDRESS_WIDTH_MAX : natural := 64;
  constant DATA_WIDTH_MAX : natural := 64;
  constant SEL_WIDTH_MAX : natural := 8;
  
  type TestPortMessageWishbone is record
    writeAccess : boolean;
    address : std_logic_vector(ADDRESS_WIDTH_MAX-1 downto 0);
    byteSelect : std_logic_vector(SEL_WIDTH_MAX-1 downto 0);
    data : std_logic_vector(DATA_WIDTH_MAX-1 downto 0);
    continue : boolean;
    latency : natural;
  end record;
  type TestPortMessageWishboneArray is
    array (natural range <>) of TestPortMessageWishbone;

  type TestPortMessageSymbol is record
    symbolType : std_logic_vector(1 downto 0);
    symbolContent : std_logic_vector(31 downto 0);
    ignoreIdle : boolean;
  end record;
  type TestPortMessageSymbolArray is
    array (natural range <>) of TestPortMessageSymbol;

  type TestPortMessagePacketBuffer is record
    frame : RioFrame;
    willAbort : boolean;
  end record;
  type TestPortMessagePacketBufferArray is
    array (natural range <>) of TestPortMessagePacketBuffer;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------

  component TestPortWishbone is
    generic(
      ADDRESS_WIDTH : natural := 31;
      SEL_WIDTH : natural := 8;
      DATA_WIDTH : natural := 64);
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      messageEmpty_o : out std_logic;
      messageWrite_i : in std_logic;
      message_i : in TestPortMessageWishbone;
      messageAck_o : out std_logic;

      cyc_i : in std_logic;
      stb_i : in std_logic;
      we_i : in std_logic;
      adr_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      sel_i : in std_logic_vector(SEL_WIDTH-1 downto 0);
      dat_i : in std_logic_vector(DATA_WIDTH-1 downto 0);
      dat_o : out std_logic_vector(DATA_WIDTH-1 downto 0);
      err_o : out std_logic;
      ack_o : out std_logic);
  end component;
  
  component TestPortPacketBuffer is
    generic(
      READ_CONTENT_END_DATA_VALID : boolean := true);
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      readEmpty_o : out std_logic;
      readWrite_i : in std_logic;
      readMessage_i : in TestPortMessagePacketBuffer;
      readAck_o : out std_logic;
      
      writeEmpty_o : out std_logic;
      writeWrite_i : in std_logic;
      writeMessage_i : in TestPortMessagePacketBuffer;
      writeAck_o : out std_logic;
      
      readFrameEmpty_o : out std_logic;
      readFrame_i : in std_logic;
      readFrameRestart_i : in std_logic;
      readFrameAborted_o : out std_logic;
      readWindowEmpty_o : out std_logic;
      readWindowReset_i : in std_logic;
      readWindowNext_i : in std_logic;
      readContentEmpty_o : out std_logic;
      readContent_i : in std_logic;
      readContentEnd_o : out std_logic;
      readContentData_o : out std_logic_vector(31 downto 0);
      
      writeFrame_i : in std_logic;
      writeFrameAbort_i : in std_logic;
      writeContent_i : in std_logic;
      writeContentData_i : in std_logic_vector(31 downto 0));
  end component;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  procedure TestPortWishboneWrite(
    signal writeSignal : out std_logic;
    signal messageSignal : out TestPortMessageWishbone;
    signal ackSignal : in std_logic;
    constant writeAccess : in boolean;
    constant address : in std_logic_vector(ADDRESS_WIDTH_MAX-1 downto 0);
    constant byteSelect : in std_logic_vector(SEL_WIDTH_MAX-1 downto 0);
    constant data : in std_logic_vector(DATA_WIDTH_MAX-1 downto 0);
    constant continue : in boolean := false;
    constant latency : in natural := 0);

  procedure TestPortPacketBufferWrite(
    signal writeSignal : out std_logic;
    signal messageSignal : out TestPortMessagePacketBuffer;
    signal ackSignal : in std_logic;
    constant frame : in RioFrame;
    constant willAbort : in boolean := false);

  -----------------------------------------------------------------------------
  -- Function to print a std_logic_vector.
  -----------------------------------------------------------------------------
  function to_string(constant value : std_logic_vector)
    return string;
    
  ---------------------------------------------------------------------------
  -- Procedures for test control.
  ---------------------------------------------------------------------------

  procedure TestSpec(constant str : string);
  procedure TestCaseStart(constant str : string);
  
  procedure TestWarning(constant tag : in string);
  procedure TestError(constant tag : in string;
                      constant stopAtError : in boolean := true);
  procedure TestCompare(constant expression : in boolean;
                        constant tag : in string := "";
                        constant stopAtError : in boolean := true);
  procedure TestCompare(constant got : in std_logic;
                        constant expected : in std_logic;
                        constant tag : in string := "";
                        constant stopAtError : in boolean := true);
  procedure TestCompare(constant got : in std_logic_vector;
                        constant expected : in std_logic_vector;
                        constant tag : in string := "";
                        constant stopAtError : in boolean := true);
  procedure TestCompare(constant got : in natural;
                        constant expected : in natural;
                        constant tag : in string := "";
                        constant stopAtError : in boolean := true);
  procedure TestCompare(constant got : in time;
                        constant expected : in time;
                        constant tag : in string := "";
                        constant stopAtError : in boolean := true);
  
  procedure TestWait(signal waitSignal : in std_logic;
                     constant waitValue : in std_logic;
                     constant tag : in string := "";
                     constant waitTime : in time := 1 ms;
                     constant stopAtError : in boolean := true);
  procedure TestWait(signal waitSignal : in std_logic;
                     constant waitValue : in std_logic;
                     signal ackSignal : inout std_logic;
                     constant tag : in string := "";
                     constant waitTime : in time := 1 ms;
                     constant stopAtError : in boolean := true);

  procedure TestEnd;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  
  -----------------------------------------------------------------------------
  -- Crc5 calculation function.
  -- ITU, polynom=0x15.
  -----------------------------------------------------------------------------
  function Crc5(constant data : in std_logic_vector(18 downto 0);
                constant crc : in std_logic_vector(4 downto 0))
    return std_logic_vector;
  
  ---------------------------------------------------------------------------
  -- Create a RapidIO physical layer control symbol.
  ---------------------------------------------------------------------------
  function RioControlSymbolCreate(
    constant stype0 : in std_logic_vector(2 downto 0);
    constant parameter0 : in std_logic_vector(4 downto 0);
    constant parameter1 : in std_logic_vector(4 downto 0);
    constant stype1 : in std_logic_vector(2 downto 0);
    constant cmd : in std_logic_vector(2 downto 0))
    return std_logic_vector;

  -----------------------------------------------------------------------------
  -- Crc16 calculation function.
  -- CITT, polynom=0x1021.
  -----------------------------------------------------------------------------
  function Crc16(constant data : in std_logic_vector(15 downto 0);
                 constant crc : in std_logic_vector(15 downto 0))
    return std_logic_vector;
  
  ---------------------------------------------------------------------------
  -- Create a randomly initialized data array.
  ---------------------------------------------------------------------------
  procedure CreateRandomPayload(
    variable payload : out HalfwordArray;
    variable seed1 : inout positive;
    variable seed2 : inout positive);
  procedure CreateRandomPayload(
    variable payload : out DoublewordArray;
    variable seed1 : inout positive;
    variable seed2 : inout positive);

  ---------------------------------------------------------------------------
  -- Create a generic RapidIO frame.
  ---------------------------------------------------------------------------
  function RioFrameCreate(
    constant ackId : in std_logic_vector(4 downto 0);
    constant vc : in std_logic;
    constant crf : in std_logic;
    constant prio : in std_logic_vector(1 downto 0);
    constant tt : in std_logic_vector(1 downto 0);
    constant ftype : in std_logic_vector(3 downto 0);
    constant sourceId : in std_logic_vector(15 downto 0);
    constant destId : in std_logic_vector(15 downto 0);
    constant payload : in RioPayload)
    return RioFrame;
  
  ---------------------------------------------------------------------------
  -- Create a NWRITE RapidIO frame.
  ---------------------------------------------------------------------------
  function RioNwrite(
    constant wrsize : in std_logic_vector(3 downto 0);
    constant address : in std_logic_vector(28 downto 0);
    constant wdptr : in std_logic;
    constant xamsbs : in std_logic_vector(1 downto 0);
    constant dataLength : in natural range 1 to 32;
    constant data : in DoublewordArray(0 to 31))
    return RioPayload;
  
  ---------------------------------------------------------------------------
  -- Create a NWRITER RapidIO frame.
  ---------------------------------------------------------------------------
  function RioNwriteR(
    constant wrsize : in std_logic_vector(3 downto 0);
    constant tid : in std_logic_vector(7 downto 0);
    constant address : in std_logic_vector(28 downto 0);
    constant wdptr : in std_logic;
    constant xamsbs : in std_logic_vector(1 downto 0);
    constant dataLength : in natural range 1 to 32;
    constant data : in DoublewordArray(0 to 31))
    return RioPayload;
  
  ---------------------------------------------------------------------------
  -- Create a NREAD RapidIO frame.
  ---------------------------------------------------------------------------
  function RioNread(
    constant rdsize : in std_logic_vector(3 downto 0);
    constant tid : in std_logic_vector(7 downto 0);
    constant address : in std_logic_vector(28 downto 0);
    constant wdptr : in std_logic;
    constant xamsbs : in std_logic_vector(1 downto 0))
    return RioPayload;
  
  ---------------------------------------------------------------------------
  -- Create a RESPONSE RapidIO frame.
  ---------------------------------------------------------------------------
  function RioResponse(
    constant status : in std_logic_vector(3 downto 0);
    constant tid : in std_logic_vector(7 downto 0);
    constant dataLength : in natural range 0 to 32;
    constant data : in DoublewordArray(0 to 31))
    return RioPayload;
  
  ---------------------------------------------------------------------------
  -- Create a Maintenance RapidIO frame.
  ---------------------------------------------------------------------------
  function RioMaintenance(
    constant transaction : in std_logic_vector(3 downto 0);
    constant size : in std_logic_vector(3 downto 0);
    constant tid : in std_logic_vector(7 downto 0);
    constant hopCount : in std_logic_vector(7 downto 0);
    constant configOffset : in std_logic_vector(20 downto 0);
    constant wdptr : in std_logic;
    constant dataLength : in natural range 0 to 8;
    constant data : in DoublewordArray(0 to 7))
    return RioPayload;
  
  -----------------------------------------------------------------------------
  -- Function to convert a std_logic_vector to a string.
  -----------------------------------------------------------------------------
  function byteToString(constant byte : std_logic_vector(7 downto 0))
    return string;
  
end package;

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
package body TestPortPackage is

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  procedure TestPortWishboneWrite(
    signal writeSignal : out std_logic;
    signal messageSignal : out TestPortMessageWishbone;
    signal ackSignal : in std_logic;
    constant writeAccess : in boolean;
    constant address : in std_logic_vector(ADDRESS_WIDTH_MAX-1 downto 0);
    constant byteSelect : in std_logic_vector(SEL_WIDTH_MAX-1 downto 0);
    constant data : in std_logic_vector(DATA_WIDTH_MAX-1 downto 0);
    constant continue : in boolean := false;
    constant latency : in natural := 0) is
  begin
    writeSignal <= '1';
    messageSignal.writeAccess <= writeAccess;
    messageSignal.address <= address;
    messageSignal.byteSelect <= byteSelect;
    messageSignal.data <= data;
    messageSignal.continue <= continue;
    messageSignal.latency <= latency;
    wait until ackSignal = '1';
    writeSignal <= '0';
    wait until ackSignal = '0';
  end procedure;
    
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  procedure TestPortPacketBufferWrite(
    signal writeSignal : out std_logic;
    signal messageSignal : out TestPortMessagePacketBuffer;
    signal ackSignal : in std_logic;
    constant frame : in RioFrame;
    constant willAbort : in boolean := false) is
  begin
    writeSignal <= '1';
    messageSignal.frame <= frame;
    messageSignal.willAbort <= willAbort;
    wait until ackSignal = '1';
    writeSignal <= '0';
    wait until ackSignal = '0';
  end procedure;
    
  -----------------------------------------------------------------------------
  -- Function to print std_logic_vector.
  -----------------------------------------------------------------------------
  function to_string(constant value : std_logic) return string is
    variable s : string(1 to 1);
  begin
    if (value = '0') then
      s(1) := '0';
    elsif (value = '1') then
      s(1) := '1';
    elsif (value = 'U') then
      s(1) := 'U';
    elsif (value = 'X') then
      s(1) := 'X';
    else
      s(1) := '?';
    end if;
    return s;
  end function;
  function to_string(constant value : std_logic_vector) return string is
    variable s : string(1 to value'length);
    variable index : positive;
    variable i : natural;
  begin
    index := 1;
    for i in value'range loop
      if (value(i) = '0') then
        s(index) := '0';
      elsif (value(i) = '1') then
        s(index) := '1';
      elsif (value(i) = 'U') then
        s(index) := 'U';
      elsif (value(i) = 'X') then
        s(index) := 'X';
      else
        s(index) := '?';
      end if;
      index := index + 1;
    end loop;
    return s;
  end function;
  
  ---------------------------------------------------------------------------
  -- Procedures to handle tests.
  ---------------------------------------------------------------------------

  procedure TestSpec(constant str : string) is
    file specFile  : text;
    variable specLine, outputLine : line;
    variable fStatus: FILE_OPEN_STATUS;
  begin
    file_open(fStatus, specFile, "testspec.txt", append_mode);
    write(specLine, string'(str));
    writeline (specFile, specLine);
    file_close(specFile);
  end procedure;

  procedure TestCaseStart(constant str : string) is
    file reportFile  : text;
    variable reportLine, outputLine : line;
    variable fStatus: FILE_OPEN_STATUS;
  begin
    report str severity note;
  end procedure;

  procedure TestWarning(constant tag : in string) is
    variable writeBuffer : line;
  begin
    write(writeBuffer, now);
    write(writeBuffer, string'(":WARNING:"));
    write(writeBuffer, tag);
    writeline(OUTPUT, writeBuffer);
  end procedure;
  
  procedure TestError(constant tag : in string;
                      constant stopAtError : in boolean := true) is
    variable writeBuffer : line;
  begin
    write(writeBuffer, now);
    write(writeBuffer, string'(":FAILED:"));
    write(writeBuffer, tag);
    writeline(OUTPUT, writeBuffer);
    
    if (stopAtError) then
      std.env.stop(0);
    end if;
  end procedure;
  
  procedure TestCompare(constant expression : in boolean;
                        constant tag : in string := "";
                        constant stopAtError : in boolean := true) is
    variable writeBuffer : line;
  begin
    write(writeBuffer, now);
    if (not expression) then
      write(writeBuffer, string'(":FAILED:"));
    else
      write(writeBuffer, string'(":PASSED:"));
    end if;
    write(writeBuffer, tag);
    writeline(OUTPUT, writeBuffer);
    
    if (stopAtError) and (not expression) then
      std.env.stop(0);
    end if;
  end procedure;
  
  procedure TestCompare(constant got : in std_logic;
                        constant expected : in std_logic;
                        constant tag : in string := "";
                        constant stopAtError : in boolean := true) is
    variable writeBuffer : line;
  begin
    write(writeBuffer, now);
    if (expected /= got) then
      write(writeBuffer, string'(":FAILED:"));
      write(writeBuffer, tag);
      write(writeBuffer, ":got=" & to_string(got));
      write(writeBuffer, ":expected=" & to_string(expected));
    else
      write(writeBuffer, string'(":PASSED:"));
      write(writeBuffer, tag);
    end if;
    writeline(OUTPUT, writeBuffer);

    if (stopAtError) and (expected /= got) then
      std.env.stop(0);
    end if;
  end procedure;
  
  procedure TestCompare(constant got : in std_logic_vector;
                        constant expected : in std_logic_vector;
                        constant tag : in string := "";
                        constant stopAtError : in boolean := true) is
    variable writeBuffer : line;
  begin
    write(writeBuffer, now);
    if (expected /= got) then
      write(writeBuffer, string'(":FAILED:"));
      write(writeBuffer, tag);
      write(writeBuffer, ":got=" & to_string(got));
      write(writeBuffer, ":expected=" & to_string(expected));
    else
      write(writeBuffer, string'(":PASSED:"));
      write(writeBuffer, tag);
    end if;
    writeline(OUTPUT, writeBuffer);

    if (stopAtError) and (expected /= got) then
      std.env.stop(0);
    end if;
  end procedure;
  
  procedure TestCompare(constant got : in natural;
                        constant expected : in natural;
                        constant tag : in string := "";
                        constant stopAtError : in boolean := true) is
    variable writeBuffer : line;
  begin
    write(writeBuffer, now);
    if (expected /= got) then
      write(writeBuffer, string'(":FAILED:"));
      write(writeBuffer, tag);
      write(writeBuffer, ":got=" & integer'image(got));
      write(writeBuffer, ":expected=" & integer'image(expected));
    else
      write(writeBuffer, string'(":PASSED:"));
      write(writeBuffer, tag);
    end if;
    writeline(OUTPUT, writeBuffer);

    if (stopAtError) and (expected /= got) then
      std.env.stop(0);
    end if;
  end procedure;
  
  procedure TestCompare(constant got : in time;
                        constant expected : in time;
                        constant tag : in string := "";
                        constant stopAtError : in boolean := true) is
    variable writeBuffer : line;
  begin
    write(writeBuffer, now);
    if (expected /= got) then
      write(writeBuffer, string'(":FAILED:"));
      write(writeBuffer, tag);
      write(writeBuffer, string'(":got="));
      write(writeBuffer, got);
      write(writeBuffer, string'(":expected="));
      write(writeBuffer, expected);
    else
      write(writeBuffer, string'(":PASSED:"));
      write(writeBuffer, tag);
    end if;
    writeline(OUTPUT, writeBuffer);

    if (stopAtError) and (expected /= got) then
      std.env.stop(0);
    end if;
  end procedure;

  procedure TestWait(signal waitSignal : in std_logic;
                     constant waitValue : in std_logic;
                     constant tag : in string := "";
                     constant waitTime : in time := 1 ms;
                     constant stopAtError : in boolean := true) is
    variable writeBuffer : line;
  begin
    if (waitSignal /= waitValue) then
      wait until waitSignal = waitValue for waitTime;
      if (waitSignal /= waitValue) then
        write(writeBuffer, now);
        write(writeBuffer, string'(":FAILED:"));
        write(writeBuffer, tag);
        writeline(OUTPUT, writeBuffer);
        
        if (stopAtError) then
          std.env.stop(0);
        end if;
      end if;
    end if;
  end procedure;
  
  procedure TestWait(signal waitSignal : in std_logic;
                     constant waitValue : in std_logic;
                     signal ackSignal : inout std_logic;
                     constant tag : in string := "";
                     constant waitTime : in time := 1 ms;
                     constant stopAtError : in boolean := true) is
    variable writeBuffer : line;
  begin
    if (waitSignal /= waitValue) then

      wait until waitSignal = waitValue for waitTime;
      
      if (waitSignal /= waitValue) then
        write(writeBuffer, now);
        write(writeBuffer, string'(":FAILED:"));
        write(writeBuffer, tag);
        writeline(OUTPUT, writeBuffer);
        
        if (stopAtError) then
          std.env.stop(0);
        end if;
      end if;
    end if;
    
    ackSignal <= not ackSignal;
      
    wait until waitSignal /= waitValue for waitTime;
      
    if (waitSignal = waitValue) then
      write(writeBuffer, now);
      write(writeBuffer, string'(":FAILED:"));
      write(writeBuffer, tag);
      writeline(OUTPUT, writeBuffer);
        
      if (stopAtError) then
        std.env.stop(0);
      end if;
    end if;
  end procedure;

  procedure TestEnd is
    variable writeBuffer : line;
  begin
    write(writeBuffer, now);
    write(writeBuffer, string'(":COMPLETED"));
    writeline(OUTPUT, writeBuffer);
    std.env.stop(0);
  end TestEnd;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -- Crc5 calculation function.
  -- ITU, polynom=0x15.
  -----------------------------------------------------------------------------
  function Crc5(constant data : in std_logic_vector(18 downto 0);
                constant crc : in std_logic_vector(4 downto 0))
    return std_logic_vector is
    type crcTableType is array (0 to 31) of std_logic_vector(7 downto 0);
    constant crcTable : crcTableType := (
      x"00", x"15", x"1f", x"0a", x"0b", x"1e", x"14", x"01",
      x"16", x"03", x"09", x"1c", x"1d", x"08", x"02", x"17",
      x"19", x"0c", x"06", x"13", x"12", x"07", x"0d", x"18",
      x"0f", x"1a", x"10", x"05", x"04", x"11", x"1b", x"0e" );
    variable index : natural range 0 to 31;
    variable result : std_logic_vector(4 downto 0);
  begin
    result := crc;
    index := to_integer(unsigned(data(18 downto 14) xor result));
    result := crcTable(index)(4 downto 0);
    index := to_integer(unsigned(data(13 downto 9) xor result));
    result := crcTable(index)(4 downto 0);
    index := to_integer(unsigned(data(8 downto 4) xor result));
    result := crcTable(index)(4 downto 0);
    index := to_integer(unsigned((data(3 downto 0) & '0') xor result));
    return crcTable(index)(4 downto 0);
  end Crc5;

  ---------------------------------------------------------------------------
  -- Create a RapidIO physical layer control symbol.
  ---------------------------------------------------------------------------
  function RioControlSymbolCreate(
    constant stype0 : in std_logic_vector(2 downto 0);
    constant parameter0 : in std_logic_vector(4 downto 0);
    constant parameter1 : in std_logic_vector(4 downto 0);
    constant stype1 : in std_logic_vector(2 downto 0);
    constant cmd : in std_logic_vector(2 downto 0))
    return std_logic_vector is
    variable returnValue : std_logic_vector(23 downto 0);
    variable symbolData : std_logic_vector(18 downto 0);
  begin
    symbolData(18 downto 16) := stype0;
    symbolData(15 downto 11) := parameter0;
    symbolData(10 downto 6) := parameter1;
    symbolData(5 downto 3) := stype1;
    symbolData(2 downto 0) := cmd;

    returnValue(23 downto 5) := symbolData;
    returnValue(4 downto 0) := Crc5(symbolData, "11111");

    return returnValue;
  end function;

  -----------------------------------------------------------------------------
  -- Crc16 calculation function.
  -- CITT, polynom=0x1021.
  -----------------------------------------------------------------------------
  function Crc16(constant data : in std_logic_vector(15 downto 0);
                 constant crc : in std_logic_vector(15 downto 0))
    return std_logic_vector is
    type crcTableType is array (0 to 255) of std_logic_vector(15 downto 0);
    constant crcTable : crcTableType := (
      x"0000", x"1021", x"2042", x"3063", x"4084", x"50a5", x"60c6", x"70e7",
      x"8108", x"9129", x"a14a", x"b16b", x"c18c", x"d1ad", x"e1ce", x"f1ef",
      x"1231", x"0210", x"3273", x"2252", x"52b5", x"4294", x"72f7", x"62d6",
      x"9339", x"8318", x"b37b", x"a35a", x"d3bd", x"c39c", x"f3ff", x"e3de",
      x"2462", x"3443", x"0420", x"1401", x"64e6", x"74c7", x"44a4", x"5485",
      x"a56a", x"b54b", x"8528", x"9509", x"e5ee", x"f5cf", x"c5ac", x"d58d",
      x"3653", x"2672", x"1611", x"0630", x"76d7", x"66f6", x"5695", x"46b4",
      x"b75b", x"a77a", x"9719", x"8738", x"f7df", x"e7fe", x"d79d", x"c7bc",
      x"48c4", x"58e5", x"6886", x"78a7", x"0840", x"1861", x"2802", x"3823",
      x"c9cc", x"d9ed", x"e98e", x"f9af", x"8948", x"9969", x"a90a", x"b92b",
      x"5af5", x"4ad4", x"7ab7", x"6a96", x"1a71", x"0a50", x"3a33", x"2a12",
      x"dbfd", x"cbdc", x"fbbf", x"eb9e", x"9b79", x"8b58", x"bb3b", x"ab1a",
      x"6ca6", x"7c87", x"4ce4", x"5cc5", x"2c22", x"3c03", x"0c60", x"1c41",
      x"edae", x"fd8f", x"cdec", x"ddcd", x"ad2a", x"bd0b", x"8d68", x"9d49",
      x"7e97", x"6eb6", x"5ed5", x"4ef4", x"3e13", x"2e32", x"1e51", x"0e70",
      x"ff9f", x"efbe", x"dfdd", x"cffc", x"bf1b", x"af3a", x"9f59", x"8f78",
      x"9188", x"81a9", x"b1ca", x"a1eb", x"d10c", x"c12d", x"f14e", x"e16f",
      x"1080", x"00a1", x"30c2", x"20e3", x"5004", x"4025", x"7046", x"6067",
      x"83b9", x"9398", x"a3fb", x"b3da", x"c33d", x"d31c", x"e37f", x"f35e",
      x"02b1", x"1290", x"22f3", x"32d2", x"4235", x"5214", x"6277", x"7256",
      x"b5ea", x"a5cb", x"95a8", x"8589", x"f56e", x"e54f", x"d52c", x"c50d",
      x"34e2", x"24c3", x"14a0", x"0481", x"7466", x"6447", x"5424", x"4405",
      x"a7db", x"b7fa", x"8799", x"97b8", x"e75f", x"f77e", x"c71d", x"d73c",
      x"26d3", x"36f2", x"0691", x"16b0", x"6657", x"7676", x"4615", x"5634",
      x"d94c", x"c96d", x"f90e", x"e92f", x"99c8", x"89e9", x"b98a", x"a9ab",
      x"5844", x"4865", x"7806", x"6827", x"18c0", x"08e1", x"3882", x"28a3",
      x"cb7d", x"db5c", x"eb3f", x"fb1e", x"8bf9", x"9bd8", x"abbb", x"bb9a",
      x"4a75", x"5a54", x"6a37", x"7a16", x"0af1", x"1ad0", x"2ab3", x"3a92",
      x"fd2e", x"ed0f", x"dd6c", x"cd4d", x"bdaa", x"ad8b", x"9de8", x"8dc9",
      x"7c26", x"6c07", x"5c64", x"4c45", x"3ca2", x"2c83", x"1ce0", x"0cc1",
      x"ef1f", x"ff3e", x"cf5d", x"df7c", x"af9b", x"bfba", x"8fd9", x"9ff8",
      x"6e17", x"7e36", x"4e55", x"5e74", x"2e93", x"3eb2", x"0ed1", x"1ef0" );
    variable index : natural range 0 to 255;
    variable result : std_logic_vector(15 downto 0);
  begin
    result := crc;
    index := to_integer(unsigned(data(15 downto 8) xor result(15 downto 8)));
    result := crcTable(index) xor (result(7 downto 0) & x"00");
    index := to_integer(unsigned(data(7 downto 0) xor result(15 downto 8)));
    result := crcTable(index) xor (result(7 downto 0) & x"00");
    return result;
  end Crc16;
  
  ---------------------------------------------------------------------------
  -- Create a randomly initialized data array.
  ---------------------------------------------------------------------------
  procedure CreateRandomPayload(
    variable payload : out HalfwordArray;
    variable seed1 : inout positive;
    variable seed2 : inout positive) is
    variable rand: real;
    variable int_rand: integer;
    variable stim: std_logic_vector(7 downto 0);
  begin
    for i in payload'range loop
      uniform(seed1, seed2, rand);
      int_rand := integer(trunc(rand*256.0));
      payload(i)(7 downto 0) := std_logic_vector(to_unsigned(int_rand, 8));
      uniform(seed1, seed2, rand);
      int_rand := integer(trunc(rand*256.0));
      payload(i)(15 downto 8) := std_logic_vector(to_unsigned(int_rand, 8));
    end loop;
  end procedure;

  procedure CreateRandomPayload(
    variable payload : out DoublewordArray;
    variable seed1 : inout positive;
    variable seed2 : inout positive) is
    variable rand: real;
    variable int_rand: integer;
    variable stim: std_logic_vector(7 downto 0);
  begin
    for i in payload'range loop
      uniform(seed1, seed2, rand);
      int_rand := integer(trunc(rand*256.0));
      payload(i)(7 downto 0) := std_logic_vector(to_unsigned(int_rand, 8));
      uniform(seed1, seed2, rand);
      int_rand := integer(trunc(rand*256.0));
      payload(i)(15 downto 8) := std_logic_vector(to_unsigned(int_rand, 8));
      uniform(seed1, seed2, rand);
      int_rand := integer(trunc(rand*256.0));
      payload(i)(23 downto 16) := std_logic_vector(to_unsigned(int_rand, 8));
      uniform(seed1, seed2, rand);
      int_rand := integer(trunc(rand*256.0));
      payload(i)(31 downto 24) := std_logic_vector(to_unsigned(int_rand, 8));
      uniform(seed1, seed2, rand);
      int_rand := integer(trunc(rand*256.0));
      payload(i)(39 downto 32) := std_logic_vector(to_unsigned(int_rand, 8));
      uniform(seed1, seed2, rand);
      int_rand := integer(trunc(rand*256.0));
      payload(i)(47 downto 40) := std_logic_vector(to_unsigned(int_rand, 8));
      uniform(seed1, seed2, rand);
      int_rand := integer(trunc(rand*256.0));
      payload(i)(55 downto 48) := std_logic_vector(to_unsigned(int_rand, 8));
      uniform(seed1, seed2, rand);
      int_rand := integer(trunc(rand*256.0));
      payload(i)(63 downto 56) := std_logic_vector(to_unsigned(int_rand, 8));
    end loop;
  end procedure;
  ---------------------------------------------------------------------------
  -- Create a generic RapidIO frame.
  ---------------------------------------------------------------------------
  function RioFrameCreate(
    constant ackId : in std_logic_vector(4 downto 0);
    constant vc : in std_logic;
    constant crf : in std_logic;
    constant prio : in std_logic_vector(1 downto 0);
    constant tt : in std_logic_vector(1 downto 0);
    constant ftype : in std_logic_vector(3 downto 0);
    constant sourceId : in std_logic_vector(15 downto 0);
    constant destId : in std_logic_vector(15 downto 0);
    constant payload : in RioPayload) return RioFrame is
    variable frame : RioFrame;
    variable result : HalfwordArray(0 to 137);
    variable crc : std_logic_vector(15 downto 0) := x"ffff";
  begin
    -- Add the header and addresses.
    result(0) := ackId & "0" & vc & crf & prio & tt & ftype;
    result(1) := destId;
    result(2) := sourceId;

    -- Update the calculated CRC with the header contents.
    crc := Crc16("00000" & result(0)(10 downto 0), crc);
    crc := Crc16(result(1), crc);
    crc := Crc16(result(2), crc);

    -- Check if a single CRC should be added or two.
    if (payload.length <= 37) then
      -- Single CRC.
      for i in 0 to payload.length-1 loop
        result(i+3) := payload.data(i);
        crc := Crc16(payload.data(i), crc);
      end loop;
      result(payload.length+3) := crc;

      -- Check if paddning is needed to make the payload even 32-bit.
      if ((payload.length mod 2) = 1) then
        result(payload.length+4) := x"0000";
        frame.length := (payload.length+5) / 2;
      else
        frame.length := (payload.length+4) / 2;
      end if;      
    else
      -- Double CRC.
      for i in 0 to 36 loop
        result(i+3) := payload.data(i);
        crc := Crc16(payload.data(i), crc);
      end loop;

      -- Add in-the-middle crc.
      result(40) := crc;
      crc := Crc16(crc, crc);
      
      for i in 37 to payload.length-1 loop
        result(i+4) := payload.data(i);
        crc := Crc16(payload.data(i), crc);
      end loop;
      result(payload.length+4) := crc;

      -- Check if paddning is needed to make the payload even 32-bit.
      if ((payload.length mod 2) = 0) then
        result(payload.length+5) := x"0000";
        frame.length := (payload.length+6) / 2;
      else
        frame.length := (payload.length+5) / 2;
      end if;      
    end if;
    
    -- Update the result length.
    for i in 0 to frame.length-1 loop
      frame.payload(i) := result(2*i) & result(2*i+1);
    end loop;

    return frame;
  end function;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  function RioNwrite(
    constant wrsize : in std_logic_vector(3 downto 0);
    constant address : in std_logic_vector(28 downto 0);
    constant wdptr : in std_logic;
    constant xamsbs : in std_logic_vector(1 downto 0);
    constant dataLength : in natural range 1 to 32;
    constant data : in DoublewordArray(0 to 31))
    return RioPayload is
    variable payload : RioPayload;
  begin
    payload.data(0)(15 downto 12) := "0100";
    payload.data(0)(11 downto 8) := wrsize;
    payload.data(0)(7 downto 0) := (others=>'0');

    payload.data(1) := address(28 downto 13);
    
    payload.data(2)(15 downto 3) := address(12 downto 0);
    payload.data(2)(2) := wdptr;
    payload.data(2)(1 downto 0) := xamsbs;

    for i in 0 to dataLength-1 loop
      payload.data(3+4*i) := data(i)(63 downto 48);
      payload.data(4+4*i) := data(i)(47 downto 32);
      payload.data(5+4*i) := data(i)(31 downto 16);
      payload.data(6+4*i) := data(i)(15 downto 0);
    end loop;

    payload.length := 3 + 4*dataLength;
    
    return payload;
  end function;
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  function RioNwriteR(
    constant wrsize : in std_logic_vector(3 downto 0);
    constant tid : in std_logic_vector(7 downto 0);
    constant address : in std_logic_vector(28 downto 0);
    constant wdptr : in std_logic;
    constant xamsbs : in std_logic_vector(1 downto 0);
    constant dataLength : in natural range 1 to 32;
    constant data : in DoublewordArray(0 to 31))
    return RioPayload is
    variable payload : RioPayload;
  begin
    payload.data(0)(15 downto 12) := "0101";
    payload.data(0)(11 downto 8) := wrsize;
    payload.data(0)(7 downto 0) := tid;

    payload.data(1) := address(28 downto 13);
    
    payload.data(2)(15 downto 3) := address(12 downto 0);
    payload.data(2)(2) := wdptr;
    payload.data(2)(1 downto 0) := xamsbs;

    for i in 0 to dataLength-1 loop
      payload.data(3+4*i) := data(i)(63 downto 48);
      payload.data(4+4*i) := data(i)(47 downto 32);
      payload.data(5+4*i) := data(i)(31 downto 16);
      payload.data(6+4*i) := data(i)(15 downto 0);
    end loop;

    payload.length := 3 + 4*dataLength;
    
    return payload;
  end function;
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  function RioNread(
    constant rdsize : in std_logic_vector(3 downto 0);
    constant tid : in std_logic_vector(7 downto 0);
    constant address : in std_logic_vector(28 downto 0);
    constant wdptr : in std_logic;
    constant xamsbs : in std_logic_vector(1 downto 0))
    return RioPayload is
    variable payload : RioPayload;
  begin
    payload.data(0)(15 downto 12) := "0100";
    payload.data(0)(11 downto 8) := rdsize;
    payload.data(0)(7 downto 0) := tid;

    payload.data(1) := address(28 downto 13);
    
    payload.data(2)(15 downto 3) := address(12 downto 0);
    payload.data(2)(2) := wdptr;
    payload.data(2)(1 downto 0) := xamsbs;

    payload.length := 3;

    return payload;
  end function;
  
  ---------------------------------------------------------------------------
  -- Create a RESPONSE RapidIO frame.
  ---------------------------------------------------------------------------
  function RioResponse(
    constant status : in std_logic_vector(3 downto 0);
    constant tid : in std_logic_vector(7 downto 0);
    constant dataLength : in natural range 0 to 32;
    constant data : in DoublewordArray(0 to 31))
    return RioPayload is
    variable payload : RioPayload;
  begin
    payload.data(0)(11 downto 8) := status;
    payload.data(0)(7 downto 0) := tid;

    if (dataLength = 0) then
      payload.data(0)(15 downto 12) := "0000";
      payload.length := 1;
    else
      payload.data(0)(15 downto 12) := "1000";
      
      for i in 0 to dataLength-1 loop
        payload.data(1+4*i) := data(i)(63 downto 48);
        payload.data(2+4*i) := data(i)(47 downto 32);
        payload.data(3+4*i) := data(i)(31 downto 16);
        payload.data(4+4*i) := data(i)(15 downto 0);
      end loop;

      payload.length := 1 + 4*dataLength;
    end if;

    return payload;
  end function;
  
  ---------------------------------------------------------------------------
  -- Create a Maintenance RapidIO frame.
  ---------------------------------------------------------------------------
  function RioMaintenance(
    constant transaction : in std_logic_vector(3 downto 0);
    constant size : in std_logic_vector(3 downto 0);
    constant tid : in std_logic_vector(7 downto 0);
    constant hopCount : in std_logic_vector(7 downto 0);
    constant configOffset : in std_logic_vector(20 downto 0);
    constant wdptr : in std_logic;
    constant dataLength : in natural range 0 to 8;
    constant data : in DoublewordArray(0 to 7))
    return RioPayload is
    variable payload : RioPayload;
  begin
    payload.data(0)(15 downto 12) := transaction;
    payload.data(0)(11 downto 8) := size;
    payload.data(0)(7 downto 0) := tid;

    payload.data(1)(15 downto 8) := hopCount;
    payload.data(1)(7 downto 0) := configOffset(20 downto 13);
    
    payload.data(2)(15 downto 3) := configOffset(12 downto 0);
    payload.data(2)(2) := wdptr;
    payload.data(2)(1 downto 0) := "00";

    if (dataLength = 0) then
      payload.length := 3;
    else
      for i in 0 to dataLength-1 loop
        payload.data(3+4*i) := data(i)(63 downto 48);
        payload.data(4+4*i) := data(i)(47 downto 32);
        payload.data(5+4*i) := data(i)(31 downto 16);
        payload.data(6+4*i) := data(i)(15 downto 0);
      end loop;

      payload.length := 3 + 4*dataLength;
    end if;

    return payload;
  end function;
  
  -----------------------------------------------------------------------------
  -- Function to convert a std_logic_vector to a string.
  -----------------------------------------------------------------------------
  function byteToString(constant byte : std_logic_vector(7 downto 0))
    return string is
    constant table : string(1 to 16) := "0123456789abcdef";
    variable returnValue : string(1 to 2);
  begin
    returnValue(1) := table(to_integer(unsigned(byte(7 downto 4))) + 1);
    returnValue(2) := table(to_integer(unsigned(byte(3 downto 0))) + 1);
    return returnValue;
  end function;
  
end package body;




-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
use work.rio_common.all;
use work.TestPortPackage.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity TestPortPacketBuffer is
  generic(
    READ_CONTENT_END_DATA_VALID : boolean := true);
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    readEmpty_o : out std_logic;
    readWrite_i : in std_logic;
    readMessage_i : in TestPortMessagePacketBuffer;
    readAck_o : out std_logic;
    
    writeEmpty_o : out std_logic;
    writeWrite_i : in std_logic;
    writeMessage_i : in TestPortMessagePacketBuffer;
    writeAck_o : out std_logic;
    
    readFrameEmpty_o : out std_logic;
    readFrame_i : in std_logic;
    readFrameRestart_i : in std_logic;
    readFrameAborted_o : out std_logic;
    readWindowEmpty_o : out std_logic;
    readWindowReset_i : in std_logic;
    readWindowNext_i : in std_logic;
    readContentEmpty_o : out std_logic;
    readContent_i : in std_logic;
    readContentEnd_o : out std_logic;
    readContentData_o : out std_logic_vector(31 downto 0);
    
    -- writeFrameFull_o is missing yes, but you can control it from the testbench directly
    -- instead.
    writeFrame_i : in std_logic;
    writeFrameAbort_i : in std_logic;
    writeContent_i : in std_logic;
    writeContentData_i : in std_logic_vector(31 downto 0));
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture TestPortPacketBufferPortImpl of TestPortPacketBuffer is
  constant QUEUE_SIZE : natural := 255;
  type QueueArray is array (natural range <>) of TestPortMessagePacketBuffer;
  
  function QueueIndexInc(constant i : natural) return natural is
    variable returnValue : natural;
  begin
    if(i = QUEUE_SIZE) then
      returnValue := 0;
    else
      returnValue := i + 1;
    end if;
    return returnValue;
  end function;
  
begin
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  Reader: process
    variable frameQueue : QueueArray(0 to QUEUE_SIZE);
    variable front, back, window : natural range 0 to QUEUE_SIZE;
    variable frameIndex : natural;
  begin
    wait until areset_n = '1';
    
    readFrameEmpty_o <= '1';
    readFrameAborted_o <= '0';
    readWindowEmpty_o <= '1';
    readContentEmpty_o <= '1';
    readContentEnd_o <= '0';
    readContentData_o <= (others=>'0');

    front := 0;
    back := 0;
    window := 0;
    frameIndex := 0;
    readEmpty_o <= '1';
    readAck_o <= '0';

    loop
      wait until clk = '1' or readWrite_i = '1';

      if (clk'event) then
        if (readFrame_i = '1') then
          if ((not frameQueue(back).willAbort) and (frameIndex < frameQueue(back).frame.length)) then
            TestError("READ:BACK:reading unfinished frame");
          end if;
          if (back /= front) then
            back := QueueIndexInc(back);
          else
            TestError("READ:BACK:reading when no frame is present");
          end if;
        end if;

        if (readFrameRestart_i = '1') then
          frameIndex := 0;
        end if;
        
        if (readWindowReset_i = '1') then
          window := back;
          frameIndex := 0;
        end if;

        if (readWindowNext_i = '1') then
          if (window /= front) then
            window := QueueIndexInc(window);
            frameIndex := 0;
          else
            TestError("READ:WINDOW:reading when no frame is present");
          end if;
        end if;
        
        if (readContent_i = '1') then
          if (back /= front) then
            if (READ_CONTENT_END_DATA_VALID) then
              if (frameIndex < frameQueue(window).frame.length) then
                readContentData_o <= frameQueue(window).frame.payload(frameIndex);
                frameIndex := frameIndex + 1;
                if (frameIndex = frameQueue(window).frame.length) then
                  readContentEnd_o <= '1';
                else
                  readContentEnd_o <= '0';
                end if;
              else
                TestError("READ:CONTENT:reading when frame has ended");
              end if;
            else
              if (frameIndex < frameQueue(window).frame.length) then
                readContentData_o <= frameQueue(window).frame.payload(frameIndex);
                readContentEnd_o <= '0';
                frameIndex := frameIndex + 1;
              elsif (frameIndex = frameQueue(window).frame.length) then
                readContentData_o <= (others=>'U');
                readContentEnd_o <= '1';
              else
                TestError("READ:CONTENT:reading when frame has ended");
              end if;
            end if;
          else
            TestError("READ:CONTENT:reading when no frame is present");
          end if;
        end if;

        if (front = back) then
          readFrameEmpty_o <= '1';
        else
          readFrameEmpty_o <= '0';
        end if;
        
        if (front = window) then
          readWindowEmpty_o <= '1';
          readContentEmpty_o <= '1';
        else
          readWindowEmpty_o <= '0';
          if (frameIndex /= frameQueue(window).frame.length) then
            readContentEmpty_o <= '0';
          else
            readContentEmpty_o <= '1';
          end if;
        end if;
        
        if (front = back) then
          readEmpty_o <= '1';
        else
          readEmpty_o <= '0';
        end if;
      elsif (readWrite_i'event) then
        frameQueue(front) := readMessage_i;
        front := QueueIndexInc(front);
        if (front = back) then
          TestError("Queue full");
        end if;

        readEmpty_o <= '0';
        readAck_o <= '1';
        wait until readWrite_i = '0';
        readAck_o <= '0';
      end if;
    end loop;    
  end process;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  Writer: process
    variable frameQueue : QueueArray(0 to QUEUE_SIZE);
    variable front, back : natural range 0 to QUEUE_SIZE;
    variable frameIndex : natural range 0 to 69;
  begin
    wait until areset_n = '1';

    writeEmpty_o <= '1';
    writeAck_o <= '0';

    front := 0;
    back := 0;
    frameIndex := 0;

    loop
      wait until clk = '1' or writeWrite_i = '1';

      if (clk'event) then

        if (writeFrame_i = '1') then
          if (frameIndex = 0) then
            TestError("WRITE:Empty frame written.");
          end if;
          if (frameIndex /= frameQueue(back).frame.length) then
            TestError("WRITE:Frame with unmatching length was written.");
          end if;
          if (back /= front) then
            back := QueueIndexInc(back);
          else
            TestError("WRITE:Unexpected frame written.");
          end if;
          frameIndex := 0;
        end if;

        if (writeFrameAbort_i = '1') then
          if (back /= front) then
            if (frameQueue(back).willAbort) then
              TestCompare(frameIndex,
                          frameQueue(back).frame.length,
                          "frameIndex abort");
              back := QueueIndexInc(back);
            else
              TestError("WRITE:Not expecting this frame to abort.");
            end if;
          end if;
          frameIndex := 0;
        end if;

        if (writeContent_i = '1') then
          if (frameIndex < frameQueue(back).frame.length) then
            TestCompare(writeContentData_i,
                        frameQueue(back).frame.payload(frameIndex),
                        "frame content");
            frameIndex := frameIndex + 1;
          else
            TestError("WRITE:Receiving more frame content than expected.");
          end if;
        end if;
        
        if (front = back) then
          writeEmpty_o <= '1';
        else
          writeEmpty_o <= '0';
        end if;
      elsif (writeWrite_i'event) then
        frameQueue(front) := writeMessage_i;
        front := QueueIndexInc(front);
        if (front = back) then
          TestError("Queue full");
        end if;

        writeEmpty_o <= '0';
        writeAck_o <= '1';
        wait until writeWrite_i = '0';
        writeAck_o <= '0';
      end if;
    end loop;
  end process;

end architecture;



-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
use work.rio_common.all;
use work.TestPortPackage.all;
 

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity TestPortWishbone is
  generic(
    ADDRESS_WIDTH : natural := 31;
    SEL_WIDTH : natural := 8;
    DATA_WIDTH : natural := 64);
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    messageEmpty_o : out std_logic;
    messageWrite_i : in std_logic;
    message_i : in TestPortMessageWishbone;
    messageAck_o : out std_logic;

    cyc_i : in std_logic;
    stb_i : in std_logic;
    we_i : in std_logic;
    adr_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
    sel_i : in std_logic_vector(SEL_WIDTH-1 downto 0);
    dat_i : in std_logic_vector(DATA_WIDTH-1 downto 0);
    dat_o : out std_logic_vector(DATA_WIDTH-1 downto 0);
    err_o : out std_logic;
    ack_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture TestPortWishboneImpl of TestPortWishbone is
  constant QUEUE_SIZE : natural := 63;
  type QueueArray is array (natural range <>) of TestPortMessageWishbone;
  
  function QueueIndexInc(constant i : natural) return natural is
    variable returnValue : natural;
  begin
    if(i = QUEUE_SIZE) then
      returnValue := 0;
    else
      returnValue := i + 1;
    end if;
    return returnValue;
  end function;

begin
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  Slave: process
    variable queue : QueueArray(0 to QUEUE_SIZE);
    variable front, back : natural range 0 to QUEUE_SIZE;
    variable latencyCounter : natural;
    variable activeCycle : boolean;
  begin
    wait until areset_n = '1';

    messageEmpty_o <= '1';
    messageAck_o <= '0';
    
    dat_o <= (others=>'U');
    err_o <= '0';
    ack_o <= '0';

    front := 0;
    back := 0;
    latencyCounter := 0;
    activeCycle := false;
    
    loop
      wait until clk = '1' or messageWrite_i = '1';
      
      if (clk'event) then
        if (cyc_i = '1') then
          if (front /= back) then
            if (stb_i = '1') then
              if (latencyCounter <= queue(back).latency) then
                TestCompare(stb_i, '1', "stb_i");
                if (queue(back).writeAccess) then
                  TestCompare(we_i, '1', "we_i");
                else
                  TestCompare(we_i, '0', "we_i");
                end if;
                TestCompare(adr_i, queue(back).address(ADDRESS_WIDTH-1 downto 0), "adr_i");
                TestCompare(sel_i, queue(back).byteSelect(SEL_WIDTH-1 downto 0), "sel_i");
                if (queue(back).writeAccess) then
                  TestCompare(dat_i, queue(back).data(DATA_WIDTH-1 downto 0), "dat_i");
                end if;
              end if;

              if (latencyCounter < queue(back).latency) then
                dat_o <= (others=>'U');
                ack_o <= '0';
                latencyCounter := latencyCounter + 1;
              elsif (latencyCounter = queue(back).latency) then
                if (queue(back).writeAccess) then
                  dat_o <= (others=>'U');
                else
                  dat_o <= queue(back).data(DATA_WIDTH-1 downto 0);
                end if;
                ack_o <= '1';
                latencyCounter := latencyCounter + 1;
              else
                dat_o <= (others=>'U');
                ack_o <= '0';
                latencyCounter := 0;
                activeCycle := queue(back).continue;
                back := QueueIndexInc(back);
              end if;
            end if;
          else
            TestError("Unexpected access.");
          end if;
        else
          if (activeCycle or (latencyCounter /= 0)) then
            TestError("Cycle unexpectedly aborted.");
            latencyCounter := 0;
          end if;
          TestCompare(stb_i, '0', "stb_i");
        end if;

        if (front = back) then
          messageEmpty_o <= '1';
        else
          messageEmpty_o <= '0';
        end if;
      elsif (messageWrite_i'event) then
        queue(front) := message_i;
        front := QueueIndexInc(front);

        messageEmpty_o <= '0';
        messageAck_o <= '1';
        wait until messageWrite_i = '0';
        messageAck_o <= '0';
      end if;
    end loop;    
  end process;

end architecture;


