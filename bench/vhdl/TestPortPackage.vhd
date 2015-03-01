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
use work.rio_common.all;
use std.textio.all;

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
package TestPortPackage is
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


