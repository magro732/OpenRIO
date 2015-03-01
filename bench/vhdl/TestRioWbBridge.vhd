-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Contains automatic test code to verify a RioWbBridge implementation.
-- 
-- To Do:
-- -
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


-- REMARK: Move the testport package and entities to a seperate file.
-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use work.rio_common.all;

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
package TestPortPackage is
  constant ADDRESS_WIDTH : natural := 31;
  constant DATA_SIZE_MAX : natural := 32;
  
  type TestPortMessageWishbone is record
    writeAccess : boolean;
    address : std_logic_vector(ADDRESS_WIDTH-1 downto 0);
    byteSelect : std_logic_vector(7 downto 0);
    length : natural range 1 to DATA_SIZE_MAX;
    data : DoublewordArray(0 to DATA_SIZE_MAX-1);
    latency : natural;
  end record;

  type TestPortMessageSymbol is record
    symbolType : std_logic_vector(1 downto 0);
    symbolContent : std_logic_vector(31 downto 0);
    ignoreIdle : boolean;
  end record;

  type TestPortMessagePacketBuffer is record
    frame : RioFrame;
    willAbort : boolean;
  end record;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------

  component TestPortWishbone is
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
      adr_i : in std_logic_vector(30 downto 0);
      sel_i : in std_logic_vector(7 downto 0);
      dat_i : in std_logic_vector(63 downto 0);
      dat_o : out std_logic_vector(63 downto 0);
      err_o : out std_logic;
      ack_o : out std_logic);
  end component;
  
  component TestPortPacketBuffer is
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
    constant address : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
    constant byteSelect : in std_logic_vector(7 downto 0);
    constant length : in natural range 1 to DATA_SIZE_MAX;
    constant data : in DoublewordArray(0 to DATA_SIZE_MAX-1);
    constant latency : natural := 0);

  procedure TestPortPacketBufferWrite(
    signal writeSignal : out std_logic;
    signal messageSignal : out TestPortMessagePacketBuffer;
    signal ackSignal : in std_logic;
    constant frame : in RioFrame;
    constant willAbort : boolean := false);

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
    constant address : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
    constant byteSelect : in std_logic_vector(7 downto 0);
    constant length : in natural range 1 to DATA_SIZE_MAX;
    constant data : in DoublewordArray(0 to DATA_SIZE_MAX-1);
    constant latency : natural := 0) is
  begin
    writeSignal <= '1';
    messageSignal.writeAccess <= writeAccess;
    messageSignal.address <= address;
    messageSignal.byteSelect <= byteSelect;
    messageSignal.length <= length;
    messageSignal.data <= data;
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
    constant willAbort : boolean := false) is
  begin
    writeSignal <= '1';
    messageSignal.frame <= frame;
    messageSignal.willAbort <= willAbort;
    wait until ackSignal = '1';
    writeSignal <= '0';
    wait until ackSignal = '0';
  end procedure;
    
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
  constant QUEUE_SIZE : natural := 63;
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
    adr_i : in std_logic_vector(30 downto 0);
    sel_i : in std_logic_vector(7 downto 0);
    dat_i : in std_logic_vector(63 downto 0);
    dat_o : out std_logic_vector(63 downto 0);
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
    variable cyclePosition : natural;
    variable latencyCounter : natural;
  begin
    wait until areset_n = '1';

    messageEmpty_o <= '1';
    messageAck_o <= '0';
    
    dat_o <= (others=>'U');
    err_o <= '0';
    ack_o <= '0';

    front := 0;
    back := 0;
    cyclePosition := 0;
    latencyCounter := 0;
    
    loop
      wait until clk = '1' or messageWrite_i = '1';
      
      if (clk'event) then
        if (cyc_i = '1') then
          if (front /= back) then
            if (cyclePosition < queue(back).length) then
              TestCompare(stb_i, '1', "stb_i");
              if (queue(back).writeAccess) then
                TestCompare(we_i, '1', "we_i");
              else
                TestCompare(we_i, '0', "we_i");
              end if;
              -- REMARK: Add this...
              --TestCompare(adr_i, std_logic_vector(unsigned(queue(back).address)+cyclePosition), "adr_i");
              TestCompare(sel_i, queue(back).byteSelect, "sel_i");
              if (queue(back).writeAccess) then
                TestCompare(dat_i, queue(back).data(cyclePosition), "dat_i");
              end if;

              if (latencyCounter = queue(back).latency) then
                dat_o <= queue(back).data(cyclePosition);
                ack_o <= '1';
                latencyCounter := 0;
                cyclePosition := cyclePosition + 1;
              else
                dat_o <= (others=>'U');
                ack_o <= '0';
                latencyCounter := latencyCounter + 1;
              end if;
            else
              back := QueueIndexInc(back);
              cyclePosition := 0;
              latencyCounter := 0;
              dat_o <= (others=>'U');
              ack_o <= '0';
            end if;
          else
            TestError("Unexpected access.");
          end if;
        else
          if (cyclePosition /= 0) or (latencyCounter /= 0) then
            TestError("Cycle unexpectedly aborted.");
            cyclePosition := 0;
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


-------------------------------------------------------------------------------
-- TestRioWbBridge.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library std;
use std.textio.all;
use work.rio_common.all;
use work.TestPortPackage.all;


-------------------------------------------------------------------------------
-- Entity for TestRioWbBridge.
-------------------------------------------------------------------------------
entity TestRioWbBridge is
end entity;


-------------------------------------------------------------------------------
-- Architecture for TestRioWbBridge.
-------------------------------------------------------------------------------
architecture TestRioWbBridgeImpl of TestRioWbBridge is
  
  component RioWbBridge is
    generic(
      EXTENDED_ADDRESS : natural range 0 to 2 := 0;
      DEVICE_IDENTITY : std_logic_vector(15 downto 0);
      DEVICE_VENDOR_IDENTITY : std_logic_vector(15 downto 0);
      DEVICE_REV : std_logic_vector(31 downto 0);
      ASSY_IDENTITY : std_logic_vector(15 downto 0);
      ASSY_VENDOR_IDENTITY : std_logic_vector(15 downto 0);
      ASSY_REV : std_logic_vector(15 downto 0));
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      readFrameEmpty_i : in std_logic;
      readFrame_o : out std_logic;
      readContent_o : out std_logic;
      readContentEnd_i : in std_logic;
      readContentData_i : in std_logic_vector(31 downto 0);
      writeFrameFull_i : in std_logic;
      writeFrame_o : out std_logic;
      writeFrameAbort_o : out std_logic;
      writeContent_o : out std_logic;
      writeContentData_o : out std_logic_vector(31 downto 0);

      cyc_o : out std_logic;
      stb_o : out std_logic;
      we_o : out std_logic;
      adr_o : out std_logic_vector(16*EXTENDED_ADDRESS+30 downto 0);
      sel_o : out std_logic_vector(7 downto 0); 
      dat_o : out std_logic_vector(63 downto 0);
      dat_i : in std_logic_vector(63 downto 0);
      err_i : in std_logic;
      ack_i : in std_logic);
  end component;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  
  signal clk : std_logic;
  signal areset_n : std_logic;
  signal enable : std_logic;

  signal writeFrameFull : std_logic;
  signal writeFrame : std_logic;
  signal writeFrameAbort : std_logic;
  signal writeContent : std_logic;
  signal writeContentData : std_logic_vector(31 downto 0);

  signal readFrameEmpty : std_logic;
  signal readFrame : std_logic;
  signal readFrameRestart : std_logic;
  signal readFrameAborted : std_logic;
  signal readContentEmpty : std_logic;
  signal readContent : std_logic;
  signal readContentEnd : std_logic;
  signal readContentData : std_logic_vector(31 downto 0);

  signal wbCyc : std_logic;
  signal wbStb : std_logic;
  signal wbWe : std_logic;
  signal wbAdr : std_logic_vector(30 downto 0);
  signal wbSel : std_logic_vector(7 downto 0);
  signal wbDatWrite : std_logic_vector(63 downto 0);
  signal wbDatRead : std_logic_vector(63 downto 0);
  signal wbAck : std_logic;
  signal wbErr : std_logic;

  signal outboundEmpty : std_logic;
  signal outboundWrite : std_logic;
  signal outboundMessage : TestPortMessagePacketBuffer;
  signal outboundAck : std_logic;
    
  signal inboundEmpty : std_logic;
  signal inboundWrite : std_logic;
  signal inboundMessage : TestPortMessagePacketBuffer;
  signal inboundAck : std_logic;
  
  signal wbMessageEmpty : std_logic;
  signal wbMessageWrite : std_logic;
  signal wbMessage : TestPortMessageWishbone;
  signal wbMessageAck : std_logic;

begin
  
  -----------------------------------------------------------------------------
  -- Clock generation.
  -----------------------------------------------------------------------------
  ClockGenerator: process
  begin
    clk <= '0';
    wait for 20 ns;
    clk <= '1';
    wait for 20 ns;
  end process;


  -----------------------------------------------------------------------------
  -- Serial port emulator.
  -----------------------------------------------------------------------------
  TestDriver: process

    -----------------------------------------------------------------------------
    -- Procedures to handle outbound and inbound packets.
    -----------------------------------------------------------------------------
    procedure OutboundFrame(constant frame : in RioFrame) is
    begin
      TestPortPacketBufferWrite(outboundWrite, outboundMessage, outboundAck,
                                frame, false);
    end procedure;
    
    procedure InboundFrame(constant frame : in RioFrame) is
    begin
      TestPortPacketBufferWrite(inboundWrite, inboundMessage, inboundAck,
                                frame, false);
    end procedure;

    ---------------------------------------------------------------------------
    -- Procedure to handle wishbone accesses.
    ---------------------------------------------------------------------------
    procedure SetSlaveAccess(constant writeAccess : in boolean;
                             constant address : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
                             constant byteSelect : in std_logic_vector(7 downto 0);
                             constant length : in natural range 1 to DATA_SIZE_MAX;
                             constant data : in DoublewordArray(0 to DATA_SIZE_MAX-1);
                             constant latency : natural := 1) is
    begin
      TestPortWishboneWrite(wbMessageWrite, wbMessage, wbMessageAck,
                            writeAccess, address, byteSelect, length, data, latency);
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    function getReadSize(constant rdsize : in std_logic_vector(3 downto 0);
                         constant wdptr : in std_logic) return natural is
    begin
      case rdsize is
        when "0000" | "0001" | "0010" | "0011" =>
          return 1;
        when "0100" | "0110" =>
          return 1;
        when "0101" =>
          return 1;
        when "1000" =>
          return 1;
        when "0111" =>
          return 1;
        when "1001" =>
          return 1;
        when "1010" =>
          return 1;
        when "1011" =>
          if (wdptr = '0') then
            return 1;
          else
            return 2;
          end if;
        when "1100" =>
          if (wdptr = '0') then
            return 4;
          else
            return 8;
          end if;
        when "1101" =>
          if (wdptr = '0') then
            return 12;
          else
            return 16;
          end if;
        when "1110" =>
          if (wdptr = '0') then
            return 20;
          else
            return 24;
          end if;
        when "1111" =>
          if (wdptr = '0') then
            return 28;
          else
            return 32;
          end if;
        when others =>
          return 0;
      end case;
    end function;
    
    function getReadMask(constant rdsize : in std_logic_vector(3 downto 0);
                         constant wdptr : in std_logic) return std_logic_vector is
    begin
      case rdsize is
        when "0000" =>
          if (wdptr = '0') then
            return "10000000";
          else
            return "00001000";
          end if;
        when "0001" =>
          if (wdptr = '0') then
            return "01000000";
          else
            return "00000100";
          end if;
        when "0010" =>
          if (wdptr = '0') then
            return "00100000";
          else
            return "00000010";
          end if;
        when "0011" =>
          if (wdptr = '0') then
            return "00010000";
          else
            return "00000001";
          end if;
        when "0100" =>
          if (wdptr = '0') then
            return "11000000";
          else
            return "00001100";
          end if;
        when "0110" =>
          if (wdptr = '0') then
            return "00110000";
          else
            return "00000011";
          end if;
        when "0101" =>
          if (wdptr = '0') then
            return "11100000";
          else
            return "00000111";
          end if;
        when "1000" =>
          if (wdptr = '0') then
            return "11110000";
          else
            return "00001111";
          end if;
        when "0111" =>
          if (wdptr = '0') then
            return "11111000";
          else
            return "00011111";
          end if;
        when "1001" =>
          if (wdptr = '0') then
            return "11111100";
          else
            return "00111111";
          end if;
        when "1010" =>
          if (wdptr = '0') then
            return "11111110";
          else
            return "01111111";
          end if;
        when others =>
          return "11111111";
      end case;
    end function;
    
    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    variable seed1 : positive := 1;
    variable seed2: positive := 1;

    variable rdsize : std_logic_vector(3 downto 0);
    variable wdptr : std_logic;
    variable ioData : DoubleWordArray(0 to 31);
    variable frame : RioFrame;
    
  begin
    areset_n <= '0';
    enable <= '1';

    inboundWrite <= '0';
    outboundWrite <= '0';
    wbMessageWrite <= '0';
    
    writeFrameFull <= '0';

    wait until clk'event and clk = '1';
    wait until clk'event and clk = '1';
    areset_n <= '1';
    wait until clk'event and clk = '1';
    wait until clk'event and clk = '1';

    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("TG_RioWbBridge");
    PrintS("-----------------------------------------------------------------");
    PrintS("TG_RioWbBridge-TC1");
    PrintS("Description: Test maintenance requests.");
    PrintS("Requirement: XXXXX");
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 1:");
    PrintS("Action: Send maintenance read request for one word on even offset.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioWbBridge-TC1-Step1");
    ---------------------------------------------------------------------------
    
    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("TG_RioWbBridge-TC2");
    PrintS("Description: Test request class packets.");
    PrintS("Requirement: XXXXX");
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 1:");
    PrintS("Action: Send maintenance read request for one word on even offset.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioWbBridge-TC2-Step1");
    ---------------------------------------------------------------------------
    -- REMARK: Change the address also...
    for i in 0 to 15 loop
      for j in 0 to 1 loop
        rdsize := std_logic_vector(to_unsigned(i, 4));
        if (j = 0) then
          wdptr := '0';
        else
          wdptr:= '1';
        end if;
        
        CreateRandomPayload(ioData, seed1, seed2);

        InboundFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                    tt=>"01", ftype=>FTYPE_REQUEST_CLASS, 
                                    sourceId=>x"dead", destId=>x"beef",
                                    payload=>RioNread(rdsize=>rdsize,
                                                      tid=>x"aa",
                                                      address=>"00000000000000000000000000000",
                                                      wdptr=>wdptr,
                                                      xamsbs=>"00")));

        OutboundFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                     tt=>"01", ftype=>FTYPE_RESPONSE_CLASS, 
                                     sourceId=>x"beef", destId=>x"dead",
                                     payload=>RioResponse(status=>"0000",
                                                          tid=>x"aa",
                                                          dataLength=>getReadSize(rdsize, wdptr),
                                                          data=>ioData)));

        SetSlaveAccess(false, "0000000000000000000000000000000",
                       getReadMask(rdsize, wdptr),
                       getReadSize(rdsize, wdptr),
                       ioData);

        TestWait(inboundEmpty, '1', "inbound frame");
        TestWait(outboundEmpty, '1', "outbound frame");
        TestWait(wbMessageEmpty, '1', "wishbone access");
      end loop;
    end loop;
    
    ---------------------------------------------------------------------------
    -- Test completed.
    ---------------------------------------------------------------------------
    
    TestEnd;
  end process;

  -----------------------------------------------------------------------------
  -- Instantiate the test object.
  -----------------------------------------------------------------------------
  TestObject: RioWbBridge
    generic map(
      EXTENDED_ADDRESS=>0,
      DEVICE_IDENTITY=>x"dead",
      DEVICE_VENDOR_IDENTITY=>x"beef",
      DEVICE_REV=>x"c0debabe",
      ASSY_IDENTITY=>x"1111",
      ASSY_VENDOR_IDENTITY=>x"2222",
      ASSY_REV=>x"3333")
    port map(
      clk=>clk, 
      areset_n=>areset_n,
      enable=>enable,
      readFrameEmpty_i=>readFrameEmpty, 
      readFrame_o=>readFrame, 
      readContent_o=>readContent, 
      readContentEnd_i=>readContentEnd, 
      readContentData_i=>readContentData, 
      writeFrameFull_i=>writeFrameFull, 
      writeFrame_o=>writeFrame, 
      writeFrameAbort_o=>writeFrameAbort, 
      writeContent_o=>writeContent, 
      writeContentData_o=>writeContentData, 
      cyc_o=>wbCyc, 
      stb_o=>wbStb, 
      we_o=>wbWe, 
      adr_o=>wbAdr, 
      sel_o=>wbSel, 
      dat_o=>wbDatWrite, 
      dat_i=>wbDatRead, 
      err_i=>wbErr, 
      ack_i=>wbAck);

  -----------------------------------------------------------------------------
  -- Instantiate the test ports.
  -----------------------------------------------------------------------------

  TestPortPacketBufferInst: TestPortPacketBuffer
    port map(
      clk=>clk, areset_n=>areset_n, 
      readEmpty_o=>inboundEmpty, 
      readWrite_i=>inboundWrite, 
      readMessage_i=>inboundMessage, 
      readAck_o=>inboundAck, 
      writeEmpty_o=>outboundEmpty, 
      writeWrite_i=>outboundWrite, 
      writeMessage_i=>outboundMessage, 
      writeAck_o=>outboundAck, 
      readFrameEmpty_o=>readFrameEmpty, 
      readFrame_i=>readFrame, 
      readFrameRestart_i=>'0', 
      readFrameAborted_o=>readFrameAborted,
      readWindowEmpty_o=>open,
      readWindowReset_i=>'0',
      readWindowNext_i=>readFrame,
      readContentEmpty_o=>readContentEmpty, 
      readContent_i=>readContent, 
      readContentEnd_o=>readContentEnd, 
      readContentData_o=>readContentData, 
      writeFrame_i=>writeFrame, 
      writeFrameAbort_i=>writeFrameAbort, 
      writeContent_i=>writeContent, 
      writeContentData_i=>writeContentData);
  
  TestPortWishboneInst: TestPortWishbone
    port map(
      clk=>clk, 
      areset_n=>areset_n, 
      messageEmpty_o=>wbMessageEmpty, 
      messageWrite_i=>wbMessageWrite, 
      message_i=>wbMessage, 
      messageAck_o=>wbMessageAck, 
      cyc_i=>wbCyc, 
      stb_i=>wbStb, 
      we_i=>wbWe, 
      adr_i=>wbAdr, 
      sel_i=>wbSel, 
      dat_i=>wbDatWrite, 
      dat_o=>wbDatRead, 
      err_o=>wbErr, 
      ack_o=>wbAck); 

end architecture;
