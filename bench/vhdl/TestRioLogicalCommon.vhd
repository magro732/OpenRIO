-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Contains automatic test code to verify a RioLogicalCommon implementation.
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


-------------------------------------------------------------------------------
-- TestRioLogicalCommon.
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
-- Entity for TestRioLogicalCommon.
-------------------------------------------------------------------------------
entity TestRioLogicalCommon is
end entity;


-------------------------------------------------------------------------------
-- Architecture for TestRioLogicalCommon.
-------------------------------------------------------------------------------
architecture TestRioLogicalCommonImpl of TestRioLogicalCommon is
  
  signal outboundMessageEmpty : std_logic;
  signal outboundMessageWrite : std_logic;
  signal outboundMessageMessage : TestPortMessagePacketBuffer;
  signal outboundMessageAck : std_logic;
    
  signal inboundMessageEmpty : std_logic;
  signal inboundMessageWrite : std_logic;
  signal inboundMessageMessage : TestPortMessagePacketBuffer;
  signal inboundMessageAck : std_logic;
  
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

  signal inboundStb : std_logic;
  signal inboundAdr : std_logic_vector(3 downto 0);
  signal inboundDat : std_logic_vector(31 downto 0);
  signal inboundStall : std_logic;

  signal outboundStb : std_logic_vector(0 downto 0);
  signal outboundAdr : std_logic_vector(0 downto 0);
  signal outboundDat : std_logic_vector(31 downto 0);
  signal outboundStall : std_logic_vector(0 downto 0);
  
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
    procedure OutboundFrame(constant frame : in RioFrame;
                            constant abort : in boolean := false) is
    begin
      TestPortPacketBufferWrite(outboundMessageWrite, outboundMessageMessage, outboundMessageAck,
                                frame, abort);
    end procedure;
    procedure InboundFrame(constant frame : in RioFrame;
                           constant abort : in boolean := false) is
    begin
      TestPortPacketBufferWrite(inboundMessageWrite, inboundMessageMessage, inboundMessageAck,
                                frame, abort);
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure InboundPayload(constant header : in std_logic_vector(15 downto 0);
                             constant dstId : in std_logic_vector(15 downto 0);
                             constant srcId : in std_logic_vector(15 downto 0);
                             constant payload : in RioPayload) is
      variable adr : std_logic_vector(3 downto 0);
    begin
      wait until clk = '1';
      while (inboundStb = '0') loop
        wait until clk = '1';
      end loop;
      adr := inboundAdr;
      TestCompare(inboundStb, '1', "stb header");
      TestCompare(inboundAdr, header(3 downto 0), "adr");
      TestCompare(inboundDat, x"0000" & header, "header");
      
      wait until clk = '1';
      TestCompare(inboundStb, '1', "stb dstId");
      TestCompare(inboundAdr, adr, "adr dstId");
      TestCompare(inboundDat, x"0000" & dstId, "dstId");
      wait until clk = '1';
      TestCompare(inboundStb, '1', "stb srcId");
      TestCompare(inboundAdr, adr, "adr srcId");
      TestCompare(inboundDat, x"0000" & srcId, "srcId");

      for i in 0 to (payload.length/2)-1 loop
        wait until clk = '1';
        TestCompare(inboundStb, '1', "stb payload");
        TestCompare(inboundAdr, adr, "adr payload");
        TestCompare(inboundDat, payload.data(2*i) & payload.data(2*i+1), "payload");
      end loop;

      if ((payload.length mod 2) = 1) then
        -- Check the last half-word of payload that has CRC appended to it.
        wait until clk = '1';
        TestCompare(inboundStb, '1', "stb payload");
        TestCompare(inboundAdr, adr, "adr payload");
        TestCompare(inboundDat(31 downto 16), payload.data(payload.length-1), "payload");
      else
        if (payload.length >= 38) then
          -- Ignore the last word since it contains only CRC and padding.
          wait until clk = '1';
          TestCompare(inboundStb, '1', "stb crc+pad");
          TestCompare(inboundAdr, adr, "adr crc+pad");
          TestCompare(inboundDat(15 downto 0), x"0000", "crc+pad");
        end if;
      end if;
      
      wait until clk = '1';
      TestCompare(inboundStb, '0', "stb end");
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure OutboundPayload(constant header : in std_logic_vector(15 downto 0);
                              constant dstId : in std_logic_vector(15 downto 0);
                              constant srcId : in std_logic_vector(15 downto 0);
                              constant payload : in RioPayload) is
    begin
      if ((payload.length mod 2) = 1) then
        outboundAdr(0) <= '1';
      else
        outboundAdr(0) <= '0';
      end if;
      
      outboundStb(0) <= '1';
      outboundDat <= "UUUUUUUUUUUUUUUU" & header;
      wait until clk = '1';
      while (outboundStall(0) = '1') loop
        wait until clk = '1';
      end loop;

      outboundDat <= "UUUUUUUUUUUUUUUU" & dstId;
      wait until clk = '1';
      while (outboundStall(0) = '1') loop
        wait until clk = '1';
      end loop;
      
      outboundDat <= "UUUUUUUUUUUUUUUU" & srcId;
      wait until clk = '1';
      while (outboundStall(0) = '1') loop
        wait until clk = '1';
      end loop;

      for i in 0 to (payload.length/2)-1 loop
        outboundDat <= payload.data(2*i) & payload.data(2*i+1);
        wait until clk = '1';
        while (outboundStall(0) = '1') loop
          wait until clk = '1';
        end loop;
      end loop;

      if ((payload.length mod 2) = 1) then
        outboundDat <= payload.data(payload.length-1) & "UUUUUUUUUUUUUUUU";
        wait until clk = '1';
        while (outboundStall(0) = '1') loop
          wait until clk = '1';
        end loop;
      end if;
      
      outboundStb(0) <= '0';
      outboundAdr(0) <= 'U';
      outboundDat <= (others=>'U');
      wait until clk = '1';
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    variable seed1 : positive := 1;
    variable seed2: positive := 1;

    variable frame : RioFrame;
    variable payload : RioPayload;
    
  begin
    areset_n <= '0';
    enable <= '1';
    
    writeFrameFull <= '0';
    inboundStall <= '0';
    outboundStb(0) <= '0';
      
    wait until clk'event and clk = '1';
    wait until clk'event and clk = '1';
    areset_n <= '1';
    wait until clk'event and clk = '1';
    wait until clk'event and clk = '1';

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioLogicalCommon");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioLogicalCommon-TC1");
    TestSpec("Description: Test all sizes of packets in the inbound direction.");
    TestSpec("Requirement: ");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Add inbound packets in all allowed sized.");
    TestSpec("Result: The payload of the inbound packets should be received on ");
    TestSpec("        the other side without CRC.");
    TestSpec("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioLogicalCommon-TC1-Step1");
    ---------------------------------------------------------------------------
    -- REMARK: Use random data...
    for j in 1 to 133 loop
      payload.length := j;
      for i in 0 to payload.length-1 loop
        payload.data(i) := std_logic_vector(to_unsigned(i, 16));
      end loop;
      frame := RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                              tt=>"01", ftype=>FTYPE_WRITE_CLASS,
                              destId=>x"beef", sourceId=>x"dead", 
                              payload=>payload);
      InboundFrame(frame);
    end loop;

    for j in 1 to 133 loop
      payload.length := j;
      InboundPayload(x"0015", x"beef", x"dead", payload);
    end loop;

    TestWait(inboundMessageEmpty, '1', "inboundMessage empty");
    
    ---------------------------------------------------------------------------
    --TestSpec("-----------------------------------------------------------------");
    --TestSpec("Step 2:");
    --TestSpec("Action: Send an inbound frame that are too long.");
    --TestSpec("Result: The tail of the packet should be discarded.");
    --TestSpec("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    --TestCaseStart("TG_RioLogicalCommon-TC1-Step2");
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioLogicalCommon-TC2");
    TestSpec("Description: Test all sizes of packets in the outbound direction.");
    TestSpec("Requirement: ");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Add outbound packets in all allowed sized.");
    TestSpec("Result: The payload of the outbound packets should be received on ");
    TestSpec("        the other side with CRC added.");
    TestSpec("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioLogicalCommon-TC2-Step1");
    ---------------------------------------------------------------------------

    for j in 1 to 133 loop
      payload.length := j;
      for i in 0 to payload.length-1 loop
        payload.data(i) := std_logic_vector(to_unsigned(i, 16));
      end loop;
      frame := RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                              tt=>"01", ftype=>FTYPE_WRITE_CLASS,
                              destId=>x"beef", sourceId=>x"dead", 
                              payload=>payload);
      OutboundFrame(frame);
    end loop;
    
    for j in 1 to 133 loop
      payload.length := j;
      OutboundPayload(x"0015", x"beef", x"dead", payload);
    end loop;

    TestWait(outboundMessageEmpty, '1', "outboundMessage empty");
    
    -----------------------------------------------------------------------------
    --TestSpec("-----------------------------------------------------------------");
    --TestSpec("Step 2:");
    --TestSpec("Action: Send an outbound frame that are too long.");
    --TestSpec("Result: The tail of the packet should be discarded.");
    --TestSpec("-----------------------------------------------------------------");
    -----------------------------------------------------------------------------
    --TestCaseStart("TG_RioLogicalCommon-TC1-Step2");
    -----------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- Test completed.
    ---------------------------------------------------------------------------
    
    TestEnd;
  end process;

  -----------------------------------------------------------------------------
  -- Instantiate the test port.
  -----------------------------------------------------------------------------

  TestPortPacketBufferInst: TestPortPacketBuffer
    generic map(READ_CONTENT_END_DATA_VALID=>false)
    port map(
      clk=>clk, areset_n=>areset_n, 
      readEmpty_o=>inboundMessageEmpty, 
      readWrite_i=>inboundMessageWrite, 
      readMessage_i=>inboundMessageMessage, 
      readAck_o=>inboundMessageAck, 
      writeEmpty_o=>outboundMessageEmpty, 
      writeWrite_i=>outboundMessageWrite, 
      writeMessage_i=>outboundMessageMessage, 
      writeAck_o=>outboundMessageAck, 
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
  
  -----------------------------------------------------------------------------
  -- Instantiate the test object.
  -----------------------------------------------------------------------------

  TestObject: RioLogicalCommon
    generic map(PORTS=>1)
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
      inboundStb_o=>inboundStb,
      inboundAdr_o=>inboundAdr,
      inboundDat_o=>inboundDat,
      inboundStall_i=>inboundStall,
      outboundStb_i=>outboundStb,
      outboundAdr_i=>outboundAdr,
      outboundDat_i=>outboundDat,
      outboundStall_o=>outboundStall);
  
end architecture;
