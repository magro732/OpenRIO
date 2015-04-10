-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Contains automatic simulation test code to verify a RioSerial implementation.
-- 
-- To Do:
-- - Replace TestSwitchPort with generic TestPortPacketBuffer from common library.
-- - Move TestSymbolPort to generic library.
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
-- REMARK: Add testcase to check that no packets are sent when the linkpartner
-- has no buffers left.


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use work.rio_common.all;
use work.TestPortPackage.all;

package TestRioSerialPackage is
  type MessageSymbol is record
    symbolType : std_logic_vector(1 downto 0);
    symbolContent : std_logic_vector(31 downto 0);
    ignoreIdle : boolean;
  end record;
  type MessageFrame is record
    frame : RioFrame;
    willAbort : boolean;
  end record;
end package;

package body TestRioSerialPackage is

end package body;



-------------------------------------------------------------------------------
-- TestRioSerial.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
use work.rio_common.all;
use work.TestRioSerialPackage.all;
use work.TestPortPackage.all;


-------------------------------------------------------------------------------
-- Entity for TestRioSerial.
-------------------------------------------------------------------------------
entity TestRioSerial is
end entity;


-------------------------------------------------------------------------------
-- Architecture for TestUart.
-------------------------------------------------------------------------------
architecture TestRioSerialImpl of TestRioSerial is
  
  component TestSwitchPort is
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      outboundWriteEmpty_o : out std_logic;
      outboundWrite_i : in std_logic;
      outboundWriteMessage_i : in MessageFrame;
      outboundWriteAck_o : out std_logic;
      
      inboundWriteEmpty_o : out std_logic;
      inboundWrite_i : in std_logic;
      inboundWriteMessage_i : in MessageFrame;
      inboundWriteAck_o : out std_logic;
      
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

  component RioSerial is
    generic(
      TIMEOUT_WIDTH : natural;
      SYMBOL_COUNTER_WIDTH : natural := 8;
      TICKS_SEND_STATUS_STARTUP : natural := 15;
      TICKS_SEND_STATUS_OPERATIONAL : natural := 255);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      portLinkTimeout_i : in std_logic_vector(TIMEOUT_WIDTH-1 downto 0);
      linkInitialized_o : out std_logic;
      inputPortEnable_i : in std_logic;
      outputPortEnable_i : in std_logic;
      
      localAckIdWrite_i : in std_logic;
      clrOutstandingAckId_i : in std_logic;
      inboundAckId_i : in std_logic_vector(4 downto 0);
      outstandingAckId_i : in std_logic_vector(4 downto 0);
      outboundAckId_i : in std_logic_vector(4 downto 0);
      inboundAckId_o : out std_logic_vector(4 downto 0);
      outstandingAckId_o : out std_logic_vector(4 downto 0);
      outboundAckId_o : out std_logic_vector(4 downto 0);
      
      readFrameEmpty_i : in std_logic;
      readFrame_o : out std_logic;
      readFrameRestart_o : out std_logic;
      readFrameAborted_i : in std_logic;
      readWindowEmpty_i : in std_logic;
      readWindowReset_o : out std_logic;
      readWindowNext_o : out std_logic;
      readContentEmpty_i : in std_logic;
      readContent_o : out std_logic;
      readContentEnd_i : in std_logic;
      readContentData_i : in std_logic_vector(31 downto 0);

      writeFrameFull_i : in std_logic;
      writeFrame_o : out std_logic;
      writeFrameAbort_o : out std_logic;
      writeContent_o : out std_logic;
      writeContentData_o : out std_logic_vector(31 downto 0);

      portInitialized_i : in std_logic;
      outboundControlValid_o : out std_logic;
      outboundControlSymbol_o : out std_logic_vector(23 downto 0);
      outboundDataValid_o : out std_logic;
      outboundDataSymbol_o : out std_logic_vector(31 downto 0);
      inboundControlValid_i : in std_logic;
      inboundControlSymbol_i : in std_logic_vector(23 downto 0);
      inboundDataValid_i : in std_logic;
      inboundDataSymbol_i : in std_logic_vector(31 downto 0));
  end component;

  component TestSymbolPort is
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      portInitialized_i : in std_logic;
      outboundControlValid_i : in std_logic;
      outboundControlSymbol_i : in std_logic_vector(23 downto 0);
      outboundDataValid_i : in std_logic;
      outboundDataSymbol_i : in std_logic_vector(31 downto 0);
      inboundControlValid_o : out std_logic;
      inboundControlSymbol_o : out std_logic_vector(23 downto 0);
      inboundDataValid_o : out std_logic;
      inboundDataSymbol_o : out std_logic_vector(31 downto 0);

      outboundWriteEmpty_o : out std_logic;
      outboundWrite_i : in std_logic;
      outboundWriteMessage_i : in MessageSymbol;
      outboundWriteAck_o : out std_logic;
      inboundWriteEmpty_o : out std_logic;
      inboundWrite_i : in std_logic;
      inboundWriteMessage_i : in MessageSymbol;
      inboundWriteAck_o : out std_logic);
  end component;

  signal clk : std_logic;
  signal areset_n : std_logic;
  signal enable : std_logic;

  signal portLinkTimeout : std_logic_vector(10 downto 0);
  signal linkInitialized, linkInitializedExpected : std_logic;
  signal inputPortEnable : std_logic;
  signal outputPortEnable : std_logic;
  
  signal localAckIdWrite : std_logic;
  signal clrOutstandingAckId : std_logic;
  signal inboundAckIdWrite : std_logic_vector(4 downto 0);
  signal outstandingAckIdWrite : std_logic_vector(4 downto 0);
  signal outboundAckIdWrite : std_logic_vector(4 downto 0);
  signal inboundAckIdRead : std_logic_vector(4 downto 0);
  signal outstandingAckIdRead : std_logic_vector(4 downto 0);
  signal outboundAckIdRead : std_logic_vector(4 downto 0);
  
  signal readFrameEmpty : std_logic;
  signal readFrame : std_logic;
  signal readFrameRestart : std_logic;
  signal readFrameAborted : std_logic;
  signal readWindowEmpty : std_logic;
  signal readWindowReset : std_logic;
  signal readWindowNext : std_logic;
  signal readContentEmpty : std_logic;
  signal readContent : std_logic;
  signal readContentEnd : std_logic;
  signal readContentData : std_logic_vector(31 downto 0);

  signal writeFrame : std_logic;
  signal writeFrameAbort : std_logic;
  signal writeContent : std_logic;
  signal writeContentData : std_logic_vector(31 downto 0);

  signal portInitialized : std_logic;
  signal outboundControlValid : std_logic;
  signal outboundControlSymbol : std_logic_vector(23 downto 0);
  signal outboundDataValid : std_logic;
  signal outboundDataSymbol : std_logic_vector(31 downto 0);
  signal inboundControlValid : std_logic;
  signal inboundControlSymbol : std_logic_vector(23 downto 0);
  signal inboundDataValid : std_logic;
  signal inboundDataSymbol : std_logic_vector(31 downto 0);

  signal outboundFrameWriteEmpty : std_logic;
  signal outboundFrameWrite : std_logic;
  signal outboundFrameWriteMessage : MessageFrame;
  signal outboundFrameWriteAck : std_logic;
  signal inboundFrameWriteEmpty : std_logic;
  signal inboundFrameWrite : std_logic;
  signal inboundFrameWriteMessage : MessageFrame;
  signal inboundFrameWriteAck : std_logic;
  signal inboundFrameFull : std_logic;

  signal outboundSymbolWriteEmpty : std_logic;
  signal outboundSymbolWrite : std_logic;
  signal outboundSymbolWriteMessage : MessageSymbol;
  signal outboundSymbolWriteAck : std_logic;
  signal inboundSymbolWriteEmpty : std_logic;
  signal inboundSymbolWrite : std_logic;
  signal inboundSymbolWriteMessage : MessageSymbol;
  signal inboundSymbolWriteAck : std_logic;

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
  -- Serial protocol test driver.
  -----------------------------------------------------------------------------
  TestDriver: process

    ---------------------------------------------------------------------------
    -- Procedures to receive symbols.
    ---------------------------------------------------------------------------
    procedure OutboundSymbolIdle is
    begin
      outboundSymbolWrite <= '1';
      outboundSymbolWriteMessage.symbolType <= "00";
      outboundSymbolWriteMessage.symbolContent <= (others=>'U');
      outboundSymbolWriteMessage.ignoreIdle <= false;
      wait until outboundSymbolWriteAck = '1';
      outboundSymbolWrite <= '0';
      wait until outboundSymbolWriteAck = '0';
    end procedure;

    procedure OutboundSymbolControl(constant symbolContent : in std_logic_vector(23 downto 0);
                                    constant ignoreIdle : in boolean := false) is
    begin
      outboundSymbolWrite <= '1';
      outboundSymbolWriteMessage.symbolType <= "01";
      outboundSymbolWriteMessage.symbolContent <= x"00" & symbolContent;
      outboundSymbolWriteMessage.ignoreIdle <= ignoreIdle;
      wait until outboundSymbolWriteAck = '1';
      outboundSymbolWrite <= '0';
      wait until outboundSymbolWriteAck = '0';
    end procedure;

    procedure OutboundSymbolData(constant symbolContent : in std_logic_vector(31 downto 0);
                                 constant ignoreIdle : in boolean := false) is
    begin
      outboundSymbolWrite <= '1';
      outboundSymbolWriteMessage.symbolType <= "10";
      outboundSymbolWriteMessage.symbolContent <= symbolContent;
      outboundSymbolWriteMessage.ignoreIdle <= ignoreIdle;
      wait until outboundSymbolWriteAck = '1';
      outboundSymbolWrite <= '0';
      wait until outboundSymbolWriteAck = '0';
    end procedure;

    ---------------------------------------------------------------------------
    -- Procedures to send symbols.
    ---------------------------------------------------------------------------
    procedure InboundSymbolIdle is
    begin
      inboundSymbolWrite <= '1';
      inboundSymbolWriteMessage.symbolType <= "00";
      inboundSymbolWriteMessage.symbolContent <= (others=>'U');
      inboundSymbolWriteMessage.ignoreIdle <= false;
      wait until inboundSymbolWriteAck = '1';
      inboundSymbolWrite <= '0';
      wait until inboundSymbolWriteAck = '0';
    end procedure;

    procedure InboundSymbolControl(constant symbolContent : in std_logic_vector(23 downto 0);
                                   constant ignoreIdle : in boolean := false) is
    begin
      inboundSymbolWrite <= '1';
      inboundSymbolWriteMessage.symbolType <= "01";
      inboundSymbolWriteMessage.symbolContent <= x"00" & symbolContent;
      inboundSymbolWriteMessage.ignoreIdle <= ignoreIdle;
      wait until inboundSymbolWriteAck = '1';
      inboundSymbolWrite <= '0';
      wait until inboundSymbolWriteAck = '0';
    end procedure;

    procedure InboundSymbolData(constant symbolContent : in std_logic_vector(31 downto 0);
                                constant ignoreIdle : in boolean := false) is
    begin
      inboundSymbolWrite <= '1';
      inboundSymbolWriteMessage.symbolType <= "10";
      inboundSymbolWriteMessage.symbolContent <= symbolContent;
      inboundSymbolWriteMessage.ignoreIdle <= ignoreIdle;
      wait until inboundSymbolWriteAck = '1';
      inboundSymbolWrite <= '0';
      wait until inboundSymbolWriteAck = '0';
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    
    procedure OutboundFrame(constant frame : in RioFrame;
                            constant willAbort : boolean := false) is
    begin
      outboundFrameWrite <= '1';
      outboundFrameWriteMessage.frame <= frame;
      outboundFrameWriteMessage.willAbort <= willAbort;
      wait until outboundFrameWriteAck = '1';
      outboundFrameWrite <= '0';
      wait until outboundFrameWriteAck = '0';
    end procedure;
    
    procedure InboundFrame(constant frame : in RioFrame;
                           constant willAbort : boolean := false) is
    begin
      inboundFrameWrite <= '1';
      inboundFrameWriteMessage.frame <= frame;
      inboundFrameWriteMessage.willAbort <= willAbort;
      wait until inboundFrameWriteAck = '1';
      inboundFrameWrite <= '0';
      wait until inboundFrameWriteAck = '0';
    end procedure;

    ---------------------------------------------------------------------------
    -- Process variables.
    ---------------------------------------------------------------------------
    variable seed1 : positive := 1;
    variable seed2 : positive := 1;
    variable payload : RioPayload;
    
    variable frame : RioFrame;
    variable frameOutbound : RioFrame;
    variable frameInbound : RioFrame;

  begin
    ---------------------------------------------------------------------------
    -- Test case initialization.
    ---------------------------------------------------------------------------

    portLinkTimeout <= (others=>'1');
    inputPortEnable <= '1';
    outputPortEnable <= '1';
    
    portInitialized <= '0';

    localAckIdWrite <= '0';
    clrOutstandingAckId <= '0';
    inboundAckIdWrite <= (others=>'0');
    outstandingAckIdWrite <= (others=>'0');
    outboundAckIdWrite <= (others=>'0');

    enable <= '1';

    linkInitializedExpected <= '0';
    
    outboundSymbolWrite <= '0';
    inboundSymbolWrite <= '0';

    outboundFrameWrite <= '0';
    inboundFrameWrite <= '0';
    inboundFrameFull <= '0';
      
    -- Generate a startup reset pulse.
    areset_n <= '0';
    wait until clk'event and clk = '1';
    wait until clk'event and clk = '1';
    areset_n <= '1';
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioSerial");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioSerial-TC1");
    TestSpec("Description: Test idle-sequence transmission at startup.");
    TestSpec("Requirement: XXXXX");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Read transmission port.");
    TestSpec("Result: Idle sequence symbols should be read.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC1-Step1");
    ---------------------------------------------------------------------------

    -- Make sure only idle-sequences are transmitted at startup.
    for i in 0 to 512 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    
    for i in 0 to 512 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioSerial-TC2");
    TestSpec("Description: Test idle-sequence and status symbol transmission");
    TestSpec("             when the port has been initialized.");
    TestSpec("Requirement: XXXXX");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Set port initialized and read transmission port.");
    TestSpec("Result: Idle sequence and status symbols should be read.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC2-Step1");
    ---------------------------------------------------------------------------

    -- Initialize the port to trigger a change of state.
    portInitialized <= '1';
    
    -- The transmitter should send idle sequences at startup and a status once
    -- in a while.
    for i in 0 to 17 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                 STYPE1_NOP, "000"));
    InboundSymbolIdle;

    for i in 0 to 16 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                 STYPE1_NOP, "000"));
    InboundSymbolIdle;

    for i in 0 to 16 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                 STYPE1_NOP, "000"));
    InboundSymbolIdle;
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 2:");
    TestSpec("Action: Toggle port initialized pin and check that no status ");
    TestSpec("        symbols are transmitted when uninitialized.");
    TestSpec("Result: Only idle sequences should be read when uninitialized.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC2-Step2");
    ---------------------------------------------------------------------------

    -- Deassert the port initialized flag.
    portInitialized <= '0';
    
    -- Make sure only idle-sequences are transmitted at startup.
    for i in 0 to 512 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    wait until outboundSymbolWriteEmpty = '1';
    for i in 0 to 512 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");

    -- Initialize the port to trigger a change of state.
    portInitialized <= '1';
    
    -- The transmitter should send idle sequences at startup and a status once
    -- in a while.
    for i in 0 to 17 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                 STYPE1_NOP, "000"));
    InboundSymbolIdle;

    for i in 0 to 16 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                 STYPE1_NOP, "000"));
    InboundSymbolIdle;

    for i in 0 to 16 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                 STYPE1_NOP, "000"));
    InboundSymbolIdle;
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 3:");
    TestSpec("Action: Send one error free status symbol to trigger the ");
    TestSpec("        transmission of status symbols with a higher frequency.");
    TestSpec("Result: Idle sequence and status symbols should be read but ");
    TestSpec("        status symbols should be recived more often.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC2-Step3");
    ---------------------------------------------------------------------------

    -- A received error-free status triggers transmission of status symbols in
    -- a more rapid past.
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 15 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;

    -- The transmitter should send at least 15 additional statuses after
    -- receiving an error free status.
    for j in 0 to 14 loop      
      OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                   STYPE1_NOP, "000"));
      InboundSymbolIdle;

      for i in 0 to 16 loop
        OutboundSymbolIdle;
        InboundSymbolIdle;
      end loop;
    end loop;

    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                 STYPE1_NOP, "000"));
    InboundSymbolIdle;

    OutboundSymbolIdle;
    InboundSymbolIdle;
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 4:");
    TestSpec("Action: Send one errornous status symbol to restart the status ");
    TestSpec("        counting.");
    TestSpec("Result: Idle sequence and status symbols should be read but ");
    TestSpec("        status symbols should still be received more often.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC2-Step4");
    ---------------------------------------------------------------------------
    
    -- REMARK: Add this...
    TestCaseStart("Not implemented.");
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 5:");
    TestSpec("Action: Send seven additional status symbols.");
    TestSpec("Result: The link should become fully initialized.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC2-Step5");
    ---------------------------------------------------------------------------

    -- Make the link fully initialized by sending 7 additional statuses.
    for i in 0 to 6 loop
      OutboundSymbolIdle;
      InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                  STYPE1_NOP, "000"));
    end loop;

    -- Compensate for some ticks before the link becomes initialized.
    for i in 0 to 3 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");

    linkInitializedExpected <= '1';

    -- Receive a status-control-symbol.
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                 STYPE1_NOP, "000"));
    InboundSymbolIdle;
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioSerial-TC3");
    TestSpec("Description: Test port reception.");
    TestSpec("Requirement: XXXXX");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Send an inbound frame with pad after the CRC.");
    TestSpec("Result: The frame should end up in a frame buffer.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC3-Step1");
    ---------------------------------------------------------------------------

    -- Create the frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 1;
    frame := RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame);
    
    -- Fill in the symbols that the packet will be fragmented into.
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00000", "11111",
                                                 STYPE1_NOP, "000"), true);
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 2:");
    TestSpec("Action: Send an inbound frame without a pad after the CRC.");
    TestSpec("Result: The frame should end up in a frame buffer.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC3-Step2");
    ---------------------------------------------------------------------------
    
    -- Create the frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 2;
    frame := RioFrameCreate(ackId=>"00001", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame);
    
    -- Fill in the symbols that the packet will be fragmented into.
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00001", "11111",
                                                 STYPE1_NOP, "000"), true);

    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 3:");
    TestSpec("Action: Send an inbound frame with maximum size.");
    TestSpec("Result: The frame should end up in a frame buffer.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC3-Step3");
    ---------------------------------------------------------------------------
    
    -- Create the frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 133;
    frame := RioFrameCreate(ackId=>"00010", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame);
    
    -- Fill in the symbols that the packet will be fragmented into.
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00010", "11111",
                                                 STYPE1_NOP, "000"), true);
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 4:");
    TestSpec("Action: Send two packets without end-of-packet in between.");
    TestSpec("Result: Both packets should be accepted.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC3-Step4");
    ---------------------------------------------------------------------------
    
    -- Create the first frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 10;
    frame := RioFrameCreate(ackId=>"00011", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));

    -- Create the second frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 13;
    frame := RioFrameCreate(ackId=>"00100", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame);
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Add expected packet-accepted symbols.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00011", "11111",
                                                 STYPE1_NOP, "000"), true);
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00100", "11111",
                                                 STYPE1_NOP, "000"), true);

    -- Wait for all symbols to be transfered and check that the packet was received.
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 5:");
    TestSpec("Action: Start to send a packet. Abort it with stomp. Then send ");
    TestSpec("        another packet.");
    TestSpec("Result: The first packet should be discarded and the second should");
    TestSpec("        be accepted. The retried packet should be acknowledged.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC3-Step5");
    ---------------------------------------------------------------------------
    
    -- Create a frame, send it and abort it with STOMP.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 7;
    frame := RioFrameCreate(ackId=>"00101", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame, true);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_STOMP, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_RESTART_FROM_RETRY, "000"));

    -- Create a new frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 8;
    frame := RioFrameCreate(ackId=>"00101", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive acknowledge of the stomped packet.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_RETRY, "00101", "11111",
                                                 STYPE1_NOP, "000"), true);
    
    -- Receive acknowledge for the transmitted frame.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00101", "11111",
                                                 STYPE1_NOP, "000"), true);
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 6:");
    TestSpec("Action: Start to send a packet but dont send any payload. Abort it");
    TestSpec("        with stomp. Then send another packet.");
    TestSpec("Result: The first packet should be discarded and the second should");
    TestSpec("        be accepted. The retried packet should be acknowledged.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC3-Step6");
    ---------------------------------------------------------------------------
    
    -- Start the reception of a frame and abort it.
    frame.length := 0;
    InboundFrame(frame, true);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_STOMP, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive acknowledge for the transmitted frame.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_RETRY, "00110", "11111",
                                                 STYPE1_NOP, "000"), true);

    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    
    -- Acknowledge the canceled packet.
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_RESTART_FROM_RETRY, "000"));

    -- Create a new frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 8;
    frame := RioFrameCreate(ackId=>"00110", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive acknowledge for the transmitted frame.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00110", "11111",
                                                 STYPE1_NOP, "000"), true);

    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 7:");
    TestSpec("Action: Start to send a packet with payload, then send a ");
    TestSpec("        link-request. Then send another packet.");
    TestSpec("Result: The first packet should be canceled without any ");
    TestSpec("        confirmation and a link-response should be returned. The");
    TestSpec("        second packet should be accepted.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC3-Step7");
    ---------------------------------------------------------------------------
    
    -- Create a new frame and abort it with a link-request/input-status to
    -- abort the current packet.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 9;
    frame := RioFrameCreate(ackId=>"00111", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame, true);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_LINK_REQUEST, "100"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive link-response indicating normal operation and expected ackId.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "00111", "10000",
                                                 STYPE1_NOP, "000"), true);

    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");

    -- Create a new frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 10;
    frame := RioFrameCreate(ackId=>"00111", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive acknowledge for the transmitted frame.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00111", "11111",
                                                 STYPE1_NOP, "000"), true);

    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 8:");
    TestSpec("Action: Start to send a packet, no payload, then send a ");
    TestSpec("        link-request. Then send another packet.");
    TestSpec("Result: The first packet should be canceled without any ");
    TestSpec("        confirmation and a link-response should be returned. The");
    TestSpec("        second packet should be accepted.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC3-Step8");
    ---------------------------------------------------------------------------

    -- Start a frame then send link-request/input-status to abort it.
    frame.length := 0;
    InboundFrame(frame, true);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_LINK_REQUEST, "100"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive link-response indicating normal operation and expected ackId.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "01000", "10000",
                                                 STYPE1_NOP, "000"), true);

    -- Create a new frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 11;
    frame := RioFrameCreate(ackId=>"01000", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive acknowledge for the transmitted frame.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "01000", "11111",
                                                 STYPE1_NOP, "000"), true);

    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 9:");
    TestSpec("Action: Send a packet when no buffers are available. Reset receiver");
    TestSpec("        with link-request.");
    TestSpec("Result: A packet-retry should be transmitted and receiver should");
    TestSpec("        enter input-retry-stopped.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC3-Step9");
    ---------------------------------------------------------------------------

    -- Indicate the inbound frame queue is full.
    inboundFrameFull <= '1';
    
    -- Create a new frame and start sending it.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 11;
    frame := RioFrameCreate(ackId=>"01001", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    InboundSymbolData(frame.payload(0));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive notification about that the packet needs to be retried.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_RETRY, "01001", "11111",
                                                 STYPE1_NOP, "000"), true);

    -- Check the status of the input port and verify the input-retry-stopped state.
    -- This should also set the receiver into normal operation.
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_LINK_REQUEST, "100"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "01001", "00100",
                                                 STYPE1_NOP, "000"), true);

    -- Check the status of the input port and verify the input-retry-stopped state.
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_LINK_REQUEST, "100"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "01001", "10000",
                                                 STYPE1_NOP, "000"), true);

    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");

    -- Indicate the inbound frame queue is ready to accept new packets again.
    inboundFrameFull <= '0';
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 10:");
    TestSpec("Action: Send a packet when no buffers is available. Reset receiver");
    TestSpec("        with restart-from-retry.");
    TestSpec("Result: A packet-retry should be transmitted and receiver should");
    TestSpec("        enter input-retry-stopped.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC3-Step10");
    ---------------------------------------------------------------------------

    -- Indicate the inbound frame queue is full.
    inboundFrameFull <= '1';
    
    -- Create a new frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 11;
    frame := RioFrameCreate(ackId=>"01001", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    InboundSymbolData(frame.payload(0));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive notification about that the packet needs to be retried.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_RETRY, "01001", "11111",
                                                 STYPE1_NOP, "000"), true);

    -- Acknowledge the retried packet.
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_RESTART_FROM_RETRY, "000"));

    -- Request the status of the input port.
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_LINK_REQUEST, "100"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Verify the input-retry-stopped state.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "01001", "10000",
                                                 STYPE1_NOP, "000"), true);

    -- Always receive a status after a link response when leaving input-error-stopped.
--    OutboundSymbol(SYMBOL_CONTROL,
--                  RioControlSymbolCreate(STYPE0_STATUS, "01001", "11111",
--                                         STYPE1_NOP, "000"));    
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");
    
    -- Indicate the inbound frame queue is ready to accept new packets again.
    inboundFrameFull <= '0';

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 11:");
    TestSpec("Action: Start a new packet when in input-retry-stopped state.");
    TestSpec("Result: The packet should be discarded.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC3-Step11");
    ---------------------------------------------------------------------------

    -- Indicate the inbound frame queue is full.
    inboundFrameFull <= '1';
    
    -- Create a new frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 11;
    frame := RioFrameCreate(ackId=>"01001", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    
    -- Start the reception of a frame.
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    InboundSymbolData(frame.payload(0));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive notification about that the packet needs to be retried.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_RETRY, "01001", "11111",
                                                 STYPE1_NOP, "000"), true);
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");

    -- Create a packet and send it. It should be silently discarded.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 12;
    frame := RioFrameCreate(ackId=>"01001", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      OutboundSymbolIdle;
      InboundSymbolData(frame.payload(i));
    end loop;
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    
    -- Acknowledge the retried packet.
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_RESTART_FROM_RETRY, "000"));
    for i in 0 to 6 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");

    -- Indicate the inbound frame queue is ready to accept new packets again.
    inboundFrameFull <= '0';

    -- Create a packet and send it.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 13;
    frame := RioFrameCreate(ackId=>"01001", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive acknowledge for the transmitted frame.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "01001", "11111",
                                                 STYPE1_NOP, "000"), true);

    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 12:");
    TestSpec("Action: Send an erronous control-symbol. Then restore with");
    TestSpec("        link-request.");
    TestSpec("Result: Receiver should enter input-error-stopped and return to");
    TestSpec("        normal operation after the link-request was receiver.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC3-Step12");
    ---------------------------------------------------------------------------

    -- Create, corrupt and send a control symbol.
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000") xor x"100000");
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive a packet-not-accepted indicating error in control-symbol crc.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_NOT_ACCEPTED, "00000", "00010",
                                                 STYPE1_NOP, "000"), true);

    -- Create a packet and send it. It should be discarded.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 14;
    frame := RioFrameCreate(ackId=>"01010", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      OutboundSymbolIdle;
      InboundSymbolData(frame.payload(i));
    end loop;
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    
    -- Make the receiver go back to normal operation by sending a link-request.
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_LINK_REQUEST, "100"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "01010", "00101",
                                                 STYPE1_NOP, "000"), true);
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");

    -- Always receive a status after a link response when leaving input-error-stopped.
--    OutboundSymbol(SYMBOL_CONTROL,
--                  RioControlSymbolCreate(STYPE0_STATUS, "01010", "11111",
--                                         STYPE1_NOP, "000"));
    
    -- Create a packet and send it.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 15;
    frame := RioFrameCreate(ackId=>"01010", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive acknowledge for the transmitted frame.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "01010", "11111",
                                                 STYPE1_NOP, "000"), true);

    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 13:");
    TestSpec("Action: Send an erronous packet. Then restore with link-request.");
    TestSpec("Result: Receiver should enter input-error-stopped and return to");
    TestSpec("        normal operation after the link-request was receiver.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC3-Step13");
    ---------------------------------------------------------------------------

    -- Create a packet and send it with a bit error. It should be discarded.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 15;
    frame := RioFrameCreate(ackId=>"01011", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    frame.payload(0) := frame.payload(0) xor x"00000010";
    InboundFrame(frame, true);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;
    
    -- Receive a packet-not-accepted indicating error in control-symbol crc.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_NOT_ACCEPTED, "00000", "00100",
                                                 STYPE1_NOP, "000"), true);

    -- Make the receiver go back to normal operation by sending a link-request.
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_LINK_REQUEST, "100"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "01011", "00101",
                                                 STYPE1_NOP, "000"), true);

    -- Send a new frame without error.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 16;
    frame := RioFrameCreate(ackId=>"01011", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frame);
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));
    for i in 0 to frame.length-1 loop
      InboundSymbolData(frame.payload(i));
    end loop;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    for i in 0 to 6 loop
      InboundSymbolIdle;
    end loop;

    -- Receive acknowledge for the transmitted frame.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "01011", "11111",
                                                 STYPE1_NOP, "000"), true);

    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty.");
    TestCompare(inboundFrameWriteEmpty, '1', "Packet was received.");

    -- REMARK: Complete with some more error situations: invalid ackId, too
    -- short packet, too long packet, etc...
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioSerial-TC4");
    TestSpec("Description: Test port transmission.");
    TestSpec("Requirement: XXXXX");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Send an outbound frame.");
    TestSpec("Result: The frame should be read from the frame buffer and ");
    TestSpec("        received as symbols.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC4-Step1");
    ---------------------------------------------------------------------------
    
    -- Create the frame.
    -- Make sure the transmitter fills in the correct ackId and dont use the
    -- one in the input packet.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 3;
    frame := RioFrameCreate(ackId=>"UUUUU", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundFrame(frame);
    frame.payload(0)(31 downto 27) := "00000";

    -- Receive the frame.
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolIdle;

    -- Send acknowledge that the frame was received.
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00000", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    TestWait(outboundSymbolWriteEmpty, '1', "Outbound symbol empty");
    TestCompare(outboundFrameWriteEmpty, '1', "packet transmitted");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 2:");
    TestSpec("Action: Send one more outbound packets than there are ackIds.");
    TestSpec("Result: The packets should be fragmented and received in symbols.");
    TestSpec("        The last packet should be delayed and sent once the first");
    TestSpec("        packet has been accepted.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC4-Step2");
    ---------------------------------------------------------------------------
    -- REMARK: 32 packet should ideally be supported, not just 31...fix.
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    for i in 1 to 31 loop
      -- Create a frame.
      CreateRandomPayload(payload.data, seed1, seed2);
      payload.length := 3+i;
      frame := RioFrameCreate(ackId=>"UUUUU", vc=>'0', crf=>'0', prio=>"00",
                              tt=>"01", ftype=>"0000",
                              sourceId=>x"0000", destId=>x"0000",
                              payload=>payload);
      OutboundFrame(frame);
      frame.payload(0)(31 downto 27) := std_logic_vector(to_unsigned(i mod 32, 5));

      -- Receive the frame.
      OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                   STYPE1_START_OF_PACKET, "000"));
      InboundSymbolIdle;
      for i in 0 to frame.length-1 loop
        OutboundSymbolData(frame.payload(i));
        InboundSymbolIdle;
      end loop;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolIdle;
    
    -- Create a frame that cannot be sent since there are no available ackids left.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 3+32;
    frame := RioFrameCreate(ackId=>"UUUUU", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundFrame(frame);
    frame.payload(0)(31 downto 27) := std_logic_vector(to_unsigned(0, 5));
    
    -- Send acknowledge that the frames was received.
    OutboundSymbolIdle;
    InboundSymbolControl(
      RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, std_logic_vector(to_unsigned(1, 5)), "11111",
                             STYPE1_NOP, "000"));
    for i in 0 to 7 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;

    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolIdle;

    
    for i in 2 to 32 loop
      OutboundSymbolIdle;
      InboundSymbolControl(
        RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, std_logic_vector(to_unsigned(i mod 32, 5)), "11111",
                               STYPE1_NOP, "000"));
    end loop;
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    TestCompare(outboundFrameWriteEmpty, '1', "packet pending");
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 3:");
    TestSpec("Action: Send an outbound packet with maximum length.");
    TestSpec("Result: The packet should be fragmented and received in symbols.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC4-Step3");
    ---------------------------------------------------------------------------
    
    -- Create the frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 133;
    frame := RioFrameCreate(ackId=>"00001", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundFrame(frame);

    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;

    -- Receive the frame.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolIdle;

    -- Send acknowledge that the frame was received.
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00001", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;

    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    TestCompare(outboundFrameWriteEmpty, '1', "packet pending");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 4:");
    TestSpec("Action: Send a packet and confirm it with packet-retry.");
    TestSpec("Result: A restart-from-retry should be transmitted and the packet");
    TestSpec("        should be retransmitted.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC4-Step4");
    ---------------------------------------------------------------------------
    
    -- Create the frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 4;
    frame := RioFrameCreate(ackId=>"00010", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundFrame(frame);

    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    -- Send the frame and acknowledge it with packet-retry.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_RETRY, "00010", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 6 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;

    -- Receive the acknowledgement for the retransmission.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_RESTART_FROM_RETRY, "000"));
    InboundSymbolIdle;

    OutboundSymbolIdle;
    InboundSymbolIdle;
    
    -- Receive the retransmitted frame.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolIdle;

    -- Send acknowledge that the frame was received.
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00010", "11111",
                                                STYPE1_NOP, "000"));
    OutboundSymbolIdle;
    
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    TestCompare(outboundFrameWriteEmpty, '1', "packet pending");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 5:");
    TestSpec("Action: Send a packet and confirm it with packet-not-accepted. ");
    TestSpec("Result: A link-request should be transmitted and the packet should");
    TestSpec("        be retransmitted.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC4-Step5");
    ---------------------------------------------------------------------------
    
    -- Create the frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 5;
    frame := RioFrameCreate(ackId=>"00011", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundFrame(frame);

    -- Receive the frame.
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_NOT_ACCEPTED, "00000", "11111",
                                                STYPE1_NOP, "000"));

    -- Receive the link-request and answer back with a link-response.
    for i in 0 to 6 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_LINK_REQUEST, "100"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "00011", "11111",
                                                STYPE1_NOP, "000"));

    -- Receive the retransmitted frame.
    -- Allow some ticks to pass to let the transmitter recover the error.
    for i in 0 to 8 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolIdle;

    -- Send acknowledge that the frame was received.
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00011", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    TestCompare(outboundFrameWriteEmpty, '1', "packet pending");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 6:");
    TestSpec("Action: Let a packet timeout expire. Then answer with link-response.");
    TestSpec("Result: A link-request should be transmitted and the packet should");
    TestSpec("        be retransmitted.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC4-Step6");
    ---------------------------------------------------------------------------
    
    -- Create the frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 5;
    frame := RioFrameCreate(ackId=>"00100", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundFrame(frame);

    -- Receive the frame.
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolIdle;

    -- Wait a while to let the timer expire and receive the link-request.
    for j in 1 to 7 loop
      for i in 0 to 256 loop
        OutboundSymbolIdle;
        InboundSymbolIdle;
      end loop;
      OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                   STYPE1_NOP, "000"));
      InboundSymbolIdle;
      TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    end loop;
    for i in 0 to 242 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    -- Receive the link-request and answer back with a link-response.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_LINK_REQUEST, "100"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "00100", "11111",
                                                STYPE1_NOP, "000"));
    
    -- Receive the retransmitted frame.
    -- Allow some ticks to pass to let the transmitter recover the error.
    for i in 0 to 8 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolIdle;

    -- Send acknowledge that the frame was received.
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00100", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    TestCompare(outboundFrameWriteEmpty, '1', "packet pending");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 7:");
    TestSpec("Action: Let a packet timeout expire. Then answer with link-response");
    TestSpec("        that indicates that the packet was received.");
    TestSpec("Result: A link-request should be transmitted and the packet should");
    TestSpec("        not be retransmitted.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC4-Step7");
    ---------------------------------------------------------------------------
    
    -- Create the frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 6;
    frame := RioFrameCreate(ackId=>"00101", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundFrame(frame);

    -- Receive the frame.
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolIdle;

    -- Wait a while to let the timer expire and receive the link-request.
    for j in 1 to 7 loop
      for i in 0 to 256 loop
        OutboundSymbolIdle;
        InboundSymbolIdle;
      end loop;
      OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                   STYPE1_NOP, "000"));
      InboundSymbolIdle;
      TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    end loop;
    for i in 0 to 242 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;

    -- Receive the link-request and answer back with a link-response.
    -- Send a link-response that indicates that the frame was received to make
    -- the transmitter go back to normal mode.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_LINK_REQUEST, "100"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "00110", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 8 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    TestCompare(outboundFrameWriteEmpty, '1', "packet pending");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 8:");
    TestSpec("Action: Let a packet timeout expire. No more replies.");
    TestSpec("Result: Three link-requests should be transmitted. When the third");
    TestSpec("        times out the link will be restarted.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC4-Step8");
    ---------------------------------------------------------------------------
    
    -- Create the frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 7;
    frame := RioFrameCreate(ackId=>"00110", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundFrame(frame);

    -- Receive the frame.
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolIdle;

    -- Let 3 link-requests timeout.
    for k in 1 to 3 loop
      -- Wait a while to let the timer expire and receive the link-request.
      for j in 1 to 7 loop
        for i in 0 to 256 loop
          OutboundSymbolIdle;
          InboundSymbolIdle;
        end loop;
        OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                     STYPE1_NOP, "000"));
        InboundSymbolIdle;
        TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
      end loop;
      for i in 0 to 242 loop
        OutboundSymbolIdle;
        InboundSymbolIdle;
      end loop;
      OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                   STYPE1_LINK_REQUEST, "100"));
      InboundSymbolIdle;
      TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    end loop;
    -- Wait for the third link-request to timeout.
    for j in 1 to 7 loop
      for i in 0 to 256 loop
        OutboundSymbolIdle;
        InboundSymbolIdle;
      end loop;
      OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                   STYPE1_NOP, "000"));
      InboundSymbolIdle;
      TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    end loop;
    for i in 0 to 240 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    linkInitializedExpected <= '0';

    -- Reinitialize the transmitter.
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00110", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 14 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    for j in 0 to 13 loop
      OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                   STYPE1_NOP, "000"));
      InboundSymbolIdle;
      for i in 0 to 16 loop
        OutboundSymbolIdle;
        InboundSymbolIdle;
      end loop;
    end loop;
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    linkInitializedExpected <= '1';
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_NOP, "000"));
    InboundSymbolIdle;

    -- Receive the frame.
    for i in 0 to 2 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00110", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;

    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    TestCompare(outboundFrameWriteEmpty, '1', "packet pending");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 9:");
    TestSpec("Action: Let a packet timeout expire. Then answer with totally ");
    TestSpec("        unexpected ackId.");
    TestSpec("Result: A link request should be transmitted and the link should ");
    TestSpec("        be restarted.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC4-Step9");
    ---------------------------------------------------------------------------
    
    -- Create the frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 8;
    frame := RioFrameCreate(ackId=>"00111", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundFrame(frame);

    -- Receive the frame.
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolIdle;

    -- Wait a while to let the timer expire and receive the link-request.
    for j in 1 to 7 loop
      for i in 0 to 256 loop
        OutboundSymbolIdle;
        InboundSymbolIdle;
      end loop;
      OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                   STYPE1_NOP, "000"));
      InboundSymbolIdle;
      TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    end loop;
    for i in 0 to 242 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;

    -- Receive the link-request and answer back with a link-response.
    -- Send a link-response that indicates a totally unexpected ackId.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_LINK_REQUEST, "100"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "10000", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    linkInitializedExpected <= '0';
    
    -- Reinitialize the transmitter.
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "00111", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 250 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    for j in 0 to 13 loop
      OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                   STYPE1_NOP, "000"));
      InboundSymbolIdle;
      for i in 0 to 16 loop
        OutboundSymbolIdle;
        InboundSymbolIdle;
      end loop;
    end loop;
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    linkInitializedExpected <= '1';
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_NOP, "000"));
    InboundSymbolIdle;

    -- Receive the frame.
    for i in 0 to 2 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "00111", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;

    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    TestCompare(outboundFrameWriteEmpty, '1', "packet pending");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 10:");
    TestSpec("Action: Send status with unexpected ackId in normal operation.");
    TestSpec("Result: The transmitter should disregard the error.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC4-Step10");
    ---------------------------------------------------------------------------

    -- Send a status with unexpected ackId.
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "10000", "11111",
                                                STYPE1_NOP, "000"));

    -- Receive no change.
    for i in 0 to 250 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_NOP, "000"));
    InboundSymbolIdle;

    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    TestCompare(outboundFrameWriteEmpty, '1', "packet pending");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 11:");
    TestSpec("Action: Send packet-retry with unexpected ackId in normal operation.");
    TestSpec("Result: The transmitter should enter output-error-stopped.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC4-Step11");
    ---------------------------------------------------------------------------
    
    -- Send a packet-retry with unexpected ackId.
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_RETRY, "10000", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 6 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;

    -- Receive link-request.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_LINK_REQUEST, "100"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "01000", "11111",
                                                STYPE1_NOP, "000"));
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    
    -- Create a frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 10;
    frame := RioFrameCreate(ackId=>"01000", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundFrame(frame);

    -- Wait for the transmitter to recover the previous error.
    for i in 0 to 8 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    -- Receive the frame.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "01000", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    TestCompare(outboundFrameWriteEmpty, '1', "packet pending");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 12:");
    TestSpec("Action: Send packet-accepted with unexpected ackId in normal ");
    TestSpec("        operation.");
    TestSpec("Result: The transmitter should enter output-error-stopped.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC4-Step12");
    ---------------------------------------------------------------------------

    -- Send a packet-accepted with unexpected ackId.
    OutboundSymbolIdle;
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "10000", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 6 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;

    -- Receive link-request.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_LINK_REQUEST, "100"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "01001", "11111",
                                                STYPE1_NOP, "000"));
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    
    -- Create the frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 11;
    frame := RioFrameCreate(ackId=>"01001", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundFrame(frame);

    -- Wait for the transmitter to recover the previous error.
    for i in 0 to 8 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;

    -- Receive the frame.
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "01001", "11111",
                                                STYPE1_NOP, "000"));
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    TestCompare(outboundFrameWriteEmpty, '1', "packet pending");

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 13:");
    TestSpec("Action: Send a packet and then accept it with unexpected ackId.");
    TestSpec("Result: The transmitter should enter output-error-stopped.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC4-Step13");
    ---------------------------------------------------------------------------
    
    -- Create the frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 12;
    frame := RioFrameCreate(ackId=>"01010", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    OutboundFrame(frame);

    -- Receive the frame.
    -- Send unexpected ackId in packet-accepted.
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;
    for i in 0 to frame.length-1 loop
      OutboundSymbolData(frame.payload(i));
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "10000", "11111",
                                                STYPE1_NOP, "000"));

    -- Receive link-request.
    for i in 0 to 6 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_LINK_REQUEST, "100"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_LINK_RESPONSE, "01011", "11111",
                                                STYPE1_NOP, "000"));        

    -- Wait some additional ticks to let the transmitter recover the previous error.
    for i in 0 to 6 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    TestCompare(outboundFrameWriteEmpty, '1', "packet pending");
 
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioSerial-TC5");
    TestSpec("Description: Test mixed port transmission and reception.");
    TestSpec("Requirement: XXXXX");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Start sending an outbound packet and while in transmission, ");
    TestSpec("        start and complete an inbound packet.");
    TestSpec("Result: The ack for the inbound packet should be inserted into the");
    TestSpec("        outbound packet.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSerial-TC5-Step1");
    ---------------------------------------------------------------------------

    -- Send a long outbound frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 133;
    frameOutbound := RioFrameCreate(ackId=>"01011", vc=>'0', crf=>'0', prio=>"00",
                                    tt=>"01", ftype=>"0000",
                                    sourceId=>x"0000", destId=>x"0000",
                                    payload=>payload);
    OutboundFrame(frameOutbound);

    -- Receive a short inbound frame.
    CreateRandomPayload(payload.data, seed1, seed2);
    payload.length := 2;
    frameInbound := RioFrameCreate(ackId=>"01100", vc=>'0', crf=>'0', prio=>"00",
                            tt=>"01", ftype=>"0000",
                            sourceId=>x"0000", destId=>x"0000",
                            payload=>payload);
    InboundFrame(frameInbound);
    

    -- Receive the outbound frame and start a short inbound frame at the same time.
    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;

    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01100", "11111",
                                                 STYPE1_START_OF_PACKET, "000"));
    InboundSymbolIdle;

    OutboundSymbolData(frameOutbound.payload(0));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "10000", "11111",
                                                STYPE1_START_OF_PACKET, "000"));

    OutboundSymbolData(frameOutbound.payload(1));
    InboundSymbolData(frameInbound.payload(0));
    
    OutboundSymbolData(frameOutbound.payload(2));
    InboundSymbolData(frameInbound.payload(1));
    
    OutboundSymbolData(frameOutbound.payload(3));
    InboundSymbolData(frameInbound.payload(2));
    
    OutboundSymbolData(frameOutbound.payload(4));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "10000", "11111",
                                                STYPE1_END_OF_PACKET, "000"));
    
    OutboundSymbolData(frameOutbound.payload(5));
    InboundSymbolIdle;
    
    OutboundSymbolData(frameOutbound.payload(6));
    InboundSymbolIdle;
    
    OutboundSymbolData(frameOutbound.payload(7));
    InboundSymbolIdle;

    OutboundSymbolData(frameOutbound.payload(8));
    InboundSymbolIdle;

    OutboundSymbolData(frameOutbound.payload(9));
    InboundSymbolIdle;

    OutboundSymbolData(frameOutbound.payload(10));
    InboundSymbolIdle;

    OutboundSymbolData(frameOutbound.payload(11));
    InboundSymbolIdle;

    OutboundSymbolData(frameOutbound.payload(12));
    InboundSymbolIdle;

    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "01100", "11111",
                                                 STYPE1_NOP, "000"));
    InboundSymbolIdle;

    for i in 13 to frameOutbound.length-1 loop
      OutboundSymbolData(frameOutbound.payload(i));
      InboundSymbolIdle;
    end loop;

    OutboundSymbolControl(RioControlSymbolCreate(STYPE0_STATUS, "01101", "11111",
                                                 STYPE1_END_OF_PACKET, "000"));
    InboundSymbolControl(RioControlSymbolCreate(STYPE0_PACKET_ACCEPTED, "01011", "11111",
                                                STYPE1_NOP, "000"));

    for i in 0 to 4 loop
      OutboundSymbolIdle;
      InboundSymbolIdle;
    end loop;
    
    TestWait(outboundSymbolWriteEmpty, '1', "symbols pending");
    TestCompare(outboundFrameWriteEmpty, '1', "packet pending");

    ---------------------------------------------------------------------------
    -- Test completed.
    ---------------------------------------------------------------------------

    TestEnd;
  end process;

  
  -----------------------------------------------------------------------------
  -- Instantiate interface simulator models.
  -----------------------------------------------------------------------------

  process
  begin
    wait until clk = '1';
    TestCompare(linkInitialized, linkInitializedExpected, "link initialized");
  end process;
  
  SwitchPort: TestSwitchPort
    port map(
      clk=>clk, areset_n=>areset_n,
      outboundWriteEmpty_o=>outboundFrameWriteEmpty, 
      outboundWrite_i=>outboundFrameWrite, 
      outboundWriteMessage_i=>outboundFrameWriteMessage, 
      outboundWriteAck_o=>outboundFrameWriteAck, 
      inboundWriteEmpty_o=>inboundFrameWriteEmpty, 
      inboundWrite_i=>inboundFrameWrite, 
      inboundWriteMessage_i=>inboundFrameWriteMessage, 
      inboundWriteAck_o=>inboundFrameWriteAck, 
      readFrameEmpty_o=>readFrameEmpty, readFrame_i=>readFrame,
      readFrameRestart_i=>readFrameRestart, readFrameAborted_o=>readFrameAborted,
      readWindowEmpty_o=>readWindowEmpty,
      readWindowReset_i=>readWindowReset, readWindowNext_i=>readWindowNext,
      readContentEmpty_o=>readContentEmpty, readContent_i=>readContent,
      readContentEnd_o=>readContentEnd, readContentData_o=>readContentData,
      writeFrame_i=>writeFrame, writeFrameAbort_i=>writeFrameAbort,
      writeContent_i=>writeContent, writeContentData_i=>writeContentData);

  PcsPort: TestSymbolPort 
    port map(
      clk=>clk, areset_n=>areset_n,
      portInitialized_i=>portInitialized,
      outboundControlValid_i=>outboundControlValid,
      outboundControlSymbol_i=>outboundControlSymbol,
      outboundDataValid_i=>outboundDataValid,
      outboundDataSymbol_i=>outboundDataSymbol,
      inboundControlValid_o=>inboundControlValid,
      inboundControlSymbol_o=>inboundControlSymbol,
      inboundDataValid_o=>inboundDataValid,
      inboundDataSymbol_o=>inboundDataSymbol,
      outboundWriteEmpty_o=>outboundSymbolWriteEmpty,
      outboundWrite_i=>outboundSymbolWrite,
      outboundWriteMessage_i=>outboundSymbolWriteMessage,
      outboundWriteAck_o=>outboundSymbolWriteAck,
      inboundWriteEmpty_o=>inboundSymbolWriteEmpty,
      inboundWrite_i=>inboundSymbolWrite,
      inboundWriteMessage_i=>inboundSymbolWriteMessage,
      inboundWriteAck_o=>inboundSymbolWriteAck);
  
  -----------------------------------------------------------------------------
  -- Instantiate the test object.
  -----------------------------------------------------------------------------
  TestObject: RioSerial
    generic map(
      TIMEOUT_WIDTH=>11, SYMBOL_COUNTER_WIDTH=>8,
      TICKS_SEND_STATUS_STARTUP=>15, TICKS_SEND_STATUS_OPERATIONAL=>255)
    port map(
      clk=>clk, areset_n=>areset_n, enable=>enable,
      portLinkTimeout_i=>portLinkTimeout,
      linkInitialized_o=>linkInitialized,
      inputPortEnable_i=>inputPortEnable,
      outputPortEnable_i=>outputPortEnable,
      localAckIdWrite_i=>localAckIdWrite, 
      clrOutstandingAckId_i=>clrOutstandingAckId, 
      inboundAckId_i=>inboundAckIdWrite, 
      outstandingAckId_i=>outstandingAckIdWrite, 
      outboundAckId_i=>outboundAckIdWrite, 
      inboundAckId_o=>inboundAckIdRead, 
      outstandingAckId_o=>outstandingAckIdRead, 
      outboundAckId_o=>outboundAckIdRead, 
      readFrameEmpty_i=>readFrameEmpty,
      readFrame_o=>readFrame,
      readFrameRestart_o=>readFrameRestart, 
      readFrameAborted_i=>readFrameAborted,
      readWindowEmpty_i=>readWindowEmpty,
      readWindowReset_o=>readWindowReset,
      readWindowNext_o=>readWindowNext,
      readContentEmpty_i=>readContentEmpty,
      readContent_o=>readContent,
      readContentEnd_i=>readContentEnd,
      readContentData_i=>readContentData,
      writeFrameFull_i=>inboundFrameFull,
      writeFrame_o=>writeFrame,
      writeFrameAbort_o=>writeFrameAbort,
      writeContent_o=>writeContent,
      writeContentData_o=>writeContentData,
      portInitialized_i=>portInitialized,
      outboundControlValid_o=>outboundControlValid, 
      outboundControlSymbol_o=>outboundControlSymbol, 
      outboundDataValid_o=>outboundDataValid, 
      outboundDataSymbol_o=>outboundDataSymbol, 
      inboundControlValid_i=>inboundControlValid, 
      inboundControlSymbol_i=>inboundControlSymbol, 
      inboundDataValid_i=>inboundDataValid, 
      inboundDataSymbol_i=>inboundDataSymbol);

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
use work.TestRioSerialPackage.all;
use work.TestPortPackage.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity TestSwitchPort is
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    outboundWriteEmpty_o : out std_logic;
    outboundWrite_i : in std_logic;
    outboundWriteMessage_i : in MessageFrame;
    outboundWriteAck_o : out std_logic;
    
    inboundWriteEmpty_o : out std_logic;
    inboundWrite_i : in std_logic;
    inboundWriteMessage_i : in MessageFrame;
    inboundWriteAck_o : out std_logic;
    
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
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture TestSwitchPortImpl of TestSwitchPort is
  constant QUEUE_SIZE : natural := 63;
  type QueueArray is array (natural range <>) of MessageFrame;
  
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
  Outbound: process
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
    outboundWriteEmpty_o <= '1';
    outboundWriteAck_o <= '0';

    loop
      wait until clk = '1' or outboundWrite_i = '1';

      if (clk'event) then
        if (readFrame_i = '1') then
          if (back /= front) then
            back := QueueIndexInc(back);
          else
            TestError("OUTBOUND:BACK:reading when no frame is present");
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
            TestError("OUTBOUND:WINDOW:reading when no frame is present");
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
              TestError("OUTBOUND:CONTENT:reading when frame has ended");
            end if;
          else
            TestError("OUTBOUND:CONTENT:reading when no frame is present");
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
          outboundWriteEmpty_o <= '1';
        else
          outboundWriteEmpty_o <= '0';
        end if;
      elsif (outboundWrite_i'event) then
        frameQueue(front) := outboundWriteMessage_i;
        front := QueueIndexInc(front);

        outboundWriteEmpty_o <= '0';
        outboundWriteAck_o <= '1';
        wait until outboundWrite_i = '0';
        outboundWriteAck_o <= '0';
      end if;
    end loop;    
  end process;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  Inbound: process
    variable frameQueue : QueueArray(0 to QUEUE_SIZE);
    variable front, back : natural range 0 to QUEUE_SIZE;
    variable frameIndex : natural range 0 to 69;
  begin
    wait until areset_n = '1';

    inboundWriteEmpty_o <= '1';
    inboundWriteAck_o <= '0';

    front := 0;
    back := 0;
    frameIndex := 0;

    loop
      wait until clk = '1' or inboundWrite_i = '1';

      if (clk'event) then

        if (writeFrame_i = '1') then
          if (frameIndex = 0) then
            TestError("INBOUND:Empty frame written.");
          end if;
          if (frameIndex /= frameQueue(back).frame.length) then
            TestError("INBOUND:Frame with unmatching length was written.");
          end if;
          if (back /= front) then
            back := QueueIndexInc(back);
          else
            TestError("INBOUND:Unexpected frame written.");
          end if;
          frameIndex := 0;
        end if;

        if (writeFrameAbort_i = '1') then
          if (back /= front) then
            if (frameQueue(back).willAbort) then
              if (frameIndex /= frameQueue(back).frame.length) then
                TestError("INBOUND:Frame with unmatching length was aborted.");
              end if;
              back := QueueIndexInc(back);
            else
              TestError("INBOUND:Not expecting this frame to abort.");
            end if;
          end if;
          frameIndex := 0;
        end if;

        if (writeContent_i = '1') then
          if (frameIndex < frameQueue(back).frame.length) then
            if (frameQueue(back).frame.payload(frameIndex) /= writeContentData_i) then
              TestError("INBOUND:Unexpected frame content written.");
            end if;
            frameIndex := frameIndex + 1;
          else
            TestError("INBOUND:Receiving more frame content than expected.");
          end if;
        end if;
        
        if (front = back) then
          inboundWriteEmpty_o <= '1';
        else
          inboundWriteEmpty_o <= '0';
        end if;
      elsif (inboundWrite_i'event) then
        frameQueue(front) := inboundWriteMessage_i;
        front := QueueIndexInc(front);

        inboundWriteEmpty_o <= '0';
        inboundWriteAck_o <= '1';
        wait until inboundWrite_i = '0';
        inboundWriteAck_o <= '0';
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
use work.TestRioSerialPackage.all;
use work.TestPortPackage.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity TestSymbolPort is
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    portInitialized_i : in std_logic;
    outboundControlValid_i : in std_logic;
    outboundControlSymbol_i : in std_logic_vector(23 downto 0);
    outboundDataValid_i : in std_logic;
    outboundDataSymbol_i : in std_logic_vector(31 downto 0);
    inboundControlValid_o : out std_logic;
    inboundControlSymbol_o : out std_logic_vector(23 downto 0);
    inboundDataValid_o : out std_logic;
    inboundDataSymbol_o : out std_logic_vector(31 downto 0);

    outboundWriteEmpty_o : out std_logic;
    outboundWrite_i : in std_logic;
    outboundWriteMessage_i : in MessageSymbol;
    outboundWriteAck_o : out std_logic;

    inboundWriteEmpty_o : out std_logic;
    inboundWrite_i : in std_logic;
    inboundWriteMessage_i : in MessageSymbol;
    inboundWriteAck_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture TestSymbolPortImpl of TestSymbolPort is
  constant QUEUE_SIZE : natural := 2047;
  type QueueArray is array (natural range <>) of MessageSymbol;

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
  Outbound: process
    variable front, back : natural range 0 to QUEUE_SIZE;
    variable symbolQueue : QueueArray(0 to QUEUE_SIZE);
  begin
    wait until areset_n = '1';

    front := 0;
    back := 0;
    outboundWriteEmpty_o <= '1';
    outboundWriteAck_o <= '0';

    loop
      wait until clk = '1' or outboundWrite_i = '1';

      if (clk'event) then
        if (back /= front) then
          if (symbolQueue(back).symbolType = "01") then
            if (not ((symbolQueue(back).ignoreIdle) and
                     (outboundControlValid_i = '0') and
                     (outboundDataValid_i = '0'))) then
              TestCompare(outboundControlValid_i, '1', "OUTBOUND:control symbol");
              TestCompare(outboundControlSymbol_i(23 downto 21), symbolQueue(back).symbolContent(23 downto 21),
                          "OUTBOUND:stype0=" & to_string(symbolQueue(back).symbolContent(23 downto 21)));
              TestCompare(outboundControlSymbol_i(20 downto 16), symbolQueue(back).symbolContent(20 downto 16),
                          "OUTBOUND:parameter0=" & to_string(symbolQueue(back).symbolContent(20 downto 16)));
              TestCompare(outboundControlSymbol_i(15 downto 11), symbolQueue(back).symbolContent(15 downto 11),
                          "OUTBOUND:parameter1=" & to_string(symbolQueue(back).symbolContent(15 downto 11)));
              TestCompare(outboundControlSymbol_i(10 downto 8), symbolQueue(back).symbolContent(10 downto 8),
                          "OUTBOUND:stype1=" & to_string(symbolQueue(back).symbolContent(10 downto 8)));
              TestCompare(outboundControlSymbol_i(7 downto 5), symbolQueue(back).symbolContent(7 downto 5),
                          "OUTBOUND:cmd=" & to_string(symbolQueue(back).symbolContent(7 downto 5)));
              TestCompare(outboundControlSymbol_i(4 downto 0), symbolQueue(back).symbolContent(4 downto 0),
                          "OUTBOUND:crc5=" & to_string(symbolQueue(back).symbolContent(4 downto 0)));
              TestCompare(outboundDataValid_i, '0', "OUTBOUND:no data symbol");
              back := QueueIndexInc(back);
            end if;
          elsif (symbolQueue(back).symbolType = "10") then
            if (not ((symbolQueue(back).ignoreIdle) and
                     (outboundControlValid_i = '0') and
                     (outboundDataValid_i = '0'))) then
              TestCompare(outboundDataValid_i, '1', "OUTBOUND:valid data symbol");
              TestCompare(outboundDataSymbol_i, symbolQueue(back).symbolContent(31 downto 0),
                "OUTBOUND:data symbol content");
              TestCompare(outboundControlValid_i, '0', "OUTBOUND:valid control symbol");
              back := QueueIndexInc(back);
            end if;
          elsif (symbolQueue(back).symbolType = "00") then
            TestCompare(outboundControlValid_i, '0', "OUTBOUND:valid control symbol");
            TestCompare(outboundDataValid_i, '0', "OUTBOUND:valid data symbol");
            back := QueueIndexInc(back);
          else
            TestCompare(outboundControlValid_i, '1', "OUTBOUND:valid control symbol");
            TestCompare(outboundDataValid_i, '1', "OUTBOUND:valid data symbol");
            back := QueueIndexInc(back);
          end if;

          if (front = back) then
            outboundWriteEmpty_o <= '1';
          else
            outboundWriteEmpty_o <= '0';
          end if;
        else
          TestError("OUTBOUND:empty symbol queue");
        end if;          
      elsif (outboundWrite_i'event) then
        symbolQueue(front) := outboundWriteMessage_i;
        front := QueueIndexInc(front);

        outboundWriteEmpty_o <= '0';
        outboundWriteAck_o <= '1';
        wait until outboundWrite_i = '0';
        outboundWriteAck_o <= '0';
      end if;
    end loop;    
  end process;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  Inbound: process
    variable front, back : natural range 0 to QUEUE_SIZE;
    variable symbolQueue : QueueArray(0 to QUEUE_SIZE);
  begin
    wait until areset_n = '1';

    front := 0;
    back := 0;
    inboundWriteEmpty_o <= '1';
    inboundWriteAck_o <= '0';

    inboundControlValid_o <= '0';
    inboundControlSymbol_o <= (others=>'U');
    inboundDataValid_o <= '0';
    inboundDataSymbol_o <= (others=>'U');
    
    loop
      wait until clk = '1' or inboundWrite_i = '1';

      if (clk'event) then
        if (back /= front) then
          if (symbolQueue(back).symbolType = "00") then
            inboundControlValid_o <= '0';
            inboundDataValid_o <= '0';
          elsif (symbolQueue(back).symbolType = "01") then
            inboundControlValid_o <= '1';
            inboundControlSymbol_o <= symbolQueue(back).symbolContent(23 downto 0);
            inboundDataValid_o <= '0';
          elsif (symbolQueue(back).symbolType = "10") then
            inboundControlValid_o <= '0';
            inboundDataValid_o <= '1';
            inboundDataSymbol_o <= symbolQueue(back).symbolContent(31 downto 0);
          else
            inboundControlValid_o <= '1';
            inboundDataValid_o <= '1';
          end if;

          back := QueueIndexInc(back);

          if (front = back) then
            inboundWriteEmpty_o <= '1';
          else
            inboundWriteEmpty_o <= '0';
          end if;
        else
          TestError("INBOUND:empty symbol queue");
        end if;
      elsif (inboundWrite_i'event) then
        symbolQueue(front) := inboundWriteMessage_i;
        front := QueueIndexInc(front);

        inboundWriteEmpty_o <= '0';
        inboundWriteAck_o <= '1';
        wait until inboundWrite_i = '0';
        inboundWriteAck_o <= '0';
      end if;
    end loop;    
  end process;

end architecture;
