-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Contains automatic simulation test code to verify a RioSwitch implementation.
-- Notice: Compile _This_file_ using VHDL'2008
-- 
-- To Do:
-- - Test all sizes of packets that go through the maintenance port.
-- - Make sure all testcases from the 1.0 branch is in here.
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
-- TestRioSwitch.
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
-- Entity for TestRioSwitch.
-------------------------------------------------------------------------------
entity TestRioSwitch is
end entity;


-------------------------------------------------------------------------------
-- Architecture for TestRioSwitch.
-------------------------------------------------------------------------------
architecture TestRioSwitchImpl of TestRioSwitch is
  
  component RioSwitch is
    generic(
      SWITCH_PORTS : natural range 3 to 255 := 4; 
      DEVICE_IDENTITY : std_logic_vector(15 downto 0);
      DEVICE_VENDOR_IDENTITY : std_logic_vector(15 downto 0);
      DEVICE_REV : std_logic_vector(31 downto 0);
      ASSY_IDENTITY : std_logic_vector(15 downto 0);
      ASSY_VENDOR_IDENTITY : std_logic_vector(15 downto 0);
      ASSY_REV : std_logic_vector(15 downto 0);
      portWriteTimeoutResetValue : std_logic_vector(15 downto 0) := x"FFFF");
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      
      writeFrameFull_i : in Array1(SWITCH_PORTS-1 downto 0);
      writeFrame_o : out Array1(SWITCH_PORTS-1 downto 0);
      writeFrameAbort_o : out Array1(SWITCH_PORTS-1 downto 0);
      writeContent_o : out Array1(SWITCH_PORTS-1 downto 0);
      writeContentData_o : out Array32(SWITCH_PORTS-1 downto 0);

      readFrameEmpty_i : in Array1(SWITCH_PORTS-1 downto 0);
      readFrame_o : out Array1(SWITCH_PORTS-1 downto 0);
      readContent_o : out Array1(SWITCH_PORTS-1 downto 0);
      readContentEnd_i : in Array1(SWITCH_PORTS-1 downto 0);
      readContentData_i : in Array32(SWITCH_PORTS-1 downto 0);
      
      portLinkTimeout_o : out std_logic_vector(23 downto 0);

      linkInitialized_i : in Array1(SWITCH_PORTS-1 downto 0);
      outputPortEnable_o : out Array1(SWITCH_PORTS-1 downto 0);
      inputPortEnable_o : out Array1(SWITCH_PORTS-1 downto 0);
      
      localAckIdWrite_o : out Array1(SWITCH_PORTS-1 downto 0);
      clrOutstandingAckId_o : out Array1(SWITCH_PORTS-1 downto 0);
      inboundAckId_o : out Array5(SWITCH_PORTS-1 downto 0);
      outstandingAckId_o : out Array5(SWITCH_PORTS-1 downto 0);
      outboundAckId_o : out Array5(SWITCH_PORTS-1 downto 0);
      inboundAckId_i : in Array5(SWITCH_PORTS-1 downto 0);
      outstandingAckId_i : in Array5(SWITCH_PORTS-1 downto 0);
      outboundAckId_i : in Array5(SWITCH_PORTS-1 downto 0);
    
      configStb_o : out std_logic;
      configWe_o : out std_logic;
      configAddr_o : out std_logic_vector(23 downto 0);
      configData_o : out std_logic_vector(31 downto 0);
      configData_i : in std_logic_vector(31 downto 0);
      configAck_i : in std_logic);
  end component;

  constant PORTS : natural := 3;
  constant SWITCH_IDENTITY : std_logic_vector(15 downto 0) := x"0123";
  constant SWITCH_VENDOR_IDENTITY : std_logic_vector(15 downto 0) := x"4567";
  constant SWITCH_REV : std_logic_vector(31 downto 0) := x"89abcdef";
  constant SWITCH_ASSY_IDENTITY : std_logic_vector(15 downto 0) := x"0011";
  constant SWITCH_ASSY_VENDOR_IDENTITY : std_logic_vector(15 downto 0) := x"2233";
  constant SWITCH_ASSY_REV : std_logic_vector(15 downto 0) := x"4455";
  
  constant verboseInfo : boolean := false;      --if True, print info in vPrint-strings (neat when debugging, set to false for regression tests)
--constant verboseInfo : boolean := true;       --if True, print info in vPrint-strings (neat when debugging, set to false for regression tests)
  
  constant tt_8bit  : std_logic_vector(1 downto 0) := b"00";
  constant tt_16bit : std_logic_vector(1 downto 0) := b"01";
  constant tt_32bit : std_logic_vector(1 downto 0) := b"10";
  
  signal clk : std_logic;
  signal areset_n : std_logic;

  signal inboundEmpty : Array1(PORTS-1 downto 0);
  signal inboundEmpty0 : std_logic;
  signal inboundEmpty1 : std_logic;
  signal inboundEmpty2 : std_logic;
  signal inboundWrite : Array1(PORTS-1 downto 0);
  signal inboundWrite0 : std_logic;
  signal inboundWrite1 : std_logic;
  signal inboundWrite2 : std_logic;
  signal inboundMessage : TestPortMessagePacketBufferArray(PORTS-1 downto 0);
  signal inboundMessage0 : TestPortMessagePacketBuffer;
  signal inboundMessage1 : TestPortMessagePacketBuffer;
  signal inboundMessage2 : TestPortMessagePacketBuffer;
  signal inboundAck : Array1(PORTS-1 downto 0);
  signal inboundAck0 : std_logic;
  signal inboundAck1 : std_logic;
  signal inboundAck2 : std_logic;
  
  signal outboundEmpty : Array1(PORTS-1 downto 0);
  signal outboundEmpty0 : std_logic;
  signal outboundEmpty1 : std_logic;
  signal outboundEmpty2 : std_logic;
  signal outboundWrite : Array1(PORTS-1 downto 0);
  signal outboundWrite0 : std_logic;
  signal outboundWrite1 : std_logic;
  signal outboundWrite2 : std_logic;
  signal outboundMessage : TestPortMessagePacketBufferArray(PORTS-1 downto 0);
  signal outboundMessage0 : TestPortMessagePacketBuffer;
  signal outboundMessage1 : TestPortMessagePacketBuffer;
  signal outboundMessage2 : TestPortMessagePacketBuffer;
  signal outboundAck : Array1(PORTS-1 downto 0);
  signal outboundAck0 : std_logic;
  signal outboundAck1 : std_logic;
  signal outboundAck2 : std_logic;
  
  signal writeFrameFull : Array1(PORTS-1 downto 0);
  signal writeFrame : Array1(PORTS-1 downto 0);
  signal writeFrameAbort : Array1(PORTS-1 downto 0);
  signal writeContent : Array1(PORTS-1 downto 0);
  signal writeContentData : Array32(PORTS-1 downto 0);

  signal readFrameEmpty : Array1(PORTS-1 downto 0);
  signal readFrame : Array1(PORTS-1 downto 0);
  signal readContent : Array1(PORTS-1 downto 0);
  signal readContentEnd : Array1(PORTS-1 downto 0);
  signal readContentData : Array32(PORTS-1 downto 0);
  
  signal portLinkTimeout : std_logic_vector(23 downto 0);
  
  signal linkInitialized : Array1(PORTS-1 downto 0);
  signal outputPortEnable : Array1(PORTS-1 downto 0);
  signal inputPortEnable : Array1(PORTS-1 downto 0);
      
  signal localAckIdWrite : Array1(PORTS-1 downto 0);
  signal clrOutstandingAckId : Array1(PORTS-1 downto 0);
  signal inboundAckIdWrite : Array5(PORTS-1 downto 0);
  signal outstandingAckIdWrite : Array5(PORTS-1 downto 0);
  signal outboundAckIdWrite : Array5(PORTS-1 downto 0);
  signal inboundAckIdRead : Array5(PORTS-1 downto 0);
  signal outstandingAckIdRead : Array5(PORTS-1 downto 0);
  signal outboundAckIdRead : Array5(PORTS-1 downto 0);
        
  signal messageEmpty : std_logic;
  signal messageWrite : std_logic;
  signal message : TestPortMessageWishbone;
  signal messageAck : std_logic;
        
  signal configStb : std_logic;
  signal configWe : std_logic;
  signal configAddr : std_logic_vector(23 downto 0);
  signal configDataWrite : std_logic_vector(31 downto 0);
  signal configDataRead : std_logic_vector(31 downto 0);
  signal configAck : std_logic;
  
  signal componentTag : std_logic_vector(31 downto 0);
  signal PortNerrorDetectCSR :  std_logic_vector(31 downto 0);
  signal destIdErrorEvent : std_logic_vector(15 downto 0) := (others=>'0');
  signal expectedDestPort : natural range 0 to 255 := 0;
  
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
  -- Instantiate the switch.
  -----------------------------------------------------------------------------

  TestSwitch: RioSwitch
    generic map(
      SWITCH_PORTS=>PORTS,
      DEVICE_IDENTITY=>SWITCH_IDENTITY,
      DEVICE_VENDOR_IDENTITY=>SWITCH_VENDOR_IDENTITY,
      DEVICE_REV=>SWITCH_REV,
      ASSY_IDENTITY=>SWITCH_ASSY_IDENTITY,
      ASSY_VENDOR_IDENTITY=>SWITCH_ASSY_VENDOR_IDENTITY,
      ASSY_REV=>SWITCH_ASSY_REV,
      portWriteTimeoutResetValue=>x"0020")  -- x"0020": override default 100ms timeout -> 48.8 us
    port map(
      clk=>clk, areset_n=>areset_n, 
      writeFrameFull_i=>writeFrameFull,
      writeFrame_o=>writeFrame, writeFrameAbort_o=>writeFrameAbort, 
      writeContent_o=>writeContent, writeContentData_o=>writeContentData, 
      readFrameEmpty_i=>readFrameEmpty, 
      readFrame_o=>readFrame, readFrameRestart_o=>readFrameRestart, 
      readFrameAborted_i=>readFrameAborted,
      readContentEmpty_i=>readContentEmpty,
      readContent_o=>readContent, readContentEnd_i=>readContentEnd, 
      readContentData_i=>readContentData,
      portLinkTimeout_o=>portLinkTimeout,
      linkInitialized_i=>linkInitialized,
      outputPortEnable_o=>outputPortEnable, inputPortEnable_o=>inputPortEnable,
      localAckIdWrite_o=>localAckIdWrite, 
      clrOutstandingAckId_o=>clrOutstandingAckId, 
      inboundAckId_o=>inboundAckIdWrite, 
      outstandingAckId_o=>outstandingAckIdWrite, 
      outboundAckId_o=>outboundAckIdWrite, 
      inboundAckId_i=>inboundAckIdRead, 
      outstandingAckId_i=>outstandingAckIdRead, 
      outboundAckId_i=>outboundAckIdRead, 
      configStb_o=>configStb, configWe_o=>configWe, configAddr_o=>configAddr,
      configData_o=>configDataWrite, configData_i=>configDataRead, configAck_i=>configAck);
  
  -----------------------------------------------------------------------------
  -- Serial port emulator.
  -----------------------------------------------------------------------------
  TestDriver: process

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure SendFrame(constant portIndex : natural range 0 to 7;
                        constant frame : RioFrame) is
    begin
      -- Crappy Modelsim cannot handle arrays of signals...
      case portIndex is
        when 0 =>
          TestPortPacketBufferWrite(inboundWrite0, inboundMessage0, inboundAck0,
                                    frame, false);
        when 1 =>
          TestPortPacketBufferWrite(inboundWrite1, inboundMessage1, inboundAck1,
                                    frame, false);
        when others =>
          TestPortPacketBufferWrite(inboundWrite2, inboundMessage2, inboundAck2,
                                    frame, false);
      end case;
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure ReceiveFrame(constant portIndex : natural range 0 to 7;
                           constant frame : RioFrame) is
    begin
      -- Crappy Modelsim cannot handle arrays of signals...
      case portIndex is
        when 0 =>
          TestPortPacketBufferWrite(outboundWrite0, outboundMessage0, outboundAck0,
                                    frame, false);
        when 1 =>
          TestPortPacketBufferWrite(outboundWrite1, outboundMessage1, outboundAck1,
                                    frame, false);
        when others =>
          TestPortPacketBufferWrite(outboundWrite2, outboundMessage2, outboundAck2,
                                    frame, false);
      end case;
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure SendFrame(constant portIndex : natural range 0 to 7;
                        constant sourceId : std_logic_vector(15 downto 0);
                        constant destinationId : std_logic_vector(15 downto 0);
                        constant payload : RioPayload) is
      variable frame : RioFrame;
    begin
      frame := RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                              tt=>"01", ftype=>"0000", 
                              sourceId=>sourceId, destId=>destinationId,
                              payload=>payload);
      SendFrame(portIndex, frame);
    end procedure;
    
    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure ReceiveFrame(constant portIndex : natural range 0 to 7;
                           constant sourceId : std_logic_vector(15 downto 0);
                           constant destinationId : std_logic_vector(15 downto 0);
                           constant payload : RioPayload) is
      variable frame : RioFrame;
    begin
      frame := RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                              tt=>"01", ftype=>"0000", 
                              sourceId=>sourceId, destId=>destinationId,
                              payload=>payload);
      ReceiveFrame(portIndex, frame);
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure ReadConfig32(constant portIndex : natural range 0 to 7;
                           constant destinationId : std_logic_vector(15 downto 0);
                           constant sourceId : std_logic_vector(15 downto 0);
                           constant hop : std_logic_vector(7 downto 0);
                           constant tid : std_logic_vector(7 downto 0);
                           constant address : std_logic_vector(23 downto 0);
                           constant data : std_logic_vector(31 downto 0)) is
      variable maintData : DoubleWordArray(0 to 7);
    begin
      SendFrame(portIndex, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                          tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                          sourceId=>sourceId, destId=>destinationId,
                                          payload=>RioMaintenance(transaction=>"0000",
                                                                  size=>"1000",
                                                                  tid=>tid,
                                                                  hopCount=>hop,
                                                                  configOffset=>address(23 downto 3),
                                                                  wdptr=>address(2),
                                                                  dataLength=>0,
                                                                  data=>maintData)));
      if (address(2) = '0') then
        maintData(0) := data & x"00000000";
      else
        maintData(0) := x"00000000" & data ;
      end if;
      
      ReceiveFrame(portIndex, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                             tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                             sourceId=>destinationId, destId=>sourceId,
                                             payload=>RioMaintenance(transaction=>"0010",
                                                                     size=>"0000",
                                                                     tid=>tid,
                                                                     hopCount=>x"ff",
                                                                     configOffset=>"000000000000000000000",
                                                                     wdptr=>'0',
                                                                     dataLength=>1,
                                                                     data=>maintData)));
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure WriteConfig32(constant portIndex : natural range 0 to 7;
                            constant destinationId : std_logic_vector(15 downto 0);
                            constant sourceId : std_logic_vector(15 downto 0);
                            constant hop : std_logic_vector(7 downto 0);
                            constant tid : std_logic_vector(7 downto 0);
                            constant address : std_logic_vector(23 downto 0);
                            constant data : std_logic_vector(31 downto 0)) is
      variable maintData : DoubleWordArray(0 to 7);
    begin
      if (address(2) = '0') then
        maintData(0) := data & x"00000000";
      else
        maintData(0) := x"00000000" & data ;
      end if;

      SendFrame(portIndex, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                          tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                          sourceId=>sourceId, destId=>destinationId,
                                          payload=>RioMaintenance(transaction=>"0001",
                                                                  size=>"1000",
                                                                  tid=>tid,
                                                                  hopCount=>hop,
                                                                  configOffset=>address(23 downto 3),
                                                                  wdptr=>address(2),
                                                                  dataLength=>1,
                                                                  data=>maintData)));
      
      ReceiveFrame(portIndex, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                             tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                             sourceId=>destinationId, destId=>sourceId,
                                             payload=>RioMaintenance(transaction=>"0011",
                                                                     size=>"0000",
                                                                     tid=>tid,
                                                                     hopCount=>x"ff",
                                                                     configOffset=>"000000000000000000000",
                                                                     wdptr=>'0',
                                                                     dataLength=>0,
                                                                     data=>maintData)));
    end procedure;

    ---------------------------------------------------------------------------
    -- Set a route table entry.
    ---------------------------------------------------------------------------
    procedure RouteSet(constant deviceId : std_logic_vector(15 downto 0);
                       constant portIndex : std_logic_vector(7 downto 0)) is
      variable frame : RioFrame;
    begin
      WriteConfig32(portIndex=>0, destinationId=>x"ffff", sourceId=>x"ffff", hop=>x"00",
                    tid=>x"de", address=>x"000070", data=>(x"0000" & deviceId));
      WriteConfig32(portIndex=>0, destinationId=>x"ffff", sourceId=>x"ffff", hop=>x"00",
                    tid=>x"ad", address=>x"000074", data=>(x"000000" & portIndex));
    end procedure;
    
    ---------------------------------------------------------------------------
    -- Set the default route table entry.
    ---------------------------------------------------------------------------
    procedure RouteSetDefault(constant portIndex : std_logic_vector(7 downto 0)) is
      variable frame : RioFrame;
    begin
      WriteConfig32(portIndex=>0, destinationId=>x"ffff", sourceId=>x"ffff", hop=>x"00",
                    tid=>x"ad", address=>x"000078", data=>(x"000000" & portIndex));
    end procedure;
    
    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure RouteFrame(constant sourcePortIndex : natural range 0 to 7;
                         constant destinationPortIndex : natural range 0 to 7;
                         constant sourceId : std_logic_vector(15 downto 0);
                         constant destinationId : std_logic_vector(15 downto 0);
                         constant payload : RioPayload) is
      variable frame : RioFrame;
    begin
      frame := RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                              tt=>"01", ftype=>"0000", 
                              destId=>destinationId, sourceId=>sourceId, 
                              payload=>payload);

      ReceiveFrame(destinationPortIndex, frame);
      SendFrame(sourcePortIndex, frame);
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure ExchangeFrames is
    begin
      TestWait(inboundEmpty0, '1', "port0 frame sent");
      TestWait(inboundEmpty1, '1', "port0 frame sent");
      TestWait(inboundEmpty2, '1', "port0 frame sent");
      TestWait(outboundEmpty0, '1', "port0 frame received");
      TestWait(outboundEmpty1, '1', "port0 frame received");
      TestWait(outboundEmpty2, '1', "port0 frame received");
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure ConfigRead(constant address : std_logic_vector(23 downto 0);
                         constant data : std_logic_vector(31 downto 0)) is
    begin
      TestPortWishboneWrite(writeSignal=>messageWrite,
                            messageSignal=>message,
                            ackSignal=>messageAck,
                            writeAccess=>false,
                            address=>(x"00000000" & x"00" & address),
                            byteSelect=>x"00",
                            data=>(x"00000000" & data),
                            latency=>3);
    end procedure;
    procedure ConfigWrite(constant address : std_logic_vector(23 downto 0);
                          constant data : std_logic_vector(31 downto 0)) is
    begin
      TestPortWishboneWrite(writeSignal=>messageWrite,
                            messageSignal=>message,
                            ackSignal=>messageAck,
                            writeAccess=>true,
                            address=>(x"00000000" & x"00" & address),
                            byteSelect=>x"00",
                            data=>(x"00000000" & data),
                            latency=>3);
    end procedure;
    
    procedure vPrint(constant text1 : in string := " ") is
    begin
       assert verboseInfo=false report text1 severity note;
    end procedure;
    
    variable lpsAddr : integer range 0 to 16#0FFFF#;  --used for LP-serial Extended Features config.space base Address
    variable embAddr : integer range 0 to 16#0FFFF#;  --used for Error Management Extended Features config.space base Address
    
    -- These variabels are needed for the random number generation.
    variable seed1 : positive := 1;
    variable seed2: positive := 1;

    variable maintData : DoubleWordArray(0 to 7);
    variable data : DoubleWordArray(0 to 31);
    variable randomPayload : RioPayload;
    variable randomPayload1 : RioPayload;
    variable randomPayload2 : RioPayload;
    variable frame : RioFrame;
    
  begin
  
    areset_n <= '0';

    linkInitialized <= (others=>'0');
      
    writeFrameFull <= (others=>'0');
    for portIndex in 0 to PORTS-1 loop
      inboundWrite0 <= '0';
      outboundWrite0 <= '0';
      inboundWrite1 <= '0';
      outboundWrite1 <= '0';
      inboundWrite2 <= '0';
      outboundWrite2 <= '0';
      localAckIdWrite(portIndex) <= '0';
      clrOutstandingAckId(portIndex) <= '0';
      inboundAckIdWrite(portIndex) <= (others=>'0');
      outstandingAckIdWrite(portIndex) <= (others=>'0');
      outboundAckIdWrite(portIndex) <= (others=>'0');
    end loop;
    
    wait until clk'event and clk = '1';
    wait until clk'event and clk = '1';
    areset_n <= '1';

    wait until clk'event and clk = '1';
    wait until clk'event and clk = '1';

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioSwitch");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioSwitch-TC1");
    TestSpec("Description: Test switch maintenance accesses on different ports.");
    TestSpec("Requirement: XXXXX");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Send maintenance read request packets to read switch identity.");
    TestSpec("Result: The switch should answer with its configured identitiy.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC1-Step1");
    ---------------------------------------------------------------------------
    
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"00", address=>x"000000", data=>(SWITCH_IDENTITY & SWITCH_VENDOR_IDENTITY));
    ReadConfig32(portIndex=>1, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"01", address=>x"000004", data=>SWITCH_REV);
    ReadConfig32(portIndex=>2, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"02", address=>x"000008", data=>(SWITCH_ASSY_IDENTITY & SWITCH_ASSY_VENDOR_IDENTITY));
    ReadConfig32(portIndex=>3, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"03", address=>x"00000c", data=>(SWITCH_ASSY_REV & x"0100"));

    ExchangeFrames;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 2:");
    TestSpec("Action: Check the switch Processing Element Features.");
    TestSpec("Result: The expected switch features should be returned. ");
    TestSpec("        Switch with extended features pointer valid. Common ");
    TestSpec("        transport large system support and standard route table ");
    TestSpec("        configuration support.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC1-Step2");
    ---------------------------------------------------------------------------

    ReadConfig32(portIndex=>4, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"04", address=>x"000010", data=>x"10000118");

    ExchangeFrames;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 3:");
    TestSpec("Action: Check the switch port information.");
    TestSpec("Result: The expected port and number of ports should be returned.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC1-Step3");
    ---------------------------------------------------------------------------

    ReadConfig32(portIndex=>2, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"05", address=>x"000014", data=>x"00000302");

    ExchangeFrames;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 4:");
    TestSpec("Action: Check the switch number of supported routes.");
    TestSpec("Result: The expected number of supported routes should be returned.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC1-Step4");
    ---------------------------------------------------------------------------

    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"000034", data=>x"00000800");

    ExchangeFrames;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 5:");
    TestSpec("Action: Test host base device id lock by reading it, then hold it ");
    TestSpec("        and try to grab it from another address.");
    TestSpec("Result: The value should follow the specification.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC1-Step5");
    ---------------------------------------------------------------------------

    -- Check that the lock is released.
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"00", address=>x"000068", data=>x"0000ffff");

    -- Try to accuire the lock.
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"01", address=>x"000068", data=>x"00000002");

    -- Check that the lock has been accuired.
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"02", address=>x"000068", data=>x"00000002");

    -- Try to accuire the lock from another source.
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"03", address=>x"000068", data=>x"00000003");

    -- Check that the lock refuses the new access.
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"04", address=>x"000068", data=>x"00000002");

    -- Release the lock.
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"05", address=>x"000068", data=>x"00000002");

    -- Check that the lock is released.
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"000068", data=>x"0000ffff");
    
    -- Check that the lock can be accuired from another source once unlocked.
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"07", address=>x"000068", data=>x"00000003");
    
    -- Check that the lock is released.
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"08", address=>x"000068", data=>x"00000003");
    
    -- Release the lock again.
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"09", address=>x"000068", data=>x"00000003");
    
    -- Check that the lock is released.
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"0a", address=>x"000068", data=>x"0000ffff");
    
    ExchangeFrames;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 6");
    TestSpec("Action: Check the component tag register.");
    TestSpec("Result: The written value in the component tag should be saved.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC1-Step6");
    ---------------------------------------------------------------------------

    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"00006c", data=>x"00000000");

    WriteConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"06", address=>x"00006c", data=>x"ffffffff");

    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"00006c", data=>x"ffffffff");

    ExchangeFrames;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 7");
    TestSpec("Action: Read and write to the port link timeout.");
    TestSpec("Result: Check that the portLinkTimeout output from the switch changes.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC1-Step7");
    ---------------------------------------------------------------------------

    assert portLinkTimeout = x"ffffff" report "Unexpected portLinkTimeout." severity error;
    
    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"000120", data=>x"ffffff00");

    WriteConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"06", address=>x"000120", data=>x"00000100");

    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"000120", data=>x"00000100");

    ExchangeFrames;

    assert portLinkTimeout = x"000001" report "Unexpected portLinkTimeout." severity error;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 8");
    TestSpec("Action: Read from the port general control.");
    TestSpec("Result: Check the discovered bit.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC1-Step8");
    ---------------------------------------------------------------------------

    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"00013c", data=>x"00000000");

    WriteConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"06", address=>x"00013c", data=>x"20000000");

    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"00013c", data=>x"20000000");

    WriteConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"06", address=>x"00013c", data=>x"00000000");

    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"00013c", data=>x"00000000");

    ExchangeFrames;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 9");
    TestSpec("Action: Read from the port N error and status.");
    TestSpec("Result: Check the port ok and port uninitialized bits.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC1-Step9");
    ---------------------------------------------------------------------------

    linkInitialized(0) <= '0';
    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"000158", data=>x"00000001");
    ExchangeFrames;
    linkInitialized(0) <= '1';
    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"000158", data=>x"00000002");
    ExchangeFrames;

    linkInitialized(1) <= '0';
    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"000178", data=>x"00000001");
    ExchangeFrames;
    linkInitialized(1) <= '1';
    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"000178", data=>x"00000002");
    ExchangeFrames;

    linkInitialized(2) <= '0';
    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"000198", data=>x"00000001");
    ExchangeFrames;
    linkInitialized(2) <= '1';
    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"000198", data=>x"00000002");
    ExchangeFrames;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 10");
    TestSpec("Action: Read and write to/from the port N control.");
    TestSpec("Result: Check the output/input port enable.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC1-Step10");
    ---------------------------------------------------------------------------

    assert outputPortEnable(0) = '0' report "Unexpected outputPortEnable." severity error;
    assert inputPortEnable(0) = '0' report "Unexpected inputPortEnable." severity error;
    
    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"00015c", data=>x"00000001");

    WriteConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"06", address=>x"00015c", data=>x"00600001");
    
    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"00015c", data=>x"00600001");

    ExchangeFrames;

    assert outputPortEnable(0) = '1' report "Unexpected outputPortEnable." severity error;
    assert inputPortEnable(0) = '1' report "Unexpected inputPortEnable." severity error;

    ---------------------------------------------------------------------------
    
    assert outputPortEnable(1) = '0' report "Unexpected outputPortEnable." severity error;
    assert inputPortEnable(1) = '0' report "Unexpected inputPortEnable." severity error;
    
    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"00017c", data=>x"00000001");

    WriteConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"06", address=>x"00017c", data=>x"00600001");
    
    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"00017c", data=>x"00600001");

    ExchangeFrames;

    assert outputPortEnable(1) = '1' report "Unexpected outputPortEnable." severity error;
    assert inputPortEnable(1) = '1' report "Unexpected inputPortEnable." severity error;

    ---------------------------------------------------------------------------
    
    assert outputPortEnable(2) = '0' report "Unexpected outputPortEnable." severity error;
    assert inputPortEnable(2) = '0' report "Unexpected inputPortEnable." severity error;
    
    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"00019c", data=>x"00000001");

    WriteConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"06", address=>x"00019c", data=>x"00600001");
    
    ReadConfig32(portIndex=>6, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"00019c", data=>x"00600001");

    ExchangeFrames;

    assert outputPortEnable(2) = '1' report "Unexpected outputPortEnable." severity error;
    assert inputPortEnable(2) = '1' report "Unexpected inputPortEnable." severity error;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 11");
    TestSpec("Action: Read and write to/from the implementation defined space.");
    TestSpec("Result: Check the accesses on the external configuration port.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC1-Step11");
    ---------------------------------------------------------------------------

    CreateRandomPayload(maintData, seed1, seed2);
    ConfigRead(x"010000", maintData(0)(63 downto 32));
    ConfigRead(x"010004", maintData(0)(31 downto 0));
    ConfigRead(x"010008", maintData(1)(63 downto 32));
    ConfigRead(x"01000c", maintData(1)(31 downto 0));
    ConfigRead(x"010010", maintData(2)(63 downto 32));
    ConfigRead(x"010014", maintData(2)(31 downto 0));
    ConfigRead(x"010018", maintData(3)(63 downto 32));
    ConfigRead(x"01001c", maintData(3)(31 downto 0));
    ConfigRead(x"010020", maintData(4)(63 downto 32));
    ConfigRead(x"010024", maintData(4)(31 downto 0));
    ConfigRead(x"010028", maintData(5)(63 downto 32));
    ConfigRead(x"01002c", maintData(5)(31 downto 0));
    ConfigRead(x"010030", maintData(6)(63 downto 32));
    ConfigRead(x"010034", maintData(6)(31 downto 0));
    ConfigRead(x"010038", maintData(7)(63 downto 32));
    ConfigRead(x"01003c", maintData(7)(31 downto 0));

    SendFrame(2, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                destId=>x"0000", sourceId=>x"dead", 
                                payload=>RioMaintenance(transaction=>"0000",
                                                        size=>"1100",
                                                        tid=>x"ef",
                                                        hopCount=>x"00",
                                                        configOffset=>"000000010000000000000",
                                                        wdptr=>'1',
                                                        dataLength=>0,
                                                        data=>maintData)));

    ReceiveFrame(2, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                   tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                   destId=>x"dead", sourceId=>x"0000", 
                                   payload=>RioMaintenance(transaction=>"0010",
                                                           size=>"0000",
                                                           tid=>x"ef",
                                                           hopCount=>x"ff",
                                                           configOffset=>"000000000000000000000",
                                                           wdptr=>'0',
                                                           dataLength=>8,
                                                           data=>maintData)));

    ExchangeFrames;
    TestWait(messageEmpty, '1', "config read");
    
    CreateRandomPayload(maintData, seed1, seed2);
    ConfigWrite(x"010000", maintData(0)(63 downto 32));
    ConfigWrite(x"010004", maintData(0)(31 downto 0));
    ConfigWrite(x"010008", maintData(1)(63 downto 32));
    ConfigWrite(x"01000c", maintData(1)(31 downto 0));
    ConfigWrite(x"010010", maintData(2)(63 downto 32));
    ConfigWrite(x"010014", maintData(2)(31 downto 0));
    ConfigWrite(x"010018", maintData(3)(63 downto 32));
    ConfigWrite(x"01001c", maintData(3)(31 downto 0));
    ConfigWrite(x"010020", maintData(4)(63 downto 32));
    ConfigWrite(x"010024", maintData(4)(31 downto 0));
    ConfigWrite(x"010028", maintData(5)(63 downto 32));
    ConfigWrite(x"01002c", maintData(5)(31 downto 0));
    ConfigWrite(x"010030", maintData(6)(63 downto 32));
    ConfigWrite(x"010034", maintData(6)(31 downto 0));
    ConfigWrite(x"010038", maintData(7)(63 downto 32));
    ConfigWrite(x"01003c", maintData(7)(31 downto 0));

    SendFrame(2, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                destId=>x"0000", sourceId=>x"dead", 
                                payload=>RioMaintenance(transaction=>"0001",
                                                        size=>"1100",
                                                        tid=>x"ef",
                                                        hopCount=>x"00",
                                                        configOffset=>"000000010000000000000",
                                                        wdptr=>'1',
                                                        dataLength=>8,
                                                        data=>maintData)));

    ReceiveFrame(2, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                   tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                   destId=>x"dead", sourceId=>x"0000", 
                                   payload=>RioMaintenance(transaction=>"0011",
                                                           size=>"0000",
                                                           tid=>x"ef",
                                                           hopCount=>x"ff",
                                                           configOffset=>"000000000000000000000",
                                                           wdptr=>'0',
                                                           dataLength=>0,
                                                           data=>maintData)));

    ExchangeFrames;
    TestWait(messageEmpty, '1', "config write");
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioSwitch-TC2");
    TestSpec("Description: Test the configuration of the routing table and the ");
    TestSpec("             routing of packets.");
    TestSpec("Requirement: XXXXX");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Configure the routing table for address 0->port 1.");
    TestSpec("Result: A packet to address 0 should be forwarded to port 1.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC2-Step1");
    ---------------------------------------------------------------------------

    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"07", address=>x"000070", data=>x"00000000");
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"08", address=>x"000074", data=>x"00000001");
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"09", address=>x"000074", data=>x"00000001");

    -- Send a frame from a port and check if it is correctly routed.
    randomPayload.length := 3;
    CreateRandomPayload(randomPayload.data, seed1, seed2);
    RouteFrame(sourcePortIndex=>0, destinationPortIndex=>1,
               sourceId=>x"ffff", destinationId=>x"0000", payload=>randomPayload);

    ExchangeFrames;
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 2:");
    TestSpec("Action: Test the configuration of the default route->port 2.");
    TestSpec("Result: An unknown address should be routed to port 2.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC2-Step2");
    ---------------------------------------------------------------------------

    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"0a", address=>x"000078", data=>x"00000000");
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"0b", address=>x"000078", data=>x"00000002");
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"0c", address=>x"000078", data=>x"00000002");
    ExchangeFrames;
    
    -- Send a frame from a port and check if it is correctly routed.
    randomPayload.length := 4;
    CreateRandomPayload(randomPayload.data, seed1, seed2);
    RouteFrame(sourcePortIndex=>1, destinationPortIndex=>2,
               sourceId=>x"0000", destinationId=>x"ffff", payload=>randomPayload);
    ExchangeFrames;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 3:");
    TestSpec("Action: Test to route a maintenance read request from port 2, ");
    TestSpec("        address 0.");
    TestSpec("Result: The packet should be routed to port 1 and hop decremented.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC2-Step3");
    ---------------------------------------------------------------------------

    SendFrame(2, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                destId=>x"0000", sourceId=>x"dead", 
                                payload=>RioMaintenance(transaction=>"0000",
                                                        size=>"1000",
                                                        tid=>x"be",
                                                        hopCount=>x"01",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>0,
                                                        data=>maintData)));
    
    ReceiveFrame(1, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                   tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                   destId=>x"0000", sourceId=>x"dead", 
                                   payload=>RioMaintenance(transaction=>"0000",
                                                           size=>"1000",
                                                           tid=>x"be",
                                                           hopCount=>x"00",
                                                           configOffset=>"000000000000000000000",
                                                           wdptr=>'0',
                                                           dataLength=>0,
                                                           data=>maintData)));

    ExchangeFrames;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 4:");
    TestSpec("Action: Test to route a maintenance write request from port 2, ");
    TestSpec("        address 0.");
    TestSpec("Result: The packet should be routed to port 1 and hop decremented.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC2-Step4");
    ---------------------------------------------------------------------------

    CreateRandomPayload(maintData, seed1, seed2);
    SendFrame(2, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                destId=>x"0000", sourceId=>x"dead", 
                                payload=>RioMaintenance(transaction=>"0001",
                                                        size=>"1100",
                                                        tid=>x"ef",
                                                        hopCount=>x"01",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'1',
                                                        dataLength=>8,
                                                        data=>maintData)));
    
    ReceiveFrame(1, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                   tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                   destId=>x"0000", sourceId=>x"dead", 
                                   payload=>RioMaintenance(transaction=>"0001",
                                                           size=>"1100",
                                                           tid=>x"ef",
                                                           hopCount=>x"00",
                                                           configOffset=>"000000000000000000000",
                                                           wdptr=>'1',
                                                           dataLength=>8,
                                                           data=>maintData)));
    ExchangeFrames;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 5:");
    TestSpec("Action: Test to route a maintenance read response from port 2, ");
    TestSpec("        address 0.");
    TestSpec("Result: The packet should be routed to port 1 and hop decremented.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC2-Step5");
    ---------------------------------------------------------------------------

    CreateRandomPayload(maintData, seed1, seed2);
    SendFrame(2, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                destId=>x"0000", sourceId=>x"dead", 
                                payload=>RioMaintenance(transaction=>"0010",
                                                        size=>"1100",
                                                        tid=>x"ef",
                                                        hopCount=>x"01",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>8,
                                                        data=>maintData)));
    
    ReceiveFrame(1, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                   tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                   destId=>x"0000",sourceId=>x"dead", 
                                   payload=>RioMaintenance(transaction=>"0010",
                                                           size=>"1100",
                                                           tid=>x"ef",
                                                           hopCount=>x"00",
                                                           configOffset=>"000000000000000000000",
                                                           wdptr=>'0',
                                                           dataLength=>8,
                                                           data=>maintData)));
    ExchangeFrames;
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 6:");
    TestSpec("Action: Test to route a maintenance write response from port 2, ");
    TestSpec("        address 0.");
    TestSpec("Result: The packet should be routed to port 1 and hop decremented.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC2-Step6");
    ---------------------------------------------------------------------------

    SendFrame(2, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                destId=>x"0000", sourceId=>x"dead", 
                                payload=>RioMaintenance(transaction=>"0011",
                                                        size=>"1000",
                                                        tid=>x"ef",
                                                        hopCount=>x"01",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>0,
                                                        data=>maintData)));
    
    ReceiveFrame(1, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                   tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                   destId=>x"0000", sourceId=>x"dead", 
                                   payload=>RioMaintenance(transaction=>"0011",
                                                           size=>"1000",
                                                           tid=>x"ef",
                                                           hopCount=>x"00",
                                                           configOffset=>"000000000000000000000",
                                                           wdptr=>'0',
                                                           dataLength=>0,
                                                           data=>maintData)));
    ExchangeFrames;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 7:");
    TestSpec("Action: ");
    TestSpec("Result: ");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC2-Step7");
    ---------------------------------------------------------------------------

    maintData(0) := x"0123456789abcdef";
    maintData(1) := x"0011223344550100";
    maintData(2) := x"1000011800000302";
    maintData(3) := x"0000000000000000";
    maintData(4) := x"0000000000000000";
    maintData(5) := x"0000000000000000";
    maintData(6) := x"0000000000000800";
    maintData(7) := x"0000000000000000";

    SendFrame(2, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                destId=>x"0000", sourceId=>x"dead", 
                                payload=>RioMaintenance(transaction=>"0001",
                                                        size=>"1100",
                                                        tid=>x"ef",
                                                        hopCount=>x"00",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'1',
                                                        dataLength=>8,
                                                        data=>maintData)));

    ReceiveFrame(2, RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                   tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                   destId=>x"dead", sourceId=>x"0000", 
                                   payload=>RioMaintenance(transaction=>"0011",
                                                           size=>"0000",
                                                           tid=>x"ef",
                                                           hopCount=>x"ff",
                                                           configOffset=>"000000000000000000000",
                                                           wdptr=>'0',
                                                           dataLength=>0,
                                                           data=>maintData)));

    ExchangeFrames;
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioSwitch-TC3");
    TestSpec("Description: Test the routing of normal packets.");
    TestSpec("Requirement: XXXXX");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Send two packets but not the same time.");
    TestSpec("Result: Both packets should be received at the expected ports.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC3-Step1");
    ---------------------------------------------------------------------------

    -- Setup the routing table for the following steps.
    RouteSet(x"0000", x"00");
    RouteSet(x"0001", x"00");
    RouteSet(x"0002", x"00");
    RouteSet(x"0003", x"00");
    RouteSet(x"0004", x"01");
    RouteSet(x"0005", x"02");
    RouteSet(x"0006", x"02");
    RouteSetDefault(x"02");
    
    -- Frame on port 0 to port 1.
    randomPayload.length := 3;
    CreateRandomPayload(randomPayload.data, seed1, seed2);
    SendFrame(portIndex=>0,
              destinationId=>x"0004", sourceId=>x"ffff", payload=>randomPayload);
    ReceiveFrame(portIndex=>1,
                 destinationId=>x"0004", sourceId=>x"ffff", payload=>randomPayload);

    -- Frame on port 1 to port 6.
    randomPayload.length := 4;
    CreateRandomPayload(randomPayload.data, seed1, seed2);
    SendFrame(portIndex=>1,
              destinationId=>x"ffff", sourceId=>x"0000", payload=>randomPayload);
    ReceiveFrame(portIndex=>6,
                 destinationId=>x"ffff", sourceId=>x"0000", payload=>randomPayload);

    ExchangeFrames;
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 2:");
    TestSpec("Action: Send two packets to the same port with is full and one to");
    TestSpec("        another that is also full. Then receive the packets one at");
    TestSpec("        a time.");
    TestSpec("Result: The packet to the port that is ready should go though.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioSwitch-TC3-Step2");
    ---------------------------------------------------------------------------

    -- Frame on port 0 to port 1.
    randomPayload.length := 5;
    CreateRandomPayload(randomPayload.data, seed1, seed2);
    RouteFrame(sourcePortIndex=>0, destinationPortIndex=>1,
               destinationId=>x"0000", sourceId=>x"ffff", payload=>randomPayload);

    -- Frame on port 1 to port 2.
    randomPayload1.length := 6;
    CreateRandomPayload(randomPayload1.data, seed1, seed2);
    RouteFrame(sourcePortIndex=>2, destinationPortIndex=>2,
               destinationId=>x"ffff", sourceId=>x"0000", payload=>randomPayload1);

    -- Frame on port 2 to port 2.
    randomPayload2.length := 7;
    CreateRandomPayload(randomPayload2.data, seed1, seed2);
    RouteFrame(sourcePortIndex=>2, destinationPortIndex=>2,
               destinationId=>x"ffff", sourceId=>x"0000", payload=>randomPayload2);

    writeFrameFull <= (others=>'1');
    wait for 10 us;

    writeFrameFull(1) <= '0';
    wait for 10 us;
    
    writeFrameFull(2) <= '0';
    wait for 10 us;
    
    ExchangeFrames;
    writeFrameFull <= (others=>'0');
   
    wait for 10 us;             
    PrintS("-----------------------------------------------------------------");
    PrintS("TG_RioSwitch-TC4");
    PrintS("Description: Test Hot Swap (error reporting) Event mechanisms.");
    PrintS("Requirement: XXXXX");
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 1:");
    PrintS("Action: Send maintenance write packets to prepare Error Reporting events.");
    PrintS("Result: The switch should Acknowledge writes.");
    ---------------------------------------------------------------------------
    PrintR("TG_RioSwitch-TC4-Step1");
    ---------------------------------------------------------------------------
    --
    lpsAddr:=16#0100#; --Base address of the LP_serial Block, within configuration space.
    embAddr:=16#0200#; --Base address of the Error Management Block, within configuration space.
    --
    vPrint("Check that 'Assembly Informaiton CAR' point to LP-Serial EF-block.");
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"01", address=>x"00000c", data=>(SWITCH_ASSY_REV & std_logic_vector(to_unsigned(lpsAddr,16))));
    --
    vPrint("Check that EF is 'LP-Serial Register Block Header' and points to Error Management EF-block.");
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"02", address=>std_logic_vector(to_unsigned(lpsAddr,24)), data=>(std_logic_vector(to_unsigned(embAddr,16)) & x"0003")); --expect address to error management block & x"0003" for LP-s.
    --
    vPrint("Check that EF is 'Error Management/Hot-Swap Register Block Header' and points to 0=N/A");
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"03", address=>std_logic_vector(to_unsigned(embAddr,24)), data=>(x"0000_0017")); --expect address 0 & x"0017" for hot-swap (later when error mangement is implemented, changes to x"0007").
    --
    vPrint("Check 'Error Managment/HotSwap Extensions Block CAR'");
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"04", address=>std_logic_vector(to_unsigned(embAddr+16#004#,24)), data=>(x"C000_0000"));
    ---------------------------------------------------------------------------
    
    linkInitialized(0) <= '1'; --assume that normal case is "link up" when HotSwap-bits is being configured by the CPU..
    --
    destIdErrorEvent <= x"E770";  --Destination Address for all Error Event Port-Writes
    expectedDestPort <= 0;        --Port where the port-write is expected to appear, i.e. default route
    --
    componentTag<=x"ABBA_5AAB";   --A unique Tag for the Device Under Test..
    wait for 10 us;             
    --
    vPrint("WriteConfig reg. at address " & integer'image(embAddr+16#028#) & " (set Target Device Id)"); 
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"f0", address=>std_logic_vector(to_unsigned(embAddr+16#028#,24)), data=>destIdErrorEvent & x"8000"); -- Port Write Target deviceID CSR:  Target device address: E770 (the receiver of the event notifications)
    ExchangeFrames;
    vPrint("Check 'Port Write Target deviceID CSR'");
    ReadConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"04", address=>std_logic_vector(to_unsigned(embAddr+16#028#,24)), data=>(destIdErrorEvent & x"8000"));
    
     --make sure that the event notifications are not disabled             
    vPrint("WriteConfig reg. at address " & integer'image(embAddr+16#034#) & " (unset the Disable Event Notifications bit)");
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
              tid=>x"f1", address=>std_logic_vector(to_unsigned(embAddr+16#034#,24)), data=>x"0000_0000"); -- Port Write Transmission Control CSR: Bit 0, '1' enabled events shall not cause new port writes to be generated, '0'=port writes shall be generated.
    ExchangeFrames;
  
    vPrint("WriteConfig reg. at address x""06c"" (configure Component Tag Id)"); --configure Component Tag Id.
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"f2", address=>x"00006c", data=>componentTag);

    ExchangeFrames;
    
    vPrint("WriteConfig reg. at address x""078"" (configure Default Route)"); --configure default route 
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"f3", address=>x"000078", data=>(others=>'0')); --set defult route to port 0
    ExchangeFrames;

    PrintS("-----------------------------------------------------------------");
    PrintS("Step 2:");
    PrintS("Action: Loop through all ports and ..");
    PrintS("   A)    ..set LinkInitialized<='0' to initiate a HotSwap event, then");
    PrintS("   B)    ..set LinkInitialized<='1' to initiate a HotSwap event.     ");
    PrintS("Result: The switch should generate a Port-Write for each event.");
    
    for i in 0 to PORTS-1 loop             
       --activate events for link OK->NOK, and NOK->OK, but not the discard timer event
       vPrint("WriteConfig reg. 'Port N Error Rate Enable CSR' at address " & integer'image(embAddr+16#044#+16#0040#*i) & " Port" & integer'image(i) & " (Activate events for linkOk2nok and linkNok2ok transitions)"); 
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"f4", address=> std_logic_vector(to_unsigned(embAddr+16#044#+16#0040#*i,24)), data=>x"5000_0000"); -- Port N Error Rate Enable CSR. : 
       ExchangeFrames;
     
       --clear error detect register  
       vPrint("WriteConfig reg. 'Port N Error Detect CSR' at address " & integer'image(embAddr+16#040#+16#0040#*i) & " Port" & integer'image(i) & " (clear error detect register)"); 
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"f5", address=> std_logic_vector(to_unsigned(embAddr+16#040#+16#0040#*i,24)), data=>x"0000_0000"); -- Port N Error Detect CSR. :
       ExchangeFrames;
       
       --To clear the port-write-pending bit, a '0' to '1' event must be generated.
       vPrint("WriteConfig reg. 'Port n Error and Status CSR' at address " & integer'image(lpsAddr+16#058#+16#0020#*i) & " Port" & integer'image(i) & " (Clear Port-Write-Pending bit)");
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e0", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0002"); -- Port n Error and Status CSR
       ExchangeFrames;
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e1", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0012"); -- Port n Error and Status CSR
       ExchangeFrames;
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e2", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0002"); -- Port n Error and Status CSR
       ExchangeFrames;
    end loop;

    for i in 0 to PORTS-1 loop             
       ---------------------------------------------------------------------------
       PrintR("TG_RioSwitch-TC4-Step2A:PORT" & integer'image(i));
       ---------------------------------------------------------------------------
       --setup expected response for linkOKtoUninit event:    
       PortNerrorDetectCSR<=x"4000_0000"; -- the Link_Ok_To_Uninit bit
       wait for 0 ns;
       maintData(0):=componentTag & PortNerrorDetectCSR;
       maintData(1):=x"0000_00" & std_logic_vector(to_unsigned(i,8)) & x"0000_0000";
       ReceiveFrame(portIndex=>expectedDestPort, frame=>RioFrameCreate(ackId=>b"00000", vc=>'0', crf=>'0', prio=>b"00",
                                                                       tt=>tt_16bit, ftype=>FTYPE_MAINTENANCE_CLASS, 
                                                                       sourceId=>x"0000", destId=>destIdErrorEvent,
                                                                       payload=>RioMaintenance(transaction => b"0100",
                                                                                               size        => b"1011",
                                                                                               tid         => x"00",
                                                                                               hopCount    => x"ff",
                                                                                               configOffset=> b"000000000000000000000",
                                                                                               wdptr       =>  '1',
                                                                                               dataLength  =>   2,
                                                                                               data=>maintData)));
       
       vPrint("linkInit '0', Port" & integer'image(i));
       linkInitialized(i)<='0';
       
       ExchangeFrames;
       
       --clear error detect register
       vPrint("WriteConfig reg. 'Port N Error Detect CSR' at address " & integer'image(embAddr+16#040#+16#0040#*i) & "Port" & integer'image(i) & " (clear error detect register)"); 
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"f6", address=>std_logic_vector(to_unsigned(embAddr+16#040#+16#0040#*i,24)), data=>x"0000_0000"); -- Port N Error Detect CSR. :
       ExchangeFrames;
       
       --To clear the port-write-pending bit, a '0' to '1' event must be generated.
       vPrint("WriteConfig reg. 'Port n Error and Status CSR' (1/2) at address " & integer'image(lpsAddr+16#058#+16#0020#*i) & " Port" & integer'image(i) & " (Clear Port-Write-Pending bit)");
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e3", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0011"); -- Port n Error and Status CSR
       ExchangeFrames;
       vPrint("WriteConfig reg. 'Port n Error and Status CSR' (2/2) at address " & integer'image(lpsAddr+16#058#+16#0020#*i) & " Port" & integer'image(i) & " (Clear Port-Write-Pending bit)");
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e5", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0001"); -- Port n Error and Status CSR
       ExchangeFrames;
       
       ---------------------------------------------------------------------------
       PrintR("TG_RioSwitch-TC4-Step2B:PORT" & integer'image(i));
       ---------------------------------------------------------------------------
       PortNerrorDetectCSR<=x"1000_0000"; -- the Link_Uninit_To_Ok bit
       wait for 0 ns;
       maintData(0):=componentTag & PortNerrorDetectCSR;
       maintData(1):=x"0000_00" & std_logic_vector(to_unsigned(i,8)) & x"0000_0000";
       ReceiveFrame(portIndex=>expectedDestPort, frame=>RioFrameCreate(ackId=>b"00000", vc=>'0', crf=>'0', prio=>b"00",
                                                                       tt=>tt_16bit, ftype=>FTYPE_MAINTENANCE_CLASS, 
                                                                       sourceId=>x"0000", destId=>destIdErrorEvent,
                                                                       payload=>RioMaintenance(transaction => b"0100",
                                                                                               size        => b"1011",
                                                                                               tid         => x"00",
                                                                                               hopCount    => x"ff",
                                                                                               configOffset=> b"000000000000000000000",
                                                                                               wdptr       =>  '1',
                                                                                               dataLength  =>   2,
                                                                                               data=>maintData)));
       vPrint("linkInit '1', Port" & integer'image(i));
       linkInitialized(i)<='1';
       
       ExchangeFrames;
       
       --clear error detect register
       vPrint("WriteConfig reg. 'Port N Error Detect CSR' at address " & integer'image(embAddr+16#040#+16#0040#*i) & " Port" & integer'image(i) & " (clear error detect register)"); 
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"f7", address=>std_logic_vector(to_unsigned(embAddr+16#040#+16#0040#*i,24)), data=>x"0000_0000");   -- Port N Error Detect CSR. :
       ExchangeFrames;

       --Clear Port-Write-Pending bit
       vPrint("WriteConfig reg. 'Port n Error and Status CSR' (1/2) at address " & integer'image(lpsAddr+16#058#+16#0020#*i) & " Port" & integer'image(i) & " (Clear Port-Write-Pending bit)");
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e3", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0012");  -- Port n Error and Status CSR
       ExchangeFrames;
       vPrint("WriteConfig reg. 'Port n Error and Status CSR' (2/2) at address " & integer'image(lpsAddr+16#058#+16#0020#*i) & " Port" & integer'image(i) & " (Clear Port-Write-Pending bit)");
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e5", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0002"); -- Port n Error and Status CSR
       ExchangeFrames;
       
    end loop;

    PrintS("-----------------------------------------------------------------");
    PrintS("Step 3:");
    PrintS("Action: For all ports at the same time..");
    PrintS("   A)    ..set LinkInitialized<='0',    ");
    PrintS("   B)    ..clear port-write Pending and wait for timeout, ");
    PrintS("   C)    ..set LinkInitialized<='1'                       ");
    PrintS("   D)    ..do Not clear port-write Pending                ");
    PrintS("Result: A)  The switch should generate a Port-Write for each port.   ");
    PrintS("        B)  The switch should Not generate a Port-Write for each port");
    PrintS("        C)  The switch should generate a Port-Write for each port.   ");
    PrintS("        D)  The switch should generate a Port-Write for each port,   ");
    PrintS("            after timeout.   ");
    ---------------------------------------------------------------------------
    PrintR("TG_RioSwitch-TC4-Step3A");
    ---------------------------------------------------------------------------
    --setup expected response for linkOKtoUninit event:    
    PortNerrorDetectCSR<=x"4000_0000"; -- the Link_Ok_To_Uninit bit
    for i in 0 to PORTS-1 loop
       wait for 0 ns;
       maintData(0):=componentTag & PortNerrorDetectCSR;
       maintData(1):=x"0000_00" & std_logic_vector(to_unsigned(i,8)) & x"0000_0000";
       ReceiveFrame(portIndex=>expectedDestPort, frame=>RioFrameCreate(ackId=>b"00000", vc=>'0', crf=>'0', prio=>b"00",
                                                                       tt=>tt_16bit, ftype=>FTYPE_MAINTENANCE_CLASS, 
                                                                       sourceId=>x"0000", destId=>destIdErrorEvent,
                                                                       payload=>RioMaintenance(transaction => b"0100",
                                                                                               size        => b"1011",
                                                                                               tid         => x"00",
                                                                                               hopCount    => x"ff",
                                                                                               configOffset=> b"000000000000000000000",
                                                                                               wdptr       =>  '1',
                                                                                               dataLength  =>   2,
                                                                                               data=>maintData)));
    end loop;
    
    --As it is not known which port will be sent first, depending on state of the tx test which use PortNindex,
    --we must wait for internal signal /testrioswitch/TestSwitch/MaintenancePort/PortNindex to reach a known state first. 
    --TestBench..ReceiveFrame expects port 0 to arrive first.
    wait until << signal .TestRioSwitch.TestSwitch.MaintenancePort.PortNindex : integer range 0 to PORTS >> = PORTS-1;
    vPrint("linkInit '0' on all ports");
    linkInitialized<=(others=>'0');
    
    ExchangeFrames;
    
    ---------------------------------------------------------------------------
    PrintR("TG_RioSwitch-TC4-Step3B");
    ---------------------------------------------------------------------------
    for i in 0 to PORTS-1 loop
       --clear error detect register
       vPrint("WriteConfig reg. 'Port N Error Detect CSR' at address " & integer'image(embAddr+16#040#+16#0040#*i) & " Port" & integer'image(i) & " (clear error detect register) 3B"); 
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"f8", address=>std_logic_vector(to_unsigned(embAddr+16#040#+16#0040#*i,24)), data=>x"0000_0000"); -- Port N Error Detect CSR. :
       ExchangeFrames;
       
       vPrint("WriteConfig reg. 'Port n Error and Status CSR' (1/2) at address " & integer'image(lpsAddr+16#058#+16#0020#*i) & " Port" & integer'image(i) & " (Clear Port-Write-Pending bit)");
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e3", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0011");  -- Port n Error and Status CSR
       ExchangeFrames;
       vPrint("WriteConfig reg. 'Port n Error and Status CSR' (2/2) at address " & integer'image(lpsAddr+16#058#+16#0020#*i) & " Port" & integer'image(i) & " (Clear Port-Write-Pending bit)");
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e5", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0001");  -- Port n Error and Status CSR
       ExchangeFrames;
    end loop;
    --
    wait for 80 us; --wait some time to verify that error-event-port-write is not re-triggered, as the port-write-pending bit has been cleared.
    wait until rising_edge(clk);
    
    ---------------------------------------------------------------------------
    PrintR("TG_RioSwitch-TC4-Step3C");
    ---------------------------------------------------------------------------
    for i in 0 to PORTS-1 loop
       PortNerrorDetectCSR <= x"1000_0000"; -- the Link_Uninit_To_Ok bit
       wait for 0 ns;
       maintData(0) := componentTag & PortNerrorDetectCSR;
       maintData(1) := x"0000_00" & std_logic_vector(to_unsigned(i,8)) & x"0000_0000";
       ReceiveFrame(portIndex=>expectedDestPort,
                    frame=>RioFrameCreate(ackId=>b"00000", vc=>'0', crf=>'0', prio=>b"00",
                                          tt=>tt_16bit, ftype=>FTYPE_MAINTENANCE_CLASS, 
                                          sourceId=>x"0000", destId=>destIdErrorEvent,
                                          payload=>RioMaintenance(transaction => b"0100",
                                                                         size => b"1011",
                                                                          tid => x"00",
                                                                     hopCount => x"ff",
                                                                 configOffset => b"000000000000000000000",
                                                                        wdptr => '1',
                                                                   dataLength =>  2,
                                                                         data => maintData)));
    end loop;
    
    --As we do not know which port will be sent first, depending on state of the tx test which use PortNindex,
    --we must wait for internal signal /testrioswitch/TestSwitch/MaintenancePort/PortNindex to reach a known state first. 
    --TestBench..ReceiveFrame expects port 0 to arrive first.
    wait until << signal .TestRioSwitch.TestSwitch.MaintenancePort.PortNindex : integer range 0 to PORTS >> = PORTS-1;
    vPrint("linkInit '1' on all ports (3C)");
    linkInitialized<=(others=>'1');
    
    ExchangeFrames;
    
    ---------------------------------------------------------------------------
    PrintR("TG_RioSwitch-TC4-Step3D");
    ---------------------------------------------------------------------------
    --now, do not clear the pending bit, thus expect a retransmission of the frames once the timeout has expired
    vPrint("Waiting for Port-Write-Pending-Ack-Timeout to expire, then a re-transmission.");
    for i in 0 to PORTS-1 loop
       PortNerrorDetectCSR <= x"1000_0000"; -- the Link_Uninit_To_Ok bit
       wait for 0 ns;
       maintData(0) := componentTag & PortNerrorDetectCSR;
       maintData(1) := x"0000_00" & std_logic_vector(to_unsigned(i,8)) & x"0000_0000";
       ReceiveFrame(portIndex=>expectedDestPort,
                    frame=>RioFrameCreate(ackId=>b"00000", vc=>'0', crf=>'0', prio=>b"00",
                                          tt=>tt_16bit, ftype=>FTYPE_MAINTENANCE_CLASS, 
                                          sourceId=>x"0000", destId=>destIdErrorEvent,
                                          payload=>RioMaintenance(transaction => b"0100",
                                                                         size => b"1011",
                                                                          tid => x"00",
                                                                     hopCount => x"ff",
                                                                 configOffset => b"000000000000000000000",
                                                                        wdptr => '1',
                                                                   dataLength =>  2,
                                                                         data => maintData)));
    end loop;
    ExchangeFrames; -- re-transmission (within ~ 48.8 us when test bench use generic x"0020")
    
    for i in 0 to PORTS-1 loop
       --clear error detect register
       vPrint("WriteConfig reg. 'Port N Error Detect CSR' at address " & integer'image(embAddr+16#040#+16#0040#*i) & "Port" & integer'image(i) & " (clear error detect register)"); 
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"f9", address=>std_logic_vector(to_unsigned(embAddr+16#040#+16#0040#*i,24)), data=>x"0000_0000"); -- Port N Error Detect CSR. :
       ExchangeFrames;
       
       vPrint("WriteConfig reg. 'Port n Error and Status CSR' (1/2) at address " & integer'image(lpsAddr+16#058#+16#0020#*i) & " Port" & integer'image(i) & " (Clear Port-Write-Pending bit)");
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e3", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0012");  -- Port n Error and Status CSR
       ExchangeFrames;
       vPrint("WriteConfig reg. 'Port n Error and Status CSR' (2/2) at address " & integer'image(lpsAddr+16#058#+16#0020#*i) & " Port" & integer'image(i) & " (Clear Port-Write-Pending bit)");
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e5", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0002");  -- Port n Error and Status CSR
       ExchangeFrames;
    end loop;

    
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 4:");
    PrintS("Action: Send for each port, a Maintenance Port-Write packet,");
    PrintS("        trough the switch via default route. (Test route-through)");
    PrintS("Result: The switch should route the packet and decrement Hop-count.");
    ---------------------------------------------------------------------------
    PrintR("TG_RioSwitch-TC4-Step4");
    ---------------------------------------------------------------------------
    expectedDestPort<=PORTS-1;
    wait for 0 ns;
    --configure default route 
    vPrint("WriteConfig reg. at address x""078"" (configure Default Route)");
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"fa", address=>x"000078", data=>x"0000_00" & std_logic_vector(to_unsigned(expectedDestPort,8))); --set defult route to port ..
    ExchangeFrames;
    
    for portIndex in 0 to PORTS-1 loop
       vPrint("TC4:Step4 - Routing from Port" & integer'image(portIndex) & " to Port" & integer'image(expectedDestPort));
       --
       maintData(0):=x"9876_5432_10FE_DCBA";
       maintData(1):=x"ABCD_EF01_2345_6789";
       SendFrame(portIndex=>portIndex,        -- Maintenence Port-Write Transaction
                frame=>RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>b"00", tt=>tt_16bit, 
                                      ftype=>FTYPE_MAINTENANCE_CLASS, sourceId=>x"D0BB", destId=>x"ADDA",  --dest Id is not in routing table -> default route.
                                      payload=>RioMaintenance(transaction => b"0100",
                                                                     size => b"1011",
                                                                      tid => x"00",
                                                                 hopCount => x"25",
                                                             configOffset => b"000000000000000000000",
                                                                    wdptr => '1',
                                                               dataLength =>  2,
                                                                     data => maintData)));
    
       ReceiveFrame(portIndex=>expectedDestPort, 
                frame=>RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>b"00", tt=>tt_16bit, 
                                      ftype=>FTYPE_MAINTENANCE_CLASS, sourceId=>x"D0BB", destId=>x"ADDA",
                                      payload=>RioMaintenance(transaction => b"0100",
                                                                     size => b"1011",
                                                                      tid => x"00",
                                                                 hopCount => x"24",
                                                             configOffset => b"000000000000000000000",
                                                                    wdptr => '1',
                                                               dataLength =>  2,
                                                                     data => maintData)));
       ExchangeFrames;
       
    end loop;
        
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 5:");
    PrintS("Action: Set PortWriteTransmissionDisable='1' and Loop through all ports and ..");
    PrintS("   A)    ..set LinkInitialized<='0' to initiate a HotSwap event, then");
    PrintS("   B)    ..set LinkInitialized<='1' to initiate a HotSwap event.     ");
    PrintS("   C)    ..Set PortWriteTransmissionDisable='0'.  ");
--  PrintS("   D)    ..set LinkInitialized<='0' to initiate a HotSwap event, then");
--  PrintS("   E)    ..set LinkInitialized<='1' to initiate a HotSwap event.     ");
    PrintS("Result:  For each event..");
    PrintS("   A)    The switch should Not generate a Port-Write for each port.");
    PrintS("   B)    The switch should Not generate a Port-Write for each port.");
    PrintS("   C)    The switch should Not generate a Port-Write for each port.");
--  PrintS("   D)    The switch should generate a Port-Write for each port.");
--  PrintS("   E)    The switch should generate a Port-Write for each port.");
    
    vPrint("WriteConfig reg. 'Port Write Transmission Control CSR' (PortWriteTransmissionDisable<='1')");
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"fa", address=>std_logic_vector(to_unsigned(embAddr+16#034#,24)), data=>x"0000_0001"); --disable port-writes globally within error management
    ExchangeFrames;
    --
    for i in 0 to PORTS-1 loop             
       --activate events for link OK->NOK, and NOK->OK, but not the discard timer event
       vPrint("WriteConfig reg. 'Port N Error Rate Enable CSR' at address " & integer'image(embAddr+16#044#+16#0040#*i) & " Port" & integer'image(i) & " (Activate events for linkOk2nok and linkNok2ok transitions)"); 
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"f4", address=> std_logic_vector(to_unsigned(embAddr+16#044#+16#0040#*i,24)), data=>x"5000_0000"); -- Port N Error Rate Enable CSR. : 
       ExchangeFrames;
     
       --clear error detect register  
       vPrint("WriteConfig reg. 'Port N Error Detect CSR' at address " & integer'image(embAddr+16#040#+16#0040#*i) & " Port" & integer'image(i) & " (clear error detect register)"); 
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"f5", address=> std_logic_vector(to_unsigned(embAddr+16#040#+16#0040#*i,24)), data=>x"0000_0000"); -- Port N Error Detect CSR. :
       ExchangeFrames;
       
       --To clear the port-write-pending bit, a '0' to '1' event must be generated.
       vPrint("WriteConfig reg. 'Port n Error and Status CSR' at address " & integer'image(lpsAddr+16#058#+16#0020#*i) & " Port" & integer'image(i) & " (Clear Port-Write-Pending bit)");
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e0", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0002"); -- Port n Error and Status CSR
       ExchangeFrames;
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e1", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0012"); -- Port n Error and Status CSR
       ExchangeFrames;
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e2", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0002"); -- Port n Error and Status CSR
       ExchangeFrames;
    end loop;

    for i in 0 to PORTS-1 loop             
       ---------------------------------------------------------------------------
       PrintR("TG_RioSwitch-TC4-Step5A:PORT" & integer'image(i));
       ---------------------------------------------------------------------------
       
    --- When 'port-Write Diabled' Enabled, no response is expected.
       vPrint("linkInit '0', Port" & integer'image(i));
       linkInitialized(i)<='0';
       
       wait for 80 us;
       wait until rising_edge(clk);
       
       ---------------------------------------------------------------------------
       PrintR("TG_RioSwitch-TC4-Step5B:PORT" & integer'image(i));
       ---------------------------------------------------------------------------
       
    --- When 'port-Write Diabled' Enabled, no response is expected.
       vPrint("linkInit '1', Port" & integer'image(i));
       linkInitialized(i)<='1';
       
       wait for 80 us;
       wait until rising_edge(clk);
       
    end loop;
    --
    ---------------------------------------------------------------------------
    PrintR("TG_RioSwitch-TC4-Step5C");
    ---------------------------------------------------------------------------
    vPrint("WriteConfig reg. 'Port Write Transmission Control CSR' (PortWriteTransmissionDisable<='0')");
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"fb", address=>std_logic_vector(to_unsigned(embAddr+16#034#,24)), data=>x"0000_0000"); --unDisable port-writes globally within error management
    ExchangeFrames;
    --
    wait for 80 us;
    wait until rising_edge(clk);
    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 6:");
    PrintS("Action: For each port; Set LinkUninitTimeout, activate event on  ");
    PrintS("        LinkUninitDiscardTimer, deassert linkInitialized, (wait).");
    PrintS("Result: After timeout, linkUninitPacketDiscardActive should be   ");
    PrintS("        set, and a port-write event should be generated.");
    ---------------------------------------------------------------------------
    PrintR("TG_RioSwitch-TC4-Step6");
    ---------------------------------------------------------------------------
    linkInitialized <= (others=>'1');
    
     --make sure that the event notifications are not disabled             
    vPrint("WriteConfig reg. at address " & integer'image(embAddr+16#034#) & " (unset the Disable Event Notifications bit)");
    WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
              tid=>x"f1", address=>std_logic_vector(to_unsigned(embAddr+16#034#,24)), data=>x"0000_0000"); -- Port Write Transmission Control CSR: Bit 0, '1' enabled events shall not cause new port writes to be generated, '0'=port writes shall be generated.
    ExchangeFrames;

    for i in 0 to PORTS-1 loop             
       --activate events for link discard timer event
       vPrint("WriteConfig reg. 'Port N Error Rate Enable CSR' at address " & integer'image(embAddr+16#044#+16#0040#*i) & " Port" & integer'image(i) & " (Activate event for linkOk2nok discard timer)"); 
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"f4", address=> std_logic_vector(to_unsigned(embAddr+16#044#+16#0040#*i,24)), data=>x"2000_0000"); -- Port N Error Rate Enable CSR. : 
       ExchangeFrames;
     
       --Define the Link Uninit Timeout
       vPrint("WriteConfig reg. 'Port N Link Uninit Discard Timer CSR' at address " & integer'image(embAddr+16#070#+16#0040#*i) & " Port" & integer'image(i)); 
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"f5", address=> std_logic_vector(to_unsigned(embAddr+16#070#+16#0040#*i,24)), data=>x"000064_00"); -- Port N Link Uninit Discard Timer CSR
       ExchangeFrames;                                                                          -- 600 ns/lsb  x"000064" = 60us
       
       --clear error detect register  
       vPrint("WriteConfig reg. 'Port N Error Detect CSR' at address " & integer'image(embAddr+16#040#+16#0040#*i) & " Port" & integer'image(i) & " (clear error detect register)"); 
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"f5", address=> std_logic_vector(to_unsigned(embAddr+16#040#+16#0040#*i,24)), data=>x"0000_0000"); -- Port N Error Detect CSR. :
       ExchangeFrames;
       
       --To clear the port-write-pending bit, a '0' to '1' event must be generated.
       vPrint("WriteConfig reg. 'Port n Error and Status CSR' at address " & integer'image(lpsAddr+16#058#+16#0020#*i) & " Port" & integer'image(i) & " (Clear Port-Write-Pending bit)");
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e0", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0002"); -- Port n Error and Status CSR
       ExchangeFrames;
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e1", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0012"); -- Port n Error and Status CSR
       ExchangeFrames;
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e2", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0002"); -- Port n Error and Status CSR
       ExchangeFrames;
       
       vPrint("linkInitialized<='0' for Port" & integer'image(i));
       linkInitialized(i) <= '0';  --to start timer..and make the ports expire in a defined sequence
       wait until rising_edge(clk);
    end loop;

    PortNerrorDetectCSR<=x"6000_0000"; -- Link Ok To Uninit transition and Link_Uninit Packet Discard Active (timer period expired)
    wait for 0 ns;
    for i in 0 to PORTS-1 loop             
       --setup expected response
       maintData(0):=componentTag & PortNerrorDetectCSR;
       maintData(1):=x"0000_00" & std_logic_vector(to_unsigned(i,8)) & x"0000_0000";
       ReceiveFrame(portIndex=>expectedDestPort, frame=>RioFrameCreate(ackId=>b"00000", vc=>'0', crf=>'0', prio=>b"00",
                                                                       tt=>tt_16bit, ftype=>FTYPE_MAINTENANCE_CLASS, 
                                                                       sourceId=>x"0000", destId=>destIdErrorEvent,
                                                                       payload=>RioMaintenance(transaction => b"0100",
                                                                                               size        => b"1011",
                                                                                               tid         => x"00",
                                                                                               hopCount    => x"ff",
                                                                                               configOffset=> b"000000000000000000000",
                                                                                               wdptr       =>  '1',
                                                                                               dataLength  =>   2,
                                                                                               data=>maintData)));
       ExchangeFrames;
       
       --To clear the port-write-pending bit, a '0' to '1' event must be generated.
       vPrint("WriteConfig reg. 'Port n Error and Status CSR' at address " & integer'image(lpsAddr+16#058#+16#0020#*i) & " Port" & integer'image(i) & " (Clear Port-Write-Pending bit)");
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e1", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0011"); -- Port n Error and Status CSR
       ExchangeFrames;
       WriteConfig32(portIndex=>0, destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                     tid=>x"e2", address=> std_logic_vector(to_unsigned(lpsAddr+16#058#+16#0020#*i,24)), data=>x"0000_0001"); -- Port n Error and Status CSR
       ExchangeFrames;
       
       vPrint("linkInitialized<='1' for Port" & integer'image(i));
       linkInitialized(i) <= '1';
       --Notice that No Event should be generated for the Uninit to OK transition
    end loop;
    --
    wait for 80 us;
    wait until rising_edge(clk);

    ---------------------------------------------------------------------------
    -- Test completed.
    ---------------------------------------------------------------------------

    TestEnd;
  end process;

  -----------------------------------------------------------------------------
  -- Instantiate a process receiving the configuration accesses to the
  -- implementation defined space.
  -----------------------------------------------------------------------------
  
  TestWishbone: TestPortWishbone
    generic map(ADDRESS_WIDTH=>24, SEL_WIDTH=>1, DATA_WIDTH=>32)
    port map(
      clk=>clk, areset_n=>areset_n, 
      messageEmpty_o=>messageEmpty, 
      messageWrite_i=>messageWrite, 
      message_i=>message, 
      messageAck_o=>messageAck, 
      cyc_i=>configStb, 
      stb_i=>configStb, 
      we_i=>configWe, 
      adr_i=>configAddr, 
      sel_i=>"0", 
      dat_i=>configDataWrite, 
      dat_o=>configDataRead, 
      err_o=>open, 
      ack_o=>configAck);
  
  -----------------------------------------------------------------------------
  -- Instantiate the test port array.
  -----------------------------------------------------------------------------
  inboundEmpty2 <= inboundEmpty(2);
  inboundEmpty1 <= inboundEmpty(1);
  inboundEmpty0 <= inboundEmpty(0);
  inboundWrite <= inboundWrite2 & inboundWrite1 & inboundWrite0;
  inboundMessage <= inboundMessage2 & inboundMessage1 & inboundMessage0;
  inboundAck2 <= inboundAck(2);
  inboundAck1 <= inboundAck(1);
  inboundAck0 <= inboundAck(0);
  outboundEmpty2 <= outboundEmpty(2);
  outboundEmpty1 <= outboundEmpty(1);
  outboundEmpty0 <= outboundEmpty(0);
  outboundWrite <= outboundWrite2 & outboundWrite1 & outboundWrite0;
  outboundMessage <= outboundMessage2 & outboundMessage1 & outboundMessage0;
  outboundAck2 <= outboundAck(2);
  outboundAck1 <= outboundAck(1);
  outboundAck0 <= outboundAck(0);
  TestPortGeneration: for portIndex in 0 to PORTS-1 generate
    TestPortPacketBufferInst: TestPortPacketBuffer
      generic map(READ_CONTENT_END_DATA_VALID=>false)
      port map(
        clk=>clk, areset_n=>areset_n, 
        readEmpty_o=>inboundEmpty(portIndex), 
        readWrite_i=>inboundWrite(portIndex), 
        readMessage_i=>inboundMessage(portIndex), 
        readAck_o=>inboundAck(portIndex), 
        writeEmpty_o=>outboundEmpty(portIndex), 
        writeWrite_i=>outboundWrite(portIndex), 
        writeMessage_i=>outboundMessage(portIndex), 
        writeAck_o=>outboundAck(portIndex), 
        readFrameEmpty_o=>readFrameEmpty(portIndex), 
        readFrame_i=>readFrame(portIndex), 
        readFrameRestart_i=>'0', 
        readFrameAborted_o=>open,
        readWindowEmpty_o=>open,
        readWindowReset_i=>'0',
        readWindowNext_i=>readFrame(portIndex),
        readContentEmpty_o=>open, 
        readContent_i=>readContent(portIndex), 
        readContentEnd_o=>readContentEnd(portIndex), 
        readContentData_o=>readContentData(portIndex), 
        writeFrame_i=>writeFrame(portIndex), 
        writeFrameAbort_i=>writeFrameAbort(portIndex), 
        writeContent_i=>writeContent(portIndex), 
        writeContentData_i=>writeContentData(portIndex));
  end generate;

end architecture;
