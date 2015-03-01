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
-- - Add testcases to NWRITE to cover all possible access lengths, not just the
--   maximum as presently.
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
                             constant addressIn : in std_logic_vector(30 downto 0);
                             constant byteSelect : in std_logic_vector(7 downto 0);
                             constant length : in natural range 1 to 32;
                             constant dataIn : in DoublewordArray(0 to 31);
                             constant latency : natural := 1) is
      variable address : std_logic_vector(ADDRESS_WIDTH_MAX-1 downto 0);
    begin
      address := x"00000000" & '0' & addressIn;
      for i in 0 to length-1 loop
        if (i = (length-1)) then
          TestPortWishboneWrite(wbMessageWrite, wbMessage, wbMessageAck,
                                writeAccess, address, byteSelect, dataIn(i), false, latency);
        else
          TestPortWishboneWrite(wbMessageWrite, wbMessage, wbMessageAck,
                                writeAccess, address, byteSelect, dataIn(i), true, latency);
          address := std_logic_vector(unsigned(address)+1);
        end if;
      end loop;
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
    -- Local variables.
    ---------------------------------------------------------------------------
    variable seed1 : positive := 1;
    variable seed2: positive := 1;

    variable rdsize : std_logic_vector(3 downto 0);
    variable wrsize : std_logic_vector(3 downto 0);
    variable wdptr : std_logic;
    variable maintData : DoubleWordArray(0 to 7);
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
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioWbBridge");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioWbBridge-TC1");
    TestSpec("Description: Test maintenance requests.");
    TestSpec("Requirement: XXXXX");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Send maintenance read request for one word on even offset.");
    TestSpec("Result: Check the accesses on the external configuration port.");
    TestSpec("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioWbBridge-TC1-Step1");
    ---------------------------------------------------------------------------

    InboundFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS,
                                sourceId=>x"dead", destId=>x"beef",
                                payload=>RioMaintenance(transaction=>"0000",
                                                        size=>"1000",
                                                        tid=>x"aa",
                                                        hopCount=>x"ff",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>0,
                                                        data=>maintData)));

    maintData(0) := x"deadbeef00000000";
    OutboundFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                 tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS,
                                 sourceId=>x"beef", destId=>x"dead",
                                 payload=>RioMaintenance(transaction=>"0010",
                                                         size=>"0000",
                                                         tid=>x"aa",
                                                         hopCount=>x"ff",
                                                         configOffset=>"000000000000000000000",
                                                         wdptr=>'0',
                                                         dataLength=>1,
                                                         data=>maintData)));
    
    TestWait(inboundEmpty, '1', "inbound frame");
    TestWait(outboundEmpty, '1', "outbound frame");
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioWbBridge-TC2");
    TestSpec("Description: Test request class packets.");
    TestSpec("Requirement: XXXXX");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Send request class NREAD packets for all sizes.");
    TestSpec("Result: The Wishbone access should match the inbound packet.");
    TestSpec("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioWbBridge-TC2-Step1");
    ---------------------------------------------------------------------------
    -- REMARK: Change the address and tid also...
    for i in 0 to 15 loop
      for j in 0 to 1 loop
        rdsize := std_logic_vector(to_unsigned(i, 4));
        if (j = 0) then
          wdptr := '0';
        else
          wdptr := '1';
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
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_RioWbBridge-TC3");
    TestSpec("Description: Test write class packets.");
    TestSpec("Requirement: XXXXX");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Send write class NWRITER packets for all sizes.");
    TestSpec("Result: The Wishbone access should match the inbound packet and a ");
    TestSpec("        response should be sent.");
    TestSpec("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioWbBridge-TC3-Step1");
    ---------------------------------------------------------------------------
    -- REMARK: Change the address and tid also...
    -- REMARK: Not really all sizes, add sizes in between the fixed as well.
    for i in 0 to 15 loop
      for j in 0 to 1 loop
        wrsize := std_logic_vector(to_unsigned(i, 4));
        if (j = 0) then
          wdptr := '0';
        else
          wdptr := '1';
        end if;
        
        CreateRandomPayload(ioData, seed1, seed2);

        InboundFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                    tt=>"01", ftype=>FTYPE_WRITE_CLASS, 
                                    sourceId=>x"dead", destId=>x"beef",
                                    payload=>RioNwriteR(wrsize=>wrsize,
                                                        tid=>x"aa",
                                                        address=>"00000000000000000000000000000",
                                                        wdptr=>wdptr,
                                                        xamsbs=>"00",
                                                        dataLength=>getReadSize(wrsize, wdptr),
                                                        data=>ioData)));

        OutboundFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                     tt=>"01", ftype=>FTYPE_RESPONSE_CLASS, 
                                     sourceId=>x"beef", destId=>x"dead",
                                     payload=>RioResponse(status=>"0000",
                                                          tid=>x"aa",
                                                          dataLength=>0,
                                                          data=>ioData)));

        SetSlaveAccess(true, "0000000000000000000000000000000",
                       getReadMask(wrsize, wdptr),
                       getReadSize(wrsize, wdptr),
                       ioData);

        TestWait(inboundEmpty, '1', "inbound frame");
        TestWait(outboundEmpty, '1', "outbound frame");
        TestWait(wbMessageEmpty, '1', "wishbone access");
      end loop;
    end loop;

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 2:");
    TestSpec("Action: Send write class NWRITE packets for all sizes.");
    TestSpec("Result: The Wishbone access should match the inbound packet.");
    TestSpec("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_RioWbBridge-TC3-Step2");
    ---------------------------------------------------------------------------
    -- REMARK: Change the address and tid also...
    for i in 0 to 15 loop
      for j in 0 to 1 loop
        wrsize := std_logic_vector(to_unsigned(i, 4));
        if (j = 0) then
          wdptr := '0';
        else
          wdptr:= '1';
        end if;
        
        CreateRandomPayload(ioData, seed1, seed2);

        InboundFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                    tt=>"01", ftype=>FTYPE_WRITE_CLASS, 
                                    sourceId=>x"dead", destId=>x"beef",
                                    payload=>RioNwrite(wrsize=>wrsize,
                                                       address=>"00000000000000000000000000000",
                                                       wdptr=>wdptr,
                                                       xamsbs=>"00",
                                                       dataLength=>getReadSize(wrsize, wdptr),
                                                       data=>ioData)));

        SetSlaveAccess(true, "0000000000000000000000000000000",
                       getReadMask(wrsize, wdptr),
                       getReadSize(wrsize, wdptr),
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
    generic map(READ_CONTENT_END_DATA_VALID=>false)
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
