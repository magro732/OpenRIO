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


-------------------------------------------------------------------------------
-- Entity for TestRioLogicalCommon.
-------------------------------------------------------------------------------
entity TestRioLogicalCommon is
end entity;


-------------------------------------------------------------------------------
-- Architecture for TestRioLogicalCommon.
-------------------------------------------------------------------------------
architecture TestRioLogicalCommonImpl of TestRioLogicalCommon is
  
  component RioLogicalCommon is
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

      configStb_o : out std_logic;
      configWe_o : out std_logic;
      configAdr_o : out std_logic_vector(21 downto 0);
      configDat_o : out std_logic_vector(31 downto 0);
      configDat_i : in std_logic_vector(31 downto 0);
      configAck_i : in std_logic);
  end component;

  component TestPort is
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      frameValid_i : in std_logic;
      frameWrite_i : in RioFrame;
      frameComplete_o : out std_logic;
      
      frameExpected_i : in std_logic;
      frameRead_i : in RioFrame;
      frameReceived_o : out std_logic;

      readFrameEmpty_o : out std_logic;
      readFrame_i : in std_logic;
      readFrameRestart_i : in std_logic;
      readFrameAborted_o : out std_logic;
      readContentEmpty_o : out std_logic;
      readContent_i : in std_logic;
      readContentEnd_o : out std_logic;
      readContentData_o : out std_logic_vector(31 downto 0);
      writeFrameFull_o : out std_logic;
      writeFrame_i : in std_logic;
      writeFrameAbort_i : in std_logic;
      writeContent_i : in std_logic;
      writeContentData_i : in std_logic_vector(31 downto 0));
  end component;

  signal clk : std_logic;
  signal areset_n : std_logic;
  signal enable : std_logic;

  signal frameValid : std_logic;
  signal frameWrite : RioFrame;
  signal frameComplete : std_logic;
      
  signal frameExpected : std_logic;
  signal frameRead : RioFrame;
  signal frameReceived : std_logic;
  
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

  signal configStb, configStbExpected : std_logic;
  signal configWe : std_logic;
  signal configAddr : std_logic_vector(21 downto 0);
  signal configDataWrite : std_logic_vector(31 downto 0);
  signal configDataRead : std_logic_vector(31 downto 0);
  signal configAck : std_logic;
  
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

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure SendFrame(constant frame : RioFrame) is
    begin
      frameValid <= '1';
      frameWrite <= frame;
      wait until frameComplete = '1';
      frameValid <= '0';
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure ReceiveFrame(constant frame : RioFrame) is
    begin
      frameExpected <= '1';
      frameRead <= frame;
      wait until frameReceived = '1';
      frameExpected <= '0';
    end procedure;

    variable seed1 : positive := 1;
    variable seed2: positive := 1;

    variable maintData : DoubleWordArray(0 to 7);
    variable frame : RioFrame;
    
  begin
    areset_n <= '0';
    enable <= '1';

    frameValid <= '0';
    frameExpected <= '0';

    configStbExpected <= '0';
    configAck <= '0';
    
    wait until clk'event and clk = '1';
    wait until clk'event and clk = '1';
    areset_n <= '1';
    wait until clk'event and clk = '1';
    wait until clk'event and clk = '1';

    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("TG_RioLogicalCommon");
    PrintS("-----------------------------------------------------------------");
    PrintS("TG_RioLogicalCommon-TC1");
    PrintS("Description: Test maintenance read requests.");
    PrintS("Requirement: XXXXX");
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 1:");
    PrintS("Action: Send maintenance read request for one word on even offset.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioLogicalCommon-TC1-Step1");
    ---------------------------------------------------------------------------
    
    SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
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

    wait until configStb = '1';
    configStbExpected <= '1';
    wait until clk = '1';
    assert configWe = '0';
    assert configAddr = "0000000000000000000000";
    wait until clk = '1';
    configDataRead <= x"deadbeef";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    configStbExpected <= '0';
    maintData(0) := x"deadbeef00000000";
    
    ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
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


    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 2:");
    PrintS("Action: Send maintenance read request for one word on odd offset.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioLogicalCommon-TC1-Step2");
    ---------------------------------------------------------------------------
    
    SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                             tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                             sourceId=>x"dead", destId=>x"beef",
                             payload=>RioMaintenance(transaction=>"0000",
                                                     size=>"1000",
                                                     tid=>x"aa",
                                                     hopCount=>x"ff",
                                                     configOffset=>"000000000000000000000",
                                                     wdptr=>'1',
                                                     dataLength=>0,
                                                     data=>maintData)));

    wait until configStb = '1';
    configStbExpected <= '1';
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000001" report "Unexpected config address." severity error;
    configDataRead <= x"c0debabe";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    configStbExpected <= '0';
    maintData(0) := x"00000000c0debabe";
    
    ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
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

    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 3:");
    PrintS("Action: Send maintenance read request for two words.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioLogicalCommon-TC1-Step3");
    ---------------------------------------------------------------------------

    SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                             tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                             sourceId=>x"dead", destId=>x"beef",
                             payload=>RioMaintenance(transaction=>"0000",
                                                     size=>"1011",
                                                     tid=>x"cc",
                                                     hopCount=>x"ff",
                                                     configOffset=>"000000000000000000001",
                                                     wdptr=>'0',
                                                     dataLength=>0,
                                                     data=>maintData)));

    wait until configStb = '1';
    configStbExpected <= '1';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000010" report "Unexpected config address." severity error;
    configDataRead <= x"11111111";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000011" report "Unexpected config address." severity error;
    configDataRead <= x"22222222";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    configStbExpected <= '0';
    maintData(0) := x"1111111122222222";
    
    ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                sourceId=>x"beef", destId=>x"dead",
                                payload=>RioMaintenance(transaction=>"0010",
                                                        size=>"0000",
                                                        tid=>x"cc",
                                                        hopCount=>x"ff",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>1,
                                                        data=>maintData)));

    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 4:");
    PrintS("Action: Send maintenance read request for four words.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioLogicalCommon-TC1-Step4");
    ---------------------------------------------------------------------------

    SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                             tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                             sourceId=>x"dead", destId=>x"beef",
                             payload=>RioMaintenance(transaction=>"0000",
                                                     size=>"1011",
                                                     tid=>x"cc",
                                                     hopCount=>x"ff",
                                                     configOffset=>"000000000000000000001",
                                                     wdptr=>'1',
                                                     dataLength=>0,
                                                     data=>maintData)));

    wait until configStb = '1';
    configStbExpected <= '1';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000010" report "Unexpected config address." severity error;
    configDataRead <= x"11111111";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000011" report "Unexpected config address." severity error;
    configDataRead <= x"22222222";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000100" report "Unexpected config address." severity error;
    configDataRead <= x"33333333";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000101" report "Unexpected config address." severity error;
    configDataRead <= x"44444444";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    configStbExpected <= '0';
    maintData(0) := x"1111111122222222";
    maintData(1) := x"3333333344444444";
    
    ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                sourceId=>x"beef", destId=>x"dead",
                                payload=>RioMaintenance(transaction=>"0010",
                                                        size=>"0000",
                                                        tid=>x"cc",
                                                        hopCount=>x"ff",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>2,
                                                        data=>maintData)));

    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 5:");
    PrintS("Action: Send maintenance read request for eight words.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioLogicalCommon-TC1-Step5");
    ---------------------------------------------------------------------------

    SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                             tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                             sourceId=>x"dead", destId=>x"beef",
                             payload=>RioMaintenance(transaction=>"0000",
                                                     size=>"1100",
                                                     tid=>x"cc",
                                                     hopCount=>x"ff",
                                                     configOffset=>"000000000000000000001",
                                                     wdptr=>'0',
                                                     dataLength=>0,
                                                     data=>maintData)));

    wait until configStb = '1';
    configStbExpected <= '1';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000010" report "Unexpected config address." severity error;
    configDataRead <= x"11111111";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000011" report "Unexpected config address." severity error;
    configDataRead <= x"22222222";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000100" report "Unexpected config address." severity error;
    configDataRead <= x"33333333";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000101" report "Unexpected config address." severity error;
    configDataRead <= x"44444444";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000110" report "Unexpected config address." severity error;
    configDataRead <= x"55555555";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000111" report "Unexpected config address." severity error;
    configDataRead <= x"66666666";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001000" report "Unexpected config address." severity error;
    configDataRead <= x"77777777";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001001" report "Unexpected config address." severity error;
    configDataRead <= x"88888888";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    configStbExpected <= '0';
    maintData(0) := x"1111111122222222";
    maintData(1) := x"3333333344444444";
    maintData(2) := x"5555555566666666";
    maintData(3) := x"7777777788888888";
    
    ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                sourceId=>x"beef", destId=>x"dead",
                                payload=>RioMaintenance(transaction=>"0010",
                                                        size=>"0000",
                                                        tid=>x"cc",
                                                        hopCount=>x"ff",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>4,
                                                        data=>maintData)));

    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 6:");
    PrintS("Action: Send maintenance read request for sixteen words.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioLogicalCommon-TC1-Step6");
    ---------------------------------------------------------------------------

    SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                             tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                             sourceId=>x"dead", destId=>x"beef",
                             payload=>RioMaintenance(transaction=>"0000",
                                                     size=>"1100",
                                                     tid=>x"cc",
                                                     hopCount=>x"ff",
                                                     configOffset=>"000000000000000000001",
                                                     wdptr=>'1',
                                                     dataLength=>0,
                                                     data=>maintData)));

    wait until configStb = '1';
    configStbExpected <= '1';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000010" report "Unexpected config address." severity error;
    configDataRead <= x"11111111";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000011" report "Unexpected config address." severity error;
    configDataRead <= x"22222222";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000100" report "Unexpected config address." severity error;
    configDataRead <= x"33333333";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000101" report "Unexpected config address." severity error;
    configDataRead <= x"44444444";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000110" report "Unexpected config address." severity error;
    configDataRead <= x"55555555";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000111" report "Unexpected config address." severity error;
    configDataRead <= x"66666666";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001000" report "Unexpected config address." severity error;
    configDataRead <= x"77777777";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001001" report "Unexpected config address." severity error;
    configDataRead <= x"88888888";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001010" report "Unexpected config address." severity error;
    configDataRead <= x"99999999";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001011" report "Unexpected config address." severity error;
    configDataRead <= x"aaaaaaaa";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001100" report "Unexpected config address." severity error;
    configDataRead <= x"bbbbbbbb";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001101" report "Unexpected config address." severity error;
    configDataRead <= x"cccccccc";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001110" report "Unexpected config address." severity error;
    configDataRead <= x"dddddddd";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001111" report "Unexpected config address." severity error;
    configDataRead <= x"eeeeeeee";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000010000" report "Unexpected config address." severity error;
    configDataRead <= x"ffffffff";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '0' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000010001" report "Unexpected config address." severity error;
    configDataRead <= x"10101010";
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    configStbExpected <= '0';
    maintData(0) := x"1111111122222222";
    maintData(1) := x"3333333344444444";
    maintData(2) := x"5555555566666666";
    maintData(3) := x"7777777788888888";
    maintData(4) := x"99999999aaaaaaaa";
    maintData(5) := x"bbbbbbbbcccccccc";
    maintData(6) := x"ddddddddeeeeeeee";
    maintData(7) := x"ffffffff10101010";
    
    ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                sourceId=>x"beef", destId=>x"dead",
                                payload=>RioMaintenance(transaction=>"0010",
                                                        size=>"0000",
                                                        tid=>x"cc",
                                                        hopCount=>x"ff",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>8,
                                                        data=>maintData)));

    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("TG_RioLogicalCommon-TC2");
    PrintS("Description: Test maintenance write requests.");
    PrintS("Requirement: XXXXX");
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 1:");
    PrintS("Action: Send maintenance write request for one word on even offset.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioLogicalCommon-TC2-Step1");
    ---------------------------------------------------------------------------

    maintData(0) := x"deadbeef00000000";
    SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                             tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                             sourceId=>x"dead", destId=>x"beef",
                             payload=>RioMaintenance(transaction=>"0001",
                                                     size=>"1000",
                                                     tid=>x"aa",
                                                     hopCount=>x"ff",
                                                     configOffset=>"100000000000000000000",
                                                     wdptr=>'0',
                                                     dataLength=>1,
                                                     data=>maintData)));

    wait until configStb = '1';
    configStbExpected <= '1';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected configWe." severity error;
    assert configAddr = "1000000000000000000000" report "Unexpected configAddr." severity error;
    assert configDataWrite = x"deadbeef" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    configStbExpected <= '0';
    
    ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                sourceId=>x"beef", destId=>x"dead",
                                payload=>RioMaintenance(transaction=>"0011",
                                                        size=>"0000",
                                                        tid=>x"aa",
                                                        hopCount=>x"ff",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>0,
                                                        data=>maintData)));

    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 2:");
    PrintS("Action: Send maintenance write request for one word on odd offset.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioLogicalCommon-TC2-Step2");
    ---------------------------------------------------------------------------
    
    maintData(0) := x"00000000c0debabe";
    SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                             tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                             sourceId=>x"dead", destId=>x"beef",
                             payload=>RioMaintenance(transaction=>"0001",
                                                     size=>"1000",
                                                     tid=>x"aa",
                                                     hopCount=>x"ff",
                                                     configOffset=>"100000000000000000000",
                                                     wdptr=>'1',
                                                     dataLength=>1,
                                                     data=>maintData)));

    wait until configStb = '1';
    configStbExpected <= '1';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "1000000000000000000001" report "Unexpected config address." severity error;
    assert configDataWrite = x"c0debabe" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    configStbExpected <= '0';
    
    ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                sourceId=>x"beef", destId=>x"dead",
                                payload=>RioMaintenance(transaction=>"0011",
                                                        size=>"0000",
                                                        tid=>x"aa",
                                                        hopCount=>x"ff",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>0,
                                                        data=>maintData)));

    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 3:");
    PrintS("Action: Send maintenance write request for two words.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioLogicalCommon-TC2-Step3");
    ---------------------------------------------------------------------------

    maintData(0) := x"1111111122222222";
    SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                             tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                             sourceId=>x"dead", destId=>x"beef",
                             payload=>RioMaintenance(transaction=>"0001",
                                                     size=>"1011",
                                                     tid=>x"cc",
                                                     hopCount=>x"ff",
                                                     configOffset=>"100000000000000000001",
                                                     wdptr=>'0',
                                                     dataLength=>1,
                                                     data=>maintData)));

    wait until configStb = '1';
    configStbExpected <= '1';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "1000000000000000000010" report "Unexpected config address." severity error;
    assert configDataWrite = x"11111111" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "1000000000000000000011" report "Unexpected config address." severity error;
    assert configDataWrite = x"22222222" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    configStbExpected <= '0';
    
    ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                sourceId=>x"beef", destId=>x"dead",
                                payload=>RioMaintenance(transaction=>"0011",
                                                        size=>"0000",
                                                        tid=>x"cc",
                                                        hopCount=>x"ff",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>0,
                                                        data=>maintData)));

    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 4:");
    PrintS("Action: Send maintenance write request for four words.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioLogicalCommon-TC2-Step4");
    ---------------------------------------------------------------------------

    maintData(0) := x"1111111122222222";
    maintData(1) := x"3333333344444444";
    SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                             tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                             sourceId=>x"dead", destId=>x"beef",
                             payload=>RioMaintenance(transaction=>"0001",
                                                     size=>"1011",
                                                     tid=>x"cc",
                                                     hopCount=>x"ff",
                                                     configOffset=>"000000000000000000001",
                                                     wdptr=>'1',
                                                     dataLength=>2,
                                                     data=>maintData)));

    wait until configStb = '1';
    configStbExpected <= '1';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000010" report "Unexpected config address." severity error;
    assert configDataWrite = x"11111111" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000011" report "Unexpected config address." severity error;
    assert configDataWrite = x"22222222" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000100" report "Unexpected config address." severity error;
    assert configDataWrite = x"33333333" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000101" report "Unexpected config address." severity error;
    assert configDataWrite = x"44444444" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    configStbExpected <= '0';
    
    ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                sourceId=>x"beef", destId=>x"dead",
                                payload=>RioMaintenance(transaction=>"0011",
                                                        size=>"0000",
                                                        tid=>x"cc",
                                                        hopCount=>x"ff",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>0,
                                                        data=>maintData)));

    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 5:");
    PrintS("Action: Send maintenance write request for eight words.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioLogicalCommon-TC2-Step5");
    ---------------------------------------------------------------------------

    maintData(0) := x"1111111122222222";
    maintData(1) := x"3333333344444444";
    maintData(2) := x"5555555566666666";
    maintData(3) := x"7777777788888888";
    SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                             tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                             sourceId=>x"dead", destId=>x"beef",
                             payload=>RioMaintenance(transaction=>"0001",
                                                     size=>"1100",
                                                     tid=>x"cc",
                                                     hopCount=>x"ff",
                                                     configOffset=>"000000000000000000001",
                                                     wdptr=>'0',
                                                     dataLength=>4,
                                                     data=>maintData)));

    wait until configStb = '1';
    configStbExpected <= '1';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000010" report "Unexpected config address." severity error;
    assert configDataWrite = x"11111111" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000011" report "Unexpected config address." severity error;
    assert configDataWrite = x"22222222" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000100" report "Unexpected config address." severity error;
    assert configDataWrite = x"33333333" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000101" report "Unexpected config address." severity error;
    assert configDataWrite = x"44444444" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000110" report "Unexpected config address." severity error;
    assert configDataWrite = x"55555555" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000111" report "Unexpected config address." severity error;
    assert configDataWrite = x"66666666" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001000" report "Unexpected config address." severity error;
    assert configDataWrite = x"77777777" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001001" report "Unexpected config address." severity error;
    assert configDataWrite = x"88888888" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    configStbExpected <= '0';
    
    ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                sourceId=>x"beef", destId=>x"dead",
                                payload=>RioMaintenance(transaction=>"0011",
                                                        size=>"0000",
                                                        tid=>x"cc",
                                                        hopCount=>x"ff",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>0,
                                                        data=>maintData)));

    ---------------------------------------------------------------------------
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 6:");
    PrintS("Action: Send maintenance write request for sixteen words.");
    PrintS("Result: Check the accesses on the external configuration port.");
    PrintS("-----------------------------------------------------------------");
    ---------------------------------------------------------------------------
    PrintR("TG_RioLogicalCommon-TC2-Step6");
    ---------------------------------------------------------------------------

    maintData(0) := x"1111111122222222";
    maintData(1) := x"3333333344444444";
    maintData(2) := x"5555555566666666";
    maintData(3) := x"7777777788888888";
    maintData(4) := x"99999999aaaaaaaa";
    maintData(5) := x"bbbbbbbbcccccccc";
    maintData(6) := x"ddddddddeeeeeeee";
    maintData(7) := x"ffffffff10101010";
    SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                             tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                             sourceId=>x"dead", destId=>x"beef",
                             payload=>RioMaintenance(transaction=>"0001",
                                                     size=>"1100",
                                                     tid=>x"cc",
                                                     hopCount=>x"ff",
                                                     configOffset=>"000000000000000000001",
                                                     wdptr=>'1',
                                                     dataLength=>8,
                                                     data=>maintData)));

    wait until configStb = '1';
    configStbExpected <= '1';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000010" report "Unexpected config address." severity error;
    assert configDataWrite = x"11111111" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000011" report "Unexpected config address." severity error;
    assert configDataWrite = x"22222222" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000100" report "Unexpected config address." severity error;
    assert configDataWrite = x"33333333" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000101" report "Unexpected config address." severity error;
    assert configDataWrite = x"44444444" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000110" report "Unexpected config address." severity error;
    assert configDataWrite = x"55555555" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000000111" report "Unexpected config address." severity error;
    assert configDataWrite = x"66666666" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001000" report "Unexpected config address." severity error;
    assert configDataWrite = x"77777777" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001001" report "Unexpected config address." severity error;
    assert configDataWrite = x"88888888" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001010" report "Unexpected config address." severity error;
    assert configDataWrite = x"99999999" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001011" report "Unexpected config address." severity error;
    assert configDataWrite = x"aaaaaaaa" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001100" report "Unexpected config address." severity error;
    assert configDataWrite = x"bbbbbbbb" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001101" report "Unexpected config address." severity error;
    assert configDataWrite = x"cccccccc" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001110" report "Unexpected config address." severity error;
    assert configDataWrite = x"dddddddd" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000001111" report "Unexpected config address." severity error;
    assert configDataWrite = x"eeeeeeee" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';
    
    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000010000" report "Unexpected config address." severity error;
    assert configDataWrite = x"ffffffff" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    wait until clk = '1';
    assert configWe = '1' report "Unexpected config write." severity error;
    assert configAddr = "0000000000000000010001" report "Unexpected config address." severity error;
    assert configDataWrite = x"10101010" report "Unexpected configDataWrite." severity error;
    configAck <= '1';
    wait until clk = '1';
    configAck <= '0';

    configStbExpected <= '0';
    
    ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
                                tt=>"01", ftype=>FTYPE_MAINTENANCE_CLASS, 
                                sourceId=>x"beef", destId=>x"dead",
                                payload=>RioMaintenance(transaction=>"0011",
                                                        size=>"0000",
                                                        tid=>x"cc",
                                                        hopCount=>x"ff",
                                                        configOffset=>"000000000000000000000",
                                                        wdptr=>'0',
                                                        dataLength=>0,
                                                        data=>maintData)));


    ---------------------------------------------------------------------------
    -- Test completed.
    ---------------------------------------------------------------------------
    
    TestEnd;
  end process;

  -----------------------------------------------------------------------------
  -- Instantiate a process receiving the configuration accesses to the
  -- implementation defined space.
  -----------------------------------------------------------------------------
  process
  begin
    loop
      wait until clk'event and clk = '1';
      assert configStbExpected = configStb report "Unexpected config-space access." severity error;
    end loop;
  end process;
  
  -----------------------------------------------------------------------------
  -- Instantiate the test port array.
  -----------------------------------------------------------------------------

  readFrameRestart <= '0';
  TestPortInst: TestPort
    port map(
      clk=>clk, areset_n=>areset_n, 
      frameValid_i=>frameValid,
      frameWrite_i=>frameWrite, 
      frameComplete_o=>frameComplete, 
      frameExpected_i=>frameExpected, 
      frameRead_i=>frameRead, 
      frameReceived_o=>frameReceived, 
      readFrameEmpty_o=>readFrameEmpty, 
      readFrame_i=>readFrame, 
      readFrameRestart_i=>readFrameRestart, 
      readFrameAborted_o=>readFrameAborted, 
      readContentEmpty_o=>readContentEmpty, 
      readContent_i=>readContent, 
      readContentEnd_o=>readContentEnd, 
      readContentData_o=>readContentData, 
      writeFrameFull_o=>writeFrameFull, 
      writeFrame_i=>writeFrame, 
      writeFrameAbort_i=>writeFrameAbort, 
      writeContent_i=>writeContent, 
      writeContentData_i=>writeContentData);

  -----------------------------------------------------------------------------
  -- Instantiate the switch.
  -----------------------------------------------------------------------------

  TestObject: RioLogicalCommon
    port map(
      clk=>clk, areset_n=>areset_n, enable=>enable,
      writeFrameFull_i=>writeFrameFull,
      writeFrame_o=>writeFrame,
      writeFrameAbort_o=>writeFrameAbort, 
      writeContent_o=>writeContent,
      writeContentData_o=>writeContentData, 
      readFrameEmpty_i=>readFrameEmpty, 
      readFrame_o=>readFrame,
      readContent_o=>readContent,
      readContentEnd_i=>readContentEnd, 
      readContentData_i=>readContentData,
      configStb_o=>configStb,
      configWe_o=>configWe,
      configAdr_o=>configAddr,
      configDat_o=>configDataWrite,
      configDat_i=>configDataRead,
      configAck_i=>configAck);
  
  
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
 

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity TestPort is
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    frameValid_i : in std_logic;
    frameWrite_i : in RioFrame;
    frameComplete_o : out std_logic;
    
    frameExpected_i : in std_logic;
    frameRead_i : in RioFrame;
    frameReceived_o : out std_logic;

    readFrameEmpty_o : out std_logic;
    readFrame_i : in std_logic;
    readFrameRestart_i : in std_logic;
    readFrameAborted_o : out std_logic;
    readContentEmpty_o : out std_logic;
    readContent_i : in std_logic;
    readContentEnd_o : out std_logic;
    readContentData_o : out std_logic_vector(31 downto 0);
    
    writeFrameFull_o : out std_logic;
    writeFrame_i : in std_logic;
    writeFrameAbort_i : in std_logic;
    writeContent_i : in std_logic;
    writeContentData_i : in std_logic_vector(31 downto 0));
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture TestPortImpl of TestPort is
begin
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  FrameReader: process
    type StateType is (STATE_IDLE, STATE_WRITE);
    variable state : StateType;
    variable frameIndex : natural range 0 to 69;
  begin
    writeFrameFull_o <= '1';
    frameReceived_o <= '0';
    wait until areset_n = '1';

    state := STATE_IDLE;

    loop
      wait until clk'event and clk = '1';
    
      case state is
        
        when STATE_IDLE =>
          frameReceived_o <= '0';
          if (frameExpected_i = '1') then
            writeFrameFull_o <= '0';
            state := STATE_WRITE;
            frameIndex := 0;
          else
            writeFrameFull_o <= '1';
          end if;
          assert writeFrame_i = '0' report "Unexpected frame." severity error;
          assert writeFrameAbort_i = '0' report "Unexpected frame abort." severity error;
          assert writeContent_i = '0' report "Unexpected data." severity error;
          
        when STATE_WRITE =>
          if (writeContent_i = '1') then
            -- Writing content.
            if (frameIndex < frameRead_i.length) then
              assert writeContentData_i = frameRead_i.payload(frameIndex)
                report "Unexpected frame content received:" &
                " index=" & integer'image(frameIndex) &
                " expected=" & integer'image(to_integer(unsigned(frameRead_i.payload(frameIndex)))) &
                " got=" & integer'image(to_integer(unsigned(writeContentData_i)))
                severity error;
              
              frameIndex := frameIndex + 1;
            else
              report "Unexpected frame content received:" &
                " index=" & integer'image(frameIndex) &
                " expected=" & integer'image(to_integer(unsigned(frameRead_i.payload(frameIndex)))) &
                " got=" & integer'image(to_integer(unsigned(writeContentData_i)))
                severity error;
              
              frameIndex := frameIndex + 1;
            end if;
          else
            -- Not writing any content.
          end if;
          
          if (writeFrame_i = '1') then
            -- Writing a complete frame.
            assert frameIndex = frameRead_i.length report "Unexpected frame length received." severity error;
            state := STATE_IDLE;
            frameReceived_o <= '1';
            writeFrameFull_o <= '1';
          else
            -- Not writing any frame.
          end if;

          if (writeFrameAbort_i = '1') then
            -- The frame should be aborted.
            frameIndex := 0;
          else
            -- Not aborting any frame.
          end if;
      end case;
    end loop;    
  end process;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  -- REMARK: add support for these signals...
  -- readFrameEmpty_i : in std_logic;
  -- readFrameAborted_i : in std_logic;
  FrameSender: process
    type StateType is (STATE_IDLE, STATE_READ);
    variable state : StateType;
    variable frameIndex : natural range 0 to 69;
  begin
    readFrameEmpty_o <= '1';
    readFrameAborted_o <= '0';
    readContentEmpty_o <= '1';
    readContentEnd_o <= '1';
    readContentData_o <= (others => 'U');
    frameComplete_o <= '0';
    wait until areset_n = '1';

    state := STATE_IDLE;

    loop
      wait until clk'event and clk = '1';
    
      case state is

        when STATE_IDLE =>
          frameComplete_o <= '0';
          if (frameValid_i = '1') then
            state := STATE_READ;
            frameIndex := 0;
            readContentEmpty_o <= '0';
            readFrameEmpty_o <= '0';
          else
            readContentEmpty_o <= '1';
          end if;
          
        when STATE_READ =>
          if (readFrameRestart_i = '1') then
            readContentEnd_o <= '0';
            frameIndex := 0;
          else
            -- Not restarting a frame.
          end if;
          
          if (readContent_i = '1') then
            if (frameIndex < frameWrite_i.length) then
              readContentData_o <= frameWrite_i.payload(frameIndex);
              readContentEnd_o <= '0';
              frameIndex := frameIndex + 1;
            elsif (frameIndex = frameWrite_i.length) then
              readContentEnd_o <= '1';
            else
              report "Reading empty frame." severity error;
            end if;
          else
            -- Not reading data.
          end if;

          if (readFrame_i = '1') then
            state := STATE_IDLE;
            assert frameIndex = frameWrite_i.length report "Unread frame data discarded." severity error;
            frameComplete_o <= '1';
            readFrameEmpty_o <= '1';
            readContentEmpty_o <= '1';
            readContentData_o <= (others => 'U');
          else
            -- Not reading a frame.
          end if;

      end case;
    end loop;
  end process;

end architecture;
