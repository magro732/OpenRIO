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

      masterCyc_o : out std_logic;
      masterStb_o : out std_logic;
      masterAdr_o : out std_logic_vector(7 downto 0);
      masterDat_o : out std_logic_vector(31 downto 0);
      masterAck_i : in std_logic;
      slaveCyc_i : in std_logic;
      slaveStb_i : in std_logic;
      slaveDat_i : in std_logic_vector(31 downto 0);
      slaveAck_o : out std_logic;

      configStb_o : out std_logic;
      configWe_o : out std_logic;
      configAdr_o : out std_logic_vector(20 downto 0);
      configDat_o : out std_logic_vector(63 downto 0);
      configSel_o : out std_logic_vector(7 downto 0);
      configDat_i : in std_logic_vector(63 downto 0);
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

  signal masterCyc : std_logic;
  signal masterStb : std_logic;
  signal masterAdr : std_logic_vector(7 downto 0);
  signal masterDat : std_logic_vector(31 downto 0);
  signal masterAck : std_logic;
  signal slaveCyc : std_logic;
  signal slaveStb : std_logic;
  signal slaveDat : std_logic_vector(31 downto 0);
  signal slaveAck : std_logic;
  
  signal configStb, configStbExpected : std_logic;
  signal configWe, configWeExpected : std_logic;
  signal configAddr, configAddrExpected : std_logic_vector(20 downto 0);
  signal configSel, configSelExpected : std_logic_vector(7 downto 0);
  signal configDataWrite, configDataWriteExpected : std_logic_vector(63 downto 0);
  signal configDataRead, configDataReadExpected : std_logic_vector(63 downto 0);
  signal configAck, configAckExpected : std_logic;
  
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

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure ReadConfig32(constant destinationId : std_logic_vector(15 downto 0);
                           constant sourceId : std_logic_vector(15 downto 0);
                           constant hop : std_logic_vector(7 downto 0);
                           constant tid : std_logic_vector(7 downto 0);
                           constant address : std_logic_vector(23 downto 0);
                           constant data : std_logic_vector(31 downto 0)) is
      variable maintData : DoubleWordArray(0 to 7);
    begin
      SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
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
      
      ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
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
    procedure WriteConfig32(constant destinationId : std_logic_vector(15 downto 0);
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

      SendFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
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
      
      ReceiveFrame(RioFrameCreate(ackId=>"00000", vc=>'0', crf=>'0', prio=>"00",
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

    variable seed1 : positive := 1;
    variable seed2: positive := 1;

    variable data : DoubleWordArray(0 to 31);
    variable randomPayload : RioPayload;
    variable randomPayload1 : RioPayload;
    variable randomPayload2 : RioPayload;
    variable frame : RioFrame;
    
  begin
    areset_n <= '0';
    enable <= '1';

    frameValid <= '0';
    frameExpected <= '0';

    configAddrExpected(20) <= '0';
    
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
    PrintS("Description: Test switch maintenance accesses on different ports.");
    PrintS("Requirement: XXXXX");
    PrintS("-----------------------------------------------------------------");
    PrintS("Step 1");
    PrintS("Action: Read and write to/from the implementation defined space.");
    PrintS("Result: Check the accesses on the external configuration port.");
    ---------------------------------------------------------------------------
    PrintR("TG_RioLogicalCommon-TC1-Step1");
    ---------------------------------------------------------------------------

    configStbExpected <= '1';
    configWeExpected <= '0';
    configAddrExpected(19 downto 0) <= x"01000";
    configDataReadExpected <= x"00000000deadbeef";
    
    ReadConfig32(destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                 tid=>x"06", address=>x"010000", data=>x"deadbeef");

    configStbExpected <= '1';
    configWeExpected <= '1';
    configAddrExpected(19 downto 0) <= x"01000";
    configDataWriteExpected <= x"c0debabe00000000";
    
    WriteConfig32(destinationId=>x"0000", sourceId=>x"0002", hop=>x"00",
                  tid=>x"06", address=>x"010004", data=>x"c0debabe");

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
      if (configStb = '1') then
        assert configWe = configWeExpected report "Unexpected configWe." severity error;
        assert configAddr = configAddrExpected report "Unexpected configAddr." severity error;
        if (configWe = '1') then
          assert configDataWrite = configDataWriteExpected
            report "Unexpected configDataWrite." severity error;
        else
          configDataRead <= configDataReadExpected;
        end if;
      else

      end if;
    end loop;
  end process;
  
  -----------------------------------------------------------------------------
  -- Instantiate the test port array.
  -----------------------------------------------------------------------------
  
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
      masterCyc_o=>masterCyc, 
      masterStb_o=>masterStb, 
      masterAdr_o=>masterAdr, 
      masterDat_o=>masterDat, 
      masterAck_i=>masterAck, 
      slaveCyc_i=>slaveCyc, 
      slaveStb_i=>slaveStb, 
      slaveDat_i=>slaveDat, 
      slaveAck_o=>slaveAck, 
      configStb_o=>configStb,
      configWe_o=>configWe,
      configAdr_o=>configAddr,
      configSel_o=>configSel,
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
