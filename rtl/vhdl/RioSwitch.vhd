-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Containing RapidIO packet switching functionality contained in the top
-- entity RioSwitch.
-- 
-- To Do:
-- - Add support for portWrite maintenance packets.
-- - Add a real crossbar as interconnect.
-- - Change the internal addressing to one-hot.
-- - Remove acknowledge cycle when transfering packets between ports to double
--   the bandwidth.
-- - Add hot-swap.
-- - Connect linkInitialized to all ports and read it from the source port
--   using the interconnect. This will allow alternative routes since the
--   sending port can see if a receiving port is up or not.
-- - Add support for extended route.
-- - Add validity-bit to know if a route has been activly set for a particular
--   deviceId. Currently, we rely on that the routing table memory is
--   initialized in the enumeration or at device startup.
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
-- RioSwitch
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 
use work.rio_common.all;

-------------------------------------------------------------------------------
-- Entity for RioSwitch.
-------------------------------------------------------------------------------
entity RioSwitch is
  generic(
    SWITCH_PORTS               : natural  range 3 to 255 := 4;
    DEVICE_IDENTITY            : std_logic_vector(15 downto 0) := x"ABCD";
    DEVICE_VENDOR_IDENTITY     : std_logic_vector(15 downto 0) := x"ACDC";
    DEVICE_REV                 : std_logic_vector(31 downto 0) := x"ABCDEFDD";
    ASSY_IDENTITY              : std_logic_vector(15 downto 0) := x"ADCA";
    ASSY_VENDOR_IDENTITY       : std_logic_vector(15 downto 0) := x"AEFB";
    ASSY_REV                   : std_logic_vector(15 downto 0) := x"AFFE";
    portWriteTimeoutResetValue : std_logic_vector(15 downto 0) := x"FFFF");  -- Set it to x"0001" to make testbench run faster: 1.5 ms timeout instead of 100 ms..
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
    readFrameRestart_o : out Array1(SWITCH_PORTS-1 downto 0);
    readFrameAborted_i : in Array1(SWITCH_PORTS-1 downto 0);
    readContentEmpty_i : in Array1(SWITCH_PORTS-1 downto 0);
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
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioSwitch.
-------------------------------------------------------------------------------
architecture RioSwitchImpl of RioSwitch is

  component RouteTableInterconnect is
    generic(
      WIDTH : natural range 1 to 256 := 8);
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      stb_i : in Array1(WIDTH-1 downto 0);
      addr_i : in Array16(WIDTH-1 downto 0);
      dataM_o : out Array8(WIDTH-1 downto 0);
      ack_o : out Array1(WIDTH-1 downto 0);

      stb_o : out std_logic;
      addr_o : out std_logic_vector(15 downto 0);
      dataS_i : in std_logic_vector(7 downto 0);
      ack_i : in std_logic);
  end component;
  
  component SwitchPortInterconnect is
    generic(
      WIDTH : natural range 1 to 256 := 8);
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      masterCyc_i : in Array1(WIDTH-1 downto 0);
      masterStb_i : in Array1(WIDTH-1 downto 0);
      masterWe_i : in Array1(WIDTH-1 downto 0);
      masterAddr_i : in Array10(WIDTH-1 downto 0);
      masterData_i : in Array32(WIDTH-1 downto 0);
      masterData_o : out Array1(WIDTH-1 downto 0);
      masterAck_o : out Array1(WIDTH-1 downto 0);

      slaveCyc_o : out Array1(WIDTH-1 downto 0);
      slaveStb_o : out Array1(WIDTH-1 downto 0);
      slaveWe_o : out Array1(WIDTH-1 downto 0);
      slaveAddr_o : out Array10(WIDTH-1 downto 0);
      slaveData_o : out Array32(WIDTH-1 downto 0);
      slaveData_i : in Array1(WIDTH-1 downto 0);
      slaveAck_i : in Array1(WIDTH-1 downto 0));
  end component;
  
  component SwitchPortMaintenance is
    generic(
      SWITCH_PORTS : natural range 0 to 255;
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

      lookupStb_i : in std_logic;
      lookupAddr_i : in std_logic_vector(15 downto 0);
      lookupData_o : out std_logic_vector(7 downto 0);
      lookupAck_o : out std_logic;
      
      masterCyc_o : out std_logic;
      masterStb_o : out std_logic;
      masterWe_o : out std_logic;
      masterAddr_o : out std_logic_vector(9 downto 0);
      masterData_o : out std_logic_vector(31 downto 0);
      masterData_i : in std_logic;
      masterAck_i : in std_logic;

      slaveCyc_i : in std_logic;
      slaveStb_i : in std_logic;
      slaveWe_i : in std_logic;
      slaveAddr_i : in std_logic_vector(9 downto 0);
      slaveData_i : in std_logic_vector(31 downto 0);
      slaveData_o : out std_logic;
      slaveAck_o : out std_logic;

      lookupStb_o : out std_logic;
      lookupAddr_o : out std_logic_vector(15 downto 0);
      lookupData_i : in std_logic_vector(7 downto 0);
      lookupAck_i : in std_logic;

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
  
  component SwitchPort is
    generic(
      MAINTENANCE_LOOKUP : boolean;
      PORT_INDEX : natural);
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      masterCyc_o : out std_logic;
      masterStb_o : out std_logic;
      masterWe_o : out std_logic;
      masterAddr_o : out std_logic_vector(9 downto 0);
      masterData_o : out std_logic_vector(31 downto 0);
      masterData_i : in std_logic;
      masterAck_i : in std_logic;

      slaveCyc_i : in std_logic;
      slaveStb_i : in std_logic;
      slaveWe_i : in std_logic;
      slaveAddr_i : in std_logic_vector(9 downto 0);
      slaveData_i : in std_logic_vector(31 downto 0);
      slaveData_o : out std_logic;
      slaveAck_o : out std_logic;

      lookupStb_o : out std_logic;
      lookupAddr_o : out std_logic_vector(15 downto 0);
      lookupData_i : in std_logic_vector(7 downto 0);
      lookupAck_i : in std_logic;

      readFrameEmpty_i : in std_logic;
      readFrame_o : out std_logic;
      readFrameRestart_o : out std_logic;
      readFrameAborted_i : in std_logic;
      readContentEmpty_i : in std_logic;
      readContent_o : out std_logic;
      readContentEnd_i : in std_logic;
      readContentData_i : in std_logic_vector(31 downto 0);
      writeFramePort_o : out std_logic_vector(7 downto 0);
      writeFrameFull_i : in std_logic;
      writeFrame_o : out std_logic;
      writeFrameAbort_o : out std_logic;
      writeContent_o : out std_logic;
      writeContentData_o : out std_logic_vector(31 downto 0));
  end component;

  signal masterLookupStb : Array1(SWITCH_PORTS downto 0);
  signal masterLookupAddr : Array16(SWITCH_PORTS downto 0);
  signal masterLookupData : Array8(SWITCH_PORTS downto 0);
  signal masterLookupAck : Array1(SWITCH_PORTS downto 0);

  signal slaveLookupStb : std_logic;
  signal slaveLookupAddr : std_logic_vector(15 downto 0);
  signal slaveLookupData : std_logic_vector(7 downto 0);
  signal slaveLookupAck : std_logic;
  
  signal masterCyc : Array1(SWITCH_PORTS downto 0);
  signal masterStb : Array1(SWITCH_PORTS downto 0);
  signal masterWe : Array1(SWITCH_PORTS downto 0);
  signal masterAddr : Array10(SWITCH_PORTS downto 0);
  signal masterDataWrite : Array32(SWITCH_PORTS downto 0);
  signal masterDataRead : Array1(SWITCH_PORTS downto 0);
  signal masterAck : Array1(SWITCH_PORTS downto 0);
  
  signal slaveCyc : Array1(SWITCH_PORTS downto 0);
  signal slaveStb : Array1(SWITCH_PORTS downto 0);
  signal slaveWe : Array1(SWITCH_PORTS downto 0);
  signal slaveAddr : Array10(SWITCH_PORTS downto 0);
  signal slaveDataWrite : Array32(SWITCH_PORTS downto 0);
  signal slaveDataRead : Array1(SWITCH_PORTS downto 0);
  signal slaveAck : Array1(SWITCH_PORTS downto 0);

begin

  -----------------------------------------------------------------------------
  -- The routing table interconnect.
  -----------------------------------------------------------------------------
  RouteInterconnect: RouteTableInterconnect
    generic map(
      WIDTH=>SWITCH_PORTS+1)
    port map(
      clk=>clk, areset_n=>areset_n, 
      stb_i=>masterLookupStb, addr_i=>masterLookupAddr, 
      dataM_o=>masterLookupData, ack_o=>masterLookupAck, 
      stb_o=>slaveLookupStb, addr_o=>slaveLookupAddr,
      dataS_i=>slaveLookupData, ack_i=>slaveLookupAck);
  
  -----------------------------------------------------------------------------
  -- The port interconnect.
  -----------------------------------------------------------------------------
  PortInterconnect: SwitchPortInterconnect
    generic map(
      WIDTH=>SWITCH_PORTS+1)
    port map(
      clk=>clk, areset_n=>areset_n, 
      masterCyc_i=>masterCyc, masterStb_i=>masterStb, masterWe_i=>masterWe, masterAddr_i=>masterAddr, 
      masterData_i=>masterDataWrite, masterData_o=>masterDataRead, masterAck_o=>masterAck, 
      slaveCyc_o=>slaveCyc, slaveStb_o=>slaveStb, slaveWe_o=>slaveWe, slaveAddr_o=>slaveAddr, 
      slaveData_o=>slaveDataWrite, slaveData_i=>slaveDataRead, slaveAck_i=>slaveAck);

  -----------------------------------------------------------------------------
  -- Data relaying port instantiation.
  -----------------------------------------------------------------------------
  PortGeneration: for portIndex in 0 to SWITCH_PORTS-1 generate
    PortInst: SwitchPort
      generic map(
        MAINTENANCE_LOOKUP=>false,
        PORT_INDEX=>portIndex)
      port map(
        clk=>clk, areset_n=>areset_n,
        masterCyc_o=>masterCyc(portIndex), masterStb_o=>masterStb(portIndex),
        masterWe_o=>masterWe(portIndex), masterAddr_o=>masterAddr(portIndex),
        masterData_o=>masterDataWrite(portIndex),
        masterData_i=>masterDataRead(portIndex), masterAck_i=>masterAck(portIndex),
        slaveCyc_i=>slaveCyc(portIndex), slaveStb_i=>slaveStb(portIndex),
        slaveWe_i=>slaveWe(portIndex), slaveAddr_i=>slaveAddr(portIndex),
        slaveData_i=>slaveDataWrite(portIndex),
        slaveData_o=>slaveDataRead(portIndex), slaveAck_o=>slaveAck(portIndex),
        lookupStb_o=>masterLookupStb(portIndex),
        lookupAddr_o=>masterLookupAddr(portIndex), 
        lookupData_i=>masterLookupData(portIndex), lookupAck_i=>masterLookupAck(portIndex),
        readFrameEmpty_i=>readFrameEmpty_i(portIndex), readFrame_o=>readFrame_o(portIndex), 
        readFrameRestart_o=>readFrameRestart_o(portIndex),
        readFrameAborted_i=>readFrameAborted_i(portIndex), 
        readContentEmpty_i=>readContentEmpty_i(portIndex), readContent_o=>readContent_o(portIndex), 
        readContentEnd_i=>readContentEnd_i(portIndex), readContentData_i=>readContentData_i(portIndex), 
        writeFramePort_o=>open,
        writeFrameFull_i=>writeFrameFull_i(portIndex), writeFrame_o=>writeFrame_o(portIndex), 
        writeFrameAbort_o=>writeFrameAbort_o(portIndex), writeContent_o=>writeContent_o(portIndex), 
        writeContentData_o=>writeContentData_o(portIndex));
  end generate;
  
  -----------------------------------------------------------------------------
  -- Maintenance port instantiation.
  -----------------------------------------------------------------------------
  MaintenancePort: SwitchPortMaintenance
    generic map(
      SWITCH_PORTS=>SWITCH_PORTS,
      DEVICE_IDENTITY=>DEVICE_IDENTITY,
      DEVICE_VENDOR_IDENTITY=>DEVICE_VENDOR_IDENTITY,
      DEVICE_REV=>DEVICE_REV,
      ASSY_IDENTITY=>ASSY_IDENTITY,
      ASSY_VENDOR_IDENTITY=>ASSY_VENDOR_IDENTITY,
      ASSY_REV=>ASSY_REV,
      portWriteTimeoutResetValue => portWriteTimeoutResetValue)
    port map(
      clk=>clk, areset_n=>areset_n, 
      lookupStb_i=>slaveLookupStb, lookupAddr_i=>slaveLookupAddr,
      lookupData_o=>slaveLookupData, lookupAck_o=>slaveLookupAck,
      masterCyc_o=>masterCyc(SWITCH_PORTS), masterStb_o=>masterStb(SWITCH_PORTS),
      masterWe_o=>masterWe(SWITCH_PORTS), masterAddr_o=>masterAddr(SWITCH_PORTS),
      masterData_o=>masterDataWrite(SWITCH_PORTS),
      masterData_i=>masterDataRead(SWITCH_PORTS), masterAck_i=>masterAck(SWITCH_PORTS),
      slaveCyc_i=>slaveCyc(SWITCH_PORTS), slaveStb_i=>slaveStb(SWITCH_PORTS),
      slaveWe_i=>slaveWe(SWITCH_PORTS), slaveAddr_i=>slaveAddr(SWITCH_PORTS),
      slaveData_i=>slaveDataWrite(SWITCH_PORTS),
      slaveData_o=>slaveDataRead(SWITCH_PORTS), slaveAck_o=>slaveAck(SWITCH_PORTS),
      lookupStb_o=>masterLookupStb(SWITCH_PORTS),
      lookupAddr_o=>masterLookupAddr(SWITCH_PORTS),
      lookupData_i=>masterLookupData(SWITCH_PORTS), lookupAck_i=>masterLookupAck(SWITCH_PORTS),
      portLinkTimeout_o=>portLinkTimeout_o,
      linkInitialized_i=>linkInitialized_i,
      outputPortEnable_o=>outputPortEnable_o, inputPortEnable_o=>inputPortEnable_o,
      localAckIdWrite_o=>localAckIdWrite_o, clrOutstandingAckId_o=>clrOutstandingAckId_o, 
      inboundAckId_o=>inboundAckId_o, outstandingAckId_o=>outstandingAckId_o, 
      outboundAckId_o=>outboundAckId_o, inboundAckId_i=>inboundAckId_i, 
      outstandingAckId_i=>outstandingAckId_i, outboundAckId_i=>outboundAckId_i, 
      configStb_o=>configStb_o, configWe_o=>configWe_o, configAddr_o=>configAddr_o,
      configData_o=>configData_o, configData_i=>configData_i, configAck_i=>configAck_i);

end architecture;



-------------------------------------------------------------------------------
-- SwitchPort.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 
use work.rio_common.all;


-------------------------------------------------------------------------------
-- Entity for SwitchPort.
-------------------------------------------------------------------------------
entity SwitchPort is
  generic(
    MAINTENANCE_LOOKUP : boolean;
    PORT_INDEX : natural);
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    -- Master port signals.
    -- Write frames to other ports.
    masterCyc_o : out std_logic;
    masterStb_o : out std_logic;
    masterWe_o : out std_logic;
    masterAddr_o : out std_logic_vector(9 downto 0);
    masterData_o : out std_logic_vector(31 downto 0);
    masterData_i : in std_logic;
    masterAck_i : in std_logic;

    -- Slave port signals.
    -- Receives frames from other ports.
    slaveCyc_i : in std_logic;
    slaveStb_i : in std_logic;
    slaveWe_i : in std_logic;
    slaveAddr_i : in std_logic_vector(9 downto 0);
    slaveData_i : in std_logic_vector(31 downto 0);
    slaveData_o : out std_logic;
    slaveAck_o : out std_logic;

    -- Address-lookup interface.
    lookupStb_o : out std_logic;
    lookupAddr_o : out std_logic_vector(15 downto 0);
    lookupData_i : in std_logic_vector(7 downto 0);
    lookupAck_i : in std_logic;

    -- Physical port frame buffer interface.
    readFrameEmpty_i : in std_logic;
    readFrame_o : out std_logic;
    readFrameRestart_o : out std_logic;
    readFrameAborted_i : in std_logic;
    readContentEmpty_i : in std_logic;
    readContent_o : out std_logic;
    readContentEnd_i : in std_logic;
    readContentData_i : in std_logic_vector(31 downto 0);
    writeFramePort_o : out std_logic_vector(7 downto 0);
    writeFrameFull_i : in std_logic;
    writeFrame_o : out std_logic;
    writeFrameAbort_o : out std_logic;
    writeContent_o : out std_logic;
    writeContentData_o : out std_logic_vector(31 downto 0));
end entity;


-------------------------------------------------------------------------------
-- Architecture for SwitchPort.
-------------------------------------------------------------------------------
architecture SwitchPortImpl of SwitchPort is

  type MasterStateType is (STATE_IDLE,
                           STATE_WAIT_HEADER_0, STATE_READ_HEADER_0,
                           STATE_READ_PORT_LOOKUP,
                           STATE_READ_TARGET_PORT,
                           STATE_WAIT_TARGET_PORT,
                           STATE_WAIT_TARGET_WRITE,
                           STATE_WAIT_COMPLETE);
  signal masterState : MasterStateType;
  alias ftype : std_logic_vector(3 downto 0) is readContentData_i(19 downto 16);
  alias tt : std_logic_vector(1 downto 0) is readContentData_i(21 downto 20);
  
  type SlaveStateType is (STATE_IDLE, STATE_SEND_ACK);
  signal slaveState : SlaveStateType;
  
begin

  -----------------------------------------------------------------------------
  -- Master interface process.
  -----------------------------------------------------------------------------
  Master: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      masterState <= STATE_IDLE;

      lookupStb_o <= '0';
      lookupAddr_o <= (others => '0');
      
      masterCyc_o <= '0';
      masterStb_o <= '0';
      masterWe_o <= '0';
      masterAddr_o <= (others => '0');
      masterData_o <= (others => '0');
      
      readContent_o <= '0';
      readFrame_o <= '0';
      readFrameRestart_o <= '0';
    elsif rising_edge(clk) then
      readContent_o <= '0';
      readFrame_o <= '0';
      readFrameRestart_o <= '0';

      case masterState is

        when STATE_IDLE =>
          ---------------------------------------------------------------------
          -- Wait for a new packet or content of a new packet.
          ---------------------------------------------------------------------

          -- Reset bus signals.
          masterCyc_o <= '0';
          masterStb_o <= '0';
          
          -- Wait for frame content to be available.
          -- Use different signals to trigger the forwarding of packets depending
          -- on the switch philosofy.
          if (readFrameEmpty_i = '0') then
            readContent_o <= '1';
            masterState <= STATE_WAIT_HEADER_0;
          end if;

        when STATE_WAIT_HEADER_0 =>
          ---------------------------------------------------------------------
          -- Wait for the frame buffer output to be updated.
          ---------------------------------------------------------------------

          -- Wait for frame buffer output to be updated.
          masterState <= STATE_READ_HEADER_0;
          
        when STATE_READ_HEADER_0 =>
          ---------------------------------------------------------------------
          -- Check the FTYPE and forward it to the maintenance port if it is a
          -- maintenance packet. Otherwise, initiate an address lookup and wait
          -- for the result.
          ---------------------------------------------------------------------

          -- Check if the frame has ended.
          if (readContentEnd_i = '0') then
            -- The frame has not ended.
            -- This word contains the header and the source id.

            -- Read the tt-field to check the source and destination id size.
            if (tt = "01") then
              -- This frame contains 16-bit addresses.
              
              -- Read the new content.
              readContent_o <= '1';

              -- Save the content of the header and destination.
              masterData_o <= readContentData_i;

              -- Check if this is a maintenance frame.
              if ((not MAINTENANCE_LOOKUP) and (ftype = FTYPE_MAINTENANCE_CLASS)) then
                -- This is a maintenance frame.

                -- Always route these frames to the maintenance module in the
                -- switch by setting the MSB bit of the port address.
                masterAddr_o <= '1' & std_logic_vector(to_unsigned(PORT_INDEX, 8)) & '0';

                -- Start an access to the maintenance port.
                masterState <= STATE_READ_TARGET_PORT;
              else
                -- This is not a maintenance frame.
                
                -- Lookup the destination address and proceed to wait for the
                -- result.
                lookupStb_o <= '1';
                lookupAddr_o <= readContentData_i(15 downto 0);

                -- Wait for the port lookup to return a result.
                masterState <= STATE_READ_PORT_LOOKUP;
              end if;
            else
              -- Unsupported tt-value, discard the frame.
              readFrame_o <= '1';
              masterState <= STATE_IDLE;
            end if;
          else
            -- End of frame.
            -- The frame is too short to contain a valid frame. Discard it.
            readFrame_o <= '1';
            masterState <= STATE_IDLE;
          end if;

        when STATE_READ_PORT_LOOKUP =>
          ---------------------------------------------------------------------
          -- Wait for the address lookup to be complete.
          ---------------------------------------------------------------------

          -- Wait for the routing table to complete the request.
          if (lookupAck_i = '1') then
            -- The address lookup is complete.
            
            -- Terminate the lookup cycle.
            lookupStb_o <= '0';

            -- Proceed to read the target port.
            masterAddr_o <= '0' & lookupData_i & '0';
            masterState <= STATE_READ_TARGET_PORT;
          else
            -- Wait until the address lookup is complete.
            -- REMARK: Timeout here???
          end if;

        when STATE_READ_TARGET_PORT =>
          ---------------------------------------------------------------------
          -- Initiate an access to the target port.
          ---------------------------------------------------------------------

          -- Read the status of the target port using the result from the
          -- lookup in the routing table.
          masterCyc_o <= '1';
          masterStb_o <= '1';
          masterWe_o <= '0';
          masterState <= STATE_WAIT_TARGET_PORT;

        when STATE_WAIT_TARGET_PORT =>
          ---------------------------------------------------------------------
          -- Wait to get access to the target port. When the port is ready
          -- check if it is ready to accept a new frame. If it cannot accept a
          -- new frame, terminate the access and go back and start a new one.
          -- This is to free the interconnect to let other ports access it if
          -- it is a shared bus. If the port is ready, initiate a write access
          -- to the selected port.
          ---------------------------------------------------------------------

          -- Wait for the target port to complete the request.
          if (masterAck_i = '1') then
            -- Target port has completed the request.

            -- Check the status of the target port.
            if (masterData_i = '0') then
              -- The target port has empty buffers to receive the frame.

              -- Hold the bus with cyc until the cycle is complete.
              -- Write the first word of the frame to the target port.
              -- The masterData_o has already been assigned.
              masterCyc_o <= '1';
              masterStb_o <= '1';
              masterWe_o <= '1';
              masterAddr_o(0) <= '1';

              -- Change state to transfer the frame.
              masterState <= STATE_WAIT_TARGET_WRITE;
            else
              -- The target port has no empty buffer to receive the frame.
              -- Terminate the cycle and retry later.
              masterCyc_o <= '0';
              masterStb_o <= '0';
              masterState <= STATE_READ_TARGET_PORT;
            end if;
          else
            -- Target port has not completed the request.
            -- Dont to anything.
          end if;

        when STATE_WAIT_TARGET_WRITE =>
          ---------------------------------------------------------------------
          -- Wait for the write access to complete. When complete, write the
          -- next content and update the content to the next. If the frame does
          -- not have any more data ready, terminate the access but keep the
          -- cycle active and proceed to wait for new data.
          ---------------------------------------------------------------------

          -- Wait for the target port to complete the request.
          -- REMARK: Remove the ack-condition to increase performance, we know
          -- that the write takes one cycle.
          if (masterAck_i = '1') then
            -- The target port is ready.

            -- Check if the frame has ended.
            if (readContentEnd_i = '0') then
              -- The frame has not ended.
              
              -- There are more data to transfer.
              masterData_o <= readContentData_i;
              readContent_o <= '1';
            else
              -- There are no more data to transfer.
              
              -- Update to the next frame.
              readFrame_o <= '1';
              
              -- Tell the target port that the frame is complete.
              masterWe_o <= '1';
              masterAddr_o(0) <= '0';
              masterData_o <= x"00000001";
              
              -- Change state to wait for the target port to finalize the write
              -- of the full frame.
              masterState <= STATE_WAIT_COMPLETE;
            end if;
          else
            -- Wait for the target port to reply.
            -- Dont do anything.
          end if;

        when STATE_WAIT_COMPLETE =>
          ---------------------------------------------------------------------
          -- Wait for the target port to signal that the frame has been
          -- completed.
          ---------------------------------------------------------------------

          -- Wait for the target port to complete the final request.
          if (masterAck_i = '1') then
            -- The target port has finalized the write of the frame.

            -- Reset master bus signals.
            masterCyc_o <= '0';
            masterStb_o <= '0';
            masterState <= STATE_IDLE;
          else
            -- Wait for the target port to reply.
            -- REMARK: Timeout here???
          end if;

        when others =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Slave interface process.
  -- Addr |  Read  | Write
  --    0 |  full  | abort & complete
  --    1 |  full  | frameData
  -----------------------------------------------------------------------------
  writeContentData_o <= slaveData_i;
  Slave: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      slaveState <= STATE_IDLE;

      slaveData_o <= '0';

      writeFramePort_o <= (others=>'0');
      writeFrame_o <= '0';
      writeFrameAbort_o <= '0';
      writeContent_o <= '0';
    elsif rising_edge(clk) then
      writeFrame_o <= '0';
      writeFrameAbort_o <= '0';
      writeContent_o <= '0';

      case slaveState is

        when STATE_IDLE =>
          ---------------------------------------------------------------------
          -- Wait for an access from a master.
          ---------------------------------------------------------------------

          -- Check if any cycle is active.
          if ((slaveCyc_i = '1') and (slaveStb_i = '1')) then
            -- Cycle is active.

            -- Check if the cycle is accessing the status- or data address.
            if (slaveAddr_i(0) = '0') then
              -- Accessing port status address.

              -- Check if writing.
              if (slaveWe_i = '1') then
                -- Writing the status address.
                -- Update the buffering output signals according to the input
                -- data.
                writeFramePort_o <= slaveAddr_i(8 downto 1);
                writeFrame_o <= slaveData_i(0);
                writeFrameAbort_o <= slaveData_i(1);
              else
                -- Reading the status address.
                slaveData_o <= writeFrameFull_i;
              end if;
            else
              -- Accessing port data address.

              -- Check if writing.
              if (slaveWe_i = '1') then
                -- Write frame content into the frame buffer.
                writeContent_o <= '1';
              else
                slaveData_o <= writeFrameFull_i;
              end if;
            end if;

            -- Change state to send an ack to the master.
            slaveState <= STATE_SEND_ACK;
          end if;

        when STATE_SEND_ACK =>
          ---------------------------------------------------------------------
          -- Wait for acknowledge to be received by the master.
          ---------------------------------------------------------------------

          -- Go back to the idle state and wait for a new cycle.
          slaveState <= STATE_IDLE;
          
        when others =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          null;
          
      end case;
    end if;
  end process;

  -- Assign the acknowledge depending on the current slave state.
  slaveAck_o <= '1' when (slaveState = STATE_SEND_ACK) else '0';
  
end architecture;





-------------------------------------------------------------------------------
-- SwitchPortMaintenance
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 
use work.rio_common.all;


-------------------------------------------------------------------------------
-- Entity for SwitchPortMaintenance.
-------------------------------------------------------------------------------
entity SwitchPortMaintenance is
  generic(
    SWITCH_PORTS : natural range 0 to 255;
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

    -- Routing table port lookup signals.
    lookupStb_i : in std_logic;
    lookupAddr_i : in std_logic_vector(15 downto 0);
    lookupData_o : out std_logic_vector(7 downto 0);
    lookupAck_o : out std_logic;
  
    -- Master port signals.
    -- Write frames to other ports.
    masterCyc_o : out std_logic;
    masterStb_o : out std_logic;
    masterWe_o : out std_logic;
    masterAddr_o : out std_logic_vector(9 downto 0);
    masterData_o : out std_logic_vector(31 downto 0);
    masterData_i : in std_logic;
    masterAck_i : in std_logic;

    -- Slave port signals.
    -- Receives frames from other ports.
    slaveCyc_i : in std_logic;
    slaveStb_i : in std_logic;
    slaveWe_i : in std_logic;
    slaveAddr_i : in std_logic_vector(9 downto 0);
    slaveData_i : in std_logic_vector(31 downto 0);
    slaveData_o : out std_logic;
    slaveAck_o : out std_logic;

    -- Address-lookup interface.
    lookupStb_o : out std_logic;
    lookupAddr_o : out std_logic_vector(15 downto 0);
    lookupData_i : in std_logic_vector(7 downto 0);
    lookupAck_i : in std_logic;

    -- Port common access interface.
    portLinkTimeout_o : out std_logic_vector(23 downto 0);

    -- Port specific access interface.
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
    
    -- Configuration space for implementation-defined space.
    configStb_o : out std_logic;
    configWe_o : out std_logic;
    configAddr_o : out std_logic_vector(23 downto 0);
    configData_o : out std_logic_vector(31 downto 0);
    configData_i : in std_logic_vector(31 downto 0);
    configAck_i : in std_logic);
end entity;


-------------------------------------------------------------------------------
-- Architecture for SwitchPort.
-------------------------------------------------------------------------------
architecture SwitchPortMaintenanceImpl of SwitchPortMaintenance is

  component SwitchPort is
    generic(
      MAINTENANCE_LOOKUP : boolean;
      PORT_INDEX : natural);
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      masterCyc_o : out std_logic;
      masterStb_o : out std_logic;
      masterWe_o : out std_logic;
      masterAddr_o : out std_logic_vector(9 downto 0);
      masterData_o : out std_logic_vector(31 downto 0);
      masterData_i : in std_logic;
      masterAck_i : in std_logic;

      slaveCyc_i : in std_logic;
      slaveStb_i : in std_logic;
      slaveWe_i : in std_logic;
      slaveAddr_i : in std_logic_vector(9 downto 0);
      slaveData_i : in std_logic_vector(31 downto 0);
      slaveData_o : out std_logic;
      slaveAck_o : out std_logic;

      lookupStb_o : out std_logic;
      lookupAddr_o : out std_logic_vector(15 downto 0);
      lookupData_i : in std_logic_vector(7 downto 0);
      lookupAck_i : in std_logic;

      readFrameEmpty_i : in std_logic;
      readFrame_o : out std_logic;
      readFrameRestart_o : out std_logic;
      readFrameAborted_i : in std_logic;
      readContentEmpty_i : in std_logic;
      readContent_o : out std_logic;
      readContentEnd_i : in std_logic;
      readContentData_i : in std_logic_vector(31 downto 0);
      writeFramePort_o : out std_logic_vector(7 downto 0);
      writeFrameFull_i : in std_logic;
      writeFrame_o : out std_logic;
      writeFrameAbort_o : out std_logic;
      writeContent_o : out std_logic;
      writeContentData_o : out std_logic_vector(31 downto 0));
  end component;

  component RioHotSwapPortStatus is 
    generic(LinkUninitTimerWidth : integer := 24); --RapidIO standard: 24 (can be changed for more convenient TB testing)
       port(  clk : in  std_logic;
         reset_ni : in  std_logic;
      LinkUninitTimerTick_i : in  std_logic;
      --
      linkInitialized_i                     : in  std_logic;
      LinkOKtoUninitTransitionEnable_i      : in  std_logic;
      LinkUninitToOkTransitionEnable_i      : in  std_logic;
      LinkUninitPacketDiscardActiveEnable_i : in  std_logic;   
      LinkUninitTimeout_i                   : in  std_logic_vector(LinkUninitTimerWidth-1 downto 0);
      linkOKtoUninitTransition_o            : out std_logic;
      linkUninitToOkTransition_o            : out std_logic;
      linkUninitPacketDiscardActive_o       : out std_logic;
      sendHotSwapEventNow_o                 : out std_logic);
  end component;

  component MaintenancePortManager is
     generic(SWITCH_PORTS : natural := 4;
             portWriteTimeoutResetValue : std_logic_vector(15 downto 0):= x"FFFF"); -- x"FFFF" = 100 ms
        port( clk   : in  std_logic;
           areset_n : in  std_logic;
           ------------------------------------------------------------------------------------------
           -- Signal interconnect for MaintenanceInbound
             readRequestReadyInbound_i : in  std_logic;
            writeRequestReadyInbound_i : in  std_logic;
            readResponseReadyInbound_i : in  std_logic;
           writeResponseReadyInbound_i : in  std_logic;
               portWriteReadyInbound_i : in  std_logic;
                           vcInbound_i : in  std_logic;
                          crfInbound_i : in  std_logic;
                         prioInbound_i : in  std_logic_vector( 1 downto 0);
                           ttInbound_i : in  std_logic_vector( 1 downto 0);
                        dstidInbound_i : in  std_logic_vector(31 downto 0);
                        srcidInbound_i : in  std_logic_vector(31 downto 0);                  
                         sizeInbound_i : in  std_logic_vector( 3 downto 0);                  
                       statusInbound_i : in  std_logic_vector( 3 downto 0);
                          tidInbound_i : in  std_logic_vector( 7 downto 0);
                          hopInbound_i : in  std_logic_vector( 7 downto 0);
                       offsetInbound_i : in  std_logic_vector(20 downto 0);
                        wdptrInbound_i : in  std_logic;
                payloadLengthInbound_i : in  std_logic_vector( 2 downto 0);
                 payloadIndexInbound_o : out std_logic_vector( 2 downto 0);                  
                      payloadInbound_i : in  std_logic_vector(63 downto 0);
                         doneInbound_o : out std_logic;
           ------------------------------------------------------------------------------------------
           -- Signal interconnect for MaintenanceOutbound   
             readRequestReadyOutbound_o : out std_logic;
            writeRequestReadyOutbound_o : out std_logic;
            readResponseReadyOutbound_o : out std_logic;
           writeResponseReadyOutbound_o : out std_logic;
               portWriteReadyOutbound_o : out std_logic;
                           vcOutbound_o : out std_logic;
                          crfOutbound_o : out std_logic;
                         prioOutbound_o : out std_logic_vector( 1 downto 0);
                           ttOutbound_o : out std_logic_vector( 1 downto 0);
                        dstidOutbound_o : out std_logic_vector(31 downto 0);
                        srcidOutbound_o : out std_logic_vector(31 downto 0);
                         sizeOutbound_o : out std_logic_vector( 3 downto 0);
                       statusOutbound_o : out std_logic_vector( 3 downto 0);
                          tidOutbound_o : out std_logic_vector( 7 downto 0);
                          hopOutbound_o : out std_logic_vector( 7 downto 0);
                       offsetOutbound_o : out std_logic_vector(20 downto 0);
                        wdptrOutbound_o : out std_logic;
                payloadLengthOutbound_o : out std_logic_vector( 2 downto 0);
                 payloadIndexOutbound_i : in  std_logic_vector( 2 downto 0);
                      payloadOutbound_o : out std_logic_vector(63 downto 0);
                         doneOutbound_i : in  std_logic;
           ------------------------------------------------------------------------------------------
           -- Signal interconnect for RioLogicalMaintenance 
                  readRequestReadyMnt_o : out std_logic;
                 writeRequestReadyMnt_o : out std_logic;
                              sizeMnt_o : out std_logic_vector( 3 downto 0);
                            offsetMnt_o : out std_logic_vector(20 downto 0);
                             wdptrMnt_o : out std_logic;
                     payloadLengthMnt_o : out std_logic_vector( 2 downto 0);
                      payloadIndexMnt_i : in  std_logic_vector( 2 downto 0);
                           payloadMnt_o : out std_logic_vector(63 downto 0);
                              doneMnt_i : in  std_logic;
                 readResponseReadyMnt_i : in  std_logic;
                writeResponseReadyMnt_i : in  std_logic;
                            statusMnt_i : in  std_logic_vector( 3 downto 0);
                     payloadLengthMnt_i : in  std_logic_vector( 2 downto 0);
                      payloadIndexMnt_o : out std_logic_vector( 2 downto 0);
                           payloadMnt_i : in  std_logic_vector(63 downto 0);
                              doneMnt_o : out std_logic;
           ------------------------------------------------------------------------------------------
           --
                     inboundFramePort_i : in  std_logic_vector( 7 downto 0);
                    outboundFramePort_o : out std_logic_vector( 7 downto 0);
                           lookupData_i : in  std_logic_vector( 7 downto 0);
                           lookupAddr_o : out std_logic_vector(15 downto 0);
                            lookupStb_o : out std_logic;
                            lookupAck_i : in  std_logic;
           ------------------------------------------------------------------------------------------
           -- Various signals for the Error Reporting Port-Write Packet Data Payload/HotSwap event --
                         hotSwapEvent_i : in  std_logic_vector(SWITCH_PORTS-1 downto 0);
                  --           Dev32_PW : in  std_logic;
                  --         Dev8_or_16 : in  std_logic;
                     Dev16_deviceID_msb : in  std_logic_vector(7 downto 0); 
                          Dev8_deviceID : in  std_logic_vector(7 downto 0); 
                  --     Dev32_deviceID : in  std_logic_vector(31 downto 0) := (others=>'0');
         PortWriteTransmissionDisable_i : in  std_logic;
                           Tick_1_5us_i : in  std_logic;
                     ComponentTag_CSR_i : in  std_logic_vector(31 downto 0);                  
                   portNwriteDisabled_i : in  std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0');
                    PortNwritePending_i : in  std_logic_vector(SWITCH_PORTS-1 downto 0);
                 setPortNwritePending_o : out std_logic_vector(SWITCH_PORTS-1 downto 0);
                           PortNindex_o : out integer range 0 to SWITCH_PORTS;
                 PortNerrorDetect_CSR_i : in  std_logic_vector(31 downto 0);
  LogicalTransportLayerErrorDetectCSR_i : in  std_logic_vector(31 downto 0) := (others=>'0') );
  end component;
      
  -----------------------------------------------------------------------------
  -- Signals between the port and the packet-queue.
  -----------------------------------------------------------------------------
  
  signal outboundFramePort, outboundFramePort0 : std_logic_vector(7 downto 0);
  signal outboundReadFrameEmpty : std_logic;
  signal outboundReadFrame : std_logic;
  signal outboundReadContent : std_logic;
  signal outboundReadContentEnd : std_logic;
  signal outboundReadContentData : std_logic_vector(31 downto 0);
  signal inboundFramePort, inboundFramePort0 : std_logic_vector(7 downto 0);
  signal inboundWriteFrameFull : std_logic;
  signal inboundWriteFrame : std_logic;
  signal inboundWriteFrameAbort : std_logic;
  signal inboundWriteContent : std_logic;
  signal inboundWriteContentData : std_logic_vector(31 downto 0);

  -----------------------------------------------------------------------------
  -- Signals between the packet-queue and RioLogicalCommon.
  -----------------------------------------------------------------------------

  signal inboundReadFrameEmpty : std_logic;
  signal inboundReadFrame : std_logic;
  signal inboundReadContent : std_logic;
  signal inboundReadContentEnd : std_logic;
  signal inboundReadContentData : std_logic_vector(31 downto 0);
  signal outboundWriteFrameFull : std_logic;
  signal outboundWriteFrame : std_logic;
  signal outboundWriteFrameAbort : std_logic;
  signal outboundWriteContent : std_logic;
  signal outboundWriteContentData : std_logic_vector(31 downto 0);

  -----------------------------------------------------------------------------
  -- Signals between RioLogicalCommon and PacketHandler.
  -----------------------------------------------------------------------------

  signal inboundStb : std_logic;
  signal inboundAdr : std_logic_vector(3 downto 0);
  signal inboundDat : std_logic_vector(31 downto 0);
  signal inboundStall : std_logic;
  signal outboundStb : std_logic_vector(0 downto 0);
  signal outboundAdr : std_logic_vector(0 downto 0);
  signal outboundDat : std_logic_vector(31 downto 0);
  signal outboundStall : std_logic_vector(0 downto 0);

  -----------------------------------------------------------------------------
  -- Signals between PacketHandlers and maintenance controllers.
  -----------------------------------------------------------------------------

  signal readRequestInbound : std_logic;
  signal writeRequestInbound : std_logic;
  signal readResponseInbound : std_logic;
  signal writeResponseInbound : std_logic;
  signal portWriteInbound : std_logic;
  signal dstIdInbound : std_logic_vector(31 downto 0);
  signal srcIdInbound : std_logic_vector(31 downto 0);
  signal sizeInbound : std_logic_vector(3 downto 0);
  signal statusInbound : std_logic_vector(3 downto 0);
  signal hopInbound : std_logic_vector(7 downto 0);
  signal offsetInbound : std_logic_vector(20 downto 0);
  signal wdptrInbound: std_logic;
  signal payloadLengthInbound : std_logic_vector(2 downto 0);
  signal payloadIndexInbound : std_logic_vector(2 downto 0);
  signal payloadInbound : std_logic_vector(63 downto 0);
  signal doneInbound : std_logic;

  signal readRequestOutbound : std_logic;
  signal writeRequestOutbound : std_logic;
  signal readResponseOutbound : std_logic;
  signal writeResponseOutbound : std_logic;
  signal portWriteOutbound : std_logic;
  signal dstIdOutbound : std_logic_vector(31 downto 0);
  signal srcIdOutbound : std_logic_vector(31 downto 0);
  signal statusOutbound : std_logic_vector(3 downto 0);
  signal hopOutbound : std_logic_vector(7 downto 0);
  signal payloadLengthOutbound : std_logic_vector(2 downto 0);
  signal payloadIndexOutbound : std_logic_vector(2 downto 0);
  signal payloadOutbound : std_logic_vector(63 downto 0);
  signal doneOutbound : std_logic;

  signal readRequestMaint : std_logic;
  signal writeRequestMaint : std_logic;
  signal readResponseMaint : std_logic;
  signal writeResponseMaint : std_logic;
  signal statusMaint : std_logic_vector(3 downto 0);
  signal payloadLengthMaint : std_logic_vector(2 downto 0);
  signal payloadIndexMaint : std_logic_vector(2 downto 0);
  signal payloadMaint : std_logic_vector(63 downto 0);
  signal doneMaint : std_logic;
  
  -----------------------------------------------------------------------------
  -- Route table access signals.
  -----------------------------------------------------------------------------

  signal lookupEnable : std_logic;
  signal lookupAddress : std_logic_vector(10 downto 0);
  signal lookupData : std_logic_vector(7 downto 0);
  signal lookupAck : std_logic;

  signal routeTableEnable : std_logic;
  signal routeTableWrite : std_logic;
  signal routeTableAddress : std_logic_vector(10 downto 0);
  signal routeTablePortWrite : std_logic_vector(7 downto 0);
  signal routeTablePortRead : std_logic_vector(7 downto 0);
  
  signal routeTablePortDefault : std_logic_vector(7 downto 0);

  -----------------------------------------------------------------------------
  -- Configuration space signals.
  -----------------------------------------------------------------------------

  signal configStb, configStbInternal : std_logic;
  signal configWe : std_logic;
  signal configAdr : std_logic_vector(23 downto 0);
  signal configDataWrite : std_logic_vector(31 downto 0);
  signal configDataRead, configDataReadInternal : std_logic_vector(31 downto 0);
  signal configAck, configAckInternal : std_logic;

  signal discovered : std_logic;
  signal hostBaseDeviceIdLocked : std_logic;
  signal hostBaseDeviceId : std_logic_vector(15 downto 0);
  signal componentTag : std_logic_vector(31 downto 0);
  signal portLinkTimeout : std_logic_vector(23 downto 0);
  signal outputPortEnable : Array1(SWITCH_PORTS-1 downto 0);
  signal inputPortEnable : Array1(SWITCH_PORTS-1 downto 0);
  
  constant LogicalTransportLayerErrorDetectCSR : std_logic_vector(31 downto 0) := (others=>'0');
  
  -----------------------------------------------------------------------------
  -- HotSwap related signals.
  -----------------------------------------------------------------------------

  constant ErrorManagementNotFullySupported    : std_logic := '1';  -- '1'=all registers/bit fields of Error management extensions may not be supported. 0=all supported.
  constant HotSwapFullySupported               : std_logic := '1';  -- '1'=all registers/bit fields specific to HotSwap support shall be supported. 0=not all fields are supported.
  constant PHlayerErrorCaptureFifoSupport      : std_logic := '0';  -- Physical Layer Error Capture FIFO: 0=may not be supported, 1=full support            ****check this bit!!****
  constant LTlayerErrorCaptureFifoSupport      : std_logic := '0';  -- Logical/Transport Layer Error Capture FIFO: 0=may not be supported, 1=full support   ****check this bit!!****
  
--signal   Dev8_or_16                          : std_logic := '0';                               -- When dev32_PW=0:  '0'=dev8, '1'=dev16 id-size
--signal   Dev32_PW                            : std_logic := '0';                               -- '0'=devID is controlled by Dev8_or_16 field, '1'=dev32 deviceID
--signal   Dev32_deviceID                      : std_logic_vector(31 downto 0) := (others=>'0'); --ID to use when Dev32_PW bit is set.
  signal   Dev16_deviceID_msb                  : std_logic_vector(7 downto 0) := (others=>'0');  -- Address to use when a device generates a Maintenance Port Write to rweport errors to system host.
  signal   Dev8_deviceID                       : std_logic_vector(7 downto 0) := (others=>'0');
  
  signal   PacketTimeToLiveValue               : std_logic_vector(15 downto 0) := (others=>'0'); --Maximum time a packet is allowed to exist within a switch device. 0xFFFF=100ms+-34%, 0=timeout is disabled.
  signal   PortWriteTransmissionDisable        : std_logic := '0';                               --'1' enabled events shall not cause new port writes to be generated. '0'=port writes shall be generated.
  
  signal   linkOKtoUninitTransition            : std_logic_vector(SWITCH_PORTS-1 downto 0);      -- '1'=link has transitioned from linkInit to linkUninit state    
  signal   linkUninitPacketDiscardActive       : std_logic_vector(SWITCH_PORTS-1 downto 0);      -- '1'=The link uninit discard timer CSR period has expired.      
  signal   linkUninitToOkTransition            : std_logic_vector(SWITCH_PORTS-1 downto 0);      -- '1'=The link has transitioned from uninit to initialized state.
  signal   LinkOKtoUninitTransitionEnable      : std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0'); -- Enable event notification for when the link has transitioned from a link initialized to link uninitialized state.
  signal   LinkUninitPacketDiscardActiveEnable : std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0'); -- Enable event notification for Link Uninit Packet Discard Timer events.                                           
  signal   LinkUninitToOkTransitionEnable      : std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0'); -- Enable event notification for when the link has transitioned from a link uninitialized to link initialized state.
  signal   hotSwapEvent                        : std_logic_vector(SWITCH_PORTS-1 downto 0);
  
--signal   packetReadyForTransmission          : std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0');
--signal   discardPacket                       : std_logic_vector(SWITCH_PORTS-1 downto 0);
  
  signal   PacketTimeToLiveTick : std_logic;
  signal   LinkUninitTimerTick  : std_logic;

  type     LinkUninitTimeout_Type        is array (0 to SWITCH_PORTS-1) of std_logic_vector(23 downto 0);
  signal   LinkUninitTimeout         : LinkUninitTimeout_Type := (others=>(others=>'0')); -- 0xFFFFFF shall correspond to 6-12 s, if 0 the discard timer shall be disabled.

  type        PortNerrorDetect_CSR_Type  is array (0 to SWITCH_PORTS-1) of std_logic_vector(31 downto 0);
  signal      PortNerrorDetect_CSR   : PortNerrorDetect_CSR_Type := (others=>(others=>'0'));       -- Error/hotswap:  Port N Error Detect CSR
  signal   stbPortNerrorDetect_CSR   : std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0');
  signal   wrtPortNerrorDetect_CSR   : PortNerrorDetect_CSR_Type := (others=>(others=>'0'));
  
  signal   setPortNwritePending      : std_logic_vector(SWITCH_PORTS-1 downto 0);                  --when '1', set the bit in PortNwritePending "sticky", from RioMaintenancePortManager
  signal   stbPortNwritePending      : std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0'); --write strobe for access from config access.
  signal      PortNwritePending      : std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0'); --LP-serial: Port N Error and Status CSR: port-write Pending  (std:bit 27)
  signal      portNwriteDisabled     : std_logic_vector(SWITCH_PORTS-1 downto 0) := (others=>'0'); --LP-serial: Port N Error and Status CSR: Port-write Disabled (std:bit 26)
  
  signal   vcOutbound     : std_logic;
  signal   crfOutbound    : std_logic;
  signal   prioOutbound   : std_logic_vector( 1 downto 0);
  signal   ttOutbound     : std_logic_vector( 1 downto 0);
  signal   tidOutbound    : std_logic_vector( 7 downto 0);
  signal   sizeOutbound   : std_logic_vector( 3 downto 0);
  signal   offsetOutbound : std_logic_vector(20 downto 0);
  signal   wdptrOutbound  : std_logic;
  
  signal   vcInbound      : std_logic;
  signal   crfInbound     : std_logic;
  signal   prioInbound    : std_logic_vector( 1 downto 0);
  signal   ttInbound      : std_logic_vector( 1 downto 0);
  signal   tidInbound     : std_logic_vector( 7 downto 0);

  signal   sizeMaint      : std_logic_vector( 3 downto 0);
  signal   offsetMaint    : std_logic_vector(20 downto 0);
  signal   wdptrMaint     : std_logic;
  signal   payloadLengthOutboundMaint : std_logic_vector( 2 downto 0);
  signal   payloadIndexOutboundMaint  : std_logic_vector( 2 downto 0);
  signal   payloadOutboundMaint       : std_logic_vector(63 downto 0);
  signal   doneOutboundMaint          : std_logic;

  --used for Port-write datapacket payload generation  
  signal   PortNerrorDetect : std_logic_vector(31 downto 0) := (others=>'0');
  signal   PortNindex       : integer range 0 to SWITCH_PORTS;
  
  -----------------------------------------------------------------------------
  
begin

  -----------------------------------------------------------------------------
  -- Normal switch port instance interfacing the switch interconnect.
  -----------------------------------------------------------------------------
  -- Note that PORT_INDEX is not used in this instantiation and set to zero.
  PortInst: SwitchPort
    generic map(
      MAINTENANCE_LOOKUP=>true,
      PORT_INDEX=>0)
    port map(
      clk=>clk, areset_n=>areset_n,
      masterCyc_o=>masterCyc_o,
      masterStb_o=>masterStb_o,
      masterWe_o=>masterWe_o,
      masterAddr_o=>masterAddr_o,
      masterData_o=>masterData_o,
      masterData_i=>masterData_i,
      masterAck_i=>masterAck_i,
      slaveCyc_i=>slaveCyc_i,
      slaveStb_i=>slaveStb_i,
      slaveWe_i=>slaveWe_i,
      slaveAddr_i=>slaveAddr_i,
      slaveData_i=>slaveData_i,
      slaveData_o=>slaveData_o,
      slaveAck_o=>slaveAck_o,
      lookupStb_o=>open,
      lookupAddr_o=>open, 
      lookupData_i=>outboundFramePort0,
      lookupAck_i=>'1',
      readFrameEmpty_i=>outboundReadFrameEmpty,
      readFrame_o=>outboundReadFrame, 
      readFrameRestart_o=>open,
      readFrameAborted_i=>'0', 
      readContentEmpty_i=>'0',
      readContent_o=>outboundReadContent, 
      readContentEnd_i=>outboundReadContentEnd,
      readContentData_i=>outboundReadContentData, 
      writeFramePort_o=>inboundFramePort0,
      writeFrameFull_i=>inboundWriteFrameFull,
      writeFrame_o=>inboundWriteFrame, 
      writeFrameAbort_o=>inboundWriteFrameAbort,
      writeContent_o=>inboundWriteContent, 
      writeContentData_o=>inboundWriteContentData);

  process(clk)
  begin
    if rising_edge(clk) then
      if (inboundReadFrame = '1') then
        inboundFramePort <= inboundFramePort0;
      end if;
      if (outboundWriteFrame = '1') then
        outboundFramePort0 <= outboundFramePort;
      end if;
    end if;
  end process;
  
  -----------------------------------------------------------------------------
  -- Packet queue.
  -- This queue should only contain one packet.
  -----------------------------------------------------------------------------
  -- REMARK: Use a packet-buffer with a configurable maximum sized packet. The
  -- size of the resulting memory is larger than needed since maintenance
  -- packets never contain more than 8 double-words.
  PacketQueue: RioPacketBuffer
    generic map(SIZE_ADDRESS_WIDTH=>1, CONTENT_ADDRESS_WIDTH=>7)
    port map(
      clk=>clk, areset_n=>areset_n, 
      inboundWriteFrameFull_o=>inboundWriteFrameFull, 
      inboundWriteFrame_i=>inboundWriteFrame, 
      inboundWriteFrameAbort_i=>inboundWriteFrameAbort, 
      inboundWriteContent_i=>inboundWriteContent, 
      inboundWriteContentData_i=>inboundWriteContentData, 
      inboundReadFrameEmpty_o=>inboundReadFrameEmpty, 
      inboundReadFrame_i=>inboundReadFrame, 
      inboundReadFrameRestart_i=>'0', 
      inboundReadFrameAborted_o=>open, 
      inboundReadContentEmpty_o=>open, 
      inboundReadContent_i=>inboundReadContent, 
      inboundReadContentEnd_o=>inboundReadContentEnd, 
      inboundReadContentData_o=>inboundReadContentData, 
      outboundWriteFrameFull_o=>outboundWriteFrameFull, 
      outboundWriteFrame_i=>outboundWriteFrame, 
      outboundWriteFrameAbort_i=>outboundWriteFrameAbort, 
      outboundWriteContent_i=>outboundWriteContent, 
      outboundWriteContentData_i=>outboundWriteContentData, 
      outboundReadFrameEmpty_o=>outboundReadFrameEmpty, 
      outboundReadFrame_i=>outboundReadFrame, 
      outboundReadFrameRestart_i=>'0', 
      outboundReadFrameAborted_o=>open, 
      outboundReadContentEmpty_o=>open, 
      outboundReadContent_i=>outboundReadContent, 
      outboundReadContentEnd_o=>outboundReadContentEnd, 
      outboundReadContentData_o=>outboundReadContentData);
  
  -----------------------------------------------------------------------------
  -- Logical common packet parser.
  -- This module removes CRC and unpack addresses in the inbound direction and
  -- adds CRC and packs addresses in the outbound direction.
  -----------------------------------------------------------------------------
  LogicalCommon: RioLogicalCommon
    generic map(PORTS=>1)
    port map(
      clk=>clk, areset_n=>areset_n, enable=>'1', 
      readFrameEmpty_i=>inboundReadFrameEmpty, 
      readFrame_o=>inboundReadFrame, 
      readContent_o=>inboundReadContent, 
      readContentEnd_i=>inboundReadContentEnd, 
      readContentData_i=>inboundReadContentData, 
      writeFrameFull_i=>outboundWriteFrameFull, 
      writeFrame_o=>outboundWriteFrame, 
      writeFrameAbort_o=>outboundWriteFrameAbort, 
      writeContent_o=>outboundWriteContent, 
      writeContentData_o=>outboundWriteContentData, 
      inboundStb_o=>inboundStb, 
      inboundAdr_o=>inboundAdr, 
      inboundDat_o=>inboundDat, 
      inboundStall_i=>inboundStall, 
      outboundStb_i=>outboundStb,
      outboundAdr_i=>outboundAdr,
      outboundDat_i=>outboundDat, 
      outboundStall_o=>outboundStall);

  -----------------------------------------------------------------------------
  -- Inbound maintenance packet parser.
  -- Unpack inbound maintenance packets.
  -----------------------------------------------------------------------------
  InboundPacket: MaintenanceInbound
    port map(
      clk=>clk, areset_n=>areset_n, enable=>'1', 
      readRequestReady_o  =>readRequestInbound,
      writeRequestReady_o =>writeRequestInbound,
      readResponseReady_o =>readResponseInbound,
      writeResponseReady_o=>writeResponseInbound,
      portWriteReady_o    =>portWriteInbound,
      vc_o=>vcInbound, 
      crf_o=>crfInbound, 
      prio_o=>prioInbound, 
      tt_o=>ttInbound, 
      dstid_o=>dstIdInbound, 
      srcid_o=>srcIdInbound,
      size_o=>sizeInbound,
      status_o=>statusInbound,
      tid_o=>tidInbound,
      hop_o=>hopInbound,
      offset_o=>offsetInbound,
      wdptr_o=>wdptrInbound,
      payloadLength_o=>payloadLengthInbound, 
      payloadIndex_i=>payloadIndexInbound, 
      payload_o=>payloadInbound, 
      done_i=>doneInbound, 
      inboundStb_i=>inboundStb, 
      inboundAdr_i=>inboundAdr, 
      inboundDat_i=>inboundDat, 
      inboundStall_o=>inboundStall);

  -----------------------------------------------------------------------------
  -- Outbound maintenance packet generator.
  -----------------------------------------------------------------------------
  OutboundPacket: MaintenanceOutbound
    port map(
      clk=>clk, areset_n=>areset_n, enable=>'1', 
      readRequestReady_i  =>readRequestOutbound,
      writeRequestReady_i =>writeRequestOutbound,
      readResponseReady_i =>readResponseOutbound, 
      writeResponseReady_i=>writeResponseOutbound, 
      portWriteReady_i    =>portWriteOutbound,
      vc_i     => vcOutbound, 
      crf_i    => crfOutbound, 
      prio_i   => prioOutbound, 
      tt_i     => ttOutbound, 
      dstid_i  => dstIdOutbound, 
      srcid_i  => srcIdOutbound,
      size_i   => sizeOutbound,
      status_i => statusOutbound,
      tid_i    => tidOutbound,
      hop_i    => hopOutbound,
      offset_i => offsetOutbound,
      wdptr_i  => wdptrOutbound,
      payloadLength_i=>payloadLengthOutbound, 
      payloadIndex_o=>payloadIndexOutbound, 
      payload_i=>payloadOutbound, 
      done_o=>doneOutbound, 
      outboundStb_o=>outboundStb(0),
      outboundAdr_o=>outboundAdr(0),
      outboundDat_o=>outboundDat, 
      outboundStall_i=>outboundStall(0));

  -----------------------------------------------------------------------------
  -- Maintenance Port Manager
  -----------------------------------------------------------------------------
  MntPortManager: MaintenancePortManager
    generic map(SWITCH_PORTS => SWITCH_PORTS,
                portWriteTimeoutResetValue => portWriteTimeoutResetValue)
      port map(      clk => clk, 
                areset_n => areset_n, 
     --
     -- Signal interconnect for MaintenanceInbound    --
       readRequestReadyInbound_i => readRequestInbound,  
      writeRequestReadyInbound_i => writeRequestInbound, 
      readResponseReadyInbound_i => readResponseInbound, 
     writeResponseReadyInbound_i => writeResponseInbound,
         portWriteReadyInbound_i => portWriteInbound,    
                     vcInbound_i => vcInbound,
                    crfInbound_i => crfInbound,
                   prioInbound_i => prioInbound, 
                     ttInbound_i => ttInbound,   
                  dstidInbound_i => dstIdInbound, 
                  srcidInbound_i => srcIdInbound,                  
                   sizeInbound_i => sizeInbound,                   
                 statusInbound_i => statusInbound,
                    tidInbound_i => tidInbound,   
                    hopInbound_i => hopInbound,   
                 offsetInbound_i => offsetInbound,
                  wdptrInbound_i => wdptrInbound,
          payloadLengthInbound_i => payloadLengthInbound, 
           payloadIndexInbound_o => payloadIndexInbound,                    
                payloadInbound_i => payloadInbound,       
                   doneInbound_o => doneInbound,
    --
    -- Signal interconnect for MaintenanceOutbound   --
      readRequestReadyOutbound_o => readRequestOutbound,  
     writeRequestReadyOutbound_o => writeRequestOutbound, 
     readResponseReadyOutbound_o => readResponseOutbound, 
    writeResponseReadyOutbound_o => writeResponseOutbound,
        portWriteReadyOutbound_o => portWriteOutbound,    
                    vcOutbound_o => vcOutbound, 
                   crfOutbound_o => crfOutbound,
                  prioOutbound_o => prioOutbound,    
                    ttOutbound_o => ttOutbound,      
                 dstidOutbound_o => dstIdOutbound,   
                 srcidOutbound_o => srcIdOutbound,   
                  sizeOutbound_o => sizeOutbound,    
                statusOutbound_o => statusOutbound,  
                   tidOutbound_o => tidOutbound,     
                   hopOutbound_o => hopOutbound,     
                offsetOutbound_o => offsetOutbound,  
                 wdptrOutbound_o => wdptrOutbound,
         payloadLengthOutbound_o => payloadLengthOutbound, 
          payloadIndexOutbound_i => payloadIndexOutbound,  
               payloadOutbound_o => payloadOutbound,       
                  doneOutbound_i => doneOutbound,    
            -- 
            -- Signal interconnect for RioLogicalMaintenance --
           readRequestReadyMnt_o => readRequestMaint,           
          writeRequestReadyMnt_o => writeRequestMaint,          
                       sizeMnt_o => sizeMaint,                  
                     offsetMnt_o => offsetMaint,                
                      wdptrMnt_o => wdptrMaint,                 
              payloadLengthMnt_o => payloadLengthMaint,         
               payloadIndexMnt_i => payloadIndexMaint,          
                    payloadMnt_o => payloadMaint,               
                       doneMnt_i => doneMaint,                  
          readResponseReadyMnt_i => readResponseMaint,          
         writeResponseReadyMnt_i => writeResponseMaint,         
                     statusMnt_i => statusMaint,                
              payloadLengthMnt_i => payloadLengthOutboundMaint, 
               payloadIndexMnt_o => payloadIndexOutboundMaint,  
                    payloadMnt_i => payloadOutboundMaint,       
                       doneMnt_o => doneOutboundMaint,          
            --
            --
              inboundFramePort_i => inboundFramePort,
             outboundFramePort_o => outboundFramePort,
                    lookupData_i => lookupData_i,
                    lookupAddr_o => lookupAddr_o,
                     lookupStb_o => lookupStb_o,
                     lookupAck_i => lookupAck_i,
            --
            -- Various signals for the Error Reporting Port-Write Packet Data Payload/HotSwap event --
                  hotSwapEvent_i => hotSwapEvent,        
                --      Dev32_PW => Dev32_PW,            
                --    Dev8_or_16 => Dev8_or_16,          
              Dev16_deviceID_msb => Dev16_deviceID_msb,  
                   Dev8_deviceID => Dev8_deviceID,       
                --Dev32_deviceID => Dev32_deviceID,      
  PortWriteTransmissionDisable_i => PortWriteTransmissionDisable,
                    Tick_1_5us_i => PacketTimeToLiveTick,
              ComponentTag_CSR_i => ComponentTag,
            portNwriteDisabled_i => portNwriteDisabled,
             PortNwritePending_i => PortNwritePending,
          setPortNwritePending_o => setPortNwritePending, 
                    PortNindex_o => PortNindex,          
          PortNerrorDetect_CSR_i => PortNerrorDetect,
  LogicalTransportLayerErrorDetectCSR_i => LogicalTransportLayerErrorDetectCSR
        );   
   
  -----------------------------------------------------------------------------
  -- Bridge between the inbound RapidIO maintenance packets to the internal
  -- config-space bus.
  -----------------------------------------------------------------------------
  MaintenanceBridge: RioLogicalMaintenance
    port map(
      clk=>clk, areset_n=>areset_n, enable=>'1', 
      readRequestReady_i=>readRequestMaint, 
      writeRequestReady_i=>writeRequestMaint,
      size_i=>sizeMaint,
      offset_i=>offsetMaint, 
      wdptr_i=>wdptrMaint, 
      payloadLength_i=>payloadLengthMaint, 
      payloadIndex_o=>payloadIndexMaint, 
      payload_i=>payloadMaint, 
      done_o=>doneMaint, 
      readResponseReady_o=> readResponseMaint, 
      writeResponseReady_o=>writeResponseMaint,
      status_o=>statusMaint,
      payloadLength_o=>payloadLengthOutboundMaint, 
      payloadIndex_i=>payloadIndexOutboundMaint, 
      payload_o=>payloadOutboundMaint, 
      done_i=>doneOutboundMaint, 
      configStb_o=>configStb, 
      configWe_o=>configWe, 
      configAdr_o=>configAdr(23 downto 2), 
      configDat_o=>configDataWrite, 
      configDat_i=>configDataRead, 
      configAck_i=>configAck);
  configAdr(1 downto 0) <= "00";
  
  -----------------------------------------------------------------------------
  -- Switch configuration memory.
  -----------------------------------------------------------------------------
  portLinkTimeout_o  <= portLinkTimeout;
  outputPortEnable_o <= outputPortEnable;
  inputPortEnable_o  <=  inputPortEnable;

  configStb_o <= '1' when ((configStb = '1') and (configAdr(23 downto 16) /= x"00")) else '0';
  configStbInternal <= '1' when ((configStb = '1') and (configAdr(23 downto 16) = x"00")) else '0';
  configWe_o <= configWe;
  configAddr_o <= configAdr;
  configData_o <= configDataWrite;
  configDataRead <= configData_i when (configStbInternal = '0') else configDataReadInternal;
  configAck <= configAck_i when (configStbInternal = '0') else configAckInternal;
  
  ConfigMemory: process(areset_n, clk)
  begin
    if (areset_n = '0') then
      configDataReadInternal <= (others => '0');
      configAckInternal <= '0';

      routeTableEnable <= '1';
      routeTableWrite <= '0';
      routeTableAddress <= (others => '0');
      routeTablePortWrite <= (others => '0');
      routeTablePortDefault <= (others => '0');

      discovered <= '0';
      
      hostBaseDeviceIdLocked <= '0';
      hostBaseDeviceId <= (others => '1');
      componentTag <= (others => '0');

      portLinkTimeout   <= (others => '1');
      portNwriteDisabled <= (others => '0');

      -- REMARK: These should be set to zero when a port gets initialized...
      outputPortEnable <= (others => '0');
      inputPortEnable <= (others => '0');

      localAckIdWrite_o <= (others => '0');
      
      --Error management/HotSwap register fields
      PortWriteTransmissionDisable<='0';
    --Dev8_or_16 <= '0';    
    --Dev32_PW   <= '0';    
    --Dev32_deviceID    <= (others=>'0');
      Dev16_deviceID_msb<= (others=>'0');
      Dev8_deviceID     <= (others=>'0');    
      PacketTimeToLiveValue <= (others=>'0');
      LinkOKtoUninitTransitionEnable <= (others=>'0');
      LinkUninitToOkTransitionEnable <= (others=>'0');
      LinkUninitPacketDiscardActiveEnable <= (others=>'0');
      stbPortNerrorDetect_CSR <= (others=>'0');
      
    elsif rising_edge(clk) then
      stbPortNerrorDetect_CSR <= (others=>'0'); --Notice that value may change further down in this process!
      stbPortNwritePending <= (others=>'0');
      routeTableWrite     <= '0';
      localAckIdWrite_o   <= (others => '0');
      
      if (configAckInternal = '0') then
        if (configStbInternal = '1') then
          configAckInternal <= '1';
          
          -- Check if the access is into implementation defined space or if the
          -- access should be handled here.
          if (configAdr(23 downto 16) /= x"00") then
            -- Accessing implementation defined space.
            -- Make an external access and return the resonse.
            configDataReadInternal <= (others=>'0');
          else
            -- Access should be handled here.
            
            case (configAdr) is
              when x"000000" =>
                -----------------------------------------------------------------
                -- Device Identity CAR. Read-only.
                -----------------------------------------------------------------

                configDataReadInternal(31 downto 16) <= DEVICE_IDENTITY;
                configDataReadInternal(15 downto 0) <= DEVICE_VENDOR_IDENTITY;
                
              when x"000004" =>
                -----------------------------------------------------------------
                -- Device Information CAR. Read-only.
                -----------------------------------------------------------------

                configDataReadInternal(31 downto 0) <= DEVICE_REV;
                
              when x"000008" =>
                -----------------------------------------------------------------
                -- Assembly Identity CAR. Read-only.
                -----------------------------------------------------------------

                configDataReadInternal(31 downto 16) <= ASSY_IDENTITY;
                configDataReadInternal(15 downto 0) <= ASSY_VENDOR_IDENTITY;
                
              when x"00000c" =>
                -----------------------------------------------------------------
                -- Assembly Informaiton CAR. Read-only.
                -----------------------------------------------------------------

                configDataReadInternal(31 downto 16) <= ASSY_REV;
                configDataReadInternal(15 downto 0) <= x"0100";
                
              when x"000010" =>
                -----------------------------------------------------------------
                -- Processing Element Features CAR. Read-only.
                -----------------------------------------------------------------
                
                -- Bridge.
                configDataReadInternal(31) <= '0';
                
                -- Memory.
                configDataReadInternal(30) <= '0';
                
                -- Processor.
                configDataReadInternal(29) <= '0';
                
                -- Switch.
                configDataReadInternal(28) <= '1';
                
                -- Reserved.
                configDataReadInternal(27 downto 10) <= (others => '0');
                
                -- Extended route table configuration support.
                configDataReadInternal(9) <= '0';
                
                -- Standard route table configuration support.
                configDataReadInternal(8) <= '1';
                
                -- Reserved.
                configDataReadInternal(7 downto 5) <= (others => '0');
                
                -- Common transport large system support.
                configDataReadInternal(4) <= '1';
                
                -- Extended features.
                configDataReadInternal(3) <= '1';
                
                -- Extended addressing support.
                -- Not a processing element.
                configDataReadInternal(2 downto 0) <= "000";
                
              when x"000014" =>
                -----------------------------------------------------------------
                -- Switch Port Information CAR. Read-only.
                -----------------------------------------------------------------

                -- Reserved.
                configDataReadInternal(31 downto 16) <= (others => '0');

                -- PortTotal.
                configDataReadInternal(15 downto 8) <=
                  std_logic_vector(to_unsigned(SWITCH_PORTS, 8));

                -- PortNumber.
                configDataReadInternal(7 downto 0) <= inboundFramePort;
                
              when x"000034" =>
                -----------------------------------------------------------------
                -- Switch Route Table Destination ID Limit CAR.
                -----------------------------------------------------------------

                -- Max_destId.
                -- Support 2048 addresses.
                configDataReadInternal(15 downto 0) <= x"0800";
                
              when x"000068" =>
                -----------------------------------------------------------------
                -- Host Base Device ID Lock CSR.
                -----------------------------------------------------------------

                if (configWe = '1') then
                  -- Check if this field has been written before.
                  if (hostBaseDeviceIdLocked = '0') then
                    -- The field has not been written.
                    -- Lock the field and set the host base device id.
                    hostBaseDeviceIdLocked <= '1';
                    hostBaseDeviceId <= configDataWrite(15 downto 0);
                  else
                    -- The field has been written.
                    -- Check if the written data is the same as the stored.
                    if (hostBaseDeviceId = configDataWrite(15 downto 0)) then
                      -- Same as stored, reset the value to its initial value.
                      hostBaseDeviceIdLocked <= '0';
                      hostBaseDeviceId <= (others => '1');
                    else
                      -- Not writing the same as the stored value.
                      -- Ignore the write.
                    end if;
                  end if;
                end if;
                
                configDataReadInternal(31 downto 16) <= (others => '0');
                configDataReadInternal(15 downto 0) <= hostBaseDeviceId;
                
              when x"00006c" =>
                -----------------------------------------------------------------
                -- Component TAG CSR.
                -----------------------------------------------------------------

                if (configWe = '1') then
                  componentTag <= configDataWrite;
                end if;
                
                configDataReadInternal <= componentTag;
                
              when x"000070" =>
                -----------------------------------------------------------------
                -- Standard Route Configuration Destination ID Select CSR.
                -----------------------------------------------------------------             

                if (configWe = '1') then
                  -- Write the address to access the routing table.
                  routeTableAddress <= configDataWrite(10 downto 0);
                end if;
                
                configDataReadInternal(31 downto 11) <= (others => '0');
                configDataReadInternal(10 downto 0) <= routeTableAddress;
                
              when x"000074" =>
                -----------------------------------------------------------------
                -- Standard Route Configuration Port Select CSR.
                -----------------------------------------------------------------

                if (configWe = '1') then
                  -- Write the port information for the address selected by the
                  -- above register.
                  routeTableWrite <= '1';
                  routeTablePortWrite <= configDataWrite(7 downto 0);
                end if;

                configDataReadInternal(31 downto 8) <= (others => '0');
                configDataReadInternal(7 downto 0) <= routeTablePortRead;
                
              when x"000078" =>
                -----------------------------------------------------------------
                -- Standard Route Default Port CSR.
                -----------------------------------------------------------------

                if (configWe = '1') then
                  -- Write the default route device id.
                  routeTablePortDefault <= configDataWrite(7 downto 0);
                end if;
                
                configDataReadInternal(31 downto 8) <= (others => '0');
                configDataReadInternal(7 downto 0) <= routeTablePortDefault;
                
              when x"000100" =>
                -----------------------------------------------------------------
                -- Extended features. LP-Serial Register Block Header.
                -----------------------------------------------------------------

                -- One feature only, 0x0003=Generic End Point Free Device.
                configDataReadInternal(31 downto 16) <= x"0200";  --next EF_PTR is Error/HotSwap
                configDataReadInternal(15 downto 0) <= x"0003";
                
              when x"000120" =>
                -----------------------------------------------------------------
                -- Port Link Timeout Control CSR.
                -----------------------------------------------------------------

                if (configWe = '1') then
                  portLinkTimeout <= configDataWrite(31 downto 8);
                end if;
                
                configDataReadInternal(31 downto 8) <= portLinkTimeout;
                configDataReadInternal(7 downto 0) <= x"00";
                
              when x"00013c" =>
                -----------------------------------------------------------------
                -- Port General Control CSR.
                -----------------------------------------------------------------

                if (configWe = '1') then
                  discovered <= configDataWrite(29);
                end if;
                
                configDataReadInternal(31 downto 30) <= "00";
                configDataReadInternal(29) <= discovered;
                configDataReadInternal(28 downto 0) <= (others => '0');

                
                
              --------------------------------------------------------------------
              -- Extended features. (Error/)HotSwap Extensions Register Block
              --------------------------------------------------------------------
              when x"000200" => --0x00
                 -----------------------------------------------------------------
                 -- Error Managment/HotSwap Extensions Block Header.
                 -----------------------------------------------------------------
                 configDataReadInternal(31 downto 16) <= x"0000";  --Next EF_PTR is N/A
                 configDataReadInternal(15 downto  0) <= x"0017";  --EF_ID=0x0017; only HotSwap part of error management is supported (0x0007=Error management, with or without hot swap support)
                 --
              when x"000204" => --0x04
                 -----------------------------------------------------------------
                 -- Error Managment/HotSwap Extensions Block CAR.
                 -----------------------------------------------------------------
                 configDataReadInternal(31) <= ErrorManagementNotFullySupported; --'1'=all registers/bit fields of Error management extensions may not be supported. 0=all supported.
                 configDataReadInternal(30) <= HotSwapFullySupported;            --'1'=all registers/bit fields specific to HotSwap support shall be supported. 0=not all fields are supported.
                 configDataReadInternal(29) <= PHlayerErrorCaptureFifoSupport;   --Physical Layer Error Capture FIFO: 0=may not be supported, 1=full support            ****check this bit!!****
                 configDataReadInternal(28) <= LTlayerErrorCaptureFifoSupport;   --Logical/Transport Layer Error Capture FIFO: 0=may not be supported, 1=full support   ****check this bit!!****
                 configDataReadInternal(27 downto  0) <= (others=>'0');
                 --
              when x"000228" => --0x28
                 -----------------------------------------------------------------
                 -- Port Write Target deviceID CSR
                 -----------------------------------------------------------------
                 if configWe = '1' then
                    Dev16_deviceID_msb <= configDataWrite(31 downto 24);
                    Dev8_deviceID      <= configDataWrite(23 downto 16);
                  --Dev8_or_16         <= configDataWrite(15);
                  --Dev32_PW           <= configDataWrite(14);
                 end if;
                 
                 configDataReadInternal(31 downto 24) <= Dev16_deviceID_msb;  --address to use when a device generates a Maintenance Port Write to rweport errors to system host.
                 configDataReadInternal(23 downto 16) <= Dev8_deviceID;       --
                 configDataReadInternal(15) <= '1';   --only support for 16 bit 
               --configDataReadInternal(15)           <= Dev8_or_16;          --when dev32_PW=0:  '0'=dev8, '1'=dev16 id-size
               --configDataReadInternal(14)           <= Dev32_PW;            --'0'=devID is controlled by Dev8_or_16 field, '1'=dev32 deviceID
                 configDataReadInternal(14 downto  0) <= (others=>'0');       --Reserved bits.
                 --
           -- when x"00022C" => --0x2C
           --    -----------------------------------------------------------------
           --    -- Packet Time-to-live CSR
           --    -----------------------------------------------------------------
           --    if configWe = '1' then
           --       PacketTimeToLiveValue <= configDataWrite(31 downto 16);
           --    end if;
           --    
           --    configDataReadInternal(31 downto 16) <= PacketTimeToLiveValue; --Maximum time a packet is allowed to exist within a switch device. 0xFFFF=100ms+-34%, 0=timeout is disabled.
           --    configDataReadInternal(15 downto  0) <= (others=>'0');
           --    --
           -- when x"000230" => --0x30
           --    -----------------------------------------------------------------
           --    -- Port-write DEV32 Target deviceID CSR
           --    -----------------------------------------------------------------
           --    if configWe = '1' then
           --       Dev32_deviceID <= configDataWrite;
           --    end if;
           --    
           --    configDataReadInternal <= Dev32_deviceID;                   --ID to use when Dev32_PW bit is set.
           --    --
              when x"000234" => --0x34
                 -----------------------------------------------------------------
                 -- Port Write Transmission Control CSR
                 -----------------------------------------------------------------
                 if configWe = '1' then
                    PortWriteTransmissionDisable <= configDataWrite(0);
                 end if;
                 
                 configDataReadInternal(31 downto 1) <= (others=>'0');       --Reserved bits.
                 configDataReadInternal(0) <= PortWriteTransmissionDisable;  --'1' enabled events shall not cause new port writes to be generated. '0'=port writes shall be generated.
                 --
       ---------------------------------------------------------------------------
       --        -- Not yet implemented: Error management
       --     when x"000208" => -- 0x08  Logical/Transport Layer Error Detect CSR
       --                       configDataReadInternal <= LogicalTransportLayerErrorDetectCSR;
       --     when x"00020C" |  -- 0x0C  Logical/Transport Layer Error Enable CSR
       --          x"000210" |  -- 0x10  Logical/Transport Layer High Address Capture SCR
       --          x"000214" |  -- 0x14  Logical/Transport Layer Address Capture SCR
       --          x"000218" |  -- 0x18  Logical/Transport Layer Device ID Capture CSR
       --          x"00021C" |  -- 0x1C  Logical/Transport Layer Control Capture CSR
       --          x"000220" |  -- 0x20  Logical/Transport Layer Dev32 Destination ID Capture CSR
       --          x"000224" => -- 0x24  Logical/Transport Layer Dev32 Source ID Capture CSR
       --                       configDataReadInternal <= (others=>'0');
                 -----------------------------------------------------------------

              when others => -- Registers based on Number of SWITCH_PORTS:
                -----------------------------------------------------------------
                -- Other port specific registers.
                -----------------------------------------------------------------
                
                -- Make sure the output is always set to something.
                configDataReadInternal <= (others=>'0');  --Notice! may be replaced by other value further down..

                -- Iterate through all active ports.
                for portIndex in 0 to SWITCH_PORTS-1 loop

                  -----------------------------------------------------
                  --Error Managment/HotSwap **Port related** Registers:
                  if unsigned(configAdr) = (x"000240" + (x"000040"*portIndex)) then
                     -----------------------------------------------------------------
                     -- Port N Error Detect CSR.
                     -----------------------------------------------------------------
                     if configWe = '1' then
                        stbPortNerrorDetect_CSR(portIndex)     <= '1';
                        wrtPortNerrorDetect_CSR(portIndex)(31) <= '0';                   
                        wrtPortNerrorDetect_CSR(portIndex)(30) <= configDataWrite(30);    -- linkOKtoUninitTransition
                        wrtPortNerrorDetect_CSR(portIndex)(29) <= configDataWrite(29);    -- linkUninitPacketDiscardActive
                        wrtPortNerrorDetect_CSR(portIndex)(28) <= configDataWrite(28);    -- linkUninitToOkTransition
                        wrtPortNerrorDetect_CSR(portIndex)(27 downto 0) <= (others=>'0');
                     end if;
                     --
                     configDataReadInternal <= PortNerrorDetect_CSR(portIndex);
                     --
                  elsif unsigned(configAdr) = (x"000244" + (x"000040"*portIndex)) then
                     -----------------------------------------------------------------
                     -- Port N Error Rate Enable CSR.
                     -----------------------------------------------------------------
                     if configWe = '1' then
                        LinkOKtoUninitTransitionEnable(portIndex)      <= configDataWrite(30);     
                        LinkUninitPacketDiscardActiveEnable(portIndex) <= configDataWrite(29);
                        LinkUninitToOkTransitionEnable(portIndex)      <= configDataWrite(28);     
                     end if;
                     
                     configDataReadInternal(31) <= '0';                   
                     configDataReadInternal(30) <= LinkOKtoUninitTransitionEnable(portIndex);      -- Enable event notification for when the link has transitioned from a link initialized to link uninitialized state.
                     configDataReadInternal(29) <= LinkUninitPacketDiscardActiveEnable(portIndex); -- Enable event notification for Link Uninit Packet Discard Timer events.
                     configDataReadInternal(28) <= LinkUninitToOkTransitionEnable(portIndex);      -- Enable event notification for when the link has transitioned from a link uninitialized to link initialized state.
                     configDataReadInternal(27 downto 0) <= (others=>'0');
                     --
                  elsif unsigned(configAdr) = (x"000270" + (x"000040"*portIndex)) then
                     -----------------------------------------------------------------
                     -- Port N Link Uninit Discard Timer CSR.
                     -----------------------------------------------------------------
                     if configWe = '1' then
                        LinkUninitTimeout(portIndex) <= configDataWrite(31 downto 8);
                     end if;
                     
                     configDataReadInternal(31 downto 8) <= LinkUninitTimeout(portIndex);   --0xFFFFFF shall correspond to 6-12 s, if 0 the discard timer shall be disabled.
                     configDataReadInternal( 7 downto 0) <= (others=>'0');
                     --
         --       elsif unsigned(configAdr) = (x"00027C" + (x"000040"*portIndex)) then
         --          -----------------------------------------------------------------
         --          -- Port N FIFO Error Detect CSR. (Not required for HotSwap according Part 8:Table 2-4, but contain hotSwap bits if bit 2 of Error Management/HotSwap Extension block CAR is set!
         --          -----------------------------------------------------------------
         --          configDataReadInternal(31) <= '0';
         --          configDataReadInternal(30) <= PortNerrorDetect_CSR(portIndex)(30); --LinkOKtoUninitTransition(portIndex); --fields are identical to (x"000240" + (x"000040"*portIndex))  ????
         --          configDataReadInternal(29) <= PortNerrorDetect_CSR(portIndex)(29); --LinkUninitPacketDiscardActive(portIndex);
         --          configDataReadInternal(28) <= PortNerrorDetect_CSR(portIndex)(28); --LinkUninitToOkTransition(portIndex);
         --          configDataReadInternal(27 downto 0) <= (others=>'0');
         --          --
         ---------------------------------------------------------------------------
         --          -- Not yet implemented: Error management
         --       elsif (unsigned(configAdr) = (x"000248" + (x"000040"*portIndex))) or  -- Port n Attributes Capture CSR
         --             (unsigned(configAdr) = (x"00024C" + (x"000040"*portIndex))) or  -- Port n Capture 0 CSR
         --             (unsigned(configAdr) = (x"000250" + (x"000040"*portIndex))) or  -- Port n Capture 1 CSR
         --             (unsigned(configAdr) = (x"000254" + (x"000040"*portIndex))) or  -- Port n Capture 2 CSR
         --             (unsigned(configAdr) = (x"000258" + (x"000040"*portIndex))) or  -- Port n Capture 3 CSR
         --             (unsigned(configAdr) = (x"00025C" + (x"000040"*portIndex))) or  -- Port n Capture 4 CSR
         --             (unsigned(configAdr) = (x"000260" + (x"000040"*portIndex))) or  -- Reserved
         --             (unsigned(configAdr) = (x"000264" + (x"000040"*portIndex))) or  -- Reserved
         --             (unsigned(configAdr) = (x"000268" + (x"000040"*portIndex))) or  -- Port n Error Rate CSR
         --             (unsigned(configAdr) = (x"00026C" + (x"000040"*portIndex))) or  -- Port n Error Rate Threshold CSR
         --             (unsigned(configAdr) = (x"000274" + (x"000040"*portIndex))) or  -- Reserved
         --             (unsigned(configAdr) = (x"000278" + (x"000040"*portIndex))) or  -- Reserved
         --           --(unsigned(configAdr) = (x"00027C" + (x"000040"*portIndex)))     -- Port n Error Detect FIFO CSR
         --       then
         --          configDataReadInternal <= (others=>'0');
         --          --
                    
                  -----------------------------------------------------
                  --LP-Serial Register Block:
                  elsif(unsigned(configAdr) = (x"000148" + (x"000020"*portIndex))) then
                    -----------------------------------------------------------------
                    -- Port N Local ackID CSR.
                    -----------------------------------------------------------------
                    if (configWe = '1') then
                      localAckIdWrite_o(portIndex) <= '1';
                      clrOutstandingAckId_o(portIndex) <= configDataWrite(31);
                      inboundAckId_o(portIndex) <= configDataWrite(28 downto 24);
                      outstandingAckId_o(portIndex) <= configDataWrite(12 downto 8);
                      outboundAckId_o(portIndex) <= configDataWrite(4 downto 0);
                    end if;
                    configDataReadInternal(31 downto 29) <= (others => '0');
                    configDataReadInternal(28 downto 24) <= inboundAckId_i(portIndex);
                    configDataReadInternal(23 downto 13) <= (others => '0');
                    configDataReadInternal(12 downto 8) <= outstandingAckId_i(portIndex);
                    configDataReadInternal(7 downto 5) <= (others => '0');
                    configDataReadInternal(4 downto 0) <= outboundAckId_i(portIndex);
                    
                  elsif(unsigned(configAdr) = (x"000154" + (x"000020"*portIndex))) then
                    -----------------------------------------------------------------
                    -- Port N Control 2 CSR.
                    -----------------------------------------------------------------
                    configDataReadInternal <= (others => '0');
                    
                  elsif(unsigned(configAdr) = (x"000158" + (x"000020"*portIndex))) then
                    -----------------------------------------------------------------
                    -- Port N Error and Status CSR.
                    -----------------------------------------------------------------
                    if (configWe = '1') then
                       --
                       --this bit is dont care!        <= configDataWrite(4);  --std:bit 27
                       stbPortNwritePending(portIndex) <= '1';
                       --
                       portNwriteDisabled(portIndex)   <= configDataWrite(5);  --std:bit 26
                    end if;
                    
                    -- Idle Sequence 2 Support.
                    configDataReadInternal(31) <= '0';
                    
                    -- Idle Sequence 2 Enable.
                    configDataReadInternal(30) <= '0';
                    
                    -- Idle Sequence.
                    configDataReadInternal(29) <= '0';
                    
                    -- Reserved.
                    configDataReadInternal(28) <= '0';
                    
                    -- Flow Control Mode.
                    configDataReadInternal(27) <= '0';
                    
                    -- Reserved.
                    configDataReadInternal(26 downto 21) <= (others => '0');
                    
                    -- Output retry-encountered.
                    configDataReadInternal(20) <= '0';
                    
                    -- Output retried.
                    configDataReadInternal(19) <= '0';
                    
                    -- Output retried-stopped.
                    configDataReadInternal(18) <= '0';
                    
                    -- Output error-encountered.
                    configDataReadInternal(17) <= '0';
                    
                    -- Output error-stopped.
                    configDataReadInternal(16) <= '0';
                    
                    -- Reserved.
                    configDataReadInternal(15 downto 11) <= (others => '0');
                    
                    -- Input retry-stopped.
                    configDataReadInternal(10) <= '0';
                    
                    -- Input error-encountered.
                    configDataReadInternal(9) <= '0';
                    
                    -- Input error-stopped.
                    configDataReadInternal(8) <= '0'; 

                    -- Reserved.  (Part 8: Error management  add functions to these three bits.)
                    configDataReadInternal(7 downto 6) <= (others => '0');  

                    -- Port-write Disabled.
                    configDataReadInternal(5) <= portNwriteDisabled(portIndex);
                    
                    -- Port-write pending.
                    configDataReadInternal(4) <= PortNwritePending(portIndex);
                    
                    -- Port unavailable.
                    configDataReadInternal(3) <= '0';
                    
                    -- Port error.
                    configDataReadInternal(2) <= '0';
                    
                    -- Port OK.
                    configDataReadInternal(1) <= linkInitialized_i(portIndex);
                    
                    -- Port uninitialized.
                    configDataReadInternal(0) <= not linkInitialized_i(portIndex);
                    
                  elsif(unsigned(configAdr) = (x"00015c" + (x"000020"*portIndex))) then
                    -----------------------------------------------------------------
                    -- Port N Control CSR.
                    -----------------------------------------------------------------
                    
                    -- Port Width Support.
                    configDataReadInternal(31 downto 30) <= (others=>'0');

                    -- Initialized Port Width.
                    configDataReadInternal(29 downto 27) <= (others=>'0');

                    -- Port Width Override.
                    configDataReadInternal(26 downto 24) <= (others=>'0');

                    -- Port disable.
                    configDataReadInternal(23) <= '0';
                    
                    -- Output Port Enable.
                    if (configWe = '1') then
                      outputPortEnable(portIndex) <= configDataWrite(22);
                    end if;
                    configDataReadInternal(22) <= outputPortEnable(portIndex);
                    
                    -- Input Port Enable.
                    if (configWe = '1') then
                      inputPortEnable(portIndex) <= configDataWrite(21);
                    end if;
                    configDataReadInternal(21) <= inputPortEnable(portIndex);

                    -- Error Checking Disabled.
                    configDataReadInternal(20) <= '0';
                    
                    -- Multicast-event Participant.
                    configDataReadInternal(19) <= '0';
                    
                    -- Reserved.
                    configDataReadInternal(18) <= '0';
                    
                    -- Enumeration Boundry.
                    configDataReadInternal(17) <= '0';

                    -- Reserved.
                    configDataReadInternal(16) <= '0';

                    -- Extended Port Width Override.
                    configDataReadInternal(15 downto 14) <= (others=>'0');

                    -- Extended Port Width Support.
                    configDataReadInternal(13 downto 12) <= (others=>'0');
                    
                    -- Implementation defined.
                    configDataReadInternal(11 downto 4) <= (others=>'0');

                    -- Reserved.
                    configDataReadInternal(3 downto 1) <= (others=>'0');

                    -- Port Type.
                    configDataReadInternal(0) <= '1';
                  end if;
                end loop;

            end case;
          end if;
        else
          -- Config memory not enabled.
        end if;
      else
        configAckInternal <= '0';
      end if;
    end if;
  end process;
  
  -----------------------------------------------------------------------------
  -- This process manages the Port n Error Detect CSR register, set and write..
  pNerrDetCSR:process(clk, areset_n)
  begin
     if areset_n = '0' then
        PortNerrorDetect_CSR <= (others=>(others=>'0'));
     elsif rising_edge(clk) then
        for portIndex in 0 to SWITCH_PORTS-1 loop
           PortNerrorDetect_CSR(portIndex)(31) <= '0';
           --
           if linkOKtoUninitTransition(portIndex)='1' then
              --A new event!
              PortNerrorDetect_CSR(portIndex)(30) <= '1';
           elsif stbPortNerrorDetect_CSR(portIndex)='1' then
              --port write (probably a clearing write)
              PortNerrorDetect_CSR(portIndex)(30) <= wrtPortNerrorDetect_CSR(portIndex)(30);
           end if;
           --
           if linkUninitPacketDiscardActive(portIndex)='1' then
              --A new event!
              PortNerrorDetect_CSR(portIndex)(29) <= '1';
           elsif stbPortNerrorDetect_CSR(portIndex)='1' then
              --port write (probably a clearing write)
              PortNerrorDetect_CSR(portIndex)(29) <= wrtPortNerrorDetect_CSR(portIndex)(29);
           end if;
           --
           if linkUninitToOkTransition(portIndex)='1' then
              --A new event!
              PortNerrorDetect_CSR(portIndex)(28) <= '1';
           elsif stbPortNerrorDetect_CSR(portIndex)='1' then
              --port write (probably a clearing write)
              PortNerrorDetect_CSR(portIndex)(28) <= wrtPortNerrorDetect_CSR(portIndex)(28);
           end if;
           --
           PortNerrorDetect_CSR(portIndex)(27 downto 0) <= (others=>'0');
        end loop;
     end if;
  end process;
  
  --used for Port-Write Packet for Error Reporting
  PortNerrorDetect<=PortNerrorDetect_CSR(PortNindex) when PortNindex<SWITCH_PORTS else (others=>'0');
        
  -----------------------------------------------------------------------------
  -- This process manages the Port-Write Pending Bits, part of the Port n Error and Status CSR register.
  pwPending:process(clk, areset_n)
  begin
     if areset_n = '0' then
        PortNwritePending <= (others=>'0');
     elsif rising_edge(clk) then
        for portIndex in 0 to SWITCH_PORTS-1 loop
           if setPortNwritePending(portIndex)='1' then
              --Make the event sticky.
              PortNwritePending(portIndex) <= '1';
           elsif stbPortNwritePending(portIndex)='1' then
              --port write (a clearing write, since writing '1' shall clear according the standard..)
              PortNwritePending(portIndex) <= '0'; --clear the bit regardless of logic level written!
           end if;
        end loop;
     end if;
  end process;
  
  -----------------------------------------------------------------------------
  -- Logic implementing the routing table access.
  -----------------------------------------------------------------------------

  -- Lookup interface port memory signals.
  lookupEnable <= '1' when (lookupStb_i = '1') and (lookupAddr_i(15 downto 11) = "00000") else '0';
  lookupAddress <= lookupAddr_i(10 downto 0);
  lookupData_o <= lookupData when (lookupEnable = '1') else routeTablePortDefault;
  lookupAck_o <= lookupAck;
  LookupProcess: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      lookupAck <= '0';
    elsif rising_edge(clk) then
      if ((lookupStb_i = '1') and (lookupAck = '0')) then
        lookupAck <= '1';
      else
        lookupAck <= '0';
      end if;
    end if;
  end process;

  -- Dual port memory containing the routing table.
  RoutingTable: MemoryDualPort
    generic map(
      ADDRESS_WIDTH=>11, DATA_WIDTH=>8)
    port map(
      clkA_i=>clk, enableA_i=>routeTableEnable, writeEnableA_i=>routeTableWrite,
      addressA_i=>routeTableAddress,
      dataA_i=>routeTablePortWrite, dataA_o=>routeTablePortRead,
      clkB_i=>clk, enableB_i=>lookupEnable,
      addressB_i=>lookupAddress, dataB_o=>lookupData);
  
      
   -----------------------------------------------------------------------------
   -- HotSwap Timers, etc. 
   -----------------------------------------------------------------------------
   Tick_1_5us: Ticker      -- Select generic so that for the used clk rate, a tick rate of below range is satisfied.             
     generic map(clksPerTick => 38)    -- Ideal: 1.52590219 us/tick --> 655350 Hz (+-34%), @25MHz: 38 clk's/tick = 1.52us=657894.7Hz (-0.387%)     
        port map(clk_i => clk,      
              reset_ni => areset_n,
                tick_o => PacketTimeToLiveTick);
   
   -- LinkUninitTimeoutTicker: max value for timer counter is 16777215, max time allowed somwhere in the range 6-12 s
   -- 6 s to 12 s divided by 16777215 = 357.62789 ns to 715.25578 ns per tick --> 2.7962025MHz to 1.39810125MHz tick rate
   --
   --    Assuming clk of 25MHz:
   --      clksPerTick   Tick rate   max LinkUninitTimeout unsing Tick+Timer
   --              9       360 ns      6.0398 s
   --             10       400 ns      6.7109 s
   --             11       440 ns      7.3820 s      
   --             12       480 ns      8.0531 s
   --             13       520 ns      8.7242 s
   --             14       560 ns      9.3952 s  
   --             15       600 ns     10.0663 s   <-- This one is used for LinkUninitTimeoutTicker
   --             16       640 ns     10.7374 s   
   --             17       680 ns     11.4085 s   
   --             18       720 ns     12.0796 s
   --             19       760 ns     12.7507 s
   --             20       800 ns     13.4218 s
   --             21       840 ns     14.0929 s
   --             22       880 ns     14.7639 s
   --             23       920 ns     15.4350 s
   --             24       960 ns     16.1061 s
   --             25      1000 ns     16.7772 s
   --
   LinkUninitTimeoutTicker: Ticker     
     generic map(clksPerTick => 15)    --Select generic so that for the used clk rate, a tick rate of below range is satisfied.
        port map(clk_i => clk,
              reset_ni => areset_n,
                tick_o => LinkUninitTimerTick);
   
   HotSwapPortStat:for pI in 0 to SWITCH_PORTS-1 
   generate
      --
      HswpPortStat: RioHotSwapPortStatus
         port map(                  clk => clk,
                               reset_ni => areset_n,
                  LinkUninitTimerTick_i => LinkUninitTimerTick,
        -----------------------------------Port related signals
                      linkInitialized_i => linkInitialized_i(pI),
       LinkOKtoUninitTransitionEnable_i => LinkOKtoUninitTransitionEnable(pI),
       LinkUninitToOkTransitionEnable_i => LinkUninitToOkTransitionEnable(pI),
  LinkUninitPacketDiscardActiveEnable_i => LinkUninitPacketDiscardActiveEnable(pI),   
                    LinkUninitTimeout_i => LinkUninitTimeout(pI),
             linkOKtoUninitTransition_o => linkOKtoUninitTransition(pI),
             linkUninitToOkTransition_o => linkUninitToOkTransition(pI),
        linkUninitPacketDiscardActive_o => linkUninitPacketDiscardActive(pI),
                  sendHotSwapEventNow_o => hotSwapEvent(pI));
      --          
   -- PacketTimeToLiveTimer: Timer
   --   generic map(timerWidth=>PacketTimeToLiveValue'length) --16 bits
   --      port map(clk_i => clk,
   --            reset_ni => areset_n,
   --        resetValue_i => PacketTimeToLiveValue,           --if x"0000" the timer is disabled
   --         timerTick_i => PacketTimeToLiveTick,            --tick rate strobe input for this timer: x"FFFF" = 100ms --> 1.52590219 us/tick --> 655350 Hz   +- 34%
   --                wd_i => packetReadyForTransmission(pI),  --watch dog input "from kicker" -ie. toggle this high whenever a packet is created
   --           expired_o => discardPacket(pI));
      --       
   end generate;
  
end architecture;

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 
use work.rio_common.all;

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity RouteTableInterconnect is
  generic(
    WIDTH : natural range 1 to 256 := 8);
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    stb_i : in Array1(WIDTH-1 downto 0);
    addr_i : in Array16(WIDTH-1 downto 0);
    dataM_o : out Array8(WIDTH-1 downto 0);
    ack_o : out Array1(WIDTH-1 downto 0);

    stb_o : out std_logic;
    addr_o : out std_logic_vector(15 downto 0);
    dataS_i : in std_logic_vector(7 downto 0);
    ack_i : in std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RouteTableInterconnectImpl of RouteTableInterconnect is
  signal activeCycle : std_logic;
  signal selectedMaster : natural range 0 to WIDTH-1;
begin
  
  -----------------------------------------------------------------------------
  -- Arbitration.
  -----------------------------------------------------------------------------
  Arbiter: process(areset_n, clk)
  begin
    if (areset_n = '0') then
      activeCycle <= '0';
      selectedMaster <= 0;
    elsif rising_edge(clk) then
      if (activeCycle = '0') then
        for i in 0 to WIDTH-1 loop
          if (stb_i(i) = '1') then
            activeCycle <= '1';
            selectedMaster <= i;
          end if;
        end loop;
      else  
        if (stb_i(selectedMaster) = '0') then
          activeCycle <= '0';
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Interconnection.
  -----------------------------------------------------------------------------
  stb_o <= stb_i(selectedMaster);
  addr_o <= addr_i(selectedMaster);

  Interconnect: for i in 0 to WIDTH-1 generate
    dataM_o(i) <= dataS_i;
    ack_o(i) <= ack_i when (selectedMaster = i) else '0';
  end generate;

end architecture;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 
use work.rio_common.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity SwitchPortInterconnect is
  generic(
    WIDTH : natural range 1 to 256 := 8);
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    masterCyc_i : in Array1(WIDTH-1 downto 0);
    masterStb_i : in Array1(WIDTH-1 downto 0);
    masterWe_i : in Array1(WIDTH-1 downto 0);
    masterAddr_i : in Array10(WIDTH-1 downto 0);
    masterData_i : in Array32(WIDTH-1 downto 0);
    masterData_o : out Array1(WIDTH-1 downto 0);
    masterAck_o : out Array1(WIDTH-1 downto 0);

    slaveCyc_o : out Array1(WIDTH-1 downto 0);
    slaveStb_o : out Array1(WIDTH-1 downto 0);
    slaveWe_o : out Array1(WIDTH-1 downto 0);
    slaveAddr_o : out Array10(WIDTH-1 downto 0);
    slaveData_o : out Array32(WIDTH-1 downto 0);
    slaveData_i : in Array1(WIDTH-1 downto 0);
    slaveAck_i : in Array1(WIDTH-1 downto 0));
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture SwitchPortInterconnectImpl of SwitchPortInterconnect is
  --component ChipscopeIcon1 is
  --  port (
  --    CONTROL0 : inout STD_LOGIC_VECTOR ( 35 downto 0 )
  --    );
  --end component;
  --component ChipscopeIlaWb is
  --  port (
  --    CLK : in STD_LOGIC := 'X';
  --    TRIG0 : in STD_LOGIC_VECTOR ( 46 downto 0);
  --    CONTROL : inout STD_LOGIC_VECTOR ( 35 downto 0 ) 
  --    );
  --end component;
  --signal control : std_logic_vector(35 downto 0);
  --signal trig : std_logic_vector(46 downto 0);
  
  signal activeCycle : std_logic;
  signal selectedMaster : natural range 0 to WIDTH-1;
  signal selectedSlave : natural range 0 to WIDTH-1;
 
begin

  -----------------------------------------------------------------------------
  -- Arbitration process.
  -----------------------------------------------------------------------------
  
  RoundRobinArbiter: process(areset_n, clk)
    variable index : natural range 0 to WIDTH-1;
  begin
    if (areset_n = '0') then
      activeCycle <= '0';
      selectedMaster <= 0;
    elsif rising_edge(clk) then
      -- Check if a cycle is ongoing.
      if (activeCycle = '0') then
        -- No ongoing cycles.
        
        -- Iterate through all ports and check if any new cycle has started.
        for i in 0 to WIDTH-1 loop
          if ((selectedMaster+i) >= WIDTH) then
            index := (selectedMaster+i) - WIDTH;
          else
            index := (selectedMaster+i);
          end if;
          
          if (masterCyc_i(index) = '1') then
            activeCycle <= '1';
            selectedMaster <= index;
          end if;
        end loop;
      else
        -- Ongoing cycle.
        
        -- Check if the cycle has ended.
        if (masterCyc_i(selectedMaster) = '0') then
          -- Cycle has ended.
          activeCycle <= '0';

          -- Check if a new cycle has started from another master.
          -- Start to check from the one that ended its cycle, this way, the
          -- ports will be scheduled like round-robin.
          for i in 0 to WIDTH-1 loop
            if ((selectedMaster+i) >= WIDTH) then
              index := (selectedMaster+i) - WIDTH;
            else
              index := (selectedMaster+i);
            end if;
              
            if (masterCyc_i(index) = '1') then
              activeCycle <= '1';
              selectedMaster <= index;
            end if;
          end loop;
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Address decoding.
  -----------------------------------------------------------------------------

  -- Select the last port when the top bit is set.
  -- The last port must be the maintenance slave port.
  selectedSlave <= WIDTH-1 when masterAddr_i(selectedMaster)(9) = '1' else
                   to_integer(unsigned(masterAddr_i(selectedMaster)(8 downto 1)));
  
  -----------------------------------------------------------------------------
  -- Interconnection matrix.
  -----------------------------------------------------------------------------
  Interconnect: for i in 0 to WIDTH-1 generate
    slaveCyc_o(i) <= masterCyc_i(selectedMaster) when (selectedSlave = i) else '0';
    slaveStb_o(i) <= masterStb_i(selectedMaster) when (selectedSlave = i) else '0';
    slaveWe_o(i) <= masterWe_i(selectedMaster);
    slaveAddr_o(i) <= masterAddr_i(selectedMaster);
    slaveData_o(i) <= masterData_i(selectedMaster);
    masterData_o(i) <= slaveData_i(selectedSlave);
    masterAck_o(i) <= slaveAck_i(selectedSlave) when (selectedMaster = i) else '0';
  end generate;

  -----------------------------------------------------------------------------
  -- Chipscope debugging probe.
  -----------------------------------------------------------------------------
  --trig <= masterCyc_i(selectedMaster) & masterStb_i(selectedMaster) &
  --        masterWe_i(selectedMaster) &  masterAddr_i(selectedMaster) &
  --        masterData_i(selectedMaster) & slaveData_i(selectedSlave) &
  --        slaveAck_i(selectedSlave);
  --ChipscopeIconInst: ChipscopeIcon1
  --  port map(CONTROL0=>control);
  --ChipscopeIlaInst: ChipscopeIlaWb
  --  port map(CLK=>clk, TRIG0=>trig, CONTROL=>control);
  
end architecture;



