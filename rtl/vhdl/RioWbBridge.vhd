-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Containing a bridge between a RapidIO network and a Wishbone bus. Packets
-- NWRITE, NWRITER and NREAD are currently supported.
-- 
-- To Do:
-- - Move packet handlers to RioLogicalPackets.
-- - Move component declarations to riocommon.
-- - Update the Maintenance handler to the new interface. It currently does not
--   compile.
-- - Set the stb_o to '0' in between read accesses to conform better to a
--   block transfer in the Wishbone standard.
-- - Clean up cyc-signals, only stb-signals are needed (between
--   RioLogicalCommon and the packet handlers).
-- - Add support for the lock_o to be sure to transfer all the packet
--   content atomically?
-- - Add support for EXTENDED_ADDRESS.
-- - Add support for addressing to implementation defined config space by
--   adding interface to top entity.
-- - Use the baseDeviceId when sending packets? Currently, all responses
--   are sent with destination<->source exchanged so the baseDeviceId is not
--   needed.
-- - Support inbound data with full bandwidth, not just half, applies to
--   RioLogicalCommon and the packet handlers.
-- - Move the packet handlers to seperate files.
-- - Increase the priority of the response-packet when sent?
-- - Implement error indications if erronous packets are received.
-- - Implement error indications if err_i is received on the Wishbone bus.
-- - Add support for extended features to dynamically configure the status
--   from the port this block is connected to. Needed for the discovered- and
--   masterEnable-bits.
-- - Add support for outbound doorbells connected to interrupt input pins.
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
-- RioWbBridge.
-- This block acts as an RapidIO endpoint and converts packets into Wishbone
-- accesses.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;

-------------------------------------------------------------------------------
-- Entity for RioWbBridge.
-------------------------------------------------------------------------------
entity RioWbBridge is
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
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioWbBridge.
-------------------------------------------------------------------------------
architecture RioWbBridgeImpl of RioWbBridge is

  component RequestClassInbound is
    generic(
      EXTENDED_ADDRESS : natural range 0 to 2 := 0);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      ready_o : out std_logic;
      vc_o : out std_logic;
      crf_o : out std_logic;
      prio_o : out std_logic_vector(1 downto 0);
      tt_o : out std_logic_vector(1 downto 0);
      dstId_o : out std_logic_vector(31 downto 0);
      srcId_o : out std_logic_vector(31 downto 0);
      tid_o : out std_logic_vector(7 downto 0);
      address_o : out std_logic_vector(16*EXTENDED_ADDRESS+30 downto 0);
      length_o : out std_logic_vector(4 downto 0);
      select_o : out std_logic_vector(7 downto 0);
      done_i : in std_logic;
      
      inboundCyc_i : in std_logic;
      inboundStb_i : in std_logic;
      inboundAdr_i : in std_logic_vector(7 downto 0);
      inboundDat_i : in std_logic_vector(31 downto 0);
      inboundAck_o : out std_logic);
  end component;
  
  component WriteClassInbound is
    generic(
      EXTENDED_ADDRESS : natural range 0 to 2 := 0);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      ready_o : out std_logic;
      responseNeeded_o : out std_logic;
      vc_o : out std_logic;
      crf_o : out std_logic;
      prio_o : out std_logic_vector(1 downto 0);
      tt_o : out std_logic_vector(1 downto 0);
      dstId_o : out std_logic_vector(31 downto 0);
      srcId_o : out std_logic_vector(31 downto 0);
      tid_o : out std_logic_vector(7 downto 0);
      address_o : out std_logic_vector(16*EXTENDED_ADDRESS+30 downto 0);
      length_o : out std_logic_vector(4 downto 0);
      select_o : out std_logic_vector(7 downto 0);
      payloadRead_i : in std_logic;
      payloadIndex_i : in std_logic_vector(4 downto 0);
      payload_o : out std_logic_vector(63 downto 0);
      done_i : in std_logic;
      
      inboundCyc_i : in std_logic;
      inboundStb_i : in std_logic;
      inboundAdr_i : in std_logic_vector(7 downto 0);
      inboundDat_i : in std_logic_vector(31 downto 0);
      inboundAck_o : out std_logic);
  end component;

  component ResponseClassOutbound is
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      ready_i : in std_logic;
      vc_i : in std_logic;
      crf_i : in std_logic;
      prio_i : in std_logic_vector(1 downto 0);
      tt_i : in std_logic_vector(1 downto 0);
      dstid_i : in std_logic_vector(31 downto 0);
      srcid_i : in std_logic_vector(31 downto 0);
      tid_i : in std_logic_vector(7 downto 0);
      error_i : in std_logic;
      payloadPresent_i :  in std_logic;
      payloadLength_i : in std_logic_vector(4 downto 0);
      payloadWrite_i : in std_logic;
      payloadIndex_i : in std_logic_vector(4 downto 0);
      payload_i : in std_logic_vector(63 downto 0);
      done_o : out std_logic;
      
      outboundCyc_o : out std_logic;
      outboundStb_o : out std_logic;
      outboundDat_o : out std_logic_vector(31 downto 0);
      outboundAck_i : in std_logic);
  end component;

  constant PORTS : natural := 2;

  type StateType is (IDLE,
                     REQUEST_CLASS, REQUEST_CLASS_RESPONSE,
                     WRITE_CLASS, WRITE_CLASS_ACCESS, WRITE_CLASS_ACK, WRITE_CLASS_RESPONSE);
  signal state : StateType;
  
  signal adr : std_logic_vector(16*EXTENDED_ADDRESS+30 downto 0);
  signal datOut : std_logic_vector(63 downto 0);

  signal payloadUpdate : std_logic;
  signal payloadIndex : std_logic_vector(4 downto 0);
  
  signal requestReady : std_logic;
  signal requestVc : std_logic;
  signal requestCrf : std_logic;
  signal requestPrio : std_logic_vector(1 downto 0);
  signal requestTt : std_logic_vector(1 downto 0);
  signal requestDstId : std_logic_vector(31 downto 0);
  signal requestSrcId : std_logic_vector(31 downto 0);
  signal requestTid : std_logic_vector(7 downto 0);
  signal requestAddress : std_logic_vector(16*EXTENDED_ADDRESS+30 downto 0);
  signal requestLength : std_logic_vector(4 downto 0);
  signal requestSelect : std_logic_vector(7 downto 0);
  signal requestDone : std_logic;
  signal requestAck : std_logic;

  signal writeReady : std_logic;
  signal writeResponse : std_logic;
  signal writeVc : std_logic;
  signal writeCrf : std_logic;
  signal writePrio : std_logic_vector(1 downto 0);
  signal writeTt : std_logic_vector(1 downto 0);
  signal writeDstId : std_logic_vector(31 downto 0);
  signal writeSrcId : std_logic_vector(31 downto 0);
  signal writeTid : std_logic_vector(7 downto 0);
  signal writeAddress : std_logic_vector(16*EXTENDED_ADDRESS+30 downto 0);
  signal writeLength : std_logic_vector(4 downto 0);
  signal writeSelect : std_logic_vector(7 downto 0);
  signal writePayload : std_logic_vector(63 downto 0);
  signal writeDone : std_logic;
  signal writeAck : std_logic;

  signal responseReady : std_logic;
  signal responseVc : std_logic;
  signal responseCrf : std_logic;
  signal responsePrio : std_logic_vector(1 downto 0);
  signal responseTt : std_logic_vector(1 downto 0);
  signal responseDstId : std_logic_vector(31 downto 0);
  signal responseSrcId : std_logic_vector(31 downto 0);
  signal responseTid : std_logic_vector(7 downto 0);
  signal responseError : std_logic;
  signal responsePayloadPresent : std_logic;
  signal responsePayload : std_logic_vector(63 downto 0);
  signal responseDone : std_logic;

  signal readRequestInbound : std_logic;
  signal writeRequestInbound : std_logic;
  signal vcInbound : std_logic;
  signal crfInbound : std_logic;
  signal prioInbound : std_logic_vector(1 downto 0);
  signal ttInbound : std_logic_vector(1 downto 0);
  signal dstIdInbound : std_logic_vector(31 downto 0);
  signal srcIdInbound : std_logic_vector(31 downto 0);
  signal tidInbound : std_logic_vector(7 downto 0);
  signal offsetInbound : std_logic_vector(20 downto 0);
  signal wdptrInbound : std_logic;
  signal payloadLengthInbound : std_logic_vector(3 downto 0);
  signal payloadInbound : std_logic_vector(31 downto 0);

  signal readResponseMaint : std_logic;
  signal writeResponseMaint : std_logic;
  signal wdptrMaint : std_logic;
  signal payloadLengthMaint : std_logic_vector(3 downto 0);
  signal payloadIndexMaint : std_logic_vector(3 downto 0);
  signal payloadMaint : std_logic_vector(31 downto 0);
  signal doneMaint : std_logic;
  
  signal payloadIndexOutbound : std_logic_vector(3 downto 0);
  signal doneOutbound : std_logic;

  signal configStb : std_logic;
  signal configWe : std_logic;
  signal configAdr : std_logic_vector(21 downto 0);
  signal configAdrByte : std_logic_vector(23 downto 0);
  signal configDatWrite : std_logic_vector(31 downto 0);
  signal configDatRead : std_logic_vector(31 downto 0);
  signal configAck : std_logic;
  signal maintenanceAck : std_logic;
  
  signal inboundCyc : std_logic;
  signal inboundStb : std_logic;
  signal inboundAdr : std_logic_vector(7 downto 0);
  signal inboundDat : std_logic_vector(31 downto 0);
  signal inboundAck : std_logic;

  signal outboundCyc : std_logic_vector(PORTS-1 downto 0);
  signal outboundStb : std_logic_vector(PORTS-1 downto 0);
  signal outboundDat : std_logic_vector(32*PORTS-1 downto 0);
  signal outboundAck : std_logic_vector(PORTS-1 downto 0);

begin

  responseVc <= requestVc when (responsePayloadPresent = '1') else writeVc;
  responseCrf <= requestCrf when (responsePayloadPresent = '1') else writeCrf;
  responsePrio <= requestPrio when (responsePayloadPresent = '1') else writePrio;
  responseTt <= requestTt when (responsePayloadPresent = '1') else writeTt;
  responseDstId <= requestSrcId when (responsePayloadPresent = '1') else writeSrcId;
  responseSrcId <= requestDstId when (responsePayloadPresent = '1') else writeDstId;
  responseTid <= requestTid when (responsePayloadPresent = '1') else writeTid;
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  adr_o <= adr;
  dat_o <= datOut;
  
  Bridge: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      cyc_o <= '0';
      stb_o <= '0';
      we_o <= '0';
      adr <= (others=>'0');
      datOut <= (others=>'0');

      payloadUpdate <= '0';
      payloadIndex <= (others=>'0');
      
      requestDone <= '0';
      
      writeDone <= '0';
      
      responseReady <= '0';
      responseError <= '0';
      responsePayloadPresent <= '0';
      responsePayload <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        requestDone <= '0';
        writeDone <= '0';
        responseReady <= '0';
        
        payloadUpdate <= '0';
        if (payloadUpdate = '1') then
          payloadIndex <= std_logic_vector(unsigned(payloadIndex) + 1);
        end if;
        
        case state is
          when IDLE =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (requestReady = '1') then
              cyc_o <= '1';
              stb_o <= '1';
              we_o <= '0';
              adr <= requestAddress;
              sel_o <= requestSelect;
              
              payloadIndex <= (others=>'0');
              
              responsePayloadPresent <= '1';
              state <= REQUEST_CLASS;
            elsif (writeReady = '1') then
              adr <= writeAddress;

              payloadUpdate <= '1';
              payloadIndex <= (others=>'0');
              
              responsePayloadPresent <= '0';
              state <= WRITE_CLASS;
            end if;          

          when REQUEST_CLASS =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (ack_i = '1') then
              -- Note that responsePayloadIndex is updated after the write has been made.
              payloadUpdate <= '1';
              responsePayload <= dat_i;
              
              adr <= std_logic_vector(unsigned(adr) + 1);

              if (payloadIndex = requestLength) then
                requestDone <= '1';
                cyc_o <= '0';
                stb_o <= '0';
                state <= REQUEST_CLASS_RESPONSE;
              end if;
--          elsif(err_i = '1') then
--            REMARK: Implement error indication from wb-bus...
            end if;

          when REQUEST_CLASS_RESPONSE =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (responseDone = '1') then
              responseReady <= '0';
              state <= IDLE;
            else
              responseReady <= '1';
            end if;

          when WRITE_CLASS =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            state <= WRITE_CLASS_ACCESS;

          when WRITE_CLASS_ACCESS =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            cyc_o <= '1';
            stb_o <= '1';
            we_o <= '1';
            sel_o <= writeSelect;
            datOut <= writePayload;
            payloadUpdate <= '1';

            state <= WRITE_CLASS_ACK;

          when WRITE_CLASS_ACK =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (ack_i = '1') then
              adr <= std_logic_vector(unsigned(adr) + 1);
              
              if (unsigned(payloadIndex) /= (unsigned(writeLength)+2)) then
                stb_o <= '0';
                state <= WRITE_CLASS_ACCESS;
              else
                writeDone <= '1';
                cyc_o <= '0';
                stb_o <= '0';
                state <= WRITE_CLASS_RESPONSE;
              end if;
            end if;

          when WRITE_CLASS_RESPONSE =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (responseDone = '1') or (writeResponse = '0') then
              responseReady <= '0';
              state <= IDLE;
            else
              responseReady <= '1';
            end if;

          when others =>

        end case;
      end if;
    end if;
  end process;
  
  -----------------------------------------------------------------------------
  -- Packet handlers.
  -----------------------------------------------------------------------------

  RequestClassInboundInst: RequestClassInbound
    generic map(
      EXTENDED_ADDRESS=>EXTENDED_ADDRESS)
    port map(
      clk=>clk,
      areset_n=>areset_n,
      enable=>enable,
      ready_o=>requestReady,
      vc_o=>requestVc,
      crf_o=>requestCrf,
      prio_o=>requestPrio,
      tt_o=>requestTt,
      dstId_o=>requestDstId,
      srcId_o=>requestSrcId,
      tid_o=>requestTid,
      address_o=>requestAddress,
      length_o=>requestLength,
      select_o=>requestSelect,
      done_i=>requestDone,
      inboundCyc_i=>inboundCyc,
      inboundStb_i=>inboundStb,
      inboundAdr_i=>inboundAdr,
      inboundDat_i=>inboundDat,
      inboundAck_o=>requestAck);

  WriteClassInboundInst: WriteClassInbound
    generic map(
      EXTENDED_ADDRESS=>EXTENDED_ADDRESS)
    port map(
      clk=>clk, 
      areset_n=>areset_n, 
      enable=>enable, 
      ready_o=>writeReady, 
      responseNeeded_o=>writeResponse,
      vc_o=>writeVc, 
      crf_o=>writeCrf, 
      prio_o=>writePrio, 
      tt_o=>writeTt, 
      dstId_o=>writeDstId, 
      srcId_o=>writeSrcId, 
      tid_o=>writeTid, 
      address_o=>writeAddress, 
      length_o=>writeLength, 
      select_o=>writeSelect, 
      payloadRead_i=>payloadUpdate, 
      payloadIndex_i=>payloadIndex, 
      payload_o=>writePayload, 
      done_i=>writeDone, 
      inboundCyc_i=>inboundCyc, 
      inboundStb_i=>inboundStb, 
      inboundAdr_i=>inboundAdr, 
      inboundDat_i=>inboundDat, 
      inboundAck_o=>writeAck);

  ResponseClassOutboundInst: ResponseClassOutbound
    port map(
      clk=>clk,
      areset_n=>areset_n,
      enable=>enable,
      ready_i=>responseReady,
      vc_i=>responseVc,
      crf_i=>responseCrf,
      prio_i=>responsePrio,
      tt_i=>responseTt,
      dstid_i=>responseDstId,
      srcid_i=>responseSrcId,
      tid_i=>responseTid,
      error_i=>responseError,
      payloadPresent_i=>responsePayloadPresent,
      payloadLength_i=>requestLength,
      payloadWrite_i=>payloadUpdate,
      payloadIndex_i=>payloadIndex,
      payload_i=>responsePayload,
      done_o=>responseDone,
      outboundCyc_o=>outboundCyc(0),
      outboundStb_o=>outboundStb(0),
      outboundDat_o=>outboundDat(31 downto 0),
      outboundAck_i=>outboundAck(0));

  -----------------------------------------------------------------------------
  -- Maintenance packet processing.
  -----------------------------------------------------------------------------
  InboundMaintenance: MaintenanceInbound
    port map(
      clk=>clk, areset_n=>areset_n, enable=>enable, 
      readRequestReady_o=>readRequestInbound,
      writeRequestReady_o=>writeRequestInbound,
      readResponseReady_o=>open,
      writeResponseReady_o=>open,
      portWriteReady_o=>open,
      vc_o=>vcInbound, 
      crf_o=>crfInbound, 
      prio_o=>prioInbound, 
      tt_o=>ttInbound, 
      dstid_o=>dstIdInbound, 
      srcid_o=>srcIdInbound, 
      tid_o=>tidInbound,
      hop_o=>open,
      offset_o=>offsetInbound,
      wdptr_o=>wdptrInbound,
      payloadLength_o=>payloadLengthInbound, 
      payloadIndex_i=>payloadIndexMaint, 
      payload_o=>payloadInbound, 
      done_i=>doneMaint, 
      inboundCyc_i=>inboundCyc, 
      inboundStb_i=>inboundStb, 
      inboundAdr_i=>inboundAdr, 
      inboundDat_i=>inboundDat, 
      inboundAck_o=>maintenanceAck);

  OutboundMaintenance: MaintenanceOutbound
    port map(
      clk=>clk, areset_n=>areset_n, enable=>enable, 
      readRequestReady_i=>'0',
      writeRequestReady_i=>'0',
      readResponseReady_i=>readResponseMaint, 
      writeResponseReady_i=>writeResponseMaint, 
      portWriteReady_i=>'0',
      vc_i=>vcInbound, 
      crf_i=>crfInbound, 
      prio_i=>prioInbound, 
      tt_i=>ttInbound, 
      dstid_i=>srcIdInbound, 
      srcid_i=>dstIdInbound,
      status_i=>"0000",
      tid_i=>tidInbound,
      hop_i=>x"ff",
      offset_i=>(others=>'0'),
      wdptr_i=>wdptrMaint,
      payloadLength_i=>payloadLengthMaint, 
      payloadIndex_o=>payloadIndexOutbound, 
      payload_i=>payloadMaint,
      done_o=>doneOutbound, 
      outboundCyc_o=>outboundCyc(1), 
      outboundStb_o=>outboundStb(1), 
      outboundDat_o=>outboundDat(63 downto 32), 
      outboundAck_i=>outboundAck(1));

  MaintenanceBridge: RioLogicalMaintenance
    port map(
      clk=>clk, areset_n=>areset_n, enable=>enable, 
      readRequestReady_i=>readRequestInbound, 
      writeRequestReady_i=>writeRequestInbound, 
      offset_i=>offsetInbound, 
      wdptr_i=>wdptrInbound, 
      payloadLength_i=>payloadLengthInbound, 
      payloadIndex_o=>payloadIndexMaint, 
      payload_i=>payloadInbound, 
      done_o=>doneMaint,
      readResponseReady_o=>readResponseMaint, 
      writeResponseReady_o=>writeResponseMaint,
      wdptr_o=>wdptrMaint, 
      payloadLength_o=>payloadLengthMaint, 
      payloadIndex_i=>payloadIndexOutbound, 
      payload_o=>payloadMaint, 
      done_i=>doneOutbound, 
      configStb_o=>configStb, 
      configWe_o=>configWe, 
      configAdr_o=>configAdr, 
      configDat_o=>configDatWrite, 
      configDat_i=>configDatRead, 
      configAck_i=>configAck);

  -----------------------------------------------------------------------------
  -- Common interface toward the packet queues.
  -----------------------------------------------------------------------------
  
  inboundAck <= requestAck or writeAck or maintenanceAck;
  RioLogicalCommonInst: RioLogicalCommon
    generic map(PORTS=>PORTS)
    port map(
      clk=>clk,
      areset_n=>areset_n,
      enable=>enable,
      readFrameEmpty_i=>readFrameEmpty_i,
      readFrame_o=>readFrame_o,
      readContent_o=>readContent_o,
      readContentEnd_i=>readContentEnd_i,
      readContentData_i=>readContentData_i,
      writeFrameFull_i=>writeFrameFull_i,
      writeFrame_o=>writeFrame_o,
      writeFrameAbort_o=>writeFrameAbort_o,
      writeContent_o=>writeContent_o,
      writeContentData_o=>writeContentData_o,
      inboundCyc_o=>inboundCyc,
      inboundStb_o=>inboundStb,
      inboundAdr_o=>inboundAdr,
      inboundDat_o=>inboundDat,
      inboundAck_i=>inboundAck,
      outboundCyc_i=>outboundCyc,
      outboundStb_i=>outboundStb,
      outboundDat_i=>outboundDat,
      outboundAck_o=>outboundAck);

  -----------------------------------------------------------------------------
  -- Configuration memory.
  -----------------------------------------------------------------------------
  configAdrByte <= configAdr & "00";
  memoryConfig : process(clk, areset_n)
    variable componentTag : std_logic_vector(31 downto 0);
    variable hostBaseDeviceIdLocked : std_logic;
    variable hostBaseDeviceId : std_logic_vector(15 downto 0);
  begin
    if (areset_n = '0') then
      componentTag := (others => '0');
      hostBaseDeviceIdLocked := '0';
      hostBaseDeviceId := (others => '1');

      configDatRead <= (others => '0');
      configAck <= '0';
    elsif (clk'event and clk = '1') then
      if (configAck = '0') then
        if (configStb = '1') then
          configAck <= '1';
          configDatRead <= (others=>'0');
          
          -- Check the address the access is for.
          case (configAdrByte) is
            when x"000000" =>
              -----------------------------------------------------------------
              -- Device Identity CAR. Read-only.
              -----------------------------------------------------------------
              configDatRead(31 downto 16) <= DEVICE_IDENTITY;
              configDatRead(15 downto 0) <= DEVICE_VENDOR_IDENTITY;
              
            when x"000004" =>
              -----------------------------------------------------------------
              -- Device Information CAR. Read-only.
              -----------------------------------------------------------------
              configDatRead(31 downto 0) <= DEVICE_REV;
              
            when x"000008" =>
              -----------------------------------------------------------------
              -- Assembly Identity CAR. Read-only.
              -----------------------------------------------------------------
              configDatRead(31 downto 16) <= ASSY_IDENTITY;
              configDatRead(15 downto 0) <= ASSY_VENDOR_IDENTITY;
              
            when x"00000c" =>
              -----------------------------------------------------------------
              -- Assembly Information CAR. Read-only.
              -----------------------------------------------------------------
              configDatRead(31 downto 16) <= ASSY_REV;
              
            when x"000010" =>
              -----------------------------------------------------------------
              -- Processing Element Features CAR. Read-only.
              -----------------------------------------------------------------
              -- Bridge.
              configDatRead(31) <= '1';
              -- Extended addressing support, 34-bit addresses.
              configDatRead(2 downto 0) <= "001";
              
            when x"000018" =>
              -----------------------------------------------------------------
              -- Source Operations CAR. Read-only.
              -----------------------------------------------------------------
              -- Cannot act as a source of any packets.
              
            when x"00001c" =>
              -----------------------------------------------------------------
              -- Destination Operations CAR. Read-only.
              -----------------------------------------------------------------
              -- Read, supported.
              configDatRead(15) <= '1';
              -- Write, supported.
              configDatRead(14) <= '1';
              -- Write-with-response, supported.
              configDatRead(12) <= '1';
              
            when x"00004c" =>
              -----------------------------------------------------------------
              -- Processing Element Logical Layer Control CSR.
              -----------------------------------------------------------------
              -- Extended addressing control, PE supports 34 bits addresses.
              configDatRead(2 downto 0) <= "001";

            when x"000060" =>
              -----------------------------------------------------------------
              -- Base Device ID CSR.
              -----------------------------------------------------------------
              -- This is not used since this endpoint only replies to packets
              -- and it can then use the destination deviceId as a source.
              
            when x"000068" =>
              -----------------------------------------------------------------
              -- Host Base Device ID Lock CSR.
              -----------------------------------------------------------------

              -- Check if writing.
              if (configWe = '1') then
                -- Write access.
                
                -- Check if this field has been written before.
                if (hostBaseDeviceIdLocked = '0') then
                  -- The field has not been written.
                  -- Lock the field and set the host base device id.
                  hostBaseDeviceIdLocked := '1';
                  hostBaseDeviceId := configDatWrite(15 downto 0);
                else
                  -- The field has been written.
                  -- Check if the written data is the same as the stored.
                  if (hostBaseDeviceId = configDatWrite(15 downto 0)) then
                    -- Same as stored, reset the value to its initial value.
                    hostBaseDeviceIdLocked := '0';
                    hostBaseDeviceId := (others => '1');
                  else
                    -- Not writing the same as the stored value.
                    -- Ignore the write.
                  end if;
                end if;
              end if;
              
              configDatRead(15 downto 0) <= hostBaseDeviceId;
              
            when x"00006C" =>
              -----------------------------------------------------------------
              -- Component TAG CSR.
              -----------------------------------------------------------------
              if (configWe = '1') then
                componentTag := configDatWrite;
              end if;

              configDatRead <= componentTag;

            when others =>
              -----------------------------------------------------------------
              -- Other access.
              -----------------------------------------------------------------
              -- Respond with all zeros.
              configDatRead <= (others => '0');
          end case;
        end if;
      else
        configAck <= '0';
      end if;
    end if;
  end process;

end architecture;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
-- REMARK: Extended addresses are not supported yet...
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity RequestClassInbound is
  generic(
    EXTENDED_ADDRESS : natural range 0 to 2 := 0);
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    ready_o : out std_logic;
    vc_o : out std_logic;
    crf_o : out std_logic;
    prio_o : out std_logic_vector(1 downto 0);
    tt_o : out std_logic_vector(1 downto 0);
    dstId_o : out std_logic_vector(31 downto 0);
    srcId_o : out std_logic_vector(31 downto 0);
    tid_o : out std_logic_vector(7 downto 0);
    address_o : out std_logic_vector(16*EXTENDED_ADDRESS+30 downto 0);
    length_o : out std_logic_vector(4 downto 0);
    select_o : out std_logic_vector(7 downto 0);
    done_i : in std_logic;
    
    inboundCyc_i : in std_logic;
    inboundStb_i : in std_logic;
    inboundAdr_i : in std_logic_vector(7 downto 0);
    inboundDat_i : in std_logic_vector(31 downto 0);
    inboundAck_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RequestClassInbound of RequestClassInbound is
  type StateType is (RECEIVE_PACKET, READY);
  signal state : StateType;

  signal rdsize : std_logic_vector(3 downto 0);
  signal wdptr : std_logic;

  signal inboundAck : std_logic;
  signal complete : std_logic;

  signal packetIndex : natural range 0 to 69;

begin

  inboundAck_o <= inboundAck;

  ready_o <= complete when (state = READY) else '0';

  RequestClass: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      state <= RECEIVE_PACKET;
      
      rdsize <= (others=>'0');
      wdptr <= '0';
      
      inboundAck <= '0';
      complete <= '0';
      
      packetIndex <= 0;
      
      vc_o <= '0';
      crf_o <= '0';
      prio_o <= "00";
      tt_o <= "00";
      dstId_o <= (others=>'0');
      srcId_o <= (others=>'0');
      tid_o <= (others=>'0');
      address_o <= (others=>'0');
    elsif (clk'event and clk = '1') then
      case state is
        when RECEIVE_PACKET =>
          ---------------------------------------------------------------------
          -- This state waits for a new REQUEST class packet, receives it
          -- and parses it.
          ---------------------------------------------------------------------
          if (inboundCyc_i = '1') then
            if (inboundAck = '0') then
              if (inboundStb_i = '1') then
                if (inboundAdr_i = x"24") then
                  -------------------------------------------------------------
                  -- NREAD packet parser.
                  -------------------------------------------------------------
                  case (packetIndex) is
                    when 0 =>
                      -- x"0000" & ackid & vc & crf & prio & tt & ftype
                      vc_o <= inboundDat_i(9);
                      crf_o <= inboundDat_i(8);
                      prio_o <= inboundDat_i(7 downto 6);
                      tt_o <= inboundDat_i(5 downto 4);
                      packetIndex <= packetIndex + 1;
                    when 1 =>
                      -- dstid
                      dstId_o <= inboundDat_i;
                      packetIndex <= packetIndex + 1;
                    when 2 =>
                      -- srcid
                      srcId_o <= inboundDat_i;
                      packetIndex <= packetIndex + 1;
                    when 3 =>
                      -- transaction(3:0) & rdsize(3:0) & srcTID(7:0) & address(28:13)
                      rdsize <= inboundDat_i(27 downto 24);
                      tid_o <= inboundDat_i(23 downto 16);
                      address_o(28 downto 13) <= inboundDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 4 =>
                      -- address(12:0) & wdptr & xamsbs(1:0) & crc(15:0)
                      address_o(12 downto 0) <= inboundDat_i(31 downto 19);
                      wdptr <= inboundDat_i(18);
                      address_o(30 downto 29) <= inboundDat_i(17 downto 16);
                      packetIndex <= packetIndex + 1;
                      complete <= '1';
                    when others =>
                      -- There should be no more content in an NREAD.
                      -- Discard.
                  end case;
                  inboundAck <= '1';
                end if;
              end if;
            else
              inboundAck <= '0';
            end if;
          else
            if (complete = '1') then
              state <= READY;
            end if;
            packetIndex <= 0;
          end if;

        when READY =>
          ---------------------------------------------------------------------
          -- Wait for the handler of the packet to signal that it has been
          -- processed.
          ---------------------------------------------------------------------
          if (done_i = '1') then
            complete <= '0';
            state <= RECEIVE_PACKET;
          end if;
          
        when others =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------

      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Transformation of rdsize and wdptr into length of access and byte lanes.
  -----------------------------------------------------------------------------
  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      length_o <= "00000";
      select_o <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (complete = '1') then
        if (wdptr = '0') then
          case rdsize is
            when "0000" =>
              length_o <= "00000";
              select_o <= "10000000";
            when "0001" =>
              length_o <= "00000";
              select_o <= "01000000";
            when "0010" =>
              length_o <= "00000";
              select_o <= "00100000";
            when "0011" =>
              length_o <= "00000";
              select_o <= "00010000";
            when "0100" =>
              length_o <= "00000";
              select_o <= "11000000";
            when "0101" =>
              length_o <= "00000";
              select_o <= "11100000";
            when "0110" =>
              length_o <= "00000";
              select_o <= "00110000";
            when "0111" =>
              length_o <= "00000";
              select_o <= "11111000";
            when "1000" =>
              length_o <= "00000";
              select_o <= "11110000";
            when "1001" =>
              length_o <= "00000";
              select_o <= "11111100";
            when "1010" =>
              length_o <= "00000";
              select_o <= "11111110";
            when "1011" =>
              length_o <= "00000";
              select_o <= "11111111";
            when "1100" =>
              length_o <= "00011";
              select_o <= "11111111";
            when "1101" =>
              length_o <= "01011";
              select_o <= "11111111";
            when "1110" =>
              length_o <= "10011";
              select_o <= "11111111";
            when others =>
              length_o <= "11011";
              select_o <= "11111111";
          end case;
        else
          case rdsize is
            when "0000" =>
              length_o <= "00000";
              select_o <= "00001000";
            when "0001" =>
              length_o <= "00000";
              select_o <= "00000100";
            when "0010" =>
              length_o <= "00000";
              select_o <= "00000010";
            when "0011" =>
              length_o <= "00000";
              select_o <= "00000001";
            when "0100" =>
              length_o <= "00000";
              select_o <= "00001100";
            when "0101" =>
              length_o <= "00000";
              select_o <= "00000111";
            when "0110" =>
              length_o <= "00000";
              select_o <= "00000011";
            when "0111" =>
              length_o <= "00000";
              select_o <= "00011111";
            when "1000" =>
              length_o <= "00000";
              select_o <= "00001111";
            when "1001" =>
              length_o <= "00000";
              select_o <= "00111111";
            when "1010" =>
              length_o <= "00000";
              select_o <= "01111111";
            when "1011" =>
              length_o <= "00001";
              select_o <= "11111111";
            when "1100" =>
              length_o <= "00111";
              select_o <= "11111111";
            when "1101" =>
              length_o <= "01111";
              select_o <= "11111111";
            when "1110" =>
              length_o <= "10111";
              select_o <= "11111111";
            when others =>
              length_o <= "11111";
              select_o <= "11111111";
          end case;
        end if;
      end if;
    end if;
  end process;

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
entity WriteClassInbound is
  generic(
    EXTENDED_ADDRESS : natural range 0 to 2 := 0);
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    ready_o : out std_logic;
    responseNeeded_o : out std_logic;
    vc_o : out std_logic;
    crf_o : out std_logic;
    prio_o : out std_logic_vector(1 downto 0);
    tt_o : out std_logic_vector(1 downto 0);
    dstId_o : out std_logic_vector(31 downto 0);
    srcId_o : out std_logic_vector(31 downto 0);
    tid_o : out std_logic_vector(7 downto 0);
    address_o : out std_logic_vector(16*EXTENDED_ADDRESS+30 downto 0);
    length_o : out std_logic_vector(4 downto 0);
    select_o : out std_logic_vector(7 downto 0);
    payloadRead_i : in std_logic;
    payloadIndex_i : in std_logic_vector(4 downto 0);
    payload_o : out std_logic_vector(63 downto 0);
    done_i : in std_logic;
    
    inboundCyc_i : in std_logic;
    inboundStb_i : in std_logic;
    inboundAdr_i : in std_logic_vector(7 downto 0);
    inboundDat_i : in std_logic_vector(31 downto 0);
    inboundAck_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture WriteClassInbound of WriteClassInbound is
  component MemorySimpleDualPort
    generic(
      ADDRESS_WIDTH : natural := 1;
      DATA_WIDTH : natural := 1);
    port(
      clkA_i : in std_logic;
      enableA_i : in std_logic;
      addressA_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      dataA_i : in std_logic_vector(DATA_WIDTH-1 downto 0);

      clkB_i : in std_logic;
      enableB_i : in std_logic;
      addressB_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      dataB_o : out std_logic_vector(DATA_WIDTH-1 downto 0));
  end component;

  type StateType is (RECEIVE_PACKET, READY);
  signal state : StateType;

  signal wdptr : std_logic;
  signal wrsize : std_logic_vector(3 downto 0);

  signal inboundAck : std_logic;
  signal complete : std_logic;

  signal packetIndex : natural range 0 to 69;

  signal doubleWord : std_logic_vector(63 downto 16);
  
  signal memoryWrite : std_logic;
  signal memoryAddress : std_logic_vector(4 downto 0);
  signal memoryDataIn : std_logic_vector(63 downto 0);

  
begin

  inboundAck_o <= inboundAck;

  ready_o <= complete when (state = READY) else '0';

  WriteClass: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      state <= RECEIVE_PACKET;

      wdptr <= '0';
      wrsize <= (others=>'0');
      
      inboundAck <= '0';
      complete <= '0';
      responseNeeded_o <= '0';
      
      packetIndex <= 0;
      doubleWord <= (others=>'0');

      memoryWrite <= '0';
      memoryAddress <= (others=>'0');
      memoryDataIn <= (others=>'0');

      vc_o <= '0';
      crf_o <= '0';
      prio_o <= "00";
      tt_o <= "00";
      dstId_o <= (others=>'0');
      srcId_o <= (others=>'0');
      tid_o <= (others=>'0');
      address_o <= (others=>'0');
    elsif (clk'event and clk = '1') then
      case state is
        when RECEIVE_PACKET =>
          if (inboundCyc_i = '1') then
            if (inboundAck = '0') then
              if (inboundStb_i = '1') then
                if ((inboundAdr_i = x"55") or (inboundAdr_i = x"54")) then
                  -------------------------------------------------------------
                  -- NWRITE/NWRITER packet parser.
                  -------------------------------------------------------------
                  case (packetIndex) is
                    when 0 =>
                      -- x"0000" & ackid & vc & crf & prio & tt & ftype
                      vc_o <= inboundDat_i(9);
                      crf_o <= inboundDat_i(8);
                      prio_o <= inboundDat_i(7 downto 6);
                      tt_o <= inboundDat_i(5 downto 4);
                      packetIndex <= packetIndex + 1;
                    when 1 =>
                      -- destId
                      dstId_o <= inboundDat_i;
                      packetIndex <= packetIndex + 1;
                    when 2 =>
                      -- srcId
                      srcId_o <= inboundDat_i;
                      packetIndex <= packetIndex + 1;
                    when 3 =>
                      -- transaction & wrsize & srcTID & address(28:13)
                      -- REMARK: Add support for extended addresses here...
                      if (inboundDat_i(31 downto 28) = "0101") then
                        responseNeeded_o <= '1';
                      else
                        responseNeeded_o <= '0';
                      end if;
                      wrsize <= inboundDat_i(27 downto 24);
                      tid_o <= inboundDat_i(23 downto 16);
                      address_o(28 downto 13) <= inboundDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 4 =>
                      -- address(12:0) & wdptr & xamsbs(1:0) & double-word(63:48)
                      address_o(12 downto 0) <= inboundDat_i(31 downto 19);
                      wdptr <= inboundDat_i(18);
                      address_o(30 downto 29) <= inboundDat_i(17 downto 16);
                      doubleWord(63 downto 48) <= inboundDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 | 21 | 23 | 25 | 27 | 29 | 31  | 33 | 35 |
                      37 | 39 | 41 | 43 | 45 | 47 | 49 | 51 | 53 | 55 | 57 | 59 | 61 | 63 | 65 | 67 =>
                      -- double-word(47:16)
                      doubleWord(47 downto 16) <= inboundDat_i;
                      packetIndex <= packetIndex + 1;
                    when 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 | 32  | 34 |
                      36 | 38 | 40 | 42 | 44 | 46 | 48 | 50 | 52 | 54 | 56 | 58 | 60 | 62 | 64 | 66 | 68 =>
                      -- double-word(15:0) & double-word(63:48)
                      doubleWord(63 downto 48) <= inboundDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;

                      memoryWrite <= '1';
                      memoryDataIn <= doubleWord(63 downto 16) & inboundDat_i(31 downto 16);
                    when others =>
                      -- There should be no more content in an NWRITE/NWRITER request.
                      -- Discard.
                  end case;
                  inboundAck <= '1';
                end if;
              end if;
            else
              if (memoryWrite = '1') then
                memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
              end if;
              
              memoryWrite <= '0';
              inboundAck <= '0';
            end if;
          else
            if (packetIndex >= 6) then
              complete <= '1';
              state <= READY;
            else
              packetIndex <= 0;
              memoryAddress <= (others=>'0');
            end if;
          end if;

        when READY =>
          ---------------------------------------------------------------------
          -- Wait for the handler of the packet to signal that it has been
          -- processed.
          ---------------------------------------------------------------------
          if (done_i = '1') then
            packetIndex <= 0;
            memoryAddress <= (others=>'0');
            complete <= '0';
            state <= RECEIVE_PACKET;
          end if;
          
        when others =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------

      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Transformation of wrsize and wdptr into length of access and byte lanes.
  -----------------------------------------------------------------------------
  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      length_o <= "00000";
      select_o <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (complete = '1') then
        if (wdptr = '0') then
          case wrsize is
            when "0000" =>
              length_o <= "00000";
              select_o <= "10000000";
            when "0001" =>
              length_o <= "00000";
              select_o <= "01000000";
            when "0010" =>
              length_o <= "00000";
              select_o <= "00100000";
            when "0011" =>
              length_o <= "00000";
              select_o <= "00010000";
            when "0100" =>
              length_o <= "00000";
              select_o <= "11000000";
            when "0101" =>
              length_o <= "00000";
              select_o <= "11100000";
            when "0110" =>
              length_o <= "00000";
              select_o <= "00110000";
            when "0111" =>
              length_o <= "00000";
              select_o <= "11111000";
            when "1000" =>
              length_o <= "00000";
              select_o <= "11110000";
            when "1001" =>
              length_o <= "00000";
              select_o <= "11111100";
            when "1010" =>
              length_o <= "00000";
              select_o <= "11111110";
            when "1011" =>
              length_o <= "00000";
              select_o <= "11111111";
            when others =>
              length_o <= std_logic_vector(unsigned(memoryAddress)-1);
              select_o <= "11111111";
          end case;
        else
          case wrsize is
            when "0000" =>
              length_o <= "00000";
              select_o <= "00001000";
            when "0001" =>
              length_o <= "00000";
              select_o <= "00000100";
            when "0010" =>
              length_o <= "00000";
              select_o <= "00000010";
            when "0011" =>
              length_o <= "00000";
              select_o <= "00000001";
            when "0100" =>
              length_o <= "00000";
              select_o <= "00001100";
            when "0101" =>
              length_o <= "00000";
              select_o <= "00000111";
            when "0110" =>
              length_o <= "00000";
              select_o <= "00000011";
            when "0111" =>
              length_o <= "00000";
              select_o <= "00011111";
            when "1000" =>
              length_o <= "00000";
              select_o <= "00001111";
            when "1001" =>
              length_o <= "00000";
              select_o <= "00111111";
            when "1010" =>
              length_o <= "00000";
              select_o <= "01111111";
            when others =>
              length_o <= std_logic_vector(unsigned(memoryAddress)-1);
              select_o <= "11111111";
          end case;
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Payload content memory.
  -----------------------------------------------------------------------------

  PayloadMemory: MemorySimpleDualPort
    generic map(ADDRESS_WIDTH=>5, DATA_WIDTH=>64)
    port map(clkA_i=>clk,
             enableA_i=>memoryWrite,
             addressA_i=>memoryAddress,
             dataA_i=>memoryDataIn,
             clkB_i=>clk,
             enableB_i=>payloadRead_i,
             addressB_i=>payloadIndex_i,
             dataB_o=>payload_o);

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
entity ResponseClassOutbound is
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    ready_i : in std_logic;
    vc_i : in std_logic;
    crf_i : in std_logic;
    prio_i : in std_logic_vector(1 downto 0);
    tt_i : in std_logic_vector(1 downto 0);
    dstid_i : in std_logic_vector(31 downto 0);
    srcid_i : in std_logic_vector(31 downto 0);
    tid_i : in std_logic_vector(7 downto 0);
    error_i : in std_logic;
    payloadPresent_i :  in std_logic;
    payloadLength_i : in std_logic_vector(4 downto 0);
    payloadWrite_i : in std_logic;
    payloadIndex_i : in std_logic_vector(4 downto 0);
    payload_i : in std_logic_vector(63 downto 0);
    done_o : out std_logic;
    
    outboundCyc_o : out std_logic;
    outboundStb_o : out std_logic;
    outboundDat_o : out std_logic_vector(31 downto 0);
    outboundAck_i : in std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture ResponseClassOutbound of ResponseClassOutbound is
  component MemorySimpleDualPort
    generic(
      ADDRESS_WIDTH : natural := 1;
      DATA_WIDTH : natural := 1);
    port(
      clkA_i : in std_logic;
      enableA_i : in std_logic;
      addressA_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      dataA_i : in std_logic_vector(DATA_WIDTH-1 downto 0);

      clkB_i : in std_logic;
      enableB_i : in std_logic;
      addressB_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      dataB_o : out std_logic_vector(DATA_WIDTH-1 downto 0));
  end component;

  signal header : std_logic_vector(31 downto 0);
  
  type StateType is (WAIT_PACKET, SEND_RESPONSE, 
                     WAIT_COMPLETE, RESPONSE_DONE);
  signal state : StateType;

  signal packetIndex : natural range 0 to 68;

  signal responsePayloadIndex : std_logic_vector(4 downto 0);
  signal responsePayload : std_logic_vector(15 downto 0);
    
  signal memoryEnable : std_logic;
  signal memoryAddress : std_logic_vector(4 downto 0);
  signal memoryDataRead : std_logic_vector(63 downto 0);

begin
  
  header <= x"0000" & "000000" & vc_i & crf_i & prio_i & tt_i & x"d";

  Response: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      state <= WAIT_PACKET;

      packetIndex <= 0;

      responsePayloadIndex <= (others=>'0');
      responsePayload <= (others=>'0');
        
      memoryEnable <= '0';
      memoryAddress <= (others=>'0');

      done_o <= '0';
      
      outboundCyc_o <= '0';
      outboundStb_o <= '0';
      outboundDat_o <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        case state is
          when WAIT_PACKET =>
            -------------------------------------------------------------------
            -- 
            -------------------------------------------------------------------
            if (ready_i = '1') then
              outboundCyc_o <= '1';
              outboundStb_o <= '1';
              outboundDat_o <= header;
              
              packetIndex <= 1;
              responsePayloadIndex <= (others=>'0');

              memoryEnable <= '1';
              memoryAddress <= (others=>'0');

              state <= SEND_RESPONSE;
            end if;

          when SEND_RESPONSE =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (outboundAck_i = '1') then
              case (packetIndex) is
                when 1 =>
                  -- destination
                  outboundDat_o <= dstId_i;
                  packetIndex <= packetIndex + 1;
                when 2 =>
                  -- source 
                  outboundDat_o <= srcId_i;
                  packetIndex <= packetIndex + 1;
                when 3 =>
                  -- transaction & status & targetTID & double-word0(63:48)
                  if (error_i = '0') then
                    if (payloadPresent_i = '0') then
                      outboundDat_o <= "0000" & "0000" & tid_i & x"0000";
                      state <= WAIT_COMPLETE;
                    else
                      outboundDat_o <= "1000" & "0000" & tid_i & memoryDataRead(63 downto 48);
                    end if;
                  else
                    outboundDat_o <= "0000" & "0111" & tid_i & x"0000";
                    state <= WAIT_COMPLETE;
                  end if;
                  packetIndex <= packetIndex + 1;
                when 4 | 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 | 32 | 34 |
                  36 | 38 | 40 | 42 | 44 | 46 | 48 | 50 | 52 | 54 | 56 | 58 | 60 | 62 | 64 | 66 =>
                  -- double-wordN(47:16)
                  outboundDat_o <= memoryDataRead(47 downto 16);
                  responsePayload <= memoryDataRead(15 downto 0);
                  memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
                  packetIndex <= packetIndex + 1;
                when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 | 21 | 23 | 25 | 27 | 29 | 31 | 33 | 35 |
                  37 | 39 | 41 | 43 | 45 | 47 | 49 | 51 | 53 | 55 | 57 | 59 | 61 | 63 | 65 | 67 =>
                  -- double-wordN(15:0) & double-wordN(63:48)
                  outboundDat_o <= responsePayload & memoryDataRead(63 downto 48);
                  packetIndex <= packetIndex + 1;

                  responsePayloadIndex <=
                    std_logic_vector(unsigned(responsePayloadIndex) + 1);
                  
                  if (responsePayloadIndex = payloadLength_i) then
                    state <= WAIT_COMPLETE;
                  else
                    packetIndex <= packetIndex + 1;
                  end if;
                when others =>
                  -- Unallowed response length.
                  -- Dont do anything.
              end case;
            end if;
            
          when WAIT_COMPLETE =>
            -------------------------------------------------------------------
            -- 
            -------------------------------------------------------------------
            if (outboundAck_i = '1') then
              outboundCyc_o <= '0';
              outboundStb_o <= '0';
              state <= RESPONSE_DONE;
            end if;
            
          when RESPONSE_DONE =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            memoryEnable <= '0';
            if (ready_i = '0') then
              state <= WAIT_PACKET;
              done_o <= '0';
            else
              done_o <= '1';
            end if;
            
          when others =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            state <= WAIT_PACKET;
            
        end case;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Payload content memory.
  -----------------------------------------------------------------------------
  PayloadMemory: MemorySimpleDualPort
    generic map(ADDRESS_WIDTH=>5, DATA_WIDTH=>64)
    port map(clkA_i=>clk,
             enableA_i=>payloadWrite_i,
             addressA_i=>payloadIndex_i,
             dataA_i=>payload_i,
             clkB_i=>clk,
             enableB_i=>memoryEnable,
             addressB_i=>memoryAddress,
             dataB_o=>memoryDataRead);

end architecture;
