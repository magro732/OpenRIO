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
-- - Add support for addressing to implementation defined config space by
--   adding interface to top entity.
-- - Add support for the lock_o to be sure to transfer all the packet
--   content atomically?
-- - Add support for EXTENDED_ADDRESS.
-- - Use the baseDeviceId when sending packets? Currently, all responses
--   are sent with destination<->source exchanged so the baseDeviceId is not
--   needed.
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
-- accesses. The bridge is acting as a Wishbone-slave only.
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
  constant PORTS : natural := 2;

  type StateType is (IDLE,
                     REQUEST_CLASS, REQUEST_CLASS_WAIT, REQUEST_CLASS_NEXT,
                     WRITE_CLASS, WRITE_CLASS_WAIT, WRITE_CLASS_NEXT,
                     RESPONSE_DONE_NO_PAYLOAD, RESPONSE_DONE_WITH_PAYLOAD,
                     RESPONSE_ERROR, RESPONSE_DONE);
  signal state : StateType;
  
  signal adr : std_logic_vector(16*EXTENDED_ADDRESS+30 downto 0);

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
  signal requestStall : std_logic;

  signal writeNoResponseReady : std_logic;
  signal writeWithResponseReady : std_logic;
  signal writeVc : std_logic;
  signal writeCrf : std_logic;
  signal writePrio : std_logic_vector(1 downto 0);
  signal writeTt : std_logic_vector(1 downto 0);
  signal writeDstId : std_logic_vector(31 downto 0);
  signal writeSrcId : std_logic_vector(31 downto 0);
  signal writeTid : std_logic_vector(7 downto 0);
  signal writeAddress : std_logic_vector(16*EXTENDED_ADDRESS+30 downto 0);
  signal writePayloadLength : std_logic_vector(4 downto 0);
  signal writePayloadIndex : std_logic_vector(4 downto 0);
  signal writeSelect : std_logic_vector(7 downto 0);
  signal writePayload : std_logic_vector(63 downto 0);
  signal writeDone : std_logic;
  signal writeStall : std_logic;

  signal responseDoneNoPayloadReady : std_logic;
  signal responseDoneWithPayloadReady : std_logic;
  signal responseErrorReady : std_logic;
  signal responseVc : std_logic;
  signal responseCrf : std_logic;
  signal responsePrio : std_logic_vector(1 downto 0);
  signal responseTt : std_logic_vector(1 downto 0);
  signal responseDstId : std_logic_vector(31 downto 0);
  signal responseSrcId : std_logic_vector(31 downto 0);
  signal responseTid : std_logic_vector(7 downto 0);
  signal responsePayloadIndex : std_logic_vector(4 downto 0);
  signal responsePayload : std_logic_vector(63 downto 0);
  signal responseDone : std_logic;

  signal maintReadRequestReady : std_logic;
  signal maintWriteRequestReady : std_logic;
  signal maintVcInbound : std_logic;
  signal maintCrfInbound : std_logic;
  signal maintPrioInbound : std_logic_vector(1 downto 0);
  signal maintTtInbound : std_logic_vector(1 downto 0);
  signal maintDstIdInbound : std_logic_vector(31 downto 0);
  signal maintSrcIdInbound : std_logic_vector(31 downto 0);
  signal maintSizeInbound : std_logic_vector(3 downto 0);
  signal maintTidInbound : std_logic_vector(7 downto 0);
  signal maintOffsetInbound : std_logic_vector(20 downto 0);
  signal maintWdptrInbound : std_logic;
  signal maintPayloadLengthInbound : std_logic_vector(2 downto 0);
  signal maintPayloadIndexInbound : std_logic_vector(2 downto 0);
  signal maintPayloadInbound : std_logic_vector(63 downto 0);
  signal maintDoneInbound : std_logic;
  signal maintStall : std_logic;

  signal maintReadResponseReady : std_logic;
  signal maintWriteResponseReady : std_logic;
  signal maintStatusOutbound : std_logic_vector(3 downto 0);
  signal maintPayloadLengthOutbound : std_logic_vector(2 downto 0);
  signal maintPayloadIndexOutbound : std_logic_vector(2 downto 0);
  signal maintPayloadOutbound : std_logic_vector(63 downto 0);
  signal maintDoneOutbound : std_logic;

  signal configStb : std_logic;
  signal configWe : std_logic;
  signal configAdr : std_logic_vector(21 downto 0);
  signal configAdrByte : std_logic_vector(23 downto 0);
  signal configDatWrite : std_logic_vector(31 downto 0);
  signal configDatRead : std_logic_vector(31 downto 0);
  signal configAck : std_logic;
  
  signal inboundStb : std_logic;
  signal inboundAdr : std_logic_vector(3 downto 0);
  signal inboundDat : std_logic_vector(31 downto 0);
  signal inboundStall : std_logic;

  signal outboundStb : std_logic_vector(PORTS-1 downto 0);
  signal outboundAdr : std_logic_vector(PORTS-1 downto 0);
  signal outboundDat : std_logic_vector(32*PORTS-1 downto 0);
  signal outboundStall : std_logic_vector(PORTS-1 downto 0);

  signal memoryWrite : std_logic;
  signal memoryAddress : std_logic_vector(4 downto 0);
  signal memoryDataIn : std_logic_vector(63 downto 0);
  
begin

  responseVc <= requestVc when (requestReady = '1') else writeVc;
  responseCrf <= requestCrf when (requestReady = '1') else writeCrf;
  responsePrio <= requestPrio when (requestReady = '1') else writePrio;
  responseTt <= requestTt when (requestReady = '1') else writeTt;
  responseDstId <= requestSrcId when (requestReady = '1') else writeSrcId;
  responseSrcId <= requestDstId when (requestReady = '1') else writeDstId;
  responseTid <= requestTid when (requestReady = '1') else writeTid;
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  adr_o <= adr;
  
  Bridge: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      cyc_o <= '0';
      stb_o <= '0';
      we_o <= '0';
      adr <= (others=>'0');
      dat_o <= (others=>'0');

      writePayloadIndex <= (others=>'0');
      
      requestDone <= '0';
      writeDone <= '0';
      
      responseDoneNoPayloadReady <= '0';
      responseDoneWithPayloadReady <= '0';
      responseErrorReady <= '0';
      
      memoryWrite <= '0';
      memoryAddress <= (others=>'0');
      memoryDataIn <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        case state is
          when IDLE =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            requestDone <= '0';
            writeDone <= '0';
            responseDoneNoPayloadReady <= '0';
            responseDoneWithPayloadReady <= '0';
            responseErrorReady <= '0';

            writePayloadIndex <= (others=>'0');
              
            if (requestReady = '1') then
              state <= REQUEST_CLASS;
            elsif (writeNoResponseReady = '1') or (writeWithResponseReady = '1') then
              state <= WRITE_CLASS;
            end if;          

          when REQUEST_CLASS =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            cyc_o <= '1';
            stb_o <= '1';
            we_o <= '0';
            adr <= requestAddress;
            sel_o <= requestSelect;

            memoryAddress <= writePayloadIndex;
            writePayloadIndex <= std_logic_vector(unsigned(writePayloadIndex) + 1);

            state <= REQUEST_CLASS_WAIT;
            
          when REQUEST_CLASS_WAIT =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (ack_i = '1') then
              if (writePayloadIndex = requestLength) then
                cyc_o <= '0';
              end if;
              stb_o <= '0';

              memoryWrite <= '1';
              memoryDataIn <= dat_i;

              state <= REQUEST_CLASS_NEXT;
            elsif (err_i = '1') then
              cyc_o <= '0';
              stb_o <= '0';
              state <= RESPONSE_ERROR;
            end if;

          when REQUEST_CLASS_NEXT =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            memoryWrite <= '0';
            memoryAddress <= writePayloadIndex;
            writePayloadIndex <= std_logic_vector(unsigned(writePayloadIndex) + 1);

            adr <= std_logic_vector(unsigned(adr) + 1);
            
            if (writePayloadIndex = requestLength) then
              state <= RESPONSE_DONE_WITH_PAYLOAD;
            else
              stb_o <= '1';
              state <= REQUEST_CLASS_WAIT;
            end if;

          when WRITE_CLASS =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            cyc_o <= '1';
            stb_o <= '1';
            we_o <= '1';
            adr <= writeAddress;
            sel_o <= writeSelect;
            dat_o <= writePayload;

            writePayloadIndex <= std_logic_vector(unsigned(writePayloadIndex) + 1);
            
            state <= WRITE_CLASS_WAIT;

          when WRITE_CLASS_WAIT =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (ack_i = '1') then
              if (writePayloadIndex = writePayloadLength) then
                cyc_o <= '0';
              end if;
              stb_o <= '0';
              
              state <= WRITE_CLASS_NEXT;
            elsif (err_i = '1') then
              writeDone <= '1';
              cyc_o <= '0';
              stb_o <= '0';
              state <= RESPONSE_ERROR;
            end if;

          when WRITE_CLASS_NEXT =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            adr <= std_logic_vector(unsigned(adr) + 1);
            dat_o <= writePayload;
            
            writePayloadIndex <= std_logic_vector(unsigned(writePayloadIndex) + 1);
            if (writePayloadIndex = writePayloadLength) then
              if (writeWithResponseReady = '1') then
                state <= RESPONSE_DONE_NO_PAYLOAD;
              else
                writeDone <= '1';
                state <= RESPONSE_DONE;
              end if;
            else
              stb_o <= '1';
              state <= WRITE_CLASS_WAIT;
            end if; 

          when RESPONSE_DONE_NO_PAYLOAD =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (responseDone = '1') then
              writeDone <= '1';
              responseDoneNoPayloadReady <= '0';
              state <= RESPONSE_DONE;
            else
              responseDoneNoPayloadReady <= '1';
            end if;

          when RESPONSE_DONE_WITH_PAYLOAD =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (responseDone = '1') then
              requestDone <= '1';
              responseDoneWithPayloadReady <= '0';
              state <= RESPONSE_DONE;
            else
              responseDoneWithPayloadReady <= '1';
            end if;

          when RESPONSE_ERROR =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            -- REMARK: Must indicate that the previous frame is done also.
            if (responseDone = '1') then
              responseErrorReady <= '0';
              state <= RESPONSE_DONE;
            else
              responseErrorReady <= '1';
            end if;

          when RESPONSE_DONE =>
            -------------------------------------------------------------------
            -- 
            -------------------------------------------------------------------
            state <= IDLE;
            
          when others =>

        end case;
      end if;
    end if;
  end process;
  
  PayloadMemory: MemorySimpleDualPort
    generic map(ADDRESS_WIDTH=>5, DATA_WIDTH=>64)
    port map(clkA_i=>clk,
             enableA_i=>memoryWrite,
             addressA_i=>memoryAddress,
             dataA_i=>memoryDataIn,
             clkB_i=>clk,
             enableB_i=>responseDoneWithPayloadReady,
             addressB_i=>responsePayloadIndex,
             dataB_o=>responsePayload);
  
  -----------------------------------------------------------------------------
  -- Inbound packet processing.
  -----------------------------------------------------------------------------

  InboundMaintenance: MaintenanceInbound
    generic map(
      ENABLE_READ_RESPONSE=>false,
      ENABLE_WRITE_RESPONSE=>false,
      ENABLE_PORT_WRITE=>false)
    port map(
      clk=>clk, areset_n=>areset_n, enable=>enable, 
      readRequestReady_o=>maintReadRequestReady,
      writeRequestReady_o=>maintWriteRequestReady,
      readResponseReady_o=>open,
      writeResponseReady_o=>open,
      portWriteReady_o=>open,
      vc_o=>maintVcInbound, 
      crf_o=>maintCrfInbound, 
      prio_o=>maintPrioInbound, 
      tt_o=>maintTtInbound, 
      dstid_o=>maintDstIdInbound, 
      srcid_o=>maintSrcIdInbound,
      size_o=>maintSizeInbound,
      status_o=>open,
      tid_o=>maintTidInbound,
      hop_o=>open,
      offset_o=>maintOffsetInbound,
      wdptr_o=>maintWdptrInbound,
      payloadLength_o=>maintPayloadLengthInbound, 
      payloadIndex_i=>maintPayloadIndexInbound, 
      payload_o=>maintPayloadInbound, 
      done_i=>maintDoneInbound, 
      inboundStb_i=>inboundStb, 
      inboundAdr_i=>inboundAdr, 
      inboundDat_i=>inboundDat, 
      inboundStall_o=>maintStall);

  RequestClassInboundInst: RequestClassInbound
    generic map(
      EXTENDED_ADDRESS=>EXTENDED_ADDRESS)
    port map(
      clk=>clk,
      areset_n=>areset_n,
      enable=>enable,
      nreadReady_o=>requestReady,
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
      inboundStb_i=>inboundStb,
      inboundAdr_i=>inboundAdr,
      inboundDat_i=>inboundDat,
      inboundStall_o=>requestStall);

  WriteClassInboundInst: WriteClassInbound
    generic map(
      EXTENDED_ADDRESS=>EXTENDED_ADDRESS)
    port map(
      clk=>clk, 
      areset_n=>areset_n, 
      enable=>enable, 
      nwriteReady_o=>writeNoResponseReady, 
      nwriterReady_o=>writeWithResponseReady,
      vc_o=>writeVc, 
      crf_o=>writeCrf, 
      prio_o=>writePrio, 
      tt_o=>writeTt, 
      dstId_o=>writeDstId, 
      srcId_o=>writeSrcId, 
      tid_o=>writeTid, 
      address_o=>writeAddress, 
      length_o=>writePayloadLength, 
      select_o=>writeSelect, 
      payloadIndex_i=>writePayloadIndex, 
      payload_o=>writePayload, 
      done_i=>writeDone, 
      inboundStb_i=>inboundStb, 
      inboundAdr_i=>inboundAdr, 
      inboundDat_i=>inboundDat, 
      inboundStall_o=>writeStall);

  -----------------------------------------------------------------------------
  -- Outbound packet processing.
  -----------------------------------------------------------------------------
  OutboundMaintenance: MaintenanceOutbound
    port map(
      clk=>clk, areset_n=>areset_n, enable=>enable, 
      readRequestReady_i=>'0',
      writeRequestReady_i=>'0',
      readResponseReady_i=>maintReadResponseReady, 
      writeResponseReady_i=>maintWriteResponseReady, 
      portWriteReady_i=>'0',
      vc_i=>maintVcInbound, 
      crf_i=>maintCrfInbound, 
      prio_i=>maintPrioInbound, 
      tt_i=>maintTtInbound, 
      dstid_i=>maintSrcIdInbound, 
      srcid_i=>maintDstIdInbound,
      size_i=>(others=>'0'),
      status_i=>maintStatusOutbound,
      tid_i=>maintTidInbound,
      hop_i=>x"ff",
      offset_i=>(others=>'0'),
      wdptr_i=>'0',
      payloadLength_i=>maintPayloadLengthOutbound, 
      payloadIndex_o=>maintPayloadIndexOutbound, 
      payload_i=>maintPayloadOutbound,
      done_o=>maintDoneOutbound, 
      outboundStb_o=>outboundStb(1), 
      outboundAdr_o=>outboundAdr(1), 
      outboundDat_o=>outboundDat(63 downto 32), 
      outboundStall_i=>outboundStall(1));

  ResponseClassOutboundInst: ResponseClassOutbound
    port map(
      clk=>clk,
      areset_n=>areset_n,
      enable=>enable,
      doneNoPayloadReady_i=>responseDoneNoPayloadReady,
      doneWithPayloadReady_i=>responseDoneWithPayloadReady,
      errorReady_i=>responseErrorReady,
      vc_i=>responseVc,
      crf_i=>responseCrf,
      prio_i=>responsePrio,
      tt_i=>responseTt,
      dstid_i=>responseDstId,
      srcid_i=>responseSrcId,
      tid_i=>responseTid,
      payloadLength_i=>requestLength,
      payloadIndex_o=>responsePayloadIndex,
      payload_i=>responsePayload,
      done_o=>responseDone,
      outboundStb_o=>outboundStb(0),
      outboundAdr_o=>outboundAdr(0),
      outboundDat_o=>outboundDat(31 downto 0),
      outboundStall_i=>outboundStall(0));

  -----------------------------------------------------------------------------
  -- Common interface toward the packet queues.
  -----------------------------------------------------------------------------
  
  inboundStall <= requestStall or writeStall or maintStall;
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
      inboundStb_o=>inboundStb,
      inboundAdr_o=>inboundAdr,
      inboundDat_o=>inboundDat,
      inboundStall_i=>inboundStall,
      outboundStb_i=>outboundStb,
      outboundAdr_i=>outboundAdr,
      outboundDat_i=>outboundDat,
      outboundStall_o=>outboundStall);

  -----------------------------------------------------------------------------
  -- Configuration memory.
  -----------------------------------------------------------------------------
  MaintenanceBridge: RioLogicalMaintenance
    port map(
      clk=>clk, areset_n=>areset_n, enable=>enable, 
      readRequestReady_i=>maintReadRequestReady, 
      writeRequestReady_i=>maintWriteRequestReady,
      size_i=>maintSizeInbound,
      offset_i=>maintOffsetInbound, 
      wdptr_i=>maintWdptrInbound, 
      payloadLength_i=>maintPayloadLengthInbound, 
      payloadIndex_o=>maintPayloadIndexInbound, 
      payload_i=>maintPayloadInbound, 
      done_o=>maintDoneInbound,
      readResponseReady_o=>maintReadResponseReady, 
      writeResponseReady_o=>maintWriteResponseReady,
      status_o=>maintStatusOutbound, 
      payloadLength_o=>maintPayloadLengthOutbound, 
      payloadIndex_i=>maintPayloadIndexOutbound, 
      payload_o=>maintPayloadOutbound, 
      done_i=>maintDoneOutbound, 
      configStb_o=>configStb, 
      configWe_o=>configWe, 
      configAdr_o=>configAdr, 
      configDat_o=>configDatWrite, 
      configDat_i=>configDatRead, 
      configAck_i=>configAck);

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
