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
-- RioWbBridge.
-------------------------------------------------------------------------------
-- REMARK: Add support for EXTENDED_ADDRESS...
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
      
      slaveCyc_i : in std_logic;
      slaveStb_i : in std_logic;
      slaveAdr_i : in std_logic_vector(7 downto 0);
      slaveDat_i : in std_logic_vector(31 downto 0);
      slaveAck_o : out std_logic);
  end component;
  
  component WriteClassInbound is
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
      payloadRead_i : in std_logic;
      payloadIndex_i : in std_logic_vector(4 downto 0);
      payload_o : out std_logic_vector(63 downto 0);
      done_i : in std_logic;
      
      slaveCyc_i : in std_logic;
      slaveStb_i : in std_logic;
      slaveAdr_i : in std_logic_vector(7 downto 0);
      slaveDat_i : in std_logic_vector(31 downto 0);
      slaveAck_o : out std_logic);
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
      
      masterCyc_o : out std_logic;
      masterStb_o : out std_logic;
      masterDat_o : out std_logic_vector(31 downto 0);
      masterAck_i : in std_logic);
  end component;

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
      slaveAck_o : out std_logic);
  end component;

  type StateType is (IDLE,
                     REQUEST_CLASS, REQUEST_CLASS_RESPONSE,
                     WRITE_CLASS, WRITE_CLASS_RESPONSE);
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

  signal inboundCyc : std_logic;
  signal inboundStb : std_logic;
  signal inboundAdr : std_logic_vector(7 downto 0);
  signal inboundDat : std_logic_vector(31 downto 0);
  signal inboundAck : std_logic;

  signal outboundCyc : std_logic;
  signal outboundStb : std_logic;
  signal outboundDat : std_logic_vector(31 downto 0);
  signal outboundAck : std_logic;
  
--  signal configStb : std_logic;
--  signal configWe : std_logic;
--  signal configAdr : std_logic_vector(23 downto 0);
--  signal configDatWrite : std_logic_vector(31 downto 0);
--  signal configDatRead : std_logic_vector(31 downto 0);
--  signal configAck : std_logic;
  
--  signal componentTag : std_logic_vector(31 downto 0);
--  signal baseDeviceId : std_logic_vector(15 downto 0) := DEFAULT_BASE_DEVICE_ID;
--  signal hostBaseDeviceIdLocked : std_logic;
--  signal hostBaseDeviceId : std_logic_vector(15 downto 0) := (others => '1');

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
              cyc_o <= '1';
              stb_o <= '1';
              we_o <= '1';
              adr <= writeAddress;
              sel_o <= writeSelect;
              datOut <= writePayload;

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
            if (ack_i = '1') then
              payloadUpdate <= '1';

              adr <= std_logic_vector(unsigned(adr) + 1);
              
              if (payloadIndex /= writeLength) then
                datOut <= writePayload;
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
            if (writeDone = '1') then
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
  -- 
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
      slaveCyc_i=>inboundCyc,
      slaveStb_i=>inboundStb,
      slaveAdr_i=>inboundAdr,
      slaveDat_i=>inboundDat,
      slaveAck_o=>requestAck);

  WriteClassInboundInst: WriteClassInbound
    generic map(
      EXTENDED_ADDRESS=>EXTENDED_ADDRESS)
    port map(
      clk=>clk, 
      areset_n=>areset_n, 
      enable=>enable, 
      ready_o=>writeReady, 
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
      slaveCyc_i=>inboundCyc, 
      slaveStb_i=>inboundStb, 
      slaveAdr_i=>inboundAdr, 
      slaveDat_i=>inboundDat, 
      slaveAck_o=>writeAck);

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
      masterCyc_o=>outboundCyc,
      masterStb_o=>outboundStb,
      masterDat_o=>outboundDat,
      masterAck_i=>outboundAck);

  inboundAck <= requestAck or writeAck;
  RioLogicalCommonInst: RioLogicalCommon
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
      masterCyc_o=>inboundCyc,
      masterStb_o=>inboundStb,
      masterAdr_o=>inboundAdr,
      masterDat_o=>inboundDat,
      masterAck_i=>inboundAck,
      slaveCyc_i=>outboundCyc,
      slaveStb_i=>outboundStb,
      slaveDat_i=>outboundDat,
      slaveAck_o=>outboundAck);

  -----------------------------------------------------------------------------
  -- Configuration memory.
  -----------------------------------------------------------------------------
--  memoryConfig : process(clk, areset_n)
--  begin
--    if (areset_n = '0') then
--      configDataRead <= (others => '0');
--      baseDeviceId <= DEFAULT_BASE_DEVICE_ID;
--      componentTag <= (others => '0');
--      hostBaseDeviceIdLocked <= '0';
--      hostBaseDeviceId <= (others => '1');
--    elsif (clk'event and clk = '1') then

--      if (configEnable = '1') then
--        case (configAddress) is
--          when x"000000" =>
--            -- Device Identity CAR. Read-only.
--            configDataRead(31 downto 16) <= DEVICE_IDENTITY;
--            configDataRead(15 downto 0) <= DEVICE_VENDOR_IDENTITY;
--          when x"000004" =>
--            -- Device Information CAR. Read-only.
--            configDataRead(31 downto 0) <= DEVICE_REV;
--          when x"000008" =>
--            -- Assembly Identity CAR. Read-only.
--            configDataRead(31 downto 16) <= ASSY_IDENTITY;
--            configDataRead(15 downto 0) <= ASSY_VENDOR_IDENTITY;
--          when x"00000c" =>
--            -- Assembly Informaiton CAR. Read-only.
--            -- Extended features pointer to "0000".
--            configDataRead(31 downto 16) <= ASSY_REV;
--            configDataRead(15 downto 0) <= x"0000";
--          when x"000010" =>
--            -- Processing Element Features CAR. Read-only.
--            -- Bridge(31), Memory(30), Processor(29), Switch(28).
--            configDataRead(31) <= '1';
--            configDataRead(30 downto 4) <= (others => '0');
--            configDataRead(3) <= '1';            -- support 16 bits common transport large system
--            configDataRead(2 downto 0) <= "001"; -- support 34 bits address
--          when x"000018" =>
--            -- Source Operations CAR. Read-only.
--            configDataRead(31 downto 0) <= (others => '0');
--          when x"00001C" =>
--            -- Destination Operations CAR. Read-only.
--            configDataRead(31 downto 16) <= (others => '0');
--            configDataRead(15) <= '1';
--            configDataRead(14) <= '1';
--            configDataRead(13 downto 0) <= (others => '0');
--          when x"00004C" =>
--            -- Processing Element Logical Layer Control CSR.
--            configDataRead(31 downto 3) <= (others => '0');
--            configDataRead(2 downto 0) <= "001"; -- support 34 bits address
--          when x"000060" =>
--            -- Base Device ID CSR.
--            -- Only valid for end point devices.
--            if (configWrite = '1') then
--              baseDeviceId <= configDataWrite(15 downto 0);
--            else
--              configDataRead(15 downto 0) <= baseDeviceId;
--            end if;
--          when x"000068" =>
--            -- Host Base Device ID Lock CSR.
--            if (configWrite = '1') then
--              -- Check if this field has been written before.
--              if (hostBaseDeviceIdLocked = '0') then
--                -- The field has not been written.
--                -- Lock the field and set the host base device id.
--                hostBaseDeviceIdLocked <= '1';
--                hostBaseDeviceId <= configDataWrite(15 downto 0);
--              else
--                -- The field has been written.
--                -- Check if the written data is the same as the stored.
--                if (hostBaseDeviceId = configDataWrite(15 downto 0)) then
--                  -- Same as stored, reset the value to its initial value.
--                  hostBaseDeviceIdLocked <= '0';
--                  hostBaseDeviceId <= (others => '1');
--                else
--                  -- Not writing the same as the stored value.
--                  -- Ignore the write.
--                end if;
--              end if;
--            else
--              configDataRead(31 downto 16) <= (others => '0');
--              configDataRead(15 downto 0) <= hostBaseDeviceId;
--            end if;
--          when x"00006C" =>
--            -- Component TAG CSR.
--            if (configWrite = '1') then
--              componentTag <= configDataWrite;
--            else
--              configDataRead <= componentTag;
--            end if;

--          when others =>
--            configDataRead <= (others => '0');
--        end case;
--      else
--        -- Config memory not enabled.
--      end if;
--    end if;
--  end process;

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
    
    slaveCyc_i : in std_logic;
    slaveStb_i : in std_logic;
    slaveAdr_i : in std_logic_vector(7 downto 0);
    slaveDat_i : in std_logic_vector(31 downto 0);
    slaveAck_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RequestClassInbound of RequestClassInbound is
  type StateType is (RECEIVE_PACKET, READY);
  signal state : StateType;

  signal rdsize : std_logic_vector(3 downto 0);
  signal wdptr : std_logic;

  signal slaveAck : std_logic;
  signal complete : std_logic;

  signal packetIndex : natural range 0 to 68;

begin

  slaveAck_o <= slaveAck;

  ready_o <= complete when (state = READY) else '0';

  RequestClass: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      state <= RECEIVE_PACKET;
      
      rdsize <= (others=>'0');
      wdptr <= '0';
      
      slaveAck <= '0';
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
          if (slaveCyc_i = '1') then
            if (slaveAck = '0') then
              if (slaveStb_i = '1') then
                if (slaveAdr_i = x"24") then
                  -------------------------------------------------------------
                  -- NREAD packet parser.
                  -------------------------------------------------------------
                  case (packetIndex) is
                    when 0 =>
                      -- x"0000" & ackid & vc & crf & prio & tt & ftype
                      vc_o <= slaveDat_i(9);
                      crf_o <= slaveDat_i(8);
                      prio_o <= slaveDat_i(7 downto 6);
                      tt_o <= slaveDat_i(5 downto 4);
                      packetIndex <= packetIndex + 1;
                    when 1 =>
                      -- dstid
                      dstId_o <= slaveDat_i;
                      packetIndex <= packetIndex + 1;
                    when 2 =>
                      -- srcid
                      srcId_o <= slaveDat_i;
                      packetIndex <= packetIndex + 1;
                    when 3 =>
                      -- transaction(3:0) & rdsize(3:0) & srcTID(7:0) & address(28:13)
                      -- REMARK: Add support for extended addresses here...
                      rdsize <= slaveDat_i(27 downto 24);
                      tid_o <= slaveDat_i(23 downto 16);
                      address_o(28 downto 13) <= slaveDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 4 =>
                      -- address(12:0) & wdptr & xamsbs(1:0) & crc(15:0)
                      address_o(12 downto 0) <= slaveDat_i(31 downto 19);
                      wdptr <= slaveDat_i(18);
                      address_o(30 downto 29) <= slaveDat_i(17 downto 16);
                      packetIndex <= packetIndex + 1;
                      complete <= '1';
                    when others =>
                      -- There should be no more content in an NREAD.
                      -- Discard.
                  end case;
                  slaveAck <= '1';
                end if;
              end if;
            else
              slaveAck <= '0';
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
-- REMARK: Support inbound data with full bandwidth, not just half...
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
    
    slaveCyc_i : in std_logic;
    slaveStb_i : in std_logic;
    slaveAdr_i : in std_logic_vector(7 downto 0);
    slaveDat_i : in std_logic_vector(31 downto 0);
    slaveAck_o : out std_logic);
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

  signal slaveAck : std_logic;
  signal complete : std_logic;

  signal packetIndex : natural range 0 to 68;

  signal doubleWord : std_logic_vector(63 downto 16);
  
  signal memoryWrite : std_logic;
  signal memoryAddress : std_logic_vector(4 downto 0);
  signal memoryDataIn : std_logic_vector(63 downto 0);

  
begin

  slaveAck_o <= slaveAck;

  ready_o <= complete when (state = READY) else '0';

  WriteClass: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      state <= RECEIVE_PACKET;

      wdptr <= '0';
      wrsize <= (others=>'0');
      
      slaveAck <= '0';
      complete <= '0';
      
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
          ---------------------------------------------------------------------
          -- This state waits for a new WRITE class packet, receives it
          -- and parses it.
          ---------------------------------------------------------------------
          if (slaveCyc_i = '1') then
            if (slaveAck = '0') then
              if (slaveStb_i = '1') then
                if (slaveAdr_i = x"55") then
                  -------------------------------------------------------------
                  -- NWRITER packet parser.
                  -------------------------------------------------------------
                  -- REMARK: Add support for NWRITE without response...
                  case (packetIndex) is
                    when 0 =>
                      -- x"0000" & ackid & vc & crf & prio & tt & ftype
                      vc_o <= slaveDat_i(9);
                      crf_o <= slaveDat_i(8);
                      prio_o <= slaveDat_i(7 downto 6);
                      tt_o <= slaveDat_i(5 downto 4);
                      packetIndex <= packetIndex + 1;
                    when 1 =>
                      -- destId
                      dstId_o <= slaveDat_i;
                      packetIndex <= packetIndex + 1;
                    when 2 =>
                      -- srcId
                      srcId_o <= slaveDat_i;
                      packetIndex <= packetIndex + 1;
                    when 3 =>
                      -- transaction & wrsize & srcTID & address(28:13)
                      -- REMARK: Add support for extended addresses here...
                      wrsize <= slaveDat_i(27 downto 24);
                      tid_o <= slaveDat_i(23 downto 16);
                      address_o(28 downto 13) <= slaveDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 4 =>
                      -- address(12:0) & wdptr & xamsbs(1:0) & double-word(63:48)
                      address_o(12 downto 0) <= slaveDat_i(31 downto 19);
                      wdptr <= slaveDat_i(18);
                      doubleWord(63 downto 48) <= slaveDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 | 21 | 23 | 25 | 27 | 29 | 31  | 33 | 35 |
                      37 | 39 | 41 | 43 | 45 | 47 | 49 | 51 | 53 | 55 | 57 | 59 | 61 | 63 | 65 | 67 =>
                      -- double-word(47:16)
                      doubleWord(31 downto 16) <= slaveDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 | 32  | 34 |
                      36 | 38 | 40 | 42 | 44 | 46 | 48 | 50 | 52 | 54 | 56 | 58 | 60 | 62 | 64 | 66 =>
                      -- double-word(15:0) & double-word(63:48)
                      doubleWord(63 downto 48) <= slaveDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;

                      memoryWrite <= '1';
                      memoryDataIn <= doubleWord(63 downto 16) & slaveDat_i(31 downto 16);
                      complete <= '1';
                    when others =>
                      -- There should be no more content in an NWRITE request.
                      -- Discard.
                  end case;
                  slaveAck <= '1';
                end if;
              end if;
            else
              if (memoryWrite = '1') then
                memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
              end if;
              
              memoryWrite <= '0';
              slaveAck <= '0';
            end if;
          else
            if (complete = '1') then
              state <= READY;
            end if;
            packetIndex <= 0;
            memoryAddress <= (others=>'0');
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
              length_o <= memoryAddress;
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
              length_o <= memoryAddress;
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
    
    masterCyc_o : out std_logic;
    masterStb_o : out std_logic;
    masterDat_o : out std_logic_vector(31 downto 0);
    masterAck_i : in std_logic);
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
      
      masterCyc_o <= '0';
      masterStb_o <= '0';
      masterDat_o <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        case state is
          when WAIT_PACKET =>
            -------------------------------------------------------------------
            -- 
            -------------------------------------------------------------------
            if (ready_i = '1') then
              masterCyc_o <= '1';
              masterStb_o <= '1';
              masterDat_o <= header;
              
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
            if (masterAck_i = '1') then
              case (packetIndex) is
                when 1 =>
                  -- destination
                  masterDat_o <= dstId_i;
                  packetIndex <= packetIndex + 1;
                when 2 =>
                  -- source 
                  masterDat_o <= srcId_i;
                  packetIndex <= packetIndex + 1;
                when 3 =>
                  -- transaction & status & targetTID & double-word0(63:48)
                  if (error_i = '0') then
                    if (payloadPresent_i = '0') then
                      masterDat_o <= "0000" & "0000" & tid_i & x"0000";
                      state <= WAIT_COMPLETE;
                    else
                      masterDat_o <= "1000" & "0000" & tid_i & memoryDataRead(63 downto 48);
                    end if;
                  else
                    masterDat_o <= "0000" & "0111" & tid_i & x"0000";
                    state <= WAIT_COMPLETE;
                  end if;
                  packetIndex <= packetIndex + 1;
                when 4 | 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 | 32 | 34 |
                  36 | 38 | 40 | 42 | 44 | 46 | 48 | 50 | 52 | 54 | 56 | 58 | 60 | 62 | 64 | 66 =>
                  -- double-wordN(47:16)
                  masterDat_o <= memoryDataRead(47 downto 16);
                  responsePayload <= memoryDataRead(15 downto 0);
                  memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
                  packetIndex <= packetIndex + 1;
                when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 | 21 | 23 | 25 | 27 | 29 | 31 | 33 | 35 |
                  37 | 39 | 41 | 43 | 45 | 47 | 49 | 51 | 53 | 55 | 57 | 59 | 61 | 63 | 65 | 67 =>
                  -- double-wordN(15:0) & double-wordN(63:48)
                  masterDat_o <= responsePayload & memoryDataRead(63 downto 48);
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
            if (masterAck_i = '1') then
              masterCyc_o <= '0';
              masterStb_o <= '0';
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
