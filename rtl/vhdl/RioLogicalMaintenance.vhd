-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Contains a platform to build endpoints on.
-- 
-- To Do:
-- REMARK: Dont set complete before the packet is ready in inbound packet
-- handler.
-- REMARK: Add error indication if erronous sizes are received.
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
-- RioLogicalMaintenance
-- This logical layer module handles ingress maintenance requests and converts
-- them into accesses on a Wishbone compatible bus accessing the configuration
-- space.
-- Addresses: 0x80 (maint read request) and 0x81 (maint write request).
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- Entity for RioLogicalMaintenance.
-------------------------------------------------------------------------------
entity RioLogicalMaintenance is
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    configStb_o : out std_logic;
    configWe_o : out std_logic;
    configAdr_o : out std_logic_vector(21 downto 0);
    configDat_o : out std_logic_vector(31 downto 0);
    configDat_i : in std_logic_vector(31 downto 0);
    configAck_i : in std_logic;
    
    inboundCyc_i : in std_logic;
    inboundStb_i : in std_logic;
    inboundAdr_i : in std_logic_vector(7 downto 0);
    inboundDat_i : in std_logic_vector(31 downto 0);
    inboundAck_o : out std_logic;

    outboundCyc_o : out std_logic;
    outboundStb_o : out std_logic;
    outboundDat_o : out std_logic_vector(31 downto 0);
    outboundAck_i : in std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioLogicalMaintenance of RioLogicalMaintenance is

  component MaintenanceRequestInbound is
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      requestReadReady_o : out std_logic;
      requestWriteReady_o : out std_logic;
      requestVc_o : out std_logic;
      requestCrf_o : out std_logic;
      requestPrio_o : out std_logic_vector(1 downto 0);
      requestTt_o : out std_logic_vector(1 downto 0);
      requestDstId_o : out std_logic_vector(31 downto 0);
      requestSrcId_o : out std_logic_vector(31 downto 0);
      requestTid_o : out std_logic_vector(7 downto 0);
      requestOffset_o : out std_logic_vector(20 downto 0);
      requestWdptr_o : out std_logic;
      requestPayloadLength_o : out std_logic_vector(3 downto 0);
      requestPayloadIndex_i : in std_logic_vector(3 downto 0);
      requestPayload_o : out std_logic_vector(31 downto 0);
      requestDone_i : in std_logic;
      
      inboundCyc_i : in std_logic;
      inboundStb_i : in std_logic;
      inboundAdr_i : in std_logic_vector(7 downto 0);
      inboundDat_i : in std_logic_vector(31 downto 0);
      inboundAck_o : out std_logic);
  end component;

  component MaintenanceResponseOutbound is
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      responseReadReady_i : in std_logic;
      responseWriteReady_i : in std_logic;
      responseVc_i : in std_logic;
      responseCrf_i : in std_logic;
      responsePrio_i : in std_logic_vector(1 downto 0);
      responseTt_i : in std_logic_vector(1 downto 0);
      responseDstId_i : in std_logic_vector(31 downto 0);
      responseSrcId_i : in std_logic_vector(31 downto 0);
      responseTid_i : in std_logic_vector(7 downto 0);
      responseWdptr_i : in std_logic;
      responsePayloadLength_i : in std_logic_vector(3 downto 0);
      responsePayloadWrite_i : in std_logic;
      responsePayloadIndex_i : in std_logic_vector(3 downto 0);
      responsePayload_i : in std_logic_vector(31 downto 0);
      responseDone_o : out std_logic;
      
      outboundCyc_o : out std_logic;
      outboundStb_o : out std_logic;
      outboundDat_o : out std_logic_vector(31 downto 0);
      outboundAck_i : in std_logic);
  end component;

  type StateType is (IDLE,
                     CONFIG_READ, CONFIG_READ_RESPONSE,
                     CONFIG_WRITE, CONFIG_WRITE_RESPONSE);
  signal state : StateType;

  signal vc : std_logic;
  signal crf : std_logic;
  signal prio : std_logic_vector(1 downto 0);
  signal tt : std_logic_vector(1 downto 0);
  signal dstId : std_logic_vector(31 downto 0);
  signal srcId : std_logic_vector(31 downto 0);
  signal tid : std_logic_vector(7 downto 0);
  signal wdptr : std_logic;

  signal configAdr : std_logic_vector(21 downto 0);
  signal configDat : std_logic_vector(31 downto 0);
  
  signal requestReadReady : std_logic;
  signal requestWriteReady : std_logic;
  signal requestOffset : std_logic_vector(20 downto 0);
  signal requestPayloadLength : std_logic_vector(3 downto 0);
  signal requestPayloadIndex : std_logic_vector(3 downto 0);
  signal requestPayload : std_logic_vector(31 downto 0);
  signal requestDone : std_logic;
  
  signal responseReadReady : std_logic;
  signal responseWriteReady : std_logic;
  signal responsePayloadWrite : std_logic;
  signal responsePayloadIndex : std_logic_vector(3 downto 0);
  signal responseDone : std_logic;
  
begin

  configAdr_o <= configAdr;
  configDat_o <= configDat;
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  Maintenance: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      configStb_o <= '0';
      configWe_o <= '0';
      configAdr <= (others=>'0');
      configDat <= (others=>'0');

      responseReadReady <= '0';
      responseWriteReady <= '0';
      responsePayloadWrite <= '0';

      requestDone <= '0';
      
      requestPayloadIndex <= (others=>'0');
    elsif (clk'event and clk = '1') then
      requestDone <= '0';
      responsePayloadWrite <= '0';

      if (responsePayloadWrite = '1') then
        responsePayloadIndex <= std_logic_vector(unsigned(responsePayloadIndex) + 1);
      end if;
      
      case state is
        when IDLE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          responsePayloadIndex <= (others=>'0');
          if (requestReadReady = '1') then
            configStb_o <= '1';
            configWe_o <= '0';
            configAdr <= requestOffset & wdptr;
            state <= CONFIG_READ;
          elsif (requestWriteReady = '1') then
            configStb_o <= '1';
            configWe_o <= '1';
            configAdr <= requestOffset & wdptr;
            configDat <= requestPayload;
            requestPayloadIndex <= std_logic_vector(unsigned(requestPayloadIndex) + 1);
            state <= CONFIG_WRITE;
          else
            responsePayloadIndex <= (others=>'0');
            requestPayloadIndex <= (others=>'0');
          end if;          

        when CONFIG_READ =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (configAck_i = '1') then
            responsePayloadWrite <= '1';
            
            if (responsePayloadIndex /= requestPayloadLength) then
              configAdr <= std_logic_vector(unsigned(configAdr) + 1);
            else
              requestDone <= '1';
              configStb_o <= '0';
              state <= CONFIG_READ_RESPONSE;
            end if;
          end if;

        when CONFIG_READ_RESPONSE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (responseDone = '1') then
            responseReadReady <= '0';
            state <= IDLE;
          else
            responseReadReady <= '1';
          end if;

        when CONFIG_WRITE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (configAck_i = '1') then
            responsePayloadWrite <= '1';

            if (responsePayloadIndex /= requestPayloadLength) then
              configAdr <= std_logic_vector(unsigned(configAdr) + 1);
              configDat <= requestPayload;
              requestPayloadIndex <= std_logic_vector(unsigned(requestPayloadIndex) + 1);
            else
              requestDone <= '1';
              configStb_o <= '0';
              state <= CONFIG_WRITE_RESPONSE;
            end if;
          end if;

        when CONFIG_WRITE_RESPONSE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (responseDone = '1') then
            responseWriteReady <= '0';
            state <= IDLE;
          else
            responseWriteReady <= '1';
          end if;

        when others =>

      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Request packet handler.
  -----------------------------------------------------------------------------
  RequestInbound: MaintenanceRequestInbound
    port map(
      clk=>clk, 
      areset_n=>areset_n, 
      enable=>enable, 
      requestReadReady_o=>requestReadReady, 
      requestWriteReady_o=>requestWriteReady, 
      requestVc_o=>vc, 
      requestCrf_o=>crf, 
      requestPrio_o=>prio, 
      requestTt_o=>tt, 
      requestDstId_o=>dstId, 
      requestSrcId_o=>srcId, 
      requestTid_o=>tid, 
      requestOffset_o=>requestOffset,
      requestWdptr_o=>wdptr,
      requestPayloadLength_o=>requestPayloadLength, 
      requestPayloadIndex_i=>requestPayloadIndex, 
      requestPayload_o=>requestPayload, 
      requestDone_i=>requestDone, 
      inboundCyc_i=>inboundCyc_i, 
      inboundStb_i=>inboundStb_i, 
      inboundAdr_i=>inboundAdr_i, 
      inboundDat_i=>inboundDat_i, 
      inboundAck_o=>inboundAck_o);

  -----------------------------------------------------------------------------
  -- Response packet handler.
  -----------------------------------------------------------------------------
  -- Note that the dstId and srcId is flipped since the response should be
  -- returned to the source.
  ResponseOutbound: MaintenanceResponseOutbound
    port map(
      clk=>clk, areset_n=>areset_n, enable=>enable, 
      responseReadReady_i=>responseReadReady, 
      responseWriteReady_i=>responseWriteReady, 
      responseVc_i=>vc, 
      responseCrf_i=>crf, 
      responsePrio_i=>prio, 
      responseTt_i=>tt, 
      responseDstId_i=>srcId, 
      responseSrcId_i=>dstId, 
      responseTid_i=>tid,
      responseWdptr_i=>wdptr,
      responsePayloadLength_i=>requestPayloadLength, 
      responsePayloadWrite_i=>responsePayloadWrite, 
      responsePayloadIndex_i=>responsePayloadIndex, 
      responsePayload_i=>configDat_i, 
      responseDone_o=>responseDone, 
      outboundCyc_o=>outboundCyc_o, 
      outboundStb_o=>outboundStb_o, 
      outboundDat_o=>outboundDat_o, 
      outboundAck_i=>outboundAck_i);

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
entity MaintenanceRequestInbound is
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    requestReadReady_o : out std_logic;
    requestWriteReady_o : out std_logic;
    requestVc_o : out std_logic;
    requestCrf_o : out std_logic;
    requestPrio_o : out std_logic_vector(1 downto 0);
    requestTt_o : out std_logic_vector(1 downto 0);
    requestDstId_o : out std_logic_vector(31 downto 0);
    requestSrcId_o : out std_logic_vector(31 downto 0);
    requestTid_o : out std_logic_vector(7 downto 0);
    requestOffset_o : out std_logic_vector(20 downto 0);
    requestWdptr_o : out std_logic;
    requestPayloadLength_o : out std_logic_vector(3 downto 0);
    requestPayloadIndex_i : in std_logic_vector(3 downto 0);
    requestPayload_o : out std_logic_vector(31 downto 0);
    requestDone_i : in std_logic;
    
    inboundCyc_i : in std_logic;
    inboundStb_i : in std_logic;
    inboundAdr_i : in std_logic_vector(7 downto 0);
    inboundDat_i : in std_logic_vector(31 downto 0);
    inboundAck_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture MaintenanceRequestInbound of MaintenanceRequestInbound is
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
  signal size : std_logic_vector(3 downto 0);
  signal words : natural range 0 to 32;

  signal inboundAck : std_logic;
  signal maintReadComplete : std_logic;
  signal maintWriteComplete : std_logic;

  signal packetIndex : natural range 0 to 33;
  signal requestData : std_logic_vector(31 downto 0);

  signal memoryWrite : std_logic;
  signal memoryAddress : std_logic_vector(3 downto 0);
  signal memoryDataIn : std_logic_vector(31 downto 0);

begin

  inboundAck_o <= inboundAck;

  requestReadReady_o <= maintReadComplete when (state = READY) else '0';
  requestWriteReady_o <= maintWriteComplete when (state = READY) else '0';

  MaintenanceRequest: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      inboundAck <= '0';

      maintReadComplete <= '0';
      maintWriteComplete <= '0';
      
      requestVc_o <= '0';
      requestCrf_o <= '0';
      requestPrio_o <= "00";
      requestTt_o <= "00";
      requestOffset_o <= (others=>'0');

      wdptr <= '0';
      size <= (others=>'0');
      
      packetIndex <= 0;
      memoryWrite <= '0';
      memoryAddress <= (others=>'0');
      memoryDataIn <= (others=>'0');
    elsif (clk'event and clk = '1') then
      case state is
        when RECEIVE_PACKET =>
          ---------------------------------------------------------------------
          -- This state waits for a new maintenance request packet, receives it
          -- and parses it.
          ---------------------------------------------------------------------
          if (inboundCyc_i = '1') then
            if (inboundAck = '0') then
              if (inboundStb_i = '1') then
                if (inboundAdr_i = x"80") then
                  -------------------------------------------------------------
                  -- Maintenance Read Request packet parser.
                  -------------------------------------------------------------
                  case (packetIndex) is
                    when 0 =>
                      -- x"0000" & ackid & vc & crf & prio & tt & ftype
                      requestVc_o <= inboundDat_i(9);
                      requestCrf_o <= inboundDat_i(8);
                      requestPrio_o <= inboundDat_i(7 downto 6);
                      requestTt_o <= inboundDat_i(5 downto 4);
                      packetIndex <= packetIndex + 1;
                    when 1 =>
                      -- destid
                      requestDstId_o <= inboundDat_i;
                      packetIndex <= packetIndex + 1;
                    when 2 =>
                      -- srcid
                      requestSrcId_o <= inboundDat_i;
                      packetIndex <= packetIndex + 1;
                    when 3 =>
                      -- transaction & rdsize & srcTID & hop & config_offset(20:13)
                      size <= inboundDat_i(27 downto 24);
                      requestTid_o <= inboundDat_i(23 downto 16);
                      requestOffset_o(20 downto 13) <= inboundDat_i(7 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 4 =>
                      -- config_offset(12:0) & wdptr & rsrv & crc(15:0)
                      requestOffset_o(12 downto 0) <= inboundDat_i(31 downto 19);
                      wdptr <= inboundDat_i(18);
                      packetIndex <= packetIndex + 1;
                      maintReadComplete <= '1';
                    when others =>
                      -- There should be no more content in a maintenance read request.
                      -- Discard.
                  end case;
                  inboundAck <= '1';
                elsif (inboundAdr_i = x"81") then
                  -------------------------------------------------------------
                  -- Maintenance Write Request packet parser.
                  -------------------------------------------------------------
                  case (packetIndex) is
                    when 0 =>
                      -- x"0000" & ackid & vc & crf & prio & tt & ftype
                      requestVc_o <= inboundDat_i(9);
                      requestCrf_o <= inboundDat_i(8);
                      requestPrio_o <= inboundDat_i(7 downto 6);
                      requestTt_o <= inboundDat_i(5 downto 4);
                      packetIndex <= packetIndex + 1;
                    when 1 =>
                      -- destId
                      requestDstId_o <= inboundDat_i;
                      packetIndex <= packetIndex + 1;
                    when 2 =>
                      -- srcId
                      requestSrcId_o <= inboundDat_i;
                      packetIndex <= packetIndex + 1;
                    when 3 =>
                      -- transaction & wrsize & srcTID & hop & config_offset(20:13)
                      size <= inboundDat_i(27 downto 24);
                      requestTid_o <= inboundDat_i(23 downto 16);
                      requestOffset_o(20 downto 13) <= inboundDat_i(7 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 4 =>
                      -- config_offset(12:0) & wdptr & rsrv & double-word(63:48)
                      requestOffset_o(12 downto 0) <= inboundDat_i(31 downto 19);
                      wdptr <= inboundDat_i(18);
                      requestData(31 downto 16) <= inboundDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 | 21 | 23 | 25 | 27 | 29 | 31 =>
                      -- double-word(47:16)
                      requestData(31 downto 16) <= inboundDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;

                      if (not ((size = "1000") and (wdptr = '1'))) then
                        memoryWrite <= '1';
                        memoryDataIn <= requestData(31 downto 16) & inboundDat_i(31 downto 16);
                      end if;
                    when 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 | 32 =>
                      -- double-word(15:0) & double-word(63:48)
                      requestData(31 downto 16) <= inboundDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;

                      memoryWrite <= '1';
                      memoryDataIn <= requestData(31 downto 16) & inboundDat_i(31 downto 16);
                      -- REMARK: Dont set complete before the packet is ready...
                      maintWriteComplete <= '1';
                    when others =>
                      -- There should be no more content in a maintenance write request.
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
            if (maintReadComplete = '1') or (maintWriteComplete = '1') then
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
          if (requestDone_i = '1') then
            maintReadComplete <= '0';
            maintWriteComplete <= '0';
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
  -- Transformation of rdsize/wrsize into length of access and byte lanes.
  -----------------------------------------------------------------------------
  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      requestPayloadLength_o <= (others=>'0');
      requestWdptr_o <= '0';
    elsif (clk'event and clk = '1') then
      if (maintReadComplete = '1') or (maintWriteComplete = '1') then
        if (wdptr = '0') then
          case size is
            when "1000" =>
              -- Read 1 word.
              requestPayloadLength_o <= "0000";
              requestWdptr_o <= '0';
            when "1011" =>
              -- Read 2 words.
              requestPayloadLength_o <= "0001";
              requestWdptr_o <= '0';
            when "1100" =>
              -- Read 8 words.
              requestPayloadLength_o <= "0111";
              requestWdptr_o <= '0';
            when others =>
              -- REMARK: Not allowed for a maintenance packet.
              requestPayloadLength_o <= "0000";
              requestWdptr_o <= '0';
          end case;
        else
          case size is
            when "1000" =>
              -- Read 1 word.
              requestPayloadLength_o <= "0000";
              requestWdptr_o <= '1';
            when "1011" =>
              -- Read 4 words.
              requestPayloadLength_o <= "0011";
              requestWdptr_o <= '0';
            when "1100" =>
              -- Read 16 words.
              requestPayloadLength_o <= "1111";
              requestWdptr_o <= '0';
            when others =>
              -- REMARK: Not allowed for a maintenance packet.
              requestPayloadLength_o <= "0000";
              requestWdptr_o <= '0';
          end case;
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Payload content memory.
  -----------------------------------------------------------------------------
  PayloadMemory: MemorySimpleDualPort
    generic map(ADDRESS_WIDTH=>4, DATA_WIDTH=>32)
    port map(clkA_i=>clk,
             enableA_i=>memoryWrite,
             addressA_i=>memoryAddress,
             dataA_i=>memoryDataIn,
             clkB_i=>clk,
             enableB_i=>enable,
             addressB_i=>requestPayloadIndex_i,
             dataB_o=>requestPayload_o);

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
entity MaintenanceResponseOutbound is
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    responseReadReady_i : in std_logic;
    responseWriteReady_i : in std_logic;
    responseVc_i : in std_logic;
    responseCrf_i : in std_logic;
    responsePrio_i : in std_logic_vector(1 downto 0);
    responseTt_i : in std_logic_vector(1 downto 0);
    responseDstId_i : in std_logic_vector(31 downto 0);
    responseSrcId_i : in std_logic_vector(31 downto 0);
    responseTid_i : in std_logic_vector(7 downto 0);
    responseWdptr_i : in std_logic;
    responsePayloadLength_i : in std_logic_vector(3 downto 0);
    responsePayloadWrite_i : in std_logic;
    responsePayloadIndex_i : in std_logic_vector(3 downto 0);
    responsePayload_i : in std_logic_vector(31 downto 0);
    responseDone_o : out std_logic;
    
    outboundCyc_o : out std_logic;
    outboundStb_o : out std_logic;
    outboundDat_o : out std_logic_vector(31 downto 0);
    outboundAck_i : in std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture MaintenanceResponseOutbound of MaintenanceResponseOutbound is
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

  type StateType is (WAIT_PACKET,
                     READ_RESPONSE, WRITE_RESPONSE,
                     WAIT_COMPLETE, RESPONSE_DONE);
  signal state : StateType;

  signal packetIndex : natural range 0 to 33;
  signal responseHeader : std_logic_vector(31 downto 0);
  signal responsePayload : std_logic_vector(31 downto 0);
  signal responsePayloadIndex : std_logic_vector(2 downto 0);
    
  signal memoryEnable : std_logic;
  signal memoryAddress : std_logic_vector(3 downto 0);
  signal memoryDataRead : std_logic_vector(31 downto 0);

begin
  
  responseHeader <=
    x"0000" & "000000" & responseVc_i & responseCrf_i &
    responsePrio_i & responseTt_i & x"8";

  MaintenanceResponse: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      outboundCyc_o <= '0';
      outboundStb_o <= '0';

      memoryEnable <= '0';
      memoryAddress <= (others=>'0');

      responsePayloadIndex <= (others=>'0');
      responseDone_o <= '0';
      
      state <= WAIT_PACKET;
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        case state is
          when WAIT_PACKET =>
            -------------------------------------------------------------------
            -- 
            -------------------------------------------------------------------
            if (responseReadReady_i = '1') then
              outboundCyc_o <= '1';
              outboundStb_o <= '1';
              outboundDat_o <= responseHeader;
              packetIndex <= 1;
              memoryEnable <= '1';
              memoryAddress <= (others=>'0');
              responsePayloadIndex <= (others=>'0');
              state <= READ_RESPONSE;
            elsif (responseWriteReady_i = '1') then
              outboundCyc_o <= '1';
              outboundStb_o <= '1';
              outboundDat_o <= responseHeader;
              packetIndex <= 1;
              state <= WRITE_RESPONSE;
            end if;

          when READ_RESPONSE =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (outboundAck_i = '1') then
              case (packetIndex) is
                when 1 =>
                  -- destination
                  outboundDat_o <= responseDstId_i;
                  packetIndex <= packetIndex + 1;
                when 2 =>
                  -- source 
                  outboundDat_o <= responseSrcId_i;
                  packetIndex <= packetIndex + 1;
                when 3 =>
                  -- transaction & status & targetTID & hop & reserved(7:0)
                  outboundDat_o <= "0010" & "0000" & responseTid_i & x"ff" & x"00";
                  packetIndex <= packetIndex + 1;
                when 4 =>
                  -- reserved(15:0) & double-wordN(63:48)
                  if (responsePayloadLength_i = "0000") and (responseWdptr_i = '0') then
                    outboundDat_o <= x"0000" & memoryDataRead(31 downto 16);
                    responsePayload(31 downto 16) <= memoryDataRead(15 downto 0);
                    memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
                  elsif (responsePayloadLength_i = "0000") and (responseWdptr_i = '1') then
                    outboundDat_o <= x"0000" & x"0000";
                  else
                    outboundDat_o <= x"0000" & memoryDataRead(31 downto 16);
                    responsePayload(31 downto 16) <= memoryDataRead(15 downto 0);
                    memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
                  end if;
                  packetIndex <= packetIndex + 1;
                when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 | 21 | 23 | 25 | 27 | 29 | 31 =>
                  -- double-wordN(47:16)
                  if (responsePayloadLength_i = "0000") and (responseWdptr_i = '0') then
                    outboundDat_o <= responsePayload(31 downto 16) & x"0000";
                  elsif (responsePayloadLength_i = "0000") and (responseWdptr_i = '1') then
                    outboundDat_o <= x"0000" & memoryDataRead(31 downto 16);
                    responsePayload(31 downto 16) <= memoryDataRead(15 downto 0);
                    memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
                  else
                    outboundDat_o <=
                      responsePayload(31 downto 16) & memoryDataRead(31 downto 16);
                    responsePayload(31 downto 16) <= memoryDataRead(15 downto 0);
                    memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
                  end if;
                  packetIndex <= packetIndex + 1;
                when 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 | 32 =>
                  -- double-wordN(15:0) & double-wordN(63:48)
                  if (responsePayloadLength_i = "0000") and (responseWdptr_i = '0') then
                    outboundDat_o <= x"0000" & x"0000";
                  elsif (responsePayloadLength_i = "0000") and (responseWdptr_i = '1') then
                    outboundDat_o <= responsePayload(31 downto 16) & x"0000";
                  else
                    if (responsePayloadIndex /= responsePayloadLength_i(3 downto 1)) then
                      outboundDat_o <=
                        responsePayload(31 downto 16) & memoryDataRead(31 downto 16);
                      responsePayload(31 downto 16) <= memoryDataRead(15 downto 0);
                      memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
                    else
                      outboundDat_o <=
                        responsePayload(31 downto 16) & x"0000";
                      responsePayload(31 downto 16) <= memoryDataRead(15 downto 0);
                      memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
                    end if;
                  end if;
                  
                  responsePayloadIndex <=
                    std_logic_vector(unsigned(responsePayloadIndex) + 1);
                  
                  if (responsePayloadIndex = responsePayloadLength_i(3 downto 1)) then
                    state <= WAIT_COMPLETE;
                  else
                    packetIndex <= packetIndex + 1;
                  end if;
                when others =>
                  -- Unallowed response length.
                  -- Dont do anything.
              end case;
            end if;
            
          when WRITE_RESPONSE =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (outboundAck_i = '1') then
              case (packetIndex) is
                when 1 =>
                  -- destination
                  outboundDat_o <= responseDstId_i;
                  packetIndex <= packetIndex + 1;
                when 2 =>
                  -- source 
                  outboundDat_o <= responseSrcId_i;
                  packetIndex <= packetIndex + 1;
                when 3 =>
                  -- transaction & status & targetTID & hop & reserved(7:0)
                  outboundDat_o <= "0011" & "0000" & responseTid_i & x"ff" & x"00";
                  packetIndex <= packetIndex + 1;
                when others =>
                  -- reserved(15:0) & crc(15:0)
                  outboundDat_o <= x"00000000";
                  packetIndex <= packetIndex + 1;
                  state <= WAIT_COMPLETE;
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
            if (responseReadReady_i = '0') and (responseWriteReady_i = '0') then
              state <= WAIT_PACKET;
              responseDone_o <= '0';
            else
              responseDone_o <= '1';
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
    generic map(ADDRESS_WIDTH=>4, DATA_WIDTH=>32)
    port map(clkA_i=>clk,
             enableA_i=>responsePayloadWrite_i,
             addressA_i=>responsePayloadIndex_i,
             dataA_i=>responsePayload_i,
             clkB_i=>clk,
             enableB_i=>memoryEnable,
             addressB_i=>memoryAddress,
             dataB_o=>memoryDataRead);

end architecture;
