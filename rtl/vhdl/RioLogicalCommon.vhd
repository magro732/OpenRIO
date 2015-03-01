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
-- RioLogicalCommon.
-------------------------------------------------------------------------------
-- Ingress:
-- * Removes in-the-middle and trailing CRC.
-- * Forwards packets to logical-layer handlers depending on ftype and
--   transaction (output as address).
-- * Outputs header and deviceIDs in seperate accesses to facilitate 8- and
--   16-bit deviceAddress support. All fields are right-justified.
-- Egress:
-- * Adds in-the-middle and trailing CRC.
-- * Receives packets from logical-layer handlers.
-- * Receives header and deviceIDs in seperate accesses to facilitate 8- and
--   16-bit deviceAddress support. All fields are right-justified.
-------------------------------------------------------------------------------
-- REMARK: Egress; Places packets in different queues depending on the packet priority?
-- REMARK: Do not use Wishbone, use request/grant scheme instead...
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity RioLogicalCommon is
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
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioLogicalCommon of RioLogicalCommon is

  component RioLogicalCommonIngress is
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      
      readFrameEmpty_i : in std_logic;
      readFrame_o : out std_logic;
      readContent_o : out std_logic;
      readContentEnd_i : in std_logic;
      readContentData_i : in std_logic_vector(31 downto 0);

      masterCyc_o : out std_logic;
      masterStb_o : out std_logic;
      masterAdr_o : out std_logic_vector(7 downto 0);
      masterDat_o : out std_logic_vector(31 downto 0);
      masterAck_i : in std_logic);
  end component;

  component RioLogicalCommonEgress is
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      writeFrameFull_i : in std_logic;
      writeFrame_o : out std_logic;
      writeFrameAbort_o : out std_logic;
      writeContent_o : out std_logic;
      writeContentData_o : out std_logic_vector(31 downto 0);

      slaveCyc_i : in std_logic;
      slaveStb_i : in std_logic;
      slaveDat_i : in std_logic_vector(31 downto 0);
      slaveAck_o : out std_logic);
  end component;

  component RioLogicalMaintenance is
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
      
      slaveCyc_i : in std_logic;
      slaveStb_i : in std_logic;
      slaveAdr_i : in std_logic_vector(7 downto 0);
      slaveDat_i : in std_logic_vector(31 downto 0);
      slaveAck_o : out std_logic;

      masterCyc_o : out std_logic;
      masterStb_o : out std_logic;
      masterDat_o : out std_logic_vector(31 downto 0);
      masterAck_i : in std_logic);
  end component;

  signal masterCyc : std_logic;
  signal masterStb : std_logic;
  signal masterAdr : std_logic_vector(7 downto 0);
  signal masterDat : std_logic_vector(31 downto 0);
  signal masterAck : std_logic;

  signal slaveCyc : std_logic;
  signal slaveStb : std_logic;
  signal slaveDat : std_logic_vector(31 downto 0);
  signal slaveAck : std_logic;

begin

  Ingress: RioLogicalCommonIngress
    port map(
      clk=>clk, areset_n=>areset_n, 
      readFrameEmpty_i=>readFrameEmpty_i, 
      readFrame_o=>readFrame_o, 
      readContent_o=>readContent_o, 
      readContentEnd_i=>readContentEnd_i, 
      readContentData_i=>readContentData_i, 
      masterCyc_o=>masterCyc, 
      masterStb_o=>masterStb, 
      masterAdr_o=>masterAdr, 
      masterDat_o=>masterDat, 
      masterAck_i=>masterAck);

  LogicalMaintenance: RioLogicalMaintenance
    port map(
      clk=>clk, areset_n=>areset_n, enable=>enable, 
      configStb_o=>configStb_o, 
      configWe_o=>configWe_o, 
      configAdr_o=>configAdr_o, 
      configDat_o=>configDat_o, 
      configDat_i=>configDat_i, 
      configAck_i=>configAck_i, 
      slaveCyc_i=>masterCyc, 
      slaveStb_i=>masterStb, 
      slaveAdr_i=>masterAdr, 
      slaveDat_i=>masterDat, 
      slaveAck_o=>masterAck, 
      masterCyc_o=>slaveCyc, 
      masterStb_o=>slaveStb, 
      masterDat_o=>slaveDat, 
      masterAck_i=>slaveAck);

  Egress: RioLogicalCommonEgress
    port map(
      clk=>clk, areset_n=>areset_n, 
      writeFrameFull_i=>writeFrameFull_i, 
      writeFrame_o=>writeFrame_o, 
      writeFrameAbort_o=>writeFrameAbort_o, 
      writeContent_o=>writeContent_o, 
      writeContentData_o=>writeContentData_o, 
      slaveCyc_i=>slaveCyc, 
      slaveStb_i=>slaveStb, 
      slaveDat_i=>slaveDat, 
      slaveAck_o=>slaveAck);

end architecture;



-------------------------------------------------------------------------------
-- RioLogicalCommonIngress.
-------------------------------------------------------------------------------
-- REMARK: Check the destination address to see if it matches the one configured???
-- REMARK: Remove the acknowledge on all accesses on the master bus.
-- REMARK: Add component declarations to riocommon.vhd.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;

-------------------------------------------------------------------------------
-- Entity for RioLogicalCommonIngress.
-------------------------------------------------------------------------------
entity RioLogicalCommonIngress is
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    
    readFrameEmpty_i : in std_logic;
    readFrame_o : out std_logic;
    readContent_o : out std_logic;
    readContentEnd_i : in std_logic;
    readContentData_i : in std_logic_vector(31 downto 0);

    masterCyc_o : out std_logic;
    masterStb_o : out std_logic;
    masterAdr_o : out std_logic_vector(7 downto 0);
    masterDat_o : out std_logic_vector(31 downto 0);
    masterAck_i : in std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioLogicalCommonIngress of RioLogicalCommonIngress is
  type StateType is (IDLE,
                     WAIT_HEADER_0, HEADER_0, HEADER_1,
                     SEND_HEADER, SEND_DESTINATION, SEND_SOURCE,
                     FORWARD_SHORT, FORWARD_CRC, FORWARD_LONG, FORWARD_LAST,
                     END_PACKET);
  signal state : StateType;

  signal packetPosition : natural range 0 to 32;
  signal packetContent : std_logic_vector(63 downto 0);

  signal tt : std_logic_vector(1 downto 0);
  signal ftype : std_logic_vector(3 downto 0);
  signal transaction : std_logic_vector(3 downto 0);
  
begin

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      masterCyc_o <= '0';
      masterStb_o <= '0';
      
      state <= IDLE;
      packetPosition <= 0;
      packetContent <= (others=>'0');
      tt <= "00";
      ftype <= "0000";
      transaction <= "0000";

      readContent_o <= '0';
      readFrame_o <= '0';
    elsif (clk'event and clk = '1') then
      readContent_o <= '0';
      readFrame_o <= '0';
      
      case state is
        when IDLE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          packetPosition <= 0;
          if (readFrameEmpty_i = '0') then
            readContent_o <= '1';
            state <= WAIT_HEADER_0;
          end if;
          
        when WAIT_HEADER_0 =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          readContent_o <= '1';
          state <= HEADER_0;

        when HEADER_0 =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          packetContent <= packetContent(31 downto 0) & readContentData_i;
          packetPosition <= packetPosition + 1;
          readContent_o <= '1';

          tt <= readContentData_i(21 downto 20);
          ftype <= readContentData_i(19 downto 16);

          state <= HEADER_1;
          
        when HEADER_1 =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          packetContent <= packetContent(31 downto 0) & readContentData_i;
          packetPosition <= packetPosition + 1;

          if (tt = "00") then
            transaction <= readContentData_i(31 downto 28);
          elsif (tt = "01") then
            transaction <= readContentData_i(15 downto 12);
          end if;
          
          state <= SEND_HEADER;

        when SEND_HEADER =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          masterCyc_o <= '1';
          masterStb_o <= '1';
          masterAdr_o <= ftype & transaction;
          masterDat_o <= x"0000" & packetContent(63 downto 48);
          packetContent <= packetContent(47 downto 0) & x"0000";

          state <= SEND_DESTINATION;

        when SEND_DESTINATION =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            if (tt = "00") then
              masterDat_o <= x"000000" & packetContent(63 downto 56);
              packetContent <= packetContent(55 downto 0) & x"00";
            elsif (tt = "01") then
              masterDat_o <= x"0000" & packetContent(63 downto 48);
              packetContent <= packetContent(47 downto 0) & x"0000";
            end if;

            state <= SEND_SOURCE;
          end if;
          
        when SEND_SOURCE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            if (tt = "00") then
              masterDat_o <= x"000000" & packetContent(63 downto 56);
              packetContent <= packetContent(55 downto 0) & x"00";
            elsif (tt = "01") then
              masterDat_o <= x"0000" & packetContent(63 downto 48);
              packetContent <= packetContent(47 downto 32) & readContentData_i & x"0000";
              readContent_o <= '1';
            end if;

            state <= FORWARD_SHORT;
          end if;
          
        when FORWARD_SHORT =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            packetPosition <= packetPosition + 1;

            if (tt = "00") then
              masterDat_o <= packetContent(63 downto 32);
              packetContent <= packetContent(31 downto 0) & readContentData_i;
            elsif (tt = "01") then
              masterDat_o <= packetContent(63 downto 32);
              packetContent <= packetContent(31 downto 16) & readContentData_i & x"0000";
            end if;
            
            if (readContentEnd_i = '0') then
              if (packetPosition = 20) then
                state <= FORWARD_CRC;
              end if;
              
              readContent_o <= '1';
            else
              readFrame_o <= '1';
              state <= END_PACKET;
            end if;
          end if;

        when FORWARD_CRC =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            masterDat_o <= packetContent(63 downto 32);

            packetPosition <= packetPosition + 1;
            packetContent <=
              packetContent(31 downto 0) & readContentData_i(15 downto 0) & x"0000";
            
            if (readContentEnd_i = '0') then
              readContent_o <= '1';
              state <= FORWARD_LONG;
            else
              readFrame_o <= '1';
              state <= FORWARD_LAST;
            end if;
          end if;

        when FORWARD_LONG =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            masterDat_o <= packetContent(63 downto 32);

            packetPosition <= packetPosition + 1;
            packetContent <=
              packetContent(15 downto 0) & readContentData_i & x"0000";
            
            if (readContentEnd_i = '0') then
              readContent_o <= '1';
            else
              readFrame_o <= '1';
              state <= FORWARD_LAST;
            end if;
          end if;

        when FORWARD_LAST =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          -- REMARK: The last always contain the CRC?
          if (masterAck_i = '1') then
            masterDat_o <= packetContent(63 downto 32);
            state <= END_PACKET;
          end if;
          
        when END_PACKET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            masterCyc_o <= '0';
            masterStb_o <= '0';
            state <= IDLE;
          end if;
          
        when others =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          state <= IDLE;
      end case;
    end if;
  end process;

end architecture;


-------------------------------------------------------------------------------
-- RioLogicalCommonEgress.
-- Only 8-bit and 16-bit deviceId are supported. The first write must contain
-- the 16-bit header, the second write must contain the destination address and
-- the third must contain the source address.
-- CRC is calculated during the transfer and is inserted at byte 81 and 82 and
-- appended to the packet when it ends.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;

-------------------------------------------------------------------------------
-- Entity for RioLogicalCommonEgress.
-------------------------------------------------------------------------------
entity RioLogicalCommonEgress is
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    writeFrameFull_i : in std_logic;
    writeFrame_o : out std_logic;
    writeFrameAbort_o : out std_logic;
    writeContent_o : out std_logic;
    writeContentData_o : out std_logic_vector(31 downto 0);

    slaveCyc_i : in std_logic;
    slaveStb_i : in std_logic;
    slaveDat_i : in std_logic_vector(31 downto 0);
    slaveAck_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioLogicalCommonEgress.
-------------------------------------------------------------------------------
architecture RioLogicalCommonEgress of RioLogicalCommonEgress is

  component Crc16CITT is
    port(
      d_i : in  std_logic_vector(15 downto 0);
      crc_i : in  std_logic_vector(15 downto 0);
      crc_o : out std_logic_vector(15 downto 0));
  end component;

  type StateType is (IDLE,
                     HEADER_GET, HEADER_ACK,
                     DESTINATION_GET, DESTINATION_ACK,
                     SOURCE_GET, SOURCE_ACK,
                     CONTENT_GET, CONTENT_ACK,
                     CRC_APPEND, SEND_FRAME,
                     RESTART_FRAME, WAIT_UPDATE);
  signal state : StateType;
  signal packetPosition : natural range 0 to 31;
  signal halfWordPending : std_logic;
  signal halfWord : std_logic_vector(15 downto 0);
  
  signal header : std_logic_vector(15 downto 0);
  signal tt : std_logic_vector(1 downto 0);
  signal dstAddr : std_logic_vector(7 downto 0);
  
  signal writeContentData : std_logic_vector(31 downto 0);

  signal crc16Current, crc16Temp, crc16Next: std_logic_vector(15 downto 0);

begin

  writeContentData_o <= writeContentData;
  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      state <= IDLE;
      crc16Current <= x"0000";

      slaveAck_o <= '0';

      halfWordPending <= '0';
      
      writeContent_o <= '0';
      writeFrame_o <= '0';
      writeFrameAbort_o <= '0';
      writeContentData <= (others=>'0');
    elsif (clk'event and clk = '1') then
      writeContent_o <= '0';
      writeFrame_o <= '0';
      
      case state is
        when IDLE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          packetPosition <= 0;
          if (writeFrameFull_i = '0') then
            crc16Current <= x"ffff";
            state <= HEADER_GET;
          end if;

        when HEADER_GET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if ((slaveCyc_i = '1') and (slaveStb_i = '1')) then
            header <= slaveDat_i(15 downto 0);
            tt <= slaveDat_i(5 downto 4);

            slaveAck_o <= '1';
            state <= HEADER_ACK;
          else
            state <= HEADER_GET;
          end if;

        when HEADER_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          slaveAck_o <= '0';
          state <= DESTINATION_GET;

        when DESTINATION_GET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          
          if ((slaveCyc_i = '1') and (slaveStb_i = '1')) then
            if (tt = "00") then
              dstAddr <= slaveDat_i(7 downto 0);
            elsif (tt = "01") then
              writeContent_o <= '1';
              writeContentData <= header & slaveDat_i(15 downto 0);
              packetPosition <= packetPosition + 1;
            else
              -- REMARK: Not supported.
            end if;
            
            slaveAck_o <= '1';
            state <= DESTINATION_ACK;
          else
            state <= RESTART_FRAME;
          end if;
        
        when DESTINATION_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          slaveAck_o <= '0';
          state <= SOURCE_GET;

          if (tt = "01") then
            crc16Current <= crc16Next;
          end if;

        when SOURCE_GET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          
          if ((slaveCyc_i = '1') and (slaveStb_i = '1')) then
            if (tt = "00") then
              halfWordPending <= '0';
              writeContent_o <= '1';
              writeContentData <= header & dstAddr & slaveDat_i(7 downto 0);
              packetPosition <= packetPosition + 1;
            elsif (tt = "01") then
              halfWordPending <= '1';
              halfWord <= slaveDat_i(15 downto 0);
            else
              -- REMARK: Not supported.
            end if;
            
            slaveAck_o <= '1';
            state <= SOURCE_ACK;
          else
            state <= RESTART_FRAME;
          end if;
        
        when SOURCE_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          slaveAck_o <= '0';
          state <= CONTENT_GET;
          
        when CONTENT_GET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if ((slaveCyc_i = '1') and (slaveStb_i = '1')) then
            if (halfWordPending = '0') then
              writeContent_o <= '1';
              writeContentData <= slaveDat_i;
              packetPosition <= packetPosition + 1;
            else
              writeContent_o <= '1';
              writeContentData <= halfWord & slaveDat_i(31 downto 16);
              packetPosition <= packetPosition + 1;
              halfWord <= slaveDat_i(15 downto 0);
            end if;
            
            slaveAck_o <= '1';
            state <= CONTENT_ACK;
          else
            state <= CRC_APPEND;
          end if;

        when CONTENT_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          slaveAck_o <= '0';
          
          crc16Current <= crc16Next;

          if (packetPosition = 20) then
            if (halfWordPending = '0') then
              halfWordPending <= '1';
              halfWord <= crc16Next;
            else
              writeContent_o <= '1';
              writeContentData <= halfWord & crc16Next;
              crc16Current <= crc16Next;
              packetPosition <= packetPosition + 1;
              halfWordPending <= '0';
              halfWord <= crc16Next;
            end if;
          end if;
          
          state <= CONTENT_GET;

        when CRC_APPEND =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (tt = "00") then
            writeContent_o <= '1';
            writeContentData <= crc16Current & x"0000";
          elsif (tt = "01") then
            writeContent_o <= '1';
            writeContentData <= crc16Current & x"0000";
          end if;

          state <= SEND_FRAME;

        when SEND_FRAME =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          writeFrame_o <= '1';
          state <= WAIT_UPDATE;

        when RESTART_FRAME =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          writeFrameAbort_o <= '1';
          state <= WAIT_UPDATE;

        when WAIT_UPDATE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          writeFrameAbort_o <= '0';
          state <= IDLE;
          
        when others =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Packet CRC calculation.
  -----------------------------------------------------------------------------

  Crc16High: Crc16CITT
    port map(
      d_i=>writeContentData(31 downto 16), crc_i=>crc16Current, crc_o=>crc16Temp);
  Crc16Low: Crc16CITT
    port map(
      d_i=>writeContentData(15 downto 0), crc_i=>crc16Temp, crc_o=>crc16Next);

end architecture;





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
    
    slaveCyc_i : in std_logic;
    slaveStb_i : in std_logic;
    slaveAdr_i : in std_logic_vector(7 downto 0);
    slaveDat_i : in std_logic_vector(31 downto 0);
    slaveAck_o : out std_logic;

    masterCyc_o : out std_logic;
    masterStb_o : out std_logic;
    masterDat_o : out std_logic_vector(31 downto 0);
    masterAck_i : in std_logic);
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
      
      slaveCyc_i : in std_logic;
      slaveStb_i : in std_logic;
      slaveAdr_i : in std_logic_vector(7 downto 0);
      slaveDat_i : in std_logic_vector(31 downto 0);
      slaveAck_o : out std_logic);
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
      
      masterCyc_o : out std_logic;
      masterStb_o : out std_logic;
      masterDat_o : out std_logic_vector(31 downto 0);
      masterAck_i : in std_logic);
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
      slaveCyc_i=>slaveCyc_i, 
      slaveStb_i=>slaveStb_i, 
      slaveAdr_i=>slaveAdr_i, 
      slaveDat_i=>slaveDat_i, 
      slaveAck_o=>slaveAck_o);

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
      masterCyc_o=>masterCyc_o, 
      masterStb_o=>masterStb_o, 
      masterDat_o=>masterDat_o, 
      masterAck_i=>masterAck_i);

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
    
    slaveCyc_i : in std_logic;
    slaveStb_i : in std_logic;
    slaveAdr_i : in std_logic_vector(7 downto 0);
    slaveDat_i : in std_logic_vector(31 downto 0);
    slaveAck_o : out std_logic);
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

  signal slaveAck : std_logic;
  signal maintReadComplete : std_logic;
  signal maintWriteComplete : std_logic;

  signal packetIndex : natural range 0 to 33;
  signal requestData : std_logic_vector(31 downto 0);

  signal memoryWrite : std_logic;
  signal memoryAddress : std_logic_vector(3 downto 0);
  signal memoryDataIn : std_logic_vector(31 downto 0);

begin

  slaveAck_o <= slaveAck;

  requestReadReady_o <= maintReadComplete when (state = READY) else '0';
  requestWriteReady_o <= maintWriteComplete when (state = READY) else '0';

  MaintenanceRequest: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      slaveAck <= '0';

      maintReadComplete <= '0';
      maintWriteComplete <= '0';
      
      requestVc_o <= '0';
      requestCrf_o <= '0';
      requestPrio_o <= "00";
      requestTt_o <= "00";
      requestOffset_o <= (others=>'0');

      wdptr <= '0';
      
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
          if (slaveCyc_i = '1') then
            if (slaveAck = '0') then
              if (slaveStb_i = '1') then
                if (slaveAdr_i = x"80") then
                  -------------------------------------------------------------
                  -- Maintenance Read Request packet parser.
                  -------------------------------------------------------------
                  case (packetIndex) is
                    when 0 =>
                      -- x"0000" & ackid & vc & crf & prio & tt & ftype
                      requestVc_o <= slaveDat_i(9);
                      requestCrf_o <= slaveDat_i(8);
                      requestPrio_o <= slaveDat_i(7 downto 6);
                      requestTt_o <= slaveDat_i(5 downto 4);
                      packetIndex <= packetIndex + 1;
                    when 1 =>
                      -- destid
                      requestDstId_o <= slaveDat_i;
                      packetIndex <= packetIndex + 1;
                    when 2 =>
                      -- srcid
                      requestSrcId_o <= slaveDat_i;
                      packetIndex <= packetIndex + 1;
                    when 3 =>
                      -- transaction & rdsize & srcTID & hop & config_offset(20:13)
                      size <= slaveDat_i(27 downto 24);
                      requestTid_o <= slaveDat_i(23 downto 16);
                      requestOffset_o(20 downto 13) <= slaveDat_i(7 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 4 =>
                      -- config_offset(12:0) & wdptr & rsrv & crc(15:0)
                      requestOffset_o(12 downto 0) <= slaveDat_i(31 downto 19);
                      wdptr <= slaveDat_i(18);
                      packetIndex <= packetIndex + 1;
                      maintReadComplete <= '1';
                    when others =>
                      -- There should be no more content in a maintenance read request.
                      -- Discard.
                  end case;
                elsif (slaveAdr_i = x"81") then
                  -------------------------------------------------------------
                  -- Maintenance Write Request packet parser.
                  -------------------------------------------------------------
                  case (packetIndex) is
                    when 0 =>
                      -- x"0000" & ackid & vc & crf & prio & tt & ftype
                      requestVc_o <= slaveDat_i(9);
                      requestCrf_o <= slaveDat_i(8);
                      requestPrio_o <= slaveDat_i(7 downto 6);
                      requestTt_o <= slaveDat_i(5 downto 4);
                      packetIndex <= packetIndex + 1;
                    when 1 =>
                      -- destId
                      requestDstId_o <= slaveDat_i;
                      packetIndex <= packetIndex + 1;
                    when 2 =>
                      -- srcId
                      requestSrcId_o <= slaveDat_i;
                      packetIndex <= packetIndex + 1;
                    when 3 =>
                      -- transaction & wrsize & srcTID & hop & config_offset(20:13)
                      size <= slaveDat_i(27 downto 24);
                      requestTid_o <= slaveDat_i(23 downto 16);
                      requestOffset_o(20 downto 13) <= slaveDat_i(7 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 4 =>
                      -- config_offset(12:0) & wdptr & rsrv & double-word(63:48)
                      requestOffset_o(12 downto 0) <= slaveDat_i(31 downto 19);
                      wdptr <= slaveDat_i(18);
                      requestData(31 downto 16) <= slaveDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;
                    when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 | 21 | 23 | 25 | 27 | 29 | 31 =>
                      -- double-word(47:16)
                      requestData(31 downto 16) <= slaveDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;

                      if (not ((size = "1000") and (wdptr = '1'))) then
                        memoryWrite <= '1';
                        memoryDataIn <= requestData(31 downto 16) & slaveDat_i(31 downto 16);
                      end if;
                    when 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 | 32 =>
                      -- double-word(15:0) & double-word(63:48)
                      requestData(31 downto 16) <= slaveDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;

                      memoryWrite <= '1';
                      memoryDataIn <= requestData(31 downto 16) & slaveDat_i(31 downto 16);
                      maintWriteComplete <= '1';
                    when others =>
                      -- There should be no more content in a maintenance write request.
                      -- Discard.
                  end case;
                end if;
                slaveAck <= '1';
              end if;
            else
              if (memoryWrite = '1') then
                memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
              end if;
              
              memoryWrite <= '0';
              slaveAck <= '0';
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
-- REMARK: Add handler for maintenance response with error also...
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
    
    masterCyc_o : out std_logic;
    masterStb_o : out std_logic;
    masterDat_o : out std_logic_vector(31 downto 0);
    masterAck_i : in std_logic);
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
      masterCyc_o <= '0';
      masterStb_o <= '0';

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
              masterCyc_o <= '1';
              masterStb_o <= '1';
              masterDat_o <= responseHeader;
              packetIndex <= 1;
              memoryEnable <= '1';
              memoryAddress <= (others=>'0');
              responsePayloadIndex <= (others=>'0');
              state <= READ_RESPONSE;
            elsif (responseWriteReady_i = '1') then
              masterCyc_o <= '1';
              masterStb_o <= '1';
              masterDat_o <= responseHeader;
              packetIndex <= 1;
              state <= WRITE_RESPONSE;
            end if;

          when READ_RESPONSE =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (masterAck_i = '1') then
              case (packetIndex) is
                when 1 =>
                  -- destination
                  masterDat_o <= responseDstId_i;
                  packetIndex <= packetIndex + 1;
                when 2 =>
                  -- source 
                  masterDat_o <= responseSrcId_i;
                  packetIndex <= packetIndex + 1;
                when 3 =>
                  -- transaction & status & targetTID & hop & reserved(7:0)
                  masterDat_o <= "0010" & "0000" & responseTid_i & x"ff" & x"00";
                  packetIndex <= packetIndex + 1;
                when 4 =>
                  -- reserved(15:0) & double-wordN(63:48)
                  if (responsePayloadLength_i = "0000") and (responseWdptr_i = '0') then
                    masterDat_o <= x"0000" & memoryDataRead(31 downto 16);
                    responsePayload(31 downto 16) <= memoryDataRead(15 downto 0);
                    memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
                  elsif (responsePayloadLength_i = "0000") and (responseWdptr_i = '1') then
                    masterDat_o <= x"0000" & x"0000";
                  else
                    masterDat_o <= x"0000" & memoryDataRead(31 downto 16);
                    responsePayload(31 downto 16) <= memoryDataRead(15 downto 0);
                    memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
                  end if;
                  packetIndex <= packetIndex + 1;
                when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 | 21 | 23 | 25 | 27 | 29 | 31 =>
                  -- double-wordN(47:16)
                  if (responsePayloadLength_i = "0000") and (responseWdptr_i = '0') then
                    masterDat_o <= responsePayload(31 downto 16) & x"0000";
                  elsif (responsePayloadLength_i = "0000") and (responseWdptr_i = '1') then
                    masterDat_o <= x"0000" & memoryDataRead(31 downto 16);
                    responsePayload(31 downto 16) <= memoryDataRead(15 downto 0);
                    memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
                  else
                    masterDat_o <=
                      responsePayload(31 downto 16) & memoryDataRead(31 downto 16);
                    responsePayload(31 downto 16) <= memoryDataRead(15 downto 0);
                    memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
                  end if;
                  packetIndex <= packetIndex + 1;
                when 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 | 32 =>
                  -- double-wordN(15:0) & double-wordN(63:48)
                  if (responsePayloadLength_i = "0000") and (responseWdptr_i = '0') then
                    masterDat_o <= x"0000" & x"0000";
                  elsif (responsePayloadLength_i = "0000") and (responseWdptr_i = '1') then
                    masterDat_o <= responsePayload(31 downto 16) & x"0000";
                  else
                    if (responsePayloadIndex /= responsePayloadLength_i(3 downto 1)) then
                      masterDat_o <=
                        responsePayload(31 downto 16) & memoryDataRead(31 downto 16);
                      responsePayload(31 downto 16) <= memoryDataRead(15 downto 0);
                      memoryAddress <= std_logic_vector(unsigned(memoryAddress) + 1);
                    else
                      masterDat_o <=
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
            if (masterAck_i = '1') then
              case (packetIndex) is
                when 1 =>
                  -- destination
                  masterDat_o <= responseDstId_i;
                  packetIndex <= packetIndex + 1;
                when 2 =>
                  -- source 
                  masterDat_o <= responseSrcId_i;
                  packetIndex <= packetIndex + 1;
                when 3 =>
                  -- transaction & status & targetTID & hop & reserved(7:0)
                  masterDat_o <= "0011" & "0000" & responseTid_i & x"ff" & x"00";
                  packetIndex <= packetIndex + 1;
                when others =>
                  -- reserved(15:0) & crc(15:0)
                  masterDat_o <= x"00000000";
                  packetIndex <= packetIndex + 1;
                  state <= WAIT_COMPLETE;
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



---- REMARK: Make this a common component? Can be used in IO-accesses as well.
---- REMARK: Not all of these are allowed in a maintenance request.
--process(clk, areset_n)
--begin
--  if (areset_n = '0') then
--    doubleWords <= 0;
--    byteLanes <= (others=>'0');
--  elsif (clk'event and clk = '1') then
--    if (maintReadComplete = '1') or (maintWriteComplete = '1') then
--      if (wdptr = '0') then
--        case to_integer(unsigned(size)) is
--          when 0 =>
--            doubleWords <= 0;
--            byteLanes <= "10000000";
--          when 1 =>
--            doubleWords <= 0;
--            byteLanes <= "01000000";
--          when 2 =>
--            doubleWords <= 0;
--            byteLanes <= "00100000";
--          when 3 =>
--            doubleWords <= 0;
--            byteLanes <= "00010000";
--          when 4 =>
--            doubleWords <= 0;
--            byteLanes <= "11000000";
--          when 5 =>
--            doubleWords <= 0;
--            byteLanes <= "11100000";
--          when 6 =>
--            doubleWords <= 0;
--            byteLanes <= "00110000";
--          when 7 =>
--            doubleWords <= 0;
--            byteLanes <= "11111000";
--          when 8 =>
--            doubleWords <= 0;
--            byteLanes <= "11110000";
--          when 9 =>
--            doubleWords <= 0;
--            byteLanes <= "11111100";
--          when 10 =>
--            doubleWords <= 0;
--            byteLanes <= "11111110";
--          when 11 =>
--            doubleWords <= 0;
--            byteLanes <= "11111111";
--          when 12 =>
--            doubleWords <= 3;
--            byteLanes <= "11111111";
--          when 13 =>
--            doubleWords <= 11;
--            byteLanes <= "11111111";
--          when 14 =>
--            doubleWords <= 19;
--            byteLanes <= "11111111";
--          when others =>
--            doubleWords <= 27;
--            byteLanes <= "11111111";
--        end case;
--      else
--        case to_integer(unsigned(size)) is
--          when 0 =>
--            doubleWords <= 0;
--            byteLanes <= "00001000";
--          when 1 =>
--            doubleWords <= 0;
--            byteLanes <= "00000100";
--          when 2 =>
--            doubleWords <= 0;
--            byteLanes <= "00000010";
--          when 3 =>
--            doubleWords <= 0;
--            byteLanes <= "00000001";
--          when 4 =>
--            doubleWords <= 0;
--            byteLanes <= "00001100";
--          when 5 =>
--            doubleWords <= 0;
--            byteLanes <= "00000111";
--          when 6 =>
--            doubleWords <= 0;
--            byteLanes <= "00000011";
--          when 7 =>
--            doubleWords <= 0;
--            byteLanes <= "00011111";
--          when 8 =>
--            doubleWords <= 0;
--            byteLanes <= "00001111";
--          when 9 =>
--            doubleWords <= 0;
--            byteLanes <= "00111111";
--          when 10 =>
--            doubleWords <= 0;
--            byteLanes <= "01111111";
--          when 11 =>
--            doubleWords <= 1;
--            byteLanes <= "11111111";
--          when 12 =>
--            doubleWords <= 7;
--            byteLanes <= "11111111";
--          when 13 =>
--            doubleWords <= 15;
--            byteLanes <= "11111111";
--          when 14 =>
--            doubleWords <= 23;
--            byteLanes <= "11111111";
--          when others =>
--            doubleWords <= 31;
--            byteLanes <= "11111111";
--        end case;
--      end if;
--    end if;
--  end if;
--end process;
