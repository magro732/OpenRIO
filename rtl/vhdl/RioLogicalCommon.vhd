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

    masterCyc_o : out std_logic;
    masterStb_o : out std_logic;
    masterAdr_o : out std_logic_vector(7 downto 0);
    masterDat_o : out std_logic_vector(31 downto 0);
    masterAck_i : in std_logic;
    
    slaveCyc_i : in std_logic;
    slaveStb_i : in std_logic;
    slaveDat_i : in std_logic_vector(31 downto 0);
    slaveAck_o : out std_logic);
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

begin

  Ingress: RioLogicalCommonIngress
    port map(
      clk=>clk, areset_n=>areset_n, 
      readFrameEmpty_i=>readFrameEmpty_i, 
      readFrame_o=>readFrame_o, 
      readContent_o=>readContent_o, 
      readContentEnd_i=>readContentEnd_i, 
      readContentData_i=>readContentData_i, 
      masterCyc_o=>masterCyc_o, 
      masterStb_o=>masterStb_o, 
      masterAdr_o=>masterAdr_o, 
      masterDat_o=>masterDat_o, 
      masterAck_i=>masterAck_i);

  Egress: RioLogicalCommonEgress
    port map(
      clk=>clk, areset_n=>areset_n, 
      writeFrameFull_i=>writeFrameFull_i, 
      writeFrame_o=>writeFrame_o, 
      writeFrameAbort_o=>writeFrameAbort_o, 
      writeContent_o=>writeContent_o, 
      writeContentData_o=>writeContentData_o, 
      slaveCyc_i=>slaveCyc_i, 
      slaveStb_i=>slaveStb_i, 
      slaveDat_i=>slaveDat_i, 
      slaveAck_o=>slaveAck_o);

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
