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
-- REMARK: Use inputOutput/message/maintenance/gsm/...-strobes instead?
-- REMARK: If the deviceId:s are removed, it will work for both 8/16-bit deviceIds.
--          case (ftype) is
--            when x"1" =>
--              -- Intervention-request class.
--              gsmStb_o <= '1';
--            when x"2" =>
--              -- Request-class.
--              if ((transaction = "0100") or
--                  (transaction = "1100") or (transaction = "1101") or
--                  (transaction = "1110") or (transaction = "1111")) then
--                inputOutputStb_o <= '1';
--              else
--                gsmStb_o <= '1';
--              end if;
--            when x"5" =>
--              -- Write-class.
--              if ((transaction = "0100") or (transaction = "0101") or 
--                  (transaction = "1100") or (transaction = "1101") or
--                  (transaction = "1110")) then
--                inputOutputStb_o <= '1';
--              elsif ((transaction = "0000") or (transaction = "0001")) then
--                gsmStb_o <= '1';
--              end if;
--            when x"6" =>
--              -- Streaming-Write class.
--              inputOutputStb_o <= '1';
--            when x"7" =>
--              -- Flow-control class.
--              flowControlStb_o <= '1';
--            when x"8" =>
--              -- Maintenance class.
--              maintenanceStb_o <= '1';
--            when x"9" =>
--              -- Data-Streaming class.
--              dataStreamingStb_o <= '1';
--            when x"a" =>
--              -- Doorbell class.
--              -- REMARK: Make this belong to input/output since the packets
--              -- and their responses look the same?
--              messagePassingStb_o <= '1';
--            when x"b" =>
--              -- Message class.
--              messagePassingStb_o <= '1';
--            when x"d" =>
--              -- Response class.
--              -- REMARK: Seperate strobe for this???
--              if ((transaction = "0000") or (transaction = "1000")) then
--                -- REMARK: Doorbell-response going in here as well... *sigh*
--                -- REMARK: GSM-responses going in here as well...
--                responseStb_o <= '1';
--              elsif (transaction = "0001") then
--                messagePassing <= '1';
--              end if;
--            when others =>
--              -- Unsupported ftype.
--              -- REMARK: Discard this packet.
--          end case;

-- tt=00
-- 0: header(15:0);dest(7:0);src(7:0);
-- 1: transaction(3:0)
-- shifter: 32 (32 empty)
-- tt=01
-- 0: header(15:0);dest(15:0);
-- 1: src(15:0);transaction(3:0)
-- shifter: 16 (48 empty)


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


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
    slaveAck_o : out std_logic;

    configStb_o : out std_logic;
    configWe_o : out std_logic;
    configAdr_o : out std_logic_vector(20 downto 0);
    configSel_o : out std_logic_vector(7 downto 0);
    configDat_o : out std_logic_vector(63 downto 0);
    configDat_i : in std_logic_vector(63 downto 0);
    configAck_i : in std_logic);
end entity;


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
      configAdr_o : out std_logic_vector(20 downto 0);
      configDat_o : out std_logic_vector(63 downto 0);
      configSel_o : out std_logic_vector(7 downto 0);
      configDat_i : in std_logic_vector(63 downto 0);
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

  signal masterMaintenanceAck : std_logic;
  signal slaveMaintenanceAck : std_logic;
  
begin

  -- REMARK: Add crossbar for slave interface?
  masterCyc_o <= masterCyc;
  masterStb_o <= masterStb;
  masterAdr_o <= masterAdr;
  masterDat_o <= masterDat;
  masterAck <= masterAck_i or masterMaintenanceAck;
  
  slaveCyc <= slaveCyc_i;
  slaveStb <= slaveStb_i;
  slaveDat <= slaveDat_i;
  slaveAck_o <= slaveAck or slaveMaintenanceAck;
  
  LogicalMaintenance: RioLogicalMaintenance
    port map(
      clk=>clk, areset_n=>areset_n, enable=>enable, 
      configStb_o=>configStb_o, 
      configWe_o=>configWe_o, 
      configAdr_o=>configAdr_o, 
      configDat_o=>configDat_o, 
      configSel_o=>configSel_o, 
      configDat_i=>configDat_i, 
      configAck_i=>configAck_i, 
      slaveCyc_i=>masterCyc, 
      slaveStb_i=>masterStb, 
      slaveAdr_i=>masterAdr, 
      slaveDat_i=>masterDat, 
      slaveAck_o=>masterMaintenanceAck, 
      masterCyc_o=>slaveCyc, 
      masterStb_o=>slaveStb, 
      masterDat_o=>slaveDat, 
      masterAck_i=>slaveMaintenanceAck);

  -- REMARK: Add interconnect for master signals...
  
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
      state <= IDLE;
      packetPosition <= 0;
      packetContent <= (others=>'0');
      tt <= "00";
      ftype <= "0000";
      transaction <= "0000";
    elsif (clk'event and clk = '1') then
      readContent_o <= '0';
      
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
              packetContent <= packetContent(31 downto 0) & readContentData_i;
              readContent_o <= '1';
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
              packetContent <= packetContent(47 downto 0) & x"0000";
            end if;

            state <= FORWARD_SHORT;
          end if;
          
        when FORWARD_SHORT =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (masterAck_i = '1') then
            masterDat_o <= packetContent(63 downto 32);

            packetPosition <= packetPosition + 1;
            packetContent <=
              packetContent(31 downto 0) & readContentData_i;
            
            if (readContentEnd_i = '0') then
              if (packetPosition = 20) then
                state <= FORWARD_CRC;
              end if;
              
              readContent_o <= '1';
            else
              readFrame_o <= '1';
              state <= FORWARD_LAST;
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
  signal dstAddr : std_logic_vector(31 downto 0);
  signal srcAddr : std_logic_vector(31 downto 0);
  
  signal writeContentData : std_logic_vector(31 downto 0);

  signal crc16Current, crc16Temp, crc16Next: std_logic_vector(15 downto 0);

begin

  writeContentData_o <= writeContentData;
  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      state <= IDLE;
      crc16Current <= x"0000";
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
            state <= HEADER_GET;
            crc16Current <= x"ffff";
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
            state <= RESTART_FRAME;
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
              dstAddr <= x"00" & slaveDat_i(7 downto 0);
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

          if (tt = "00") then
            crc16Current <= crc16Next;
          end if;
          
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
              -- REMARK: The current CRC has to be updated when this is written.
              writeContent_o <= '1';
              writeContentData <= halfWord & crc16Next;
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
          if (halfWordPending = '0') then
            writeContent_o <= '1';
            writeContentData <= crc16Current & x"0000";
            packetPosition <= packetPosition + 1;
          else
            writeContent_o <= '1';
            writeContentData <= halfWord & crc16Current;
            packetPosition <= packetPosition + 1;
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
    configAdr_o : out std_logic_vector(20 downto 0);
    configDat_o : out std_logic_vector(63 downto 0);
    configSel_o : out std_logic_vector(7 downto 0);
    configDat_i : in std_logic_vector(63 downto 0);
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
      requestPayloadSelect_o : out std_logic_vector(7 downto 0);
      requestPayloadLength_o : out std_logic_vector(4 downto 0);
      requestPayloadIndex_i : in std_logic_vector(4 downto 0);
      requestPayload_o : out std_logic_vector(63 downto 0);
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
      responsePayloadLength_i : in std_logic_vector(3 downto 0);
      responsePayloadWrite_i : in std_logic;
      responsePayloadIndex_i : in std_logic_vector(4 downto 0);
      responsePayload_i : in std_logic_vector(63 downto 0);
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

  signal configAdr : std_logic_vector(20 downto 0);
  
  signal requestReadReady : std_logic;
  signal requestWriteReady : std_logic;
  signal requestOffset : std_logic_vector(20 downto 0);
  signal requestPayloadLength : std_logic_vector(4 downto 0);
  signal requestPayloadIndex : std_logic_vector(4 downto 0);
  signal requestDone : std_logic;
  
  signal responseReadReady : std_logic;
  signal responseWriteReady : std_logic;
  signal responsePayloadLength : std_logic_vector(3 downto 0);
  signal responsePayloadWrite : std_logic;
  signal responsePayloadIndex : std_logic_vector(4 downto 0);
  signal responseDone : std_logic;
  
begin

  configAdr_o <= configAdr;
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  Maintenance: process(clk, areset_n)
  begin
    if (areset_n = '0') then

    elsif (clk'event and clk = '1') then
      case state is
        when IDLE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (requestReadReady = '1') then
            configStb_o <= '1';
            configWe_o <= '0';
            configAdr <= requestOffset;
            responsePayloadIndex <= (others=>'0');
            state <= CONFIG_READ;
          elsif (requestWriteReady = '1') then
            configStb_o <= '1';
            configWe_o <= '1';
            configAdr <= requestOffset;
            requestPayloadIndex <= (others=>'0');
            state <= CONFIG_WRITE;
          end if;          

        when CONFIG_READ =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (configAck_i = '1') then
            responsePayloadWrite <= '1';
            responsePayloadIndex <=
              std_logic_vector(unsigned(responsePayloadIndex) + 1);
            
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
          responseReadReady <= '1';
          if (responseDone = '1') then
            state <= IDLE;
          end if;

        when CONFIG_WRITE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (configAck_i = '1') then
            if (responsePayloadIndex /= requestPayloadLength) then
              configAdr <= std_logic_vector(unsigned(configAdr) + 1);
              requestPayloadIndex <=
                std_logic_vector(unsigned(requestPayloadIndex) + 1);
            else
              requestDone <= '1';
              configStb_o <= '0';
              state <= CONFIG_READ_RESPONSE;
            end if;
          end if;

        when CONFIG_WRITE_RESPONSE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          responseWriteReady <= '1';
          if (responseDone = '1') then
            state <= IDLE;
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
      requestPayloadSelect_o=>configSel_o, 
      requestPayloadLength_o=>requestPayloadLength, 
      requestPayloadIndex_i=>requestPayloadIndex, 
      requestPayload_o=>configDat_o, 
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
      responsePayloadLength_i=>responsePayloadLength, 
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
    requestPayloadSelect_o : out std_logic_vector(7 downto 0);
    requestPayloadLength_o : out std_logic_vector(4 downto 0);
    requestPayloadIndex_i : in std_logic_vector(4 downto 0);
    requestPayload_o : out std_logic_vector(63 downto 0);
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
  signal byteLanes : std_logic_vector(7 downto 0);
  signal doubleWords : natural range 0 to 32;

  signal slaveAck : std_logic;
  signal maintReadComplete : std_logic;
  signal maintWriteComplete : std_logic;

  signal packetIndex : natural range 0 to 33;
  signal requestData : std_logic_vector(63 downto 0);

  signal memoryEnable : std_logic;
  signal memoryWrite : std_logic;
  signal memoryAddress : std_logic_vector(4 downto 0);
  signal memoryDataIn : std_logic_vector(63 downto 0);

begin

  slaveAck_o <= slaveAck;

  requestReadReady_o <= maintReadComplete;
  requestWriteReady_o <= maintWriteComplete;
  
  MaintenanceRequest: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      packetIndex <= 0;
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
                      requestData(63 downto 48) <= slaveDat_i(15 downto 0);
                      wdptr <= slaveDat_i(18);
                      packetIndex <= packetIndex + 1;
                    when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 | 21 | 23 | 25 | 27 | 29 | 31 =>
                      -- double-word(47:16)
                      requestData(47 downto 16) <= slaveDat_i;
                      packetIndex <= packetIndex + 1;
                    when 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 | 32 =>
                      -- double-word(15:0) & double-word(63:48)
                      requestData(63 downto 48) <= slaveDat_i(15 downto 0);
                      packetIndex <= packetIndex + 1;

                      memoryEnable <= '1';
                      memoryAddress <=
                        std_logic_vector(unsigned(memoryAddress) + 1);
                      memoryWrite <= '1';
                      memoryDataIn <=
                        requestData(63 downto 16) & slaveDat_i(31 downto 16);
                      maintWriteComplete <= '1';
                    when others =>
                      -- There should be no more content in a maintenance write request.
                      -- Discard.
                  end case;
                end if;
                slaveAck <= '1';
              end if;
            else
              memoryEnable <= '0';
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

      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Transformation of rdsize/wrsize into length of access and byte lanes.
  -----------------------------------------------------------------------------
  requestPayloadLength_o <= std_logic_vector(to_unsigned(doubleWords, 5));
  requestPayloadSelect_o <= byteLanes;
  
  -- REMARK: Make this a common component? Can be used in IO-accesses as well.
  -- REMARK: Not all of these are allowed in a maintenance request.
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      doubleWords <= 0;
      byteLanes <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (maintReadComplete = '1') or (maintWriteComplete = '1') then
        if (wdptr = '0') then
          case to_integer(unsigned(size)) is
            when 0 =>
              doubleWords <= 1;
              byteLanes <= "10000000";
            when 1 =>
              doubleWords <= 1;
              byteLanes <= "01000000";
            when 2 =>
              doubleWords <= 1;
              byteLanes <= "00100000";
            when 3 =>
              doubleWords <= 1;
              byteLanes <= "00010000";
            when 4 =>
              doubleWords <= 1;
              byteLanes <= "11000000";
            when 5 =>
              doubleWords <= 1;
              byteLanes <= "11100000";
            when 6 =>
              doubleWords <= 1;
              byteLanes <= "00110000";
            when 7 =>
              doubleWords <= 1;
              byteLanes <= "11111000";
            when 8 =>
              doubleWords <= 1;
              byteLanes <= "11110000";
            when 9 =>
              doubleWords <= 1;
              byteLanes <= "11111100";
            when 10 =>
              doubleWords <= 1;
              byteLanes <= "11111110";
            when 11 =>
              doubleWords <= 1;
              byteLanes <= "11111111";
            when 12 =>
              doubleWords <= 4;
              byteLanes <= "11111111";
            when 13 =>
              doubleWords <= 12;
              byteLanes <= "11111111";
            when 14 =>
              doubleWords <= 20;
              byteLanes <= "11111111";
            when others =>
              doubleWords <= 28;
              byteLanes <= "11111111";
          end case;
        else
          case to_integer(unsigned(size)) is
            when 0 =>
              doubleWords <= 1;
              byteLanes <= "00001000";
            when 1 =>
              doubleWords <= 1;
              byteLanes <= "00000100";
            when 2 =>
              doubleWords <= 1;
              byteLanes <= "00000010";
            when 3 =>
              doubleWords <= 1;
              byteLanes <= "00000001";
            when 4 =>
              doubleWords <= 1;
              byteLanes <= "00001100";
            when 5 =>
              doubleWords <= 1;
              byteLanes <= "00000111";
            when 6 =>
              doubleWords <= 1;
              byteLanes <= "00000011";
            when 7 =>
              doubleWords <= 1;
              byteLanes <= "00011111";
            when 8 =>
              doubleWords <= 1;
              byteLanes <= "00001111";
            when 9 =>
              doubleWords <= 1;
              byteLanes <= "00111111";
            when 10 =>
              doubleWords <= 1;
              byteLanes <= "01111111";
            when 11 =>
              doubleWords <= 2;
              byteLanes <= "11111111";
            when 12 =>
              doubleWords <= 8;
              byteLanes <= "11111111";
            when 13 =>
              doubleWords <= 16;
              byteLanes <= "11111111";
            when 14 =>
              doubleWords <= 24;
              byteLanes <= "11111111";
            when others =>
              doubleWords <= 32;
              byteLanes <= "11111111";
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
    responsePayloadLength_i : in std_logic_vector(3 downto 0);
    responsePayloadWrite_i : in std_logic;
    responsePayloadIndex_i : in std_logic_vector(4 downto 0);
    responsePayload_i : in std_logic_vector(63 downto 0);
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
                     RESPONSE_DONE);
  signal state : StateType;

  signal packetIndex : natural range 0 to 33;
  signal responseHeader : std_logic_vector(31 downto 0);
  signal responsePayload : std_logic_vector(63 downto 0);
  
  signal memoryEnable : std_logic;
  signal memoryAddress : std_logic_vector(4 downto 0);
  signal memoryDataRead : std_logic_vector(63 downto 0);

begin
  
  responseHeader <=
    x"0000" & "000000" & responseVc_i & responseCrf_i &
    responsePrio_i & responseTt_i & x"8";

  MaintenanceResponse: process(clk, areset_n)
  begin
    if (areset_n = '0') then

    elsif (clk'event and clk = '1') then
      if (responseReadReady_i = '1') or (responseWriteReady_i = '1') then
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
              if (packetIndex = unsigned(responsePayloadLength_i)) then
                masterCyc_o <= '0';
                masterStb_o <= '0';
                state <= RESPONSE_DONE;
              else
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
                    responsePayload <= memoryDataRead;
                    memoryAddress <=
                      std_logic_vector(unsigned(memoryAddress) + 1);
                    packetIndex <= packetIndex + 1;
                  when 4 =>
                    -- reserved(15:0) & double-wordN(63:48)
                    masterDat_o <= x"0000" & responsePayload(63 downto 48);
                    packetIndex <= packetIndex + 1;
                  when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 | 21 | 23 | 25 | 27 | 29 | 31 =>
                    -- double-wordN(47:16)
                    masterDat_o <= responsePayload(47 downto 16);
                    packetIndex <= packetIndex + 1;
                  when 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 | 32 =>
                    -- double-wordN(15:0) & double-wordN(63:32)
                    masterDat_o <= responsePayload(15 downto 0) & responsePayload_i(63 downto 48);
                    responsePayload <= memoryDataRead;
                    memoryAddress <=
                      std_logic_vector(unsigned(memoryAddress) + 1);
                    packetIndex <= packetIndex + 1;
                  when others =>
                    -- Unallowed response length.
                    -- Dont do anything.
                end case;
              end if;
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
                  masterDat_o <= "0010" & "0000" & responseTid_i & x"ff" & x"00";
                  responsePayload <= memoryDataRead;
                  packetIndex <= packetIndex + 1;
                when 4 =>
                  -- reserved(15:0) & crc(15:0)
                  masterDat_o <= x"00000000";
                  packetIndex <= packetIndex + 1;
                when others =>
                  -- Response packet has been completed.
                  masterCyc_o <= '0';
                  masterStb_o <= '0';
                  state <= RESPONSE_DONE;
              end case;
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
    generic map(ADDRESS_WIDTH=>5, DATA_WIDTH=>64)
    port map(clkA_i=>clk,
             enableA_i=>responsePayloadWrite_i,
             addressA_i=>responsePayloadIndex_i,
             dataA_i=>responsePayload_i,
             clkB_i=>clk,
             enableB_i=>memoryEnable,
             addressB_i=>memoryAddress,
             dataB_o=>memoryDataRead);

end architecture;
