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
-- - Clean up and increase the speed of the interface to packet handlers.
-- - 8-bit deviceId has not been verified, fix.
-- - Egress; Place packets in different queues depending on the packet priority?
-- - Add verification of all sizes of packets.
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
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity RioLogicalCommon is
  generic(
    PORTS : natural);
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

    inboundCyc_o : out std_logic;
    inboundStb_o : out std_logic;
    inboundAdr_o : out std_logic_vector(7 downto 0);
    inboundDat_o : out std_logic_vector(31 downto 0);
    inboundAck_i : in std_logic;
    
    outboundCyc_i : in std_logic_vector(PORTS-1 downto 0);
    outboundStb_i : in std_logic_vector(PORTS-1 downto 0);
    outboundDat_i : in std_logic_vector(32*PORTS-1 downto 0);
    outboundAck_o : out std_logic_vector(PORTS-1 downto 0));
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioLogicalCommon of RioLogicalCommon is

  component RioLogicalCommonInterconnect is
    generic(
      WIDTH : natural);
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      stb_i : in std_logic_vector(WIDTH-1 downto 0);
      dataM_i : in std_logic_vector(32*WIDTH-1 downto 0);
      ack_o : out std_logic_vector(WIDTH-1 downto 0);

      stb_o : out std_logic;
      dataS_o : out std_logic_vector(31 downto 0);
      ack_i : in std_logic);
  end component;
  
  component RioLogicalCommonIngress is
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      
      readFrameEmpty_i : in std_logic;
      readFrame_o : out std_logic;
      readContent_o : out std_logic;
      readContentEnd_i : in std_logic;
      readContentData_i : in std_logic_vector(31 downto 0);

      inboundCyc_o : out std_logic;
      inboundStb_o : out std_logic;
      inboundAdr_o : out std_logic_vector(7 downto 0);
      inboundDat_o : out std_logic_vector(31 downto 0);
      inboundAck_i : in std_logic);
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

      outboundCyc_i : in std_logic;
      outboundStb_i : in std_logic;
      outboundDat_i : in std_logic_vector(31 downto 0);
      outboundAck_o : out std_logic);
  end component;

  signal outboundStb : std_logic;
  signal outboundDat : std_logic_vector(31 downto 0);
  signal outboundAck : std_logic;
  
begin

  Ingress: RioLogicalCommonIngress
    port map(
      clk=>clk, areset_n=>areset_n, 
      readFrameEmpty_i=>readFrameEmpty_i, 
      readFrame_o=>readFrame_o, 
      readContent_o=>readContent_o, 
      readContentEnd_i=>readContentEnd_i, 
      readContentData_i=>readContentData_i, 
      inboundCyc_o=>inboundCyc_o, 
      inboundStb_o=>inboundStb_o, 
      inboundAdr_o=>inboundAdr_o, 
      inboundDat_o=>inboundDat_o, 
      inboundAck_i=>inboundAck_i);

  EgressInterconnect: RioLogicalCommonInterconnect
    generic map(WIDTH=>PORTS)
    port map(
      clk=>clk, areset_n=>areset_n, 
      stb_i=>outboundStb_i, 
      dataM_i=>outboundDat_i, 
      ack_o=>outboundAck_o, 
      stb_o=>outboundStb, 
      dataS_o=>outboundDat, 
      ack_i=>outboundAck);

  Egress: RioLogicalCommonEgress
    port map(
      clk=>clk, areset_n=>areset_n, 
      writeFrameFull_i=>writeFrameFull_i, 
      writeFrame_o=>writeFrame_o, 
      writeFrameAbort_o=>writeFrameAbort_o, 
      writeContent_o=>writeContent_o, 
      writeContentData_o=>writeContentData_o, 
      outboundCyc_i=>'1', 
      outboundStb_i=>outboundStb, 
      outboundDat_i=>outboundDat, 
      outboundAck_o=>outboundAck);

end architecture;



-------------------------------------------------------------------------------
-- RioLogicalCommonIngress.
-------------------------------------------------------------------------------
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

    inboundCyc_o : out std_logic;
    inboundStb_o : out std_logic;
    inboundAdr_o : out std_logic_vector(7 downto 0);
    inboundDat_o : out std_logic_vector(31 downto 0);
    inboundAck_i : in std_logic);
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

  signal packetPosition : natural range 0 to 68;
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

      readContent_o <= '0';
      readFrame_o <= '0';

      inboundCyc_o <= '0';
      inboundStb_o <= '0';
      inboundAdr_o <= (others=>'0');
      inboundDat_o <= (others=>'0');
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
          inboundCyc_o <= '1';
          inboundStb_o <= '1';
          inboundAdr_o <= ftype & transaction;
          inboundDat_o <= x"0000" & packetContent(63 downto 48);
          packetContent <= packetContent(47 downto 0) & x"0000";

          state <= SEND_DESTINATION;

        when SEND_DESTINATION =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (inboundAck_i = '1') then
            if (tt = "00") then
              inboundDat_o <= x"000000" & packetContent(63 downto 56);
              packetContent <= packetContent(55 downto 0) & x"00";
            elsif (tt = "01") then
              inboundDat_o <= x"0000" & packetContent(63 downto 48);
              packetContent <= packetContent(47 downto 0) & x"0000";
            end if;

            state <= SEND_SOURCE;
          end if;
          
        when SEND_SOURCE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (inboundAck_i = '1') then
            if (tt = "00") then
              inboundDat_o <= x"000000" & packetContent(63 downto 56);
              packetContent <= packetContent(55 downto 0) & x"00";
            elsif (tt = "01") then
              inboundDat_o <= x"0000" & packetContent(63 downto 48);
              packetContent <= packetContent(47 downto 32) & readContentData_i & x"0000";
              readContent_o <= '1';
            end if;

            state <= FORWARD_SHORT;
          end if;
          
        when FORWARD_SHORT =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (inboundAck_i = '1') then
            packetPosition <= packetPosition + 1;

            if (tt = "00") then
              inboundDat_o <= packetContent(63 downto 32);
              packetContent <= packetContent(31 downto 0) & readContentData_i;
            elsif (tt = "01") then
              inboundDat_o <= packetContent(63 downto 32);
              packetContent <= packetContent(31 downto 16) & readContentData_i & x"0000";
            end if;
            
            if (readContentEnd_i = '0') then
              if (packetPosition = 18) then
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
          if (inboundAck_i = '1') then
            inboundDat_o <= packetContent(63 downto 32);

            packetPosition <= packetPosition + 1;
            packetContent <=
              packetContent(31 downto 16) & readContentData_i(15 downto 0) & x"00000000";
            
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
          if (inboundAck_i = '1') then
            inboundDat_o <= packetContent(63 downto 32);

            packetPosition <= packetPosition + 1;
            packetContent <=
              readContentData_i & x"00000000";
            
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
          if (inboundAck_i = '1') then
            inboundDat_o <= packetContent(63 downto 32);
            state <= END_PACKET;
          end if;
          
        when END_PACKET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (inboundAck_i = '1') then
            inboundCyc_o <= '0';
            inboundStb_o <= '0';
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

    outboundCyc_i : in std_logic;
    outboundStb_i : in std_logic;
    outboundDat_i : in std_logic_vector(31 downto 0);
    outboundAck_o : out std_logic);
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
                     CRC_APPEND, CRC_UPDATE, CRC_LAST, SEND_FRAME,
                     RESTART_FRAME, WAIT_UPDATE);
  signal state : StateType;
  signal packetPosition : natural range 0 to 69;
  
  signal temp : std_logic_vector(15 downto 0);
  
  signal tt : std_logic_vector(1 downto 0);
  signal dstAddr : std_logic_vector(7 downto 0);

  signal writeContent : std_logic;
  signal writeContentData1 : std_logic_vector(31 downto 0);
  signal writeContentData2 : std_logic_vector(31 downto 0);

  signal crcReset : std_logic;
  signal crc16Current, crc16Temp, crc16Next: std_logic_vector(15 downto 0);

begin

  writeContent_o <= writeContent;
  writeContentData_o <= writeContentData1;

  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      crc16Current <= x"0000";
    elsif (clk'event and clk = '1') then
      if (crcReset = '1') then
        crc16Current <= x"ffff";
      elsif (writeContent = '1') then
        crc16Current <= crc16Next;
      end if;
    end if;
  end process;
  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      state <= IDLE;
      packetPosition <= 0;

      tt <= (others=>'0');
      dstAddr <= (others=>'0');

      temp <= (others=>'0');
      writeContent <= '0';
      writeContentData1 <= (others=>'0');
      writeContentData2 <= (others=>'0');
      
      crcReset <= '0';
      
      outboundAck_o <= '0';

      writeFrame_o <= '0';
      writeFrameAbort_o <= '0';
    elsif (clk'event and clk = '1') then
      writeContent <= '0';
      writeFrame_o <= '0';
      
      crcReset <= '0';
      
      case state is
        when IDLE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          packetPosition <= 0;
          crcReset <= '1';
          if (writeFrameFull_i = '0') then
            state <= HEADER_GET;
          end if;

        when HEADER_GET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if ((outboundCyc_i = '1') and (outboundStb_i = '1')) then
            temp <= outboundDat_i(15 downto 0);
            tt <= outboundDat_i(5 downto 4);

            outboundAck_o <= '1';
            state <= HEADER_ACK;
          else
            state <= HEADER_GET;
          end if;

        when HEADER_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          outboundAck_o <= '0';
          state <= DESTINATION_GET;

        when DESTINATION_GET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          
          if ((outboundCyc_i = '1') and (outboundStb_i = '1')) then
            if (tt = "01") then
              writeContentData2 <= temp & outboundDat_i(15 downto 0);
            else
              report "TT-field not supported." severity error;
            end if;
            
            outboundAck_o <= '1';
            state <= DESTINATION_ACK;
          else
            state <= RESTART_FRAME;
          end if;
        
        when DESTINATION_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          outboundAck_o <= '0';
          state <= SOURCE_GET;

        when SOURCE_GET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          
          if ((outboundCyc_i = '1') and (outboundStb_i = '1')) then
            if (tt = "01") then
              temp <= outboundDat_i(15 downto 0);
            end if;
            
            outboundAck_o <= '1';
            state <= SOURCE_ACK;
          else
            state <= RESTART_FRAME;
          end if;
        
        when SOURCE_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          outboundAck_o <= '0';
          state <= CONTENT_GET;
          
        when CONTENT_GET =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if ((outboundCyc_i = '1') and (outboundStb_i = '1')) then
            if (packetPosition < 19) then
              if (tt = "01") then
                writeContentData2 <= temp & outboundDat_i(31 downto 16);
                temp <= outboundDat_i(15 downto 0);
                outboundAck_o <= '1';
              end if;
            elsif (packetPosition = 19) then
              if (tt = "01") then
                writeContentData2 <= crc16Next & temp;
              end if;
            else
              if (tt = "01") then
                writeContentData2 <= outboundDat_i;
                outboundAck_o <= '1';
              end if;
            end if;
            writeContent <= '1';
            writeContentData1 <= writeContentData2;
            packetPosition <= packetPosition + 1;
            state <= CONTENT_ACK;
          else
            state <= CRC_APPEND;
          end if;
          
        when CONTENT_ACK =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (packetPosition = 20) then
            if (tt = "01") then
              writeContentData2 <= crc16Next & temp;
            end if;
          end if;
          outboundAck_o <= '0';
          state <= CONTENT_GET;

        when CRC_APPEND =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (packetPosition < 19) then
            if (tt = "01") then
              writeContent <= '1';
              writeContentData1 <= writeContentData2;
              packetPosition <= packetPosition + 1;
            end if;
          elsif (packetPosition = 19) then
            if (tt = "01") then
              writeContent <= '1';
              writeContentData1 <= writeContentData2;
              packetPosition <= packetPosition + 1;
            end if;
          else
            if (tt = "01") then
              writeContentData1 <= writeContentData2(31 downto 16) & x"0000";
              packetPosition <= packetPosition + 1;
            end if;
          end if;
          state <= CRC_UPDATE;
          
        when CRC_UPDATE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          state <= CRC_LAST;

        when CRC_LAST =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (packetPosition < 19) then
            if (tt = "01") then
              writeContent <= '1';
              writeContentData1 <= crc16Current & x"0000";
            end if;
          elsif (packetPosition = 19) then
            if (tt = "01") then

            end if;
          else
            if (tt = "01") then
              writeContent <= '1';
              writeContentData1 <= writeContentData2(31 downto 16) & crc16Temp;
              packetPosition <= packetPosition + 1;
            end if;
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
      d_i=>writeContentData1(31 downto 16), crc_i=>crc16Current, crc_o=>crc16Temp);
  Crc16Low: Crc16CITT
    port map(
      d_i=>writeContentData1(15 downto 0), crc_i=>crc16Temp, crc_o=>crc16Next);

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
entity RioLogicalCommonInterconnect is
  generic(
    WIDTH : natural);
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    stb_i : in std_logic_vector(WIDTH-1 downto 0);
    dataM_i : in std_logic_vector(32*WIDTH-1 downto 0);
    ack_o : out std_logic_vector(WIDTH-1 downto 0);

    stb_o : out std_logic;
    dataS_o : out std_logic_vector(31 downto 0);
    ack_i : in std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioLogicalCommonInterconnectImpl of RioLogicalCommonInterconnect is
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
    elsif (clk'event and clk = '1') then
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
  stb_o <= stb_i(selectedMaster) and activeCycle;
  dataS_o <= dataM_i(32*(selectedMaster+1)-1 downto 32*selectedMaster);

  Interconnect: for i in 0 to WIDTH-1 generate
    ack_o(i) <= ack_i when (selectedMaster = i) else '0';
  end generate;

end architecture;


