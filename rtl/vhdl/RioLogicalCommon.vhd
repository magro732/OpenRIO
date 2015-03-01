-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Contains a platform to build endpoints on. It handles CRC insertion/removal
-- and unpacks the deviceId in a packet into a fixed 32-bit to make the parsing
-- of packets in higher layers easier. It also discards packets that does not
-- have a handler.
-- 
-- To Do:
-- - 8-bit deviceId has not been implemented, fix either as seperate
--   architecture or an architecture with combined 8- and 16-bit support.
-- - Egress; Place packets in different queues depending on the packet priority?
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
-- * Removes in-the-middle CRC. The trailing CRC is not removed since it is not
--   possible to know in which half-word it is placed without knowing how the
--   packet should be parsed.
-- * Forwards packets to logical-layer handlers depending on ftype. The
--   ftype-field is output as address.
-- * Outputs header and deviceIDs in seperate accesses to facilitate supporting
--   different deviceId sizes. All fields are right-justified.
-- * stall_i is used to stop the flow of data. The flow will continue when
--   stall_i is deasserted. The stall_i signals should not be registered. If
--   there is no handler for a packet, stall_i will not go high and the packet
--   will be automatically discarded.
-- Egress:
-- * Adds in-the-middle and trailing CRC.
-- * Receives packets from a configurable number of logical-layer handlers.
--   This enables more complex endpoints with several independent
--   funtionalities.
-- * Receives header and deviceId in seperate accesses to facilitate
--   supporting different deviceId sizes. All fields are right-justified. The
--   size of the deviceId is indicated by the TT-field in the header.
-- * The adr_i-input signal on the egress side is used to indicate if the last
--   word contains one or two half-words. This is used to know where to insert
--   the trailing CRC. It should be set at the start of the frame. If all
--   frames always have the CRC in the same place it is ok to set this signal
--   constant.
-- Examples of how to write a handler for a packet can be found in
-- RioLogicalPackets.vhd.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- Entity for RioLogicalCommon.
-------------------------------------------------------------------------------
entity RioLogicalCommon is
  generic(
    PORTS : natural := 1);
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

    inboundStb_o : out std_logic;
    inboundAdr_o : out std_logic_vector(3 downto 0);
    inboundDat_o : out std_logic_vector(31 downto 0);
    inboundStall_i : in std_logic;
    
    outboundStb_i : in std_logic_vector(PORTS-1 downto 0);
    outboundAdr_i : in std_logic_vector(PORTS-1 downto 0);
    outboundDat_i : in std_logic_vector(32*PORTS-1 downto 0);
    outboundStall_o : out std_logic_vector(PORTS-1 downto 0));
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioLogicalCommon.
-------------------------------------------------------------------------------
architecture RioLogicalCommon of RioLogicalCommon is

  component RioLogicalCommonInterconnect is
    generic(
      WIDTH : natural);
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      stb_i : in std_logic_vector(WIDTH-1 downto 0);
      adr_i : in std_logic_vector(WIDTH-1 downto 0);
      data_i : in std_logic_vector(32*WIDTH-1 downto 0);
      stall_o : out std_logic_vector(WIDTH-1 downto 0);

      stb_o : out std_logic;
      adr_o : out std_logic;
      data_o : out std_logic_vector(31 downto 0);
      stall_i : in std_logic);
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

      stb_o : out std_logic;
      adr_o : out std_logic_vector(3 downto 0);
      dat_o : out std_logic_vector(31 downto 0);
      stall_i : in std_logic);
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

      stb_i : in std_logic;
      adr_i : in std_logic;
      dat_i : in std_logic_vector(31 downto 0);
      stall_o : out std_logic);
  end component;

  signal outboundStb : std_logic;
  signal outboundAdr : std_logic;
  signal outboundDat : std_logic_vector(31 downto 0);
  signal outboundStall : std_logic;
  
begin

  Ingress: RioLogicalCommonIngress
    port map(
      clk=>clk, areset_n=>areset_n, 
      readFrameEmpty_i=>readFrameEmpty_i, 
      readFrame_o=>readFrame_o, 
      readContent_o=>readContent_o, 
      readContentEnd_i=>readContentEnd_i, 
      readContentData_i=>readContentData_i, 
      stb_o=>inboundStb_o, 
      adr_o=>inboundAdr_o, 
      dat_o=>inboundDat_o, 
      stall_i=>inboundStall_i);

  EgressInterconnect: RioLogicalCommonInterconnect
    generic map(WIDTH=>PORTS)
    port map(
      clk=>clk, areset_n=>areset_n, 
      stb_i=>outboundStb_i,
      adr_i=>outboundAdr_i,
      data_i=>outboundDat_i, 
      stall_o=>outboundStall_o, 
      stb_o=>outboundStb,
      adr_o=>outboundAdr,
      data_o=>outboundDat, 
      stall_i=>outboundStall);

  Egress: RioLogicalCommonEgress
    port map(
      clk=>clk, areset_n=>areset_n, 
      writeFrameFull_i=>writeFrameFull_i, 
      writeFrame_o=>writeFrame_o, 
      writeFrameAbort_o=>writeFrameAbort_o, 
      writeContent_o=>writeContent_o, 
      writeContentData_o=>writeContentData_o, 
      stb_i=>outboundStb,
      adr_i=>outboundAdr,
      dat_i=>outboundDat, 
      stall_o=>outboundStall);

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

    stb_o : out std_logic;
    adr_o : out std_logic_vector(3 downto 0);
    dat_o : out std_logic_vector(31 downto 0);
    stall_i : in std_logic);
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioLogicalCommonIngress16.
-- Only 16-bit deviceId are supported.
-------------------------------------------------------------------------------
architecture RioLogicalCommonIngress16 of RioLogicalCommonIngress is

  signal packetPosition : natural range 0 to 74;

  signal loadValue, loadValue16 : std_logic_vector(63 downto 0);
  signal packetContent : std_logic_vector(63 downto 0);

  signal tt : std_logic_vector(1 downto 0);
  signal ftype : std_logic_vector(3 downto 0);

  signal readContent : std_logic;
  signal readFrame : std_logic;
  
begin
  readContent_o <= readContent;
  readFrame_o <= readFrame;
  
  adr_o <= ftype;
  dat_o <= packetContent(63 downto 32);

  loadValue16 <=
    (x"0000" & packetContent(31 downto 16) & readContentData_i) when (packetPosition = 4) else
    (x"0000" & packetContent(31 downto 0) & x"0000") when (packetPosition = 5) else
    (packetContent(31 downto 16) & readContentData_i & x"0000") when (packetPosition < 24) else
    (packetContent(31 downto 16) & readContentData_i(15 downto 0) & x"00000000") when (packetPosition = 24) else
    (readContentData_i & x"00000000");
  loadValue <= loadValue16 when (tt = "01") else (x"0000" & readContentData_i & x"0000");
  shifter: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      packetContent <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if ((stall_i = '0') and (readFrameEmpty_i = '0')) then
        packetContent <= loadValue;
      end if;
    end if;
  end process;

  packetCounter: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      packetPosition <= 0;
    elsif (clk'event and clk = '1') then
      if (readFrame = '1') or (readFrameEmpty_i = '1') then
        packetPosition <= 0;
      elsif (stall_i = '0') then
        packetPosition <= packetPosition + 1;
      end if;
    end if;
  end process;

  headerRegister: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      tt <= "00";
      ftype <= "0000";
    elsif (clk'event and clk = '1') then
      if (readFrame = '1') then
        tt <= "00";
        ftype <= "0000";
      elsif (stall_i = '0') and (packetPosition = 3) then
        tt <= readContentData_i(21 downto 20);
        ftype <= readContentData_i(19 downto 16);
      end if;
    end if;
  end process;

  controller: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      readContent <= '0';
      readFrame <= '0';
      stb_o <= '0';
    elsif (clk'event and clk = '1') then
      if (stall_i = '0') then
        case packetPosition is
          when 0 =>
            readContent <= '0';
            readFrame <= '0';
            stb_o <= '0';
          when 1 =>
            readContent <= '1';
          when 2 =>
            readContent <= '1';
          when 3 =>
            readContent <= '0';
            stb_o <= '1';
          when others =>
            if (readFrame = '0') then
              stb_o <= not readContentEnd_i;
              readFrame <= readContentEnd_i;
              readContent <= not readContentEnd_i;
            else
              readFrame <= '0';
            end if;
        end case;
      end if;
    end if;
  end process;

end architecture;



-------------------------------------------------------------------------------
-- RioLogicalCommonEgress.
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

    stb_i : in std_logic;
    adr_i : in std_logic;
    dat_i : in std_logic_vector(31 downto 0);
    stall_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioLogicalCommonEgress16.
-- Only 16-bit deviceId are supported. The first write must contain
-- the 16-bit header, the second write must contain the destination address and
-- the third must contain the source address.
-- CRC is calculated during the transfer and is inserted at byte 81 and 82 and
-- appended to the packet when it ends.
-------------------------------------------------------------------------------
architecture RioLogicalCommonEgress16 of RioLogicalCommonEgress is

  signal stb, cycleEndCurrent, cycleEndNext : std_logic;
  
  signal packetPosition : natural range 0 to 72;

  signal loadValue : std_logic_vector(47 downto 0);
  signal packetContent : std_logic_vector(47 downto 0);
  signal packetContentReady : std_logic;
  signal packetContentOdd : std_logic;
  signal packetContentLong : std_logic;
  signal packetContentEnd : std_logic;
  signal packetContentPending : std_logic;

  signal writeContent : std_logic;
  signal writeFrame : std_logic;
  signal writeContentData : std_logic_vector(31 downto 0);

  signal crcCurrent, crcTemp, crcNext: std_logic_vector(15 downto 0);
  
begin

  -----------------------------------------------------------------------------
  -- Packet cycle end detection.
  -----------------------------------------------------------------------------
  stbDelayFF: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      stb <= '0';
    elsif (clk'event and clk = '1') then
      if (writeFrame = '1') then
        stb <= '0';
      elsif (writeFrameFull_i = '0') then
        stb <= stb_i;
      end if;
    end if;
  end process;
  cycleEndNext <= (stb and (not stb_i));
  cycleEndFF: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      cycleEndCurrent <= '0';
    elsif (clk'event and clk = '1') then
      if (writeFrame = '1') then
        cycleEndCurrent <= '0';
      elsif (cycleEndNext = '1') then
        cycleEndCurrent <= '1';
      end if;
    end if;
  end process;
  packetContentEnd <= cycleEndNext or cycleEndCurrent;

  -----------------------------------------------------------------------------
  -- Packet positioning.
  -----------------------------------------------------------------------------
  packetPositionCounter: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      packetPosition <= 0;
    elsif (clk'event and clk = '1') then
      if (writeFrame = '1') then
        packetPosition <= 0;
      elsif (stb_i = '1') and (writeFrameFull_i = '0') then
        packetPosition <= packetPosition + 1;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Packet content creation.
  -----------------------------------------------------------------------------
  -- REMARK: The critical path is the crcNext through the loadValue-mux into
  -- packetContent. Register this path if possible.
  loadValue <=
    (packetContent(31 downto 0) & dat_i(15 downto 0)) when (packetContentReady = '0') else    
    (packetContent(15 downto 0) & dat_i) when (packetContentLong = '0') else    
    (crcNext & packetContent(15 downto 0) & x"0000") when (packetContentPending = '1') else    
    (dat_i & x"0000");
  packetContentPlace: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      packetContent <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (stb_i = '1') or (stb = '1') then
        packetContent <= loadValue;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Packet content generation controller.
  -----------------------------------------------------------------------------
  stall_o <= writeFrameFull_i when (packetContentReady = '0') else
             packetContentPending or packetContentEnd;
  controller: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      packetContentReady <= '0';
      packetContentPending <= '0';
      packetContentLong <= '0';
      packetContentOdd <= '0';
    elsif (clk'event and clk = '1') then
      if (writeFrame = '1') then
        packetContentReady <= '0';
        packetContentPending <= '0';
        packetContentLong <= '0';
        packetContentOdd <= adr_i;
      elsif (stb_i = '1') and (writeFrameFull_i = '0') then
        packetContentOdd <= adr_i;
        
        case packetPosition is
          when 2 =>
            packetContentReady <= '1';
          when 21 =>
            packetContentPending <= '1';
            packetContentLong <= '1';
          when 22 =>
            packetContentPending <= '0';
            packetContentLong <= '1';
          when others =>
        end case;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- CRC calculation and interface towards the packet queue.
  -----------------------------------------------------------------------------
  crcCalculation: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      crcCurrent <= x"0000";
    elsif (clk'event and clk = '1') then
      if (packetContentReady = '0') then
        crcCurrent <= x"ffff";
      elsif (packetContentReady = '1') then
        crcCurrent <= crcNext;
      end if;
    end if;
  end process;
  Crc16High: Crc16CITT
    port map(
      d_i=>packetContent(47 downto 32), crc_i=>crcCurrent, crc_o=>crcTemp);
  Crc16Low: Crc16CITT
    port map(
      d_i=>packetContent(31 downto 16), crc_i=>crcTemp, crc_o=>crcNext);

  -----------------------------------------------------------------------------
  -- Frame buffer output interface.
  -----------------------------------------------------------------------------
  -- REMARK: This process needs to be optimized further. It is not part of the critical
  -- path though.
  writeFrameContent: process(clk, areset_n)
    variable flush : std_logic;
    variable appendCrc : std_ulogic;
    variable appendHigh : std_ulogic;
    variable endFrame : std_ulogic;
  begin
    if (areset_n = '0') then
      writeFrame <= '0';
      writeContent <= '0';
      writeContentData <= (others=>'0');
      flush := '0';
      appendCrc := '0';
      appendHigh := '0';
      endFrame := '0';
    elsif (clk'event and clk = '1') then
      if (writeFrame = '1') then
        writeFrame <= '0';
        writeContent <= '0';
        writeContentData <= (others=>'0');
        flush := '0';
        appendCrc := '0';
        appendHigh := '0';
        endFrame := '0';
      else
        if (flush = '1') then
          writeContent <= '1';
          writeContentData <= packetContent(47 downto 16);
          flush := '0';
        elsif (appendCrc = '1') then
          writeContent <= '1';
          if (appendHigh = '0') then
            writeContentData <= packetContent(47 downto 32) & crcTemp;
          else
            writeContentData <= crcCurrent & x"0000";
          end if;
          appendCrc := '0';
        elsif (endFrame = '1') then
          writeContent <= '0';
          writeFrame <= '1';
          endFrame := '0';
        elsif (packetContentPending = '1') and (packetContentEnd = '1') then
          writeContent <= '1';
          writeContentData <= packetContent(47 downto 16);
          flush := not packetContentOdd;
          appendCrc := '1';
          appendHigh := '1';
          endFrame := '1';
        elsif (packetContentEnd = '1') then
          if (packetContentLong = '0') then
            writeContent <= '1';
            writeContentData <= packetContent(47 downto 16);
            flush := '0';
            appendCrc := '1';
            appendHigh := packetContentOdd;
            endFrame := '1';
          else
            if (packetContentOdd = '1') then
              writeContent <= '1';
              writeContentData <= packetContent(47 downto 32) & crcTemp;
              flush := '0';
              appendCrc := '0';
              appendHigh := '0';
              endFrame := '1';
            else
              writeContent <= '1';
              writeContentData <= packetContent(47 downto 16);
              flush := '0';
              appendCrc := '1';
              appendHigh := '1';
              endFrame := '1';
            end if;
          end if;
        elsif (packetContentReady = '1') then
          writeContent <= '1';
          writeContentData <= packetContent(47 downto 16);
        else
          writeContent <= '0';
          writeFrame <= '0';
        end if;
      end if;
    end if;
  end process;
  
  writeContent_o <= writeContent;
  writeFrame_o <= writeFrame;
  writeFrameAbort_o <= '0';
  writeContentData_o <= writeContentData;
  
end architecture;



--------------------------------------------------------------------------------
-- RioLogicalCommonInterconnect.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 
use work.rio_common.all;


-------------------------------------------------------------------------------
-- Entity for RioLogicalCommonInterconnect.
-------------------------------------------------------------------------------
entity RioLogicalCommonInterconnect is
  generic(
    WIDTH : natural);
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    stb_i : in std_logic_vector(WIDTH-1 downto 0);
    adr_i : in std_logic_vector(WIDTH-1 downto 0);
    data_i : in std_logic_vector(32*WIDTH-1 downto 0);
    stall_o : out std_logic_vector(WIDTH-1 downto 0);

    stb_o : out std_logic;
    adr_o : out std_logic;
    data_o : out std_logic_vector(31 downto 0);
    stall_i : in std_logic);
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioLogicalCommonInterconnect.
-------------------------------------------------------------------------------
architecture RioLogicalCommonInterconnectImpl of RioLogicalCommonInterconnect is
  signal activeCycle : std_logic := '0';
  signal selectedMaster : natural range 0 to WIDTH-1 := 0;
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
  adr_o <= adr_i(selectedMaster);
  data_o <= data_i(32*(selectedMaster+1)-1 downto 32*selectedMaster);

  Interconnect: for i in 0 to WIDTH-1 generate
    stall_o(i) <= stall_i when (selectedMaster = i) and (activeCycle = '1') else '1';
  end generate;

end architecture;


