-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Containing RapidIO packet parsers and generators.
-- 
-- To Do:
-- -
-- 
-- Author(s): 
-- - Magnus Rosenius, magro732@opencores.org 
-- 
-------------------------------------------------------------------------------
-- 
-- Copyright (C) 2014 Authors and OPENCORES.ORG 
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
-- MaintenanceInbound
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- Entity for MaintenanceInbound.
-------------------------------------------------------------------------------
entity MaintenanceInbound is
  generic(
    ENABLE_READ_REQUEST : boolean := true;
    ENABLE_WRITE_REQUEST : boolean := true;
    ENABLE_READ_RESPONSE : boolean := true;
    ENABLE_WRITE_RESPONSE : boolean := true;
    ENABLE_PORT_WRITE : boolean := true);
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    readRequestReady_o : out std_logic;
    writeRequestReady_o : out std_logic;
    readResponseReady_o : out std_logic;
    writeResponseReady_o : out std_logic;
    portWriteReady_o : out std_logic;
    vc_o : out std_logic;
    crf_o : out std_logic;
    prio_o : out std_logic_vector(1 downto 0);
    tt_o : out std_logic_vector(1 downto 0);
    dstid_o : out std_logic_vector(31 downto 0);
    srcid_o : out std_logic_vector(31 downto 0);
    size_o : out std_logic_vector(3 downto 0);
    status_o : out std_logic_vector(3 downto 0);
    tid_o : out std_logic_vector(7 downto 0);
    hop_o : out std_logic_vector(7 downto 0);
    offset_o : out std_logic_vector(20 downto 0);
    wdptr_o : out std_logic;
    payloadLength_o : out std_logic_vector(2 downto 0);
    payloadIndex_i : in std_logic_vector(2 downto 0);
    payload_o : out std_logic_vector(63 downto 0);
    done_i : in std_logic;
    
    inboundStb_i : in std_logic;
    inboundAdr_i : in std_logic_vector(3 downto 0);
    inboundDat_i : in std_logic_vector(31 downto 0);
    inboundStall_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- Architecture for MaintenanceInbound.
-------------------------------------------------------------------------------
architecture MaintenanceInbound of MaintenanceInbound is

  signal busy : std_ulogic;
  signal transaction : std_logic_vector(3 downto 0);
  signal payloadIndex : unsigned(2 downto 0);
  signal packetIndex : natural range 0 to 21;
  signal packetData : std_logic_vector(47 downto 0);

  signal readRequestComplete : std_logic;
  signal writeRequestComplete : std_logic;
  signal readResponseComplete : std_logic;
  signal writeResponseComplete : std_logic;
  signal portWriteComplete : std_logic;

  signal memoryWrite : std_logic;
  signal memoryAddress : std_logic_vector(2 downto 0);
  signal memoryDataIn : std_logic_vector(63 downto 0);

begin

  readRequestReady_o <= readRequestComplete and busy;
  writeRequestReady_o <= writeRequestComplete and busy;
  readResponseReady_o <= readResponseComplete and busy;
  writeResponseReady_o <= writeResponseComplete and busy;
  portWriteReady_o <= portWriteComplete and busy;

  payloadLength_o <= std_logic_vector(payloadIndex);

  inboundStall_o <= busy when ((inboundStb_i = '1') and (inboundAdr_i = x"8")) else '0';
  
  MaintenanceRequest: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      readRequestComplete <= '0';
      writeRequestComplete <= '0';
      readResponseComplete <= '0';
      writeResponseComplete <= '0';
      portWriteComplete <= '0';
      
      vc_o <= '0';
      crf_o <= '0';
      prio_o <= "00";
      tt_o <= "00";
      dstid_o <= (others=>'0');
      srcid_o <= (others=>'0');
      status_o <= (others=>'0');
      tid_o <= (others=>'0');
      hop_o <= (others=>'0');
      offset_o <= (others=>'0');

      wdptr_o <= '0';
      size_o <= (others=>'0');

      busy <= '0';
      transaction <= "0000";
      payloadIndex <= "000";
      
      packetIndex <= 0;
      memoryWrite <= '0';
      memoryAddress <= (others=>'0');
      memoryDataIn <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        if (busy = '0') then
          ---------------------------------------------------------------------
          -- This state waits for a new maintenance packet, receives it
          -- and parses it.
          ---------------------------------------------------------------------
          if (inboundStb_i = '1') and (inboundAdr_i = x"8") then
            -- New inbound packet content.
            
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
                dstid_o <= inboundDat_i;
                packetIndex <= packetIndex + 1;

              when 2 =>
                -- srcid
                srcid_o <= inboundDat_i;
                packetIndex <= packetIndex + 1;

              when 3 =>
                -- READ-REQUEST:   transaction & rdsize & srcTID & hop & config_offset(20:13)
                -- WRITE-REQUEST:  transaction & wrsize & srcTID & hop & config_offset(20:13)
                -- READ-RESPONSE:  transaction & status & srcTID & hop & reserved(7:0)
                -- WRITE-RESPONSE: transaction & status & srcTID & hop & reserved(7:0)
                -- PORT-WRITE:     transaction & status & reserved(7:0) & hop & reserved(7:0)
                transaction <= inboundDat_i(31 downto 28);
                size_o <= inboundDat_i(27 downto 24);
                status_o <= inboundDat_i(27 downto 24);
                tid_o <= inboundDat_i(23 downto 16);
                hop_o <= inboundDat_i(15 downto 8);
                offset_o(20 downto 13) <= inboundDat_i(7 downto 0);

                packetIndex <= packetIndex + 1;

              when 4 =>
                -- NO-PAYLOAD:   config_offset(12:0) & wdptr & rsrv(1:0) & crc(15:0)
                -- WITH-PAYLOAD: config_offset(12:0) & wdptr & rsrv(1:0) & double-word0(63:48)
                offset_o(12 downto 0) <= inboundDat_i(31 downto 19);
                wdptr_o <= inboundDat_i(18);
                packetData(47 downto 32) <= inboundDat_i(15 downto 0);

                if (ENABLE_READ_REQUEST and (transaction = TTYPE_MAINTENANCE_READ_REQUEST)) then
                  readRequestComplete <= '1';
                elsif (ENABLE_WRITE_RESPONSE and (transaction = TTYPE_MAINTENANCE_WRITE_RESPONSE)) then
                  writeResponseComplete <= '1';
                end if;

                packetIndex <= packetIndex + 1;
                
              when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 =>
                -- double-word(47:16)
                packetData(31 downto 0) <= inboundDat_i;
                memoryWrite <= '0';
                packetIndex <= packetIndex + 1;

              when 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 =>
                -- double-word(15:0) & double-word(63:48)
                packetData(47 downto 32) <= inboundDat_i(15 downto 0);
                payloadIndex <= payloadIndex + 1;

                memoryWrite <= '1';
                memoryAddress <= std_logic_vector(payloadIndex);
                memoryDataIn <= packetData & inboundDat_i(31 downto 16);

                if (ENABLE_WRITE_REQUEST and (transaction = TTYPE_MAINTENANCE_WRITE_REQUEST)) then
                  writeRequestComplete <= '1';
                elsif (ENABLE_READ_RESPONSE and (transaction = TTYPE_MAINTENANCE_READ_RESPONSE)) then
                  readResponseComplete <= '1';
                elsif (ENABLE_PORT_WRITE and (transaction = TTYPE_MAINTENANCE_PORT_WRITE)) then
                  -- REMARK: Do this when 2 double words has been received?
                  portWriteComplete <= '1';
                end if;

                packetIndex <= packetIndex + 1;

              when others =>
                -- There should be no more content in a maintenance packet.
                -- Discard.
                report "MaintenanceClass: Received unexpected packet content." severity warning;
                
            end case;
          else
            -- No incoming packet content.

            -- Make sure there is no write access anymore.
            memoryWrite <= '0';

            -- Check if a packet has been completed.
            if ((readRequestComplete = '1') or (writeRequestComplete = '1') or
                (readResponseComplete = '1') or (writeResponseComplete = '1') or
                (portWriteComplete = '1')) then
              -- Packet completed.
              busy <= '1';
            else
              -- No packet completed.
              packetIndex <= 0;
              payloadIndex <= (others=>'0');
            end if;
          end if;
        else
          ---------------------------------------------------------------------
          -- Stall any incoming maintenance packet until the current has been
          -- processed and wait for the handler of the packet to signal that it
          -- has been processed.
          ---------------------------------------------------------------------
          if (done_i = '1') then
            busy <= '0';
            packetIndex <= 0;
            payloadIndex <= (others=>'0');
            
            readRequestComplete <= '0';
            writeRequestComplete <= '0';
            readResponseComplete <= '0';
            writeResponseComplete <= '0';
            portWriteComplete <= '0';
          end if;
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Payload content memory.
  -----------------------------------------------------------------------------
  PayloadMemory: MemorySimpleDualPort
    generic map(ADDRESS_WIDTH=>3, DATA_WIDTH=>64)
    port map(clkA_i=>clk,
             enableA_i=>memoryWrite,
             addressA_i=>memoryAddress,
             dataA_i=>memoryDataIn,
             clkB_i=>clk,
             enableB_i=>enable,
             addressB_i=>payloadIndex_i,
             dataB_o=>payload_o);

end architecture;



-------------------------------------------------------------------------------
-- RequestClassInbound.
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

    nreadReady_o : out std_logic;
    
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
    
    inboundStb_i : in std_logic;
    inboundAdr_i : in std_logic_vector(3 downto 0);
    inboundDat_i : in std_logic_vector(31 downto 0);
    inboundStall_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RequestClassInbound of RequestClassInbound is
  signal busy : std_logic;
  signal transaction : std_logic_vector(3 downto 0);
  signal complete : std_logic;
  signal packetIndex : natural range 0 to 69;

  signal rdsize : std_logic_vector(3 downto 0);
  signal wdptr : std_logic;

begin

  nreadReady_o <= complete and busy;

  inboundStall_o <= busy when ((inboundStb_i = '1') and (inboundAdr_i = x"2")) else '0';

  RequestClass: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      complete <= '0';
      transaction <= "0000";
      
      vc_o <= '0';
      crf_o <= '0';
      prio_o <= "00";
      tt_o <= "00";
      dstId_o <= (others=>'0');
      srcId_o <= (others=>'0');
      tid_o <= (others=>'0');
      address_o <= (others=>'0');

      rdsize <= (others=>'0');
      wdptr <= '0';
      
      busy <= '0';
      packetIndex <= 0;
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        if (busy = '0') then
          ---------------------------------------------------------------------
          -- This state waits for a new REQUEST class packet, receives it
          -- and parses it.
          ---------------------------------------------------------------------
          if ((inboundStb_i = '1') and (inboundAdr_i = x"2")) then
            -- New inbound packet content.
            
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
                transaction <= inboundDat_i(31 downto 28);
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
                
                if (transaction = TTYPE_NREAD_TRANSACTION) then
                  -- An NREAD packet has been completed.
                  complete <= '1';
                end if;
                
              when others =>
                -- There should be no more content in a REQUEST.
                -- Discard.
                report "RequestClass: Received unexpected packet content." severity warning;
                
            end case;
          else
            -- No incoming packet content.
            busy <= complete;
          end if;
        else
          ---------------------------------------------------------------------
          -- Stall any incoming REQUEST packet until the current has been
          -- processed and wait for the handler of the packet to signal that it
          -- has been processed.
          ---------------------------------------------------------------------
          if (done_i = '1') then
            busy <= '0';
            packetIndex <= 0;
            complete <= '0';
          end if;
        end if;
      end if;
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
              length_o <= "00001";
              select_o <= "10000000";
            when "0001" =>
              length_o <= "00001";
              select_o <= "01000000";
            when "0010" =>
              length_o <= "00001";
              select_o <= "00100000";
            when "0011" =>
              length_o <= "00001";
              select_o <= "00010000";
            when "0100" =>
              length_o <= "00001";
              select_o <= "11000000";
            when "0101" =>
              length_o <= "00001";
              select_o <= "11100000";
            when "0110" =>
              length_o <= "00001";
              select_o <= "00110000";
            when "0111" =>
              length_o <= "00001";
              select_o <= "11111000";
            when "1000" =>
              length_o <= "00001";
              select_o <= "11110000";
            when "1001" =>
              length_o <= "00001";
              select_o <= "11111100";
            when "1010" =>
              length_o <= "00001";
              select_o <= "11111110";
            when "1011" =>
              length_o <= "00001";
              select_o <= "11111111";
            when "1100" =>
              length_o <= "00100";
              select_o <= "11111111";
            when "1101" =>
              length_o <= "01100";
              select_o <= "11111111";
            when "1110" =>
              length_o <= "10100";
              select_o <= "11111111";
            when others =>
              length_o <= "11100";
              select_o <= "11111111";
          end case;
        else
          case rdsize is
            when "0000" =>
              length_o <= "00001";
              select_o <= "00001000";
            when "0001" =>
              length_o <= "00001";
              select_o <= "00000100";
            when "0010" =>
              length_o <= "00001";
              select_o <= "00000010";
            when "0011" =>
              length_o <= "00001";
              select_o <= "00000001";
            when "0100" =>
              length_o <= "00001";
              select_o <= "00001100";
            when "0101" =>
              length_o <= "00001";
              select_o <= "00000111";
            when "0110" =>
              length_o <= "00001";
              select_o <= "00000011";
            when "0111" =>
              length_o <= "00001";
              select_o <= "00011111";
            when "1000" =>
              length_o <= "00001";
              select_o <= "00001111";
            when "1001" =>
              length_o <= "00001";
              select_o <= "00111111";
            when "1010" =>
              length_o <= "00001";
              select_o <= "01111111";
            when "1011" =>
              length_o <= "00010";
              select_o <= "11111111";
            when "1100" =>
              length_o <= "01000";
              select_o <= "11111111";
            when "1101" =>
              length_o <= "10000";
              select_o <= "11111111";
            when "1110" =>
              length_o <= "11000";
              select_o <= "11111111";
            when others =>
              length_o <= "00000";
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
-- REMARK: Add support for extended addresses...
-- length_o is the actual size of the access. A 32 double-word access has length=0.
entity WriteClassInbound is
  generic(
    ENABLE_NWRITE : boolean := true;
    ENABLE_NWRITER : boolean := true;
    EXTENDED_ADDRESS : natural range 0 to 2 := 0);
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    nwriteReady_o : out std_logic;
    nwriterReady_o : out std_logic;
    
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
    payloadIndex_i : in std_logic_vector(4 downto 0);
    payload_o : out std_logic_vector(63 downto 0);
    done_i : in std_logic;
    
    inboundStb_i : in std_logic;
    inboundAdr_i : in std_logic_vector(3 downto 0);
    inboundDat_i : in std_logic_vector(31 downto 0);
    inboundStall_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture WriteClassInbound of WriteClassInbound is
  
  signal busy : std_logic;
  signal transaction : std_logic_vector(3 downto 0);
  signal payloadIndex : unsigned(4 downto 0);
  signal packetIndex : natural range 0 to 69;
  signal doubleWord : std_logic_vector(63 downto 16);

  signal nwriteComplete : std_logic;
  signal nwriterComplete : std_logic;

  signal wdptr : std_logic;
  signal wrsize : std_logic_vector(3 downto 0);
  
  signal memoryWrite : std_logic;
  signal memoryAddress : std_logic_vector(4 downto 0);
  signal memoryDataIn : std_logic_vector(63 downto 0);
  
begin

  nwriteReady_o <= nwriteComplete and busy;
  nwriterReady_o <= nwriterComplete and busy;

  inboundStall_o <= busy when ((inboundStb_i = '1') and (inboundAdr_i = x"5")) else '0';

  WriteClass: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      nwriteComplete <= '0';
      nwriterComplete <= '0';
      
      vc_o <= '0';
      crf_o <= '0';
      prio_o <= "00";
      tt_o <= "00";
      dstId_o <= (others=>'0');
      srcId_o <= (others=>'0');
      tid_o <= (others=>'0');
      address_o <= (others=>'0');

      busy <= '0';
      transaction <= "0000";
      payloadIndex <= (others=>'0');
      packetIndex <= 0;
      doubleWord <= (others=>'0');

      wdptr <= '0';
      wrsize <= (others=>'0');
      
      memoryWrite <= '0';
      memoryAddress <= (others=>'0');
      memoryDataIn <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        if (busy = '0') then
          ---------------------------------------------------------------------
          -- This state waits for a new maintenance packet, receives it
          -- and parses it.
          ---------------------------------------------------------------------
          if ((inboundStb_i = '1') and (inboundAdr_i = x"5")) then
            -- New inbound packet content.

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
                transaction <= inboundDat_i(31 downto 28);
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
                memoryWrite <= '0';
                packetIndex <= packetIndex + 1;
                
              when 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 | 32  | 34 |
                36 | 38 | 40 | 42 | 44 | 46 | 48 | 50 | 52 | 54 | 56 | 58 | 60 | 62 | 64 | 66 | 68 =>
                -- double-word(15:0) & double-word(63:48)
                doubleWord(63 downto 48) <= inboundDat_i(15 downto 0);
                payloadIndex <= payloadIndex + 1;

                memoryWrite <= '1';
                memoryAddress <= std_logic_vector(payloadIndex);
                memoryDataIn <= doubleWord(63 downto 16) & inboundDat_i(31 downto 16);
                
                if (ENABLE_NWRITE and (transaction = TTYPE_NWRITE_TRANSACTION)) then
                  nwriteComplete <= '1';
                elsif (ENABLE_NWRITER and (transaction = TTYPE_NWRITER_TRANSACTION)) then
                  nwriterComplete <= '1';
                end if;
                
                packetIndex <= packetIndex + 1;
                
              when others =>
                -- There should be no more content in an NWRITE/NWRITER request.
                -- Discard.
                report "WriteClass: Received unexpected packet content." severity warning;
                
            end case;
          else
            -- No incoming packet content.

            -- Make sure there is no write access anymore.
            memoryWrite <= '0';

            -- Check if a packet has been completed.
            if (nwriteComplete = '1') or (nwriteRComplete = '1') then
              -- Packet completed.
              busy <= '1';
            else
              -- No packet completed.
              packetIndex <= 0;
              payloadIndex <= (others=>'0');
            end if;
          end if;
        else
          ---------------------------------------------------------------------
          -- Stall any incoming write packet until the current has been
          -- processed and wait for the handler of the packet to signal that it
          -- has been processed.
          ---------------------------------------------------------------------
          if (done_i = '1') then
            busy <= '0';
            packetIndex <= 0;
            payloadIndex <= (others=>'0');
            
            nwriteComplete <= '0';
            nwriterComplete <= '0';
          end if;
        end if;
      end if;
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
      if ((nwriteComplete = '1') or (nwriterComplete = '1')) then
        if (wdptr = '0') then
          case wrsize is
            when "0000" =>
              length_o <= "00001";
              select_o <= "10000000";
            when "0001" =>
              length_o <= "00001";
              select_o <= "01000000";
            when "0010" =>
              length_o <= "00001";
              select_o <= "00100000";
            when "0011" =>
              length_o <= "00001";
              select_o <= "00010000";
            when "0100" =>
              length_o <= "00001";
              select_o <= "11000000";
            when "0101" =>
              length_o <= "00001";
              select_o <= "11100000";
            when "0110" =>
              length_o <= "00001";
              select_o <= "00110000";
            when "0111" =>
              length_o <= "00001";
              select_o <= "11111000";
            when "1000" =>
              length_o <= "00001";
              select_o <= "11110000";
            when "1001" =>
              length_o <= "00001";
              select_o <= "11111100";
            when "1010" =>
              length_o <= "00001";
              select_o <= "11111110";
            when "1011" =>
              length_o <= "00001";
              select_o <= "11111111";
            when others =>
              length_o <= std_logic_vector(payloadIndex);
              select_o <= "11111111";
          end case;
        else
          case wrsize is
            when "0000" =>
              length_o <= "00001";
              select_o <= "00001000";
            when "0001" =>
              length_o <= "00001";
              select_o <= "00000100";
            when "0010" =>
              length_o <= "00001";
              select_o <= "00000010";
            when "0011" =>
              length_o <= "00001";
              select_o <= "00000001";
            when "0100" =>
              length_o <= "00001";
              select_o <= "00001100";
            when "0101" =>
              length_o <= "00001";
              select_o <= "00000111";
            when "0110" =>
              length_o <= "00001";
              select_o <= "00000011";
            when "0111" =>
              length_o <= "00001";
              select_o <= "00011111";
            when "1000" =>
              length_o <= "00001";
              select_o <= "00001111";
            when "1001" =>
              length_o <= "00001";
              select_o <= "00111111";
            when "1010" =>
              length_o <= "00001";
              select_o <= "01111111";
            when others =>
              length_o <= std_logic_vector(payloadIndex);
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
             enableB_i=>'1',
             addressB_i=>payloadIndex_i,
             dataB_o=>payload_o);

end architecture;



-------------------------------------------------------------------------------
-- MaintenanceOutbound.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;

-------------------------------------------------------------------------------
-- Entity for MaintenanceOutbound.
-------------------------------------------------------------------------------
entity MaintenanceOutbound is
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    readRequestReady_i : in std_logic;
    writeRequestReady_i : in std_logic;
    readResponseReady_i : in std_logic;
    writeResponseReady_i : in std_logic;
    portWriteReady_i : in std_logic;
    
    vc_i : in std_logic;
    crf_i : in std_logic;
    prio_i : in std_logic_vector(1 downto 0);
    tt_i : in std_logic_vector(1 downto 0);
    dstid_i : in std_logic_vector(31 downto 0);
    srcid_i : in std_logic_vector(31 downto 0);
    size_i : in std_logic_vector(3 downto 0);
    status_i : in std_logic_vector(3 downto 0);
    tid_i : in std_logic_vector(7 downto 0);
    hop_i : in std_logic_vector(7 downto 0);
    offset_i : in std_logic_vector(20 downto 0);
    wdptr_i : in std_logic;
    payloadLength_i : in std_logic_vector(2 downto 0);
    payloadIndex_o : out std_logic_vector(2 downto 0);
    payload_i : in std_logic_vector(63 downto 0);
    done_o : out std_logic;
    
    outboundStb_o : out std_logic;
    outboundAdr_o : out std_logic;
    outboundDat_o : out std_logic_vector(31 downto 0);
    outboundStall_i : in std_logic);
end entity;


-------------------------------------------------------------------------------
-- Architecture for MaintenanceOutbound.
-------------------------------------------------------------------------------
-- REMARK: use the same variable for status and size internally...
architecture MaintenanceOutbound of MaintenanceOutbound is
  type StateType is (WAIT_PACKET, SEND_PACKET, WAIT_COMPLETE);
  signal state : StateType;
  signal packetIndex : natural range 0 to 21;
  
  signal payloadIndex : std_logic_vector(2 downto 0);
  signal payload : std_logic_vector(47 downto 0);
    
begin

  payloadIndex_o <= payloadIndex;

  outboundAdr_o <= '1';
  
  MaintenanceResponse: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      state <= WAIT_PACKET;
      packetIndex <= 0;

      payload <= (others=>'0');
      payloadIndex <= (others=>'0');
      
      outboundStb_o <= '0';
      outboundDat_o <= (others=>'0');
      done_o <= '0';
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        case state is
          when WAIT_PACKET =>
            -------------------------------------------------------------------
            -- 
            -------------------------------------------------------------------
            outboundStb_o <= '0';
            payloadIndex <= (others=>'0');
            if ((readRequestReady_i = '1') or (writeRequestReady_i = '1') or
                (readResponseReady_i = '1') or (writeResponseReady_i = '1') or
                (portWriteReady_i = '1')) then
              state <= SEND_PACKET;
            end if;

            -- unused(31:16) | ackId(15:10) | vc(9) | crf(8) | prio(7:6) | tt(5:4) | ftype(3:0).
            outboundDat_o <= x"0000" & "000000" & vc_i & crf_i & prio_i & tt_i & x"8";
            packetIndex <= 1;
            
          when SEND_PACKET =>
            -------------------------------------------------------------------
            -- 
            -------------------------------------------------------------------
            outboundStb_o <= '1';
            if (outboundStall_i = '0') then
              case (packetIndex) is

                when 1 =>
                  -- dstid
                  outboundDat_o <= dstid_i;
                  packetIndex <= packetIndex + 1;
              
                when 2 =>
                  -- srcid 
                  outboundDat_o <= srcid_i;
                  packetIndex <= packetIndex + 1;
                  
                when 3 =>
                  -- transaction & size & srcTID & hop & config_offset(20:13)
                  -- transaction & status & targetTID & hop & reserved(20:13)
                  -- transaction & status & reserved(7:0) & hop & reserved(20:13)
                  if (readRequestReady_i = '1') then
                    outboundDat_o <= TTYPE_MAINTENANCE_READ_REQUEST & size_i & tid_i & hop_i & offset_i(20 downto 13);
                  elsif (writeRequestReady_i = '1') then
                    outboundDat_o <= TTYPE_MAINTENANCE_WRITE_REQUEST & size_i & tid_i & hop_i & offset_i(20 downto 13);
                  elsif (readResponseReady_i = '1') then
                    outboundDat_o <= TTYPE_MAINTENANCE_READ_RESPONSE & status_i & tid_i & hop_i & x"00";
                  elsif (writeResponseReady_i = '1') then
                    outboundDat_o <= TTYPE_MAINTENANCE_WRITE_RESPONSE & status_i & tid_i & hop_i & x"00";
                  elsif (portWriteReady_i = '1') then
                    outboundDat_o <= TTYPE_MAINTENANCE_PORT_WRITE & status_i & x"00" & hop_i & x"00";
                  end if;
                  
                  packetIndex <= packetIndex + 1;
                  
                when 4 =>
                  -- READ-REQUEST:   config_offset(12:0) & wdptr & rsrv & crc(15:0)
                  -- WRITE-REQUEST:  config_offset(12:0) & wdptr & rsrv & double-wordN(63:48)
                  -- READ-RESPONSE:  reserved(15:0) & double-wordN(63:48)
                  -- WRITE-RESPONSE: reserved(15:0) & crc(15:0)
                  -- PORT-WRITE:     reserved(15:0) & crc(15:0)
                  if (readRequestReady_i = '1') then
                    outboundDat_o <= offset_i(12 downto 0) & wdptr_i & "00" & payload_i(63 downto 48);
                    state <= WAIT_COMPLETE;
                  elsif (writeRequestReady_i = '1') then
                    outboundDat_o <= offset_i(12 downto 0) & wdptr_i & "00" & payload_i(63 downto 48);
                  elsif (readResponseReady_i = '1') then
                    outboundDat_o <= x"0000" & payload_i(63 downto 48);
                  elsif (writeResponseReady_i = '1') then
                    outboundDat_o <= x"0000" & payload_i(63 downto 48);
                    state <= WAIT_COMPLETE;
                  elsif (portWriteReady_i = '1') then
                    outboundDat_o <= x"0000" & payload_i(63 downto 48);
                  end if;

                  payload <= payload_i(47 downto 0);
                  payloadIndex <= std_logic_vector(unsigned(payloadIndex) + 1);
                  
                  packetIndex <= packetIndex + 1;
                  
                when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 =>
                  -- double-wordN(47:16)
                  outboundDat_o <= payload(47 downto 16);
                  packetIndex <= packetIndex + 1;
                  
                when 6 | 8 | 10 | 12 | 14 | 16 | 18 =>
                  -- double-wordN(15:0) & double-wordN(63:48)
                  outboundDat_o <= payload(15 downto 0) & payload_i(63 downto 48);

                  payload <= payload_i(47 downto 0);
                  payloadIndex <= std_logic_vector(unsigned(payloadIndex) + 1);
                  if (payloadIndex = payloadLength_i) then
                    state <= WAIT_COMPLETE;
                  end if;

                  packetIndex <= packetIndex + 1;
                  
                when others =>
                  -- double-wordN(15:0) & double-wordN(63:48)
                  outboundDat_o <= payload(15 downto 0) & x"0000";
                  state <= WAIT_COMPLETE;
                  
              end case;
            end if;

          when WAIT_COMPLETE =>
            -------------------------------------------------------------------
            -- 
            -------------------------------------------------------------------
            outboundStb_o <= '0';
            if ((readRequestReady_i = '0') and (writeRequestReady_i = '0') and
                (readResponseReady_i = '0') and (writeResponseReady_i = '0') and
                (portWriteReady_i = '0')) then
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

end architecture;



-------------------------------------------------------------------------------
-- ResponseClassOutbound
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

    doneNoPayloadReady_i : in std_logic;
    doneWithPayloadReady_i :  in std_logic;
    errorReady_i : in std_logic;
    
    vc_i : in std_logic;
    crf_i : in std_logic;
    prio_i : in std_logic_vector(1 downto 0);
    tt_i : in std_logic_vector(1 downto 0);
    dstid_i : in std_logic_vector(31 downto 0);
    srcid_i : in std_logic_vector(31 downto 0);
    tid_i : in std_logic_vector(7 downto 0);
    payloadLength_i : in std_logic_vector(4 downto 0);
    payloadIndex_o : out std_logic_vector(4 downto 0);
    payload_i : in std_logic_vector(63 downto 0);
    done_o : out std_logic;
    
    outboundStb_o : out std_logic;
    outboundAdr_o : out std_logic;
    outboundDat_o : out std_logic_vector(31 downto 0);
    outboundStall_i : in std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture ResponseClassOutbound of ResponseClassOutbound is
  type StateType is (WAIT_PACKET, SEND_PACKET, WAIT_COMPLETE);
  signal state : StateType;
  signal packetIndex : natural range 0 to 68;

  signal payloadIndex : std_logic_vector(4 downto 0);
  signal payload : std_logic_vector(47 downto 0);
    
begin

  payloadIndex_o <= payloadIndex;
  
  outboundAdr_o <= '1';
  
  Response: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      state <= WAIT_PACKET;
      packetIndex <= 0;

      payloadIndex <= (others=>'0');
      payload <= (others=>'0');
      
      outboundStb_o <= '0';
      outboundDat_o <= (others=>'0');
      
      done_o <= '0';
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        case state is
          when WAIT_PACKET =>
            -------------------------------------------------------------------
            -- 
            -------------------------------------------------------------------
            outboundStb_o <= '0';
            payloadIndex <= (others=>'0');
            if ((doneNoPayloadReady_i = '1') or
                (doneWithPayloadReady_i = '1') or
                (errorReady_i = '1')) then
              state <= SEND_PACKET;
            end if;
            
            outboundDat_o <= x"0000" & "000000" & vc_i & crf_i & prio_i & tt_i & x"d";
            packetIndex <= 1;

          when SEND_PACKET =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            outboundStb_o <= '1';
            if (outboundStall_i = '0') then
              case (packetIndex) is
                when 1 =>
                  -- dstid
                  outboundDat_o <= dstId_i;
                  packetIndex <= packetIndex + 1;
                  
                when 2 =>
                  -- srcid 
                  outboundDat_o <= srcId_i;
                  packetIndex <= packetIndex + 1;
                  
                when 3 =>
                  -- transaction & status & targetTID & double-word0(63:48)
                  if (doneNoPayloadReady_i = '1') then
                    outboundDat_o <= TTYPE_RESPONSE_NO_PAYLOAD & "0000" & tid_i & x"0000";
                    state <= WAIT_COMPLETE;
                  elsif (doneWithPayloadReady_i = '1') then
                    outboundDat_o <= TTYPE_RESPONSE_WITH_PAYLOAD & "0000" & tid_i & payload_i(63 downto 48);
                  elsif (errorReady_i = '1') then
                    outboundDat_o <= TTYPE_RESPONSE_NO_PAYLOAD & "0111" & tid_i & x"0000";
                    state <= WAIT_COMPLETE;
                  end if;

                  payload <= payload_i(47 downto 0);
                  payloadIndex <= std_logic_vector(unsigned(payloadIndex) + 1);
                  
                  packetIndex <= packetIndex + 1;
                  
                when 4 | 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 | 26 | 28 | 30 | 32 | 34 |
                  36 | 38 | 40 | 42 | 44 | 46 | 48 | 50 | 52 | 54 | 56 | 58 | 60 | 62 | 64 | 66 =>
                  -- double-wordN(47:16)
                  outboundDat_o <= payload(47 downto 16);
                  packetIndex <= packetIndex + 1;
                  
                when 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19 | 21 | 23 | 25 | 27 | 29 | 31 | 33 | 35 |
                  37 | 39 | 41 | 43 | 45 | 47 | 49 | 51 | 53 | 55 | 57 | 59 | 61 | 63 | 65 | 67 =>
                  -- double-wordN(15:0) & double-wordN(63:48)
                  outboundDat_o <= payload(15 downto 0) & payload_i(63 downto 48);

                  payload <= payload_i(47 downto 0);
                  payloadIndex <= std_logic_vector(unsigned(payloadIndex) + 1);
                  if (payloadIndex = payloadLength_i) then
                    state <= WAIT_COMPLETE;
                  end if;
                  
                  packetIndex <= packetIndex + 1;
                  
                when others =>
                  -- Unallowed response length.
                  -- Dont do anything.
                  
              end case;
            end if;
            
          when WAIT_COMPLETE =>
            -------------------------------------------------------------------
            -- 
            -------------------------------------------------------------------
            outboundStb_o <= '0';
            if ((doneNoPayloadReady_i = '0') and (doneWithPayloadReady_i = '0') and
                (errorReady_i = '0')) then
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

end architecture;
