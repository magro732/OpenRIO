-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Contains a converter of RapidIO maintenance packets into a Wishbone similar
-- access. It relies on the Maintenance-packet modules from
-- RioLogicalPackets.vhd to function.
-- 
-- To Do:
-- - Clean up the code for reading. Works but it is messy.
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
-- them into accesses on a Wishbone similar bus accessing the configuration
-- space.
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

    readRequestReady_i : in std_logic;
    writeRequestReady_i : in std_logic;
    size_i : in std_logic_vector(3 downto 0);
    offset_i : in std_logic_vector(20 downto 0);
    wdptr_i : in std_logic;
    payloadLength_i : in std_logic_vector(2 downto 0);
    payloadIndex_o : out std_logic_vector(2 downto 0);
    payload_i : in std_logic_vector(63 downto 0);
    done_o : out std_logic;
    
    readResponseReady_o : out std_logic;
    writeResponseReady_o : out std_logic;
    status_o : out std_logic_vector(3 downto 0);
    payloadLength_o : out std_logic_vector(2 downto 0);
    payloadIndex_i : in std_logic_vector(2 downto 0);
    payload_o : out std_logic_vector(63 downto 0);
    done_i : in std_logic;
    
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
architecture RioLogicalMaintenance of RioLogicalMaintenance is

  type StateType is (IDLE,
                     CONFIG_READ_START, CONFIG_READ, CONFIG_READ_NEXT, CONFIG_READ_RESPONSE,
                     CONFIG_WRITE_START, CONFIG_WRITE, CONFIG_WRITE_NEXT, CONFIG_WRITE_RESPONSE,
                     WAIT_REQUEST);
  signal state : StateType;

  signal payloadLength : std_logic_vector(3 downto 0);
  signal payloadIndex : std_logic_vector(3 downto 0);
  
  signal payloadWrite : std_logic;
  signal payloadAddress : std_logic_vector(2 downto 0);
  signal payload : std_logic_vector(63 downto 0);
  
  signal configAdr : std_logic_vector(21 downto 0);
  signal configDat : std_logic_vector(31 downto 0);

begin

  configAdr_o <= configAdr;
  configDat_o <= configDat;

  payloadLength_o <= payloadLength(3 downto 1);
  payloadIndex_o <= payloadIndex(3 downto 1);
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  Maintenance: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      state <= IDLE;

      readResponseReady_o <= '0';
      writeResponseReady_o <= '0';
      done_o <= '0';
      
      configStb_o <= '0';
      configWe_o <= '0';
      configAdr <= (others=>'0');
      configDat <= (others=>'0');

      payloadWrite <= '0';
      payloadIndex <= (others=>'0');
      payload <= (others=>'0');
    elsif (clk'event and clk = '1') then
      payloadWrite <= '0';

      case state is
        when IDLE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          payloadIndex <= (others=>'0');
          done_o <= '0';
          if (readRequestReady_i = '1') then
            state <= CONFIG_READ_START;
          elsif (writeRequestReady_i = '1') then
            state <= CONFIG_WRITE_START;
          end if;          

        when CONFIG_READ_START =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          configStb_o <= '1';
          configWe_o <= '0';
          if (size_i = "1000") then
            configAdr <= offset_i & wdptr_i;
          else
            configAdr <= offset_i & '0';
          end if;
          payloadIndex <= "0000";
          payload <= (others=>'0');
          state <= CONFIG_READ;
          
        when CONFIG_READ =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (configAck_i = '1') then
            configStb_o <= '0';
            configAdr <= std_logic_vector(unsigned(configAdr) + 1);
            state <= CONFIG_READ_NEXT;
          end if;

          if (size_i = "1000") and (wdptr_i = '0') then
            payload(63 downto 32) <= configDat_i;
          elsif (size_i = "1000") and (wdptr_i = '1') then
            payload(31 downto 0) <= configDat_i;
          else
            if (payloadIndex(0) = '0') then
              payload(63 downto 32) <= configDat_i;
            else
              payload(31 downto 0) <= configDat_i;
            end if;
          end if;

        when CONFIG_READ_NEXT =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (size_i = "1000") and (wdptr_i = '0') then
            -- 1 word.
            status_o <= "0000";
            payloadLength <= "0010";
            payloadWrite <= '1';
            state <= CONFIG_READ_RESPONSE;
          elsif (size_i = "1000") and (wdptr_i = '1') then
            -- 1 word.
            status_o <= "0000";
            payloadLength <= "0010";
            payloadWrite <= '1';
            state <= CONFIG_READ_RESPONSE;
          elsif (size_i = "1011") and (wdptr_i = '0') then
            -- 2 words.
            status_o <= "0000";
            payloadLength <= "0010";
            payloadWrite <= payloadIndex(0);
            if (payloadIndex = "0001") then
              state <= CONFIG_READ_RESPONSE;
            else
              configStb_o <= '1';
              state <= CONFIG_READ;
            end if;
          elsif (size_i = "1011") and (wdptr_i = '1') then
            -- 4 words.
            status_o <= "0000";
            payloadLength <= "0100";
            payloadWrite <= payloadIndex(0);
            if (payloadIndex = "0011") then
              state <= CONFIG_READ_RESPONSE;
            else
              configStb_o <= '1';
              state <= CONFIG_READ;
            end if;
          elsif (size_i = "1100") and (wdptr_i = '0') then
            -- 8 words.
            status_o <= "0000";
            payloadLength <= "1000";
            payloadWrite <= payloadIndex(0);
            if (payloadIndex = "0111") then
              state <= CONFIG_READ_RESPONSE;
            else
              configStb_o <= '1';
              state <= CONFIG_READ;
            end if;
          elsif (size_i = "1100") and (wdptr_i = '1') then
            -- 16 words.
            status_o <= "0000";
            payloadLength <= "0000";
            payloadWrite <= payloadIndex(0);
            if (payloadIndex = "1111") then
              state <= CONFIG_READ_RESPONSE;
            else
              configStb_o <= '1';
              state <= CONFIG_READ;
            end if;
          else
            -- Unallowed packet.
            -- Send write-response with status indicating error.
            status_o <= "0111";
            state <= CONFIG_READ_RESPONSE;
          end if;
          
          payloadAddress <= payloadIndex(3 downto 1);
          payloadIndex <= std_logic_vector(unsigned(payloadIndex) + 1);

        when CONFIG_READ_RESPONSE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (done_i = '1') then
            readResponseReady_o <= '0';
            state <= WAIT_REQUEST;
          else
            readResponseReady_o <= '1';
          end if;

        when CONFIG_WRITE_START =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          configWe_o <= '1';
          if (size_i = "1000") then
            configAdr <= offset_i & wdptr_i;
          else
            configAdr <= offset_i & '0';
          end if;
          if (size_i = "1000") and (wdptr_i = '0') then
            -- 1 word.
            configStb_o <= '1';
            configDat <= payload_i(63 downto 32);
            payloadLength <= "0001";
            status_o <= "0000";
            state <= CONFIG_WRITE;
          elsif (size_i = "1000") and (wdptr_i = '1') then
            -- 1 word.
            configStb_o <= '1';
            configDat <= payload_i(31 downto 0);
            payloadLength <= "0001";
            status_o <= "0000";
            state <= CONFIG_WRITE;
          elsif (size_i = "1011") and (wdptr_i = '0') then
            -- 2 words.
            configStb_o <= '1';
            configDat <= payload_i(63 downto 32);
            payloadLength <= "0010";
            status_o <= "0000";
            state <= CONFIG_WRITE;
          elsif (size_i = "1011") and (wdptr_i = '1') then
            -- maximum 4 words.
            configStb_o <= '1';
            configDat <= payload_i(63 downto 32);
            payloadLength <= payloadLength_i & '0';
            status_o <= "0000";
            state <= CONFIG_WRITE;
          elsif (size_i = "1100") and (wdptr_i = '0') then
            -- maximum 8 words.
            configStb_o <= '1';
            configDat <= payload_i(63 downto 32);
            payloadLength <= payloadLength_i & '0';
            status_o <= "0000";
            state <= CONFIG_WRITE;
          elsif (size_i = "1100") and (wdptr_i = '1') then
            -- maximum 16 words.
            configStb_o <= '1';
            configDat <= payload_i(63 downto 32);
            payloadLength <= payloadLength_i & '0';
            status_o <= "0000";
            state <= CONFIG_WRITE;
          else
            -- Unallowed packet.
            -- Send write-response with status indicating error.
            status_o <= "0111";
            state <= CONFIG_WRITE_RESPONSE;
          end if;
          payloadIndex <= std_logic_vector(unsigned(payloadIndex) + 1);
          
        when CONFIG_WRITE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (configAck_i = '1') then
            configStb_o <= '0';
            configAdr <= std_logic_vector(unsigned(configAdr) + 1);
            state <= CONFIG_WRITE_NEXT;
          end if;

        when CONFIG_WRITE_NEXT =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (payloadIndex(0) = '0') then
            configDat <= payload_i(63 downto 32);
          else
            configDat <= payload_i(31 downto 0);
          end if;
          
          payloadIndex <= std_logic_vector(unsigned(payloadIndex) + 1);
          if (payloadIndex /= payloadLength) then
            configStb_o <= '1';
            state <= CONFIG_WRITE;
          else
            state <= CONFIG_WRITE_RESPONSE;
          end if;

        when CONFIG_WRITE_RESPONSE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (done_i = '1') then
            writeResponseReady_o <= '0';
            state <= WAIT_REQUEST;
          else
            writeResponseReady_o <= '1';
          end if;

        when WAIT_REQUEST =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          done_o <= '1';
          if (readRequestReady_i = '0') and (writeRequestReady_i = '0') then
            state <= IDLE;
          end if;
        when others =>

      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Payload content memory.
  -----------------------------------------------------------------------------

  PayloadMemory: MemorySimpleDualPort
    generic map(ADDRESS_WIDTH=>3, DATA_WIDTH=>64)
    port map(clkA_i=>clk,
             enableA_i=>payloadWrite,
             addressA_i=>payloadAddress,
             dataA_i=>payload,
             clkB_i=>clk,
             enableB_i=>'1',
             addressB_i=>payloadIndex_i,
             dataB_o=>payload_o);

end architecture;
