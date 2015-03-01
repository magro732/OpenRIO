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
    offset_i : in std_logic_vector(20 downto 0);
    wdptr_i : in std_logic;
    payloadLength_i : in std_logic_vector(3 downto 0);
    payloadIndex_o : out std_logic_vector(3 downto 0);
    payload_i : in std_logic_vector(31 downto 0);
    done_o : out std_logic;
    
    readResponseReady_o : out std_logic;
    writeResponseReady_o : out std_logic;
    wdptr_o : out std_logic;
    payloadLength_o : out std_logic_vector(3 downto 0);
    payloadIndex_i : in std_logic_vector(3 downto 0);
    payload_o : out std_logic_vector(31 downto 0);
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
                     CONFIG_READ, CONFIG_READ_RESPONSE,
                     CONFIG_WRITE, CONFIG_WRITE_RESPONSE);
  signal state : StateType;
  
  signal payloadWrite : std_logic;
  signal payloadIndex : std_logic_vector(3 downto 0);
  
  signal configAdr : std_logic_vector(21 downto 0);
  signal configDat : std_logic_vector(31 downto 0);

begin

  wdptr_o <= wdptr_i;
  
  configAdr_o <= configAdr;
  configDat_o <= configDat;

  payloadLength_o <= payloadLength_i;
  payloadIndex_o <= payloadIndex;
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  Maintenance: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      state <= IDLE;

      configStb_o <= '0';
      configWe_o <= '0';
      configAdr <= (others=>'0');
      configDat <= (others=>'0');

      readResponseReady_o <= '0';
      writeResponseReady_o <= '0';

      done_o <= '0';
      
      payloadWrite <= '0';
      payloadIndex <= (others=>'0');
    elsif (clk'event and clk = '1') then
      payloadWrite <= '0';

      if (payloadWrite = '1') then
        payloadIndex <= std_logic_vector(unsigned(payloadIndex) + 1);
      end if;
      
      case state is
        when IDLE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          payloadIndex <= (others=>'0');
          if (readRequestReady_i = '1') then
            configStb_o <= '1';
            configWe_o <= '0';
            configAdr <= offset_i & wdptr_i;
            state <= CONFIG_READ;
          elsif (writeRequestReady_i = '1') then
            configStb_o <= '1';
            configWe_o <= '1';
            configAdr <= offset_i & wdptr_i;
            configDat <= payload_i;
            state <= CONFIG_WRITE;
          end if;          

        when CONFIG_READ =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (configAck_i = '1') then
            payloadWrite <= '1';
            
            if (payloadIndex /= payloadLength_i) then
              configAdr <= std_logic_vector(unsigned(configAdr) + 1);
            else
              done_o <= '1';
              configStb_o <= '0';
              state <= CONFIG_READ_RESPONSE;
            end if;
          end if;

        when CONFIG_READ_RESPONSE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (done_i = '1') then
            done_o <= '0';
            readResponseReady_o <= '0';
            state <= IDLE;
          else
            readResponseReady_o <= '1';
          end if;

        when CONFIG_WRITE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (configAck_i = '1') then
            payloadWrite <= '1';

            if (payloadIndex /= payloadLength_i) then
              configAdr <= std_logic_vector(unsigned(configAdr) + 1);
              configDat <= payload_i;
              payloadIndex <= std_logic_vector(unsigned(payloadIndex) + 1);
            else
              done_o <= '1';
              configStb_o <= '0';
              state <= CONFIG_WRITE_RESPONSE;
            end if;
          end if;

        when CONFIG_WRITE_RESPONSE =>
          ---------------------------------------------------------------------
          -- 
          ---------------------------------------------------------------------
          if (done_i = '1') then
            done_o <= '0';
            writeResponseReady_o <= '0';
            state <= IDLE;
          else
            writeResponseReady_o <= '1';
          end if;

        when others =>

      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Payload content memory.
  -----------------------------------------------------------------------------

  PayloadMemory: MemorySimpleDualPort
    generic map(ADDRESS_WIDTH=>4, DATA_WIDTH=>32)
    port map(clkA_i=>clk,
             enableA_i=>payloadWrite,
             addressA_i=>payloadIndex,
             dataA_i=>configDat_i,
             clkB_i=>clk,
             enableB_i=>'1',
             addressB_i=>payloadIndex_i,
             dataB_o=>payload_o);

end architecture;
