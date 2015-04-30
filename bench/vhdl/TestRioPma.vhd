-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Containing a testbench that verifies the RioPma module.
-- 
-- To Do:
-- -
-- 
-- Author(s): 
-- - Magnus Rosenius, magro732@opencores.org 
-- 
-------------------------------------------------------------------------------
-- 
-- Copyright (C) 2015 Authors and OPENCORES.ORG 
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
-- 
-------------------------------------------------------------------------------
-- REMARK: Use one testbench instance, feeded with two different clocks.
library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;
use ieee.numeric_std.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity TestRioPma is
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture TestRioPmaImpl of TestRioPma is

  component RioPma is
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      realign_i : in std_logic;
      aligned_o : out std_logic;
      
      outboundCodegroup_i : in std_logic_vector(0 to 9);
      outboundRead_o : out std_logic;
      inboundWrite_o : out std_logic;
      inboundCodegroup_o : out std_logic_vector(0 to 9);

      serial_i : in std_logic;
      serial_o : out std_logic);
  end component;
  
  signal clkFast, clkSlow : std_logic;
  signal areset_n : std_logic;
  
  signal realign : std_logic;
  signal alignedFast, alignedSlow : std_logic;

  signal outboundDataFast, outboundDataSlow : std_logic_vector(0 to 9);
  signal outboundReadFast, outboundReadSlow : std_logic;
  signal outputFast, outputSlow : std_logic;

  signal inboundWriteFast, inboundWriteSlow : std_logic;
  signal inboundDataFast, inboundDataSlow : std_logic_vector(0 to 9);
  signal inputFast, inputSlow : std_logic;
  
  -- /K/=/K28.5/
  constant K_10N : std_logic_vector(0 to 9) := "0011111010";
  constant K_10P : std_logic_vector(0 to 9) := "1100000101";
  -- /R/=/K29.7/
  constant R_10N : std_logic_vector(0 to 9) := "1011101000";
  constant R_10P : std_logic_vector(0 to 9) := "0100010111";
  
begin

  -----------------------------------------------------------------------------
  -- Testing a faster clock.
  -----------------------------------------------------------------------------
  process
  begin
    clkFast <= '0';
    wait for 4 ns;
    wait for 90 ps;
    clkFast <= '1';
    wait for 4 ns;
    wait for 90 ps;
  end process;
 
  process
    variable seed1 : positive := 1;
    variable seed2: positive := 1;
    
    procedure send10b(constant tx : std_logic_vector(0 to 9);
                      constant ber : real := 0.01) is
      variable rand: real;
      variable bitValue : std_logic;
    begin
      for i in tx'left to tx'right loop
        for j in 0 to 3 loop
          uniform(seed1, seed2, rand);
          if (rand < ber) then
            report "Bit error.";
            inputFast <= not tx(i);
          else
            inputFast <= tx(i);
          end if;
          wait for 10 ns;
        end loop;
      end loop;
    end procedure;

  begin
    inputFast <= '0';
    realign <= '0';
    
    areset_n <= '0';
    wait until rising_edge(clkFast);
    areset_n <= '1';
    wait until rising_edge(clkFast);

    loop
      send10b(K_10N);
      send10b(R_10P);
      send10b(R_10N);
      send10b(R_10P);
    end loop;
  end process;

  process
  begin
    outboundDataFast <= K_10N;
    wait until inboundWriteFast = '1';
    assert inboundDataFast = R_10P report "Fast:Expected /R/[1]." severity error;
    wait until inboundWriteFast = '1';
    assert inboundDataFast = R_10N report "Fast:Expected /R/[2]." severity error;
    wait until inboundWriteFast = '1';
    assert inboundDataFast = R_10P report "Fast:Expected /R/[3]." severity error;
    wait until inboundWriteFast = '1';
    assert inboundDataFast = K_10N report "Fast:Expected /K/[0]." severity error;
  end process;

  RioPmaFastInst: RioPma
    port map(
      clk=>clkFast,
      areset_n=>areset_n,
      realign_i=>realign, aligned_o=>alignedFast,
      outboundCodegroup_i=>outboundDataFast, outboundRead_o=>outboundReadFast,
      inboundWrite_o=>inboundWriteFast, inboundCodegroup_o=>inboundDataFast,
      serial_i=>inputFast, serial_o=>outputFast);
  
  -----------------------------------------------------------------------------
  -- Testing a slower clock.
  -----------------------------------------------------------------------------
  process
  begin
    clkSlow <= '0';
    wait for 5 ns;
    wait for 10 ps;
    clkSlow <= '1';
    wait for 5 ns;
    wait for 10 ps;
  end process;
  
  process
    procedure send10b(constant tx : std_logic_vector(0 to 9)) is
      variable bitValue : std_logic;
    begin
      for i in tx'left to tx'right loop
        inputSlow <= tx(i);
        wait for 40 ns;
      end loop;
    end procedure;

  begin
    inputSlow <= '0';
    realign <= '0';
    
    areset_n <= '0';
    wait until rising_edge(clkSlow);
    areset_n <= '1';
    wait until rising_edge(clkSlow);

    loop
      send10b(K_10N);
      send10b(R_10P);
      send10b(R_10N);
      send10b(R_10P);
    end loop;
  end process;

  process
  begin
    outboundDataSlow <= K_10N;
    wait until inboundWriteSlow = '1';
    assert inboundDataSlow = R_10P report "Slow:Expected /R/[1]." severity error;
    wait until inboundWriteSlow = '1';
    assert inboundDataSlow = R_10N report "Slow:Expected /R/[2]." severity error;
    wait until inboundWriteSlow = '1';
    assert inboundDataSlow = R_10P report "Slow:Expected /R/[3]." severity error;
    wait until inboundWriteSlow = '1';
    assert inboundDataSlow = K_10N report "Slow:Expected /K/[0]." severity error;
  end process;
  
  RioPmaSlowInst: RioPma
    port map(
      clk=>clkSlow,
      areset_n=>areset_n,
      realign_i=>realign, aligned_o=>alignedSlow,
      outboundCodegroup_i=>outboundDataSlow, outboundRead_o=>outboundReadSlow,
      inboundWrite_o=>inboundWriteSlow, inboundCodegroup_o=>inboundDataSlow,
      serial_i=>inputSlow, serial_o=>outputSlow);
  
end architecture;
    



