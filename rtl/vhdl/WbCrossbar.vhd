-------------------------------------------------------------------------------
-- This file is part of the OpenRIO project
-- https://github.com/magro732/OpenRIO
-- 
-- Description
-- This file contains a Wishbone'ish crossbar. Its address map can be specified
-- by binding an achitecture to WbCrossbarAddressMap and instantiate it under
-- the AddressMapping instance. 
-- 
-- To Do:
-- -
-- 
-- Author(s): 
-- - Magnus Rosenius, magro732@hemmai.se 
-- 
-------------------------------------------------------------------------------
-- 
-- Copyright (C) 2015 Authors and the Free Software Foundation
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
-- WbCrossbar.
-- Note that an WbCrossbarAddressMap architecture needs to be implemented and
-- defined elsewhere in order to build this entity.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.baseio_common.all;

-------------------------------------------------------------------------------
-- Entity for WbCrossbar.
-------------------------------------------------------------------------------
entity WbCrossbar is
  generic(
    MASTER_PORTS : natural := 4;
    MASTER_ADDRESS_WIDTH : natural := 3;
    SLAVE_PORTS : natural := 4;
    SLAVE_ADDRESS_WIDTH : natural := 1;
    DATA_WIDTH : natural := 8);
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    
    cyc_i : in std_logic_vector(MASTER_PORTS-1 downto 0);
    stb_i : in std_logic_vector(MASTER_PORTS-1 downto 0);
    we_i : in std_logic_vector(MASTER_PORTS-1 downto 0);
    addr_i : in std_logic_vector((MASTER_PORTS*MASTER_ADDRESS_WIDTH)-1 downto 0);
    dataWrite_i : in std_logic_vector((MASTER_PORTS*DATA_WIDTH)-1 downto 0);
    dataRead_o : out std_logic_vector((MASTER_PORTS*DATA_WIDTH)-1 downto 0);
    ack_o : out std_logic_vector(MASTER_PORTS-1 downto 0);

    cyc_o : out std_logic_vector(SLAVE_PORTS-1 downto 0);
    stb_o : out std_logic_vector(SLAVE_PORTS-1 downto 0);
    we_o : out std_logic_vector(SLAVE_PORTS-1 downto 0);
    addr_o : out std_logic_vector((SLAVE_PORTS*SLAVE_ADDRESS_WIDTH)-1 downto 0);
    dataWrite_o : out std_logic_vector((SLAVE_PORTS*DATA_WIDTH)-1 downto 0);
    dataRead_i : in std_logic_vector((SLAVE_PORTS*DATA_WIDTH)-1 downto 0);
    ack_i : in std_logic_vector(SLAVE_PORTS-1 downto 0) );
end entity;

-------------------------------------------------------------------------------
-- Architecture for WbCrossbar.
-------------------------------------------------------------------------------
architecture WbCrossbarImpl of WbCrossbar is
  component WbCrossbarAddressMap is
    generic(
      PORTS : natural;
      ADDRESS_WIDTH : natural);
    port(
      addr_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      slaveSelect_o : out std_logic_vector(PORTS-1 downto 0));
  end component;

  component Arbiter is
    generic(
      PORTS : natural);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      
      cyc_i : in std_logic_vector(PORTS-1 downto 0);
      cyc_o : out std_logic_vector(PORTS-1 downto 0));
  end component;
  
  signal slaveSelect : std_logic_vector((MASTER_PORTS*SLAVE_PORTS)-1 downto 0);
  signal slaveCyc : std_logic_vector((MASTER_PORTS*SLAVE_PORTS)-1 downto 0);

  signal masterSelect : std_logic_vector((MASTER_PORTS*SLAVE_PORTS)-1 downto 0);
  signal masterAck : std_logic_vector((MASTER_PORTS*SLAVE_PORTS)-1 downto 0);
  
begin

  -----------------------------------------------------------------------------
  -- Master interface.
  -----------------------------------------------------------------------------
  MasterInterface: for i in 0 to MASTER_PORTS-1 generate
    AddressMapping: WbCrossbarAddressMap
      generic map(ADDRESS_WIDTH=>MASTER_ADDRESS_WIDTH, PORTS=>SLAVE_PORTS)
      port map(addr_i=>addr_i(MASTER_ADDRESS_WIDTH*(i+1)-1 downto MASTER_ADDRESS_WIDTH*i),
               slaveSelect_o=>slaveSelect(SLAVE_PORTS*(i+1)-1 downto SLAVE_PORTS*i));
    
    process(masterAck, dataRead_i, ack_i)
    begin
      dataRead_o(DATA_WIDTH*(i+1)-1 downto DATA_WIDTH*i) <= (others=>'0');
      ack_o(i) <= '0';
      for j in 0 to SLAVE_PORTS-1 loop
        if (masterAck(SLAVE_PORTS*i+j) = '1') then
          dataRead_o(DATA_WIDTH*(i+1)-1 downto DATA_WIDTH*i) <=
            dataRead_i(DATA_WIDTH*(j+1)-1 downto DATA_WIDTH*j);
          ack_o(i) <= ack_i(j);
        end if;
      end loop;
    end process;
  end generate;

  -----------------------------------------------------------------------------
  -- Interconnect masters to slaves.
  -----------------------------------------------------------------------------
  process(slaveSelect, cyc_i)
  begin
    for slave in 0 to SLAVE_PORTS-1 loop
      for master in 0 to MASTER_PORTS-1 loop
        slaveCyc(MASTER_PORTS*slave+master) <=
          cyc_i(master) and slaveSelect(slave+SLAVE_PORTS*master);
      end loop;
    end loop;
  end process;

  -- Since return data from the slave will not be present until earliest the
  -- next tick, pipeline this signal to get better timing.
  process(clk)
  begin
    if (clk'event and clk = '1') then
      for slave in 0 to SLAVE_PORTS-1 loop
        for master in 0 to MASTER_PORTS-1 loop
          masterAck(master*SLAVE_PORTS+slave) <= masterSelect(master+slave*MASTER_PORTS);
        end loop;
      end loop;
    end if;
  end process;
  
  -----------------------------------------------------------------------------
  -- Slave interface.
  -----------------------------------------------------------------------------
  SlaveInterface: for i in 0 to SLAVE_PORTS-1 generate
    SlaveArbiter: Arbiter
      generic map(PORTS=>MASTER_PORTS)
      port map(clk=>clk, areset_n=>areset_n,
               cyc_i=>slaveCyc(MASTER_PORTS*(i+1)-1 downto MASTER_PORTS*i),
               cyc_o=>masterSelect(MASTER_PORTS*(i+1)-1 downto MASTER_PORTS*i));

    cyc_o(i) <= or_reduce(masterSelect(MASTER_PORTS*(i+1)-1 downto MASTER_PORTS*i));

    process(masterSelect, stb_i, we_i, addr_i, dataWrite_i)
      variable masterAddress : std_logic_vector(MASTER_ADDRESS_WIDTH-1 downto 0);
    begin
      stb_o(i) <= '0';
      we_o(i) <= '0';
      addr_o(SLAVE_ADDRESS_WIDTH*(i+1)-1 downto SLAVE_ADDRESS_WIDTH*i) <= (others=>'0');
      dataWrite_o(DATA_WIDTH*(i+1)-1 downto DATA_WIDTH*i) <= (others=>'0');
      for j in 0 to MASTER_PORTS-1 loop
        if (masterSelect(i*MASTER_PORTS+j) = '1') then
          stb_o(i) <= stb_i(j);
          we_o(i) <= we_i(j);
          masterAddress := addr_i(MASTER_ADDRESS_WIDTH*(j+1)-1 downto MASTER_ADDRESS_WIDTH*j);
          addr_o(SLAVE_ADDRESS_WIDTH*(i+1)-1 downto SLAVE_ADDRESS_WIDTH*i) <=
            masterAddress(SLAVE_ADDRESS_WIDTH-1 downto 0);
          dataWrite_o(DATA_WIDTH*(i+1)-1 downto DATA_WIDTH*i) <=
            dataWrite_i((DATA_WIDTH*(j+1))-1 downto (DATA_WIDTH*j));
        end if;
      end loop;
    end process;
  end generate;

end architecture;

-------------------------------------------------------------------------------
-- Arbiter
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.baseio_common.all;

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity Arbiter is
  generic(
    PORTS : natural);
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    cyc_i : in std_logic_vector(PORTS-1 downto 0);
    cyc_o : out std_logic_vector(PORTS-1 downto 0));
end entity;

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture ArbiterImpl of Arbiter is
  
  component Rotater is
    generic(
      WIDTH : natural);
    port(
      input_i : in std_logic_vector(WIDTH-1 downto 0);
      output_o : out std_logic_vector(WIDTH-1 downto 0));
  end component;

  component TopEncoder is
    generic(
      WIDTH : natural);
    port(
      input_i : in std_logic_vector(WIDTH-1 downto 0);
      output_o : out std_logic_vector(WIDTH-1 downto 0));
  end component;

  type ArrayActive is array (natural range <>) of std_logic_vector(PORTS-1 downto 0);
  signal active : ArrayActive(PORTS-1 downto 0) := (others=>(others=>'1'));
  
  signal wantBus : std_logic_vector(PORTS-1 downto 0);
  signal gotBus : std_logic_vector(PORTS-1 downto 0);
  signal cycOut : std_logic_vector(PORTS-1 downto 0);
  
begin
  
  GenerateRotateIf: if (PORTS > 1) generate
    GenerateRotate: for i in 0 to PORTS-2 generate
      Rotate: Rotater
        generic map(WIDTH=>PORTS)
        port map(input_i=>active(i), output_o=>active(i+1));
    end generate;
  end generate;

  GenerateWantBus: for i in 0 to PORTS-1 generate
    wantBus(i) <= '1' when (or_reduce(active(i) and cyc_i) = '1') else '0';
  end generate;
    
  ChooseMaster: TopEncoder
    generic map(WIDTH=>PORTS)
    port map(input_i=>wantBus, output_o=>gotBus);

  process(gotBus, cyc_i, active)
  begin
    cycOut <= (others=>'0');
    for i in 0 to PORTS-1 loop
      if (gotBus(i) = '1') then
        cycOut <= active(i);
      end if;
    end loop;
  end process;
  cyc_o <= cycOut;
  
  process(clk)
  begin
    if (clk'event and clk = '1') then
      if (areset_n = '0') then
        active(0) <= (others=>'0');
        active(0)(0) <= '1';
      elsif (unsigned(cycOut) /= 0) then
        active(0) <= cycOut;
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

entity Rotater is
  generic(
    WIDTH : natural);
  port(
    input_i : in std_logic_vector(WIDTH-1 downto 0);
    output_o : out std_logic_vector(WIDTH-1 downto 0));
end entity;

architecture RotaterImpl of Rotater is
begin
  output_o <= input_i(0) & input_i(WIDTH-1 downto 1);
end architecture;



-------------------------------------------------------------------------------
-- TopEncoder implementation.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


-------------------------------------------------------------------------------
-- Entity for TopEncoder.
-------------------------------------------------------------------------------
entity TopEncoder is
  generic(
    WIDTH : natural);
  port(
    input_i : in std_logic_vector(WIDTH-1 downto 0);
    output_o : out std_logic_vector(WIDTH-1 downto 0));
end entity;
       

-------------------------------------------------------------------------------
-- Architecture for TopEncoder.
-------------------------------------------------------------------------------
architecture TopEncoderImpl of TopEncoder is
begin

  process(input_i)
    variable found : boolean;
  begin
    found := false;
    output_o <= (others=>'0');
    for i in 0 to WIDTH-1 loop
      if (not found) and (input_i(i) = '1') then
        output_o(i) <= '1';
        found := true;
      else
        output_o(i) <= '0';
      end if;
    end loop;
  end process;
  
end architecture;



-------------------------------------------------------------------------------
-- WbCrossbarAddressMap.
-- This module is used by the WbCrossbar to determine the address mapping to
-- slaves.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


-------------------------------------------------------------------------------
-- Entity for WbCrossbarAddressMap.
-------------------------------------------------------------------------------
entity WbCrossbarAddressMap is
  generic(
    PORTS : natural;
    ADDRESS_WIDTH : natural);
  port(
    addr_i : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
    slaveSelect_o : out std_logic_vector(PORTS-1 downto 0));
end entity;

-------------------------------------------------------------------------------
-- No architecture defined.
-- It is up to the instantiator to bind a suitable architecture to this entity.
-------------------------------------------------------------------------------
