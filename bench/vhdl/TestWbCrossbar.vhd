-------------------------------------------------------------------------------
-- This file is part of the OpenRIO project
-- https://github.com/magro732/OpenRIO
-- 
-- Description
-- This file contains the verification of WbCrossbar. 
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
-- WbCrossbarAddressMapTest
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-------------------------------------------------------------------------------
-- Architecture for WbCrossbarAddressMapTest.
-------------------------------------------------------------------------------
architecture WbCrossbarAddressMapTest of WbCrossbarAddressMap is
  
begin
  
  slaveSelect_o(0) <=
    '1' when (addr_i(3 downto 1) = "000") else '0';
  slaveSelect_o(1) <=
    '1' when (addr_i(3 downto 1) = "001") else '0';
  slaveSelect_o(2) <=
    '1' when (addr_i(3 downto 1) = "010") else '0';
  slaveSelect_o(3) <=
    '1' when (addr_i(3 downto 1) = "011") else '0';
  slaveSelect_o(4) <= addr_i(3);

end architecture;


-------------------------------------------------------------------------------
-- Configuration to use WbCrossbarAddressMapTest in a WbCrossbar.
-------------------------------------------------------------------------------
use work.all;
configuration TestWbCrossbarConfig of WbCrossbar is
  for WbCrossbarImpl
    for MasterInterface
      for all : WbCrossbarAddressMap
        use entity work.WbCrossbarAddressMap(WbCrossbarAddressMapTest);
      end for;
    end for;
  end for;
end configuration;


-------------------------------------------------------------------------------
-- TestWbCrossbar
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.TestPortPackage.all;


-------------------------------------------------------------------------------
-- Entity for TestWbCrossbar.
-------------------------------------------------------------------------------
entity TestWbCrossbar is
end entity;


-------------------------------------------------------------------------------
-- Architecture for TestWbCrossbar.
-------------------------------------------------------------------------------
architecture TestWbCrossbarImpl of TestWbCrossbar is

  component WbCrossbar is
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
      addr_o : out std_logic_vector(SLAVE_PORTS*SLAVE_ADDRESS_WIDTH-1 downto 0);
      dataWrite_o : out std_logic_vector(SLAVE_PORTS*DATA_WIDTH-1 downto 0);
      dataRead_i : in std_logic_vector(SLAVE_PORTS*DATA_WIDTH-1 downto 0);
      ack_i : in std_logic_vector(SLAVE_PORTS-1 downto 0));
  end component;

  -- Use the WbCrossbarAddressMapTest as the address mapping achitecture.
  for all : WbCrossbar use configuration work.TestWbCrossbarConfig;

  constant MASTER_PORTS : natural := 4;
  constant MASTER_ADDRESS_WIDTH : natural := 4;
  constant SLAVE_PORTS : natural := 5;
  constant SLAVE_ADDRESS_WIDTH : natural := 1;
  constant DATA_WIDTH : natural := 8;
  
  signal clk : std_logic;
  signal areset_n : std_logic;
  
  signal cycIn : std_logic_vector(MASTER_PORTS-1 downto 0);
  signal stbIn : std_logic_vector(MASTER_PORTS-1 downto 0);
  signal weIn : std_logic_vector(MASTER_PORTS-1 downto 0);
  signal addrIn : std_logic_vector(MASTER_PORTS*MASTER_ADDRESS_WIDTH-1 downto 0);
  signal dataWriteIn : std_logic_vector(MASTER_PORTS*DATA_WIDTH-1 downto 0);
  signal dataReadOut : std_logic_vector(MASTER_PORTS*DATA_WIDTH-1 downto 0);
  signal ackOut : std_logic_vector(MASTER_PORTS-1 downto 0);
  
  signal cycOut : std_logic_vector(SLAVE_PORTS-1 downto 0);
  signal stbOut : std_logic_vector(SLAVE_PORTS-1 downto 0);
  signal weOut : std_logic_vector(SLAVE_PORTS-1 downto 0);
  signal addrOut : std_logic_vector(SLAVE_PORTS*SLAVE_ADDRESS_WIDTH-1 downto 0);
  signal dataWriteOut : std_logic_vector(SLAVE_PORTS*DATA_WIDTH-1 downto 0);
  signal dataReadIn : std_logic_vector(SLAVE_PORTS*DATA_WIDTH-1 downto 0);
  signal ackIn : std_logic_vector(SLAVE_PORTS-1 downto 0);

begin

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  process
  begin
    clk <= '0';
    wait for 1 ns;
    clk <= '1';
    wait for 1 ns ;
  end process;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  TestDriver: process
    
    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure MasterWriteStart(constant masterIndex : natural;
                               constant masterAddress : std_logic_vector(MASTER_ADDRESS_WIDTH-1 downto 0);
                               constant masterData : std_logic_vector(DATA_WIDTH-1 downto 0)) is
    begin
      cycIn(masterIndex) <= '1';
      stbIn(masterIndex) <= '1';
      weIn(masterIndex) <= '1';
      addrIn(MASTER_ADDRESS_WIDTH*(masterIndex+1)-1 downto MASTER_ADDRESS_WIDTH*masterIndex) <= masterAddress;
      dataWriteIn(DATA_WIDTH*(masterIndex+1)-1 downto DATA_WIDTH*masterIndex) <= masterData;      
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure MasterWriteEnd(constant masterIndex : natural) is
    begin
      cycIn(masterIndex) <= '0';
      stbIn(masterIndex) <= 'U';
      weIn(masterIndex) <= 'U';
      addrIn(MASTER_ADDRESS_WIDTH*(masterIndex+1)-1 downto MASTER_ADDRESS_WIDTH*masterIndex) <= (others=>'U');
      dataWriteIn(DATA_WIDTH*(masterIndex+1)-1 downto DATA_WIDTH*masterIndex) <= (others=>'U');
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure SlaveWriteStart(constant slaveIndex : natural) is
    begin
      dataReadIn(DATA_WIDTH*(slaveIndex+1)-1 downto DATA_WIDTH*slaveIndex) <= (others=>'U');
      ackIn(slaveIndex) <= '1';
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure SlaveWriteEnd(constant slaveIndex : natural) is
    begin
      dataReadIn(DATA_WIDTH*(slaveIndex+1)-1 downto DATA_WIDTH*slaveIndex) <= (others=>'U');
      ackIn(slaveIndex) <= 'U';
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure WriteVerify(constant masterIndex : natural;
                          constant slaveIndex : natural;
                          constant slaveData : std_logic_vector(DATA_WIDTH-1 downto 0);
                          constant checkAll : boolean := true) is
    begin
      for i in 0 to MASTER_PORTS-1 loop
        if (i = masterIndex) then
          TestCompare(ackOut(i), '1', "WriteVerify:ackOut1");
        else
          if (checkAll) then
            TestCompare(ackOut(i), '0', "WriteVerify:ackOut0");
          end if;
        end if;
      end loop;
      
      TestCompare(dataWriteOut(DATA_WIDTH*(slaveIndex+1)-1 downto DATA_WIDTH*slaveIndex), slaveData, "WriteVerify:slaveData");
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure MasterWrite(constant masterIndex : natural;
                          constant masterAddress : std_logic_vector(MASTER_ADDRESS_WIDTH-1 downto 0);
                          constant masterData : std_logic_vector(DATA_WIDTH-1 downto 0);
                          constant slaveIndex : natural) is
    begin
      MasterWriteStart(masterIndex, masterAddress, masterData);
      wait until clk'event and clk = '1';

      SlaveWriteStart(slaveIndex);
      wait until clk'event and clk = '1';      

      WriteVerify(masterIndex, slaveIndex, masterData);
      
      SlaveWriteEnd(slaveIndex);
      MasterWriteEnd(masterIndex);
    end procedure;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure MasterRead(constant masterIndex : natural;
                         constant masterAddress : std_logic_vector(MASTER_ADDRESS_WIDTH-1 downto 0);
                         constant slaveIndex : natural;
                         constant slaveData : std_logic_vector(DATA_WIDTH-1 downto 0)) is
    begin
      cycIn(masterIndex) <= '1';
      stbIn(masterIndex) <= '1';
      weIn(masterIndex) <= '0';
      addrIn(MASTER_ADDRESS_WIDTH*(masterIndex+1)-1 downto MASTER_ADDRESS_WIDTH*masterIndex) <= masterAddress;
      dataWriteIn(DATA_WIDTH*(masterIndex+1)-1 downto DATA_WIDTH*masterIndex) <= (others=>'U');
      wait until clk'event and clk = '1';
      
      dataReadIn(DATA_WIDTH*(slaveIndex+1)-1 downto DATA_WIDTH*slaveIndex) <= slaveData;
      ackIn(slaveIndex) <= '1';
      wait until clk'event and clk = '1';

      for i in 0 to MASTER_PORTS-1 loop
        if (i = masterIndex) then
          TestCompare(ackOut(i), '1', "MasterRead:ackOut1");
        else
          TestCompare(ackOut(i), '0', "MasterRead:ackOut0");
        end if;
      end loop;
      
      TestCompare(dataReadOut(DATA_WIDTH*(masterIndex+1)-1 downto DATA_WIDTH*masterIndex), slaveData, "MasterRead:slaveData.");
      
      cycIn(masterIndex) <= '0';
      stbIn(masterIndex) <= 'U';
      weIn(masterIndex) <= 'U';
      addrIn(SLAVE_ADDRESS_WIDTH*(masterIndex+1)-1 downto SLAVE_ADDRESS_WIDTH*masterIndex) <= (others=>'U');
      dataWriteIn(DATA_WIDTH*(masterIndex+1)-1 downto DATA_WIDTH*masterIndex) <= (others=>'U');
      dataReadIn(DATA_WIDTH*(slaveIndex+1)-1 downto DATA_WIDTH*slaveIndex) <= (others=>'U');
      ackIn(slaveIndex) <= 'U';
    end procedure;
    
  begin
    areset_n <= '0';

    cycIn <= "0000";
    
    wait until clk'event and clk = '1';
    areset_n <= '1';
    wait until clk'event and clk = '1';

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_WbCrossbar");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_WbCrossbar-TC1");
    TestSpec("Description: Test single master accesses.");
    TestSpec("Requirement: N/A");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Test write accesses from all masters to all slaves.");
    TestSpec("Result: The write operation should target the correct slave.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_WbCrossbar-TC1-Step1");
    ---------------------------------------------------------------------------

    for i in 0 to 3 loop
      MasterWrite(i, "0000", x"11", 0);
      MasterWrite(i, "0001", x"12", 0);
      MasterWrite(i, "0010", x"21", 1);
      MasterWrite(i, "0011", x"22", 1);
      MasterWrite(i, "0100", x"31", 2);
      MasterWrite(i, "0101", x"32", 2);
      MasterWrite(i, "0110", x"41", 3);
      MasterWrite(i, "0111", x"42", 3);
      MasterWrite(i, "1000", x"51", 4);
      MasterWrite(i, "1001", x"52", 4);
      MasterWrite(i, "1010", x"53", 4);
      MasterWrite(i, "1011", x"54", 4);
      MasterWrite(i, "1100", x"55", 4);
      MasterWrite(i, "1101", x"56", 4);
      MasterWrite(i, "1110", x"57", 4);
      MasterWrite(i, "1111", x"58", 4);
    end loop;

    ---------------------------------------------------------------------------
    TestSpec("Step 2:");
    TestSpec("Action: Test read accesses from all masters to all slaves.");
    TestSpec("Result: The read operation should target the correct slave.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_WbCrossbar-TC1-Step2");
    ---------------------------------------------------------------------------

    for i in 0 to 3 loop
      MasterRead(i, "0000", 0, x"11");
      MasterRead(i, "0001", 0, x"12");
      MasterRead(i, "0010", 1, x"21");
      MasterRead(i, "0011", 1, x"22");
      MasterRead(i, "0100", 2, x"31");
      MasterRead(i, "0101", 2, x"32");
      MasterRead(i, "0110", 3, x"41");
      MasterRead(i, "0111", 3, x"42");
      MasterRead(i, "1000", 4, x"51");
      MasterRead(i, "1001", 4, x"52");
      MasterRead(i, "1010", 4, x"53");
      MasterRead(i, "1011", 4, x"54");
      MasterRead(i, "1100", 4, x"55");
      MasterRead(i, "1101", 4, x"56");
      MasterRead(i, "1110", 4, x"57");
      MasterRead(i, "1111", 4, x"58");
    end loop;
    
    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_WbCrossbar-TC2");
    TestSpec("Description: Test multi master access to the same slave.");
    TestSpec("Requirement: N/A");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Test simultanous write accesses from all masters to the ");
    TestSpec("        same slave.");
    TestSpec("Result: Each masters access should be scheduled after one another.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_WbCrossbar-TC2-Step1");
    ---------------------------------------------------------------------------
    
    MasterWriteStart(0, "0000", x"11");
    MasterWriteStart(1, "0000", x"21");
    MasterWriteStart(2, "0000", x"31");
    MasterWriteStart(3, "0000", x"41");
    wait until clk'event and clk = '1';

    SlaveWriteStart(0);
    wait until clk'event and clk = '1';
    WriteVerify(3, 0, x"41");
    SlaveWriteEnd(0);
    MasterWriteEnd(3);
    wait until clk'event and clk = '1';

    SlaveWriteStart(0);
    wait until clk'event and clk = '1';
    WriteVerify(2, 0, x"31");
    SlaveWriteEnd(0);
    MasterWriteEnd(2);
    wait until clk'event and clk = '1';

    SlaveWriteStart(0);
    wait until clk'event and clk = '1';
    WriteVerify(1, 0, x"21");
    SlaveWriteEnd(0);
    MasterWriteEnd(1);
    wait until clk'event and clk = '1';
    
    SlaveWriteStart(0);
    wait until clk'event and clk = '1';
    WriteVerify(0, 0, x"11");
    SlaveWriteEnd(0);
    MasterWriteEnd(0);
    wait until clk'event and clk = '1';

    ---------------------------------------------------------------------------
    TestSpec("-----------------------------------------------------------------");
    TestSpec("TG_WbCrossbar-TC3");
    TestSpec("Description: Test multi master access to different slaves.");
    TestSpec("Requirement: N/A");
    TestSpec("-----------------------------------------------------------------");
    TestSpec("Step 1:");
    TestSpec("Action: Initiate accesses from all masters to each different slaves.");
    TestSpec("Result: All the transferes should be activeated simultanously.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_WbCrossbar-TC3-Step1");
    ---------------------------------------------------------------------------

    MasterWriteStart(0, "0000", x"11");
    MasterWriteStart(1, "0010", x"21");
    MasterWriteStart(2, "0100", x"31");
    MasterWriteStart(3, "0110", x"41");
    wait until clk'event and clk = '1';
    
    SlaveWriteStart(0);
    SlaveWriteStart(1);
    SlaveWriteStart(2);
    SlaveWriteStart(3);
    wait until clk'event and clk = '1';
    
    WriteVerify(0, 0, x"11", false);
    WriteVerify(1, 1, x"21", false);
    WriteVerify(2, 2, x"31", false);
    WriteVerify(3, 3, x"41", false);

    SlaveWriteEnd(0);
    MasterWriteEnd(0);
    SlaveWriteEnd(1);
    MasterWriteEnd(1);
    SlaveWriteEnd(2);
    MasterWriteEnd(2);
    SlaveWriteEnd(3);
    MasterWriteEnd(3);

    ---------------------------------------------------------------------------
    TestSpec("Step 2:");
    TestSpec("Action: Test to start a new access while one is currently ongoing.");
    TestSpec("Result: The first access should complete first and the new access");
    TestSpec("        should following it directly.");
    ---------------------------------------------------------------------------
    TestCaseStart("TG_WbCrossbar-TC1-Step2");
    ---------------------------------------------------------------------------

    MasterWriteStart(0, "0000", x"11");
    wait until clk'event and clk = '1';

    SlaveWriteStart(0);
    wait until clk'event and clk = '1';

    WriteVerify(0, 0, x"11", false);
    
    MasterWriteStart(1, "0000", x"21");
    wait until clk'event and clk = '1';

    WriteVerify(0, 0, x"11", false);

    MasterWriteEnd(0);
    wait until clk'event and clk = '1';
    wait until clk'event and clk = '1';

    WriteVerify(1, 0, x"21", false);

    SlaveWriteEnd(0);
    MasterWriteEnd(1);

    ---------------------------------------------------------------------------
    -- Test complete.
    ---------------------------------------------------------------------------
    
    TestEnd;
    
  end process;

  -----------------------------------------------------------------------------
  -- TestObject instantiation.
  -----------------------------------------------------------------------------
  TestObject: WbCrossbar
    generic map(MASTER_PORTS=>MASTER_PORTS,
                MASTER_ADDRESS_WIDTH=>MASTER_ADDRESS_WIDTH,
                SLAVE_PORTS=>SLAVE_PORTS,
                SLAVE_ADDRESS_WIDTH=>SLAVE_ADDRESS_WIDTH,
                DATA_WIDTH=>DATA_WIDTH)
    port map(
      clk=>clk, areset_n=>areset_n,
      cyc_i=>cycIn, stb_i=>stbIn, we_i=>weIn,
      addr_i=>addrIn, dataWrite_i=>dataWriteIn,
      dataRead_o=>dataReadOut, ack_o=>ackOut,
      cyc_o=>cycOut, stb_o=>stbOut, we_o=>weOut,
      addr_o=>addrOut, dataWrite_o=>dataWriteOut,
      dataRead_i=>dataReadIn, ack_i=>ackIn);

end architecture;
