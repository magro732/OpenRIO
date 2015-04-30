-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Containing a testbench that verifies the RioPcs-module.
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
-- REMARK: Test the silence timeout.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use work.rio_common.all;
use work.TestPortPackage.all;

-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity TestRioPcs is
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture TestRioPcsImpl of TestRioPcs is

  component RioPcs is
    generic(
      TICKS_PER_US : natural;
      CLOCK_COMPENSATION_LIMIT : natural := 4095);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      portInitialized_o : out std_logic;
      outboundControlValid_i : in std_logic;
      outboundControlSymbol_i : in std_logic_vector(23 downto 0);
      outboundDataValid_i : in std_logic;
      outboundDataSymbol_i : in std_logic_vector(31 downto 0);
      inboundControlValid_o : out std_logic;
      inboundControlSymbol_o : out std_logic_vector(23 downto 0);
      inboundDataValid_o : out std_logic;
      inboundDataSymbol_o : out std_logic_vector(31 downto 0);
      
      outboundOutputEnable_o : out std_logic;
      outboundRead_i : in std_logic;
      outboundCodegroup_o : out std_logic_vector(9 downto 0);
      inboundTrained_i : in std_logic;
      inboundWrite_i : in std_logic;
      inboundCodegroup_i : in std_logic_vector(9 downto 0));
  end component;

  type TestPortPcsMessage is record
    symbolControl : boolean;
    scDelimiter : boolean;
    symbolContent : std_logic_vector(31 downto 0);
  end record;
  type TestPortPcsMessageArray is
    array (natural range <>) of TestPortPcsMessage;

  constant CODE_GROUP_PD_N : std_logic_vector(9 downto 0) := "0011110011";
  constant CODE_GROUP_PD_P : std_logic_vector(9 downto 0) := "1100001100";
  constant CODE_GROUP_SC_N : std_logic_vector(9 downto 0) := "0011110100";
  constant CODE_GROUP_SC_P : std_logic_vector(9 downto 0) := "1100001011";
  constant CODE_GROUP_K_N : std_logic_vector(9 downto 0) := "0011111010";
  constant CODE_GROUP_K_P : std_logic_vector(9 downto 0) := "1100000101";
  constant CODE_GROUP_A_P : std_logic_vector(9 downto 0) := "1101101000";
  constant CODE_GROUP_A_N : std_logic_vector(9 downto 0) := "0010010111";
  constant CODE_GROUP_R_P : std_logic_vector(9 downto 0) := "0100010111";
  constant CODE_GROUP_R_N : std_logic_vector(9 downto 0) := "1011101000";
      
  signal clk : std_logic := '0';
  signal areset_n : std_logic := '0';

  signal isPD : boolean;
  signal isSC : boolean;
  signal isK : boolean;
  signal isA : boolean;
  signal isR : boolean;
  signal isD : boolean;
  
  signal outboundCodegroup : std_logic_vector(9 downto 0);
  signal portInitialized : std_logic;
  signal outboundControlValid : std_logic;
  signal outboundControlSymbol : std_logic_vector(23 downto 0);
  signal outboundDataValid : std_logic;
  signal outboundDataSymbol : std_logic_vector(31 downto 0);
  signal inboundControlValid : std_logic;
  signal inboundControlSymbol : std_logic_vector(23 downto 0);
  signal inboundDataValid : std_logic;
  signal inboundDataSymbol : std_logic_vector(31 downto 0);

  signal outboundMessageEmpty : std_logic;
  signal outboundMessageWrite : std_logic;
  signal outboundMessage : TestPortPcsMessage;
  signal outboundMessageAck : std_logic;
  
  
begin

  -----------------------------------------------------------------------------
  -- Clock generation.
  -----------------------------------------------------------------------------
  process
  begin
    clk <= '0';
    wait for 5 ns;
    clk <= '1';
    wait for 5 ns;
  end process;

  -----------------------------------------------------------------------------
  -- Outbound character recognition.
  -----------------------------------------------------------------------------
  isPD <= ((outboundCodegroup = CODE_GROUP_PD_N) or (outboundCodegroup = CODE_GROUP_PD_P));
  isSC <= ((outboundCodegroup = CODE_GROUP_SC_N) or (outboundCodegroup = CODE_GROUP_SC_P));
  isK <= ((outboundCodegroup = CODE_GROUP_K_N) or (outboundCodegroup = CODE_GROUP_K_P));
  isA <= ((outboundCodegroup = CODE_GROUP_A_N) or (outboundCodegroup = CODE_GROUP_A_P));
  isR <= ((outboundCodegroup = CODE_GROUP_R_N) or (outboundCodegroup = CODE_GROUP_R_P));
  isD <= not (isPd or isSC or isK or isA or isR);

  -----------------------------------------------------------------------------
  -- Test driver.
  -----------------------------------------------------------------------------
  TestDriver: process
  begin
    outboundMessageWrite <= '0';
    
    areset_n <= '0';
    wait for 3 ns;
    areset_n <= '1';
    wait until rising_edge(clk);

    ---------------------------------------------------------------------------
    -- Test sending idle sequence for a while.
    ---------------------------------------------------------------------------
    outboundControlValid <= '0';
    outboundDataValid <= '0';
    wait for 100 us;

    ---------------------------------------------------------------------------
    -- Test sending control symbols.
    ---------------------------------------------------------------------------
    report "sending control with SC";

    -- REMARK: This should not need to be here...
    wait for 1 ps;
    outboundMessageWrite <= '1';
    outboundMessage.symbolControl <= true;
    outboundMessage.scDelimiter <= true;
    outboundMessage.symbolContent <=
      x"00" & RioControlSymbolCreate(STYPE0_STATUS, "10101", "01010",
                                     STYPE1_NOP, "000");
    wait until outboundMessageAck = '1';
    outboundMessageWrite <= '0';
    wait until outboundMessageAck = '0';
    
    outboundControlValid <= '1';
    outboundControlSymbol <=
      RioControlSymbolCreate(STYPE0_STATUS, "10101", "01010",
                             STYPE1_NOP, "000");
    wait until clk = '1';
    outboundControlValid <= '0';
    wait until clk = '1';

    wait for 10 us;
    
    ---------------------------------------------------------------------------
    -- Test sending a control symbol as a data-delimiter.
    ---------------------------------------------------------------------------

    report "sending control with PD";

    -- REMARK: This should not need to be here...
    wait for 1 ps;
    outboundMessageWrite <= '1';
    outboundMessage.symbolControl <= true;
    outboundMessage.scDelimiter <= false;
    outboundMessage.symbolContent <=
      x"00" & RioControlSymbolCreate(STYPE0_STATUS, "10101", "01010",
                                     STYPE1_START_OF_PACKET, "000");
    wait until outboundMessageAck = '1';
    outboundMessageWrite <= '0';
    wait until outboundMessageAck = '0';
    outboundMessageWrite <= '1';
    outboundMessage.symbolControl <= false;
    outboundMessage.symbolContent <= x"11223344";
    wait until outboundMessageAck = '1';
    outboundMessageWrite <= '0';
    wait until outboundMessageAck = '0';
    outboundMessageWrite <= '1';
    outboundMessage.symbolControl <= true;
    outboundMessage.scDelimiter <= false;
    outboundMessage.symbolContent <=
      x"00" & RioControlSymbolCreate(STYPE0_STATUS, "10101", "01010",
                                     STYPE1_END_OF_PACKET, "000");
    wait until outboundMessageAck = '1';
    outboundMessageWrite <= '0';
    wait until outboundMessageAck = '0';
    
    outboundControlValid <= '1';
    outboundControlSymbol <=
      RioControlSymbolCreate(STYPE0_STATUS, "10101", "01010",
                             STYPE1_START_OF_PACKET, "000");
    wait until clk = '1';
    outboundControlValid <= '0';
    wait until clk = '1';
    wait until clk = '1';
    wait until clk = '1';
    
    outboundDataValid <= '1';
    outboundDataSymbol <= x"11223344";
    wait until clk = '1';
    outboundDataValid <= '0';
    wait until clk = '1';
    wait until clk = '1';
    wait until clk = '1';

    outboundControlValid <= '1';
    outboundControlSymbol <=
      RioControlSymbolCreate(STYPE0_STATUS, "10101", "01010",
                             STYPE1_END_OF_PACKET, "000");
    wait until clk = '1';
    outboundControlValid <= '0';
    wait until clk = '1';
    wait until clk = '1';
    wait until clk = '1';

    wait for 10 us;

    ---------------------------------------------------------------------------
    -- REMARK: Add a testcase that sends many data symbols which prevents the
    -- sending of the clock-compensation sequence. It should then be checked
    -- that the clock compensation sequence is transmitted as the first four
    -- characters. 
    ---------------------------------------------------------------------------

    TestEnd;
  end process;

  -----------------------------------------------------------------------------
  -- Process that checks the distribution of ones and zeros on the link.
  -----------------------------------------------------------------------------
  process
    variable disparity : integer;
  begin
    wait until areset_n = '1';
    disparity := 0;
    
    loop
      wait until clk = '1';
      for i in outboundCodegroup'range loop
        if (outboundCodegroup(i) = '1') then
          disparity := disparity + 1;
        elsif (outboundCodegroup(i) = '0') then
          disparity := disparity - 1;
        end if;
      end loop;
      TestCompare(disparity <= 2, "positive disparity check");
      TestCompare(disparity >= -2, "negative disparity check");
    end loop;
  end process;

  -----------------------------------------------------------------------------
  -- Check that the idle sequence fulfills the requirements and that
  -- control-symbols and data-symbols are transmitted correctly.
  -----------------------------------------------------------------------------
  Outbound: process
    type AHistogram_t is array(16 to 31) of natural;
    constant QUEUE_SIZE : natural := 128;
    type QueueArray is array (natural range <>) of TestPortPcsMessage;

    function QueueIndexInc(constant i : natural) return natural is
      variable returnValue : natural;
    begin
      if(i = QUEUE_SIZE) then
        returnValue := 0;
      else
        returnValue := i + 1;
      end if;
      return returnValue;
    end function;

    variable front, back : natural range 0 to QUEUE_SIZE;
    variable symbolQueue : QueueArray(0 to QUEUE_SIZE);
    
    variable idleStart : boolean;
    variable kCounter : natural;
    variable rCounter : natural;

    variable aSpacing : natural;
    variable aHistogram : AHistogram_t;

    variable ccsCounter : natural;
    variable ccsDetect : natural;

    variable symbolCounter : natural;
    
    variable writeBuffer : line;

  begin
    wait until areset_n = '1';
    wait until clk = '1';
    wait until clk = '1';
    wait until clk = '1';

    front := 0;
    back := 0;
    outboundMessageEmpty <= '1';
    outboundMessageAck <= '0';

    idleStart := true;
    kCounter := 0;
    rCounter := 0;
    aSpacing := 0;
    aHistogram := (others=>0);
    ccsCounter := 0;
    ccsDetect := 0;

    symbolCounter := 0;
    
    loop
      wait on clk, outboundMessageWrite
        until clk = '1' or outboundMessageWrite = '1';

      if (clk'event) then
        -- Detect the clock-compensation sequence.
        case (ccsDetect) is
          when 0 =>
            if (isK) then
              ccsDetect := 1;
            end if;
          when 1 =>
            if (isR) then
              ccsDetect := 2;
            else
              ccsDetect := 0;
            end if;
          when 2 =>
            if (isR) then
              ccsDetect := 3;
            else
              ccsDetect := 0;
            end if;
          when others =>
            if (isR) then
              ccsCounter := 0;
            end if;
            ccsDetect := 0;
        end case;
        ccsCounter := ccsCounter + 1;
        TestCompare(ccsCounter < 5000, "clock compensation sequence check");

        -- If this is the first character in an idle sequence, it should be
        -- the K-character.
        if (idleStart) then
          TestCompare(isK, "K-char as first character");
          idleStart := false;
        end if;

        if (isSC) then
          TestCompare(front /= back, "empty queue");
          TestCompare(symbolCounter = 0, "symbol position");
          TestCompare(symbolQueue(back).symbolControl, "expecting control");
          TestCompare(symbolQueue(back).scDelimiter, "expecting SC");
          symbolCounter := symbolCounter + 1;
        end if;
        
        if (isPD) then
          TestCompare(front /= back, "empty queue");
          TestCompare(symbolCounter = 0, "symbol position");
          TestCompare(symbolQueue(back).symbolControl, "expecting control");
          TestCompare(not symbolQueue(back).scDelimiter, "expecting PD");
          symbolCounter := symbolCounter + 1;
        end if;
        
        if (isD) then
          TestCompare(front /= back, "empty queue");
          case symbolCounter is
            when 0 =>
              TestCompare(not symbolQueue(back).symbolControl, "expecting D");
              --TestCompare(symbolQueue(back).symbolContent(31 downto 24) = XXX,
              --            "data symbol 31:24");
              symbolCounter := symbolCounter + 1;
            when 1 =>
              -- REMARK: Has to convert the 10B-encoded character to 8B.
              --TestCompare(symbolQueue(back).symbolContent(23 downto 16) = XXX,
              --            "control/data symbol 23:16");
              symbolCounter := symbolCounter + 1;
            when 2 =>
              --TestCompare(symbolQueue(back).symbolContent(15 downto 8) = XXX,
              --            "control/data symbol 15:8");
              symbolCounter := symbolCounter + 1;
            when 3 =>
              --TestCompare(symbolQueue(back).symbolContent(7 downto 0) = XXX,
              --            "control/data symbol 7:0");              
              symbolCounter := 0;
              back := QueueIndexInc(back);
              if (front = back) then
                outboundMessageEmpty <= '1';
                idleStart := true;
              else
                outboundMessageEmpty <= '0';
              end if;
            when others =>
              TestError("unexpected D-chars following a SC/PD");
          end case;
        end if;        
        
        if (isK) then
          aSpacing := aSpacing + 1;
          TestCompare(aSpacing <= 31, "A-char high limit");
          kCounter := kCounter + 1;
        elsif (isR) then
          aSpacing := aSpacing + 1;
          TestCompare(aSpacing <= 31, "A-char high limit");
          rCounter := rCounter + 1;
        elsif (isA) then
          TestCompare(aSpacing >= 16, "A-char low limit");
          aHistogram(aSpacing) := aHistogram(aSpacing) + 1;
          aSpacing := 0;

          -- REMARK: Add warning if the distribution is not good enough.
          write(writeBuffer, string'("A-char histogram"));
          writeline(OUTPUT, writeBuffer);
          for i in aHistogram'range loop
            write(writeBuffer, integer'image(i));
            write(writeBuffer, ":" & integer'image(aHistogram(i)));
            writeline(OUTPUT, writeBuffer);
          end loop;
          write(writeBuffer, string'("K-chars=") & integer'image(kCounter));
          write(writeBuffer, string'(" R-chars=") & integer'image(rCounter));
          writeline(OUTPUT, writeBuffer);
        end if;
      elsif (outboundMessageWrite'event) then
        symbolQueue(front) := outboundMessage;
        front := QueueIndexInc(front);

        outboundMessageEmpty <= '0';
        outboundMessageAck <= '1';
        wait until outboundMessageWrite = '0';
        outboundMessageAck <= '0';
      end if;
      
    end loop;    
  end process;

  -----------------------------------------------------------------------------
  -- Test object instantiation.
  -----------------------------------------------------------------------------
  TestObject: RioPcs
    generic map(
      TICKS_PER_US=>0,
      CLOCK_COMPENSATION_LIMIT=>4095)
    port map(
      clk=>clk, 
      areset_n=>areset_n,
      enable=>'1',
      --REMARK: Dont loop-back the outbound codegroup.
      outboundOutputEnable_o=>open,
      outboundRead_i=>'1',
      outboundCodegroup_o=>outboundCodegroup,
      inboundTrained_i=>'1',
      inboundWrite_i=>'1',
      inboundCodegroup_i=>outboundCodegroup,
      portInitialized_o=>portInitialized, 
      outboundControlValid_i=>outboundControlValid, 
      outboundControlSymbol_i=>outboundControlSymbol, 
      outboundDataValid_i=>outboundDataValid, 
      outboundDataSymbol_i=>outboundDataSymbol, 
      inboundControlValid_o=>inboundControlValid, 
      inboundControlSymbol_o=>inboundControlSymbol, 
      inboundDataValid_o=>inboundDataValid, 
      inboundDataSymbol_o=>inboundDataSymbol);
    
end architecture;
    



-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
use work.rio_common.all;
-- REMARK: Compile TestRioSerial.vhd first to resolve the TestRioSerialPackage.
use work.TestRioSerialPackage.all;
use work.TestPortPackage.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity TestSymbolPort is
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    outboundControlValid_i : in std_logic;
    outboundControlSymbol_i : in std_logic_vector(23 downto 0);
    outboundDataValid_i : in std_logic;
    outboundDataSymbol_i : in std_logic_vector(31 downto 0);
    inboundControlValid_o : out std_logic;
    inboundControlSymbol_o : out std_logic_vector(23 downto 0);
    inboundDataValid_o : out std_logic;
    inboundDataSymbol_o : out std_logic_vector(31 downto 0);

    outboundWriteEmpty_o : out std_logic;
    outboundWrite_i : in std_logic;
    outboundWriteMessage_i : in MessageSymbol;
    outboundWriteAck_o : out std_logic;

    inboundWriteEmpty_o : out std_logic;
    inboundWrite_i : in std_logic;
    inboundWriteMessage_i : in MessageSymbol;
    inboundWriteAck_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture TestSymbolPortImpl of TestSymbolPort is
  constant QUEUE_SIZE : natural := 2047;
  type QueueArray is array (natural range <>) of MessageSymbol;

  function QueueIndexInc(constant i : natural) return natural is
    variable returnValue : natural;
  begin
    if(i = QUEUE_SIZE) then
      returnValue := 0;
    else
      returnValue := i + 1;
    end if;
    return returnValue;
  end function;

begin
  
  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  Outbound: process
    variable front, back : natural range 0 to QUEUE_SIZE;
    variable symbolQueue : QueueArray(0 to QUEUE_SIZE);
  begin
    wait until areset_n = '1';

    front := 0;
    back := 0;
    outboundWriteEmpty_o <= '1';
    outboundWriteAck_o <= '0';

    loop
      wait until clk = '1' or outboundWrite_i = '1';

      if (clk'event) then
        if (back /= front) then
          if (symbolQueue(back).symbolType = "01") then
            if (not ((symbolQueue(back).ignoreIdle) and
                     (outboundControlValid_i = '0') and
                     (outboundDataValid_i = '0'))) then
              TestCompare(outboundControlValid_i, '1', "OUTBOUND:control symbol");
              TestCompare(outboundControlSymbol_i(23 downto 21), symbolQueue(back).symbolContent(23 downto 21),
                          "OUTBOUND:stype0=" & to_string(symbolQueue(back).symbolContent(23 downto 21)));
              TestCompare(outboundControlSymbol_i(20 downto 16), symbolQueue(back).symbolContent(20 downto 16),
                          "OUTBOUND:parameter0=" & to_string(symbolQueue(back).symbolContent(20 downto 16)));
              TestCompare(outboundControlSymbol_i(15 downto 11), symbolQueue(back).symbolContent(15 downto 11),
                          "OUTBOUND:parameter1=" & to_string(symbolQueue(back).symbolContent(15 downto 11)));
              TestCompare(outboundControlSymbol_i(10 downto 8), symbolQueue(back).symbolContent(10 downto 8),
                          "OUTBOUND:stype1=" & to_string(symbolQueue(back).symbolContent(10 downto 8)));
              TestCompare(outboundControlSymbol_i(7 downto 5), symbolQueue(back).symbolContent(7 downto 5),
                          "OUTBOUND:cmd=" & to_string(symbolQueue(back).symbolContent(7 downto 5)));
              TestCompare(outboundControlSymbol_i(4 downto 0), symbolQueue(back).symbolContent(4 downto 0),
                          "OUTBOUND:crc5=" & to_string(symbolQueue(back).symbolContent(4 downto 0)));
              TestCompare(outboundDataValid_i, '0', "OUTBOUND:no data symbol");
              back := QueueIndexInc(back);
            end if;
          elsif (symbolQueue(back).symbolType = "10") then
            if (not ((symbolQueue(back).ignoreIdle) and
                     (outboundControlValid_i = '0') and
                     (outboundDataValid_i = '0'))) then
              TestCompare(outboundDataValid_i, '1', "OUTBOUND:valid data symbol");
              TestCompare(outboundDataSymbol_i, symbolQueue(back).symbolContent(31 downto 0),
                "OUTBOUND:data symbol content");
              TestCompare(outboundControlValid_i, '0', "OUTBOUND:valid control symbol");
              back := QueueIndexInc(back);
            end if;
          elsif (symbolQueue(back).symbolType = "00") then
            TestCompare(outboundControlValid_i, '0', "OUTBOUND:valid control symbol");
            TestCompare(outboundDataValid_i, '0', "OUTBOUND:valid data symbol");
            back := QueueIndexInc(back);
          else
            TestCompare(outboundControlValid_i, '1', "OUTBOUND:valid control symbol");
            TestCompare(outboundDataValid_i, '1', "OUTBOUND:valid data symbol");
            back := QueueIndexInc(back);
          end if;

          if (front = back) then
            outboundWriteEmpty_o <= '1';
          else
            outboundWriteEmpty_o <= '0';
          end if;
        else
          TestError("OUTBOUND:empty symbol queue");
        end if;          
      elsif (outboundWrite_i'event) then
        symbolQueue(front) := outboundWriteMessage_i;
        front := QueueIndexInc(front);

        outboundWriteEmpty_o <= '0';
        outboundWriteAck_o <= '1';
        wait until outboundWrite_i = '0';
        outboundWriteAck_o <= '0';
      end if;
    end loop;    
  end process;

  -----------------------------------------------------------------------------
  -- 
  -----------------------------------------------------------------------------
  Inbound: process
    variable front, back : natural range 0 to QUEUE_SIZE;
    variable symbolQueue : QueueArray(0 to QUEUE_SIZE);
  begin
    wait until areset_n = '1';

    front := 0;
    back := 0;
    inboundWriteEmpty_o <= '1';
    inboundWriteAck_o <= '0';

    inboundControlValid_o <= '0';
    inboundControlSymbol_o <= (others=>'U');
    inboundDataValid_o <= '0';
    inboundDataSymbol_o <= (others=>'U');
    
    loop
      wait until clk = '1' or inboundWrite_i = '1';

      if (clk'event) then
        if (back /= front) then
          if (symbolQueue(back).symbolType = "00") then
            inboundControlValid_o <= '0';
            inboundDataValid_o <= '0';
          elsif (symbolQueue(back).symbolType = "01") then
            inboundControlValid_o <= '1';
            inboundControlSymbol_o <= symbolQueue(back).symbolContent(23 downto 0);
            inboundDataValid_o <= '0';
          elsif (symbolQueue(back).symbolType = "10") then
            inboundControlValid_o <= '0';
            inboundDataValid_o <= '1';
            inboundDataSymbol_o <= symbolQueue(back).symbolContent(31 downto 0);
          else
            inboundControlValid_o <= '1';
            inboundDataValid_o <= '1';
          end if;

          back := QueueIndexInc(back);

          if (front = back) then
            inboundWriteEmpty_o <= '1';
          else
            inboundWriteEmpty_o <= '0';
          end if;
        else
          TestError("INBOUND:empty symbol queue");
        end if;
      elsif (inboundWrite_i'event) then
        symbolQueue(front) := inboundWriteMessage_i;
        front := QueueIndexInc(front);

        inboundWriteEmpty_o <= '0';
        inboundWriteAck_o <= '1';
        wait until inboundWrite_i = '0';
        inboundWriteAck_o <= '0';
      end if;
    end loop;    
  end process;

end architecture;
