-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Containing a generic implementation of the Physical Medium Access (PMA)
-- layer in RapidIO.
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
-- RioPma
-- This is one possible implementation of the Physical Media Attachment layer
-- for RapidIO defined in RapidIO 3.1 part6, chapter 4.3.
-------------------------------------------------------------------------------
-- REMARK: The clock is 4x times the bit-rate for the edge alignment to work.
-- REMARK: Fix the alignment statemachine in RX.
-- REMARK: Add something that changes the alignment state when the CDR looses
-- its lock.
-- REMARK: The CDR should loose its lock when no flanks has been detected for
-- a too long time.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


-------------------------------------------------------------------------------
-- clk - System clock.
-- REMARK: Add signal descriptions.
-- outboundCodegroup_i - Bit9 is MSB.
-- inboundCodegroup_i - Bit9 is MSB.
-------------------------------------------------------------------------------
entity RioPma is
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    realign_i : in std_logic;
    aligned_o : out std_logic;
    
    outboundRead_o : out std_logic;
    outboundCodegroup_i : in std_logic_vector(9 downto 0);
    inboundWrite_o : out std_logic;
    inboundCodegroup_o : out std_logic_vector(9 downto 0);

    serial_i : in std_logic;
    serial_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioPmaImpl of RioPma is
  component RioPmaTx is
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      in_i : in std_logic_vector(0 to 9);
      read_o : out std_logic;

      out_o : out std_logic);
  end component;

  component RioPmaRx is
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      realign_i : in std_logic;
      aligned_o : out std_logic;
      
      valid_i : in std_logic;
      in_i : in std_logic;

      valid_o : out std_logic;
      out_o : out std_logic_vector(0 to 9));
  end component;
  
  component cdr is
    port(
      clk : in std_logic;
      areset_n : in std_logic;

      resync_i : in std_logic;
      lock_o : out std_logic;
      
      in_i : in std_logic;

      valid_o : out std_logic;
      out_o : out std_logic);
  end component;

  signal divider : unsigned(1 downto 0);
  signal txEnable : std_logic;

  signal cdrLock : std_logic;
  signal cdrValid : std_logic;
  signal cdrData : std_logic;

  signal pmaRealign : std_logic;
begin

  -----------------------------------------------------------------------------
  -- Transmitter with prescaling to fit the receiver oversampling.
  -----------------------------------------------------------------------------

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      divider <= (others=>'0');
    elsif (clk'event and clk = '1') then
      divider <= divider + 1;
    end if;
  end process;

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      txEnable <= '0';
    elsif (clk'event and clk = '1') then
      if (divider = "11") then
        txEnable <= '1';
      else
        txEnable <= '0';
      end if;
    end if;
  end process;
  
  RioPmaTxInst: RioPmaTx
    port map(
      clk=>clk,
      areset_n=>areset_n,
      enable=>txEnable,
      in_i=>outboundCodegroup_i,
      read_o=>outboundRead_o,
      out_o=>serial_o);

  -----------------------------------------------------------------------------
  -- Receiver with clock-data-recovery.
  -----------------------------------------------------------------------------
  
  CdrInst: cdr
    port map(
      clk=>clk,
      areset_n=>areset_n,
      in_i=>serial_i,
      resync_i=>realign_i,
      lock_o=>cdrLock,
      valid_o=>cdrValid,
      out_o=>cdrData);

  pmaRealign <= not cdrLock;
  RioPmaRxInst: RioPmaRx
    port map(
      clk=>clk,
      areset_n=>areset_n,
      realign_i=>pmaRealign,
      aligned_o=>aligned_o,
      valid_i=>cdrValid,
      in_i=>cdrData,
      valid_o=>inboundWrite_o,
      out_o=>inboundCodegroup_o);
  
end architecture;



-------------------------------------------------------------------------------
-- RioPmaTx
-- Serializes a 10-bit code-group into a serial stream.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity RioPmaTx is
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    in_i : in std_logic_vector(0 to 9);
    read_o : out std_logic;

    out_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioPmaTxImpl of RioPmaTx is
  signal shiftRegister : std_logic_vector(0 to 9);
  signal counter : natural range 0 to 9;
  signal wrap, wrap0 : std_logic;
begin

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      out_o <= '0';
    elsif (clk'event and clk = '1') then
      out_o <= shiftRegister(0);
    end if;
  end process;
  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      shiftRegister <= (others=>'0');
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        if (wrap = '1') then
          shiftRegister <= in_i;
        else
          shiftRegister <= shiftRegister(1 to 9) & '0';
        end if;
      end if;
    end if;
  end process;

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      counter <= 0;
      wrap <= '0';
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        if (counter = 9) then
          counter <= 0;
          wrap <= '1';
        else
          counter <= counter + 1;
          wrap <= '0';
        end if;
      end if;
    end if;
  end process;
  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      wrap0 <= '0';
      read_o <= '0';
    elsif (clk'event and clk = '1') then
      wrap0 <= wrap;
      if (wrap0 = '0') and (wrap = '1') then
        read_o <= '1';
      else
        read_o <= '0';
      end if;
    end if;
  end process;

end architecture;


-------------------------------------------------------------------------------
-- RioPmaRx
-- Parallellizes a serial stream into 10-bit code-groups.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity RioPmaRx is
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    realign_i : in std_logic;
    aligned_o : out std_logic;
    
    valid_i : in std_logic;
    in_i : in std_logic;

    valid_o : out std_logic;
    out_o : out std_logic_vector(0 to 9));
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioPmaRxImpl of RioPmaRx is
  constant COMMA_N : std_logic_vector(0 to 6) := "1100000";
  constant COMMA_P : std_logic_vector(0 to 6) := "0011111";

  signal shiftRegister : std_logic_vector(0 to 9);
  signal valid0, valid1 : std_logic;

  signal commaDetected : std_logic;
  
  signal aligned : std_logic;
  signal counter : natural range 0 to 9;

begin

  -- Shifter that parallellizes the serial stream into a stream of code-groups.
  CodeGroupShifter: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      valid0 <= '0';
      shiftRegister <= (others=>'0');
    elsif (clk'event and clk = '1') then
      valid0 <= valid_i;
      if (valid_i = '1') then
        shiftRegister <= shiftRegister(1 to 9) & in_i;
      end if;
    end if;
  end process;

  -- Find the /COMMA/ code group.
  CommaDetector: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      valid1 <= '0';
      commaDetected <= '0';
    elsif (clk'event and clk = '1') then
      valid1 <= valid0;
      if ((shiftRegister(0 to 6) = COMMA_P) or
          (shiftRegister(0 to 6) = COMMA_N)) then
        commaDetected <= '1';
      else
        commaDetected <= '0';
      end if;
    end if;
  end process;

  -- Statemachine to determine when the shifter content contains an aligned
  -- code-group.
  aligned_o <= aligned;
  AlignmentStatemachine: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      aligned <= '0';
      valid_o <= '0';
      out_o <= (others=>'0');
      counter <= 0;
    elsif (clk'event and clk = '1') then
      if (valid1 = '1') then
        out_o <= shiftRegister;
        if (realign_i = '1') then
          -- REMARK: Should a synchronous reset be inside the clock-enable?
          aligned <= '0';
          counter <= 0;
          valid_o <= '0';
        else
          if (aligned = '0') then
            aligned <= commaDetected;
            valid_o <= '0';
          else
            if (counter = 9) then
              counter <= 0;
              valid_o <= '1';
            else
              counter <= counter + 1;
              valid_o <= '0';
            end if;
          end if;
        end if;
      end if;
    end if;
  end process;

end architecture;


-------------------------------------------------------------------------------
-- cdr
-- Fully digital clock and data recovery unit. It needs 4x oversampling to be
-- able to determine, and to correct, the phase error.
-- Since the clock is higher than the actural bitrate it is also possible to do
-- some bit-error corrections.
-------------------------------------------------------------------------------
-- REMARK: The flank is the most sentitive area to read bits, dont use them in
-- the flank detection???
-- REMARK: Write a CDR that needs 3x instead... would give more jitter but
-- higher bitrates.
-- REMARK: Is it possible to do a 1x sampling when you know that R-characters
-- can be removed/inserted from the stream?
-- REMARK: Possible to divide into two processes with 2x-oversampling, one for
-- databit-creation and one for flank-synchronization?
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


-------------------------------------------------------------------------------
-- Entity for cdr.
-------------------------------------------------------------------------------
entity cdr is
  port(
    clk : in std_logic;
    areset_n : in std_logic;

    resync_i : in std_logic;
    lock_o : out std_logic;
    
    in_i : in std_logic;

    valid_o : out std_logic;
    out_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- Architecture for cdr.
-------------------------------------------------------------------------------
architecture cdr4xMemory of cdr is
  signal input : std_logic;

  signal shiftRegister : std_logic_vector(3 downto 0);

  constant FLANK_NONE : std_logic_vector(1 downto 0) := "00";
  constant FLANK_CENTERED : std_logic_vector(1 downto 0) := "00";
  constant FLANK_AHEAD : std_logic_vector(1 downto 0) := "10";
  constant FLANK_BEHIND : std_logic_vector(1 downto 0) := "01";
  signal flank : std_logic_vector(1 downto 0);
  signal flankError : std_logic;
  signal data : std_logic;
  signal dataError : std_logic;
  
  type SampleType is (WAIT_DATA, SAMPLE_DATA, ADVANCE_FLANK, NORMAL_FLANK, DELAY_FLANK);
  signal sampleState : SampleType;
  signal sampleUpdate : std_logic;
  signal sampleAdvance : std_logic;
  signal sampleDelay : std_logic;
  signal sampleError : std_logic;

  signal advanceHistory, delayHistory, errorHistory : std_logic;
  signal adjustAdvance, adjustDelay, adjustError : std_logic;
  
  signal lock : std_logic;
  
begin

  -----------------------------------------------------------------------------
  -- Input synchronization flip-flop.
  -- This could be placed in the input pad.
  -----------------------------------------------------------------------------
  -- REMARK: More FFs to avoid metastabillity?
  
  process(clk, areset_n)
  begin
    if (clk'event and clk = '1') then
      input <= in_i;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Input data shift register that keep track of all the sampled bits.
  -----------------------------------------------------------------------------

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      shiftRegister <= (others=>'0');
    elsif (clk'event and clk = '1') then
      shiftRegister <= shiftRegister(2 downto 0) & input;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Lookup table to determine the status of the current bit pattern in the
  -- shift register.
  -----------------------------------------------------------------------------

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      flank <= FLANK_NONE;
      flankError <= '0';
      data <= '0';
      dataError <= '0';
    elsif (clk'event and clk = '1') then
      case shiftRegister is
        when "0000" =>
          flank <= FLANK_NONE;
          flankError <= '0';
          data <= '0';
          dataError <= '0';
        when "0001" =>
          flank <= FLANK_BEHIND;
          flankError <= '0';
          data <= '0';
          dataError <= '0';
        when "0010" =>
          flank <= FLANK_CENTERED;
          flankError <= '1';
          data <= '0';
          dataError <= '1';
        when "0011" =>
          flank <= FLANK_CENTERED;
          flankError <= '0';
          data <= '1';
          dataError <= '1';
        when "0100" =>
          flank <= FLANK_CENTERED;
          flankError <= '1';
          data <= '0';
          dataError <= '1';
        when "0101" =>
          flank <= FLANK_CENTERED;
          flankError <= '1';
          data <= '0';
          dataError <= '1';
        when "0110" =>
          flank <= FLANK_NONE;
          flankError <= '1';
          data <= '1';
          dataError <= '0';
        when "0111" =>
          flank <= FLANK_AHEAD;
          flankError <= '0';
          data <= '1';
          dataError <= '0';
        when "1000" =>
          flank <= FLANK_AHEAD;
          flankError <= '0';
          data <= '0';
          dataError <= '0';
        when "1001" =>
          flank <= FLANK_NONE;
          flankError <= '1';
          data <= '0';
          dataError <= '0';
        when "1010" =>
          flank <= FLANK_CENTERED;
          flankError <= '1';
          data <= '1';
          dataError <= '1';
        when "1011" =>
          flank <= FLANK_CENTERED;
          flankError <= '1';
          data <= '1';
          dataError <= '1';
        when "1100" =>
          flank <= FLANK_CENTERED;
          flankError <= '0';
          data <= '0';
          dataError <= '1';
        when "1101" =>
          flank <= FLANK_CENTERED;
          flankError <= '1';
          data <= '1';
          dataError <= '1';
        when "1110" =>
          flank <= FLANK_BEHIND;
          flankError <= '0';
          data <= '1';
          dataError <= '0';
        when "1111" =>
          flank <= FLANK_NONE;
          flankError <= '0';
          data <= '1';
          dataError <= '0';
        when others =>
          null;
      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Synchronization state machine that keeps track of the synchronization to
  -- the inbound bit stream and outputs the detected data bits.
  -----------------------------------------------------------------------------

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      sampleState <= WAIT_DATA;
      
      sampleUpdate <= '0';
      sampleAdvance <= '0';
      sampleDelay <= '0';
      sampleError <= '0';
      
      valid_o <= '0';
      out_o <= '0';
    elsif (clk'event and clk = '1') then
      sampleUpdate <= '0';
      sampleAdvance <= flank(1);
      sampleDelay <= flank(0);
      sampleError <= flankError;
      
      valid_o <= '0';
      
      -- Check if locked to the inbound stream.
      if (lock = '0') then
        -----------------------------------------------------------------------
        -- Not locked to the inbound stream.
        -----------------------------------------------------------------------
        sampleUpdate <= '1';
        sampleState <= WAIT_DATA;
      else
        -----------------------------------------------------------------------
        -- Locked to the inbound stream.
        -----------------------------------------------------------------------

        -- Sample data and align to the inbound stream.
        case (sampleState) is
          when WAIT_DATA =>
            -- Wait for data to be centered in the window.
            sampleState <= SAMPLE_DATA;
          when SAMPLE_DATA =>
            -- Output the data bit.
            valid_o <= '1';
            out_o <= data;
            sampleState <= ADVANCE_FLANK;
          when ADVANCE_FLANK =>
            if (adjustAdvance = '1') then
              sampleState <= WAIT_DATA;
              sampleUpdate <= '1';
            else
              sampleState <= NORMAL_FLANK;
            end if;
          when NORMAL_FLANK =>
            if (adjustDelay = '0') then
              sampleState <= WAIT_DATA;
              sampleUpdate <= '1';
            else
              sampleState <= DELAY_FLANK;
            end if;
          when others =>
            sampleState <= WAIT_DATA;
            sampleUpdate <= '1';
        end case;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Adjustment tracking logic that LP-filters decisions.
  -----------------------------------------------------------------------------

  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      advanceHistory <= '0';
      adjustAdvance <= '0';
    elsif (clk'event and clk = '1') then
      if (sampleUpdate = '1') then
        advanceHistory <= sampleAdvance;
        if (advanceHistory = '1') and (sampleAdvance = '1') then
          adjustAdvance <= '1';
        else
          adjustAdvance <= '0';
        end if;
      end if;
    end if;
  end process;
  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      delayHistory <= '0';
      adjustDelay <= '0';
    elsif (clk'event and clk = '1') then
      if (sampleUpdate = '1') then
        delayHistory <= sampleDelay;
        if (delayHistory = '1') and (sampleDelay = '1') then
          adjustDelay <= '1';
        else
          adjustDelay <= '0';
        end if;
      end if;
    end if;
  end process;
  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      errorHistory <= '0';
      adjustError <= '0';
    elsif (clk'event and clk = '1') then
      if (sampleUpdate = '1') then
        errorHistory <= sampleError;
        if (errorHistory = '1') and (sampleError = '1') then
          adjustError <= '1';
        else
          adjustError <= '0';
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Logic for maintaining the flank lock.
  -----------------------------------------------------------------------------
  
  process(clk, areset_n)
  begin
    if (areset_n = '0') then
      lock <= '0';
    elsif (clk'event and clk = '1') then
      if (lock = '0') then
        if ((flank = FLANK_CENTERED) and (flankError = '0')) then
          lock <= '1';
        end if;
      else
        if ((resync_i = '1') or (adjustError = '1')) then
          lock <= '0';
        end if;
      end if;
    end if;
  end process;
  
  lock_o <= lock;
  
end architecture;
