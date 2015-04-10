-------------------------------------------------------------------------------
-- 
-- RapidIO IP Library Core
-- 
-- This file is part of the RapidIO IP library project
-- http://www.opencores.org/cores/rio/
-- 
-- Description
-- Contains a generic implementation of the Physical Coding Sublayer (PCS) for
-- 8b/10b, idle-sequence 1 for RapidIO.
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
-- NOTE THAT THIS HAS NOT BEEN TESTED IN HW YET.

-------------------------------------------------------------------------------
-- This block serializes LP-serial symbols into 8b/10b code-groups for a PMA
-- to handle.
-------------------------------------------------------------------------------
-- Only idle sequence 1 is supported.
--
-- Only a 1x-lane is supported.
--
-- Note that a K-character is only required to start an idle-sequence, it is
-- not required to be sent every fourth character. Or am I missing something?
-- Figure 4-11 seems to imply (last idle char-0, char-1, char2-, char-3,
-- char-0 in the figure) that idle sequences are four characters long but it
-- is not explicitly written in 4.7.2 in RapidIO 3.1.
--
-- The spacing of A-special characters should be ensured even when a clock-
-- compensation sequence is transmitted so that it is at least 16 code-groups
-- between.
--
-- The silence time at startup should be 120us+-40us.
--
-- REMARK: Clock compensation sequence must not be interrupted once it has been
-- enforced.
-- REMARK: Check that the 1x-statemachine transitions are followed. Also check
-- the signals defined in the standard and see that they are present.
-- REMARK: Inbound statemachine, laneSync->0 should trigger Outbound
-- statemachine->SILENT.
-- REMARK: Inbound statemachine, laneReady->0 should trigger Outbound
-- statemachine->SEEK.
-- REMARK: (clock-)enable input pin is not used, should it?
-- REMARK: When inbound idle sequences does not contain even four characters, how to
-- clock the symbols into the LP-serial layer?
-- REMARK: Clock-compensation sequence transmission cannot be implemented
-- unless there is a feed-back to the LP-serial layer telling it to hold packet
-- transmission. Or it can also be implemented implicitly by knowing that the
-- LP-serial transmitter will always end a long packet transmission.
-- REMARK: A couple of D-characters are output in the startup.
-- REMARK: Move the codec out of this layer? Would facilitate the testbench. 
-- REMARK: Add generic to tell how many lanes to output to on the PMA interface.
-- REMARK: Add force_reinit input signal. From SW-controlled bit in config-space.
-- REMARK: Move the knowledge of when to send status-control symbols from the
-- LP-serial layer into this layer? The LP-serial layer should not need to know
-- about codegroups (send status-control symbol every 1024 code-group).
-- REMARK: The clock compensation limit should be set to <(4096-276) to know
-- that a long sequence of packets do not disrupt the interval between them.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity RioPcs is
  generic(
    TICKS_PER_US : natural;
    CLOCK_COMPENSATION_LIMIT : natural := 4095);
  port(
    -- System interface.
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    -- LP-serial interface.
    portInitialized_o : out std_logic;
    outboundControlValid_i : in std_logic;
    outboundControlSymbol_i : in std_logic_vector(23 downto 0);
    outboundDataValid_i : in std_logic;
    outboundDataSymbol_i : in std_logic_vector(31 downto 0);
    inboundControlValid_o : out std_logic;
    inboundControlSymbol_o : out std_logic_vector(23 downto 0);
    inboundDataValid_o : out std_logic;
    inboundDataSymbol_o : out std_logic_vector(31 downto 0);

    -- PMA interface.
    -- REMARK: Use abcdeifghj notation instead?
    outboundOutputEnable_o : out std_logic;
    outboundRead_i : in std_logic;
    outboundCodegroup_o : out std_logic_vector(9 downto 0);
    inboundTrained_i : in std_logic;
    inboundWrite_i : in std_logic;
    inboundCodegroup_i : in std_logic_vector(9 downto 0));
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture RioPcs of RioPcs is

  component Rio8b10bEncode is
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;
      
      control_i : in std_logic;
      character_i : in std_logic_vector(7 downto 0);

      codegroup_o : out std_logic_vector(9 downto 0));
  end component;

  component Rio8b10bDecode is
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;
      
      codegroup_i : in std_logic_vector(9 downto 0);

      invalid_o : out std_logic;
      control_o : out std_logic;
      character_o : out std_logic_vector(7 downto 0));
  end component;

  component IdleSequence1 is
    generic(
      CLOCK_COMPENSATION_LIMIT : natural := 4095);
    port( 
      clk : in  std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      sendIdle_i : std_logic;
      
      sendK_o : out std_logic;
      sendA_o : out std_logic;
      sendR_o : out std_logic);
  end component;
  
  component LaneSynchronization is
    generic(
      VMIN : natural range 0 to 4095 := 0;
      IMAX : natural range 0 to 3 := 3);
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;

      signal_detect : in std_logic;
      invalid_i : in std_logic;
      control_i : in std_logic;
      character_i : in std_logic_vector(7 downto 0);

      laneSynchronized_o : out std_logic);
  end component;

  -----------------------------------------------------------------------------
  -- Constant declarations.
  -----------------------------------------------------------------------------
  constant CHARACTER_PD : std_logic_vector(8 downto 0) := "1" & x"7c";
  constant CHARACTER_SC : std_logic_vector(8 downto 0) := "1" & x"1c";
  constant CHARACTER_K : std_logic_vector(8 downto 0) := "1" & x"bc";
  constant CHARACTER_A : std_logic_vector(8 downto 0) := "1" & x"fb";
  constant CHARACTER_R : std_logic_vector(8 downto 0) := "1" & x"fd";

  -----------------------------------------------------------------------------
  -- Transmitter signals.
  -----------------------------------------------------------------------------

  type OutboundState_t is (STATE_SILENT, STATE_IDLE, STATE_CONTROL, STATE_DATA);
  signal outboundState : OutboundState_t;
  signal outboundCharacterCounter : unsigned(1 downto 0);
  signal outboundCharacter : std_logic_vector(8 downto 0);
  signal outboundPacketOngoing : std_logic;

  signal outboundSilenceTimer : natural range 0 to 120*TICKS_PER_US;
  
  signal sendIdle : std_logic;
  signal sendK : std_logic;
  signal sendA : std_logic;
  signal sendR : std_logic;
  signal sendPD : std_logic;

  alias stype1 : std_logic_vector(2 downto 0) is outboundControlSymbol_i(10 downto 8); 
  alias cmd : std_logic_vector(2 downto 0) is outboundControlSymbol_i(7 downto 5); 
  
  -----------------------------------------------------------------------------
  -- Receiver signals.
  -----------------------------------------------------------------------------

  type InboundState_t is (STATE_ILLEGAL, STATE_IDLE, STATE_D);
  signal inboundState : InboundState_t;
  signal inboundCharacterCounter : unsigned(1 downto 0);
  signal inboundControlValid : std_logic;
  signal inboundDataValid : std_logic;
  signal inboundSymbol : std_logic_vector(31 downto 0);
  
  signal inboundInvalid : std_logic;
  signal inboundCharacter : std_logic_vector(8 downto 0);
  signal laneReady : std_logic;
  signal laneSynchronized : std_logic;

begin

  portInitialized_o <=
    '1' when ((laneSynchronized = '1') and (outboundState /= STATE_SILENT))
    else '0';
  
  -----------------------------------------------------------------------------
  -- Outbound direction.
  -- Four triggers:
  -- * Clock compensation sequence.
  -- * Idle sequence.
  -- * Control symbol.
  -- * Data symbol.
  -----------------------------------------------------------------------------

  -- Determine when to send a CHARACTER_PD instead of CHARACTER_SC before the
  -- D-characters of a control symbol.
  sendPD <= '1' when (((stype1 = STYPE1_START_OF_PACKET) and (cmd = "000")) or
                      ((stype1 = STYPE1_STOMP) and (cmd = "000")) or
                      ((stype1 = STYPE1_END_OF_PACKET) and (cmd = "000")) or
                      ((outboundPacketOngoing = '1') and
                       (((stype1 = STYPE1_RESTART_FROM_RETRY) and (cmd = "000")) or
                        ((stype1 = STYPE1_LINK_REQUEST) and (cmd = "010")) or
                        ((stype1 = STYPE1_LINK_REQUEST) and (cmd = "011")) or
                        ((stype1 = STYPE1_LINK_REQUEST) and (cmd = "100")))))
            else '0';
  
  -- Idle sequence should be sent when in the idle state.
  sendIdle <= '1' when (outboundState = STATE_IDLE) else '0';
  
  -- Process deciding which code groups that should be transmitted in the
  -- outbound direction.
  Outbound: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      outboundState <= STATE_SILENT;
      outboundSilenceTimer <= 0;
      outboundOutputEnable_o <= '0';      
      
      outboundCharacterCounter <= (others=>'0');
      outboundCharacter <= (others=>'0');
      
      outboundPacketOngoing <= '0';
    elsif (clk'event and clk = '1') then
      if (outboundRead_i = '1') then
        case outboundState is

          when STATE_SILENT =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            -- REMARK: The transmitter should be disconnected here, not sending
            -- D-characters. 
            outboundOutputEnable_o <= '0';      
            if (outboundSilenceTimer /= 120*TICKS_PER_US) then
              outboundSilenceTimer <= outboundSilenceTimer + 1;
            else
              outboundState <= STATE_IDLE;
            end if;
            
          when STATE_IDLE =>
            -----------------------------------------------------------------------
            -- Idle sequence.
            -----------------------------------------------------------------------

            outboundOutputEnable_o <= '1';      
            outboundCharacterCounter <= (others=>'0');

            -- Check if there is a new symbol from the LP-serial layer.
            -- If not continue to send idle symbols.
            if (outboundControlValid_i = '1') then
              outboundState <= STATE_CONTROL;
            elsif (outboundDataValid_i = '1') then
              outboundState <= STATE_DATA;
            end if;

            -- Check which character to send.
            if (sendK = '1') then
              outboundCharacter <= CHARACTER_K;
            elsif (sendA = '1') then
              outboundCharacter <= CHARACTER_A;
            else
              outboundCharacter <= CHARACTER_R;
            end if;
            
          when STATE_CONTROL =>
            ---------------------------------------------------------------------
            -- Control symbol.
            ---------------------------------------------------------------------

            outboundCharacterCounter <= outboundCharacterCounter + 1;
            
            if (outboundCharacterCounter = "00") then
              if (sendPD = '1') then
                outboundCharacter <= CHARACTER_PD;
                if (stype1 = STYPE1_START_OF_PACKET) then
                  -- A packet is starting.
                  outboundPacketOngoing <= '1';
                else
                  -- A packet is not starting.
                  outboundPacketOngoing <= '0';
                end if;
              else
                outboundCharacter <= CHARACTER_SC;
              end if;
            elsif (outboundCharacterCounter = "01") then
              outboundCharacter <= '0' & outboundControlSymbol_i(23 downto 16);
            elsif (outboundCharacterCounter = "10") then
              outboundCharacter <= '0' & outboundControlSymbol_i(15 downto 8);
            else
              outboundCharacter <= '0' & outboundControlSymbol_i(7 downto 0);

              if (outboundControlValid_i = '1') then
                outboundState <= STATE_CONTROL;
              elsif (outboundDataValid_i = '1') then
                outboundState <= STATE_DATA;
              else
                outboundState <= STATE_IDLE;
              end if;
            end if;

          when STATE_DATA =>
            ---------------------------------------------------------------------
            -- Data symbol.
            ---------------------------------------------------------------------

            outboundCharacterCounter <= outboundCharacterCounter + 1;

            if (outboundCharacterCounter = "00") then
              outboundCharacter <= '0' & outboundDataSymbol_i(31 downto 24);
            elsif (outboundCharacterCounter = "01") then
              outboundCharacter <= '0' & outboundDataSymbol_i(23 downto 16);
            elsif (outboundCharacterCounter = "10") then
              outboundCharacter <= '0' & outboundDataSymbol_i(15 downto 8);
            else
              outboundCharacter <= '0' & outboundDataSymbol_i(7 downto 0);

              if (outboundControlValid_i = '1') then
                outboundState <= STATE_CONTROL;
              elsif (outboundDataValid_i = '1') then
                outboundState <= STATE_DATA;
              else
                outboundState <= STATE_IDLE;
              end if;
            end if;

          when others =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            outboundState <= STATE_IDLE;
        end case;
      end if;
    end if;
  end process;

  -- Generate random sequences of K, A, and R code words for the idle sequence.
  IdleSequeceGeneration: IdleSequence1
    generic map(
      CLOCK_COMPENSATION_LIMIT=>CLOCK_COMPENSATION_LIMIT)
    port map( 
      clk=>clk,
      areset_n=>areset_n,
      enable=>outboundRead_i,
      sendIdle_i=>sendIdle,
      sendK_o=>sendK,
      sendA_o=>sendA,
      sendR_o=>sendR);

  -- Convert 8-bit character into a 10-bit code-group.
  Encoder8b10b: Rio8b10bEncode
    port map(
      clk=>clk,
      areset_n=>areset_n,
      enable=>outboundRead_i,
      control_i=>outboundCharacter(8),
      character_i=>outboundCharacter(7 downto 0),
      codegroup_o=>outboundCodeGroup_o);

  -----------------------------------------------------------------------------
  -- Inbound direction.
  -----------------------------------------------------------------------------

  laneReady <= inboundTrained_i and laneSynchronized;
  
  -- Merge characters into symbols for the LP-serial layer to handle.
  Inbound: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      inboundState <= STATE_IDLE;
      inboundCharacterCounter <= "00";
      
      inboundControlValid <= '0';
      inboundDataValid <= '0';
      inboundSymbol <= (others=>'0');

      inboundControlValid_o <= '0';
      inboundDataValid_o <= '0';
    elsif (clk'event and clk = '1') then
      if (inboundWrite_i = '1') then
        if (laneReady = '1') then
          case inboundState is
            
            when STATE_IDLE =>
              -------------------------------------------------------------------
              -- Wait for SC-, PD- or D-characters.
              -------------------------------------------------------------------

              inboundControlValid_o <= inboundControlValid;
              inboundControlSymbol_o <= inboundSymbol(23 downto 0);
              inboundDataValid_o <= inboundDataValid;
              inboundDataSymbol_o <= inboundSymbol;

              inboundCharacterCounter <= "00";

              -- Determine which symbol to create.
              if ((inboundCharacter = CHARACTER_SC) or
                  (inboundCharacter = CHARACTER_PD)) then
                -- Control symbol.
                inboundState <= STATE_D;
                inboundControlValid <= '1';
                inboundDataValid <= '0';
              elsif ((inboundInvalid = '0') and (inboundCharacter(8) = '0')) then
                -- Data symbol.
                inboundState <= STATE_D;
                inboundSymbol(31 downto 24) <= inboundCharacter(7 downto 0);
                inboundControlValid <= '0';
                inboundDataValid <= '1';
              elsif (inboundInvalid = '1') then
                -- Invalid symbol.
                inboundState <= STATE_ILLEGAL;
              else
                -- Idle symbol.
                inboundControlValid <= '0';
                inboundDataValid <= '0';
              end if;

            when STATE_D =>
              -------------------------------------------------------------------
              -- Create the rest of a symbol.
              -------------------------------------------------------------------

              inboundControlValid_o <= '0';
              inboundDataValid_o <= '0';

              inboundCharacterCounter <= inboundCharacterCounter + 1;

              if ((inboundInvalid = '0') and (inboundCharacter(8) = '0')) then
                if (inboundCharacterCounter = "00") then
                  inboundSymbol(23 downto 16) <= inboundCharacter(7 downto 0);
                elsif (inboundCharacterCounter = "01") then
                  inboundSymbol(15 downto 8) <= inboundCharacter(7 downto 0);
                elsif (inboundCharacterCounter = "10") then
                  inboundSymbol(7 downto 0) <= inboundCharacter(7 downto 0);
                  inboundState <= STATE_IDLE;
                end if;
              else
                inboundState <= STATE_ILLEGAL;
              end if;

            when others =>
              -------------------------------------------------------------------
              -- Illegal combination of characters received.
              -------------------------------------------------------------------

              inboundControlValid_o <= '0';
              inboundDataValid_o <= '0';

              inboundControlValid <= '1';
              inboundDataValid <= '1';
              inboundState <= STATE_IDLE;

          end case;
        end if;
      end if;
    end if;
  end process;

  -- Instantiate the lane synchronization state machine that determines if the
  -- lane is in sync.
  -- REMARK: signal_detect, do something with it? Is it not almost the same as
  -- lane_trained (as in functionallity)?
  -- REMARK: Add VMIN and IMAX as generics to the PCS top entity?
  LaneSync: LaneSynchronization
    generic map(VMIN=>255, IMAX=>3)
    port map(
      clk=>clk,
      areset_n=>areset_n,
      enable=>inboundWrite_i,
      signal_detect=>'1',
      invalid_i=>inboundInvalid,
      control_i=>inboundCharacter(8),
      character_i=>inboundCharacter(7 downto 0),
      laneSynchronized_o=>laneSynchronized);

  -- Convert 10b code-groups to 8b control/data characters.
  Decoder8b10b: Rio8b10bDecode
    port map(
      clk=>clk,
      areset_n=>areset_n,
      enable=>inboundWrite_i,
      codegroup_i=>inboundCodegroup_i,
      invalid_o=>inboundInvalid,
      control_o=>inboundCharacter(8),
      character_o=>inboundCharacter(7 downto 0));

end architecture;



-------------------------------------------------------------------------------
-- IdleSequence1
-- This entity generates idle sequence 1. A new idle character is presented
-- at the outputs when the sendIdle_i input is asserted.
-- When enable is asserted but not sendIdle_i, it is assumed a non-idle character
-- is transmitted. This is needed to be able to know when to enforce a
-- clock-compensation sequence to be generated in the idle sequence.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity IdleSequence1 is
  generic(
    CLOCK_COMPENSATION_LIMIT : natural := 4095);
  port( 
    clk : in  std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    sendIdle_i : in std_logic;
    
    sendK_o : out std_logic;
    sendA_o : out std_logic;
    sendR_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture rtl of IdleSequence1 is

  component PseudoRandomNumberGenerator    
    port(
      clk : in std_logic;
      areset_n : in std_logic;
      enable : in std_logic;
      q_o : out std_logic_vector(3 downto 0);
      randomBit_o : out std_logic);
  end component;

  signal clockCompensationCounter : natural range 0 to CLOCK_COMPENSATION_LIMIT;
  signal clockCompensationState : unsigned(1 downto 0);
  signal clockCompensationSend : std_logic;
  signal clockCompensationSendK : std_logic;
  signal clockCompensationSendR : std_logic;

  signal pseudoRandomNumber : std_logic_vector(3 downto 0);
  signal pseudoRandomBit : std_logic;

  signal sendACounter : unsigned(4 downto 0);
  signal sendACounterZero : std_logic;

  signal sendIdle : std_logic;
  signal sendIdleDelayed : std_logic;

  signal sendK : std_logic;
  signal sendA : std_logic;
  signal sendR : std_logic;
  
begin

  -----------------------------------------------------------------------------
  -- Clock compensation sequence insertion.
  -- This logic watches the outbound characters for KRRR-sequences and
  -- increments a counter when that sequence is not detected. Once the counter
  -- has reached its limit it is stopped and a signal is asserted to indicate
  -- that a clock-compensation sequence must be transmitted. The logic below
  -- then uses the state of this logic to send the KRRR-sequence.
  -----------------------------------------------------------------------------
  clockCompensationSend <= '1' when (clockCompensationCounter = CLOCK_COMPENSATION_LIMIT) else '0';
  clockCompensationSendK <= '1' when ((clockCompensationCounter = CLOCK_COMPENSATION_LIMIT) and
                                      (clockCompensationState = "00")) else '0';
  clockCompensationSendR <= '1' when ((clockCompensationCounter = CLOCK_COMPENSATION_LIMIT) and
                                      (clockCompensationState /= "00")) else '0';
  ClockCompensationSequence: process(clk, areset_n)
  begin
    if (areset_n = '0') then
      clockCompensationCounter <= 0;
      clockCompensationState <= "00";
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        case clockCompensationState is
          when "00" =>
            if (sendK = '1') then
              clockCompensationState <= "01";
            else
              clockCompensationState <= (others=>'0');
            end if;
            if (clockCompensationCounter /= CLOCK_COMPENSATION_LIMIT) then
              clockCompensationCounter <= clockCompensationCounter + 1;
            end if;
          when "01" | "10" =>
            if (sendK = '1') then
              clockCompensationState <= "01";
            elsif (sendR = '1') then
              clockCompensationState <= clockCompensationState + 1;
            else
              clockCompensationState <= (others=>'0');
            end if;
            if (clockCompensationCounter /= CLOCK_COMPENSATION_LIMIT) then
              clockCompensationCounter <= clockCompensationCounter + 1;
            end if;
          when others =>
            if (sendK = '1') then
              clockCompensationState <= "01";
            elsif (sendR = '1') then
              clockCompensationCounter <= 0;
            else
              if (clockCompensationCounter /= CLOCK_COMPENSATION_LIMIT) then
                clockCompensationCounter <= clockCompensationCounter + 1;
              end if;
            end if;
            clockCompensationState <= (others=>'0');
        end case;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Random number generator for K-, A- and R-character insertion .
  -----------------------------------------------------------------------------

  sendIdle <= enable and sendIdle_i;
  
  -- Random bitstream generator. Used for K- and R-character generation.
  RandomKRGeneration: PseudoRandomNumberGenerator
    port map(clk=>clk, areset_n=>areset_n, enable=>sendIdle,
             q_o=>open, randomBit_o=>pseudoRandomBit);

  -- Using this extra random number generator just for the A-spacing. If only one
  -- is used, the distribution is not very good:
  -- 16:0 17:2 18:0 19:0 20:1 21:1 22:394 23:1 24:393 25:0 26:393 27:1 28:1179 29:786 30:0 31:393
  -- When this extra random generator is used as below it looks like this instead:
  -- 16:228 17:264 18:264 19:260 20:262 21:262 22:262 23:262 24:261 25:263 26:262 27:261 28:262 29:260 30:260 31:261
  RandomASpacing: PseudoRandomNumberGenerator
    port map(clk=>clk, areset_n=>areset_n, enable=>sendACounterZero,
             q_o=>pseudoRandomNumber, randomBit_o=>open);

  -- Detect when the A-spacing counter has expired.
  sendACounterZero <= '1' when (sendACounter = "00000") else '0';

  -- Counter that determines when to send an A-character.
  -- Note that A-characters are needed for alignment between lanes and is not
  -- really needed in a 1x-PCS.
  ASpacingCounter: process(areset_n, clk)
  begin    
    if areset_n = '0'  then    
      sendACounter <= (others => '0');       
    elsif rising_edge(clk) then
      if (sendIdle = '1') then
        if (sendACounterZero = '1') then
          sendACounter <= '1' & unsigned(pseudoRandomNumber);
        else
          sendACounter <= sendACounter - 1;
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Output when to send each of the K-, A- and R-character.
  -----------------------------------------------------------------------------

  SendIdleDFF: process(areset_n, clk)
  begin    
    if areset_n = '0'  then    
      sendIdleDelayed <= '0';
    elsif rising_edge(clk) then
      if (enable = '1') then
        sendIdleDelayed <= sendIdle_i;
      end if;
    end if;
  end process;

  -- Send K- at start of idle, when the random bit is 1 and when a
  -- clock-compensation sequence at the first position.
  sendK <= sendIdle_i and ((not sendIdleDelayed) or
                           (sendIdleDelayed and (not sendACounterZero) and
                            (((not clockCompensationSend) and pseudoRandomBit) or
                             clockCompensationSendK)));

  -- Send R- when not at the start of idle, when the random bit is 0 and when a
  -- clock-compensation sequence at the other positions than the first.
  sendR <= sendIdle_i and sendIdleDelayed and (not sendACounterZero) and
           (((not clockCompensationSend) and (not pseudoRandomBit)) or
            clockCompensationSendR);

  -- Send A- when the A-spacing counter has expired.
  sendA <= sendIdle_i and sendIdleDelayed and sendACounterZero and
           (not clockCompensationSend);

  -- Assign the outputs.
  sendK_o <= sendK;
  sendA_o <= sendA;
  sendR_o <= sendR;
  
end architecture;



-------------------------------------------------------------------------------
-- PseudoRandomNumberGenerator
-- This entity generates random bits and 4-bit numbers. It is used in figure
-- 4.4 in RapidIO 3.1 part6.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;


-------------------------------------------------------------------------------
-- Entity for PseudoRandomNumberGenerator.
-------------------------------------------------------------------------------
entity PseudoRandomNumberGenerator is
  port( 
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;
    
    q_o : out std_logic_vector(3 downto 0);
    randomBit_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- Architecture for PseudoRandomNumberGenerator.
-------------------------------------------------------------------------------
architecture behavioral of PseudoRandomNumberGenerator is
  signal lfsr : std_logic_vector(7 downto 1);
  signal q0 : std_logic;
  
begin
  
  -- Output assignment.
  -- Note that this is the same assignment as in the standard.
  q_o <= lfsr(6) & lfsr(4) & lfsr(3) & lfsr(1);
  randomBit_o <= lfsr(7);

  -- P(x) = x^7 + x^6 + 1
  -- This is a maximal-length polynomial that repeats itself after 127 ticks.
  q0 <= lfsr(7) xor lfsr(6);

  process(clk, areset_n)
  begin 
    if areset_n = '0' then 
      lfsr <= "0000001";
    elsif rising_edge(clk) then
      if (enable = '1') then
        lfsr <= lfsr(6 downto 1) & q0;
      end if;
    end if; 
  end process;
  
end architecture;



-------------------------------------------------------------------------------
-- This entity implements the Lane_Synchronization State Machine described in
-- Figure 4-14, part 6 in the 3.1 RapidIO standard.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rio_common.all;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
entity LaneSynchronization is
  generic(
    VMIN : natural range 0 to 4095 := 0;
    IMAX : natural range 0 to 3 := 3);
  port(
    clk : in std_logic;
    areset_n : in std_logic;
    enable : in std_logic;

    signal_detect : in std_logic;
    invalid_i : in std_logic;
    control_i : in std_logic;
    character_i : in std_logic_vector(7 downto 0);

    laneSynchronized_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------
architecture LaneSynchronizationImpl of LaneSynchronization is
  type StateType is (STATE_NOSYNC, STATE_SYNC_PENDING,
                     STATE_SYNC, STATE_NOSYNC_PENDING);
  signal state : StateType;
  
  signal kCounter : natural range 0 to 127;
  signal vCounter : natural range 0 to 4095;
  signal iCounter : natural range 0 to 3;

  signal comma : std_logic;
  signal invalid : std_logic;

  signal inboundCharacter : std_logic_vector(8 downto 0);
  
begin

  inboundCharacter <= control_i & character_i;
  comma <= '1' when (inboundCharacter = ('1' & x"bc")) else '0';
  invalid <= invalid_i;
  
  process(areset_n, clk)
  begin
    if (areset_n = '0') then
      state <= STATE_NOSYNC;
      kCounter <= 0;
      vCounter <= 0;
      iCounter <= 0;
      laneSynchronized_o <= '0';
    elsif (clk'event and clk = '1') then
      if (enable = '1') then
        case state is
          
          when STATE_NOSYNC =>
            -------------------------------------------------------------------
            -- 
            -------------------------------------------------------------------
            kCounter <= 0;
            vCounter <= 0;
            laneSynchronized_o <= '0';
            if (signal_detect = '1') and (comma = '1') then
              state <= STATE_SYNC_PENDING;
            end if;

          when STATE_SYNC_PENDING =>
            -------------------------------------------------------------------
            -- 
            -------------------------------------------------------------------
            if (comma = '1') then
              if (kCounter /= 127) then
                kCounter <= kCounter + 1;
              end if;
              if (vCounter /= VMIN) then
                vCounter <= vCounter + 1;
              end if;
              if ((kCounter = 127) and (vCounter = VMIN)) then
                state <= STATE_SYNC;
              end if;
            elsif (invalid = '1') then
              state <= STATE_NOSYNC;
            else
              if (vCounter /= VMIN) then
                vCounter <= vCounter + 1;
              end if;
            end if;

          when STATE_SYNC =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            laneSynchronized_o <= '1';
            iCounter <= 1;
            vCounter <= 0;
            if (invalid = '1') then
              state <= STATE_NOSYNC_PENDING;
            end if;

          when STATE_NOSYNC_PENDING =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            if (invalid = '1') then
              if (iCounter = IMAX) then
                state <= STATE_NOSYNC;
              else
                iCounter <= iCounter + 1;
              end if;
            else
              if (vCounter = 255) then
                vCounter <= 0;
                if (iCounter = 0) then
                  state <= STATE_SYNC;
                else
                  iCounter <= iCounter - 1;
                end if;
              else
                vCounter <= vCounter + 1;
              end if;
            end if;

          when others =>
            ---------------------------------------------------------------------
            -- 
            ---------------------------------------------------------------------
            state <= STATE_NOSYNC;
            
        end case;
      end if;
    end if;
  end process;

end architecture;

