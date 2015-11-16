-------------------------------------------------------------------------------
-- (C) Copyright 2013-2015 Authors and the Free Software Foundation.
--
-- This file is part of OpenRIO.
--
-- OpenRIO is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- OpenRIO is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU General Lesser Public License
-- along with OpenRIO. If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Description
-- Helper module for RioSwitch implementing hotswap event handling.
-- 
-- Backlog:
-- - Rename this file with a RioSwitch prefix.
-- 
-- Author(s): 
-- - Anders Thornemo, anders.thornemo@se.transport.bombardier.com
-- - Magnus Rosenius, magro732@hemmai.se
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- RioSwitchHotSwapPortStatus.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 


-------------------------------------------------------------------------------
-- Entity for RioSwitchHotSwapPortStatus.
-------------------------------------------------------------------------------
entity RioSwitchHotSwapPortStatus is
  generic(
    LINK_UNINIT_TIMER_WIDTH : integer);
  port(
    clk : in std_logic;
    reset_ni : in std_logic;

    linkInitialized_i : in std_logic;
    
    linkOKToUninitTransitionEnable_i : in std_logic;
    linkUninitToOKTransitionEnable_i : in std_logic;
    linkUninitPacketDiscardActiveEnable_i : in std_logic;
    linkUninitPacketDiscardActiveClear_i : in std_logic;
    linkUninitTimeout_i : in std_logic_vector(23 downto 0);
    
    linkOKToUninitTransition_o : out std_logic;
    linkUninitToOKTransition_o : out std_logic;
    linkUninitPacketDiscardActiveEvent_o : out std_logic;

    linkUninitPacketDiscardActive_o : out std_logic;
    
    sendHotSwapEventNow_o : out std_logic);
end entity;


-------------------------------------------------------------------------------
-- Architecture for RioSwitchHotSwapPortStatus.
-------------------------------------------------------------------------------
architecture rtl of RioSwitchHotSwapPortStatus is
  signal linkUpEvent : std_logic := '0';
  signal linkDownEvent : std_logic := '0';
  
  signal linkUninitPacketDiscardActive : std_logic;
  signal linkUninitPacketDiscardEvent : std_logic;

  signal linkInitialized_D : std_logic := '0';
  signal linkUninitPacketDiscardActive_D : std_logic := '0';

  signal counter : unsigned(LINK_UNINIT_TIMER_WIDTH-1 downto 0);
  signal counterReset : unsigned(LINK_UNINIT_TIMER_WIDTH-1 downto 0);
begin

  linkOKToUninitTransition_o <= linkDownEvent and linkOKToUninitTransitionEnable_i;
  linkUninitToOKTransition_o <= linkUpEvent and linkUninitToOKTransitionEnable_i;
  linkUninitPacketDiscardActiveEvent_o <= linkUninitPacketDiscardEvent and linkUninitPacketDiscardActiveEnable_i;
  linkUninitPacketDiscardActive_o <= linkUninitPacketDiscardActive;
  
  sendHotSwapEventNow_o <=
    '1' when (((linkUninitToOKTransitionEnable_i = '1') and (linkUpEvent = '1')) or
              ((linkOKToUninitTransitionEnable_i = '1') and (linkDownEvent = '1')) or
              ((linkUninitPacketDiscardActiveEnable_i = '1') and (linkUninitPacketDiscardEvent = '1'))) else '0';

  linkEvent: process(reset_ni, clk)
  begin
    if reset_ni = '0' then
      linkInitialized_D <= '0';
      linkUninitPacketDiscardActive_D <= '0';

      linkUpEvent <= '0';
      linkDownEvent <= '0';
    elsif rising_edge(clk) then
      linkInitialized_D <= linkInitialized_i;
      linkUninitPacketDiscardActive_D <= linkUninitPacketDiscardActive;

      if (linkUninitPacketDiscardActive_D = '0') and (linkUninitPacketDiscardActive = '1') then
        linkUninitPacketDiscardEvent <= '1';
      else
        linkUninitPacketDiscardEvent <= '0';
      end if;
      
      if linkInitialized_D = '0' and linkInitialized_i = '1' then
        linkUpEvent <= '1';
        linkDownEvent <= '0';
      elsif linkInitialized_D = '1' and linkInitialized_i = '0' then
        linkUpEvent <= '0';
        linkDownEvent <= '1';
      else
        linkUpEvent <= '0';
        linkDownEvent <= '0';
      end if;
    end if;
  end process;

  process(linkUninitTimeout_i)
  begin
    for i in 0 to LINK_UNINIT_TIMER_WIDTH-1 loop
      if (linkUninitTimeout_i'left-i) >= 0 then
        counterReset(counterReset'left-i) <= linkUninitTimeout_i(linkUninitTimeout_i'left-i);
      else
        counterReset(counterReset'left-i) <= '0';
      end if;
    end loop;
  end process;

  process(clk, reset_ni)

  begin
    if (reset_ni = '0') then
      counter <= (others=>'0');
      linkUninitPacketDiscardActive <= '0';
    elsif (clk'event and clk = '1') then
      if (linkInitialized_i = '1') then
        counter <= counterReset;
        linkUninitPacketDiscardActive <= '0';
      else
        if (counter /= 0) then
          counter <= counter - 1;
          linkUninitPacketDiscardActive <= '0';
        else
          if (linkUninitPacketDiscardActiveClear_i = '1') then
            linkUninitPacketDiscardActive <= '0';
          else
            linkUninitPacketDiscardActive <= '1';
          end if;
        end if;
      end if;
    end if;
  end process;
  
end architecture;
