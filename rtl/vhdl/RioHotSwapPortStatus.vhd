
-------------------------------------------------------------------------------
-- Hot Swap Port Status logic
-------------------------------------------------------------------------------

library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all; 

entity RioHotSwapPortStatus is   -- instantiate one RioHotSwapPortStatusManager per port
   generic(LinkUninitTimerWidth : integer := 24); --RapidIO standard: 24 (can be changed for more convenient TB testing)
     port(clk   : in  std_logic;
       reset_ni : in  std_logic;
    LinkUninitTimerTick_i : in  std_logic;        -- tick rate "strobe" input for this timer: x"FFFFFF" = 6-12 s --> 6 to 12 / 16777215 = 357.62789 ps to 715.25578 ps per tick --> 2.7962025MHz to 1.39810125MHz
    --
    linkInitialized_i                     : in  std_logic;  -- from RioPcs.. When '1' the link is in active state, when '0' the link is down.
    LinkOKtoUninitTransitionEnable_i      : in  std_logic;
    LinkUninitToOkTransitionEnable_i      : in  std_logic;
    LinkUninitPacketDiscardActiveEnable_i : in  std_logic;   
    LinkUninitTimeout_i                   : in  std_logic_vector(LinkUninitTimerWidth-1 downto 0);  -- if x"000000" the timer is disabled
    linkOKtoUninitTransition_o            : out std_logic;                      -- event
    linkUninitToOkTransition_o            : out std_logic;                      -- event
    linkUninitPacketDiscardActive_o       : out std_logic;                      -- event?? --(timer has expired)
    sendHotSwapEventNow_o                 : out std_logic                       -- Trigger a hotSwap message to be sent
   );
end entity;

architecture rtl of RioHotSwapPortStatus is
   signal  Expired           : std_logic;
   signal  linkInitialized_D : std_logic := '0';
   signal  linkUpEvent       : std_logic := '0';
   signal  linkDownEvent     : std_logic := '0';
   signal  linkUp            : std_logic := '0';
   signal  linkUninitPacketDiscardActive   : std_logic;
   signal  linkUninitPacketDiscardActive_D : std_logic_vector(1 downto 0) := (others=>'0');
begin

   linkEvent:process(reset_ni, clk)  -- edge detector for linkInitialized_i
   begin
      if reset_ni='0' then
         linkInitialized_D <= '0';
         linkUpEvent       <= '0';
         linkDownEvent     <= '0';
         linkUp            <= '0';
         linkUninitToOkTransition_o<='0';
         --
      elsif rising_edge(clk) then
         linkInitialized_D <= linkInitialized_i;
         linkUninitPacketDiscardActive_D<=linkUninitPacketDiscardActive_D(0) & linkUninitPacketDiscardActive;
         --
         if    linkInitialized_D = '0' and linkInitialized_i='1' then
            linkUpEvent   <= '1';
            linkDownEvent <= '0';
            linkUp        <= '1';
            linkUninitToOkTransition_o<='1';
         elsif linkInitialized_D = '1' and linkInitialized_i='0' then
            linkUpEvent   <= '0';
            linkDownEvent <= '1';
            linkUp        <= '0';
            linkUninitToOkTransition_o<='0';
         else
            linkUpEvent   <= '0';
            linkDownEvent <= '0';
            linkUninitToOkTransition_o<='0';
         end if;
         --
      end if;
   end process;
   
   linkOKtoUninitTransition_o <= linkDownEvent;
   
   sendHotSwapEventNow_o <= '1' when (LinkUninitToOkTransitionEnable_i   =  '1' and linkUpEvent   = '1') 
                                  or (LinkOKtoUninitTransitionEnable_i   =  '1' and linkDownEvent = '1') 
                                  or (LinkUninitPacketDiscardActiveEnable_i='1' and linkUninitPacketDiscardActive_D = b"01")
                                else '0';
   
   linkUninitPacketDiscardActive <= Expired when LinkUninitPacketDiscardActiveEnable_i = '1' else '0';  -- should activate after a timeout expired -from link unavailable event
                                            
   linkUninitPacketDiscardActive_o <= linkUninitPacketDiscardActive;

   LinkUninitDiscardTimer: entity work.Timer(rtl)   --implemented for IDLE1 and IDLE2 links.. not yet for IDLE3
     generic map(timerWidth => LinkUninitTimerWidth,
              repeatedPulse => false)
        port map(clk_i => clk,
              reset_ni => reset_ni,
          resetValue_i => LinkUninitTimeout_i,     -- if x"000000" the timer is disabled
           timerTick_i => LinkUninitTimerTick_i,   -- tick rate strobe input for this timer: x"FFFFFF" = 6-12 s --> 6 to 12 / 16777215 = 357.62789 ps to 715.25578 ps per tick --> 2.7962025MHz to 1.39810125MHz
                  wd_i => linkUp,                  -- watch dog input "from kicker"
             expired_o => Expired);
      
end architecture;
